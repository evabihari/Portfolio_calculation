%%%-------------------------------------------------------------------
%%% @author eva.bihari <evabihari@Evas-MacBook-Pro.local>
%%% @copyright (C) 2016, eva.bihari
%%% @doc
%%%
%%% @end
%%% Created :  9 Feb 2016 by eva.bihari <evabihari@Evas-MacBook-Pro.local>
%%%-------------------------------------------------------------------
-module(arfolyam).
-export [start/0,start/1,
         read_portfolio/0,
         read_portfolio/1,
         calculate_portfolio/0].
-define (VL_URL,"https://www.viennalife.hu/befektetes/eszkozalapok/napi-arfolyam").
-define (ML_URL,"http://www.metlifehungary.hu/portfoliok").
-define (PORTFOLIO_FILE,"portfolio.txt").
-include("records.hrl").

%%%===================================================================
%%% API
%%%===================================================================
start() ->
    start(?VL_URL),
    start(?ML_URL,ok).
start(Url) ->
    start(Url,init()).

read_portfolio()->
    read_portfolio(?PORTFOLIO_FILE).
read_portfolio(Name) ->
    {ok,F} = file:open(Name,[read,write]),
    read_papers(F).

calculate_portfolio()->
    utils:calculate_portfolio().

%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------

%%%===================================================================
%%% Internal functions
%%%===================================================================
init() ->
    code:add_pathz("/Users/evabihari/external/mochiweb/ebin"),
    code:add_pathz("/Users/evabihari/external/mochiweb_xpath/ebin"),
    error_logger:tty(false),
    error_logger:logfile({open, log_report}),
    inets:start(),
    ssl:start(),
    %% lager:start(),
    mnesia:create_schema([node()]),
    mnesia:start(),
    create_portfolio_table(),
    create_daily_values_table(),
    case mnesia:create_table(exchanges, 
                             [{disc_copies,[node()]},
                              {type, ordered_set},
                              {attributes, record_info(fields, exchange)},
                              {record_name, exchange}]) of
        {atomic,ok} -> ok;
        {aborted,{already_exists,tickets}} -> 
            error_logger:info_msg("exchanges table already_exists");
        Other ->
            error_logger:error_msg("exchanges table creation failed , reason = ~p~n",[Other])
    end.

start(Url,ok) ->
    %% httpc:set_options([{cookies,verify}]),
    _Method=get,
    %% Header=[],
    _Type = "application/x-www-form-urlencoded",
    %% Body = "username=enzian1&password=ethebi",
    _HTTPOptions=[],
    _Options=[],
    %% R = httpc:request(Method, {Url, Header, Type, Body}, HTTPOptions,Options), 
    R = httpc:request(Url),
    {ok, {{"HTTP/1.1",_ReturnCode, _State}, _Head, Body}} = R,
    {ok,F} = file:open("page.txt",[read,write]),
    file:write(F,Body),
    %% tokenize and then transform the token stream into a HTML tree.
    Struct=mochiweb_html:parse(Body),
    {ok,F2} = file:open("structs.txt",[read,write]),
    file:write(F2,io_lib:print(Struct)),
    Date=extract_date(Struct,Url),
    extract_and_store_rates(Struct,Date,Url);
start(_Url,Other) ->
    io:format("Init failed with reason = ~p~n",[Other]).

extract_and_store_rates(Struct,Date,?VL_URL) ->
    Path="html/body/div/div/div/div/div/ul",
    Results = mochiweb_xpath:execute(Path,Struct),
    {ok,File} = file:open("values.txt",[read,write]),
    file:write(File,io_lib:print(Results)),
    decode_types(Results,Date);
extract_and_store_rates(Struct,Date,?ML_URL) ->
    Path="html/body/div/div/div/div/div/div/table/tbody/tr",
    Results = mochiweb_xpath:execute(Path,Struct),
    {ok,File} = file:open("ML_values.txt",[read,write]),
    file:write(File,io_lib:print(Results)),
    decode_ml_types(Results,Date).

decode_types([],_) ->
    ok;
decode_types([Result|_List],Date) ->
    Path="ul/li/p",
    R = mochiweb_xpath:execute(Path,Result),
    decode_values(R,Date).
    %%    only the 1st part is interesting at this moment
    %%    decode_types(List).

decode_values([],_) ->
    ok;
decode_values([Value|List],Date) ->
    PName="p/span[position() = 1]",
    PValue="p/span[position() = 2]",

    [{_,_,[RName]}] =  mochiweb_xpath:execute(PName,Value),
    Name=binary_to_list(RName),
    RValue = case mochiweb_xpath:execute(PValue,Value) of
                 [{_,_,[RV]}] -> Rate=binary_to_list(RV),
                                 create_record_and_store(Name, Rate, Date),
                                 Rate;
                 _ -> "don't know"
             end,
    io:format("~nName=~p,  Value=~p~n",[Name,RValue]),
    decode_values(List,Date).

extract_date(Struct,?VL_URL)->
    Path="html/body/div/div/div/div/div/h5[position() = 1]",
    R=mochiweb_xpath:execute(Path,Struct),
    case R of
        [{_,_,[BinString]}|_] ->
            String=binary_to_list(BinString),
            SList=string:tokens(String,"()"),
            DateString=lists:last(SList),
            [Y,M,D]=string:tokens(DateString,". "),
            Y++"-"++M++"-"++D;
        _ -> {{Y,M,D},_}=calendar:local_time(),
             utils:create_date(Y,M,D)
    end;
extract_date(Struct,?ML_URL) ->
    Path="html/body/div/div/div/div/div/div/table/thead/tr[position() = 2]",
    [R]=mochiweb_xpath:execute(Path,Struct),
    [R1]=mochiweb_xpath:execute("tr/td[position() = 2]",R),
    [{_,_,[DateBin]}]=mochiweb_xpath:execute("td/div/span[position() =2]",R1),
    %% [{<<"span">>,[{<<"class">>,<<"data">>}],[<<"2016.02.01">>]}]
    re:replace(binary_to_list(DateBin),"\\Q.\\E","-",[global,{return,list}]).

create_record_and_store(Name, RateInfo, Date) ->
    [ValueString,Currency]=string:tokens(RateInfo," "),
    {Value,_}=string:to_float(ValueString),
    Record=#exchange{name_and_date={utils:encode_name(string:strip(Name,both)),
                                    Date},
                     value=Value,
                     currency=Currency},    
    mnesia:dirty_write(exchanges,Record).

decode_ml_types([],_Date) ->
    ok;
decode_ml_types([Result|List],Date) ->
    io:format(" ~p ~p~n",[Date, Result]),
    NamePart=mochiweb_xpath:execute("tr/td/a",Result),
    Name=extract_value(NamePart),
    %% [{<<"a">>,
    %%   [{<<"href">>,<<"/portfoliok/Y1/">>}],
    %%   [<<"Ázsiai ingatlan részvény eszközalap (HUF)"/utf8>>]}]    
    ValuePart=mochiweb_xpath:execute("tr/td[position() = 2]",Result),
    %% [{<<"td">>,
    %%   [{<<"class">>,<<"number number-1">>}],
    %%   [<<"1,13703 HUF">>]}]
    ValueString=extract_value(ValuePart),
    io:format(" ~p ~p~n",[Name, ValueString]),
    create_record_and_store(Name, ValueString,Date),
    decode_ml_types(List,Date).

extract_value([{_,_,[ValueString]}]) ->
    binary_to_list(ValueString).

read_papers(F) ->
    case file:read_line(F) of
        {ok,Data} ->
            handle_data(Data,F);
        eof ->
            ok;
        {error,Reason} ->
            {error,Reason}
    end.

handle_data(Data,F) ->
    [Name,Number,Currency,_Type|_]=string:tokens(Data,"|"),
    {N,_}=string:to_integer(string:strip(Number)),
    Name1=string:strip(Name,both),
    Record=#paper{name=Name1,
                  number=N,
                  currency=Currency},
    NewRecord= case mnesia:dirty_read(portfolio,Name1) of
                   [] -> Record;
                   [Old_rec] -> %same type of paper already in the portfolio
                       Old_number=Old_rec#paper.number,
                       Record#paper{number=Old_number+N}
               end,
    mnesia:dirty_write(portfolio,NewRecord),
    read_papers(F).

create_daily_values_table()->
    case mnesia:create_table(daily_values, 
                             [{disc_copies,[node()]},
                              {type, ordered_set},
                              {attributes, 
                               record_info(fields, daily_value)},
                              {record_name, daily_value}]) of
        {atomic,ok} -> ok;
        {aborted,{already_exists,daily_values}} -> 
            error_logger:info_msg("daily_values table already_exists");
        Other ->
            error_logger:error_msg("daily_values table creation failed , 
                                    reason = ~p~n",[Other])
    end.

create_portfolio_table()->
    mnesia:delete_table(portfolio),
    case mnesia:create_table(portfolio, 
                             [{disc_copies,[node()]},
                              {type, ordered_set},
                              {attributes, record_info(fields, paper)},
                              {record_name, paper}]) of
        {atomic,ok} -> ok;
        {aborted,{already_exists,portfolio}} -> 
            error_logger:info_msg("portfolio table already_exists");
        Other ->
            error_logger:error_msg("portfolio table creation failed , reason = ~p~n",[Other])
    end.








