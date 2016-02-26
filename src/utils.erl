%%%-------------------------------------------------------------------
%%% @author eva.bihari <evabihari@Evas-MacBook-Pro.local>
%%% @copyright (C) 2016, eva.bihari
%%% @doc
%%%
%%% @end
%%% Created :  9 Feb 2016 by eva.bihari <evabihari@Evas-MacBook-Pro.local>
%%%-------------------------------------------------------------------
-module(utils).
-include("../include/records.hrl").
%% API
-export([calculate_portfolio/0,
         encode_name/1,
         create_date/3,
         dump_daily_values_table/0]).

-define (GEN_GP,"scripts/generic.gp").
-define (SPEC_GP,"scripts/specific.gp").

%%%===================================================================
%%% API
%%%===================================================================
calculate_portfolio()->
    {{Y,M,D},_}=calendar:local_time(),
    Today=int_to_string(Y)++"-"++int_to_string(M)++"-"++int_to_string(D),
    case mnesia:dirty_match_object(daily_values,
                       #daily_value{date_currency_type={Today,'_','_'},_='_'}) of
        [] -> ok;
        Results -> remove_old_data(Results)
    end,
    FirstKey=mnesia:dirty_first(portfolio),
    calculate_portfolio2(FirstKey,Today).
%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------

%%%===================================================================
%%% Internal functions
%%%===================================================================
calculate_portfolio2('$end_of_table',Today) ->
    sum_daily_values(Today),
    ok;
calculate_portfolio2(Key,Today) ->
    {Name,_Type}=Key,
    Value=find_daily_value(Name,Today),
    [Portfolio]=mnesia:dirty_read(portfolio,Key),
    agregate_daily_values(Portfolio,Value,Today),
    calculate_portfolio2(mnesia:dirty_next(portfolio,Key),Today).

find_daily_value(Name,Date) ->
    case mnesia:dirty_read(exchanges,{Name,Date}) of
        [] ->
            Date1=less(Date),
            find_daily_value(Name,Date1);
        [Record] ->
            Record#exchange.value
    end.

less(Date) ->
    [Y,M,D]=string:tokens(Date,"-"),
    {Year,_}=string:to_integer(Y),
    {Month,_}=string:to_integer(M),
    {Day,_}=string:to_integer(D),
    less(Year,Month,Day).
less(Y,M,D) when (D==1) ->
    {Year,Month,Day} =case M of
                  1 -> {Y-1,12,31};
                  2 -> {Y,1,31};
                  3 -> {Y,2,29};
                  4 -> {Y,3,31};
                  5 -> {Y,4,30};
                  6 -> {Y,5,31};
                  7 -> {Y,6,30};
                  8 -> {Y,7,30};
                  9 -> {Y,8,31};
                  10 ->{Y,9,30};
                  11 ->{Y,10,31};
                  12 ->{Y,11,30}
                      end,
    int_to_string(Year)++"-"++int_to_string(Month)++"-"++int_to_string(Day);
less(Y,M,D) ->
    int_to_string(Y)++"-"++int_to_string(M)++"-"++int_to_string(D-1).

int_to_string(Int) when Int<10 ->
    "0"++integer_to_list(Int);
int_to_string(Int) ->
    integer_to_list(Int).

remove_old_data([]) ->
    ok;
remove_old_data([DV|List]) ->
    mnesia:dirty_delete(daily_values,DV#daily_value.date_currency_type),
    remove_old_data(List).

agregate_daily_values(Portfolio,Value,Date) ->
    {_Name,Type}=Portfolio#paper.name_and_type,
    Number=Portfolio#paper.number,
    Currency=Portfolio#paper.currency,
    Val=Number*Value,
    New_DV=case mnesia:dirty_read(daily_values,{Date,Currency,Type}) of
               [] ->   #daily_value{
                        date_currency_type={Date,Currency,Type},
                        value=Val};
               [DV|_] -> case DV#daily_value.date_currency_type of
                             %% {_,_,"SUM"} ->
                             %%     #daily_value{
                             %%        date_currency_type={Date,Currency,Type},
                             %%        value=Val};
                             {_,_,_Other} ->
                                 OldV=DV#daily_value.value,
                                 NewV=OldV+Val,
                                 DV#daily_value{value=NewV}
                         end
           end,
    mnesia:dirty_write(daily_values,New_DV).

sum_daily_values(Date) ->
    sum_daily_values(Date,0,0,mnesia:dirty_first(daily_values)).
sum_daily_values(Date,EUR_SUM, HUF_SUM,'$end_of_table') ->
    EUR_DV=#daily_value{
              date_currency_type={Date,"EUR","SUM"},
              value=EUR_SUM},
    mnesia:dirty_write(daily_values,EUR_DV),
    HUF_DV=#daily_value{
              date_currency_type={Date,"HUF","SUM"},
              value=HUF_SUM},
    mnesia:dirty_write(daily_values,HUF_DV);
sum_daily_values(Date,EUR_SUM, HUF_SUM,Key) ->
   {New_EUR_SUM,NEW_HUF_SUM} = case Key of
                                   {Date,"EUR","SUM"} -> {EUR_SUM,HUF_SUM};
                                   {Date,"HUF","SUM"} -> {EUR_SUM,HUF_SUM};
                                   {Date,"EUR",_} ->
                                       [Record]=mnesia:dirty_read(daily_values,Key),
                                       {EUR_SUM+Record#daily_value.value,HUF_SUM};
                                   {Date,"HUF",_} ->
                                       [Record]=mnesia:dirty_read(daily_values,Key),
                                       {EUR_SUM,HUF_SUM+Record#daily_value.value};
                                   _ ->
                                        {EUR_SUM,HUF_SUM}
                               end,
    New_key=mnesia:dirty_next(daily_values,Key),
    sum_daily_values(Date,New_EUR_SUM,NEW_HUF_SUM,New_key).
    
encode_name([]) -> "";
encode_name([225|Name])->
    "a"++encode_name(Name);
encode_name([246|Name])->
    "o"++encode_name(Name);
encode_name([195,188|Name])->
    "u"++encode_name(Name);
encode_name([195,186|Name])->
    "u"++encode_name(Name);
encode_name([195,161|Name])->
    "a"++encode_name(Name);
encode_name([195,169|Name])->
    "e"++encode_name(Name);
encode_name([195,179|Name])->
    "o"++encode_name(Name);
encode_name([195,173|Name])->
    "i"++encode_name(Name);
encode_name([195,182|Name])->
    "o"++encode_name(Name);
encode_name([195,129|Name])->
    "A"++encode_name(Name);
encode_name([197,177|Name])->
    "u"++encode_name(Name);
encode_name([197,145|Name])->
    "o"++encode_name(Name);
encode_name([233|Name])->
    "e"++encode_name(Name);
encode_name([193|Name])->
    "A"++encode_name(Name);
encode_name([252|Name])->
    "u"++encode_name(Name);
encode_name([237|Name])->
    "i"++encode_name(Name);
encode_name([250|Name])->
    "u"++encode_name(Name);
encode_name([369|Name])->
    "u"++encode_name(Name);
encode_name([Ch|Name]) when Ch>127 ->
    encode_name(Name);
encode_name([Ch|Name]) ->
    lists:flatten([Ch|encode_name(Name)]).

create_date(Y,M,D) when D<0 ->
    create_date(Y,M-1,30);
create_date(Y,M,D) when (M<10),(D<10) ->
    integer_to_list(Y)++"-0"++integer_to_list(M)++"-0"++integer_to_list(D);
create_date(Y,M,D) when M<10  ->
    integer_to_list(Y)++"-0"++integer_to_list(M)++"-"++integer_to_list(D);
create_date(Y,M,D) when D<10  ->
    integer_to_list(Y)++"-"++integer_to_list(M)++"-0"++integer_to_list(D);
create_date(Y,M,D) ->
    integer_to_list(Y)++"-"++integer_to_list(M)++"-"++integer_to_list(D).

dump_daily_values_table() ->
    dump_daily_values_table("HUF"),
    dump_daily_values_table("EUR").


dump_daily_values_table(Type) ->
    Name="data/D_"++Type++".dat",
    {ok,F} = file:open(Name,[read,write]),
    Sets=store_values(F,ets:match(daily_values,{'_',{'$1',Type,'$2'},'$3'}),sets:new()),
    file:close(F),
    draw_diagram(Name,sets:to_list(Sets)).

store_values(_F,[],Types) ->
    Types;
store_values(F,[Value|VList],Set) ->
    io:format("~p ~n",[Value]),
    [Date, Type,Money]=Value,
    String=Date ++ "|" ++ Type ++ "|" ++ io_lib:write(Money) ++ "\n",
    file:write(F,String),
    NewSet=sets:add_element(Type,Set),
    store_values(F,VList,NewSet).

draw_diagram(FileName,Types) ->
    file:copy(?GEN_GP,?SPEC_GP),
    {ok,Target}=file:open(?SPEC_GP,[read,append]),
    String="set title \"" ++ FileName ++ " egyenlegek \" \n",
    ok=file:write(Target, String),
    add_diagram(Target,FileName,Types),
    file:close(Target),
    Cmd="gnuplot "++ ?SPEC_GP,
    os:cmd(Cmd).


add_diagram(Target,FileName,[Type|Types]) ->
    String = "plot "++generate_string(FileName,Type),
    io:format("String is~p ~n",[String]),
    ok=file:write(Target,String),
    add_diagrams(Target,FileName,Types).

add_diagrams(Target,FN,[]) ->
    String = "set output '| /usr/local/bin/ps2pdf - " ++ FN ++ ".pdf \n",
    ok=file:write(Target,String),
    S2= "set size 1,1 \n" ++
        "set term post portrait color \"Times-Roman\" 12 \n" ++
        "replot \n",
    ok=file:write(Target,S2),
    ok;
add_diagrams(Target,FileName,[Type|Types]) ->
    String = "replot "++generate_string(FileName,Type),
    ok=file:write(Target,String),
    add_diagrams(Target,FileName,Types).

generate_string(FileName,Type) ->
    "\""++FileName++"\""++" using 1:(stringcolumn(2) eq "++"\""++Type++"\""++
        "? column(3):1/0) title "++"\""++Type++"\""++" ls 15*(rand(0))" ++ "\n".
