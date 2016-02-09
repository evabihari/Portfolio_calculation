-record(exchange,
        {name_and_date, 
          %ex: {"Arany arupiaci forint eszkozalap","2016-01-28"}
         value,
         currency}).
-record(paper,
        {name,
         number,
         currency,
         type}).
-record(daily_value,
        {date_currency_type,
          %ex: {"2016-01-28,"HUF"}
         value}).
