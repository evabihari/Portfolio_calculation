#+begin_src markdown
Purpose:
========

Extract daily exchange rates from Viennalife and MetLife Hungarian
portal and calculate the value of the existing portfolio. All data are
stored into Mnesia disc copies.

Input files:
------------

`data/portfolio.txt` - structure with example:
`Paper Name | number_of_papers |Currency(HUF/EUR| Paper type
Azsia reszveny | 102 | HUF | VL_eseti`

## Usage: ##

1. parse web pages with the latest exchange rates: `arfolyam:start(). ` 
2. read the above mentioned input file and store portfolio information
into mnesia: ` arfolyam:read_portfolio(). `
3. calculate the actual value of the portfolio based on the latest
exchange rates: ` arfolyam:calculate_portfolio().`

In order to check the Mnesia content the easiest way:
` mnesia:dump_to_textfile(“data/mnesia.txt"). `

## Cron job on OS X ##

time when I want to execute the task:
0 13 * * * (every day at 1:00 p.m)

Name of the escript: 
`daily`
crontab entry:
`0 13 * * * <PATH_TO_THE ESCRIPT\daily  `

env EDITOR=nano crontab -e

## Create CSV files from the Mnesia database ##

`utils:dump_to_csv(FileName)`
will dump the daily portfolio values to `Filename` it is ordered by
Currency, Type and Date -> esy to create graphs to see trend for a
different paper types

`utils:sum_csv(FileName)`
will write a summary about the portfolio to `Filename`
Ordered by Date and all calculated values are written to 1 row/date

To open the CSV files on Mac in Excel you need to use the "Import"
function, than choose fields by delimiter (not the fixed with)

$+end_src
