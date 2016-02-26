# gnuplot script
# input parameter: gnuplot -e 'type="SUM"; currency="HUF"' test.gp
if (exist("type")) print type
if (!exist("type")) type="SUM"
if (!exist("currency")) currency="HUF"
set terminal x11 
set title "Statistics" 
set key below
set key left invert
set grid y
set yrange [0 :*]
set ylabel "SUM" tc lt 1
set ytics nomirror
set xtics nomirror rotate by -45 scale 0 font ",10" 
set key noinvert box
set xdata time
set timefmt '%Y-%m-%d'
## set xrange ['2016-01-28':'2016-03-01']
set xrange ['2016-01-28':*]
set format x "%y-%m-%d"
set datafile separator "|"
file="../data/D_".currency.".dat"
print file
plot file using 1:(stringcolumn(2) eq type? column(3):1/0) title "a" lc rgb "blue"

