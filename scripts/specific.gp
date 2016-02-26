# gnuplot script
# input parameter: gnuplot -e 'type="SUM"; currency="HUF"' test.gp
set terminal postscript eps enhanced color solid lw 2 font 'Helvetica,12
if (exist("type")) print type
if (!exist("type")) type="SUM"
if (!exist("currency")) currency="HUF"
set terminal x11 
set key below
set key left invert
set grid y
set yrange [9000 :*]
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
set title "data/D_EUR.dat egyenlegek " 
plot "data/D_EUR.dat" using 1:(stringcolumn(2) eq "ML EUR"? column(3):1/0) title "ML EUR" ls 15*(rand(0))
replot "data/D_EUR.dat" using 1:(stringcolumn(2) eq "SUM"? column(3):1/0) title "SUM" ls 15*(rand(0))
set output '| /usr/local/bin/ps2pdf - data/D_EUR.dat.pdf 
set size 1,1 
set term post portrait color "Times-Roman" 12 
replot 
