#scatterplot.plt
set term x11 font "-*-helvetica-medium-r-*-*-14-*-*-*-*-*-*-*"
set title "Algebraic Grid Mesh with Imax = 80 and Jmax = 20"
set nokey
set xlabel "x"
set ylabel "y"
set xrange [0:5]
set yrange [-1:2]
m="dataY.txt"
n="dataX.txt"
plot m with lines lt -1 , n with lines lt -1 

set term png             
set output "AlgebraicGridPlot.png" 
replot
set term x11