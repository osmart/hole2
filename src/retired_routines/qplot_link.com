# qplot_link.com 
echo
echo "# link qplot ***"
echo "makver.exe # update link time subroutine"
makver.exe
echo "ar x hole.a qplot.o              # get qplot.o from archive"
ar x hole.a qplot.o
echo "f77 qplot.o hole.a -o ../exe/qplot # link "
f77 qplot.o hole.a -o ../exe/qplot
echo "rm qplot.o                       # delete .o file"
rm qplot.o
