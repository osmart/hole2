# labqpt_link.com 
echo
echo "# link labqpt ***"
echo "makver.exe # update link time subroutine"
makver.exe
echo "ar x hole.a labqpt.o              # get labqpt.o from archive"
ar x hole.a labqpt.o
echo "f77 labqpt.o hole.a -o ../exe/labqpt # link "
f77 labqpt.o hole.a -o ../exe/labqpt
echo "rm labqpt.o                       # delete .o file"
rm labqpt.o
