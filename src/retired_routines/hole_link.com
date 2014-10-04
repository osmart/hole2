# hole_link.com 
echo
echo "# link hole ***"
echo "makver.exe # update link time subroutine"
makver.exe
echo "ar x hole.a hole.o              # get hole.o from archive"
ar x hole.a hole.o
echo "f77 hole.o hole.a -o ../exe/hole # link "
f77 hole.o hole.a -o ../exe/hole
echo "rm hole.o                       # delete .o file"
rm hole.o
