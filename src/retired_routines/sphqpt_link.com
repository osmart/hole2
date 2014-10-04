# sphqpt_link.com 
echo
echo "# link sphqpt ***"
echo "makver.exe # update link time subroutine"
makver.exe
echo "ar x hole.a sphqpt.o              # get sphqpt.o from archive"
ar x hole.a sphqpt.o
echo "f77 sphqpt.o hole.a -o ../exe/sphqpt # link "
f77 sphqpt.o hole.a -o ../exe/sphqpt
echo "rm sphqpt.o                       # delete .o file"
rm sphqpt.o
