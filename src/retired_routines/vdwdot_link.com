# vdwdot_link.com 
echo
echo "# link vdwdot ***"
echo "makver.exe # update link time subroutine"
makver.exe
echo "ar x hole.a vdwdot.o              # get vdwdot.o from archive"
ar x hole.a vdwdot.o
echo "f77 vdwdot.o hole.a -o ../exe/vdwdot # link "
f77 vdwdot.o hole.a -o ../exe/vdwdot
echo "rm vdwdot.o                       # delete .o file"
rm vdwdot.o
