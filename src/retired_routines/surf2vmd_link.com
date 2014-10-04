# surf2vmd_link.com 
echo
echo "# link surf2vmd ***"
echo "makver.exe # update link time subroutine"
makver.exe
echo "ar x hole.a  surf2vmd.o              # get  surf2vmd.o from archive"
      ar x hole.a  surf2vmd.o              # get  surf2vmd.o from archive
echo "f77 surf2vmd.o hole.a -o ../exe/surf2vmd # link "
      f77 surf2vmd.o hole.a -o ../exe/surf2vmd
echo "rm surf2vmd.o                       # delete .o file"
      rm surf2vmd.o
