# msms2vmd_link.com 
echo
echo "# link msms2vmd ***"
echo "makver.exe # update link time subroutine"
makver.exe
echo "ar x hole.a  msms2vmd.o              # get  msms2vmd.o from archive"
      ar x hole.a  msms2vmd.o              # get  msms2vmd.o from archive
echo "f77 msms2vmd.o hole.a -o ../exe/msms2vmd # link "
      f77 msms2vmd.o hole.a -o ../exe/msms2vmd
echo "rm msms2vmd.o                       # delete .o file"
      rm msms2vmd.o
