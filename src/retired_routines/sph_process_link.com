# sph_process_link.com 
echo
echo "# link sph_process ***"
echo "makver.exe # update link time subroutine"
makver.exe
echo "ar x hole.a  sph_process.o              # get  sph_process.o from archive"
      ar x hole.a  sph_process.o              # get  sph_process.o from archive
echo "f77 sph_process.o hole.a -o ../exe/sph_process # link "
      f77 sph_process.o hole.a -o ../exe/sph_process
echo "rm sph_process.o                       # delete .o file"
      rm sph_process.o
