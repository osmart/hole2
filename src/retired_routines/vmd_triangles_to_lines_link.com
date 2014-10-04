# vmd_triangles_to_lines_link.com 
echo
echo "# link vmd_triangles_to_lines ***"
echo "makver.exe # update link time subroutine"
makver.exe
echo "ar x hole.a  vmd_triangles_to_lines.o              # get  vmd_triangles_to_lines.o from archive"
      ar x hole.a  vmd_triangles_to_lines.o              # get  vmd_triangles_to_lines.o from archive
echo "f77 vmd_triangles_to_lines.o hole.a -o ../exe/vmd_triangles_to_lines # link "
      f77 vmd_triangles_to_lines.o hole.a -o ../exe/vmd_triangles_to_lines
echo "rm vmd_triangles_to_lines.o                       # delete .o file"
      rm vmd_triangles_to_lines.o
