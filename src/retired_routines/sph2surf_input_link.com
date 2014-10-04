# sph2surf_input_link.com 
echo
echo "# link sph2surf_input ***"
echo "makver.exe # update link time subroutine"
makver.exe
echo "ar x hole.a sph2surf_input.o              # get sph2surf_input.o from archive"
ar x hole.a sph2surf_input.o
echo "f77 sph2surf_input.o hole.a -o ../exe/sph2surf_input # link "
f77 sph2surf_input.o hole.a -o ../exe/sph2surf_input
echo "rm sph2surf_input.o                       # delete .o file"
rm sph2surf_input.o
