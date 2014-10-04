# make_pmap_link.com 
echo
echo "# link make_pmap ***"
echo "makver.exe # update link time subroutine"
makver.exe
echo "ar x hole.a make_pmap.o              # get make_pmap.o from archive"
ar x hole.a make_pmap.o
echo "f77 make_pmap.o hole.a -o ../exe/make_post_map # link "
f77 make_pmap.o hole.a -o ../exe/make_post_map
echo "rm make_pmap.o                       # delete .o file"
rm make_pmap.o
