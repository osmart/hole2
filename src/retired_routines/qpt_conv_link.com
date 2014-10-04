# qpt_conv_link.com 
echo
echo "# link qpt_conv ***"
echo "makver.exe # update link time subroutine"
makver.exe
echo "ar x hole.a qpt_conv.o              # get qpt_conv.o from archive"
ar x hole.a qpt_conv.o
echo "f77 qpt_conv.o hole.a -o ../exe/qpt_conv # link "
f77 qpt_conv.o hole.a -o ../exe/qpt_conv
echo "rm qpt_conv.o                       # delete .o file"
rm qpt_conv.o
