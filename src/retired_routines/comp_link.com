#!/bin/ksh 
# get rid of any existing machine_dep.f and object archive
rm machine_dep.f hole.a makver.exe
echo   "Please enter machine type:"
echo   "  \"s\" for Silicon Graphics or Dec"
echo   "  \"l\" for linux pc with g77 setup"
echo   "  \"a\" for linux alpha with compac fort setup"
echo   "  \"h\" for Hewlett Packard "
echo   "  \"i\" for IBM rs6000 series"
echo   " (The HP and IBM options have not been tested for this release"
echo   "   as I no longer have access to these machine types - OSS)"
echo   "Enter machine type <stop script>: \c"
read MACHINETYPE
# have to copy correct machine dependent codes for setup
case $MACHINETYPE in
    s* | S*) 
	echo "(reply Silicon Graphics)" 
        echo "ln -s machine_dep.sg machine_dep.f" 
	ln -s machine_dep.sg machine_dep.f 	;;
    l* | L*) 
	echo "(reply Linux)" 
        echo "ln -s machine_dep.g77 machine_dep.f" 
	ln -s machine_dep.g77 machine_dep.f 	
        if [  -f /usr/bin/g77 ]  
        then 
           echo "ln -s /usr/bin/g77 f77"
           ln -s /usr/bin/g77 f77
         else
           echo "ERROR cannot find g77 compilier"
           exit 0
        fi
						;;
    a* | a*)
        echo "(reply alpha)"
        echo "ln -s machine_dep.sg machine_dep.f"
        ln -s machine_dep.sg machine_dep.f      
        echo " "
	echo "(For alpha use a script for f77) "
        echo "ln -s f77_alpha_linux_script f77"
        ln -s f77_alpha_linux_script f77      
        echo "cd 2dmap_with_gnuplot"
	cd 2dmap_with_gnuplot
        echo "ln -s f77_alpha_linux_script f77"
        ln -s f77_alpha_linux_script f77      
        echo "cd .."
	cd .. 
	;;

    h* | H*) 
	echo "(reply Hewlett Packard)" 
        echo "ln -s machine_dep.hp machine_dep.f" 
	ln -s machine_dep.hp machine_dep.f 	;;
    i* | I*) 
	echo "(reply IBM)" 
        echo "ln -s machine_dep.rs6000 machine_dep.f" 
	ln -s machine_dep.rs6000 machine_dep.f 	;;
    *)       exit 1  ;;
esac
echo "# Script to compile and link all programs in HOLE suite"
echo " "
f77 makver.f -o makver.exe
# check for the existance of makver.exe
if [  -f makver.exe ]  
then :
else
  echo "ERROR makver.exe not produced - complier not found?"
  exit 0
fi
#
# compile all fortran files (for hp's add +e flag)
case $MACHINETYPE in
    h* | H*)
    echo "f77 -c -O +e *.f      # compile all fortran files HP ADD +e FLAG"
    f77 -c -O +e *.f 
    echo "cc -c hp_flush.c      # c function need to flush stdout"
    cc -c hp_flush.c ;;
# not hp do normally
    *)       
    echo "f77 -c -O *.f      # compile all fortran files (non-HP)"
    f77 -c -O *.f ;;
esac
echo "ar rv hole.a *.o   # put all .o files in an archive"
ar rv hole.a *.o
ranlib hole.a
echo "rm *.o             # remove all .o files"
rm *.o 
# 
# now do linking with individual .com files (leave echoing to them)
./hole_link.com
./labqpt_link.com
./make_pmap_link.com
./qplot_link.com
./qpt_conv_link.com
./sphqpt_link.com
./vdwdot_link.com
./sos_triangle_link.com
./check_number_of_atoms_in_pdb_file_link.com 
./vmd_triangles_to_lines_link.com
# also do gnuplot utils
echo "cd 2dmap_with_gnuplot"
cd 2dmap_with_gnuplot
echo "compile_and_link_script"
compile_and_link_script
echo "cd .."
cd ..
