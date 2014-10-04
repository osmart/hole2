#
# Makefile for hole interfaces (based on TRON versions)
#
# environmental vbles to be set before make invoked with this file
#     $FC set to fortran compilier e.g., ifc or f90
#     $ARCH set to architecture e.g. linuxi
#     $OPTFLAGS set to complier flags
#     OPTFLAGS = -O -c -w -mp1 -r8 
#
FC=g77
CC=gcc
FFLAGS = -static
CFLAGS = -static
MACHINE_DEP=machine_dep.g77
LIB_NAME = hole.a
AR = ar -rcv
#L_ARCH   = $(ARCH)
#LIB_NAME = d-$(L_ARCH).a
#FFLAGS = $(OPTFLAGS)

FILES =  addend.o \
calper.o \
cguess.o \
cirova.o \
coarea.o \
concal.o \
freda.o \
get_rec_commands.o \
h2dmap.o \
hcapen.o \
hcapgr.o \
helefi.o \
hocapd.o \
hocapr.o \
hodotb.o \
hodotc.o \
hodotu.o \
hograp.o \
holcal.o \
holeen.o \
hole.o \
hollin.o \
holset.o \
homulf.o \
honewp.o \
honewv.o \
hopegg.o \
horadr.o \
horchr.o \
hosetg.o \
hsbxen.o \
hsbxmi.o \
hsurfp.o \
hydasc.o \
labqpt.o \
lastf.o \
linter.o \
lpause.o \
machine_dep.o \
make_pmap.o \
newop.o \
peg_writeall_header.o \
ptgen.o \
qgetbi.o \
qhsp.o \
qplot.o \
qpspic.o \
qpswr.o \
qpt_conv.o \
qptdot.o \
qptins.o \
qptkin.o \
qpto.o \
qptras.o \
qptspl.o \
qptsyb.o \
qptvmd.o \
qptvrm.o \
qreadi.o \
qusemx.o \
ratcal.o \
raten.o \
ratesc.o \
rcontr.o \
readse.o \
sbdclo.o \
sbgen.o \
sbgrow.o \
sphpeg.o \
sph_process.o \
sph_process_read.o \
sphqpc.o \
sphqpu.o \
sphtri.o \
ssafn2.o \
textat.o \
trisphere.o \
tsatr.o \
tsradr.o \
ut_strings.o \
ut_vector.o \
vdrcon.o \
vdset.o \
vdwdot.o \
vmd_triangles_to_lines.o \
whatu.o \
wmolqp.o \
wpdbsp.o 

all: src                           \
     makver.exe                    \
     ../exe/hole                   \
     ../exe/labqpt                 \
     ../exe/qpt_conv               \
     ../exe/qplot                  \
     ../exe/sph_process            \
     ../exe/vdwdot                 \
     ../exe/vmd_triangles_to_lines \
     ../exe/sos_triangle           \
     ../exe/sph_process            \
     ../exe/2dmap/make_post_map    \
     ../exe/2dmap/bln2gnu          \
     ../exe/2dmap/make_post2gnu    \
     ../exe/2dmap/capost2gnu       \
     ../exe/2dmap/grd2gnu


src: $(FILES) 
#	$(AR) $(LIB_NAME) $(FILES)  

clean:
	- rm -f *.o
	- rm -f *.a
	- rm machine_dep.f vertim.f
	- rm -f                   \
	     makver.exe           \
	     ../exe/hole          \
	     ../exe/labqpt        \
	     ../exe/qpt_conv      \
	     ../exe/sph_process   \
	     ../exe/sos_triangle  \
	     ../exe/qplot         \
	     ../exe/vmd_triangles_to_lines \
	     ../exe/vdwdot         
	- rm -f                   \
	     ../exe/2dmap/* 

.f.o:
	$(FC) $(FFLAGS) -c $*.f
	$(AR) $(LIB_NAME) $*.o

machine_dep.f:
	ln -s $(MACHINE_DEP) machine_dep.f

makver.exe: makver.f
	$(FC) makver.f -o makver.exe

../exe/labqpt: $(FILES)
	./makver.exe
	$(FC)  $(FFLAGS) labqpt.o $(LIB_NAME) -o ../exe/labqpt

../exe/hole: $(FILES)
	./makver.exe
	$(FC)  $(FFLAGS) hole.o $(LIB_NAME) -o ../exe/hole

../exe/qpt_conv:  $(FILES)
	./makver.exe
	$(FC)  $(FFLAGS) qpt_conv.o $(LIB_NAME) -o ../exe/qpt_conv

../exe/qplot: $(FILES)
	./makver.exe
	$(FC)  $(FFLAGS) qplot.o $(LIB_NAME) -o ../exe/qplot

../exe/sph_process: $(FILES)
	./makver.exe
	$(FC)  $(FFLAGS) sph_process.o $(LIB_NAME) -o ../exe/sph_process

../exe/vdwdot: $(FILES)
	./makver.exe
	$(FC)  $(FFLAGS) vdwdot.o $(LIB_NAME) -o ../exe/vdwdot

../exe/vmd_triangles_to_lines: $(FILES)
	./makver.exe
	$(FC)  $(FFLAGS) vmd_triangles_to_lines.o $(LIB_NAME) -o ../exe/vmd_triangles_to_lines

../exe/sos_triangle: sos_triangle.c
	$(CC) $(CFLAGS) sos_triangle.c -O2 -o  ../exe/sos_triangle -lm 

../exe/2dmap/make_post_map: $(FILES)
	./makver.exe
	$(FC)  $(FFLAGS) make_pmap.o $(LIB_NAME) -o ../exe/2dmap/make_post_map

../exe/2dmap/bln2gnu: $(FILES) 2dmap_with_gnuplot/bln2gnu.f
	$(FC)  $(FFLAGS) 2dmap_with_gnuplot/bln2gnu.f $(LIB_NAME) -o ../exe/2dmap/bln2gnu

../exe/2dmap/make_post2gnu: $(FILES) 2dmap_with_gnuplot/make_post2gnu.f
	$(FC)  $(FFLAGS) 2dmap_with_gnuplot/make_post2gnu.f $(LIB_NAME) -o ../exe/2dmap/make_post2gnu

../exe/2dmap/capost2gnu: $(FILES) 2dmap_with_gnuplot/capost2gnu.f
	$(FC)  $(FFLAGS) 2dmap_with_gnuplot/capost2gnu.f $(LIB_NAME) -o ../exe/2dmap/capost2gnu

../exe/2dmap/grd2gnu: $(FILES) 2dmap_with_gnuplot/grd2gnu.f
	$(FC)  $(FFLAGS) 2dmap_with_gnuplot/grd2gnu.f $(LIB_NAME) -o ../exe/2dmap/grd2gnu


