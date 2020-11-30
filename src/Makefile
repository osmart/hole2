#
#
# Makefile for hole interfaces (based on TRON versions)
# (updated to function with different Fortran compilers and
# add 'make install' target)

# installation directory
PREFIX  := /usr/local#
BIN_DIR := $(PREFIX)/bin#
DATA_DIR:= $(PREFIX)/share/hole2#



# environmental vbles to be set before make invoked with this file
#     $FC set to fortran compilier e.g., ifc or f90
#     $ARCH set to architecture e.g. linuxi
#     $OPTFLAGS set to complier flags
#     OPTFLAGS = -O -c -w -mp1 -r8 
#
FC := gfortran
CC := gcc
FFLAGS := -fd-lines-as-comments -fbackslash -std=legacy 
# for linux we need -static, OSX does not
UNAME := $(shell uname)
# linux needs -static but OSX needs -static-libgfortran
ifeq ($(UNAME), Linux)
   FFLAGS +=  -static
endif
ifeq ($(UNAME), Darwin)
   FFLAGS +=  -static-libgfortran
endif
CFLAGS := -static-libgfortran
MACHINE_DEP := machine_dep.g77
LIB_NAME := hole.a
AR := ar -rcv
RANLIB := ranlib
#L_ARCH   = $(ARCH)
#LIB_NAME = d-$(L_ARCH).a
#FFLAGS = $(OPTFLAGS)

FILES :=  addend.o \
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

# vertim.f is generated during compilation (see makver.exe below)
FILES += vertim.o

PROGRAMS       :=  hole sph_process sos_triangle qpt_conv
OPT_PROGRAMS   :=  labqpt qplot vdwdot vmd_triangles_to_lines
2DMAP_PROGRAMS :=  make_post_map bln2gnu make_post2gnu capost2gnu grd2gnu

EXE_D   := ../exe#
2DMAP_D := $(EXE_D)/2dmap#

# The original philosophy of this package is to have the binaries
# directly compiled into the exe directory; keep this for the
# time being...
EXE_PROGRAMS      := $(addprefix $(EXE_D)/,$(PROGRAMS))
EXE_OPT_PROGRAMS  := $(addprefix $(EXE_D)/,$(OPT_PROGRAMS))
EXE_2DMAP_PROGRAMS:= $(addprefix $(2DMAP_D)/,$(2DMAP_PROGRAMS)) 


# data file (for DATA_DIR)
DATA := ../rad

.phony: all exe src clean dist-clean

all: 	exe \
	src \
	makver.exe \
	$(EXE_PROGRAMS) \
	$(EXE_OPT_PROGRAMS) \
	$(EXE_2DMAP_PROGRAMS)

exe: $(EXE_D) $(2DMAP_D)

$(EXE_D):
	mkdir $@
$(2DMAP_D):
	mkdir $@

src: $(LIB_NAME)

$(LIB_NAME): $(FILES) 
	$(AR) $(LIB_NAME) $(FILES)  
	$(RANLIB) $(LIB_NAME)
.f.o:
	$(FC) $(FFLAGS) -c $*.f
	$(AR) $(LIB_NAME) $*.o

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
	- rm machine_dep.f vertim.f makver.temp_file
	- rm -f makver.exe

dist-clean: clean
	- rm -f                   \
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



machine_dep.f:
	ln -s $(MACHINE_DEP) machine_dep.f

makver.exe: makver.f
	$(FC) makver.f -o makver.exe

# makver.exe uses environment variable FC to set Fortran compiler
# (note: 256 character limit to the path length of FC)
RUN_MAKVER := FC=$(FC) ./makver.exe

vertim.f: makver.exe
	$(RUN_MAKVER)
	@echo "$<: Generated '$@'"

# vertim.o is generated together with vertim.f
vertim.o: vertim.f

# compilation of vertim.f was hard coded in makver.exe so I have to
# fudge it here to make it work for non-gfortran compilers
#vertim.o: vertim.f
#	$(FC) $(FFLAGS) -c vertim.f


../exe/labqpt: $(FILES)
	$(RUN_MAKVER)
	$(FC)  $(FFLAGS) $(LFLAGS) labqpt.o $(LIB_NAME) -o ../exe/labqpt

../exe/hole: $(FILES)
	$(RUN_MAKVER)
	$(FC)  $(FFLAGS) $(LFLAGS) hole.o $(LIB_NAME) -o ../exe/hole

../exe/qpt_conv:  $(FILES)
	$(RUN_MAKVER)
	$(FC)  $(FFLAGS) $(LFLAGS)qpt_conv.o $(LIB_NAME) -o ../exe/qpt_conv

../exe/qplot: $(FILES)
	$(RUN_MAKVER)
	$(FC)  $(FFLAGS) $(LFLAGS) qplot.o $(LIB_NAME) -o ../exe/qplot

../exe/sph_process: $(FILES)
	$(RUN_MAKVER)
	$(FC)  $(FFLAGS) $(LFLAGS) sph_process.o $(LIB_NAME) -o ../exe/sph_process

../exe/vdwdot: $(FILES)
	$(RUN_MAKVER)
	$(FC)  $(FFLAGS) $(LFLAGS) vdwdot.o $(LIB_NAME) -o ../exe/vdwdot

../exe/vmd_triangles_to_lines: $(FILES)
	$(RUN_MAKVER)
	$(FC)  $(FFLAGS) $(LFLAGS)vmd_triangles_to_lines.o $(LIB_NAME) -o ../exe/vmd_triangles_to_lines

../exe/sos_triangle: sos_triangle.c
	$(CC) $(CFLAGS) $(LFLAGS) sos_triangle.c -O2 -o  ../exe/sos_triangle -lm 

../exe/2dmap/make_post_map: $(FILES)
	$(RUN_MAKVER)
	$(FC)  $(FFLAGS) $(LFLAGS) make_pmap.o $(LIB_NAME) -o ../exe/2dmap/make_post_map

../exe/2dmap/bln2gnu: $(FILES) 2dmap_with_gnuplot/bln2gnu.f
	$(FC)  $(FFLAGS) $(LFLAGS) 2dmap_with_gnuplot/bln2gnu.f $(LIB_NAME) -o ../exe/2dmap/bln2gnu

../exe/2dmap/make_post2gnu: $(FILES) 2dmap_with_gnuplot/make_post2gnu.f
	$(FC)  $(FFLAGS) $(LFLAGS) 2dmap_with_gnuplot/make_post2gnu.f $(LIB_NAME) -o ../exe/2dmap/make_post2gnu

../exe/2dmap/capost2gnu: $(FILES) 2dmap_with_gnuplot/capost2gnu.f
	$(FC)  $(FFLAGS) $(LFLAGS) 2dmap_with_gnuplot/capost2gnu.f $(LIB_NAME) -o ../exe/2dmap/capost2gnu

../exe/2dmap/grd2gnu: $(FILES) 2dmap_with_gnuplot/grd2gnu.f
	$(FC)  $(FFLAGS) $(LFLAGS) 2dmap_with_gnuplot/grd2gnu.f $(LIB_NAME) -o ../exe/2dmap/grd2gnu

.phony: install install-data install-opt install-2dmap install-all

install-all: install install-data install-opt install-2dmap

install: $(EXE_PROGRAMS) install-data
	@echo "Installing standard HOLE binaries in $(BIN_DIR)"
	mkdir -p $(BIN_DIR)
	for f in $(EXE_PROGRAMS); do \
	  install -m 755 $$f $(BIN_DIR); \
	done;

install-data: $(DATA)
	@echo "Installing HOLE data files in $(DATA_DIR)"
	mkdir -p $(DATA_DIR)
	cp -R $(DATA) $(DATA_DIR)

install-opt: $(EXE_OPT_PROGRAMS)
	@echo "Installing optional HOLE binaries in $(BIN_DIR)"
	mkdir -p $(BIN_DIR)
	for f in $^; do \
	  install -m 755 $$f $(BIN_DIR); \
	done;

install-2dmap: $(EXE_2DMAP_PROGRAMS)
	@echo "Installing optional 2dmap HOLE binaries in $(BIN_DIR)"
	mkdir -p $(BIN_DIR)
	for f in $^; do \
	  install -m 755 $$f $(BIN_DIR); \
	done;

