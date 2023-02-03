#!/bin/bash


failed () {
    local msg="$1" err="${2:1}"
    echo 1>&2 "FAILED $msg"
    exit $err
}

passed () {
    local msg="$1"
    echo 1>&2 "PASSED $msg"
}

# set PREFIX to the installation prefix. The radii are
# under PREFIX/share/hole2/rad

: ${PREFIX:="/usr/local"}

BIN=$PREFIX/bin

HOLE=$BIN/hole
SPH_PROCESS=$BIN/sph_process
SOS_TRIANGLE=$BIN/sos_triangle


HOLE_INPUT=hole.inp
HOLE_OUTPUT=hole.out

SPH=hole_out.sph

# minimal HOLE input file
# see http://www.holeprogram.org/doc/index.html

cat > ${HOLE_INPUT}<<EOF
RADIUS ${PREFIX}/share/hole2/rad/simple.rad
COORD 1grm.pdb
SPHPDB ${SPH}
ENDRAD 5.
RASEED 1993
EOF

# get test file from PDB if necessary
PDB="1grm.pdb"

test -e $PDB \
    || curl https://files.rcsb.org/download/1grm.pdb.gz | gzip -dc > $PDB \\
    || failed "Failed to get PDB test file $PDB" 255

# clean up
rm *.old ${HOLE_OUTPUT} ${SPH} solid_surface.sos solid_surface.vmd_plot


$HOLE < ${HOLE_INPUT} | tee ${HOLE_OUTPUT} || failed "$HOLE failed"


diff -U2 reference/${HOLE_OUTPUT} ${HOLE_OUTPUT} || failed "HOLE output differed"
passed "hole (output)"

diff -U2 reference/${SPH} ${SPH} || failed "sph file differed"
passed "hole (sphere file)"

# vmd triangulated surface

$SPH_PROCESS -dotden 15 -color $SPH solid_surface.sos || failed "$SPH_PROCESS failed"
diff reference/solid_surface.sos solid_surface.sos || failed "solid_surface.sos binary difference"
passed "sph_process (sos file)"

$SOS_TRIANGLE -s < solid_surface.sos > solid_surface.vmd_plot
diff -U2 reference/solid_surface.vmd_plot solid_surface.vmd_plot || failed "sos_triangle (solid_surface.vmd_plot)"
passed "sos_triangle  (solid_surface.vmd_plot)"

# dot surface
$SPH_PROCESS -dotden 15 -color $SPH dotsurface.qpt || failed "sph_process (dotsurface.qpt)"
diff reference/dotsurface.qpt dotsurface.qpt || failed "sph_process (dotsurface.qpt)"
passed "sph_process (dotsurface.qpt)"

# the following does not work on macOS: (hangs because stdin is not correctly
# read by the interactive prompt)

### printf "D\n\n\n" | $BIN/qpt_conv
