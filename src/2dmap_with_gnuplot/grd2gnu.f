      PROGRAM GRDGNU
      IMPLICIT NONE
C this little program is an attempt to convert the GRD type output
C from HOLE for use with SURFER into a form which is usable in gnuplot
C read from standard output, write to standard input

C checking word
      CHARACTER*4		WORD

C number of data points in x , y.
C (2nd of input)
      INTEGER			NDPX, NDPY
      
C X data limits, Y Data limits
      DOUBLE PRECISION		XLOW, XHIGH, YLOW, YHIGH

C array for slab in constant Y
C value for Y
      DOUBLE PRECISION		YCOORD, ZVALUE(100000)

C count in x
      INTEGER			XCOUNT

C xcoord value
      DOUBLE PRECISION		XCOORD

C end of decs ******************

C read first line (should be DSAA)
      READ( 5,'(A4)') WORD
      IF (WORD.NE.'DSAA') STOP 'ERROR NOT A GRD TYPE FILE'

C read the number of points, data limits
      READ( 5, *) NDPX, NDPY
      READ( 5, *) XLOW, XHIGH
      READ( 5, *) YLOW, YHIGH
C skip a line (z data lims not of interest).
      READ( 5, *) 

      DO 10 YCOORD = YLOW, YHIGH, (YHIGH-YLOW)/(NDPY-1.)
C read the z data values for each x at this coord
        READ( 5, *) (ZVALUE(XCOUNT), XCOUNT = 1, NDPX)

C now write out each triplex seperated
        DO 20 XCOUNT = 1, NDPX
          XCOORD = XLOW + (XHIGH-XLOW)*(XCOUNT-1.)/(NDPX-1.)
          WRITE( 6, *) XCOORD, YCOORD, ZVALUE(XCOUNT)
20      CONTINUE        
C seperate each "isoline" with a blank
        WRITE( 6, *)
10    CONTINUE      
     
      END
