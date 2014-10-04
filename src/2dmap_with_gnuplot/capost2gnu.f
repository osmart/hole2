      PROGRAM CAPGNU
      IMPLICIT NONE

C label coords
      DOUBLE PRECISION		LABXY(2)

C the label, its last character
      CHARACTER*5		LABEL
      INTEGER			IEND

C end of decs ******************

      WRITE( 6, '(A)')
     &'# Output of of program capost_gnu',
     &'# this little program is an attempt to convert the DAT type',
     &'# output from HOLE (filename root_capost.dat) ',
     &'# (for use with SURFER) into a form which is usable in gnuplot.',
     &'# read from standard output, write to standard input',
     &'# to use output file in gnuplot use the',
     &'# load "filename"',
     &'# command'
     
C first line is to be ignored
      READ( 5, *)
 

C jump back up to here after each read/write - except on EOF
10    CONTINUE
C read input (from standard input stream but this will be redirected to file)
        READ(5, '(2F12.3,5X,A5)', END= 55) LABXY, LABEL
C strip of leading blanks
        CALL STRIPS( LABEL)
        CALL CHREND( LABEL, IEND)
        WRITE( 6, '(A,A,A,F8.3,A,F8.3,A)') 
     &'set label "', LABEL(1:IEND),'" at ', LABXY(1), ' , ', 
     &                             LABXY(2), ' , 0 left'
        GOTO 10

55    CONTINUE
      END
