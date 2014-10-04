      PROGRAM BLNGNU
      IMPLICIT NONE
C this little program is an attempt to convert the BLN type output
C from HOLE for use with SURFER into a form which is usable in gnuplot
C read from standard output, write to standard input

      CHARACTER*80		LINE

C end of decs ******************

C all we have to do is ignore every third line and write
C out the rest with zero appended

C jump up to here on read
10    CONTINUE

C read input line
        READ( 5,'(A)', END= 55) LINE
        WRITE(6,*)
        WRITE(6,*)
        READ( 5,'(A)', END= 55) LINE
        WRITE( 6, '(A,A)') LINE(1:INDEX(LINE,'      ')+2), ' 0.0'
        READ( 5,'(A)', END= 55) LINE
        WRITE( 6, '(A,A)') LINE(1:INDEX(LINE,'      ')+2), ' 0.0'
C read next triplet
      GOTO 10

55    CONTINUE
      END
