      SUBROUTINE QPTSYB
      IMPLICIT NONE
C ********************************************************************
C *                                                                  *
C * This software is an unpublished work containing confidential and *
C * proprietary information of Birkbeck College. Use, disclosure,    *
C * reproduction and transfer of this work without the express       *
C * written consent of Birkbeck College are prohibited. This notice  *
C * must be attached to all copies or extracts of the software.      *
C *                                                                  *
C * (c) 1994 Oliver Smart & Birkbeck College, All rights reserved    *
C * (c) 1996 Oliver Smart & Birkbeck College, All rights reserved    *
C *                                                                  *
C ********************************************************************
C
C Modification history:
C
C Date	Author		Modification
C 01/94	O.S. Smart	Original version
C 10/95 O.S. Smart      Converted to s/r for qpt_conv program
C                       (bundles all small conversion programs together)
C 02/96 O.S. Smart	Adapted to follow HOLE colouring convention -
C			various objects to red, green blue etc.
C
C
C Takes hydra/quanta binary 3D plot file and converts to
C sybyl plot (.plt2) file format. Syntax of the file taken from p3121 of
C v5.4 manual. 

C the vectors
      REAL 			RVEC4(4)

C screen, keyboard
      INTEGER			NIN
      PARAMETER(		NIN = 5)
      INTEGER			NOUT
      PARAMETER(		NOUT= 6)
C input/output file stream
      INTEGER			SIN, SOUT

C dummy filename
      CHARACTER*200		FDUM

C string for text records in hydra file
      CHARACTER*80		STRING

C abort indicator
      LOGICAL			LABORT

C indicator - true have processed at least one dot/line
      LOGICAL			LFIRST

C previous co-ords
      REAL			PXYZ(3)

C colour number, colour string
      INTEGER			ICOL
      CHARACTER*20		COLSTR

C end of decs ***********

      WRITE( NOUT, '(A)') 
     &' Program qpt_sybyl. ',
     &'   Reads in a hydra/quanta 3D binary plot and writes',
     &'   out a plot file for sybyl.'

C routine to find latest file of
C type in the directory.  N.b. only works on unix machines
      CALL LASTF( FDUM, '.qpt')
      IF (FDUM(1:4).EQ.'none') FDUM = 'input'

C get input filename
      LABORT = .TRUE.
C (input stream, output, oldfile?, file_stream, file_type, name,
C  allow abort?, default extension)
C N.B. as file_type includes 'BINARY' then will open as binary
      CALL INTERF( NIN, NOUT, .TRUE., SIN, 
     &  'input binary hydra/quanta plot', FDUM, LABORT, '.qpt')
      IF (LABORT) GOTO 55555

C open output file - default id should be .plt2 to agree with sybyl
      LABORT = .TRUE.
      CALL INTERF( NIN, NOUT, .FALSE., SOUT, 
     &  'output sybyl plot', FDUM, LABORT, '.plt2')
      IF (LABORT) GOTO 55555

C write header for plot file n.b. wordview needed to avoid clipping
      WRITE ( SOUT, '(A)') 
     &'PLOT2', 
     &'SET VERSION 1.0',
     &'SET WORLDVIEW -10000. 10000. -10000. 10000. -10000. 10000.'

C start with purple         
      COLSTR = 'MAGENTA'

C will read the .qpt file TWICE
C (i) read and output any dot records
C (ii) read and output any line records

C first dots - indicate that we have not yet output any
      LFIRST = .TRUE.
10    CONTINUE
C read line from quanta file
C at end of the file then goto line 20 - finish dots and start lines
        READ( SIN, END= 20) RVEC4
C a dotat record and doing dots?
        IF (RVEC4(1).EQ.4.0) THEN
C first dot - specify colour
          IF (LFIRST) THEN
            LFIRST = .FALSE.
C Feb 1996 output dot to the current colour
            WRITE( NOUT, '(A)')
     &' File contains dot records these will be output as',
     &' object "holedot" '
            WRITE( SOUT, '(A,3F9.3,1X,A,A)') 
     &        'DRAW DOT "holedot" ', 
     &        RVEC4(2), RVEC4(3), RVEC4(4), COLSTR, ' |'
          ELSE
C just write the record
            WRITE( SOUT, '(3F9.3,1X,A,A)')
     &        RVEC4(2), RVEC4(3), RVEC4(4), COLSTR, ' |'
          ENDIF 
C text record? - do not use but must read to avoid error
        ELSEIF (RVEC4(1).EQ.5.0) THEN
C 2nd number read gives the number of characters
          READ( SIN)   STRING(1:INT(RVEC4(2)))
          WRITE( NOUT, '(A)')
     &' Warning - ignoring text record in .qpt file'//CHAR(7)

C change colour record?
        ELSEIF (RVEC4(1).EQ.1.0) THEN
C we use standard hole colour mapping:
C
C Colour #
C 1 to 14   Output normal quanta colours to magenta
C alt 15    centre line to yellow
C alt 16    low rad surf to red
C alt 17    mid rad surf to green
C alt 18    low rad surf to blue
C alt 19    capsule vectors to cyan
C alt 20    closest atom vectors to white (no grey)

C do we have a standard quanta colour or a hole alternative?            
          IF (INT(RVEC4(3)).EQ.-55) THEN
            ICOL = INT(RVEC4(4))
          ELSE
            ICOL = INT(RVEC4(2))
          ENDIF
C what colour shall we set?
          IF (ICOL.LT.15) THEN
C all normal to magenta
            COLSTR = 'MAGENTA'
          ELSEIF (ICOL.EQ.15) THEN
            COLSTR = 'YELLOW' 
          ELSEIF (ICOL.EQ.16) THEN
            COLSTR = 'RED'
          ELSEIF (ICOL.EQ.17) THEN
            COLSTR = 'GREEN'
          ELSEIF (ICOL.EQ.18) THEN
            COLSTR = 'BLUE' 
          ELSEIF (ICOL.EQ.19) THEN
            COLSTR = 'PURPLE'
          ELSE
            COLSTR = 'WHITE'
          ENDIF


        ENDIF
   
C read next line of quanta file
      GOTO 10

20    CONTINUE
C first time we have got to end of quanta file
C (i) need to make another pass (to do lines) so rewind input file
C (ii) need to end dot records if any processed
       REWIND( SIN)
C end of dot records
       IF (.NOT. LFIRST) WRITE( SOUT, '(A)') '|'
C reinitialize indicator which says whether we have output anything
       LFIRST = .TRUE.

C start with purple         
      COLSTR = 'MAGENTA'

C read .qpt file (again)
30    CONTINUE
C read line from quanta file
C at end of the file then goto line 40 - finish line records
        READ( SIN, END= 40) RVEC4
C do we have a draw to record?
C n.b. draw from pxyz to the read co-ords
        IF (RVEC4(1).EQ.3.0) THEN
C first line - specify colour
          IF (LFIRST) THEN
            LFIRST = .FALSE.
            WRITE( NOUT, '(A)')
     &' File contains line records these will be output as',
     &' object "holeline"'
C n.b. width of record is 1.0 - do not know what effect this will have
            WRITE( SOUT, '(A,7F9.3,1X,A,A)')
     &        'DRAW LINE "holeline" ',
     &        PXYZ(1), PXYZ(2), PXYZ(3), 
     &        RVEC4(2), RVEC4(3), RVEC4(4),  1.0, COLSTR,' |'
          ELSE
C just write the record
            WRITE( SOUT, '(7F9.3,1X,A,A)')
     &        PXYZ(1), PXYZ(2), PXYZ(3),
     &        RVEC4(2), RVEC4(3), RVEC4(4),  1.0, COLSTR,' |'
          ENDIF
C text record? - do not use but must read to avoid error
        ELSEIF (RVEC4(1).EQ.5.0) THEN
C 2nd number read gives the number of characters
          READ( SIN)   STRING(1:INT(RVEC4(2)))

C change colour record?
        ELSEIF (RVEC4(1).EQ.1.0) THEN
C we use standard hole colour mapping:
C
C Colour #
C 1 to 14   Output normal quanta colours to magenta
C alt 15    centre line to yellow
C alt 16    low rad surf to red
C alt 17    mid rad surf to green
C alt 18    low rad surf to blue
C alt 19    capsule vectors to cyan
C alt 20    closest atom vectors to white (no grey)

C do we have a standard quanta colour or a hole alternative?
          IF (INT(RVEC4(3)).EQ.-55) THEN
            ICOL = INT(RVEC4(4))
          ELSE
            ICOL = INT(RVEC4(2))
          ENDIF
C what colour shall we set?
          IF (ICOL.LT.15) THEN
C all normal to magenta
            COLSTR = 'MAGENTA'
          ELSEIF (ICOL.EQ.15) THEN
            COLSTR = 'YELLOW'
          ELSEIF (ICOL.EQ.16) THEN
            COLSTR = 'RED'
          ELSEIF (ICOL.EQ.17) THEN
            COLSTR = 'GREEN'
          ELSEIF (ICOL.EQ.18) THEN
            COLSTR = 'BLUE'
          ELSEIF (ICOL.EQ.19) THEN
            COLSTR = 'PURPLE'
          ELSE
            COLSTR = 'WHITE'
          ENDIF

        ENDIF

C store the coords for next line
        PXYZ(1) = RVEC4(2)
        PXYZ(2) = RVEC4(3)
        PXYZ(3) = RVEC4(4)

C read next record
        GOTO 30

C end of .qpt file (for the second time)
40      CONTINUE

C if we have done lines then end record
      IF (.NOT. LFIRST) WRITE( SOUT, '(A)') '| '

C last line of file
      WRITE( SOUT, *) 'QUIT'

55555 WRITE(NOUT,*)
      RETURN
      END
