      SUBROUTINE QREADI( NIN, NOUT, SIN, 
     &  MAXST,MAXCOL, ISTORE, RSTORE, 
     &  MAXTXT, MAXLEN, TXTNO, TXTLEN, TXTPOS, TXTDIR, TXTSTR, LABORT,
     &  DOTAT, SWITCH) 
      IMPLICIT NONE
C ********************************************************************
C *                                                                  *
C * This software is an unpublished work containing confidential and *
C * proprietary information of Birkbeck College. Use, disclosure,    *
C * reproduction and transfer of this work without the express       *
C * written consent of Birkbeck College are prohibited. This notice  *
C * must be attached to all copies or extracts of the software.      *
C *                                                                  *
C * (c) 1993 Oliver Smart & Birkbeck College, All rights reserved    *
C * (c) 1996 Oliver Smart & Birkbeck College, All rights reserved    *
C *                                                                  *
C ********************************************************************
C
C Modification history:
C
C Date	Author		Modification
C 12/93	O.S. Smart	Original public release in HOLE suite beta1.0
C
C
      SAVE
C this s/r is called by program qplot - deals with reading
C information from hydra/quanta plot file already opened
C to stream SIN.


C passed variables

C user input/output streams returned unchanged
      INTEGER                   NIN, NOUT

C hydra binary file stream number 
C The input file has been opened to this stream
C before this s/r is called - returned unchanged
      INTEGER                   SIN

C the store for move draws read
C MAXCOL - stores the maximum number of colours and 
C it is assigned a value that MUST not be changed
C MAXST - is the maximum number of move draws to be stored
C         this is set a parameter in qplot and MUST not be changed
C istore( 1 to MAXCOL) is returned with the number of move
C    draws read for each colour
C rstore is the store for moves/draws 
      INTEGER                   MAXST,MAXCOL
      INTEGER                   ISTORE(MAXCOL)
      REAL                      RSTORE( MAXST, 0:3, MAXCOL)
C store for text strings
C txtno             the number of strings stored
C txtlen(*)         the length of string *
C txtpos(1 to 3, *) the position of its anchor point in angs space
C txtdir(*)         an integer to indicate where the string should be
C                   placed on page 1 indicates below to the left
C                                  2 ............... centred etc.
C                   key 789
C                       456 (like a numeric keypad)
C                       123
C txtstr(*)         the string to be written
C the maximum number of strings
      INTEGER                   MAXTXT
C the maximum numbers of characters in each text string
      INTEGER                   MAXLEN
      INTEGER                   TXTNO
      INTEGER                   TXTLEN( MAXTXT)
      REAL                      TXTPOS( 3, MAXTXT)
      INTEGER                   TXTDIR( MAXTXT)
      CHARACTER*(*)             TXTSTR( MAXTXT)

C an abort indicator
C if we find a problem in this s/r return true
      LOGICAL                   LABORT

C variables internal to this routine 

C a vector to read lines from quanta file
      REAL                      QVEC(0:3)

C the colour which move/draws should be read into
C - between 1 & MAXCOL
      INTEGER                   ICOL 

C line for input
      CHARACTER*80              LINE80
C vble which states the level of difficulty to
C be applied to the sub-routine
      CHARACTER *1              SWITCH 

C Vble which passes on the information whether
C there are "dot at" records in the file:
C -55555     means dots are processed into 3d crosses in s/r qreadi
C -ve no     dots should be processed into open circles (white centre)
C 0.0        no dots in file
C +ve number the point size for circles to which dots processed
C n.b. only deal with circle option here - crosses done in s/r qreadi
      REAL                       DOTAT

C size in angstroms of a cross
C (length of line)
      REAL			SCROSS

C end of declarations ********** (declarations above, exe's below)

C default colour is one
       ICOL = 1

C should allow the user to abort by an EOF on read
       LABORT = .TRUE.

C will jump back to this line until the end of file is meet 
C when we jump to line 20 further down the program.
10    CONTINUE
C read the 4-vector from binary file
C if there is a problem got error statements at the end of this s/r
	READ( SIN, ERR= 900, END= 20) QVEC
	
C Is this a change of colour?
       IF     (QVEC(0).EQ.1.0) THEN
C quanta 3.3 produces its own title and lines at the end of the
C file - can detect this by a change to colour 1 while
C colour is set to 14
          IF ( (ICOL.EQ.14) .AND. (QVEC(1).EQ.1.0)) THEN
            IF (SWITCH.EQ.'E') THEN
C ask user whether rest of file should be read
	      WRITE( NOUT, '(A)')
     &' Detected what I think is rubbish at end of file' 
	      CALL PROMPT( NOUT,
     &' Do you want this? (y/n) <n>: ')
              READ( NIN, '(A1)') LINE80(1:1)
              IF ( (LINE80(1:1).NE.'Y') .AND. 
     &             (LINE80(1:1).NE.'y')) THEN
                LABORT = .FALSE.
                GOTO 55555  
              ENDIF
            ELSE
C amateur - default to stop read
              LABORT = .FALSE.
	      GOTO 55555
   	    ENDIF
          ENDIF
C 24/10/95 - allow alternative colours for hole objects 15 to 20
C these are indicated by a -55 as second number and are in third
          IF (INT(QVEC(2)).EQ.-55) THEN
	    ICOL = INT(QVEC(3))
          ELSE
	    ICOL = INT(QVEC(1))
          ENDIF
    

C override the colours only if the SWITCH mode
C permits this
       IF(SWITCH.EQ.'E') THEN
C change of colour read
	  WRITE( NOUT, '(A,I6)')
     &' Have read change of colour to number ', ICOL
C do you want to change the colour?
	  CALL PROMPT( NOUT, 
     &' Do you want to change the colour?'//
     &                 ' Enter new colour no. <no change>: ')
C read reply into line - to allow default
          READ( NIN, '(A)', ERR= 55555, END= 55555) LINE80 
C has the user specified a number?
	  IF (LINE80(1:6).NE.'     ') 
     &       READ( LINE80, '(BN,I8)', ERR= 55555) ICOL

C If the read colour is exceeding the boundaries (1:MAXCOL), it is reset here
	    IF   (ICOL.GT.MAXCOL) THEN
	      WRITE(NOUT,'(A,I8/,A,I8)')
     &' Exceeding the maximum colour number:'//CHAR(7),MAXCOL,
     &' I have set the colour to:',MAXCOL
	      ICOL=MAXCOL 
	    ELSEIF  (ICOL.LT.1) THEN
	      WRITE(NOUT,'(A)')
     &' The read colour is below the minimum accepted - 1'//CHAR(7),
     &' I have set the colour to: 1'
	      ICOL=1
	    ENDIF
C end of 'switch' test commands
          ENDIF
C a move to or draw to or a dot at ?
      ELSEIF ( QVEC(0).EQ.2.0 .OR. QVEC(0).EQ.3.0
     &         .OR. QVEC(0).EQ.4.0) THEN
C add one more record to store 
	  ISTORE(ICOL) = ISTORE(ICOL) + 1
C task1 test to make sure array bound not exceeded here
	  IF  (ISTORE(ICOL).GT.MAXST)  THEN
	    WRITE( NOUT ,'(A,I8/A/A)') 
     &' Exceeding the maximum moves draws allowed :'//CHAR(7),MAXST,
     &' Please increase value of MAXST and recompile the program',
     &' QPLOT aborted' 
	    LABORT=.TRUE.
	    GOTO 55555
	  ENDIF
C    end of test 
	  RSTORE( ISTORE(ICOL), 0, ICOL) = QVEC(0)
	  RSTORE( ISTORE(ICOL), 1, ICOL) = QVEC(1)
	  RSTORE( ISTORE(ICOL), 2, ICOL) = QVEC(2)
	  RSTORE( ISTORE(ICOL), 3, ICOL) = QVEC(3)

C do we have a "dot at" record?
          IF (QVEC(0).EQ.4.) THEN
            IF (DOTAT.EQ.0.) THEN
C first time dot at record encountered
C default option for dotat records is a 1 point open circle
              DOTAT = -1.5
              IF (SWITCH.EQ.'E') THEN
C does the user want to overide default?
                WRITE( NOUT, '(A)')
     &'  ',
     &' Have encountered a dotat record in file.',
     &'   How do you want dots to be output?',
     &'   Enter ''C '' for 3D cross or ''D'' for dot'
                CALL PROMPT( NOUT,
     & '  Cross or Dot? <dot>:')
                READ( NIN, '(A1)', ERR= 55555, END= 55555) LINE80(1:1)
                CALL UCASE(LINE80(1:1))
                IF (LINE80(1:1).EQ.'C') THEN
C cross
                  DOTAT = -55555.
C what size?
                  SCROSS = 0.2
                  CALL PROMPT( NOUT,
     & '  What length (in angs) do you the cross? <0.2>:')
                  READ( NIN, '(A)', ERR= 55555, END= 55555) LINE80 
C has the user specified a number?
	          IF (LINE80(1:6).NE.'     ') 
     &              READ( LINE80, '(BN,F10.0)', ERR= 55555) SCROSS
C                  write(*,*) 'debug scross ', scross
                ELSE
C want a dot what point size
                  CALL PROMPT( NOUT,
     &'  What size dots (in points)? -ve for open dots <-1.5>:')
                  READ( NIN, '(A)', ERR= 55555, END= 55555) LINE80 
C has the user specified a number?
	          IF (LINE80(1:6).NE.'     ') 
     &              READ( LINE80, '(BN,F10.0)', ERR= 55555) DOTAT 
C                  write(*,*) 'debug dotat  ', dotat 
C end of dot or cross?
                ENDIF
C end of override default option if not amateur
              ENDIF
C end of first time cross encountered
            ENDIF
C produce a cross?
            IF (DOTAT.EQ.-55555.) THEN
C draw three lines for point 
C (n.b. do not test for istore exceeding limit 
C            - a bit naughty - could be changed)
	      RSTORE( ISTORE(ICOL), 0, ICOL) = 2.0
              RSTORE( ISTORE(ICOL), 1, ICOL) = QVEC(1) - 0.5*SCROSS
              RSTORE( ISTORE(ICOL), 2, ICOL) = QVEC(2)
	      RSTORE( ISTORE(ICOL), 3, ICOL) = QVEC(3)
              ISTORE(ICOL) = ISTORE(ICOL) + 1
	      RSTORE( ISTORE(ICOL), 0, ICOL) = 3.0
              RSTORE( ISTORE(ICOL), 1, ICOL) = QVEC(1) + 0.5*SCROSS
              RSTORE( ISTORE(ICOL), 2, ICOL) = QVEC(2)
	      RSTORE( ISTORE(ICOL), 3, ICOL) = QVEC(3)
C line in y
              ISTORE(ICOL) = ISTORE(ICOL) + 1
	      RSTORE( ISTORE(ICOL), 0, ICOL) = 2.0
              RSTORE( ISTORE(ICOL), 1, ICOL) = QVEC(1)
              RSTORE( ISTORE(ICOL), 2, ICOL) = QVEC(2) - 0.5*SCROSS
	      RSTORE( ISTORE(ICOL), 3, ICOL) = QVEC(3)
              ISTORE(ICOL) = ISTORE(ICOL) + 1
	      RSTORE( ISTORE(ICOL), 0, ICOL) = 3.0
              RSTORE( ISTORE(ICOL), 1, ICOL) = QVEC(1)
              RSTORE( ISTORE(ICOL), 2, ICOL) = QVEC(2) + 0.5*SCROSS
	      RSTORE( ISTORE(ICOL), 3, ICOL) = QVEC(3)
C line in z
              ISTORE(ICOL) = ISTORE(ICOL) + 1
	      RSTORE( ISTORE(ICOL), 0, ICOL) = 2.0
              RSTORE( ISTORE(ICOL), 1, ICOL) = QVEC(1)
              RSTORE( ISTORE(ICOL), 2, ICOL) = QVEC(2) 
	      RSTORE( ISTORE(ICOL), 3, ICOL) = QVEC(3)- 0.5*SCROSS
              ISTORE(ICOL) = ISTORE(ICOL) + 1
	      RSTORE( ISTORE(ICOL), 0, ICOL) = 3.0
              RSTORE( ISTORE(ICOL), 1, ICOL) = QVEC(1)
              RSTORE( ISTORE(ICOL), 2, ICOL) = QVEC(2)
	      RSTORE( ISTORE(ICOL), 3, ICOL) = QVEC(3) + 0.5*SCROSS
            ENDIF
          ENDIF
C character record
	ELSEIF (QVEC(0).EQ.5.0) THEN
C do we have to many strings?
	  IF (TXTNO.EQ.MAXTXT) THEN
	    WRITE( NOUT, '()')
     &' Have exceed maximum no of text strings ', MAXTXT,
     &' Please increase parameter MAXTXT in program QPLOT'
	    LABORT = .TRUE.
	    GOTO 55555
	  ENDIF
	  TXTNO = TXTNO + 1
C the second record read (qvec(1)) is the 
C number of characters in the string
	  TXTLEN(TXTNO) = NINT(QVEC(1))
C the position for the string is the last move/draw record read
	  TXTPOS(1,TXTNO) = RSTORE( ISTORE(ICOL), 1, ICOL)
	  TXTPOS(2,TXTNO) = RSTORE( ISTORE(ICOL), 2, ICOL)
	  TXTPOS(3,TXTNO) = RSTORE( ISTORE(ICOL), 3, ICOL)
C the integer control on the direction to place text relative
C to ancor point is read as QVEC(3)
	  TXTDIR(TXTNO) = NINT(QVEC(3))
C read string from binary file
	  READ( SIN, ERR= 900) TXTSTR(TXTNO)(1:TXTLEN(TXTNO))
C quanta files have rubbish at the eof - try to detect above
C but if that fails detected by the fact that 
C a one character blank string is specified
          IF ( (TXTLEN(TXTNO).EQ.1) .AND. 
     &         (TXTSTR(TXTNO)(1:1).EQ.' ') ) THEN
            IF (SWITCH.EQ.'E') THEN
C ask user whether rest of file should be read
              WRITE( NOUT, '(A)')
     &' Detected what I think is rubbish at end of file (blank string)'
              CALL PROMPT( NOUT,
     &' Do you want this? (y/n) <n>: ')
              READ( NIN, '(A1)') LINE80(1:1)
              IF ( (LINE80(1:1).NE.'Y') .AND.
     &             (LINE80(1:1).NE.'y')) THEN
C ignore the last string
                TXTNO = TXTNO - 1
                LABORT = .FALSE.
                GOTO 55555
              ENDIF
            ELSE
C amateur - default to stop read
C ignore the last string
              TXTNO = TXTNO - 1
              LABORT = .FALSE.
              GOTO 55555
            ENDIF
          ENDIF



C 16 record - rgb for iris no use to us
	ELSEIF (QVEC(0).EQ.16.0) THEN
          CONTINUE
	ELSE
C do not understand record
	  WRITE( NOUT, '(A/ 4F12.3, A)')
     &' Do not recognize record read from quanta file: ',
     & QVEC,
     &' Will ignore & try to proceed'//CHAR(7)
	ENDIF
C read next record
	GOTO 10
C got to the end of input file
20    CONTINUE

C normal return no error
      LABORT = .FALSE.

55555 RETURN
900   CONTINUE
C include bleep in error message with char(7)
      WRITE( NOUT, '(A)')
     &' ***error***'//CHAR(7),
     &' On reading from quanta/hydra binary file',
     &' Format wrong?',
     &' Aborting'
      LABORT = .TRUE.
      END
