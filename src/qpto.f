      SUBROUTINE QPTO
      IMPLICIT NONE
C ********************************************************************
C *                                                                  *
C * This software is an unpublished work containing confidential and *
C * proprietary information of Birkbeck College. Use, disclosure,    *
C * reproduction and transfer of this work without the express       *
C * written consent of Birkbeck College are prohibited. This notice  *
C * must be attached to all copies or extracts of the software.      *
C *                                                                  *
C * (c) 1996 Oliver Smart & Birkbeck College, All rights reserved    *
C *                                                                  *
C ********************************************************************
C
C Modification history:
C
C Date	Author		Modification
C 02/96	O.S. Smart	Original version
C
C
C Takes hydra/quanta binary 3D plot file and converts to
C an O object file.

C the vectors
      REAL 			RVEC4(4)

C screen, keyboard
      INTEGER			NIN
      PARAMETER(		NIN = 5)
      INTEGER			NOUT
      PARAMETER(		NOUT= 6)
C input/output file stream
      INTEGER			SIN, SOUT

C input/output filename
      CHARACTER*200		FIN, FOUT

C string for text records in hydra file
      CHARACTER*80		STRING

C abort indicator
      LOGICAL			LABORT

C indicator - true have processed at least one dot/line
      LOGICAL			LFIRST


C we should output the current records
      LOGICAL			LVALID

C current colour being output
      INTEGER			CURCOL

C pass count
      INTEGER			IPASS

C end of decs ***********

      WRITE( NOUT, '(A)') 
     &' S/r qpto. ',
     &'   Reads in a hydra/quanta 3D binary plot and writes',
     &'   out an O equivalent.'

C routine to find latest file of
C type in the directory.  N.b. only works on unix machines
      CALL LASTF( FIN, '.qpt')
      IF (FIN(1:4).EQ.'none') FIN = 'input'

C get input filename
      LABORT = .TRUE.
C (input stream, output, oldfile?, file_stream, file_type, name,
C  allow abort?, default extension)
C N.B. as file_type includes 'BINARY' then will open as binary
      CALL INTERF( NIN, NOUT, .TRUE., SIN, 
     &  'input binary hydra/quanta plot', FIN, LABORT, '.qpt')
      IF (LABORT) GOTO 55555

      FOUT = FIN
      LABORT = .TRUE.
      CALL INTERF( NIN, NOUT, .FALSE., SOUT, 
     &  'output O object file', FOUT, LABORT, '.obo')
      IF (LABORT) GOTO 55555

C write header for plot file 
      WRITE ( SOUT, '(A)') 
     &'begin hole'

C go thru the qpt file seven times
C (this has been adapted from qptkin)
C Pass  Colour #
C 1 	1 to 14	  Output normal quanta colours to magenta
C 2     alt 15    centre line to yellow 
C 3     alt 16    low rad surf to red
C 4     alt 17    mid rad surf to green
C 5     alt 18    low rad surf to red
C 6     alt 19    capsule vectors to cyan
C 7     alt 20    closest atom vectors to gray
      DO 25 IPASS = 1, 7
C only output header if any objects of that colour picked up
C default colour is 1
        CURCOL = 1
C rewind file for new  read
        REWIND(SIN)
C n.b. default will not output any objects before change to
C colour number
        LVALID = .FALSE.
        LFIRST = .TRUE.

C read line from quanta file
C at end of the file then goto line 25 read file for next colour 
10      CONTINUE
          READ( SIN, END= 25) RVEC4
C change to colour record?
          IF (RVEC4(1).EQ.1.0) THEN
C store colour number
C allow alternative colours for hole objects 15 to 20
C these are indicated by a -55 as second number and are in third
            IF (INT(RVEC4(3)).EQ.-55) THEN
	      CURCOL = INT(RVEC4(4))
            ELSE
	      CURCOL = INT(RVEC4(2))
            ENDIF

C is the colour valid for this pass?
C normal quanta plot objects
            IF ((IPASS.EQ.1).AND.(CURCOL.LT.15)) THEN
              LVALID = .TRUE.
C is this the first time for this colour?
              IF (LFIRST) WRITE ( SOUT, '(A)') 
     &'colour purple'
              LFIRST = .FALSE.
            ELSEIF ((IPASS.EQ.2).AND.(CURCOL.EQ.15)) THEN
              LVALID = .TRUE.
              IF (LFIRST) WRITE ( SOUT, '(A)') 
     &'colour yellow'
              LFIRST = .FALSE.                                 
            ELSEIF ((IPASS.EQ.3).AND.(CURCOL.EQ.16)) THEN
              LVALID = .TRUE.
              IF (LFIRST) WRITE ( SOUT, '(A)') 
     &'colour red'
              LFIRST = .FALSE.
            ELSEIF ((IPASS.EQ.4).AND.(CURCOL.EQ.17)) THEN
              LVALID = .TRUE.
              IF (LFIRST) WRITE ( SOUT, '(A)') 
     &'colour green'
              LFIRST = .FALSE.
            ELSEIF ((IPASS.EQ.5).AND.(CURCOL.EQ.18)) THEN
              LVALID = .TRUE.
              IF (LFIRST) WRITE ( SOUT, '(A)')
     &'colour blue'
              LFIRST = .FALSE.
            ELSEIF ((IPASS.EQ.5).AND.(CURCOL.EQ.19)) THEN
              LVALID = .TRUE.
              IF (LFIRST) WRITE ( SOUT, '(A)')
     &'colour purple'
              LFIRST = .FALSE.
            ELSEIF ((IPASS.EQ.5).AND.(CURCOL.EQ.20)) THEN
              LVALID = .TRUE.
              IF (LFIRST) WRITE ( SOUT, '(A)') 
     &'colour gray'
              LFIRST = .FALSE.                                 
            ELSE
C do not output this object
              LVALID = .FALSE.
            ENDIF

C a dotat record and current colour is valid?
          ELSEIF ((RVEC4(1).EQ.4.0) .AND. LVALID) THEN
            WRITE( SOUT, '(A,3F9.3)') 
     &        'dot ', RVEC4(2)-0.1, RVEC4(3), RVEC4(4)

C a moveto record and current colour is valid?
          ELSEIF ((RVEC4(1).EQ.2.0) .AND. LVALID) THEN
            WRITE( SOUT, '(A,3F9.3)')
     &        'move ', RVEC4(2), RVEC4(3), RVEC4(4)

C a moveto record and current colour is valid?
          ELSEIF ((RVEC4(1).EQ.3.0) .AND. LVALID) THEN
            WRITE( SOUT, '(A,3F9.3)')
     &        'line ', RVEC4(2), RVEC4(3), RVEC4(4)

C text record? - do not use but must read to avoid error
        ELSEIF (RVEC4(1).EQ.5.0) THEN
C 2nd number read gives the number of characters
          READ( SIN)   STRING(1:INT(RVEC4(2)))
          WRITE( NOUT, '(A)')
     &' Warning - ignoring text record in .qpt file'//CHAR(7)

        ENDIF
   
C read next line of quanta file
        GOTO 10

C end of pass ipass - 
25    CONTINUE

C write final line
      WRITE( SOUT, '(A)') 'end_object'


55555 WRITE(NOUT,*)
      RETURN
      END
