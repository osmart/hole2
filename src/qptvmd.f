      SUBROUTINE QPTVMD
      IMPLICIT NONE
C ********************************************************************
C *                                                                  *
C * This software is an unpublished work containing confidential and *
C * proprietary information of Oliver Smart. Use, disclosure,    *
C * reproduction and transfer of this work without the express       *
C * written consent of Oliver Smart are prohibited. This notice  *
C * must be attached to all copies or extracts of the software.      *
C *                                                                  *
C * (c) 2000 Oliver Smart *
C *                                                                  *
C ********************************************************************
C
C s/r to perform connolly type surface calc within hole
C
C Modification history:
C
C Date  Author          Modification
C 11/00 O.S.S.		First version
C
C Takes hydra/quanta binary 3D plot file and converts to
C an VMD FILE

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

C previous co-ords
      REAL			PXYZ(3)

C line width
      INTEGER			WIDTH


C end of decs ***********

      WRITE( NOUT, '(A)') 
     &' S/r qptvmd. ',
     &'   Reads in a hydra/quanta 3D binary plot and writes',
     &'   out an VMD equivalent.  To use this file in VMD type:',
     &' source blah.vmd_plot',
     &'   at the vmd prompt'

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
     &  'vmd format file', FOUT, LABORT, '.vmd_plot')
      IF (LABORT) GOTO 55555

C prompt for the linewidth
C ask for cross bar size
      CALL PROMPT( NOUT,
     &'What width do you want lines to appear <1>: ')
      READ( NIN, '(A)', ERR=55555) STRING
      IF (STRING(1:5).NE.'     ') THEN
        READ( STRING,'(BN,I10.0)') WIDTH
      ELSE
        WIDTH = 1
      ENDIF
      CALL VTCLEAR( NOUT)



C write header for plot file 
      WRITE ( SOUT, '(A)') 
     &'draw delete all',
     &'draw materials off'

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
     &'draw color purple'
              LFIRST = .FALSE.
            ELSEIF ((IPASS.EQ.2).AND.(CURCOL.EQ.15)) THEN
              LVALID = .TRUE.
              IF (LFIRST) WRITE ( SOUT, '(A)') 
     &'draw color yellow'
              LFIRST = .FALSE.                                 
            ELSEIF ((IPASS.EQ.3).AND.(CURCOL.EQ.16)) THEN
              LVALID = .TRUE.
              IF (LFIRST) WRITE ( SOUT, '(A)') 
     &'draw color red'
              LFIRST = .FALSE.
            ELSEIF ((IPASS.EQ.4).AND.(CURCOL.EQ.17)) THEN
              LVALID = .TRUE.
              IF (LFIRST) WRITE ( SOUT, '(A)') 
     &'draw color green'
              LFIRST = .FALSE.
            ELSEIF ((IPASS.EQ.5).AND.(CURCOL.EQ.18)) THEN
              LVALID = .TRUE.
              IF (LFIRST) WRITE ( SOUT, '(A)')
     &'draw color blue'
              LFIRST = .FALSE.
            ELSEIF ((IPASS.EQ.5).AND.(CURCOL.EQ.19)) THEN
              LVALID = .TRUE.
              IF (LFIRST) WRITE ( SOUT, '(A)')
     &'draw color purple'
              LFIRST = .FALSE.
            ELSEIF ((IPASS.EQ.5).AND.(CURCOL.EQ.20)) THEN
              LVALID = .TRUE.
              IF (LFIRST) WRITE ( SOUT, '(A)') 
     &'draw color gray'
              LFIRST = .FALSE.                                 
            ELSE
C do not output this object
              LVALID = .FALSE.
            ENDIF

C a dotat record and current colour is valid?
          ELSEIF ((RVEC4(1).EQ.4.0) .AND. LVALID) THEN
            WRITE( SOUT, '(A,3F9.3,A)') 
     &        'draw point {', RVEC4(2), RVEC4(3), RVEC4(4),'}'

C a moveto record and current colour is valid?
! do nothing - 
!          ELSEIF ((RVEC4(1).EQ.2.0) .AND. LVALID) THEN
!            WRITE( SOUT, '(A,3F9.3)')
!     &        'move ', RVEC4(2), RVEC4(3), RVEC4(4)

C a moveto record and current colour is valid?
          ELSEIF ((RVEC4(1).EQ.3.0) .AND. LVALID) THEN
            WRITE( SOUT, '(A,3F9.3,A,3F9.3,A,I5)')
     &        'draw line  {', RVEC4(2), RVEC4(3), RVEC4(4), '} {',
     &			      PXYZ(1),  PXYZ(2), PXYZ(3), 
     &                                          '} width ', WIDTH

C text record? - do not use but must read to avoid error
        ELSEIF (RVEC4(1).EQ.5.0) THEN
C 2nd number read gives the number of characters
          READ( SIN)   STRING(1:INT(RVEC4(2)))
          WRITE( NOUT, '(A)')
     &' Warning - ignoring text record in .qpt file'//CHAR(7)

        ENDIF
 
! store the coords for next line
        PXYZ(1) = RVEC4(2)
        PXYZ(2) = RVEC4(3)
        PXYZ(3) = RVEC4(4)
  
C read next line of quanta file
        GOTO 10

C end of pass ipass - 
25    CONTINUE


55555 WRITE(NOUT,*)
      RETURN
      END
