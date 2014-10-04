      SUBROUTINE QPTVRM
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
C 09/96	O.S. Smart &
C	Xiaonan Wang	Original version
C
C
C Takes hydra/quanta binary 3D plot file and converts to
C a Virtual Reality Markup Language file.

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

C number of move/draw records output
      INTEGER			NOMVDR
C current point
      REAL			CURPNT(3)
      
C loop count for index
      INTEGER			ICOUNT
      
C end of decs ***********

      WRITE( NOUT, '(A)') 
     &'  ',
     &'   This option Reads in a quanta 3D binary plot and writes',
     &'   out a VRML equivalent.'

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
     &  'output VRML', FOUT, LABORT, '.wrl')
      IF (LABORT) GOTO 55555

C write header for plot file 
      WRITE ( SOUT, '(A)') 
     &'#VRML V1.0 ascii',
     &'Separator {',
     &'Info {',
     &'string "Output of V option of program qpt_conv',
     &' Input file: '//FIN(1:INDEX(FIN,'     ')-1),
     &'" }'


C go thru the qpt file seven times
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
C at end of the file then goto line 20 read file for next colour 
10      CONTINUE
          READ( SIN, END= 20) RVEC4
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
C if so write header
C normal quanta to magenta
              IF (LFIRST) THEN 
                WRITE ( SOUT, '(A)') 
     &'Separator { # quanta colours 1 to 14 to cyan',
     &'Separator {',
     &'Material { diffuseColor 0.00 1.00 1.00 }',
     &'Coordinate3 { ',
     &'    point ['
                LFIRST = .FALSE.
                NOMVDR = 0
              ENDIF
              
            ELSEIF ((IPASS.EQ.2).AND.(CURCOL.EQ.15)) THEN
              LVALID = .TRUE.
C centreline to yellow
              IF (LFIRST) THEN 
                WRITE ( SOUT, '(A)') 
     &'Separator { # col 15 centreline to yellow',
     &'Separator {',
     &'Material { diffuseColor 1.00 1.00 0.00 }',
     &'Coordinate3 { ',
     &'    point ['
                LFIRST = .FALSE.
                NOMVDR = 0
              ENDIF

            ELSEIF ((IPASS.EQ.3).AND.(CURCOL.EQ.16)) THEN
              LVALID = .TRUE.
C low rad to red 
              IF (LFIRST) THEN 
                WRITE ( SOUT, '(A)') 
     &'Separator { # col 16 low rad to red',
     &'Separator {',
     &'Material { diffuseColor 1.00 0.00 0.00 }',
     &'Coordinate3 { ',
     &'    point ['
                LFIRST = .FALSE.
                NOMVDR = 0
              ENDIF

            ELSEIF ((IPASS.EQ.4).AND.(CURCOL.EQ.17)) THEN
              LVALID = .TRUE.
C mid rad to red 
              IF (LFIRST) THEN 
                WRITE ( SOUT, '(A)') 
     &'Separator { # col 17 mid rad to green',
     &'Separator {',
     &'Material { diffuseColor 0.00 1.00 0.00 }',
     &'Coordinate3 { ',
     &'    point ['
                LFIRST = .FALSE.
                NOMVDR = 0
              ENDIF

            ELSEIF ((IPASS.EQ.5).AND.(CURCOL.EQ.18)) THEN
              LVALID = .TRUE.
              IF (LFIRST) THEN 
                WRITE ( SOUT, '(A)') 
     &'Separator { # col 18 high rad to blue',
     &'Separator {',
     &'Material { diffuseColor 0.00 0.00 1.00 }',
     &'Coordinate3 { ',
     &'    point ['
                LFIRST = .FALSE.
                NOMVDR = 0
              ENDIF

            ELSEIF ((IPASS.EQ.5).AND.(CURCOL.EQ.19)) THEN
              LVALID = .TRUE.
              IF (LFIRST) THEN 
                WRITE ( SOUT, '(A)') 
     &'Separator { # col 19 capsule vecs to purple',
     &'Separator {',
     &'Material { diffuseColor 1.00 0.00 1.00 }',
     &'Coordinate3 { ',
     &'    point ['
                LFIRST = .FALSE.
                NOMVDR = 0
              ENDIF

            ELSEIF ((IPASS.EQ.5).AND.(CURCOL.EQ.20)) THEN
              LVALID = .TRUE.
              IF (LFIRST) THEN 
                WRITE ( SOUT, '(A)') 
     &'Separator { # col 20 spikes grey ',
     &'Separator {',
     &'Material { diffuseColor 0.50 0.50 0.50 }',
     &'Coordinate3 { ',
     &'    point ['
                LFIRST = .FALSE.
                NOMVDR = 0
              ENDIF

            ELSE
C do not output this object
              LVALID = .FALSE.
            ENDIF

C a dotat record and current colour is valid?
          ELSEIF ((RVEC4(1).EQ.4.0) .AND. LVALID) THEN
C draw small horizontal line
            WRITE( SOUT, '(4X,3F9.3,A)')
     &        RVEC4(2)-0.02, RVEC4(3), RVEC4(4), ',',
     &        RVEC4(2)+0.02, RVEC4(3), RVEC4(4), ','
            NOMVDR = NOMVDR + 1
            CURPNT(1) = RVEC4(2)
            CURPNT(2) = RVEC4(3)
            CURPNT(3) = RVEC4(4)
            
C a moveto record and current colour is valid?
          ELSEIF ((RVEC4(1).EQ.2.0) .AND. LVALID) THEN
            CURPNT(1) = RVEC4(2)
            CURPNT(2) = RVEC4(3)
            CURPNT(3) = RVEC4(4)

C a moveto record and current colour is valid?
          ELSEIF ((RVEC4(1).EQ.3.0) .AND. LVALID) THEN
C want to draw to this point so output current point
            WRITE( SOUT, '(4X,3F9.3,A)')
     &        CURPNT(1), CURPNT(2), CURPNT(3), ',',
     &        RVEC4(2), RVEC4(3), RVEC4(4), ','
            CURPNT(1) = RVEC4(2)
            CURPNT(2) = RVEC4(3)
            CURPNT(3) = RVEC4(4)
            NOMVDR = NOMVDR + 1

C text record? - do not use but must read to avoid error
        ELSEIF (RVEC4(1).EQ.5.0) THEN
C 2nd number read gives the number of characters
          READ( SIN)   STRING(1:INT(RVEC4(2)))
          WRITE( NOUT, '(A)')
     &' Warning - ignoring text record in .qpt file'//CHAR(7)

        ENDIF
   
C read next line of quanta file
        GOTO 10
C jump here at end of reading file
20      CONTINUE
C have we output any points of this colour?
        IF (.NOT.LFIRST) THEN
C must end "point [" list
          WRITE( SOUT, '(4X,A)') ']} # end of point records'
C now must join up points
          WRITE( SOUT, '(A)')
     &'IndexedLineSet { coordIndex ['
C join 0 to 1, 2 to 3, 4 to 5
          DO 567 ICOUNT = 1, NOMVDR
C records comma delimited
             WRITE( SOUT, '(3(I8,A))') 
     &       2*ICOUNT-2, ',', 2*ICOUNT-1, ',', -1, ','
567       CONTINUE
          WRITE( SOUT, '(A)')
     &'                            ] } #IndexedLineSet',
     &'}',
     &'}'
        ENDIF
C end of pass ipass - 
25    CONTINUE

C write end of final seperator
      WRITE( SOUT, '(A)')
     &'} #final seperator'

55555 WRITE(NOUT,*)
      RETURN
      END
