      SUBROUTINE QPTINS
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
C 10/95 O.S. Smart      Converted to s/r for qpt_conv program
C                       (bundles all small conversion programs together)
C
C
C Takes hydra/quanta binary 3D plot file and converts to
C Biosym Insight II special user file format.

C the vectors
      REAL 			RVEC4(4)
C no. of records converted
      INTEGER			NOCONV

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

C working integer
      INTEGER			IDUM

C abort indicator
      LOGICAL			LABORT

C one character command to store whether output file
C is to contain line type records (L) or dot type records (anything else)
C to difficult to do texxt records
      CHARACTER*1		FTYPE

C string to allow reading of number
      CHARACTER*80		LINE80

C colour number for dots
      INTEGER			COLNO

C end of decs ***********


      WRITE( NOUT, '(A)') 
     &' Program qpt_insight.  ',
     &'   Reads in a hydra/quanta 3D binary plot and writes',
     &'   out a Biosym Insight II special user format file.'

C March 1993 - new routine to find latest file of
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

C The insight II file can contain only line type records,
C dot type records or text string records - must nominate one now.
      WRITE( NOUT, '(A)')
     &' A single Insight II user plot file can contain only',
     &' line type records or dot at records - NOT both at once.',
     &' If you want to convert a .qpt file containing both ',
     &' types of record you must run this program twice',
     &' to produce two seperate files',
     &' NB CANNOT CONVERT TEXT RECORDS - AS FORMAT NOT COMPATIBLE'
      CALL PROMPT( NOUT,
     & 'Do you want Lines (L) or Dots (D) in the output file? <D>:')
      READ( NIN, '(A1)', END= 55555, ERR= 55555) FTYPE
      CALL UCASE( FTYPE)

C if one character command FTYPE is 'L' then process lines
C or if 'T' text strings. Otherwise process dots

C adjust default output filename so it reflects this info
      IDUM = INDEX(FDUM,'.qpt')
      IF (IDUM.NE.0) THEN
        IF (FTYPE.EQ.'L') THEN
          FDUM = FDUM(1:IDUM-1)//'_line'
        ELSE
          FDUM = FDUM(1:IDUM-1)//'_dot'
        ENDIF
      ENDIF

C open output file - default id should be .usr to agree with Biosym
      LABORT = .TRUE.
      CALL INTERF( NIN, NOUT, .FALSE., SOUT, 
     &  'output insight II user', FDUM, LABORT, '.usr')
      IF (LABORT) GOTO 55555

C first line of output file says what the file contains
      IF (FTYPE.EQ.'L') THEN
        WRITE( SOUT, '(A4)') 'LINE'
      ELSE
        WRITE( SOUT, '(A4)') 'DOTS'
C as we are doint dots ask what colour number the user wants for each
        WRITE( NOUT, '(A)')
     &' Each dot in the .usr file has a colour number',
     &'  which lies between 0 and 360.  Will assign same colour',
     &'  number to all of the dots (for now).'
        CALL PROMPT( NOUT,
     & 'What number do you want (specifiy value) <0>:')
        READ( NIN, '(A)', END= 55555, ERR= 55555) LINE80
        IF (LINE80(1:6).NE.'     ') THEN
          READ( LINE80, '(BN,I10)') COLNO
        ELSE
          COLNO = 0
        ENDIF
      ENDIF

C start the read
10    CONTINUE
C read line from quanta file
        READ( SIN, END=55) RVEC4
C a dotat record and doing dots?
        IF ( (RVEC4(1).EQ.4.0) .AND. (FTYPE.NE.'L')) THEN
C documentation says to leave just one space between 
C numbers - but assume free format for now
          WRITE( SOUT, *) RVEC4(2), RVEC4(3), RVEC4(4), COLNO
          NOCONV = NOCONV + 1

C outputing lines and got a move to?
        ELSEIF ( (RVEC4(1).EQ.2.0).AND. (FTYPE.EQ.'L')) THEN
C format x, y, z  ' P' for move
          WRITE( SOUT, *) RVEC4(2), RVEC4(3), RVEC4(4), ' P'
          NOCONV = NOCONV + 1

C outputing lines and got a draw to?
        ELSEIF ( (RVEC4(1).EQ.3.0).AND. (FTYPE.EQ.'L')) THEN
C format x, y, z  ' L' for a draw to
          WRITE( SOUT, *) RVEC4(2), RVEC4(3), RVEC4(4), ' L'
          NOCONV = NOCONV + 1

C text record? - do not use but must read to avoid error
         ELSEIF (RVEC4(1).EQ.5.0) THEN
C 2nd number read gives the number of characters
            READ( SIN)   STRING(1:INT(RVEC4(2)))
            WRITE( NOUT, '(A)')
     &' Warning - ignoring text record in .qpt file'//CHAR(7)
        ENDIF

        GOTO 10


55    WRITE(NOUT,*) 'Number of records converted ', NOCONV

55555 WRITE(NOUT,*)
      RETURN
      END
