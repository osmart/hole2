      SUBROUTINE HYDASC
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
C 10/95	O.S. Smart	Converted to s/r for qpt_conv program 
C                       (bundles all small conversion programs together)
C
C converts hydra/quanta binary plot files to asc
C and viceversa

C the vectors
      REAL 			RVEC4(4)
C no. of records converted
      INTEGER			NOCONV
C command
      CHARACTER*1		COM1

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

C end of decs ***********

C 2/5/95 initialize number of records count (need for sg)
      NOCONV = 0

C ask user whether binary to ascii
      WRITE( NOUT, '(A)') 
     &' Converts Hydra/Quanta plot files from binary to ascii',
     &' and vice-versa. (Now deals with character records.)'

      CALL PROMPT( NOUT,
     & 'From binary to ascii "a" or ascii to binary "b"? <a>: ')
      READ( NIN, '(A1)', ERR= 55555, END= 55555) COM1

      IF ((COM1.EQ.'B').OR.(COM1.EQ.'b')) THEN
C to binary

C March 1993 - new routine to find latest file of
C type in the directory.  N.b. only works on unix machines
        CALL LASTF( FDUM, '.asc')
        IF (FDUM(1:4).EQ.'none') FDUM = 'input'

C get input filename
        LABORT = .TRUE.
C (input stream, output, oldfile?, file_stream, file_type, name,
C  allow abort?, default extension)
C N.B. as file_type includes 'BINARY' then will open as binary
        CALL INTERF( NIN, NOUT, .TRUE., SIN, 
     &  'input ascii format', FDUM, LABORT, '.asc')
        IF (LABORT) GOTO 55555

C get output filename
        LABORT = .TRUE.
        CALL INTERF( NIN, NOUT, .FALSE., SOUT, 
     &  'output binary hydra/quanta plot', FDUM, LABORT, '.qpt')
        IF (LABORT) GOTO 55555

C start the read
20      CONTINUE
        READ( SIN, *, END=55) RVEC4
C must do special things if this is a text indicator
        IF (RVEC4(1).EQ.5.0) THEN
C read the next line of file into string
          READ( SIN, '(A)') STRING
C this string MUST contain %% - indicating the end
C ignore second number and replace with this
          RVEC4(2) = INDEX(STRING,'%%')-1
          WRITE(SOUT) RVEC4
          WRITE(SOUT) STRING(1:INT(RVEC4(2)))
        ELSE
C normal record
          WRITE(SOUT) RVEC4
        ENDIF
      	NOCONV = NOCONV + 1
        GOTO 20

      ELSE

C to ascii

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

C get output filename
        LABORT = .TRUE.
        CALL INTERF( NIN, NOUT, .FALSE., SOUT, 
     &  'output ascii format', FDUM, LABORT, '.asc')
        IF (LABORT) GOTO 55555

C start the read
10      CONTINUE
        READ( SIN, END=55) RVEC4
        WRITE(SOUT,*) RVEC4
C text record?
        IF (RVEC4(1).EQ.5.0) THEN
C 2nd number read gives the number of characters
          READ( SIN) STRING(1:INT(RVEC4(2)))
C write record followed by %%
          WRITE( SOUT, '(A)') STRING(1:INT(RVEC4(2)))//'%%'
        ENDIF
      	NOCONV = NOCONV + 1
        GOTO 10

      ENDIF

55    WRITE(NOUT,*) 'Number of records converted ', NOCONV

55555 CONTINUE
      WRITE(NOUT,*)
      RETURN
      END
