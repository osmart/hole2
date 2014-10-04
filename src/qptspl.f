      SUBROUTINE QPTSPL
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
C 06/94	O.S. Smart	first version
C 10/95 O.S. Smart      Converted to s/r for qpt_conv program
C                       (bundles all small conversion programs together)
C
C
C Takes hydra/quanta binary plot file and splits long lines up
C so that depth queueing works properly in qplot.

C the vectors (n.b. zeroth record is the type 
C 2, moveto 3, draw to 4, dotat etc.)
      REAL 			RVEC4(0:3)
C no. of records converted
      INTEGER			NOCONV
C size of line section
      REAL			MAXLEN

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

C length of move draw
      REAL			LINLEN

C old record, vector from old to new
      REAL			OLDR(3), PREVEC(3)

C loop counts
      INTEGER			ICOUNT, XCOUNT


C end of decs ***********


      WRITE( NOUT, '(A)') 
     &' Program qpt_split_long_line.',
     &' Reads in a hydra/quanta binary plot file and splits',
     &' long lines up so that depth queueing works properly in qplot.'

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
C default filename
      IDUM = INDEX(FDUM,'.qpt')
      IF (IDUM.NE.0) FDUM = FDUM(1:IDUM-1)//'_split'
     
      CALL INTERF( NIN, NOUT, .FALSE., SOUT, 
     &  'output binary quanta plot file', FDUM, LABORT, '.qpt')
      IF (LABORT) GOTO 55555

C ask for cross bar size
      CALL PROMPT( NOUT,
     &'How long do you want the line sections? <0.1angs>: ')
      READ( NIN, '(A)') STRING
      IF (STRING(1:5).NE.'     ') THEN
        READ( STRING,'(BN,F10.0)') MAXLEN
      ELSE
        MAXLEN = 0.10
      ENDIF

C start the read
10    CONTINUE
        READ( SIN, END=55) RVEC4
C is this a draw to record?
        IF (RVEC4(0).EQ.3.) THEN
C vector from previous record to this
          PREVEC(1) = RVEC4(1)-OLDR(1)
          PREVEC(2) = RVEC4(2)-OLDR(2)
          PREVEC(3) = RVEC4(3)-OLDR(3)
C find the length of the vector from previous
C record
          LINLEN = SQRT(PREVEC(1)**2+PREVEC(2)**2+PREVEC(3)**2)
          IF (LINLEN.GT.MAXLEN) THEN
C make prevec unit
            PREVEC(1) = PREVEC(1)/LINLEN
            PREVEC(2) = PREVEC(2)/LINLEN
            PREVEC(3) = PREVEC(3)/LINLEN
C work out number of additional sections
            DO 33 ICOUNT = 1, INT(LINLEN/MAXLEN)
              WRITE(SOUT) 3., (OLDR(XCOUNT) + 
     &          FLOAT(ICOUNT)*MAXLEN*PREVEC(XCOUNT),XCOUNT = 1,3)
              NOCONV = NOCONV + 1
33          CONTINUE
          ENDIF
        ENDIF

C write the record
        WRITE( SOUT) RVEC4
        NOCONV = NOCONV + 1
C store it
        OLDR(1) = RVEC4(1)
        OLDR(2) = RVEC4(2)
        OLDR(3) = RVEC4(3)

C text record?
        IF (RVEC4(0).EQ.5.0) THEN
C 2nd number read gives the number of characters
          READ( SIN)   STRING(1:INT(RVEC4(1)))
          WRITE( SOUT) STRING(1:INT(RVEC4(1)))
        ENDIF
        GOTO 10


55    CONTINUE
      WRITE(NOUT,*) 'Number of records output ', NOCONV

55555 CONTINUE
      WRITE(NOUT,*)
      RETURN
      END
