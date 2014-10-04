      SUBROUTINE QPTDOT
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
C Takes hydra/quanta binary plot file and converts dotat
C records to 3-D crosses.

C the vectors
      REAL 			RVEC4(4)
C no. of records converted
      INTEGER			NOCONV
C size of cross bar 
      REAL			CROSSD

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

C end of decs ***********


C 2/6/95 sg's do not initialize vbles properly - put no of recs to zero
      NOCONV = 0

      WRITE( NOUT, '(A)') 
     &' S/r qptdot.  Reads in a hydra/quanta binary plot',
     &' file and converts all dotat records to 3-D crosses.',
     &' N.B. the cross size used refers to size of a cross',
     &' bar not the length from the centre to edge of cross.'

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
      IF (IDUM.NE.0) FDUM = FDUM(1:IDUM-1)//'_cross'
     
      CALL INTERF( NIN, NOUT, .FALSE., SOUT, 
     &  'output binary quanta plot file', FDUM, LABORT, '.qpt')
      IF (LABORT) GOTO 55555

C ask for cross bar size
      CALL PROMPT( NOUT,
     &'What size do you want the cross bars to be? <0.1angs>: ')
      READ( NIN, '(A)') STRING
      IF (STRING(1:5).NE.'     ') THEN
        READ( STRING,'(BN,F10.0)') CROSSD
        CROSSD = CROSSD/2.
      ELSE
        CROSSD = 0.05
      ENDIF
      CALL VTCLEAR( NOUT)

C start the read
10    CONTINUE
        READ( SIN, END=55) RVEC4
C a dotat record?
        IF (RVEC4(1).EQ.4.0) THEN
C convert to a cross
C first X
C move to - a 2.0 as first record
          RVEC4(1) = 2.0
          RVEC4(2) = RVEC4(2) + CROSSD
          WRITE( SOUT) RVEC4
C draw to minus 2*cross size
          RVEC4(1) = 3.0
          RVEC4(2) = RVEC4(2) - 2.*CROSSD
          WRITE( SOUT) RVEC4
C back to orig
          RVEC4(2) = RVEC4(2) + CROSSD

C then y
          RVEC4(1) = 2.0
          RVEC4(3) = RVEC4(3) + CROSSD
          WRITE( SOUT) RVEC4
          RVEC4(1) = 3.0
          RVEC4(3) = RVEC4(3) - 2.*CROSSD
          WRITE( SOUT) RVEC4
          RVEC4(3) = RVEC4(3) + CROSSD

C then z 
          RVEC4(1) = 2.0
          RVEC4(4) = RVEC4(4) + CROSSD
          WRITE( SOUT) RVEC4
          RVEC4(1) = 3.0
          RVEC4(4) = RVEC4(4) - 2.*CROSSD
          WRITE( SOUT) RVEC4
          RVEC4(4) = RVEC4(4) + CROSSD

C keep a record of number of crosses converted.
      	  NOCONV = NOCONV + 1
        ELSE
C write the record
          WRITE( SOUT) RVEC4

C text record?
          IF (RVEC4(1).EQ.5.0) THEN
C 2nd number read gives the number of characters
            READ( SIN)   STRING(1:INT(RVEC4(2)))
            WRITE( SOUT) STRING(1:INT(RVEC4(2)))
          ENDIF
        ENDIF
        GOTO 10


55    WRITE(NOUT,*) 'Number of dot records converted ', NOCONV

55555 WRITE(NOUT,*)
      RETURN
      END
