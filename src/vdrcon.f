      SUBROUTINE VDRCON( NIN, NOUT, FCOORD, FRADIU, FHYDRA, 
     &             DOTDEN, DCUT, LERR, LDBUG)
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
C 05/94	O.S. Smart	Original version
C 07/96 O.S. Smart	filename striping and de~ing added
C
C Adapted from s/r rcontr (called by hole), this s/r does keyword entry for
C program vdwdot.

C routine FREDA used here to decode lines include common vbles
C vbles used by freda
C col: 	c*80		the command?
C kl: 	int 		no of words inputed 0 then default
C fn:	r*4(40) 	no.'s as they occur
C kn:     int     	no. of no's
C fl:     c*1(40,6)	words?
      INCLUDE 'FREDAINC'

C input and output streams
C returned unchanged!
      INTEGER			NIN, NOUT

C input coordinate filename
      CHARACTER*200		FCOORD
C input radius filename
      CHARACTER*200		FRADIU
C hydra binary plot output file - default to none 
      CHARACTER*200		FHYDRA

C a dot density for dot surface 
      INTEGER			DOTDEN

C cutoff distance for calculation
      DOUBLE PRECISION		DCUT

C if error found set LERR true
      LOGICAL			LERR

C if ldbug set true then mirror input
      LOGICAL			LDBUG

C internal variables

C the first word returned by freda
      CHARACTER*6		KEY

C dummy integer
      INTEGER			IDUM

C end of decs ******************

C mirror input lines
      WRITE( NOUT,  '(/1X,A)')
     & 'Control variables read:'

C read line do until EOF or STOP read and then return @55555
10    CONTINUE

	READ( NIN, '(A)', END= 55555) COL
	CALL CHREND( COL, IDUM)
	WRITE( NOUT,'(A)') COL(1:IDUM)

C remove any comment
	CALL DECOM( COL)

C if debugging mirror line back
	IF (LDBUG) WRITE( NOUT,  '(1X,A,A)')
     &	'debug dcom line: ', COL(1:IDUM)

C if line blank then read next
	IF (COL(1:5).EQ.'     ') GOTO 10

C decode line with freda
	CALL FREDA(1,80,FL,6)

C first word is key
C No word?
	IF (KL.EQ.0) GOTO 10
C fl:	  c*1(40,6)	words?
	KEY = FL(1,1)//FL(1,2)//FL(1,3)//FL(1,4)//FL(1,5)//FL(1,6)
C make upper case
	CALL UCASE( KEY)
	IF (LDBUG) WRITE( NOUT,  '(1X,A,A)')
     &		  'debug key: ', key

C key
	IF (KEY.EQ.'STOP') THEN
C stop input
	  GOTO 55555
C pdb input filename
	ELSEIF (KEY.EQ.'COORD ') THEN
	  FCOORD = COL(7:80)
C strips strips off any leading blanks or tabs from the filename
          CALL STRIPS(FCOORD)
C resolve any tilda's (~) in filename
          CALL DTILDA(FCOORD)

C radius filename
	ELSEIF (KEY.EQ.'RADIUS') THEN
	  FRADIU = COL(7:80)
C strips strips off any leading blanks or tabs from the filename
          CALL STRIPS(FRADIU)
C resolve any tilda's (~) in filename
          CALL DTILDA(FRADIU)

C binary plot filename - output
	ELSEIF (KEY.EQ.'PLTOUT') THEN
	  FHYDRA = COL(7:80)
C strips strips off any leading blanks or tabs from the filename
          CALL STRIPS(FHYDRA)
C resolve any tilda's (~) in filename
          CALL DTILDA(FHYDRA)

C dot density
	ELSEIF (KEY.EQ.'DOTDEN') THEN
	  IF (KN.LT.1) THEN
	    LERR = .TRUE.
	    WRITE( NOUT,'(/20(1X,A:))')
     &	'ERROR: DOTDEN (the dot density for surface)',
     &	'must be specified with a number!',
     &	'line read:', COL
	  ENDIF
	  DOTDEN = INT(FN(1))
C cutoff distance
        ELSEIF (KEY(1:4).EQ.'DCUT') THEN
          IF (KN.LT.1) THEN
            LERR = .TRUE.
            WRITE( NOUT,'(/20(1X,A:))')
     &  'ERROR: DCUT (the cutoff distance for calc)',
     &  'must be specified with a number!',
     &  'line read:', COL
          ENDIF
          DCUT = FN(1)

	ELSE
C unregonized line - tell user
	  WRITE( NOUT, '(1X,A,A)')
     &		  '***Unrecognized line read: ', COL
	ENDIF


C read next line
      GOTO 10

55555 RETURN
      END
