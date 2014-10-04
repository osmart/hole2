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
C Date  Author          Modification
C 12/93 O.S. Smart      Original public release in HOLE suite beta1.0
C
C ***This routine lifted straight from program series_stat***
C
C new fn including chain id - will replace ssafnd
      LOGICAL FUNCTION SSAFN2( BRK, IAT, CHN,
     &	ND, NAT, ATBRK, RESNO, CHAIN)
      IMPLICIT NONE
C - like fn. ssafnd but includes chain id
C this function looks for atomname name brk, residue no iat
C if found returns .true. and iat as list no.
C if not found returns .false. & iat unchanged

      CHARACTER*4		BRK
      INTEGER			IAT
      CHARACTER*1		CHN


C ND is the maxmimum no of atoms in each file dealt with
C (parameter in sstat)
      INTEGER			ND

C number of atoms read in from each file:
      INTEGER			NAT

C atom names found in Brookhaven file:
      CHARACTER*4 		ATBRK(ND)

C integer residue no. chain identifier
      INTEGER			RESNO(ND)
      CHARACTER*1		CHAIN(ND)

C working vbles
      CHARACTER*4		TBRK1, TBRK2
      CHARACTER*1		TCH1, TCH2

C loop vble
      INTEGER			ICOUNT

C end of decs ******************

C upper case atom name + chain we are looking for
      TBRK1 = BRK
      CALL UCASE( TBRK1)
      TCH1 = CHN
      CALL UCASE( TCH1)

C initial value
      SSAFN2 = .FALSE.
      DO 10 ICOUNT = 1, NAT
	IF (IAT.EQ.RESNO(ICOUNT)) THEN
C correct residue no.
	  TBRK2 = ATBRK(ICOUNT)
	  CALL UCASE( TBRK2)
C compare upper case versions
	  IF (TBRK1.EQ.TBRK2) THEN
C same names - what about chains?
	    TCH2 = CHAIN(ICOUNT)
	    CALL UCASE( TCH2)
	    IF ((TCH1.EQ.TCH2).OR.(TCH1.EQ.'?')) THEN
C number, name and chain all match!
	      SSAFN2 = .TRUE.
	      IAT = ICOUNT
	      GOTO 55555
	    ENDIF
          ENDIF
	ENDIF

10    CONTINUE

55555 RETURN
      END
