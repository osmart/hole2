      SUBROUTINE TSRADR( SIN, NOUT, LERR, LDBUG,
     1	MAXLST, BNDNO, BNDBRK, BNDR, VDWNO, VDWBRK, VDWRES, VDWR)
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
C This s/r reads atomic bond and vdW radius records,
C from file RADIUS, already opened to sin

C output stream no (to user)
      INTEGER			NOUT

C file input stream no.
      INTEGER			SIN


C maximum no of entries in lists
C (PARAMETER so dont change!)
      INTEGER			MAXLST

C bond radius list
      INTEGER			BNDNO
      CHARACTER*4		BNDBRK(MAXLST)
      DOUBLE PRECISION		BNDR(MAXLST)

C vdW radius list
      INTEGER			VDWNO
      CHARACTER*4		VDWBRK(MAXLST)
      CHARACTER*3		VDWRES(MAXLST)
      DOUBLE PRECISION		VDWR(MAXLST)

C logical vairable - produce debug output
      LOGICAL			LDBUG

C if an error found is found in s/r - set lerr true
C and program will stop
       LOGICAL			LERR

C line to read info
      CHARACTER*132		LINE

C end of decs ******************

C initialize varibles
      BNDNO = 0
      VDWNO = 0

C read input file line by line
C until the EOF encountered - then continue
10	READ( SIN, FMT='(A)', END= 55555) LINE
C deal with all chars as uppercase
	  CALL UCASE(LINE)

C bond entry?
C format BOND C??? 0.8
	  IF (LINE(1:4).EQ.'BOND') THEN
C one more entry
	    BNDNO = BNDNO + 1
C reached limit?
	    IF (BNDNO.GT.MAXLST) THEN
	      WRITE(NOUT,*) '*** ERROR ***', CHAR(7)
	      WRITE(NOUT,*) 'list limit MAXLST: ', MAXLST
	      WRITE(NOUT,*) 'exceeded in reading bond rad'
	      LERR = .TRUE.
	      GOTO 55555
	    ENDIF
C atom name
	    BNDBRK(BNDNO) = LINE(6:9)
	    READ( LINE(10:132), FMT='(F10.3)') BNDR(BNDNO)
C debug - mirror output
	    IF (LDBUG) WRITE(NOUT,*) 'debug bond rec ',
     1		       BNDNO, BNDBRK(BNDNO), BNDR(BNDNO)

C vdW entry?
C format VDWR C??? 1.85
	  ELSEIF (LINE(1:4).EQ.'VDWR') THEN
C one more entry
	    VDWNO = VDWNO + 1
C reached limit?
	    IF (VDWNO.GT.MAXLST) THEN
	      WRITE(NOUT,*) '*** ERROR ***', CHAR(7)
	      WRITE(NOUT,*) 'list limit MAXLST: ', MAXLSt
	      WRITE(NOUT,*) 'exceeded in reading vdW rad'
	      LERR = .TRUE.
	      GOTO 55555
	    ENDIF
C atom name
	    VDWBRK(VDWNO) = LINE(6:9)
	    VDWRES(VDWNO) = LINE(11:13)
	    READ( LINE(14:132), FMT='(F10.3)') VDWR(VDWNO)
C debug - mirror output
	    IF (LDBUG) WRITE(NOUT,*) 'debug vdW  rec ',
     1	 VDWNO, '   ', VDWBRK(VDWNO), '  ', VDWRES(VDWNO), VDWR(VDWNO)
	  ENDIF
C read next line (do until EOF)
	GOTO 10

C successful radius input finished
55555 RETURN

      END
