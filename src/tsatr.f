      SUBROUTINE TSATR(  SIN, NOUT, LERR, LDBUG, LVDW, LBND,
     &	ATMAX, ATNO, ATBRK, ATRES, ATCHN, ATRNO, ATXYZ, ATVDW, ATBND,
     &	MAXLST, BNDNO, BNDBRK, BNDR, VDWNO, VDWBRK, VDWRES, VDWR,
     &  IGNRES, OATNO)
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
C 11/95 O.S.S.		Addition of IGNRES string for residue types to be
C			ignored, OATNO to store original atom numbers
C 03/96 O.S.S.		Support for proper hydrogen naming vble HTEST
C
C
C this s/r read pdb atom records from file opened to stream SIN
C and set up vdW and bond radii for each atom (if LVDW and LBND are
C set .true.).
C Adapted to read chain identifier Nov '93

C co-ord file input stream no.
C (return unchanged)
      INTEGER			SIN

C output stream no (to user)
C (return unchanged)
      INTEGER			NOUT

C maximum no. of atoms
C set up as parameter in program (so don't change!)
      INTEGER			ATMAX

C number of atoms read in:
      INTEGER			ATNO

C atom names found in Brookhaven file:
      CHARACTER*4		ATBRK(ATMAX)

C residue name in brook
      CHARACTER*3		ATRES(ATMAX)

C chain identifier in brookhaven pdb file
      CHARACTER*1		ATCHN(ATMAX)

C integer residue no.
      INTEGER			ATRNO(ATMAX)

C co-ordinates
      DOUBLE PRECISION		ATXYZ(3,ATMAX)

C vdw and bond radii of each atoms
      DOUBLE PRECISION		ATVDW(ATMAX), ATBND(ATMAX)

C maximum no of entries in lists
C set up as parameter in program (so don't change!)
      INTEGER			MAXLST

C bond radius list
C (return unchanged)
      INTEGER			BNDNO
      CHARACTER*4		BNDBRK(MAXLST)
      DOUBLE PRECISION		BNDR(MAXLST)

C vdW radius list
C (return unchanged)
      INTEGER			VDWNO
      CHARACTER*4		VDWBRK(MAXLST)
      CHARACTER*3		VDWRES(MAXLST)
      DOUBLE PRECISION		VDWR(MAXLST)


C string listing residue types to be ignored
      CHARACTER*80		IGNRES

C need to record on the initial read of the pdb file the
C original atom numbers of the each atom from the pdb file
C - as we ignore some residues in the read.
C oatno(0) is the total number of original atoms in the pdb file
C oatno(1) is the original atom number of the stored atom#1 etc.
      INTEGER                   OATNO(0:ATMAX)

C logical vairable - produce debug output
C (return unchanged)
      LOGICAL			LDBUG

C if an error found is found in s/r - set lerr true
C and program will stop
C (return unchanged)
      LOGICAL			LERR

C set up bond list if LBND, vdW if LVDW
C (return unchanged)
      LOGICAL			LBND, LVDW

C line to read info
      CHARACTER*132		LINE

C count index
      INTEGER			JCOUNT

C function to match character strings
      LOGICAL			LMATCH

C one character test vble to see whether we have funny
C hydrogen type record
      CHARACTER*1		HTEST

C end of decs ******************

C initialize variables
      ATNO = 0
      OATNO(0) = 0
      LERR = .FALSE.


C read from file until 'atom' record is found
C (do until loop to 55555 - the return statement)
10    CONTINUE
        READ( SIN, '(A)', END= 55555) LINE
C October 1993 read HETATM's as well as atoms
	IF ( (LINE(1:4).NE.'ATOM') .AND. 
     &       (LINE(1:6).NE.'HETATM') ) GOTO 10

C 'ATOM' found ___read in line
C one more atom
	ATNO = ATNO + 1
C increment original number of atoms
        OATNO(0) = OATNO(0) + 1

C make sure that we have not exceeded maximum 
C no of atoms which can be read in
	IF (ATNO.GT.ATMAX) THEN
	  WRITE(NOUT,*) '***ERROR***', CHAR(7)
	  WRITE(NOUT,*) 'Have exeeded array bound ATMAX ', ATMAX
	  WRITE(NOUT,*) '(the maximum no. of atoms allowed)'
	  LERR = .TRUE.
	  GOTO 55555
	ENDIF

C read line in pdb format
C reading four character brookhaven atomname but if 
C the standard can put an "H" preceeding this -
C read into HTEST
	READ(LINE(5:80), '(8X,A1,A4,A3,1X,A1,I4,4X,3F8.3)')
     &    HTEST,
     &	  ATBRK(ATNO), ATRES(ATNO), ATCHN(ATNO), ATRNO(ATNO),
     &	  (ATXYZ(JCOUNT,ATNO), JCOUNT = 1, 3)
        IF (HTEST.EQ.'H') ATBRK(ATNO) = HTEST//ATBRK(ATNO)
C 22/5/98 support for FE records of heme
        IF ((ATBRK(ATNO).EQ.'E  ').AND.(HTEST.EQ.'F'))
     &                                      ATBRK(ATNO) = 'FE  '

C should we ignore this residue?
C Check to see whether residue type appears in ignres
C comparison should be between upper case strings
        CALL UCASE(ATRES(ATNO))
        IF (INDEX(IGNRES, ATRES(ATNO)).NE.0) THEN
          ATNO = ATNO - 1
C (leave oatno(0) alone)
C read next atom
          GOTO 10
        ENDIF

C setup bond radius
	IF (LBND) THEN
C go thru' bond radius lists
	  DO 20 JCOUNT = 1, BNDNO
C use function lmatch to match character strings
C 4 is the no. of characters to be matched and ? is the wildcard
C character
	    IF (LMATCH( '?', 4, BNDBRK(JCOUNT), ATBRK(ATNO))) THEN
	       ATBND(ATNO) = BNDR(JCOUNT)
	       GOTO 201
	    ENDIF
20	  CONTINUE
C error cannot find bond radius for atom
	  WRITE(NOUT,*) '***ERROR***', CHAR(7)
	  WRITE(NOUT,*) 'Cannot find bond radius for atom:'
	  WRITE(NOUT,*) ATBRK(ATNO), ATRES(ATNO), ATRNO(ATNO)
	  LERR = .TRUE.
	  GOTO 55555

	ENDIF

C have found bond radius for atom
201	CONTINUE

C setup van der Waals radius
	IF (LVDW) THEN
C go thru' bond radius lists
	  DO 30 JCOUNT = 1, VDWNO
C use function lmatch to match character strings
C 4 is the no. of characters to be matched and ? is the wildcard
C character HERE we must match 3*C residue name as well.
	    IF ( LMATCH( '?', 4, VDWBRK(JCOUNT), ATBRK(ATNO)) .AND.
     &		 LMATCH( '?', 3, VDWRES(JCOUNT), ATRES(ATNO))  ) THEN
	       ATVDW(ATNO) = VDWR(JCOUNT)
	       GOTO 301
	    ENDIF
30	  CONTINUE
C error cannot find vdW radius for atom
	  WRITE(NOUT,*) '***ERROR***', CHAR(7)
	  WRITE(NOUT,*) 'Cannot find vdW radius for atom:'
	  WRITE(NOUT,*) ATBRK(ATNO), ATRES(ATNO), ATRNO(ATNO)
	  LERR = .TRUE.
	  GOTO 55555

	ENDIF

C have found bond radius for atom
301	CONTINUE

C debug output
	IF (LDBUG) WRITE(NOUT,*) 'debug Atom: ',
     &	  ATBRK(ATNO), ATRES(ATNO), ATRNO(ATNO),
     &	  (ATXYZ(JCOUNT,ATNO), JCOUNT = 1, 3)
	IF (LDBUG) WRITE(NOUT,*) '           ',
     &	  ' number ', ATNO, ATBND(ATNO), ATVDW(ATNO)

C record original atom number
        OATNO(ATNO) = OATNO(0)

C read next entry:
      GOTO 10

55555 RETURN
      END
