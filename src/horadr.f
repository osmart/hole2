      SUBROUTINE HORADR( NOUT, LERR, LDBUG, SHORTO, FRADIU,
     &	MAXLST, BNDNO, BNDBRK, BNDR, VDWNO, VDWBRK, VDWRES, VDWR)
      IMPLICIT NONE
C ********************************************************************
C *                                                                  *
C * This software is an unpublished work containing confidential and *
C * proprietary information of Birkbeck College. Use, disclosure,    *
C * reproduction and transfer of this work without the express       *
C * written consent of Birkbeck College are prohibited. This notice  *
C * must be attached to all copies or extracts of the software.      *
C *                                                                  *
C * (c) 1995 Oliver Smart & Birkbeck College, All rights reserved    *
C * (c) 1996 Oliver Smart & Birkbeck College, All rights reserved    *
C *                                                                  *
C ********************************************************************
C
C Modification history:
C
C Date	Author		Modification
C 11/95	O.S. Smart	First version
C
C
C This s/r reads deals with opening radius file reading
C atomic bond and vdW radius record for HOLE.

C output stream no (to user)
      INTEGER			NOUT


C if an error found is found in s/r - set lerr true and program will stop, 
C debug vbles
      LOGICAL			LERR, LDBUG

C  variable to reduce text output -
C if oshort =
C 0: full text output
C 1: no in run progress reported
C 2: ditto plus no graph type output - only leaving
C    Minimum radius & conductance calculations.
C 3: All text output other than input card mirroring
C    and error messages turned off
      INTEGER                   SHORTO

C radius filename
      CHARACTER*200		FRADIU

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

C internal vbles ***************

C input stream number
      INTEGER			SIN

C line to read info
      CHARACTER*132		LINE

C function to open files readonly
      LOGICAL			OPENRO

C end of decs ******************

C the reading of radius file and pdb file copied from
C program TooShort

C open vdw/bond radius file (to stream sin)
      IF (.NOT.OPENRO( SIN, FRADIU, NOUT)) THEN
C cannot open radius file
        WRITE(NOUT,*) '*** ERROR ***'
        WRITE(NOUT,*) 'Cannot open bond/vdw radius input file:'
        WRITE(NOUT,*) FRADIU(1:INDEX(FRADIU,'     ')-1)
        LERR = .TRUE.
        GOTO 55555
      ENDIF

C read bond and vdw radii from file
      CALL TSRADR( SIN, NOUT, LERR, LDBUG,
     &  MAXLST, BNDNO, BNDBRK, BNDR, VDWNO, VDWBRK, VDWRES, VDWR)
      IF (LERR) GOTO 55555

C tell user extact filename and no of records read
      INQUIRE( SIN, NAME = LINE)
      IF (SHORTO.LT.3) WRITE( NOUT, '(/3(A,I6,A/))')
     &' Have read',  BNDNO, ' bond radius records and',
     &'          ',  VDWNO,' vdW radius records.',
     &' From file: '//LINE(1:INDEX(LINE,'   ')-1)

C close RADIUS file
      CLOSE( SIN)

55555 RETURN
      END
