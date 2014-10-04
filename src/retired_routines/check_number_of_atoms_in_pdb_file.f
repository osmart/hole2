      PROGRAM  check_number_of_atoms_in_pdb_file
      IMPLICIT NONE
C ********************************************************************
C *                                                                  *
C * This software is an unpublished work containing confidential and *
C * proprietary information of Birkbeck College. Use, disclosure,    *
C * reproduction and transfer of this work without the express       *
C * written consent of Birkbeck College are prohibited. This notice  *
C * must be attached to all copies or extracts of the software.      *
C * (c) 2000 Oliver Smart                    *
C *                                                                  *
C ********************************************************************
C
C Modification history:
C
C Date	Author		Modification
C 08/00 O.S.S.		First version
C			
C This program is intended to check the number atoms in a pdf file
C to be read from standard input and mirror back to the user
C needed to server version of HOLE.
C simply a very cut down version of hole doing single call to tsatr

C input and output streams
      INTEGER			NIN, NOUT
      PARAMETER(		NIN  = 5)
      PARAMETER(		NOUT = 6)

C stream for input
      INTEGER			SIN

C error found flag
      LOGICAL			LERR

C logical vairable - produce debug output
      LOGICAL			LDBUG

C maximum no of entries in lists
      INTEGER			MAXLST
      PARAMETER(		MAXLST = 100)

C bond radius list
      INTEGER			BNDNO
      CHARACTER*4		BNDBRK(MAXLST)
      DOUBLE PRECISION		BNDR(MAXLST)

C vdW radius list
      INTEGER			VDWNO
      CHARACTER*4		VDWBRK(MAXLST)
      CHARACTER*3		VDWRES(MAXLST)
      DOUBLE PRECISION		VDWR(MAXLST)

C logical function which opens file (readonly on vax)
      LOGICAL			OPENRO

C maximum no. of atoms
      INTEGER			ATMAX
      PARAMETER(		ATMAX =  30000)

C number of atoms read in from each file:
      INTEGER			ATNO

C atom names found in Brookhaven file:
      CHARACTER*4		ATBRK(ATMAX)

C residue name in brook
      CHARACTER*3		ATRES(ATMAX)

C chain identifier in brookhaven pdb file
      CHARACTER*1               ATCHN(ATMAX)

C integer residue no.
      INTEGER			ATRNO(ATMAX)

C co-ordinates
      DOUBLE PRECISION		ATXYZ( 3, ATMAX)

C vdw and bond radii of each atoms
      DOUBLE PRECISION		ATVDW(ATMAX), ATBND(ATMAX)

C 16/6/94 - spherebox option.
C s/r ****** needs vble ATRAD as store of the
C radius squared between the centre of boxsphere and
C each atom to avoid unnecessary calc
      DOUBLE PRECISION		ATRAD2( ATMAX)

C 11/95 introduce a string which will list residues to be ignored on read
      CHARACTER*80		IGNRES
      INTEGER			OATNO(0:ATMAX)

C end of decs ******************

C initial values
      LERR  = .FALSE.
      LDBUG = .FALSE.
C ignore residue string
      IGNRES = '........................................'//
     &         '........................................'

      CALL TSATR(  NIN, NOUT, LERR, LDBUG, .FALSE., .FALSE.,
     &	ATMAX, ATNO, ATBRK, ATRES, ATCHN, ATRNO, ATXYZ, ATVDW, ATBND,
     &	MAXLST, BNDNO, BNDBRK, BNDR, VDWNO, VDWBRK, VDWRES, VDWR,
     &  IGNRES, OATNO)
     
      WRITE( NOUT, '(I10)') ATNO
      END
