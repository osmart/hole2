      PROGRAM LABQPT
      IMPLICIT NONE
C ********************************************************************
C *                                                                  *
C * This software is an unpublished work containing confidential and *
C * proprietary information of Birkbeck College. Use, disclosure,    *
C * reproduction and transfer of this work without the express       *
C * written consent of Birkbeck College are prohibited. This notice  *
C * must be attached to all copies or extracts of the software.      *
C *                                                                  *
C * (c) 1993, 1996 Oliver Smart & Birkbeck College,                  *
C *                                                                  *
C ********************************************************************
C
C Modification history:
C
C Date	Author		Modification
C 12/93	O.S. Smart	Original public release in HOLE suite beta1.0
C 28/02/97 O.S.S.	Release HOLE2 beta001
C 11/97    O.S.S.       vt control codes
C
C
C This program allows the user to produce a QUANTA  binary 3D plot file 
C containing labels for selected atoms read from a pdb format co-ordinate file.

C use freda to decode some replies
      INCLUDE 'FREDAINC'

C screen, keyboard
      INTEGER			NIN
      PARAMETER(		NIN = 5)
      INTEGER			NOUT
      PARAMETER(		NOUT= 6)
C input/output file stream
      INTEGER			SIN, SOUT

C dummy filename
      CHARACTER*200		FDUM


C working integer
      INTEGER			IDUM

C abort indicator
      LOGICAL			LABORT

C s/r tsatr which reads pdb file needs bonding and vdw radius arrays
C (though they are not used here)
C maximum no of entries in lists
      INTEGER                   MAXLST
      PARAMETER(                MAXLST = 1)
C bond radius list
      INTEGER                   BNDNO
      CHARACTER*4               BNDBRK(MAXLST)
      DOUBLE PRECISION          BNDR(MAXLST)
C vdW radius list
      INTEGER                   VDWNO
      CHARACTER*4               VDWBRK(MAXLST)
      CHARACTER*3               VDWRES(MAXLST)
      DOUBLE PRECISION          VDWR(MAXLST)

C arrays to store atoms
C maximum no. of atoms
      INTEGER                   ATMAX
      PARAMETER(                ATMAX = 10000)
C number of atoms read in from each file:
      INTEGER                   ATNO
C atom names found in Brookhaven file:
      CHARACTER*4               ATBRK(ATMAX)
C residue name in brook
      CHARACTER*3               ATRES(ATMAX)
C integer residue no.
      INTEGER                   ATRNO(ATMAX)
C chain identifier in brookhaven pdb file
      CHARACTER*1               ATCHN(ATMAX)
C co-ordinates
      DOUBLE PRECISION          ATXYZ( 3, ATMAX)
C vdw and bond radii of each atoms
      DOUBLE PRECISION          ATVDW(ATMAX), ATBND(ATMAX)

C error indicator
      LOGICAL			LERR

C test name to find atom, test residue number 
C (which becomes atom's list numb) and test chain id
      CHARACTER*4		BRK
      INTEGER			IAT
      CHARACTER*1		TCH
C logical function which finds list number for a particular atom
      LOGICAL			SSAFN2

C character to be output
      CHARACTER*80		LAB

C number of characters in label to be output
C the position controling integer
      INTEGER			LABEND, POSINT

C real number for writing
      REAL			RVEC4(4)


C (added 11/97 for call to tsatr - not neeeded here)
C introduce a string which will list residues to be ignored on read
      CHARACTER*80		IGNRES
C need to record on the initial read of the pdb file the 
C original atom numbers of the each atom from the pdb file
C - as we ignore some residues in the read.
C oatno(0) is the total number of original atoms in the pdb file
C oatno(1) is the original atom number of the stored atom#1 etc.
      INTEGER			OATNO(0:ATMAX)

C end of decs ***********

C ignore residue string
      IGNRES = '........................................'//
     &         '........................................'

C turn on VT codes  with bold after prompt- 
      CALL VTCON( .TRUE.)

      WRITE( NOUT, '(A)') 
     &' This program allows the user to produce a QUANTA',
     &'  binary 3D plot file containing labels for selected atoms',
     &'   read from a pdb format co-ordinate file.',
     &' Copyright 1993,1997 by Oliver Smart and Birkbeck College',
     &' Copyright 2004 by Oliver Smart ',
     &' Program modification number 2.2 001'

C write link time of program to screen
      CALL VERTIM( NOUT)
      WRITE( NOUT, '(A)') 
     &' ',
     &' Program qplot includes an extension to the standard qpt format.',
     &'   You can specify an integer to indicate where each string ',
     &'   should be placed on the page in a qplot output file',
     &'       1 indicates below to the left',
     &'       2 ............... centred etc.',
     &'       key to remember  789',
     &'                        456 (like a numeric keypad)',
     &'                        123',
     &' '

C find last .pdb file in the current directory
      CALL LASTF( FDUM, '.pdb')
      IF (FDUM(1:4).EQ.'none') FDUM = 'input'

C get input filename
      LABORT = .TRUE.
C (input stream, output, oldfile?, file_stream, file_type, name,
C  allow abort?, default extension)
      CALL INTERF( NIN, NOUT, .TRUE., SIN, 
     &  'input pdb co-ordinate', FDUM, LABORT, '.pdb')
      IF (LABORT) GOTO 55555

C get output filename
      LABORT = .TRUE.
C default filename should be same as pdb filename with _label on the end
C assume that pdb file will be identified with one of .pdb, .brk or .atm
      IDUM = MAX( INDEX(FDUM,'.pdb'), 
     &            INDEX(FDUM,'.brk'), INDEX(FDUM,'.atm'))
      IF (IDUM.NE.0) FDUM = FDUM(1:IDUM-1)//'_label'
      CALL INTERF( NIN, NOUT, .FALSE., SOUT, 
     &  'output binary quanta plot file', FDUM, LABORT, '.qpt')
      IF (LABORT) GOTO 55555


C Use s/r tsatr to read in pdb file,
C As in porgram HOLE - originally written for program TooShort.
C The two logicals set false mean that bond and vdw radii are
C not set up for each atom - as they are not needed here.
      CALL TSATR(  SIN, NOUT, LERR, .FALSE., .FALSE., .FALSE.,
     &  ATMAX, ATNO, ATBRK, ATRES, ATCHN, ATRNO, ATXYZ, ATVDW, ATBND,
     &  MAXLST, BNDNO, BNDBRK, BNDR, VDWNO, VDWBRK, VDWRES, VDWR,
     &  IGNRES, OATNO)
      IF (LERR) GOTO 55555

      WRITE( NOUT, '(/A,I5,A)')
     &' Have read', ATNO, ' atoms from the pdb file'

C start loop where we ask for an atom to be labelled
10    CONTINUE
      WRITE( NOUT,  '(/20(A:/))' )
     &' Which atom do you want to be labelled?',
     &' Specify atom name, resno (eg. CA 1) or',
     &'         atom name, resno, chain id. (eg. CG2 23 D).',
     &' (If no chain id. is specified then no any chain id will'//
     &                    ' do for a match).',
     &' Numerical chain id''s should be preceeded by a / eg. /2'
      CALL PROMPT( NOUT,
     & 'Which atoms <no more labels>: ')
      READ( NIN, '(A)', ERR= 55555, END= 55555) COL
      CALL VTCLEAR( NOUT)
      IF (INDEX(COL,'      ').EQ.1) GOTO 55555
      
C used freda to decode col
      CALL FREDA(1,80,FL,6)
C info returned
C kl must be at least one kn at least one
C fl(1,1 to 4): brookhaven atom name  
C fn(1)       : residue no 
C and optionally 
C fl(1,1) chain id
C clear col
      COL = ' '
      IF ((KL.LT.1) .OR. (KN.LT.1)) THEN
	WRITE( NOUT, *)
     &'Must specify at least one name & one number!'//CHAR(7)
	GOTO 10
      ELSE
C assume two names + 1 no.
	BRK = FL(1,1)//FL(1,2)//FL(1,3)//FL(1,4)
	IAT = FN(1)
	TCH = '?'
      ENDIF
C chain id specified?
      IF (KL.GE.2) THEN
	TCH = FL(2,1)
C if a backslash specified take 2nd character
	IF (TCH.EQ.'/') TCH = FL(2,2)
      ENDIF

C to find list number for atom use routine grabbed from series_stat
C ssafn2( C4, I, C1, *) will find out whether atom
C name C4 resno I chain C1 exists:
C if it does i will be returned as list no. 
C if not ssafn2 will be returned .false.
      IF (.NOT.SSAFN2( BRK, IAT, TCH,
     &  ATMAX, ATNO, ATBRK, ATRNO, ATCHN) )THEN
	WRITE(NOUT,  '( A,I5, A/ A)' )
     &' Cannot find atom: '//BRK//' residue no:', IAT,
     &' chain: '//TCH,
     &' Please try again!'//CHAR(7)
        GOTO 10
      ENDIF

C user wants to label atom iat
C What label?
C make up default
      WRITE( LAB, '(A,I5)') ATBRK(IAT), ATRNO(IAT)
C eliminate all double spaces do until all gone
15    CONTINUE
C find last non-blank character in default label
	CALL CHREND( LAB, LABEND)
C first (if any double space
        IDUM = INDEX(LAB(1:LABEND),'  ')
	IF (IDUM.GT.0) THEN
	  LAB(IDUM:LABEND) = LAB(IDUM+1:LABEND+1)
          GOTO 15
        ENDIF
C label stripped of all double spaces

C make up prompt
      COL =
     & 'What label do you to place at atom''s position? <'//
     & 			LAB(1:LABEND)//'>:'
      CALL PROMPT( NOUT, COL)
      READ( NIN, '(A)', END= 55555, ERR= 55555) COL
      CALL VTCLEAR( NOUT)

C wants default?
      IF (COL(1:5).NE.'     ') THEN
        LAB = COL
C has specified label in col - find last non-blank character
        CALL CHREND( LAB, LABEND)
C if the last character in label is a '/' then make a space
        IF (LAB(LABEND:LABEND).EQ.'/') LAB(LABEND:LABEND) = ' '
      ENDIF

C ask for positioning integer
      CALL PROMPT( NOUT,
     & 'What position # do you want for label <6>:')
      POSINT = 6
      READ( NIN, '(A)', END= 55555, ERR= 55555) COL
      CALL VTCLEAR( NOUT)
      CALL FREDA(1,80,FL,6)
      IF (KN.GE.1) POSINT = FN(1)
C make sure that it is within limits
      IF (POSINT.GT.9) POSINT = 9
      IF (POSINT.LT.1) POSINT = 1

C finally write the record
      IF (LABEND.EQ.0) THEN
	WRITE( NOUT, '(A)')
     &' ERROR'//CHAR(7)//' cannot plot zero length string!!!'
      ELSE
C move to atom
        RVEC4(1) = 2.
        RVEC4(2) = ATXYZ(1,IAT)
        RVEC4(3) = ATXYZ(2,IAT)
        RVEC4(4) = ATXYZ(3,IAT)
        WRITE( SOUT)  RVEC4
C character record - indicate by 5.0 number of character
C the last record is positioning integer
	RVEC4(1) = 5.
        RVEC4(2) = LABEND
        RVEC4(3) = 0.
        RVEC4(4) = POSINT
        WRITE( SOUT)  RVEC4
C write the bloody thing
	WRITE( SOUT) LAB(1:LABEND)
      ENDIF

C got back up and do next label
      GOTO 10

55555 CONTINUE
      WRITE( NOUT,*)
      STOP 'FORTRAN STOP - labqpt normal completion.'
      END
