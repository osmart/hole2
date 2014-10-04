      PROGRAM VDWDOT
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
C 05/94 O.S. Smart      Original version - input based on HOLE b1.1
C 28/02/97 O.S.S.	Release HOLE2 beta001
C 19/7/99 O.S.S.	Bug around "DO 20"
C
C This program is intended to produce a quanta plot file
C of the "internal" vdw dot surface.  Input is from the 
C stream 5 and is based on HOLE - with the same keywords used.
C
C The program says a dot is "internal" if it is opposite an
C atom i.e. if the dot is extended into a line it hits some atom.
C
C Keywords which are applicable (same as HOLE):
C COORD filename (pdb coord input) MUST BE SPECIFIED
C RADIUS filename (van der Waals radius for each atom) MUST BE SPECIFIED
C PLTOUT filename (output qpt file) MUST BE SPECIFIED 
C DOTDEN number (the dot density) May be specified - default 10.
C
C One key word which is extra to those used in HOLE may be specified:
C DCUT real 
C This is to save time in the routine.  The routine will only check
C for overlap and opposition between a dot
C and a particular atom if the distance between the atom centres
C is under the sum of the van der Waals radii and dcut.
C Bascally DCUT must be set to over twice the maximum radius of
C the channel - a value of 6 angstroms would be alright for gramicidin.
C The default is 10 angstroms.


C input and output streams
      INTEGER			NIN, NOUT
      PARAMETER(		NIN  = 5)
      PARAMETER(		NOUT = 6)

C input coordinate filename
C (must be specified)
      CHARACTER*200		FCOORD
C input radius filename
C (must be specified)
      CHARACTER*200		FRADIU
C stream for input
      INTEGER			SIN

C hydra binary plot output file, a stream for it
      CHARACTER*200		FHYDRA
      INTEGER			SHYDRA

C a dot density for dot surface 
C same definition as in hydra - value between 5 and 30
C default to 10
      INTEGER			DOTDEN

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
      PARAMETER(		ATMAX = 10000)

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

C in this routine record whether particular atom is 'active'
C - this means that it is sufficently close to be checked
      LOGICAL			ATACTV(ATMAX)

C need to know the distance2 between an atom under consideration
C and active atoms and the unit vector from the atom under consideration
C to the target atom.
      DOUBLE PRECISION		ATD2(ATMAX), ATUDSP(3, ATMAX) 

C line for output
      CHARACTER*132		LINE

C program uses a sphere of isotropically distributed points
C on a sphere of unit radius. PTMAX is the array bound,
C PTNO is the number of dots (dependent on dotden)
      INTEGER                   PTMAX, PTNO
      PARAMETER(                PTMAX = 4000)
      DOUBLE PRECISION          PTXYZ( 3, PTMAX)

C cutoff distance - see discussion at top of program
      DOUBLE PRECISION		DCUT

C loop counts for atoms
      INTEGER			ACONE, ACTWO

C distance squared, and that found with vdw radius increased
      DOUBLE PRECISION		DIST2
c#, DISPLU

C loop count to go thru points
      INTEGER			PCOUNT

C position of a dot
C position with a small increase in the vdw radius
      DOUBLE PRECISION		DOTXYZ(3)
c#, PLUXYZ(3)

C if lclos is true it means that adding a small amount to vdw makes dot
C gets closer to some atom
      LOGICAL			LCLOS

C cosine of angle subtended by dot from other atom
      DOUBLE PRECISION		COSA

C 11/95 introduce a string which will list residues to be ignored on read
      CHARACTER*80		IGNRES
C need to record on the initial read of the pdb file the 
C original atom numbers of the each atom from the pdb file
C - as we ignore some residues in the read.
C oatno(0) is the total number of original atoms in the pdb file
C oatno(1) is the original atom number of the stored atom#1 etc.
      INTEGER			OATNO(0:ATMAX)

C end of decs ******************

C greetings
      WRITE( NOUT,  '(A)')
     &' *** Program vdwdot ***',
     &' Written by Oliver Smart at Birkbeck College from May 1994.',
     &' Copyright 1994,1997 by Oliver Smart and Birkbeck College',
     &' Copyright 2004 by Oliver Smart ',
     &' Program modification number 2.2 001'


C write link time of program to screen
      CALL VERTIM( NOUT)


C default values
      DOTDEN = 10
      FCOORD = 'NONE'
      FRADIU = 'NONE'
      FHYDRA = 'NONE'
      LERR  = .FALSE.
      LDBUG = .FALSE.
      DCUT = 10.0
C ignore residue string
      IGNRES = '........................................'//
     &         '........................................'

C read control variables from NIN
      CALL VDRCON( NIN, NOUT, FCOORD, FRADIU, FHYDRA, 
     &             DOTDEN, DCUT, LERR, LDBUG)

C if error found flag is returned true then stop
      IF (LERR) THEN
	WRITE(NOUT,*) 'ERROR found reading control vbles (s/r VDRCON)'
	GOTO 55555
      ENDIF

C have read all control values
C s/r holset
C (a) checks that everything which needs to be specified has been
C (b) mirror values back to user
C but does not open files
      CALL VDSET( NOUT, FCOORD, FRADIU, FHYDRA, 
     &             DOTDEN, DCUT, LERR)

C if error found flag is returned true then stop
      IF (LERR) THEN
	WRITE(NOUT,*) 'ERROR in VDWDOT setup (s/r VDSET)'
	GOTO 55555
      ENDIF

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
     &	MAXLST, BNDNO, BNDBRK, BNDR, VDWNO, VDWBRK, VDWRES, VDWR)
      IF (LERR) GOTO 55555

C tell user extact filename and no of records read
      INQUIRE( SIN, NAME = LINE)
      WRITE(NOUT,99900) BNDNO, VDWNO, LINE(1:INDEX(LINE,'   ')-1)

C close RADIUS file
      CLOSE( SIN)
     
C open pdb co-ordinate file
      IF (.NOT.OPENRO( SIN, FCOORD, NOUT)) THEN
C cannot open file
	WRITE(NOUT,*) '*** ERROR ***'
	WRITE(NOUT,*)'Cannot open pdb co-ord input file:'
	WRITE(NOUT,*) FCOORD(1:INDEX(FCOORD,'     ')-1)
	LERR = .TRUE.
	GOTO 55555
      ENDIF

C read pdb atom records and set up vdW and bond radii for each atom
C (the two logical control flags set up vdw and bond radii
C  for each atom - do not need bond radii in hole)
      CALL TSATR(  SIN, NOUT, LERR, LDBUG, .TRUE., .FALSE.,
     &	ATMAX, ATNO, ATBRK, ATRES, ATCHN, ATRNO, ATXYZ, ATVDW, ATBND,
     &	MAXLST, BNDNO, BNDBRK, BNDR, VDWNO, VDWBRK, VDWRES, VDWR,
     &  IGNRES, OATNO)
      IF (LERR) GOTO 55555

C tell user no. of atoms read
C tell user extact filename and no of records read
      INQUIRE( SIN, NAME = LINE)
      WRITE(NOUT,99910) ATNO, LINE(1:INDEX(LINE,'   ')-1)
C close co-ord input file
      CLOSE( SIN)

C atoms read and vdw radii setup

C load up the set of points =lly distributed in space
C density determined by dotden
      CALL PTGEN( DOTDEN, PTMAX, PTNO, PTXYZ, NOUT, LERR)
      IF (LERR) THEN
        WRITE( NOUT,'(A)')
     &' Have found error in s/r PTGEN',
     &'   Presumably dotden set too high - aborting'
        GOTO 55555
      ENDIF

C open the plot file - if we need to
      IF (FHYDRA(1:4).NE.'NONE') THEN
C find stream no.
	CALL STREAM( SHYDRA)
C open file
	OPEN( SHYDRA, FILE= FHYDRA, STATUS= 'NEW',
     &		      FORM= 'UNFORMATTED', ERR= 900)
C tell user:
        INQUIRE( SHYDRA, NAME = FHYDRA)
	WRITE( NOUT,  '(/1X,A)')
     &	  'Have opened hydra binary plot output file:  '//
     &	  FHYDRA(1:INDEX( FHYDRA,'      ')-1)

C dot dots  quanta colour 7, qplot number 17 (indicated by -55 as Y)
	WRITE(SHYDRA) 1.0, 7.0, -55.0, 17.0

      ELSE
	WRITE(NOUT,*) 'Will not produce graphical output'
      ENDIF

      WRITE( NOUT, *) '*** Drawing dot surface'

C Start the real work of routine
      DO 10 ACONE = 1, ATNO
C tell user rough progess
         IF (MOD(ACONE,100).EQ.0) 
     &     WRITE( NOUT, '(A,I5)') 
     &     ' Working out dots for atom no ', ACONE

C work out dots for atom list# acone
C first work out which atoms are active - can restrict
        DO 20 ACTWO = 1, ATNO
C No need to consider the atom itself 
C to get around bug spotted by Thomas.mcdermott@ucd.ie
	  IF (ACTWO.EQ.ACONE) GOTO 20
C distance squared between centres
          DIST2 = (ATXYZ(1,ACONE)- ATXYZ(1,ACTWO))**2 +
     &            (ATXYZ(2,ACONE)- ATXYZ(2,ACTWO))**2 +
     &            (ATXYZ(3,ACONE)- ATXYZ(3,ACTWO))**2 
C for atom actwo to need checking for overlap
C the distance between atom centres must be less than
C the sum of dcut and the van der waals radii
          IF (DIST2.LT.(DCUT+ATVDW(ACONE)+ATVDW(ACTWO))**2) THEN
            ATACTV(ACTWO) = .TRUE.
            ATD2(ACTWO) = DIST2
            DIST2 = SQRT(DIST2)
            ATUDSP(1,ACTWO) = (ATXYZ(1,ACTWO)-ATXYZ(1,ACONE))/DIST2
            ATUDSP(2,ACTWO) = (ATXYZ(2,ACTWO)-ATXYZ(2,ACONE))/DIST2
            ATUDSP(3,ACTWO) = (ATXYZ(3,ACTWO)-ATXYZ(3,ACONE))/DIST2
C            write(nout,*) atbrk(actwo), atchn(actwo), atrno(actwo)
          ELSE
            ATACTV(ACTWO) = .FALSE.
          ENDIF
20      CONTINUE

C check each point on sphere for atom# acone
        DO 30 PCOUNT= 1, PTNO
C xyz position for dot
           DOTXYZ(1) = ATXYZ(1,ACONE)+ATVDW(ACONE)*PTXYZ(1,PCOUNT)
           DOTXYZ(2) = ATXYZ(2,ACONE)+ATVDW(ACONE)*PTXYZ(2,PCOUNT)
           DOTXYZ(3) = ATXYZ(3,ACONE)+ATVDW(ACONE)*PTXYZ(3,PCOUNT)
c#C work out xyz for dot slightly further out
c#           PLUXYZ(1)=ATXYZ(1,ACONE)+(1E-04+ATVDW(ACONE))*PTXYZ(1,PCOUNT)
c#           PLUXYZ(2)=ATXYZ(2,ACONE)+(1E-04+ATVDW(ACONE))*PTXYZ(2,PCOUNT)
c#           PLUXYZ(3)=ATXYZ(3,ACONE)+(1E-04+ATVDW(ACONE))*PTXYZ(3,PCOUNT)
C reset closer indicator
           LCLOS = .FALSE.

C look at distance to all other atoms
C work away from atom in list i.e.
C if we are dealing with acone=5
C look at 4,6 then 3,7 then 2,8 then 1, 9 then 10, 11, 12 etc.
            DO 40 ACTWO = 1, MAX(ATNO-ACONE,ACONE-1)
C first look at whether point is within the sphere of atom# acone+actwo 
                 IF (ACONE+ACTWO.LE.ATNO) THEN
C is the atom active?
                   IF (ATACTV(ACONE+ACTWO)) THEN
C distance squared between dot and atom# acone+actwo
                     DIST2 =
     &  (DOTXYZ(1)-ATXYZ(1,ACONE+ACTWO))**2 +
     &  (DOTXYZ(2)-ATXYZ(2,ACONE+ACTWO))**2 +
     &  (DOTXYZ(3)-ATXYZ(3,ACONE+ACTWO))**2 
C do we have overlap? - if so jump to next point 
                     IF (DIST2.LT.ATVDW(ACONE+ACTWO)**2) GOTO 30
                     IF (.NOT.LCLOS) THEN
C work out cosine of angle between the displacement vector of the
C dot and the unit vector from atom acone to (acone+actwo)
                         CALL DDOT( PTXYZ(1,PCOUNT), 
     &                     ATUDSP(1,ACONE+ACTWO), COSA)
C if the dot product is greater than zero then work out
C distance made by line from first atom through dot in
C right handed triangle to second atom - see h151
                           IF (COSA.GT.0) THEN
                              DIST2 = ATD2(ACONE+ACTWO)*
     &                               (1./(COSA**2)-1.)
C if this distance is less than the vdw radii of the atom
C then dot is internal
                              IF (DIST2.LT.ATVDW(ACONE+ACTWO)**2)
     &				LCLOS = .TRUE.
                           ENDIF
c#C is the slightly further out dot closer to atom than the dot itself?
c#                       DISPLU =
c#     &  (PLUXYZ(1)-ATXYZ(1,ACONE+ACTWO))**2 +
c#     &  (PLUXYZ(2)-ATXYZ(2,ACONE+ACTWO))**2 +
c#     &  (PLUXYZ(3)-ATXYZ(3,ACONE+ACTWO))**2
c#                      IF (DISPLU.LT.DIST2) LCLOS = .TRUE.
                     ENDIF
                   ENDIF
                 ENDIF
C now look at whether point is within the sphere of atom# acone-actwo
                 IF (ACONE-ACTWO.GE.1) THEN
                   IF (ATACTV(ACONE-ACTWO)) THEN
C distance squared between dot and atom# acone-actwo
                     DIST2 =
     &  (DOTXYZ(1)-ATXYZ(1,ACONE-ACTWO))**2 +
     &  (DOTXYZ(2)-ATXYZ(2,ACONE-ACTWO))**2 +
     &  (DOTXYZ(3)-ATXYZ(3,ACONE-ACTWO))**2
C do we have overlap? - if so jump to next point
                     IF (DIST2.LT.ATVDW(ACONE-ACTWO)**2) GOTO 30
                     IF (.NOT.LCLOS) THEN
C work out cosine of angle between the displacement vector of the
C dot and the unit vector from atom acone to (acone-actwo)
                         CALL DDOT( PTXYZ(1,PCOUNT),
     &                     ATUDSP(1,ACONE-ACTWO), COSA)
C if the dot product is greater than zero then work out
C distance made by line from first atom through dot in
C right handed triangle to second atom - see h151
                           IF (COSA.GT.0) THEN
                              DIST2 = ATD2(ACONE-ACTWO)*
     &                               (1./(COSA**2)-1.)
C if this distance is less than the vdw radii of the atom
C then dot is internal
                              IF (DIST2.LT.ATVDW(ACONE-ACTWO)**2)
     &                          LCLOS = .TRUE.
                           ENDIF

c#C is the slightly further out dot closer to atom than the dot itself?
c#                       DISPLU =
c#     &  (PLUXYZ(1)-ATXYZ(1,ACONE-ACTWO))**2 +
c#     &  (PLUXYZ(2)-ATXYZ(2,ACONE-ACTWO))**2 +
c#     &  (PLUXYZ(3)-ATXYZ(3,ACONE-ACTWO))**2
c#                        IF (DISPLU.LT.DIST2) LCLOS = .TRUE.
                     ENDIF
                   ENDIF
                 ENDIF
40          CONTINUE 
C have looked thru all necessary atoms
C and do not have overlap
C if the closer to indicator is true plot
C allow the full van der Waals surface to be produced if the distance
C dcut is equal to zero
            IF (LCLOS.OR.(DCUT.LE.0.)) WRITE(SHYDRA) 
     &      4., REAL(DOTXYZ(1)), REAL(DOTXYZ(2)), REAL(DOTXYZ(3))
30      CONTINUE
C end of point# pcount
10    CONTINUE
C end of atom# acone


55555 STOP 'FORTRAN STOP / VDWDOT: normal completion'

C error cannot open hydra binary plot output file
900   CONTINUE
      WRITE( NOUT,  '(20(A/))')
     &	' ERROR',
     &	' cannot open hydra binary plot output file:  '//
     &	 FHYDRA(1:INDEX( FHYDRA,'      ')-1)
      LERR = .TRUE.
      GOTO 55555

99900 FORMAT(/
     &' Have read',I6,' bond radius records and',/
     &'          ',I6,' vdW radius records.',/
     &' From file: ',A)
99910 FORMAT(/
     &' Have read',I6,' atom records.',/
     &' From file: ',A)
      END
