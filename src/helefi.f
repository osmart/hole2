      SUBROUTINE HELEFI( NOUT, AVGPOT, CVECT, SHORTO,
     &             STRMAX, STRNOP, STRNON, STRC1, STRC2,
     &             ATMAX, ATNO, ATBRK, ATRES,
     &             ATCHN, ATRNO, ATXYZ, ATVDW, ATBND)
      IMPLICIT NONE
C ********************************************************************
C *                                                                  *
C * This software is an unpublished work containing confidential and *
C * proprietary information of Birkbeck College. Use, disclosure,    *
C * reproduction and transfer of this work without the express       *
C * written consent of Birkbeck College are prohibited. This notice  *
C * must be attached to all copies or extracts of the software.      *
C *                                                                  *
C * (c) 1996 Oliver Smart & Birkbeck College, All rights reserved    *
C *                                                                  *
C ********************************************************************
C
C Modification history:
C
C Date	Author		Modification
C 11/96 O.S. Smart      First version
C 
C 5/11/96 Routine to work out the electrostatic potential created along
C pore centre line by formal charges in the system

C passed variables *************

C output stream no. (usually set to 6) RETURN UNCHANGED
      INTEGER			NOUT

C average electrostatic potential as calculated by HELEFI
C excluding HIS - needed for conductance prediction algorithm contained
C in s/r HCAPGR
      DOUBLE PRECISION		AVGPOT

C The channel vector - already united
      DOUBLE PRECISION		CVECT(3)

C store for sphere centres and radii
C store the initial point as no 0 and subseqently points in the +ve
C bit. Points below the initial point are stored in -ve bit.
C maximum number of entries
      INTEGER                   STRMAX
C the no. of entries, +Ve entries -Ve entries
      INTEGER                   STRNOP, STRNON
C the sphere centres or ends of capsule
C n.b. this routine has to work just the same for
C normal spheres (when strc1 = strc2 for each element)
C and capsule option when the centre line is given by (strc1+strc2)/2
      DOUBLE PRECISION          STRC1(3,-STRMAX:STRMAX),
     &                          STRC2(3,-STRMAX:STRMAX)

C 11/95 introduce new variable to reduce text output -
C if oshort =
C 0: full text output
C 1: no in run progress reported
C 2: ditto plus no graph type output - only leaving
C    Minimum radius & conductance calculations.
C 3: All text output other than input card mirroring
C    and error messages turned off
      INTEGER                   SHORTO

C maximum no. of atoms
      INTEGER                   ATMAX

C number of atoms read in from each file:
      INTEGER                   ATNO

C atom names found in Brookhaven file:
      CHARACTER*4               ATBRK(ATMAX)

C residue name in brook
      CHARACTER*3               ATRES(ATMAX)

C chain identifier in brookhaven pdb file
      CHARACTER*1               ATCHN(ATMAX)

C integer residue no.
      INTEGER                   ATRNO(ATMAX)

C co-ordinates
      DOUBLE PRECISION          ATXYZ( 3, ATMAX)

C vdw and bond radii of each atoms
      DOUBLE PRECISION          ATVDW(ATMAX), ATBND(ATMAX)

C internal vbles ***************

C formal charge store
C maximum number of entries
      INTEGER			FCHMAX
      PARAMETER(		FCHMAX = 5000)
C the actual number of entries
      INTEGER			FCHNUM 
C value of the charge in |electron| charge units
      DOUBLE PRECISION          FCHVAL( FCHMAX)
C coordinates of the charge
      DOUBLE PRECISION          FCHXYZ( 3, FCHMAX)
C is this charge a his?
C (do 2calc's one with his +ve the other with his neutral)
      LOGICAL 			FCHHIS( FCHMAX)

C loop count for atoms, for sphere centres, field calc
      INTEGER			ACOUNT, LCOUNT, FCOUNT

C temporary charge
      DOUBLE PRECISION		TCHARG

C keep count of total charge (a) on his's (b) on every thing else
      DOUBLE PRECISION		TOTHIS, TOTNOR

C test centre, the coord
      DOUBLE PRECISION		TSTCEN(3), CCOORD

C electrostatic field due to normal groups, and due to +1HIS
      DOUBLE PRECISION		FELNOR, FELHIS

C vble for distance calc
      DOUBLE PRECISION		DIST
      
C end of decs ******************

C initialize avg pot
      AVGPOT = 0.

C only produce graph info if shorto = 0 or 1
      IF (SHORTO.LE.1) THEN
        WRITE( NOUT, '(A)')
     &' ',
     &' This is routine helefi: will work out electrostatic potential',
     &' produced by formal charges of molecule down the pore centre',
     &' in vacuo',
     &' ',
     &' Identifying formal charges on the molecule:'

C write header
        WRITE( NOUT, '(5X,A4,8X,A8,1X,A1,5X,A5)') 
     &     'atom', 'charge', 'H?', ' coords '
      ENDIF
      
C initialize the charge store array
      FCHNUM = 0.
      TOTHIS = 0.
      TOTNOR = 0.

C go thru atoms finding the formal charges:
      DO 10 ACOUNT = 1, ATNO
      
C default charge is zero - no charge
        TCHARG = 0.0
              
C do we have an ASP? 
        IF     (ATRES(ACOUNT).EQ.'ASP') THEN
C is the atom OD1 or OD2 - if so give it a charge of -0.5
          IF (ATBRK(ACOUNT)(1:2).EQ.'OD') TCHARG = -0.5   
                
C do we have an GLU? 
        ELSEIF (ATRES(ACOUNT).EQ.'GLU') THEN
C is the atom OE1 or OE2 - if so give it a charge of -0.5
          IF (ATBRK(ACOUNT)(1:2).EQ.'OE') TCHARG = -0.5   
         
C do we have an ARG? 
        ELSEIF (ATRES(ACOUNT).EQ.'ARG') THEN
C is the atom NE, NH1 or NH2 - if so give it a charge of +0.333333   
          IF (ATBRK(ACOUNT)(1:2).EQ.'NE') TCHARG = +0.333333   
          IF (ATBRK(ACOUNT)(1:2).EQ.'NH') TCHARG = +0.333333   
         
C do we have an LYS? 
        ELSEIF (ATRES(ACOUNT).EQ.'LYS') THEN
C is the atom NZ  - if so give it a charge of +1.0
          IF (ATBRK(ACOUNT)(1:2).EQ.'NZ') TCHARG = +1.0
         
C do we have an HIS/HID/HIE? 
        ELSEIF ( (ATRES(ACOUNT).EQ.'HIS') .OR.
     &           (ATRES(ACOUNT).EQ.'HIE') .OR.
     &           (ATRES(ACOUNT).EQ.'HID')     ) THEN
C is the atom ND1 or NE  -  if so give it a charge of +0.5
C and indicate it should be regarded as a HIS by multiplying by 1E10
          IF (ATBRK(ACOUNT)(1:2).EQ.'ND') TCHARG = +0.5E10
          IF (ATBRK(ACOUNT)(1:2).EQ.'NE') TCHARG = +0.5E10
     
        ENDIF     

C if TCHARG is not zero then have formal charge so should make another list
C entry
        IF (ABS(TCHARG).GT.0.005) THEN
C do we have enough room?
          IF (FCHNUM.GE.FCHMAX) THEN
            WRITE( NOUT, '(A/ A,I5,A/ A)')
     &' ERROR in s/r helefi',
     &' Have hit array bound limit FCHMAX=', FCHMAX,
     &     ' for number of formal charges identified',
     &' Cannot continue with routine. ' 
            GOTO 55555
          ENDIF
C we have enough room so make this entry
          FCHNUM = FCHNUM + 1
C an a very big positive value for charge indicates HIS
C change to +0.5 and log
          IF (TCHARG.GT.1E9) THEN
            FCHVAL(FCHNUM) = +0.5
            FCHHIS(FCHNUM) = .TRUE.
C keep count of total formal charge with and without HIS
            TOTHIS = TOTHIS + FCHVAL(FCHNUM)
          ELSE
C normal non-his charge
            FCHVAL(FCHNUM) = TCHARG
            FCHHIS(FCHNUM) = .FALSE.
C keep count of total formal charge with and without HIS
            TOTNOR = TOTNOR + FCHVAL(FCHNUM)
          ENDIF
C coordinates of the charge
          FCHXYZ( 1, FCHNUM) = ATXYZ( 1, ACOUNT)
          FCHXYZ( 2, FCHNUM) = ATXYZ( 2, ACOUNT)
          FCHXYZ( 3, FCHNUM) = ATXYZ( 3, ACOUNT)
C echo to screen
          IF (SHORTO.LE.1) 
     &      WRITE( NOUT, '(A4,1X,A3,1X,A1,I5,F8.3,2X,L1,3F8.3)')
     &      ATBRK(ACOUNT), ATRES(ACOUNT), ATCHN(ACOUNT), ATRNO(ACOUNT),
     &      FCHVAL(FCHNUM), FCHHIS(FCHNUM),
     &      FCHXYZ( 1, FCHNUM), FCHXYZ( 2, FCHNUM), FCHXYZ( 3, FCHNUM)
        ENDIF
      

C end of going thru atoms.
10    CONTINUE

      IF (SHORTO.LE.1) THEN
        WRITE( NOUT, '(A/2(A,F8.3/))' )
     &' ',
     &' Total charge  excluding HIS = ', TOTNOR,
     &'               including HIS = ', TOTNOR+TOTHIS


C write header of data
        WRITE( NOUT, '(A)')
     &' ',
     &' Data for graph of potential versus channel coordinate',
     &' potential given in units of kcal/mol per proton charge'
        WRITE( NOUT, '(3A12)') 'ch_coord', 'potexclHIS', 'potinclHIS'
      ENDIF


C Have got all the charges set up in the appropriate array
C now go up sphere centre line
      DO 20 LCOUNT = -STRNON, STRNOP
C load the mid-point into tstcen
        TSTCEN(1) = 0.5*(STRC1(1,LCOUNT)+STRC2(1,LCOUNT))
        TSTCEN(2) = 0.5*(STRC1(2,LCOUNT)+STRC2(2,LCOUNT))
        TSTCEN(3) = 0.5*(STRC1(3,LCOUNT)+STRC2(3,LCOUNT))
C coordinate of point
        CALL DDOT( TSTCEN, CVECT, CCOORD)

C find electrostatic field at point tstcen caused by charges
C held in array FCH***
        FELNOR = 0D0
        FELHIS = 0D0
        DO 30 FCOUNT = 1, FCHNUM
C find distance between charge centre and tstcen
          DIST = (TSTCEN(1)-FCHXYZ(1,FCOUNT))**2 +
     &           (TSTCEN(2)-FCHXYZ(2,FCOUNT))**2 +
     &           (TSTCEN(3)-FCHXYZ(3,FCOUNT))**2
          DIST = SQRT(DIST)  
C add potential component to either the histidine count or the normal
          IF (FCHHIS(FCOUNT)) THEN
C n.b. units of potential are kcal/mol per proton charge
C (value of constant taken from tic)
            FELHIS = FELHIS + 331.850*FCHVAL(FCOUNT)/DIST
          ELSE
C normal charge
            FELNOR = FELNOR + 331.850*FCHVAL(FCOUNT)/DIST
          ENDIF
30      CONTINUE

C have worked out potential so write it out
        IF (SHORTO.LE.1)
     &    WRITE( NOUT, '(3F12.5)') CCOORD, FELNOR, FELNOR+FELHIS

C keep total so we can work out the average
        AVGPOT = AVGPOT + FELNOR

20    CONTINUE

C work out average potential
      AVGPOT = AVGPOT/(STRNON+STRNOP+1)


C return here      
55555 CONTINUE
C flush output stream
      IF (NOUT.EQ.6) CALL FLUSH6

      RETURN
      END
