      SUBROUTINE HOLEEN( CENTRE, ENERGY, IAT1, IAT2, IAT3, DAT2, DAT3,
     &                   ATMAX, ATNO, ATXYZ, ATVDW, CUTSIZE)
      IMPLICIT NONE
      SAVE
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
C * (c) 1997 Oliver Smart                    *
C *                                                                  *
C ********************************************************************
C
C Modification history:
C
C Date	Author		Modification
C 12/93	O.S. Smart	Original public release in HOLE suite beta1.0
C 06/94 O.S. Smart	modified to speed up the procedure by
C                       avoiding unnecessary sqrt's
C 12/97 O.S.S.		Cutoff procedure to further speed calculation
C			and introduction to DAT3.  n.b. call arguements 
C			have been changed compared to old versions.
C 07/98 O.S.S.		Avoid **2 by work vbles
C
C
C this s/r finds the largest radius for a sphere centered at
C point CENTRE without vdW overlap with the atoms supplied.
C Energy is returned as minus this number.
C The procedure is accomplished by looking at the
C distance to centre-vdw radius for each atom 
C and finding the smallest value.
C n.b. the energy may become positive if centre is within
C an atoms vdw radius.
C

C the point
      DOUBLE PRECISION		CENTRE(3)

C the 'energy' see above 
      DOUBLE PRECISION		ENERGY

C atom list no. with smallest dist-vdw radius, 2nd smallest
C 07/06/94 as iat's may or may not be supplied with previously 
C found numbers then we can use this to speed up the procedure
C Dec 97 add iat3 
      INTEGER			IAT1, IAT2, IAT3

C 2nd/3rd smallest distance-vdw radius (of iat2/3)
      DOUBLE PRECISION		DAT2, DAT3
C maximum no. of atoms
C returned unchanged
      INTEGER			ATMAX

C number of atoms read in from each file:
C returned unchanged
      INTEGER			ATNO

C co-ordinates
C returned unchanged
      DOUBLE PRECISION		ATXYZ( 3, ATMAX)

C vdw radius of each atom
C returned unchanged
      DOUBLE PRECISION		ATVDW(ATMAX)

C internal vbles

C loop for atoms
      INTEGER			ACOUNT, CCOUNT

C upper array bound
      INTEGER			UPLIM

C for distance etc.
      DOUBLE PRECISION		DIST

C adaptation for distance list to speed calculation from Dec 97
C
C turn on cutoff procedure by setting cutsize positive 
C if not do simple all atom comparison
C
C control of size of the store vble -
C at any stage make cutdist = last rad + cutsize
C This is a passed vble - controlled by a cutsize card read
C DO NOT CHANGE as may be fixed arguement - introduce passed vble
      DOUBLE PRECISION		CUTSIZE, VCUTSIZ
C the centre for cutoff list
      DOUBLE PRECISION		CUTCENTRE(3)
C will store the list numbers of all atoms within "cutdist" of cutcentre
C (i.e. they touch or cross over this sphere)
      DOUBLE PRECISION		CUTDIST
C the store for the cutoff
C maximum number of atoms
      INTEGER			CUTSTOREMAX
      PARAMETER(		CUTSTOREMAX = 30000)
C the number of list numbers in the store is element zero, all other 
C elements are the list numbers
      INTEGER			CUTSTORE(0:CUTSTOREMAX)
      
C working vbles
      DOUBLE PRECISION		WORK1, WORK2, WORK3
C value for cutmaxvdw to ensure correct initialization
      DATA 			CUTSTORE(0) /0/


C end of decs ******************

C set varying cutsize
      VCUTSIZ = CUTSIZE

C jump back to here if the cutoff list is recalculated
57575 CONTINUE

C initialize energy etc.
C N.B. ONLY CHANGE SIGN OF ENERGY ON RETURN 
      ENERGY = 99999.
      DAT2 = 99999.
      DAT3 = 99999.
      IAT1 = -1000
      IAT2 = -1000
      IAT3 = -1000


C main loop either (a) look through all atoms
C               or (b) look cutoff list 
      IF (VCUTSIZ.LT.0) THEN
        UPLIM = ATNO
      ELSE 
        UPLIM = CUTSTORE(0)
      ENDIF

C go thru atom by atom/list
      DO 10 CCOUNT = 1, UPLIM
C atom number to check is:
        IF (VCUTSIZ.LT.0) THEN
          ACOUNT = CCOUNT
        ELSE 
          ACOUNT = CUTSTORE(CCOUNT)
        ENDIF

C find distance from atom icount to centre
C old line:	
c        DIST =  (CENTRE(1)-ATXYZ(1,ACOUNT))**2 +
c     &          (CENTRE(2)-ATXYZ(2,ACOUNT))**2 +
c     &          (CENTRE(3)-ATXYZ(3,ACOUNT))**2
        WORK1 = CENTRE(1)-ATXYZ(1,ACOUNT)
	WORK2 = CENTRE(2)-ATXYZ(2,ACOUNT)
	WORK3 = CENTRE(3)-ATXYZ(3,ACOUNT)
        DIST = WORK1*WORK1 + WORK2*WORK2 + WORK3*WORK3
	
C 7/6/94 avoid unnecessary sqrt's by comparing
C the distance squared (DIST) to the sum of the exisiting
C radius and the van der Waals
C old line       
C        IF (DIST.LT.(ENERGY+ATVDW(ACOUNT))**2) THEN
       WORK1 = ENERGY+ATVDW(ACOUNT)
       WORK2 = WORK1*WORK1
       IF (DIST.LT.WORK2) THEN
C this atom provides the constriction
          DIST = SQRT( DIST)
C take of vdw radius
      	  DIST = DIST - ATVDW(ACOUNT)
C old iat2 becomes iat3
      	  IAT3 = IAT2
      	  DAT3 = DAT2
C old iat1 becomes iat2
      	  IAT2 = IAT1
      	  DAT2 = ENERGY
C present atom icount becomes iat1
      	  IAT1 = ACOUNT
          ENERGY = DIST
C not smaller than energy but smaller than dat2?
C old line
C        ELSEIF (DIST.LT.(DAT2+ATVDW(ACOUNT))**2) THEN
        ELSEIF (DIST.LT.(DAT2+ATVDW(ACOUNT))*
     &                  (DAT2+ATVDW(ACOUNT))  ) THEN
          DIST = SQRT( DIST)
C take of vdw radius
          DIST = DIST - ATVDW(ACOUNT)
C old iat2 becomes iat3
          IAT3 = IAT2
          DAT3 = DAT2
C present atom acount becomes iat2
          IAT2 = ACOUNT
          DAT2 = DIST
          
C not smaller than energy but smaller than dat3?
C old line
C        ELSEIF (DIST.LT.(DAT3+ATVDW(ACOUNT))**2) THEN
        ELSEIF (DIST.LT.(DAT3+ATVDW(ACOUNT))*
     &                  (DAT3+ATVDW(ACOUNT))) THEN
          DIST = SQRT( DIST)
C take of vdw radius
          DIST = DIST - ATVDW(ACOUNT)
C present atom acount becomes iat2
          IAT3 = ACOUNT
          DAT3 = DIST            
        ENDIF

10    CONTINUE

C if we are doing cutoff
      IF (VCUTSIZ.GE.0.) THEN
C is the list calculation ok?
C find distance between centre and cutcentre
        DIST = (CENTRE(1) - CUTCENTRE(1))**2 +
     &         (CENTRE(2) - CUTCENTRE(2))**2 +
     &         (CENTRE(3) - CUTCENTRE(3))**2
        DIST = SQRT(DIST)
C add on the maximum radius found (in energy) to check find 
C furthest point on sphere from cutcentre
C - use DAT3 (the distance to third most distant atom)
C to make sure this is correctly calculated
        DIST = DIST + DAT3

C check to see whether this dist exceeds the safe limit
        IF (DIST.GT.CUTDIST) CUTSTORE(0) = 0

C also cause recalculation if radius has got much smaller
        IF (ENERGY.LT.0.3*(CUTDIST-VCUTSIZ)) THEN
          CUTSTORE(0) = 0
C          write(*,*) 'debug - recalc due to reduction in radius'
        ENDIF

C has list been cleared?
        IF (CUTSTORE(0).EQ.0) THEN
C must recalulate
C what radius to use?
          CUTDIST = DAT3 + VCUTSIZ
          CUTCENTRE(1) = CENTRE(1)
          CUTCENTRE(2) = CENTRE(2)
          CUTCENTRE(3) = CENTRE(3)
C go thru atom by atom
          DO 610 ACOUNT = 1, ATNO

C find distance from atom icount to centre
            DIST =  (CUTCENTRE(1)-ATXYZ(1,ACOUNT))**2 +
     &              (CUTCENTRE(2)-ATXYZ(2,ACOUNT))**2 +
     &              (CUTCENTRE(3)-ATXYZ(3,ACOUNT))**2

C unnecessary sqrt's by comparing
C the distance squared (DIST) to the sum of the exisiting
C radius and the van der Waals
            IF (DIST.LT.(CUTDIST+ATVDW(ACOUNT))**2) THEN
C this atom cuts the sphere
C enough room?
              IF(CUTSTORE(0)+1.GT.CUTSTOREMAX) THEN
                WRITE(*,*) 'WARNING ARRAY BOUND CUTSTOREMAX REACHED'
                WRITE(*,*) 'WILL CONTINUE WITHOUT USING CUTOFF (slow)'
                VCUTSIZ = -1.
                GOTO 57575
              ENDIF
C store it
              CUTSTORE(0) = CUTSTORE(0)+1
              CUTSTORE(CUTSTORE(0)) = ACOUNT
            ENDIF
C end of going thru atoms
610       CONTINUE
C end of recalulating list
c           write(*,*) 'debug new list'
c           write(*,*) 'debug cutcentre(1) = ', cutcentre(1)
c           write(*,*) 'debug cutcentre(2) = ', cutcentre(2)
c           write(*,*) 'debug cutcentre(3) = ', cutcentre(3)
c           write(*,*) 'debug number = ', cutstore(0)
c           write(*,*) 'debug cutdist, VCUTSIZ = ', cutdist, VCUTSIZ
C redo whole calculation - but only if the recalulation not caused by
C too large cutoff list
          IF (ENERGY.GT.0.3*(CUTDIST-VCUTSIZ)) GOTO 57575
        ENDIF
      ENDIF




C change sign of energy
      ENERGY = -ENERGY

55555 RETURN
      END
