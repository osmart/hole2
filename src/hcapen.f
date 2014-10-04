      SUBROUTINE HCAPEN( CENTRE, ENERGY, SECCEN, CAPRAD, 
     &		     	 IAT1, IAT2, DAT2,
     &                   ATMAX, ATNO, ATXYZ, ATVDW, PI)
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
C * (c) 1997 Oliver Smart                    *
C *                                                                  *
C ********************************************************************
C
C Modification history:
C
C Date	Author		Modification
C 05/95	O.S. Smart	Original version
C 12/97 O.S.S.		modification to call to holeen - will introduce
C			cutoff procedure here in the end	
C
C This s/r is an alternative to s/r HOLEEN for the capsule option
C finds the largest radius possible for a capsule starting
C at centre and going to seccen.
C
C energy is returned as minus the effective radius of the capsule
C which is the square root of the area divided by pi

C the point, the second point
      DOUBLE PRECISION		CENTRE(3), SECCEN(3)

C the 'energy' see above 
      DOUBLE PRECISION		ENERGY

C caprad is the real capsule radius
      DOUBLE PRECISION		CAPRAD

C atom list no. with smallest dist-vdw radius, 2nd smallest
C 07/06/94 as iat's may or may not be supplied with previously 
C found numbers then we can use this to speed up the procedure
      INTEGER			IAT1, IAT2, IAT3

C 2nd smallest distance-vdw radius (of iat2)
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

C pi
      DOUBLE PRECISION		PI

C internal vbles

C unit vector & distance between centres
      DOUBLE PRECISION		UJOIN(3), DCENT

C vector from centre to an atom
      DOUBLE PRECISION		RVECT(3)

C dot product
      DOUBLE PRECISION		RDOTU

C loop for atoms
      INTEGER			ACOUNT

C for distance etc.
      DOUBLE PRECISION		DIST

C end of decs ******************

C      write( *, '(a/ 2(a,3f8.3/))')
C     &' debug call to s/r hcapen',
C     &'   centre= ', centre,
C     &'   seccen= ', seccen

C first find out unit vector between the two centres
      UJOIN(1) = SECCEN(1) - CENTRE(1)
      UJOIN(2) = SECCEN(2) - CENTRE(2)
      UJOIN(3) = SECCEN(3) - CENTRE(3)

C find out distance between the two centres
      DCENT = SQRT( UJOIN(1)**2 + UJOIN(2)**2 + UJOIN(3)**2)

C trap zero distance
      IF (DCENT.LT.1E-09) THEN
C got the same point as centre and seccen
C use holeen to give energy - use hardcoded cutsize for now
        CALL HOLEEN( CENTRE, ENERGY, IAT1, IAT2, IAT3, DAT2, DAT3,
     &               ATMAX, ATNO, ATXYZ, ATVDW, 5D0)
C        write(*,'(a,f9.5)')
C     &' Both points the same holeen returns energy= ', energy 
C the capsule radius is minus the energy in this case
        CAPRAD = -ENERGY
      ELSE
        
C make UJOIN a unit vector
        UJOIN(1) = UJOIN(1)/DCENT
        UJOIN(2) = UJOIN(2)/DCENT
        UJOIN(3) = UJOIN(3)/DCENT

C        write(*,'(a,3f8.3/a,f8.3)')
C     &'   ujoin = ', ujoin,
C     &'   dcent = ', dcent


C initialize energy etc.
C N.B. ONLY CHANGE SIGN OF ENERGY ON RETURN 
        ENERGY = 99999.
        IAT1 = -1000
        IAT2 = -1000
        DAT2 = 99999.

C go thru atom by atom
        DO 10 ACOUNT = 1, ATNO

C find distance from atom icount to centre
          RVECT(1) = ATXYZ(1,ACOUNT) - CENTRE(1)
          RVECT(2) = ATXYZ(2,ACOUNT) - CENTRE(2)
          RVECT(3) = ATXYZ(3,ACOUNT) - CENTRE(3)

C find the dot product with ujoin
          CALL DDOT( RVECT, UJOIN, RDOTU)

C          write(*,*) ' atom# ', acount, ' rdotu= ', rdotu

C the distance from atom to capsule is dependent on 
C value of rdotu see oss j008
          IF (RDOTU.LT.0) THEN
            DIST = RVECT(1)**2 + RVECT(2)**2 + RVECT(3)**2
C            write(*,*) 'closest to centre'
          ELSEIF (RDOTU.GT.DCENT) THEN
C closest to second centre
            DIST =  (SECCEN(1)-ATXYZ(1,ACOUNT))**2 +
     &              (SECCEN(2)-ATXYZ(2,ACOUNT))**2 +
     &              (SECCEN(3)-ATXYZ(3,ACOUNT))**2
C            write(*,*) 'closest to seccen'
          ELSE
C closest to point on line joining two centre
C put into rvect
            RVECT(1) = RVECT(1) - UJOIN(1)*RDOTU
            RVECT(2) = RVECT(2) - UJOIN(2)*RDOTU
            RVECT(3) = RVECT(3) - UJOIN(3)*RDOTU
C distance squared
            DIST = RVECT(1)**2 + RVECT(2)**2 + RVECT(3)**2
C            write(*,*) 'closest to line joining'
          ENDIF  

C          write(*,*) ' atom# ', acount, 'distance ', sqrt(dist)

C 7/6/94 avoid unnecessary sqrt's by comparing
C the distance squared (DIST) to the sum of the exisiting
C radius and the van der Waals
          IF (DIST.LT.(ENERGY+ATVDW(ACOUNT))**2) THEN
C this atom provides the constriction
      	    DIST = SQRT( DIST)
C take of vdw radius
      	    DIST = DIST - ATVDW(ACOUNT)
C old iat1 becomes iat2
      	    IAT2 = IAT1
      	    DAT2 = ENERGY
C present atom icount becomes iat1
      	    IAT1 = ACOUNT
      	    ENERGY = DIST
C not smaller than energy but smaller than dat2?
          ELSEIF (DIST.LT.(DAT2+ATVDW(ACOUNT))**2) THEN
            DIST = SQRT( DIST)
C take of vdw radius
            DIST = DIST - ATVDW(ACOUNT)
C present atom icount becomes iat2
      	    IAT2 = ACOUNT
      	    DAT2 = DIST
          ENDIF

10      CONTINUE

C after all that "energy" is the radius of capsule
        CAPRAD = ENERGY
C if the radius is negative then do not proceed with effective radius calc - just return radius
        IF (ENERGY.GT.0.) THEN
C work out area
          ENERGY = PI*ENERGY**2 + 2*ENERGY*DCENT
C effective radius
          ENERGY = SQRT(ENERGY/PI)
        ENDIF
C change sign of energy
        ENERGY = -ENERGY

C do same for DAT2
        DAT2 = PI*DAT2**2 + 2*DAT2*DCENT
        DAT2 = SQRT(DAT2/PI)
C        write(*,*) 'final efective radius is ', energy
C        if (pi.ne.0.) stop ' temp stop in hcapen'
      ENDIF

55555 RETURN
      END
