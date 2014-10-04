      SUBROUTINE HSBXEN( CENTRE, ENERGY, IAT1, IAT2, DAT2,
     &        ATMAX, ATNO, ATXYZ, ATVDW,
     &        ATRAD2, CVECT, LVECT, LBOXDM, SBOXDM, BOXRAD,
     &        NOUT, LERR, PI)
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
C 06/94	O.S. Smart	Original version
C
C
C This s/r finds the spherebox with the largest area normal
C to cvect.  It is based on s/r holeen which finds the largest
C sphere which can fit on the point.
C
C

C the point
      DOUBLE PRECISION		CENTRE(3)

C the 'energy' see above 
      DOUBLE PRECISION		ENERGY

C atom list no. with smallest dist-vdw radius, 2nd smallest
      INTEGER			IAT1, IAT2

C 2nd smallest distance-vdw radius (of iat2)
      DOUBLE PRECISION		DAT2
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

C (vbles required for s/r hsbxen but not needed for holeen)

C to speed up procedure store the distance squared
C from each atom to the centre
      DOUBLE PRECISION		ATRAD2(ATMAX)

C the channel vector (n.b. should be supplied as unit)
C return unchanged
      DOUBLE PRECISION		CVECT(3)

C vector in the direction of the long axis of
C the sphere box
C should be supplied as unit - 
C may be changed on return to cvect^lvect 
C depending on values of lboxdm and sboxdm found
      DOUBLE PRECISION		LVECT(3)

C box length in direction LVECT
C to be calculated in this routine
      DOUBLE PRECISION		LBOXDM

C box length in direction lvect^cvect
C to be calculated in this routine
      DOUBLE PRECISION		SBOXDM

C radius of the sphere box
C to be calculated in this routine
      DOUBLE PRECISION		BOXRAD


C output stream and error indicator to be used if
C an error is found - e.g. cvect and lvect are
C not orthogonal or united
      INTEGER			NOUT
      LOGICAL			LERR	

C pi
      DOUBLE PRECISION		PI


C internal vbles****************

C loop for atoms
      INTEGER			ACOUNT

C for distance etc.
      DOUBLE PRECISION		DIST

C radius of a sphere centred at point
C (used to save unnecessary calc's)
      DOUBLE PRECISION		SPHRAD

C matrix to store the three axes (cvect is#1, lvect#2)
      DOUBLE PRECISION		AXIS(3,3)

C function (-area of spherebox)
      DOUBLE PRECISION		FN

C vbles for stepest descents
C old function
      DOUBLE PRECISION		FNOLD

C vbles to calculate derivs by central differences
      DOUBLE PRECISION		FNPLUS, FNMINU

C derivs 
      DOUBLE PRECISION		SDERIV, LDERIV

C length of deriv
      DOUBLE PRECISION		DERLEN

C step count
      INTEGER			STEPNO

C step length
      DOUBLE PRECISION		STEPL

C variables to swap
      DOUBLE PRECISION		RSWAP

C end of decs ******************


C initialize energy etc.
C N.B. ONLY CHANGE SIGN OF ENERGY ON RETURN 
      ENERGY = 99999.
      DAT2 = 99999.
      SPHRAD = 99999.

C n.b. iat1 and iat2 not worked out in this routine just set to 1
      IAT1 = 1
      IAT2 = 1

C as in s/r holeen first work out sphere radius for centre
C but storeing all the distance squared
C go thru atom by atom
      DO 10 ACOUNT = 1, ATNO

C find distance from atom icount to centre
        DIST =  (CENTRE(1)-ATXYZ(1,ACOUNT))**2 +
     &          (CENTRE(2)-ATXYZ(2,ACOUNT))**2 +
     &          (CENTRE(3)-ATXYZ(3,ACOUNT))**2

C store the radius squared
        ATRAD2(ACOUNT) = DIST

C 7/6/94 avoid unnecessary sqrt's by comparing
C the distance squared (DIST) to the sum of the exisiting
C radius and the van der Waals
        IF (DIST.LT.(SPHRAD+ATVDW(ACOUNT))**2) THEN
C this atom provides the constriction
      	  DIST = SQRT( DIST)
C take of vdw radius
      	  DIST = DIST - ATVDW(ACOUNT)
      	  SPHRAD = DIST
        ENDIF

10    CONTINUE
       

C*      write(*,*) 'centre: ', centre
C*      write(*,*) 'cvect   ', cvect 
C*      write(*,*) 'lvect   ', lvect 
C*      write(*,*) 'sphrad calculated to be ', sphrad

C load up axis etc.
C axis #1 is cvect
      AXIS(1,1) = CVECT(1)
      AXIS(2,1) = CVECT(2)
      AXIS(3,1) = CVECT(3)
C axis#2 is the long axis
      AXIS(1,2) = LVECT(1)
      AXIS(2,2) = LVECT(2)
      AXIS(3,2) = LVECT(3)
C third axis is the cross product of the other two
      CALL DCROSS( AXIS(1,1), AXIS(1,2), AXIS(1,3))
C just check that this has length 1 - if not then stop
      IF ( ABS(1.-(AXIS(1,3)**2+AXIS(2,3)**2+AXIS(3,3)**2))
     &     .GT. 1E-06) THEN
        WRITE( NOUT, *) 'ERROR in s/r hsbxen'
        WRITE( NOUT, *) 'CVECT and LVECT not orthogonal!'
        WRITE( NOUT,*) ' CVECT  = ', CVECT 
        WRITE( NOUT,*) ' LVECT  = ', LVECT 
        LERR = .TRUE.
        GOTO 55555
      ENDIF

C*      write(*,*) 'axis#1', axis(1,1), axis(2,1), axis(3,1)
C*      write(*,*) 'axis#2', axis(1,2), axis(2,2), axis(3,2)
C*      write(*,*) 'axis#3', axis(1,3), axis(2,3), axis(3,3)

C apply crude stepest descents minimization
C start at 0.01, 0.01
      SBOXDM = 0.01
      LBOXDM = 0.01
C take initial step of one hundreth of an angstrom
      STEPL = 1E-02

C do up to 500 steps
      DO 20 STEPNO = 1, 501 

C if we get sboxdm close to zero then set it there
C to minimize other co-ord
        IF ((ABS(SBOXDM).LE.1E-5).AND.(ABS(SBOXDM).NE.0.)) THEN
          SBOXDM = 0.
          SDERIV = 0.
          STEPL = 0.01
C*          write(*,*) 'setting short to zero ', stepno
        ENDIF

        IF (ABS(SBOXDM).GE.1E-5) THEN
C calculate derivative in sboxdm direction
C use central differences
           SBOXDM = SBOXDM - 1E-06
           CALL HSBXFN( FNMINU, BOXRAD, 
     &                CENTRE, ATMAX, ATNO, ATXYZ, ATVDW,
     &                ATRAD2, AXIS, LBOXDM, SBOXDM, PI, SPHRAD)
           SBOXDM = SBOXDM + 2E-06
           CALL HSBXFN( FNPLUS, BOXRAD,
     &                CENTRE, ATMAX, ATNO, ATXYZ, ATVDW,
     &                ATRAD2, AXIS, LBOXDM, SBOXDM, PI, SPHRAD)
           SBOXDM = SBOXDM - 1E-06
           SDERIV = (FNPLUS-FNMINU)/2.E-06
         ENDIF


C the other derivative
C once again set to zero if sufficently close
        IF ((ABS(LBOXDM).LE.1E-5).AND.(ABS(LBOXDM).NE.0.)) THEN
          LBOXDM = 0.
          LDERIV = 0.
          STEPL = 0.01
C*          write(*,*) 'setting long  to zero ', stepno
        ENDIF

        IF (ABS(LBOXDM).GE.1E-5) THEN
          LBOXDM = LBOXDM - 1E-06
          CALL HSBXFN( FNMINU, BOXRAD,
     &               CENTRE, ATMAX, ATNO, ATXYZ, ATVDW,
     &               ATRAD2, AXIS, LBOXDM, SBOXDM, PI, SPHRAD)
          LBOXDM = LBOXDM + 2E-06
          CALL HSBXFN( FNPLUS, BOXRAD,
     &               CENTRE, ATMAX, ATNO, ATXYZ, ATVDW,
     &               ATRAD2, AXIS, LBOXDM, SBOXDM, PI, SPHRAD)
          LBOXDM = LBOXDM - 1E-06
          LDERIV = (FNPLUS-FNMINU)/2.E-06
        ENDIF

C calculate the function
        CALL HSBXFN( FN, BOXRAD,
     &               CENTRE, ATMAX, ATNO, ATXYZ, ATVDW,
     &               ATRAD2, AXIS, LBOXDM, SBOXDM, PI, SPHRAD)

c*        if (mod(stepno-1,10).eq.0) then
c*        write(*, *) ' step ', stepno
c*        write(*,'(a, 4f8.5)') '  short long rad fn ',
c*     &                        sboxdm, lboxdm, boxrad, fn    
c*        write(*, *) ' derivs ', sderiv, lderiv
c*        write(*, *) ' stepl ', stepl
c*        write(*, *) 
c*        endif

C converged? - look at step length and whether co-ords are both zero
        IF ((SBOXDM.EQ.0.).AND.(LBOXDM.EQ.0.)) THEN
C*          write(*,*) 'converged to zero/zero sol stepno= ',stepno
          GOTO 30
        ENDIF

C find length of deriv
        DERLEN = SQRT(LDERIV**2+SDERIV**2)

C take step
        SBOXDM = SBOXDM - (STEPL*SDERIV)/DERLEN
        LBOXDM = LBOXDM - (STEPL*LDERIV)/DERLEN

C has function changed?
        IF (FN.LT.FNOLD) THEN
          STEPL = 1.2*STEPL 
        ELSE
          STEPL = 0.5*STEPL 
        ENDIF
        FNOLD = FN
C converged? has step gone below 1e-05
        IF (STEPL.LT.1E-05) THEN
C*          write(*,*) 'converged stepl= ', stepl, ' stepno= ',stepno
          GOTO 30
        ENDIF

20    CONTINUE

C on converging jump here
30    CONTINUE

C make sure that both the box dimensions are +VE
      SBOXDM = ABS(SBOXDM)
      LBOXDM = ABS(LBOXDM)

C swap long and short?
      IF (SBOXDM.GT.LBOXDM) THEN
C axis#3 is the axis for sboxdm
        LVECT(1) = AXIS(1,3) 
        LVECT(2) = AXIS(2,3) 
        LVECT(3) = AXIS(3,3) 
C*        write(*,*) 'swap l&s lvect= ', lvect
C swap length
        RSWAP =  SBOXDM
        SBOXDM = LBOXDM
        LBOXDM = RSWAP 
      ENDIF

C the energy is minus the root of the area (-fn) over pi rooted
C (effective radius)
C 25/6/94 - if the function is +ve then the energy should be returned as such
C old:      ENERGY = -SQRT(-FN/PI)
      ENERGY = SIGN(SQRT(ABS(FN)/PI),FN)


C*      write(*,*) ' final sboxdm= ', sboxdm
C*      write(*,*) ' final lboxdm= ', lboxdm
C*      write(*,*) ' final boxrad= ', boxrad
C*      write(*,*) ' final fn=     ', fn
C*      write(*,*) ' energy=       ', energy
C*      stop 'temp stop in s/r hsbxen'

55555 RETURN
      END
C
      SUBROUTINE HSBXFN( FN, BOXRAD,
     &             CENTRE, ATMAX, ATNO, ATXYZ, ATVDW,
     &             ATRAD2, AXIS, LBOXDM, SBOXDM, PI, SPHRAD)
      IMPLICIT NONE
C s/r works out the maximum value of the spherebox
C radius (boxrad) for a given set up the boxdim's
C (lboxdm and sboxdm)

C function is returned as minus the area of the sphere box
C normal to cvect (first axis)
      DOUBLE PRECISION		FN

C radius of boxsphere (value which ensures that sphere box
C touches an atom is found in this s/r).
      DOUBLE PRECISION		BOXRAD

C centre of spherebox
      DOUBLE PRECISION		CENTRE(3)

C maximum no. of atoms
C returned unchanged
      INTEGER                   ATMAX

C number of atoms read in from each file:
C returned unchanged
      INTEGER                   ATNO

C co-ordinates
C returned unchanged
      DOUBLE PRECISION          ATXYZ( 3, ATMAX)

C vdw radius of each atom
C returned unchanged
      DOUBLE PRECISION          ATVDW(ATMAX)

C the distance squared from each atom to the centre
C (calculated in s/r hsbxen - return unchanged)
      DOUBLE PRECISION          ATRAD2(ATMAX)

C matrix to store the three axes (cvect is#1, lvect#2)
      DOUBLE PRECISION          AXIS(3,3)

C box lengths long (#2) and short (#3)
      DOUBLE PRECISION		LBOXDM, SBOXDM

C pi
      DOUBLE PRECISION		PI

C radius of sphere at centre
C needed to save time by avoiding unnec calls to s/r sbgrow
C return unchanged
      DOUBLE PRECISION		SPHRAD

C internal vbles ***************

C lengths of sides for spherebox
      DOUBLE PRECISION		BOXDIM(3)

C maximum possible "radius" of spherebox
C (need only call sbgrow if the atom is within this)
      DOUBLE PRECISION 		MAXRAD

C sphere box radius for atom aboxr
      DOUBLE PRECISION		ABOXR

C loop count for atoms
      INTEGER			ACOUNT

C end of decs ******************

C load lengths of spherebox into boxdim
C (axis 1 cvect, 2 lvec and 3 the cross product)
      BOXDIM(2) = ABS(LBOXDM)
      BOXDIM(3) = ABS(SBOXDM)
C length along cvect is the longer
      BOXDIM(1) = MAX(BOXDIM(2),BOXDIM(3))
C boxrad starts at 99999.
      BOXRAD = 99999.


C maximum possible "radius" of spherebox
C (need only call sbgrow if the atom is within this)
C distance to corner of box + the sphere radius
      MAXRAD = SQRT(BOXDIM(1)**2+BOXDIM(2)**2+BOXDIM(3)**2) +
     &         SPHRAD + 1E-03

C*      write(*,*) 'boxdim= ', boxdim
C*      write(*,*) 'maxrad= ', maxrad

C go through atoms
      DO 10 ACOUNT = 1, ATNO

C distance of atom to centre must be less than the
C "maximum radius" of the spherebox + the vdwradius
        IF (ATRAD2(ACOUNT).LT.(MAXRAD+ATVDW(ACOUNT))**2) THEN
C find maximum value of boxrad for this atom=aboxr
          CALL SBGROW( ATXYZ(1,ACOUNT), ATVDW(ACOUNT),
     &                 AXIS, ABOXR, BOXDIM, CENTRE)
          IF (ABOXR.LE.BOXRAD) BOXRAD = ABOXR
C*          write(*,*) 'atom#  ', acount, ' boxrad ', aboxr
        ENDIF

10    CONTINUE

C work out area
C n.b. can be -ve if radius is negative (i.e. overlap)
      FN = PI*BOXRAD*ABS(BOXRAD)+
     &     4.*(BOXDIM(2)*BOXDIM(3)+BOXRAD*(BOXDIM(2)+BOXDIM(3)))

C should return -ve
      FN = -FN
C*      write(*,*) ' final boxrad= ', boxrad
C*      write(*,*) ' final fn=     ', fn

      RETURN
      END
