      SUBROUTINE HSBXMI( CENTRE, ENERGY, IAT1, IAT2, DAT2,
     &        ATMAX, ATNO, ATXYZ, ATVDW,
     &        ATRAD2, CVECT, LVECT, LBOXDM, SBOXDM, BOXRAD,
     &        NOUT, LERR, PI, NOSTEP, ENDRAD)
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
C 21/6/94 - problem when doing spherebox option
C is that degree of refinement due to Monte Carlo
C is insufficient so further refine lowest energy solution
C using conventional minimization.
C
C Idea in this routine is to optimize centre and lvect.
C This is essentially a 3D problem centre should be
C adjusted in the plane normal to cvect (2D) and lvect should
C be adjusted in the plane (1D - an angle).
C 
C S/r is based on hsbxen - which this routine calls to
C find the best area (lowest "energy") when doing min.
C (in future may combine to do united optimization
C  using simplex method - i016)
C

C the point (adjusted in this routine)
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
C spherebox (must be unit and orthogonal to cvect)
C adjusted in this routine
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

C number of steps of sd applied
      INTEGER			NOSTEP

C 08/07/94 avoid getting out of bounds by stopping if the
C function exceeds the end radius
      DOUBLE PRECISION		ENDRAD

C internal vbles****************

C In this routine need to store two sets of axes
C -the original as supplied and the one of the
C current set of LDISP, SDISP and theta
C matrix to store the three axes (cvect is#1, lvect#2)
      DOUBLE PRECISION		ORIGAX(3,3), PRESAX(3,3)

C store for the original centre
      DOUBLE PRECISION		ORIGCN(3)

C the variables we do the minimization in 
C displacement along original long, short
C and angle from
      DOUBLE PRECISION		LDISP, SDISP, THETA
C sin and cosine of theta
      DOUBLE PRECISION		SINTHE, COSTHE

C derivatives
      DOUBLE PRECISION		LDERIV, SDERIV, TDERIV

C vbles for stepest descents
C old function
      DOUBLE PRECISION		FNOLD

C vbles to calculate derivs by central differences
      DOUBLE PRECISION		FNPLUS, FNMINU

C length of deriv
      DOUBLE PRECISION		DERLEN


C step length
      DOUBLE PRECISION		STEPL

C end of decs ******************


C*      c*write(*,*)
C*      write(*,*) 'top of s/r hsbxmi *************'
C*      write(*,*) 'centre: ', centre
C*      write(*,*) 'cvect   ', cvect 
C*      write(*,*) 'lvect   ', lvect 

C load up the original axis 
C axis #1 is cvect
      ORIGAX(1,1) = CVECT(1)
      ORIGAX(2,1) = CVECT(2)
      ORIGAX(3,1) = CVECT(3)
C axis#2 is the long axis
      ORIGAX(1,2) = LVECT(1)
      ORIGAX(2,2) = LVECT(2)
      ORIGAX(3,2) = LVECT(3)
C third axis is the cross product of the other two
      CALL DCROSS( ORIGAX(1,1), ORIGAX(1,2), ORIGAX(1,3))
C just check that this has length 1 - if not then stop
      IF ( ABS(1.-(ORIGAX(1,3)**2+ORIGAX(2,3)**2+ORIGAX(3,3)**2))
     &     .GT. 1E-06) THEN
        WRITE( NOUT, *) 'ERROR in s/r hsbxen'
        WRITE( NOUT, *) 'CVECT and LVECT not orthogonal!'
        WRITE( NOUT,*) ' CVECT  = ', CVECT 
        WRITE( NOUT,*) ' LVECT  = ', LVECT 
        LERR = .TRUE.
        GOTO 55555
      ENDIF

C*      write(*,*) 'origax#1', origax(1,1), origax(2,1), origax(3,1)
C*      write(*,*) 'origax#2', origax(1,2), origax(2,2), origax(3,2)
C*      write(*,*) 'origax#3', origax(1,3), origax(2,3), origax(3,3)

C store the original centre
      ORIGCN(1) = CENTRE(1)
      ORIGCN(2) = CENTRE(2)
      ORIGCN(3) = CENTRE(3)

C cvect will remain unchanged
      PRESAX(1,1) = ORIGAX(1,1)
      PRESAX(2,1) = ORIGAX(2,1)
      PRESAX(3,1) = ORIGAX(3,1)

C apply crude stepest descents minimization
C take initial step of one hundreth of an angstrom
      STEPL = 1E-04
C initial displacements and angles start at zero
      LDISP = 0.
      SDISP = 0.
      THETA = 0.

C do some s.d.      
      DO 20 NOSTEP = 1, 101 

C work out new centre
        CENTRE(1) = ORIGCN(1) + LDISP*ORIGAX(1,2) + SDISP*ORIGAX(1,3)
        CENTRE(2) = ORIGCN(2) + LDISP*ORIGAX(2,2) + SDISP*ORIGAX(2,3)
        CENTRE(3) = ORIGCN(3) + LDISP*ORIGAX(3,2) + SDISP*ORIGAX(3,3)
C work out new axes
        COSTHE = COS(THETA)
        SINTHE = SIN(THETA)
        PRESAX(1,2) =  ORIGAX(1,2)*COSTHE + ORIGAX(1,3)*SINTHE
        PRESAX(2,2) =  ORIGAX(2,2)*COSTHE + ORIGAX(2,3)*SINTHE
        PRESAX(3,2) =  ORIGAX(3,2)*COSTHE + ORIGAX(3,3)*SINTHE

C find function value at the point
        CALL HSBXEN( CENTRE, ENERGY, IAT1, IAT2, DAT2,
     &        ATMAX, ATNO, ATXYZ, ATVDW,
     &        ATRAD2, CVECT, PRESAX(1,2), LBOXDM, SBOXDM, BOXRAD,
     &        NOUT, LERR, PI)
        IF (LERR) THEN
          WRITE( NOUT, *) 'ERROR has been found on call to '
          WRITE( NOUT, *) '  HSBXEN from HSBXMI (1)'
          GOTO 55555
        ENDIF

C*      if (mod(nostep-1,10).eq.0.) then
C*      write(*,*)
C*      write(*,*) 'sd step# ', nostep
C*      write(*,*) 'centre  ', centre
C*      write(*,*) 'ldisp= ', ldisp
C*      write(*,*) 'sdisp= ', sdisp
C*      write(*,*) 'theta= ', theta
C*      write(*,*) 'presax#1', presax(1,1), presax(2,1), presax(3,1)
C*      write(*,*) 'presax#2', presax(1,2), presax(2,2), presax(3,2)
C*      write(*,*) 'hsbxen returns energy= ', energy
C*      write(*,*) '               lboxdm= ', lboxdm
C*      write(*,*) '               sboxdm= ', sboxdm
C*      write(*,*) '               boxrad= ', boxrad
C*      endif

C work out derivative in direction ldisp
        LDISP = LDISP - 1E-04
C work out new centre (axes stay the same)
        CENTRE(1) = ORIGCN(1) + LDISP*ORIGAX(1,2) + SDISP*ORIGAX(1,3)
        CENTRE(2) = ORIGCN(2) + LDISP*ORIGAX(2,2) + SDISP*ORIGAX(2,3)
        CENTRE(3) = ORIGCN(3) + LDISP*ORIGAX(3,2) + SDISP*ORIGAX(3,3)
C function into fnminu
        CALL HSBXEN( CENTRE, FNMINU, IAT1, IAT2, DAT2,
     &        ATMAX, ATNO, ATXYZ, ATVDW,
     &        ATRAD2, CVECT, PRESAX(1,2), LBOXDM, SBOXDM, BOXRAD,
     &        NOUT, LERR, PI)
        IF (LERR) THEN
          WRITE( NOUT, *) 'ERROR has been found on call to '
          WRITE( NOUT, *) '  HSBXEN from HSBXMI (2)'
          GOTO 55555
        ENDIF
C same in +ve - fn into fnplus
        LDISP = LDISP + 2E-04
C work out new centre (axes stay the same)
        CENTRE(1) = ORIGCN(1) + LDISP*ORIGAX(1,2) + SDISP*ORIGAX(1,3)
        CENTRE(2) = ORIGCN(2) + LDISP*ORIGAX(2,2) + SDISP*ORIGAX(2,3)
        CENTRE(3) = ORIGCN(3) + LDISP*ORIGAX(3,2) + SDISP*ORIGAX(3,3)
        CALL HSBXEN( CENTRE, FNPLUS, IAT1, IAT2, DAT2,
     &        ATMAX, ATNO, ATXYZ, ATVDW,
     &        ATRAD2, CVECT, PRESAX(1,2), LBOXDM, SBOXDM, BOXRAD,
     &        NOUT, LERR, PI)
        IF (LERR) THEN
          WRITE( NOUT, *) 'ERROR has been found on call to '
          WRITE( NOUT, *) '  HSBXEN from HSBXMI (3)'
          GOTO 55555
        ENDIF
C ldisp back to original
        LDISP = LDISP - 1E-04
C derivative in LDISP
        LDERIV = (FNPLUS-FNMINU)/2E-04

C similarly work out derivative in direction sdisp
        SDISP = SDISP - 1E-04
        CENTRE(1) = ORIGCN(1) + LDISP*ORIGAX(1,2) + SDISP*ORIGAX(1,3)
        CENTRE(2) = ORIGCN(2) + LDISP*ORIGAX(2,2) + SDISP*ORIGAX(2,3)
        CENTRE(3) = ORIGCN(3) + LDISP*ORIGAX(3,2) + SDISP*ORIGAX(3,3)
        CALL HSBXEN( CENTRE, FNMINU, IAT1, IAT2, DAT2,
     &        ATMAX, ATNO, ATXYZ, ATVDW,
     &        ATRAD2, CVECT, PRESAX(1,2), LBOXDM, SBOXDM, BOXRAD,
     &        NOUT, LERR, PI)
        IF (LERR) THEN
          WRITE( NOUT, *) 'ERROR has been found on call to '
          WRITE( NOUT, *) '  HSBXEN from HSBXMI (4)'
          GOTO 55555
        ENDIF
        SDISP = SDISP + 2E-04
        CENTRE(1) = ORIGCN(1) + LDISP*ORIGAX(1,2) + SDISP*ORIGAX(1,3)
        CENTRE(2) = ORIGCN(2) + LDISP*ORIGAX(2,2) + SDISP*ORIGAX(2,3)
        CENTRE(3) = ORIGCN(3) + LDISP*ORIGAX(3,2) + SDISP*ORIGAX(3,3)
        CALL HSBXEN( CENTRE, FNPLUS, IAT1, IAT2, DAT2,
     &        ATMAX, ATNO, ATXYZ, ATVDW,
     &        ATRAD2, CVECT, PRESAX(1,2), LBOXDM, SBOXDM, BOXRAD,
     &        NOUT, LERR, PI)
        IF (LERR) THEN
          WRITE( NOUT, *) 'ERROR has been found on call to '
          WRITE( NOUT, *) '  HSBXEN from HSBXMI (5)'
          GOTO 55555
        ENDIF
        SDISP = SDISP - 1E-04
C derivative in SDISP
        SDERIV = (FNPLUS-FNMINU)/2E-04

C work out derivative in theta
C first set centre back to exactly where it should be
        CENTRE(1) = ORIGCN(1) + LDISP*ORIGAX(1,2) + SDISP*ORIGAX(1,3)
        CENTRE(2) = ORIGCN(2) + LDISP*ORIGAX(2,2) + SDISP*ORIGAX(2,3)
        CENTRE(3) = ORIGCN(3) + LDISP*ORIGAX(3,2) + SDISP*ORIGAX(3,3)
C displace theta
        THETA = THETA - 1E-04
C work out new axes
        COSTHE = COS(THETA)
        SINTHE = SIN(THETA)
        PRESAX(1,2) =  ORIGAX(1,2)*COSTHE + ORIGAX(1,3)*SINTHE
        PRESAX(2,2) =  ORIGAX(2,2)*COSTHE + ORIGAX(2,3)*SINTHE
        PRESAX(3,2) =  ORIGAX(3,2)*COSTHE + ORIGAX(3,3)*SINTHE
        CALL HSBXEN( CENTRE, FNPLUS, IAT1, IAT2, DAT2,
     &        ATMAX, ATNO, ATXYZ, ATVDW,
     &        ATRAD2, CVECT, PRESAX(1,2), LBOXDM, SBOXDM, BOXRAD,
     &        NOUT, LERR, PI)
        IF (LERR) THEN
          WRITE( NOUT, *) 'ERROR has been found on call to '
          WRITE( NOUT, *) '  HSBXEN from HSBXMI (6)'
          GOTO 55555
        ENDIF
C now in +ve
        THETA = THETA + 2E-04
C work out new axes
        COSTHE = COS(THETA)
        SINTHE = SIN(THETA)
        PRESAX(1,2) =  ORIGAX(1,2)*COSTHE + ORIGAX(1,3)*SINTHE
        PRESAX(2,2) =  ORIGAX(2,2)*COSTHE + ORIGAX(2,3)*SINTHE
        PRESAX(3,2) =  ORIGAX(3,2)*COSTHE + ORIGAX(3,3)*SINTHE
        CALL HSBXEN( CENTRE, FNPLUS, IAT1, IAT2, DAT2,
     &        ATMAX, ATNO, ATXYZ, ATVDW,
     &        ATRAD2, CVECT, PRESAX(1,2), LBOXDM, SBOXDM, BOXRAD,
     &        NOUT, LERR, PI)
        IF (LERR) THEN
          WRITE( NOUT, *) 'ERROR has been found on call to '
          WRITE( NOUT, *) '  HSBXEN from HSBXMI (7)'
          GOTO 55555
        ENDIF
C back to orig
        THETA = THETA - 1E-04
C derivative in SDISP
        TDERIV = (FNPLUS-FNMINU)/2E-04

C have calculated (long hand!) the three derivatives
C find length of deriv
        DERLEN = SQRT(LDERIV**2+SDERIV**2+TDERIV**2)

C take step
        SDISP = SDISP - (STEPL*SDERIV)/DERLEN
        LDISP = LDISP - (STEPL*LDERIV)/DERLEN
        THETA = THETA - (STEPL*TDERIV)/DERLEN



C has function changed?
        IF (ENERGY.LT.FNOLD) THEN
          STEPL = 1.2*STEPL
        ELSE
          STEPL = 0.5*STEPL
        ENDIF
        FNOLD = ENERGY

C converged when step gets below 1e-05
C 08/07/94 avoid getting out of bounds by stopping if the
C function exceeds the end radius
        IF ( (ENDRAD.LT.-ENERGY) .OR.
     &       (STEPL.LT.1E-05)         ) GOTO 30

C*      if (mod(nostep-1,10).eq.0.) then
C*      write(*,*) 'lderiv= ', lderiv
C*      write(*,*) 'sderiv= ', sderiv
C*      write(*,*) 'tderiv= ', tderiv
C*      write(*,*) 'next step length= ', stepl
C*      endif
20    CONTINUE

C converge here
30    CONTINUE
C*      write(*,*)
C*      write(*,*) 'sd final *********** #', nostep
C*      write(*,*) 'centre  ', centre
C*      write(*,*) 'ldisp= ', ldisp
C*      write(*,*) 'sdisp= ', sdisp
C*      write(*,*) 'theta= ', theta
C*      write(*,*) 'presax#1', presax(1,1), presax(2,1), presax(3,1)
C*      write(*,*) 'presax#2', presax(1,2), presax(2,2), presax(3,2)
C*      write(*,*) 'hsbxen returns energy= ', energy
C*      write(*,*) '               lboxdm= ', lboxdm
C*      write(*,*) '               sboxdm= ', sboxdm
C*      write(*,*) '               boxrad= ', boxrad
C*      stop 'temp stop in s/r hsbxmi'

55555 RETURN
      END
