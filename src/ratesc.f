      SUBROUTINE RATESC( RCOORD, PHI, THETA, CPOINT, 
     &                   RATE, DRATEDTHE, DRATEDPHI, XYZCOR,
     &                   IAT1, IAT2, IAT3, DAT2, DAT3,
     &                   ATMAX, ATNO, ATXYZ, ATVDW, CUTSIZE,
     &			 PENDIS, STRMAX, STRNOP, STRNON, STRCEN, STRRAD,
     &			 LHORR, SSHELL)
      IMPLICIT NONE
C ********************************************************************
C *                                                                  *
C * This software is an unpublished work containing confidential and *
C * proprietary information of Oliver Smart. Use,           *
C * disclosure, reproduction and transfer of this work without the   *
C * express written consent of Oliver Smart are prohibited. *
C * This notice must be attached to all copies or extracts of the    *
C * software.                                                        *
C *                                                                  *
C * (c) 1998 Oliver Smart                    *
C *                                                                  *
C ********************************************************************
C
C Modification history:
C
C Date	Author		Modification
C 05/98	O.S. Smart	Original version.

C this s/r (a) converts rcoord, phi, theta, cpoint to cartesian coords xyzcor
C          (b) finds rate the rat function value at this point
C          (c) finds the derivatives of rate wrt to the and phi numerically

C passed arguements **************

C spherical polar coords
      DOUBLE PRECISION		RCOORD, PHI, THETA
C polar coords work from cpoint "an initial point in the channel"
      DOUBLE PRECISION          CPOINT(3)

C RATE is the third most distant atomic vdw surface
C minus the closest - always +VE, zero when
C have got to locally best squeeze through.
      DOUBLE PRECISION		RATE

C derivatives of rate wrt to phi and theta
      DOUBLE PRECISION	 	DRATEDTHE, DRATEDPHI
C cartesian coords      
      DOUBLE PRECISION		XYZCOR(3)

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

C cut off list control vble, -ve: no cutoff lists used in calculation
C otherwise this is the additional radius above the sphere radius
C to be used for cutoff list (see s/r holeen)
      DOUBLE PRECISION          CUTSIZE
C penalty distance if this point is less than this distance to an already
C stored point then add on a penalty
      DOUBLE PRECISION		PENDIS
      
C store for sphere centres and radii
C store the initial point as no 0 and subseqently points in the +ve
C bit. Points below the initial point are stored in -ve bit.
C maximum number of entries
      INTEGER                   STRMAX
C the no. of entries, +Ve entries -Ve entries
      INTEGER                   STRNOP, STRNON
C the centres and radii
      DOUBLE PRECISION          STRCEN(3,-STRMAX:STRMAX),
     &                          STRRAD(-STRMAX:STRMAX)

C choice of objective function
C lmord: true - then use normal hole max radius
C        false - then use rat function third most distant
      LOGICAL			LHORR
       
C For s/r raten cut down on the number of records needed to go 
C through to check overlap by storring the start number of this shell -
C n.b. assumes that shells are further apart than pendist
      INTEGER			SSHELL
       
C
C internal vbles ***************

C vbles for central difference derivs
      DOUBLE PRECISION		RPLUS, RMINUS, DELTA
      
C end of decs ******************

C work out derivatives using central differences and step of 1e-06
C should be eps1/3 where eps is the machine precision
      DELTA = 1D-06
C first wrt to phi
C displace +VE
      XYZCOR(1) = CPOINT(1) + RCOORD*COS(THETA+DELTA)*SIN(PHI)
      XYZCOR(2) = CPOINT(2) + RCOORD*SIN(THETA+DELTA)*SIN(PHI)
      XYZCOR(3) = CPOINT(3) + RCOORD*COS(PHI)
C function value to RPLUS
      CALL RATEN( XYZCOR, RPLUS, IAT1, IAT2, IAT3, DAT2, DAT3,
     &                    ATMAX, ATNO, ATXYZ, ATVDW, CUTSIZE,
     &			 PENDIS, STRMAX, STRNOP, STRNON, STRCEN, STRRAD,
     &			 LHORR, SSHELL)
C now -ve
      XYZCOR(1) = CPOINT(1) + RCOORD*COS(THETA-DELTA)*SIN(PHI)
      XYZCOR(2) = CPOINT(2) + RCOORD*SIN(THETA-DELTA)*SIN(PHI)
      XYZCOR(3) = CPOINT(3) + RCOORD*COS(PHI)
C function value to RMINUS
      CALL RATEN( XYZCOR, RMINUS, IAT1, IAT2, IAT3, DAT2, DAT3,
     &                    ATMAX, ATNO, ATXYZ, ATVDW, CUTSIZE,
     &			 PENDIS, STRMAX, STRNOP, STRNON, STRCEN, STRRAD,
     &			 LHORR, SSHELL)
C the derivative
      DRATEDTHE = (RPLUS-RMINUS)/(2*DELTA)

C now phi 
      XYZCOR(1) = CPOINT(1) + RCOORD*COS(THETA)*SIN(PHI+DELTA)
      XYZCOR(2) = CPOINT(2) + RCOORD*SIN(THETA)*SIN(PHI+DELTA)
      XYZCOR(3) = CPOINT(3) + RCOORD*COS(PHI+DELTA)

      CALL RATEN( XYZCOR, RPLUS, IAT1, IAT2, IAT3, DAT2, DAT3,
     &                    ATMAX, ATNO, ATXYZ, ATVDW, CUTSIZE,
     &			 PENDIS, STRMAX, STRNOP, STRNON, STRCEN, STRRAD,
     &			 LHORR, SSHELL)     
      XYZCOR(1) = CPOINT(1) + RCOORD*COS(THETA)*SIN(PHI-DELTA)
      XYZCOR(2) = CPOINT(2) + RCOORD*SIN(THETA)*SIN(PHI-DELTA)
      XYZCOR(3) = CPOINT(3) + RCOORD*COS(PHI-DELTA)
      CALL RATEN( XYZCOR, RMINUS, IAT1, IAT2, IAT3, DAT2, DAT3,
     &                    ATMAX, ATNO, ATXYZ, ATVDW, CUTSIZE,
     &			 PENDIS, STRMAX, STRNOP, STRNON, STRCEN, STRRAD,
     &			 LHORR, SSHELL)

C the derivative
      DRATEDPHI = (RPLUS-RMINUS)/(2*DELTA)

C finally just find function value
C convert spherical polars to cartesian
      XYZCOR(1) = CPOINT(1) + RCOORD*COS(THETA)*SIN(PHI)
      XYZCOR(2) = CPOINT(2) + RCOORD*SIN(THETA)*SIN(PHI)
      XYZCOR(3) = CPOINT(3) + RCOORD*COS(PHI)
C call to  RATE
      CALL RATEN( XYZCOR, RATE, IAT1, IAT2, IAT3, DAT2, DAT3,
     &                    ATMAX, ATNO, ATXYZ, ATVDW, CUTSIZE,
     &			 PENDIS, STRMAX, STRNOP, STRNON, STRCEN, STRRAD,
     &			 LHORR, SSHELL)

55555 RETURN
      END
