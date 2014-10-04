      SUBROUTINE HOLLIN( SHYDRA, CENTRE, IAT, ATMAX, ATXYZ, ATVDW)
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
C this s/r draws a line from atom iat towards centre
C distance ATVDW(IAT)

C hydra plot file stream no.
      INTEGER			SHYDRA

C the sphere centre
      DOUBLE PRECISION		CENTRE(3)

C the atom no.
      INTEGER			IAT

C max no. of atoms - array bound,
C coords of atoms, vdw radii
      INTEGER			ATMAX
      DOUBLE PRECISION		ATXYZ(3, ATMAX),
     &				ATVDW(ATMAX)

C vector for write
      REAL			RVEC4(0:3)
C vector to work things out
      DOUBLE PRECISION		DISP(3)

C end of decs ******************

C move to IAT
      RVEC4(0) = 2.0
      RVEC4(1) = ATXYZ(1,IAT)
      RVEC4(2) = ATXYZ(2,IAT)
      RVEC4(3) = ATXYZ(3,IAT)
      WRITE(SHYDRA) RVEC4
C find unit vector in direction IAT -> lowcen
      DISP(1) = CENTRE(1) - ATXYZ(1,IAT)
      DISP(2) = CENTRE(2) - ATXYZ(2,IAT)
      DISP(3) = CENTRE(3) - ATXYZ(3,IAT)
C unit this vector
      CALL dUVEC2( DISP)
C draw to point atvdw along this vector
      RVEC4(0) = 3.0
      RVEC4(1) = ATXYZ(1,IAT) + DISP(1)*ATVDW(IAT)
      RVEC4(2) = ATXYZ(2,IAT) + DISP(2)*ATVDW(IAT)
      RVEC4(3) = ATXYZ(3,IAT) + DISP(3)*ATVDW(IAT)
      WRITE(SHYDRA) RVEC4

55555 RETURN
      END
