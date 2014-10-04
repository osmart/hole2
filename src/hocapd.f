      SUBROUTINE HOCAPD( 
     &         CCENT1, CCENT2, POINT, DIST2, CUVEC, CLEN)
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
C * (c) 1995 Oliver Smart & Birkbeck College, All rights reserved    *
C * (c) 1996 Oliver Smart & Birkbeck College, All rights reserved    *
C *                                                                  *
C ********************************************************************
C
C Modification history:
C
C Date  Author          Modification
C 10/95 O.S. Smart      Original version
C 07/96 O.S.S.		trap zero distance between centres
C
C This s/r is for use with a capsule. Centres CCENT1 and CCENT2
C it returns the distance squared from POINT to the closest
C point on the capsule centre line.
C
C To save time CUVEC the unit vector in the direction of the
C channel and CLEN can be supplied - if they are found to be
C zero these will be found in this routine
      DOUBLE PRECISION		CCENT1(3), CCENT2(3), POINT(3),
     &				DIST2, CUVEC(3), CLEN

C internal vbles ***************

C local copies of CUVEC and CLEN
      DOUBLE PRECISION		LCUVEC(3), LCLEN

C the vector from ccent1 to point
      DOUBLE PRECISION		PVEC(3), PLEN

C end of decs ******************

C are CUVEC and CLEN supplied
      IF (CLEN.NE. 0.) THEN
        LCLEN = CLEN
        LCUVEC(1) = CUVEC(1)
        LCUVEC(2) = CUVEC(2)
        LCUVEC(3) = CUVEC(3)
      ELSE
C work these out
        LCUVEC(1) = CCENT2(1) - CCENT1(1)
        LCUVEC(2) = CCENT2(2) - CCENT1(2)
        LCUVEC(3) = CCENT2(3) - CCENT1(3)
        LCLEN = SQRT(LCUVEC(1)**2+LCUVEC(2)**2+LCUVEC(3)**2)
        IF (LCLEN.GT.0.) THEN
          LCUVEC(1) =  LCUVEC(1)/LCLEN
          LCUVEC(2) =  LCUVEC(2)/LCLEN
          LCUVEC(3) =  LCUVEC(3)/LCLEN
        ENDIF
      ENDIF

C find vector from CCENT1 to POINT
      PVEC(1) = POINT(1) - CCENT1(1) 
      PVEC(2) = POINT(2) - CCENT1(2)
      PVEC(3) = POINT(3) - CCENT1(3)

C now work out the dot product of this vector to ccent
      CALL dDOT( LCUVEC, PVEC, PLEN)

C If plen is smaller than zero then point is closest to ccent1
C also trap zero dist
      IF (PLEN.LE.0.) THEN
        DIST2 = PVEC(1)**2 + PVEC(2)**2 + PVEC(3)**2
      ELSEIF (PLEN.GE.LCLEN) THEN
C this means that the point is closest to ccent2
        DIST2 =  (POINT(1)-CCENT2(1))**2 +
     &           (POINT(2)-CCENT2(2))**2 +
     &           (POINT(3)-CCENT2(3))**2 
      ELSE
C this means point is closest to a position on the centre line
C which is PLEN fro CCENT1.  The distance squared is:
        PVEC(1) = PVEC(1) - PLEN*LCUVEC(1) 
        PVEC(2) = PVEC(2) - PLEN*LCUVEC(2) 
        PVEC(3) = PVEC(3) - PLEN*LCUVEC(3)
        DIST2 = PVEC(1)**2 + PVEC(2)**2 + PVEC(3)**2
      ENDIF

      RETURN
      END
