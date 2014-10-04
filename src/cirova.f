      SUBROUTINE CIROVA( RAD1, RAD2, DCEN, AREA, PI)
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

C this s/r calculates the overlap area of two circles radius RAD1
C and RAD2. Their centres are distance DCEN apart.  
C The result is returned as AREA.

C passed variables *********************

C see opening comment - leave all unchanged expect area on return
      DOUBLE PRECISION			RAD1, RAD2, DCEN, AREA

C constant PI
      DOUBLE PRECISION			PI

C internal vbles ***********************

C two distances, two angles
      DOUBLE PRECISION			B, C, ALPHA, BETA

C end of decs **************************

C possibilities
C (a) no overlap
      IF (DCEN.GT.(RAD1+RAD2)) THEN
        AREA = 0.

C (b) circle 2 is completly within circle 1
      ELSEIF ((DCEN+RAD2).LE.RAD1) THEN
        AREA = PI*(RAD2**2)

C (c) circle 1 is completly within circle 2
      ELSEIF ((DCEN+RAD1).LE.RAD2) THEN
        AREA = PI*(RAD1**2)

C (d) a partial overlap has occured
      ELSE
C formulae on j100 lab-book
        B = (RAD2**2 + DCEN**2 - RAD1**2)/(2.*DCEN)
        ALPHA  = ACOS((DCEN-B)/RAD1)
        BETA   = ACOS(B/RAD2)
        C = RAD2*SIN(BETA)
        AREA = ALPHA*(RAD1**2) + BETA*(RAD2**2) - DCEN*C
      ENDIF

C return here
      RETURN
      END
