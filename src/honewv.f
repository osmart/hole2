      SUBROUTINE HONEWV( NEWLVC, CURLVC, CVECT) 
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
C Date  Author          Modification
C 05/94 O.S. Smart      Original version
C
C This s/r finds a new long axis vector NEWLVC
C which must lie in plane defined by CVECT.
C The vector is biased towards the current LVC 
C n.b. it must be returned as a unit vector
C
C n.b. cvect must be supplied as unit and curlvc must be
C orthogonal to it (and preferably unit)

C passed variables

C see initial message, n.b. only newlvc to be changed in this s/r
      DOUBLE PRECISION			NEWLVC(3), CURLVC(3), CVECT(3)

C internal variables *******************

C a working number
      DOUBLE PRECISION			WORK

C end of decs ***************************

C find a random 3d unit vector put into NEWLVC
      CALL dURAN3( NEWLVC)
C make the vector orthogonal to CVECT
C WHICH MUST BE A UNIT VECTOR
      CALL dDOT( CVECT, NEWLVC, WORK)
C disp = disp - cvect(cvect.disp) (c036)
C - a vector in the plane normal to cvect - maximum length 1
      NEWLVC(1) =  NEWLVC(1) - CVECT(1)*WORK
      NEWLVC(2) =  NEWLVC(2) - CVECT(2)*WORK
      NEWLVC(3) =  NEWLVC(3) - CVECT(3)*WORK

C make this vector unit length
      CALL dUVEC2( NEWLVC)

C make this a random length - maximum 1.6 - see h154
      CALL DRAND( WORK)
      NEWLVC(1) =  1.6*NEWLVC(1)*WORK
      NEWLVC(2) =  1.6*NEWLVC(2)*WORK
      NEWLVC(3) =  1.6*NEWLVC(3)*WORK

C add on the existing vector
      NEWLVC(1) = NEWLVC(1) + CURLVC(1)
      NEWLVC(2) = NEWLVC(2) + CURLVC(2)
      NEWLVC(3) = NEWLVC(3) + CURLVC(3)

C unit this vector
      CALL dUVEC2( NEWLVC)

      RETURN
      END
