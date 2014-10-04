      SUBROUTINE HONEWP( NEWCEN, CURCEN, CVECT, MCLEN) 
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
C This s/r finds a new point NEWCEN on the plane through
C CURCEN defined by channel vector CVECT with a maximum
C displacement from CURCEN of MCLEN.
C Code used to be in the main program of HOLE but seperate out

C passed variables

C see initial message, n.b. only newcen to be changed in this s/r
      DOUBLE PRECISION			NEWCEN(3), CURCEN(3), 
     &					CVECT(3), MCLEN

C internal variables *******************

C a displacement vector, a working number
      DOUBLE PRECISION			DISP(3), WORK

C end of decs ***************************

C find a random 3d unit vector put into DISP
      CALL dURAN3( DISP)
C make the vector orthogonal to CVECT
      CALL dDOT( CVECT, DISP, WORK)
C disp = disp - cvect(cvect.disp) (c036)
C - a vector in the plane normal to cvect - maximum length 1
      DISP(1) =  DISP(1) - CVECT(1)*WORK
      DISP(2) =  DISP(2) - CVECT(2)*WORK
      DISP(3) =  DISP(3) - CVECT(3)*WORK
C unit this vector
      CALL dUVEC2( DISP)

C make random length, maximum MCLEN
      CALL DRAND( WORK)
      DISP(1) =  DISP(1)*MCLEN*WORK
      DISP(2) =  DISP(2)*MCLEN*WORK
      DISP(3) =  DISP(3)*MCLEN*WORK

C take this step
      NEWCEN(1) = CURCEN(1) + DISP(1)
      NEWCEN(2) = CURCEN(2) + DISP(2)
      NEWCEN(3) = CURCEN(3) + DISP(3)

      RETURN
      END
