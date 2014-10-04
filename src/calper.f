      SUBROUTINE CALPER( NOUT, SHORTO, CVECT, PERPVE, PERPVN)
      IMPLICIT NONE
C ********************************************************************
C *                                                                  *
C * This software is an unpublished work containing confidential and *
C * proprietary information of Oliver Smart. Use, disclosure,    *
C * reproduction and transfer of this work without the express       *
C * written consent of Oliver Smart are prohibited. This notice  *
C * must be attached to all copies or extracts of the software.      *
C *                                                                  *
C * (c) 2000 Oliver Smart *
C *                                                                  *
C ********************************************************************
C
C s/r to calculate/mirror 2 vectors normal to channel vector
C
C Modification history:
C
C Date  Author          Modification
C 01/00 O.S.S.    	first version

C passed vbles *****************

C output stream number
      INTEGER			NOUT

C 11/95 introduce new variable to reduce text output -
C if oshort =
C 0: full text output
C 1: no in run progress reported
C 2: ditto plus no graph type output - only leaving
C    Minimum radius & conductance calculations.
C 3: All text output other than input card mirroring
C    and error messages turned off
      INTEGER                   SHORTO

C a vector in the direction of the channel
C must be a unit vector - return unchanged
      DOUBLE PRECISION          CVECT(3)
C unit vectors in plane normal to cvect
C needed to a number of routines - originally h2dmap but now conn
C and addend
C these are normal - first is east direction (x on plane)
C                    and second is north     (y on plane)
C TO BE CALCULATE IN THIS S/R
      DOUBLE PRECISION		PERPVE(3), PERPVN(3)

C dot product between cvect and PERPVE
      DOUBLE PRECISION		CDOTP

C end of decs ******************

C work out "East" and "North" vectors perpendicular to cvect
C this code is adapted from s/r h2dmap  
      IF ( (ABS(CVECT(2)).LT.1E-06) .AND.
     &      (ABS(CVECT(3)).LT.1E-06)       ) THEN
        PERPVE(1) = 0.
        PERPVE(2) = 1.
        PERPVE(3) = 0.
      ELSE
        PERPVE(1) = 1.
        PERPVE(2) = 0.
        PERPVE(3) = 0.
      ENDIF
C now PERPVE must be at right angles to CVECT - so find the component
C in common
      CALL DDOT( CVECT, PERPVE, CDOTP)
C as CVECT is a unit vector then can easily correct
      PERPVE(1) = PERPVE(1) - CDOTP*CVECT(1)
      PERPVE(2) = PERPVE(2) - CDOTP*CVECT(2)
      PERPVE(3) = PERPVE(3) - CDOTP*CVECT(3)
C make sure that this is a unit vector
      CALL DUVECT2( PERPVE)
C now get a vector which is normal both to CVECT and PERPVE
      CALL DCROSS( PERPVE, CVECT, PERPVN)

C tell user vectors used to define rotation
      IF (SHORTO.LT.2) WRITE( NOUT, '(A/ A,3F8.3/ A,3F8.3)')
     &'  Some routines (e.g. conn) need vectors normal to cvect: ',
     &'     EAST Zero degrees angle is along vector ', PERPVE,
     &'     NORTH  90 degrees angle is along vector ', PERPVN
C have north and south vectors

      RETURN
      END
