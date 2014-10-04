      SUBROUTINE PTGEN( DOTDEN, PTMAX, PTNO, PTXYZ, ERROUT, LERR)
      IMPLICIT NONE
C ********************************************************************
C *                                                                  *
C * This software is an unpublished work containing confidential and *
C * proprietary information of Birkbeck College. Use, disclosure,    *
C * reproduction and transfer of this work without the express       *
C * written consent of Birkbeck College are prohibited. This notice  *
C * must be attached to all copies or extracts of the software.      *
C *                                                                  *
C * (c) 1993 Oliver Smart & Birkbeck College, All rights reserved    *
C * (c) 1996 Oliver Smart & Birkbeck College, All rights reserved    *
C *                                                                  *
C ********************************************************************
C
C Modification history:
C
C Date	Author		Modification
C 12/93	O.S. Smart	Original public release in HOLE suite beta1.0
C
C
C this s/r generates dots coordinates ptxyz
C reasonably distributed on a sphere of unit radius

C Dotden controls the number of dots generated
C 2*dotden dots should be generated on a great circle
      INTEGER				DOTDEN

C array bound for number of points
      INTEGER				PTMAX

C number of points found (n.b. should not exceed number of points)
      INTEGER				PTNO

C co-ordinates of points
      DOUBLE PRECISION			PTXYZ( 3, PTMAX)

C stream number for error output, error indicator
      INTEGER				ERROUT
      LOGICAL				LERR

C internal variables *******************

C loop count circle number along Z
      INTEGER				ZCOUNT

C angle from z axis to edge of circle, radius of the circle
C z coordinate of circle
      DOUBLE PRECISION			ZANG, RCIRC, ZCOORD
C numb of dots on a particular circle
      INTEGER				RNUMB
C loop count for angle in circle
      INTEGER				ACOUNT
C xcoord and y coord of point
      DOUBLE PRECISION                  XCOORD, YCOORD
C number to swap
      DOUBLE PRECISION			DUM

C pi
      DOUBLE PRECISION                  PI

C end of declarations ******************

C initialize things
      PI = ACOS(0.)*2.
      PTNO = 0

C will produce points on series of circles with constant z
C there should be 2*dotden dots on a great circle 
C the circles should be distributed with equal angle
      DO 10 ZCOUNT = 0, DOTDEN/2
C angle of the circle to z axis
        ZANG = REAL(ZCOUNT)/REAL(DOTDEN)*PI
C radius of the circle at this angle
        RCIRC = SIN(ZANG)
C z coordinate of the circle
        ZCOORD = COS(ZANG)
C number of points on the circle 
C (radius=1 - number of points = 2*dotden)
C add 0.95 to round up
        RNUMB = REAL(2*DOTDEN)*RCIRC+0.95
C now have to draw rnumb points in x, y plane
        IF (RNUMB.EQ.0) THEN
C top and bottom
          PTNO = PTNO+1
          PTXYZ(1,PTNO) = 0.
          PTXYZ(2,PTNO) = 0.
          PTXYZ(3,PTNO) = ZCOORD
          PTNO = PTNO+1
          PTXYZ(1,PTNO) = 0.
          PTXYZ(2,PTNO) = 0.
          PTXYZ(3,PTNO) = -ZCOORD
        ELSE
C draw rnumb points
          DO 20 ACOUNT = 0, RNUMB-1
C angle from xaxis is (acount/rnumb)*2pi
            XCOORD = RCIRC*COS(2.*PI*REAL(ACOUNT)/REAL(RNUMB))
            YCOORD = RCIRC*SIN(2.*PI*REAL(ACOUNT)/REAL(RNUMB))
C make sure that we don't get one half great circle with points
C and the other without - swap x and y every 2nd time
            IF (MOD(ZCOUNT,2).EQ.0) THEN
              DUM = XCOORD
              XCOORD = YCOORD
              YCOORD = DUM
            ENDIF
C make point at x, y, and +/- z
            IF (PTNO+2.GT.PTMAX) THEN
              WRITE( ERROUT, '(A/ A,I10,A/ A)')
     &' Error in s/r PTGET (making isotropic sphere of dots)',
     &' Dotden specified to be ', DOTDEN, ' causes too many dots',
     &' Either decrease dotden or increase PTMAX'
              LERR = .TRUE.
              GOTO 55555
            ENDIF
            PTNO = PTNO+1
            PTXYZ(1,PTNO) = XCOORD
            PTXYZ(2,PTNO) = YCOORD
            PTXYZ(3,PTNO) = ZCOORD
            PTNO = PTNO+1
            PTXYZ(1,PTNO) = XCOORD
            PTXYZ(2,PTNO) = YCOORD
            PTXYZ(3,PTNO) = -ZCOORD
20        CONTINUE
        ENDIF
10    CONTINUE

55555 RETURN
      END
