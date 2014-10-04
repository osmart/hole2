      SUBROUTINE SBGROW( ATMXYZ, ATMVDW,
     &                   AXIS, RADMAX, BOXDIM, CENTRE)
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
C * (c) 1996 Oliver Smart & Birkbeck College, All rights reserved    *
C *                                                                  *
C ********************************************************************

C This s/r finds value the radmax of a "sphere-box" (oss i005) with 
C fixed boxdim etc. so that an atom of radius atmvdw centred on
C atmxyz is touched.

C position and radius of atom
      DOUBLE PRECISION			ATMXYZ(3), ATMVDW

C centre of sphere box
      DOUBLE PRECISION			CENTRE(3)

C unit vector giving directions of axes which define sphere box
C first axis is vector (axis(1,1),axis(2,1),axis(3,1)
C N.B. no check is made in this s/r that axes are unit
C and orthogonal - This must be made certain in calling routine.
      DOUBLE PRECISION			AXIS(3,3)

C radius of sphere box, its additional dimensions along the three axis
      DOUBLE PRECISION			RADMAX, BOXDIM(3)

C internal vbles ***********************

C a distance
      DOUBLE PRECISION 			DIST

C the vector from centre to point (cartesian)
      DOUBLE PRECISION			DISP(3)

C components of DISP along axis(*,1), axis(*,2) and axis(*,3)
      DOUBLE PRECISION			COMP(3)

C closest point in "axis" units
      DOUBLE PRECISION			AXISCL(3)

C x,y,z loop count
       INTEGER				XCOUNT

C end of decs **************************

C the vector from centre to point
      DISP(1) = ATMXYZ(1) - CENTRE(1)
      DISP(2) = ATMXYZ(2) - CENTRE(2)
      DISP(3) = ATMXYZ(3) - CENTRE(3)

C work out components along the various axes
      CALL DDOT( DISP, AXIS(1,1), COMP(1))
      CALL DDOT( DISP, AXIS(1,2), COMP(2))
      CALL DDOT( DISP, AXIS(1,3), COMP(3))
C*      write(*,*) 'comp''s ', comp

C closest point on (or inside) box in axis units
      DO 10 XCOUNT = 1, 3
        IF (COMP(XCOUNT).GT.0.) THEN
          AXISCL(XCOUNT) = MIN(COMP(XCOUNT), BOXDIM(XCOUNT))
        ELSE
          AXISCL(XCOUNT) = MAX(COMP(XCOUNT),-BOXDIM(XCOUNT))
        ENDIF
10    CONTINUE

C make comp's vector from axiscl to point
      COMP(1) = COMP(1) - AXISCL(1)
      COMP(2) = COMP(2) - AXISCL(2)
      COMP(3) = COMP(3) - AXISCL(3)
C*      write(*,*) 'comp vector ', comp

C work out length of this
      DIST = SQRT(COMP(1)**2+ COMP(2)**2 + COMP(3)**2)
C*      write(*,*) ' dist', dist
C maximum radius given by:
C dist = radmax + atmvdw
      RADMAX = DIST-ATMVDW
C n.b. may be negative if atom overlap the box

      RETURN
      END

