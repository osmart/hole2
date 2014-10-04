      SUBROUTINE SBGEN( MDMAX, MDNO, MDREC, AXIS, BOXRAD, BOXDIM,
     &                  CENTRE, NOUT, LERR)
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

C This s/r generates move draw records for a "sphere-box" (oss i005)

C move/draw records 
C MDMAX: Maximum number set as a parameter in previous routine
C MDNO:  total number of records
C MDREC: first number should be 2 for a moveto, 3 for a draw to,
C        other 3 x, y, z of record
      INTEGER				MDMAX, MDNO
      DOUBLE PRECISION			MDREC(0:3,MDMAX)

C centre of sphere box
      DOUBLE PRECISION			CENTRE(3)

C unit vector giving directions of axes which define sphere box
C first axis is vector (axis(1,1),axis(2,1),axis(3,1)
C N.B. no check is made in this s/r that axes are unit
C and orthogonal - This must be made certain in calling routine.
      DOUBLE PRECISION			AXIS(3,3)

C radius of sphere box, its additional dimensions along the three axis
      DOUBLE PRECISION			BOXRAD, BOXDIM(3)

C output stream # for errors and logical indicator
      INTEGER				NOUT
      LOGICAL				LERR

C internal vbles ***********************

C the axis# of the particular section, the other two
      INTEGER				XCOUNT, XPLUS1, XPLUS2

C vble to allow sections to be done in -ve then +ve direction
      INTEGER				PLMN1, PLMN2, PLMN3

C a move to, draw to record in axis units
      DOUBLE PRECISION			AXMV(3), AXDW(3)
C in cartesian
      DOUBLE PRECISION			CARTMV(3), CARTDW(3)


C store the sin and cosine for 5,10,15... 90 degrees
C increment parameter to allow easy change
      INTEGER				ANGMAX, ANGNO
      PARAMETER(			ANGMAX = 100)
      DOUBLE PRECISION			ANGSIN(ANGMAX), ANGCOS(ANGMAX)
      DOUBLE PRECISION			ANGSTP
      PARAMETER(			ANGSTP = 5.)
C angle, loop count for angle
      DOUBLE PRECISION			ANGLE
      INTEGER				ACOUNT
C number pi
      DOUBLE PRECISION			PI

C end of decs **************************

C clear all previous records
      MDNO = 0

C on first call then must load up angle sin/cos storage arrays.
      IF (ANGNO.EQ.0) THEN
        PI = 2.*ASIN(1.)
        ANGNO = NINT(90./ANGSTP)
        DO 60 ACOUNT = 1, ANGNO
C angle in rad's
          ANGLE = (PI/180.)*ANGSTP*ACOUNT
          ANGSIN(ACOUNT) = SIN(ANGLE)
          ANGCOS(ACOUNT) = COS(ANGLE)
C*          write(*,*) angle, angsin(acount), angcos(acount)
60      CONTINUE          
C end of working out sin/cos arrays
      ENDIF

C draw sections in plane normal to axis 1, then 2 and 3
      DO 10 XCOUNT = 1, 3
C need to know what the other two axes are called
        XPLUS1 = XCOUNT + 1 - 3*(XCOUNT/3)
        XPLUS2 = XCOUNT + 2 - 3*((XCOUNT+1)/3)
C*        write(*,*) 'xcount,+1,+2 ', xcount, xplus1, xplus2

C need to do sections in both +ve and -ve directions
        DO 20 PLMN1 = -1, 1, 2
C*          write(*,*) 'direction (plmn1)', plmn1
C check that we will not overfill arrays
C will do 4 lines = 8 move/draws
C        +4 circles = 4*(angno+1) move/draws
          IF ( MDNO+8+4*(ANGNO+1).GT.MDMAX) THEN
            WRITE( NOUT, *) 'ERROR in s/r sbgen'
            WRITE( NOUT, *) 'array bound MDMAX exceeded'
            LERR = .TRUE.
            GOTO 55555
          ENDIF

C the displacement along axis xcount should be the box dim
C either +ve or -ve
          AXMV(XCOUNT) = PLMN1*BOXDIM(XCOUNT)
          AXDW(XCOUNT) = AXMV(XCOUNT)

C first do line parrallel to xplus2 in +ve xplus1 direction
          AXMV(XPLUS1) = BOXDIM(XPLUS1) + BOXRAD
          AXMV(XPLUS2) = BOXDIM(XPLUS2)
          AXDW(XPLUS1) = AXMV(XPLUS1)
          AXDW(XPLUS2) = -AXMV(XPLUS2)
C find the corresponding co-ords in cartesian co-ords.
          CALL SBCART( AXMV, CARTMV, CENTRE, AXIS)
          CALL SBCART( AXDW, CARTDW, CENTRE, AXIS)
C load up records for line (s/r after this one)
          CALL LOADLN( MDMAX, MDNO, MDREC, CARTMV, CARTDW)

C now do line in -ve xplus1 direction
          AXMV(XPLUS1) = -AXMV(XPLUS1)
          AXDW(XPLUS1) = AXMV(XPLUS1)
          CALL SBCART( AXMV, CARTMV, CENTRE, AXIS)
          CALL SBCART( AXDW, CARTDW, CENTRE, AXIS)
          CALL LOADLN( MDMAX, MDNO, MDREC, CARTMV, CARTDW)
          
C 3rd do line parrallel to xplus1 in +ve xplus2 direction
          AXMV(XPLUS1) = BOXDIM(XPLUS1)
          AXMV(XPLUS2) = BOXDIM(XPLUS2) + BOXRAD
          AXDW(XPLUS1) = -AXMV(XPLUS1)
          AXDW(XPLUS2) = AXMV(XPLUS2)
          CALL SBCART( AXMV, CARTMV, CENTRE, AXIS)
          CALL SBCART( AXDW, CARTDW, CENTRE, AXIS)
          CALL LOADLN( MDMAX, MDNO, MDREC, CARTMV, CARTDW)

C change xplus2's sign for 4th
          AXMV(XPLUS2) = -AXMV(XPLUS2)
          AXDW(XPLUS2) = AXMV(XPLUS2)
          CALL SBCART( AXMV, CARTMV, CENTRE, AXIS)
          CALL SBCART( AXDW, CARTDW, CENTRE, AXIS)
          CALL LOADLN( MDMAX, MDNO, MDREC, CARTMV, CARTDW)

C now do the four circular sections 
C (-,-) (-,+) (+,-) then (+,+)
          DO 30 PLMN2 = -1, 1, 2
            DO 40 PLMN3 = -1, 1, 2
C first thing is a move
              AXMV(XPLUS1) = PLMN2*(BOXDIM(XPLUS1)+BOXRAD)
              AXMV(XPLUS2) = PLMN3*BOXDIM(XPLUS2)
C convert to cartesian
              CALL SBCART( AXMV, CARTMV, CENTRE, AXIS)
              MDNO = MDNO + 1
              MDREC(0,MDNO) = 2.
              MDREC(1,MDNO) = CARTMV(1)
              MDREC(2,MDNO) = CARTMV(2)
              MDREC(3,MDNO) = CARTMV(3)
C go through angle records
              DO 50 ACOUNT = 1, ANGNO
                AXDW(XPLUS1) = PLMN2*(BOXDIM(XPLUS1) + 
     &                                ANGCOS(ACOUNT)*BOXRAD)
                AXDW(XPLUS2) = PLMN3*(BOXDIM(XPLUS2) + 
     &                                ANGSIN(ACOUNT)*BOXRAD)
C convert to cartesian
                CALL SBCART( AXDW, CARTDW, CENTRE, AXIS)
                MDNO = MDNO + 1
                MDREC(0,MDNO) = 3.
                MDREC(1,MDNO) = CARTDW(1)
                MDREC(2,MDNO) = CARTDW(2)
                MDREC(3,MDNO) = CARTDW(3)
50            CONTINUE
C end of particular circle
40          CONTINUE 
30        CONTINUE          
C end of the four quarter circles


20      CONTINUE
C end of particular direction along xcount

10    CONTINUE
C end of doing sections for xcount

C return here
55555 RETURN
      END
C
      SUBROUTINE LOADLN( MDMAX, MDNO, MDREC, CARTMV, CARTDW)
      IMPLICIT NONE
C loads up line record for s/r sbgen (above here)

C move/draw records 
C MDMAX: Maximum number set as a parameter in previous routine
C MDNO:  total number of records
C MDREC: first number should be 2 for a moveto, 3 for a draw to,
C        other 3 x, y, z of record
      INTEGER				MDMAX, MDNO
      DOUBLE PRECISION			MDREC(0:3,MDMAX)

C move to x, y, z and draw to 
      DOUBLE PRECISION   		CARTMV(3), CARTDW(3) 

C end of decs **************************

C first move to
C n.b. checks to make sure that array bound not exceed done
C in calling routine (sbgen)
      MDNO = MDNO + 1
      MDREC(0,MDNO) = 2.
      MDREC(1,MDNO) = CARTMV(1)
      MDREC(2,MDNO) = CARTMV(2)
      MDREC(3,MDNO) = CARTMV(3)

C then draw to
      MDNO = MDNO + 1
      MDREC(0,MDNO) = 3.
      MDREC(1,MDNO) = CARTDW(1)
      MDREC(2,MDNO) = CARTDW(2)
      MDREC(3,MDNO) = CARTDW(3)

      RETURN
      END
C
      SUBROUTINE SBCART( AXVEC, CARVEC, CENTRE, AXIS)
      IMPLICIT NONE
C this s/r converts co-ordinates in the frame of reference
C of a set of orthogonal axis (axis) centred at "centre"
C to Cartesian
      DOUBLE PRECISION			AXVEC(3), CARVEC(3), 
     &					CENTRE(3), AXIS(3,3)

C end of decs **************************

      CARVEC(1) = CENTRE(1) + AXIS(1,1)*AXVEC(1) +
     &                        AXIS(1,2)*AXVEC(2) +
     &                        AXIS(1,3)*AXVEC(3)
 
      CARVEC(2) = CENTRE(2) + AXIS(2,1)*AXVEC(1) +
     &                        AXIS(2,2)*AXVEC(2) +
     &                        AXIS(2,3)*AXVEC(3)

      CARVEC(3) = CENTRE(3) + AXIS(3,1)*AXVEC(1) +
     &                        AXIS(3,2)*AXVEC(2) +
     &                        AXIS(3,3)*AXVEC(3)
 
      RETURN
      END
