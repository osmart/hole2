      SUBROUTINE SPHQPU( NOUT, SOUT, LERR,
     &               DOTDEN, PTMAX, PTNO, PTXYZ,
     &               SPMAX, SPNO, SPXYZ, SPRAD, SPEFFR, SPLAST,
     &               LGRID, NPASS, RCUT, DMULT, 
     &               VERMAX, VERNO, VERXYZ, LINMAX, LINNO, LINXYZ,
     &               SOSOUT)
      IMPLICIT NONE
C ********************************************************************
C *                                                                  *
C * This software is an unpublished work containing confidential and *
C * proprietary information of Birkbeck College. Use, disclosure,    *
C * reproduction and transfer of this work without the express       *
C * written consent of Birkbeck College are prohibited. This notice  *
C * must be attached to all copies or extracts of the software.      *
C *                                                                  *
C * (c) 1996 Oliver Smart & Birkbeck College,                        *
C * (c) 1998 Oliver Smart                    *
C *                                                                  *
C ********************************************************************
C
C Modification history:
C
C Date  Author          Modification
C 07/96 O.S. Smart        Original version split from sphqpt
C 03/98    O.S.S.	special output file for Guy Coates "solid_surf"
! 11/00    O.S.S.	sos type modified to produce end records
!
C n.b. code for sos output with lgrid set true never used as the
C default output seems superior - left in incase it ever becomes necessary
C in the future.
C
C contains code to output the dot or grid surface from records set up
C by program sphqpt.  This version is for normal spheres - see
C sphqpc for "capsule" option bits.

C passed vbles *****************

C screen, keyboard
      INTEGER                   NOUT

C output file stream (already opened)
      INTEGER                   SOUT

C error indicator
      LOGICAL			LERR

C dot density to be used in output
      INTEGER                   DOTDEN

C program uses a sphere of isotropically distributed points
C on a sphere of unit radius. PTMAX is the array bound,
C PTNO is the number of dots (dependent on dotden)
      INTEGER                   PTMAX, PTNO
      DOUBLE PRECISION          PTXYZ( 3, PTMAX)

C sphere centre storage arrays
C maximum number, actual number, co-ords and radius
C logical indicates whether the record is the last centre
      INTEGER                   SPMAX, SPNO
      DOUBLE PRECISION          SPXYZ( 3, SPMAX), SPRAD( SPMAX)
      LOGICAL                   SPLAST( SPMAX)

      ! need effective radius for colouring connolly
      DOUBLE PRECISION		SPEFFR( SPMAX)

C control flag - if true do a grid surface
      LOGICAL			LGRID

C if we are doing colour NPASS=3
C if we are not doing colour NPASS=1
      INTEGER                   NPASS

C variable to define colours
      DOUBLE PRECISION          RCUT(0:3)

C vble to control the mesh size of the grid produced
      DOUBLE PRECISION          DMULT

C storage arrays for vertices
      INTEGER                   VERMAX, VERNO               
      DOUBLE PRECISION          VERXYZ( 4, VERMAX)

C also store points taken from output lines
      INTEGER                   LINMAX, LINNO
      DOUBLE PRECISION          LINXYZ( 4,LINMAX)

C doing sos output - only do dots and add on a rough surface normal
      LOGICAL			SOSOUT

C internals ********************

C original value of dmult
      DOUBLE PRECISION		DMULOR

C loop counts: sphere centre count, dots count, 
C 2nd sphere centre count, colour Pass count
      INTEGER                   SCOUNT, DCOUNT, CCOUNT, PCOUNT
      INTEGER                   COUNTD
      DOUBLE PRECISION		RCOUNT

C array to write dot out
      REAL                      RVEC3(3)

C variable to store distance squared
      DOUBLE PRECISION          DIST2


C distance between potential point and accepted dots
      DOUBLE PRECISION          DISXYZ

C try to improve surface by swaping x, y and z
C co-ords of dots on each consideration (to try to
C avoids dots from different spheres lining up)
      INTEGER			ISWAP1, ISWAP2, ISWAP3


C number of times through the whole business for a particular
C colour to fill in gaps for a particular colour
      INTEGER			NTIME

C vbles to test to see whether two points on line should be connected
      DOUBLE PRECISION		PREXYZ(3), MAXDIS, NOWDIS

C end of decs ******************

C initialize vbles
      NTIME = 0
      VERNO = 0
      LINNO = 0
C store dmult
      DMULOR = DMULT

C load up unit sphere of uniformly distributed points
C density dotden
      CALL PTGEN( DOTDEN, PTMAX, PTNO, PTXYZ, NOUT, LERR)
      IF (LERR) THEN
	WRITE( NOUT,'(A)')' Have found error in s/r PTGEN'//CHAR(7)
	GOTO 55555
      ENDIF
      
!18/11/00
! if the output type is sos then need to do another pass
! need to output end record dots to get nice sharp surface edge
      IF (SOSOUT) NPASS = NPASS+1

C if doing colour mapping make 3 passes (otherwise NPASS=1)
      DO 45 PCOUNT= 1, NPASS

C Change colours: as same code used in sphqpu and sphqpc (capsule)
C and its a bit involved put into seperate s/r at the bottom of this file
        CALL SPHCHC( NOUT, SOUT, NPASS, PCOUNT, SOSOUT)

C try to avoid gaps when doing uniform output by
C making another pass with dmult reduced
99      CONTINUE

C go through sphere centres one by one
	DO 40 SCOUNT= 1, SPNO
C if this is not an end and radius is greater than zero
! 18-11-00 this line was:
!	  IF ( (.NOT.SPLAST(SCOUNT)) .AND.
!     &       (SPRAD(SCOUNT).GT.0)  .AND.
!     &       (SPRAD(SCOUNT).LE.RCUT(PCOUNT)).AND.
!     &       (SPRAD(SCOUNT).GT.RCUT(PCOUNT-1))    ) THEN
! but now want special output for sos type want the dots of the endrad
! this to be done on the additional pass thru.
! 11/00 replace sprad with speffr
	  IF (
     &	       ( (.NOT.SPLAST(SCOUNT)) .AND.
     &           (SPEFFR(SCOUNT).GT.0)  .AND.
     &           (SPEFFR(SCOUNT).LE.RCUT(PCOUNT)).AND.
     &           (SPEFFR(SCOUNT).GT.RCUT(PCOUNT-1))   ) .OR.
     &           (SOSOUT .AND. (PCOUNT.EQ.NPASS) .AND. SPLAST(SCOUNT))
     &       ) THEN


C try to improve surface by swaping x, y and z
C co-ords of dots on each consideration (to try to
C avoids dots from different spheres lining up)
             ISWAP1 = MOD(SCOUNT+2,3) + 1
             ISWAP2 = MOD(SCOUNT  ,3) + 1
             ISWAP3 = MOD(SCOUNT+1,3) + 1

C go through dots one by one
	    DO 50 DCOUNT= 1, PTNO
C co-ordinates of this dot
	      RVEC3(1) = SPXYZ(1,SCOUNT) + 
     &                   SPRAD(SCOUNT)*PTXYZ(ISWAP1,DCOUNT)
	      RVEC3(2) = SPXYZ(2,SCOUNT) + 
     &                   SPRAD(SCOUNT)*PTXYZ(ISWAP2,DCOUNT)
	      RVEC3(3) = SPXYZ(3,SCOUNT) +
     &                   SPRAD(SCOUNT)*PTXYZ(ISWAP3,DCOUNT)
C check whether dot lies with ANY other sphere
C start at the records closest to the sphere centre and work
C backwards and forward
C whole idea is to save time as most dots will be rejected by
C thhe centres close to them

C first do list in common
	      DO 60 CCOUNT = 1, MIN(SPNO-SCOUNT,SCOUNT-1)
C distance between dot and sphere centre scount+ccount
		DIST2 = (RVEC3(1)-SPXYZ(1,SCOUNT+CCOUNT))**2 +
     &                  (RVEC3(2)-SPXYZ(2,SCOUNT+CCOUNT))**2 +
     &                  (RVEC3(3)-SPXYZ(3,SCOUNT+CCOUNT))**2
C abort the dot dcount if the dot is within sphere
		IF (DIST2.LT.SPRAD(SCOUNT+CCOUNT)**2) GOTO 50
C same for scount-ccount
		DIST2 = (RVEC3(1)-SPXYZ(1,SCOUNT-CCOUNT))**2 +
     &                  (RVEC3(2)-SPXYZ(2,SCOUNT-CCOUNT))**2 +
     &                  (RVEC3(3)-SPXYZ(3,SCOUNT-CCOUNT))**2
		IF (DIST2.LT.SPRAD(SCOUNT-CCOUNT)**2) GOTO 50
60            CONTINUE
C go through rest of one of the routes
	      DO 70 CCOUNT = MIN(SPNO-SCOUNT,SCOUNT-1)+1, 
     &                       MAX(SPNO-SCOUNT,SCOUNT-1)
		IF (SCOUNT-CCOUNT.GE.1) THEN
		  DIST2 = (RVEC3(1)-SPXYZ(1,SCOUNT-CCOUNT))**2 +
     &                    (RVEC3(2)-SPXYZ(2,SCOUNT-CCOUNT))**2 +
     &                    (RVEC3(3)-SPXYZ(3,SCOUNT-CCOUNT))**2
		  IF (DIST2.LT.SPRAD(SCOUNT-CCOUNT)**2) GOTO 50
		  ELSE
		  DIST2 = (RVEC3(1)-SPXYZ(1,SCOUNT+CCOUNT))**2 +
     &                    (RVEC3(2)-SPXYZ(2,SCOUNT+CCOUNT))**2 +
     &                    (RVEC3(3)-SPXYZ(3,SCOUNT+CCOUNT))**2
		  IF (DIST2.LT.SPRAD(SCOUNT+CCOUNT)**2) GOTO 50
		ENDIF
70            CONTINUE

C are we doing grid?
              IF (.NOT.LGRID) THEN
C dot should be plotted!
C Are we producing sos (ascii) output?
                IF (.NOT.SOSOUT) THEN
C NO - 	quanta binary record
                  WRITE( SOUT) 4.0, RVEC3
C debug produce surface normal
C                  write( sout) 2.0, rvec3
C		  rvec3(1) = rvec3(1) + PTXYZ(ISWAP1,DCOUNT)
C		  rvec3(2) = rvec3(2) + PTXYZ(ISWAP2,DCOUNT)
C		  rvec3(3) = rvec3(3) + PTXYZ(ISWAP3,DCOUNT)
C		  write( sout) 3.0, rvec3
		ELSE
C write out rough normal - original ptxyz record used
		  WRITE( SOUT, '(7F12.5)') 
     &              4.0, RVEC3, PTXYZ(ISWAP1,DCOUNT), 
     &              PTXYZ(ISWAP2,DCOUNT), PTXYZ(ISWAP3,DCOUNT) 
                ENDIF
		
              ELSE
C We are doing griding
C First ask is this dot to close to a line to be output?
C Have a store of points along lines already output
	        DO 100 COUNTD = 1, LINNO
		  DISXYZ = (RVEC3(1)-LINXYZ(1,COUNTD))**2
     &                    +(RVEC3(2)-LINXYZ(2,COUNTD))**2  
     &                    +(RVEC3(3)-LINXYZ(3,COUNTD))**2
C dot has to be at least 0.3 times dmult*the minimum radius
C of a sphere in the vicinity
c                  dotd2 = 0.6*dmult*min( real(linxyz(4,countd)),
c     &                                 sprad(scount) )
c 		  if (disxyz.lt.dotd2**2) goto 50

C if new point lies within range of points along line then
C reject it and generate next dot. Range is sqrt(0.4)*dmult*minimum radius
C of the two points
                  IF   (DISXYZ  .LT.
     &  (0.6*DMULT*MIN(LINXYZ(4,COUNTD),SPRAD(SCOUNT)))**2) GOTO 50

100             CONTINUE

C o.k. the dot under question has be accepted as a vertex
C if we are sos type output only want vertices - work out the lines
C as in proper qpt output but only for internal purposes
                IF (SOSOUT) THEN
		  WRITE( SOUT, '(7F12.5)') 
     &              4.0, RVEC3, PTXYZ(ISWAP1,DCOUNT), 
     &              PTXYZ(ISWAP2,DCOUNT), PTXYZ(ISWAP3,DCOUNT) 
		ENDIF
C Now lets check to see whether it is in range of any other
C Vertex's
                DO 110 COUNTD = 1, VERNO
                  DISXYZ = (RVEC3(1)-VERXYZ(1,COUNTD))**2
     &                    +(RVEC3(2)-VERXYZ(2,COUNTD))**2
     &                    +(RVEC3(3)-VERXYZ(3,COUNTD))**2
C is the line in range of the other vertex? 
                  IF   (DISXYZ  .LT.
     &  (DMULT*MIN(VERXYZ(4,COUNTD),SPRAD(SCOUNT)))**2) THEN

C draw line in file
C 29/7/94 - want to correct the line so that it appears more spherical
C use already calculated intermediate points on the line and correct
C them
C move to first point
                    IF (.NOT.SOSOUT) WRITE(SOUT) 2., 
     &                          REAL(VERXYZ(1,COUNTD)), 
     &                          REAL(VERXYZ(2,COUNTD)), 
     &                          REAL(VERXYZ(3,COUNTD))

C store the co-ords
                    PREXYZ(1) = VERXYZ(1,COUNTD)
                    PREXYZ(2) = VERXYZ(2,COUNTD)
                    PREXYZ(3) = VERXYZ(3,COUNTD)

C work out the distance between the end points of line
                    MAXDIS = (VERXYZ(1,COUNTD)-RVEC3(1))**2 +
     &                       (VERXYZ(2,COUNTD)-RVEC3(2))**2 +
     &                       (VERXYZ(3,COUNTD)-RVEC3(3))**2
C will draw lines between sections at 0.15*the distance
C - only draw line if the distance is less than double expected
                    MAXDIS = 0.3*SQRT(MAXDIS)

C store four points along line 0.05, 0.35, 0.65, 0.95
C these will then be used to make sure that no vertex is output
C close to these
C do extra points at 0.20 for graphical output - but overwrite at the end
                    DO 111 RCOUNT = 0.05, 1.0, 0.15
                      LINNO = LINNO + 1
                      LINXYZ(1,LINNO) = RCOUNT*RVEC3(1) + 
     &                                  (1.-RCOUNT)*VERXYZ(1,COUNTD)
                      LINXYZ(2,LINNO) = RCOUNT*RVEC3(2) + 
     &                                  (1.-RCOUNT)*VERXYZ(2,COUNTD)
                      LINXYZ(3,LINNO) = RCOUNT*RVEC3(3) + 
     &                                  (1.-RCOUNT)*VERXYZ(3,COUNTD)
C correct the co-ords
                      CALL CORDOT( LINXYZ(1,LINNO), 
     &                             SPMAX, SPNO, SPXYZ, SPRAD)
C if routine fails to give point on surface abort the whole line
C this is indicated by first coord being set to big no
                      IF (LINXYZ(1,LINNO).GT.1E10) THEN
                        LINNO = LINNO - 1
C look at next vertex
			GOTO 110
	              ENDIF

C work out distance between current point and previous
                      NOWDIS = (LINXYZ(1,LINNO)-PREXYZ(1))**2 +
     &                         (LINXYZ(2,LINNO)-PREXYZ(2))**2 + 
     &                         (LINXYZ(3,LINNO)-PREXYZ(3))**2 
                      NOWDIS = SQRT(NOWDIS)


C if the distance is within max then draw to point
                      IF (NOWDIS.LT.MAXDIS) THEN
                        IF (.NOT.SOSOUT) WRITE(SOUT) 3., 
     &                          REAL(LINXYZ(1,LINNO)), 
     &                          REAL(LINXYZ(2,LINNO)), 
     &                          REAL(LINXYZ(3,LINNO))
                      ELSE
C just move to the point
                        IF (.NOT.SOSOUT) WRITE(SOUT) 2.,
     &                          REAL(LINXYZ(1,LINNO)),
     &                          REAL(LINXYZ(2,LINNO)),
     &                          REAL(LINXYZ(3,LINNO))
                      ENDIF

C store the co-ords
                     PREXYZ(1) = LINXYZ(1,LINNO)
                     PREXYZ(2) = LINXYZ(2,LINNO)
                     PREXYZ(3) = LINXYZ(3,LINNO)

C store the lower of the two radii
                      LINXYZ(4,LINNO) = MIN( SPRAD(SCOUNT), 
     &				             VERXYZ(4,COUNTD))

C make sure that we do not store the points except at 0.05, 0.35, 0.65, 0.95
C try just two points
                      IF ( 
C     &                     (abs(rcount-0.05).gt.1e-05) .and.
     &                     (ABS(RCOUNT-0.35).GT.1E-05) .AND. 
     &                     (ABS(RCOUNT-0.65).GT.1E-05) 
C      &             .and. (abs(rcount-0.95).gt.1e-05)      
     &                    ) LINNO = LINNO-1
111                 CONTINUE
C draw to last point
                    IF (.NOT.SOSOUT) WRITE(SOUT) 3., RVEC3
                  ENDIF
110             CONTINUE


C then store the vertex we have just output
C increment no of dots accepted by 1 and store it.
	        VERNO = VERNO + 1
		VERXYZ(1,VERNO) = RVEC3(1)
                VERXYZ(2,VERNO) = RVEC3(2) 
                VERXYZ(3,VERNO) = RVEC3(3) 
C also store the radius of the sphere 
                VERXYZ(4,VERNO) = SPRAD(SCOUNT) 
C vertex also counts as a line
                LINNO = LINNO + 1
		LINXYZ(1,LINNO) = RVEC3(1)
                LINXYZ(2,LINNO) = RVEC3(2) 
                LINXYZ(3,LINNO) = RVEC3(3) 
                LINXYZ(4,LINNO) = SPRAD(SCOUNT) 

              ENDIF

C end of particular dot
50          CONTINUE
C if block for whether the dots of a particular sphere should
C be drawn
          ENDIF
C end of considering sphere# scount
40      CONTINUE
C try to avoid gaps when doing uniform output by
C making another pass with dmult reduced
        IF (LGRID) THEN
          NTIME = NTIME+1
          WRITE( NOUT, '(A,I5,A)')
     &' Pass ', NTIME, ' for this colour',
     &'   Number of vertices output so far ', VERNO
          IF (NTIME.GE.2) THEN
C reset back to original
            DMULT = DMULOR
            NTIME = 0
          ELSE 
C reduce dmult to try to fill in any large gaps 
            DMULT = DMULT*0.8
            GOTO 99
          ENDIF
        ENDIF

C end of loop for three colours
45    CONTINUE

55555 RETURN
      END
      
      SUBROUTINE CORDOT( CORXYZ, SPMAX, SPNO, SPXYZ, SPRAD)
      IMPLICIT NONE
C This s/r "corrects" point linxyz so that it lies on the surface of
C the series of spheres defined by sp****.

C the point
      DOUBLE PRECISION		CORXYZ(3)

C the spheres
      INTEGER                   SPMAX, SPNO
      DOUBLE PRECISION          SPXYZ( 3, SPMAX), SPRAD( SPMAX)

C internal vbles

C loop counts, #step, x,y&z
      INTEGER			MSTEP, XCOUNT

C the derivative, vbles to work it out
      DOUBLE PRECISION		DERIV(3), FPLUS, FMINU

C minimization step
      DOUBLE PRECISION 		STEPL

C old function
      DOUBLE PRECISION		FOLD

C end of decs ******************

      STEPL = 1E-03

C we are going to do it by steepest descents min
C The function for a point will be the largest overlap
C squared i.e. the smallest distance to sphere centre - sphere radius.
C The actual function will be the modulus of the overlap
      DO 10 MSTEP = 1, 100
C work out derivative in x, y and z
        DO 20 XCOUNT = 1, 3
C add a bit
          CORXYZ(XCOUNT) = CORXYZ(XCOUNT) + 1E-06
C work out function 
          CALL CORFUN( FPLUS, CORXYZ, SPMAX, SPNO, SPXYZ, SPRAD)
C take a bit off
          CORXYZ(XCOUNT) = CORXYZ(XCOUNT) - 2E-06
C new function
          CALL CORFUN( FMINU, CORXYZ, SPMAX, SPNO, SPXYZ, SPRAD)
C return point to orig
          CORXYZ(XCOUNT) = CORXYZ(XCOUNT) + 1E-06
C central diff deriv
          DERIV(XCOUNT) = (FPLUS-FMINU)/2E-06
20      CONTINUE
C worked out derivative - unit it
        CALL DUVEC2( DERIV)
C take a step along
        CORXYZ(1) = CORXYZ(1) - STEPL*DERIV(1)
        CORXYZ(2) = CORXYZ(2) - STEPL*DERIV(2)
        CORXYZ(3) = CORXYZ(3) - STEPL*DERIV(3)

C has function go up or down on this step
        CALL CORFUN( FPLUS, CORXYZ, SPMAX, SPNO, SPXYZ, SPRAD)
        IF (FPLUS.LT.FOLD) THEN
          STEPL = 1.2*STEPL
        ELSE
          STEPL = 0.5*STEPL
        ENDIF
C store function 
        FOLD = FPLUS
C converged?
        IF (STEPL.LT.1E-05) GOTO 55555
C        write(*,*) 'debug step# ', mstep, ' func ', fplus, 
C     &              'stepl ', stepl, 'linxyz ', linxyz

10    CONTINUE

55555 CONTINUE
C indicate we have a problem if the final function is above 0.05
C tell calling routine this is the case by setting corrected coord to
C very big value
      IF (FPLUS.GT.1E-03) CORXYZ(1) = 1E20

      RETURN
      END
      SUBROUTINE CORFUN( FUNC, CORXYZ, SPMAX, SPNO, SPXYZ, SPRAD)
      IMPLICIT NONE
C This s/r provides the function for the s/r cormin

C the function
      DOUBLE PRECISION		FUNC

C the point
      DOUBLE PRECISION          CORXYZ(3)

C the spheres
      INTEGER                   SPMAX, SPNO
      DOUBLE PRECISION          SPXYZ( 3, SPMAX), SPRAD( SPMAX)

C internal vbles

C loop count for spheres
      INTEGER			SCOUNT

C distance squared
      DOUBLE PRECISION		DIST2

C end of decs ******************

C initial value
      FUNC = 1E06

C go thru spheres
      DO 10 SCOUNT = 1, SPNO
C work out distance between sphere centre# scount and point linxyz
        DIST2 = (CORXYZ(1) - SPXYZ(1,SCOUNT))**2 +
     &          (CORXYZ(2) - SPXYZ(2,SCOUNT))**2 +
     &          (CORXYZ(3) - SPXYZ(3,SCOUNT))**2
C take off the radius squared
        DIST2 = DIST2 - SPRAD(SCOUNT)**2
C if this is smaller than present value then replace
        IF (DIST2.LT.FUNC) FUNC = DIST2
10    CONTINUE

C must return an absolute value
      FUNC = ABS(FUNC)

      RETURN
      END
      SUBROUTINE SPHCHC( NOUT, SOUT, NPASS, PCOUNT, SOSOUT)
      IMPLICIT NONE
C seperate s/r to do change colour for sphqpu and sphqpc
      INTEGER		NOUT, SOUT, NPASS, PCOUNT
      LOGICAL		SOSOUT
C end of decs ***********	

C 20/10/95 change default colours:
C
C If only doing one colour this should be green quanta color 7 and have
C alternative (qplot) colour 17.
C
C If doing radius colouring:
C pass quanta# colour qplot#
C    1    3      red    16
C    2    7     green   17
C    3    2      blue   18
        IF (      (NPASS.EQ.1).OR.
     &       (SOSOUT.AND.(PCOUNT.EQ.1).AND.(NPASS.EQ.2)) ) THEN
          WRITE( NOUT, '(A)') 
     &' Outputing surface to quanta colour 7 (default green)'
C change to quanta colour 7, qplot number 17 (indicated by -55 as Y)
C SOS Should be ascii - otherwise binary
	  IF (SOSOUT) THEN
	    WRITE(SOUT, '(7F12.5)') 1.0, 7.0, -55.0, 17.0, 0., 0., 0.
	  ELSE
	    WRITE(SOUT) 1.0, 7.0, -55.0, 17.0
	  ENDIF

        ELSEIF (SOSOUT.AND.(PCOUNT.EQ.NPASS)) THEN
          WRITE( NOUT, '(A)') 
     &' Special sos outputing endrad points to color -1',
     &' (needed by sos_triangle to produce sharp edges)'
! special sos type
	  IF (SOSOUT) THEN
	    WRITE(SOUT, '(7F12.5)')  1.0, -1., -1., -1., 0., 0., 0.
	  ELSE
	    STOP 'programing error report to o.s.smart@bham.ac.uk'
	  ENDIF

        ELSEIF (PCOUNT.EQ.1) THEN
C low radius pass
          WRITE( NOUT, '(A)') 
     &' Outputing low rad surface to quanta colour 3 (default red)'
C change to quanta colour 3, qplot number 16 (indicated by -55 as Y)
	  IF (SOSOUT) THEN
	    WRITE(SOUT, '(7F12.5)') 1.0, 3.0, -55.0, 16.0, 0., 0., 0.
	  ELSE
	    WRITE(SOUT) 1.0, 3.0, -55.0, 16.0
	  ENDIF

        ELSEIF (PCOUNT.EQ.2) THEN
C mid radius pass
          WRITE( NOUT, '(A)') 
     &' Outputing mid rad surface to quanta colour 7 (default green)'
C change to quanta colour 7, qplot number 17 (indicated by -55 as Y)
	  IF (SOSOUT) THEN
	    WRITE(SOUT, '(7F12.5)')  1.0, 7.0, -55.0, 17.0, 0., 0., 0.
	  ELSE
	    WRITE(SOUT) 1.0, 7.0, -55.0, 17.0
	  ENDIF

        ELSEIF (PCOUNT.EQ.3) THEN
          WRITE( NOUT, '(A)') 
     &' Outputing high rad surface to quanta colour 2 (default blue)'
C change to quanta colour 2, qplot number 18 (indicated by -55 as Y)
	  IF (SOSOUT) THEN
	    WRITE(SOUT, '(7F12.5)')  1.0, 2.0, -55.0, 18.0, 0., 0., 0.
	  ELSE
	    WRITE(SOUT) 1.0, 2.0, -55.0, 18.0
	  ENDIF
	  
	  
	  
        ENDIF
      RETURN
      END	

