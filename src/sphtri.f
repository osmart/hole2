      SUBROUTINE SPHTRI( NOUT, SOUT, LERR,
     &               DOTDEN, 
     &               SPMAX, SPNO, SPXYZ, SPRAD, SPEFFR, SPLAST,
     &               NPASS, RCUT, DMULT)
      IMPLICIT NONE
C ********************************************************************
C *                                                                  *
C * This software is an unpublished work containing confidential and *
C * proprietary information of Birkbeck College. Use, disclosure,    *
C * reproduction and transfer of this work without the express       *
C * written consent of Birkbeck College are prohibited. This notice  *
C * must be attached to all copies or extracts of the software.      *
C *                                                                  *
C * (c) 2002 Oliver Smart                    *
C *                                                                  *
C ********************************************************************
C
C Modification history:
C
C Date  Author          Modification
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

C internals ********************

C original value of dmult
      DOUBLE PRECISION		DMULOR

C loop counts: sphere centre count, dots count, 
C 2nd sphere centre count, colour Pass count
      INTEGER                   SCOUNT, DCOUNT, CCOUNT, PCOUNT, 
     &                          TCOUNT
      INTEGER                   COUNTD
      DOUBLE PRECISION		RCOUNT

C array to write dot out
      REAL                      RVEC3(3)

C variable to store distance squared
      DOUBLE PRECISION          DIST2, CUTDIST2 


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


! triangle store - loaded from s/r trisphere
      INTEGER			TRIMAX ! parameter array bound
      PARAMETER(		TRIMAX = 1004)
      INTEGER			TRINUM ! number of triangles
      DOUBLE PRECISION		TRIXYZ(3,3,TRIMAX) ! vertices
      DOUBLE PRECISION		TRINOR(3,3,TRIMAX) ! normals
      DOUBLE PRECISION		TRICHECK(3,10,TRIMAX) ! check points within triangle
      DOUBLE PRECISION		TRILENGTH(TRIMAX) ! maximum dimension of the triangle


! closest store
      INTEGER			CUTLISTMAX
      PARAMETER(		CUTLISTMAX = 500)
      INTEGER			CUTLISTNUM, CUTLIST(CUTLISTMAX)


! individual triangles
      DOUBLE PRECISION		INVTRI(3,3), INVCHECK(3,10)

! flag indicating whether vertext is within another sphere
      LOGICAL			LWITHIN(10)
      
      INTEGER			VCOUNT, NCOUNT, ILIST


C end of decs ******************

C initialize vbles
      NTIME = 0


C load up unit sphere of uniformly distributed triangles
C density dotden
      CALL TRISPHERE( DOTDEN, TRIMAX, TRINUM, TRIXYZ, TRINOR, 
     &                TRICHECK, TRILENGTH)
      WRITE( NOUT, '(A,I5,A,I5)') 
     &'s/r SPHTRI loaded triangulated sphere with density= ',
     &  DOTDEN, ' number of faces= ',TRINUM

      PCOUNT = 1
! go through sphere centres one by one
	DO SCOUNT= 1, SPNO
	  IF (
     &	       ( (.NOT.SPLAST(SCOUNT)) .AND.
     &           (SPEFFR(SCOUNT).GT.0)  .AND.
     &           (SPEFFR(SCOUNT).LE.RCUT(PCOUNT)).AND.
     &           (SPEFFR(SCOUNT).GT.RCUT(PCOUNT-1))   ) 
     &       ) THEN
            ! make up a list of the spheres that this sphere cuts
            CUTLISTNUM = 0
	    ! go thru the other spheres 
            DO TCOUNT = 1, SPNO
	      IF (TCOUNT.NE.SCOUNT) THEN ! ignore the sphere itself
	        DIST2 = (SPXYZ(1,SCOUNT)-SPXYZ(1,TCOUNT))*
     &                  (SPXYZ(1,SCOUNT)-SPXYZ(1,TCOUNT)) +
     &                  (SPXYZ(2,SCOUNT)-SPXYZ(2,TCOUNT))*
     &                  (SPXYZ(2,SCOUNT)-SPXYZ(2,TCOUNT)) +	      
     &                  (SPXYZ(3,SCOUNT)-SPXYZ(3,TCOUNT))*
     &                  (SPXYZ(3,SCOUNT)-SPXYZ(3,TCOUNT)) 
                CUTDIST2 = (SPRAD(SCOUNT)+SPRAD(TCOUNT))*
     &                     (SPRAD(SCOUNT)+SPRAD(TCOUNT))
D               write( nout, '(a,i5,a,i5,a,f8.3,a,f8.3)')
D    &' DBUG scount = ', scount,' tcount = ', tcount, 
D    &       ' dist2= ', dist2, ' cutdist2= ', cutdist2 
		IF (DIST2.LT.CUTDIST2) THEN
		  ! the spheres cut
		  CUTLISTNUM = CUTLISTNUM + 1
		  IF (CUTLISTNUM.GT.CUTLISTMAX) THEN
		  ! have hit the array bound
		    LERR = .TRUE.
		    WRITE ( NOUT, '(A/ A,I5)')
     &' Error in s/r sphtri - CANNOT CONTINUE',
     &' Error have hit array bound for closest list CUTLISTMAX= ', 
     &                                                CUTLISTMAX
                    GOTO 55555
		  ENDIF
		  CUTLIST(CUTLISTNUM) = TCOUNT
		ENDIF
                	      
              ENDIF
	    ENDDO ! make up a list of the spheres that this sphere cuts
D	    write( nout, '(a,i5,a,i5)')
D    &' debug scount= ', scount, ' #cutting spheres= ', cutlistnum
            ! go thru triangles 
	    DO TCOUNT = 1, TRINUM! go thru triangles
	      ! go thru the three vertices of the triangle 
	      DO VCOUNT = 1, 3! go thru the three vertices of the triangle
	        ! coordinates of the triangle vertex
                INVTRI(1,VCOUNT) = SPXYZ(1,SCOUNT) +
     &                             SPRAD(SCOUNT)*TRIXYZ(1,VCOUNT,TCOUNT)
                INVTRI(2,VCOUNT) = SPXYZ(2,SCOUNT) +
     &                             SPRAD(SCOUNT)*TRIXYZ(2,VCOUNT,TCOUNT)
                INVTRI(3,VCOUNT) = SPXYZ(3,SCOUNT) +
     &                             SPRAD(SCOUNT)*TRIXYZ(3,VCOUNT,TCOUNT)
              ENDDO
              ! work out the check points
	      DO VCOUNT = 1, 10! go thru the three vertices of the triangle
	        ! coordinates of the triangle vertex
                INVCHECK(1,VCOUNT) = SPXYZ(1,SCOUNT) +
     &                          SPRAD(SCOUNT)*TRICHECK(1,VCOUNT,TCOUNT)
                INVCHECK(2,VCOUNT) = SPXYZ(2,SCOUNT) +
     &                          SPRAD(SCOUNT)*TRICHECK(2,VCOUNT,TCOUNT)
                INVCHECK(3,VCOUNT) = SPXYZ(3,SCOUNT) +
     &                          SPRAD(SCOUNT)*TRICHECK(3,VCOUNT,TCOUNT)
              ENDDO
	      	      
	      ! a first check to see whether the triangle is clearly 
	      ! completely inside one of the other spheres
	      DO NCOUNT = 1, CUTLISTNUM! go thru neighbours of sphere scount
		ILIST = CUTLIST(NCOUNT)
		! find distance squared between vertex 1 and the sphere cent
                  DIST2 = (SPXYZ(1,ILIST)-INVTRI(1,1))*
     &                    (SPXYZ(1,ILIST)-INVTRI(1,1))  +
     &                    (SPXYZ(2,ILIST)-INVTRI(2,1))*
     &                    (SPXYZ(2,ILIST)-INVTRI(2,1))  +
     &                    (SPXYZ(3,ILIST)-INVTRI(3,1))*
     &                    (SPXYZ(3,ILIST)-INVTRI(3,1)) 
                ! is this distance smaller than the radius of the sphere
		! minus the biggest dimension of the triangle
                  IF (DIST2.LT.(SPRAD(ILIST)-TRILENGTH(TCOUNT))**2) 
     &                                                      GOTO 44 
		  ! jump straight out of this triangle
	      ENDDO
	      
	      
	      DO VCOUNT = 1, 10! go thru the ten check points of the triangle
                LWITHIN(VCOUNT) = .FALSE.
	        ! go thru neighbours of sphere scount
	        DO NCOUNT = 1, CUTLISTNUM! go thru neighbours of sphere scount
		  ILIST = CUTLIST(NCOUNT)
		  ! does this triangle lie within one or more of the spheres?
                  ! FIND distance squared between the vertex and the sphere
                  DIST2 = (SPXYZ(1,ILIST)-INVCHECK(1,VCOUNT))*
     &                    (SPXYZ(1,ILIST)-INVCHECK(1,VCOUNT))  +
     &                    (SPXYZ(2,ILIST)-INVCHECK(2,VCOUNT))*
     &                    (SPXYZ(2,ILIST)-INVCHECK(2,VCOUNT))  +
     &                    (SPXYZ(3,ILIST)-INVCHECK(3,VCOUNT))*
     &                    (SPXYZ(3,ILIST)-INVCHECK(3,VCOUNT)) 
                  IF (DIST2.LT.(SPRAD(ILIST)-0.003)**2) 
     &              LWITHIN(VCOUNT) = .TRUE.   
		
		ENDDO! go thru neighbours of sphere scount
	      
	       
	      ENDDO! go thru the ten check points of the triangle
	      ! output a triangle if one or more of the vertices is not within
	      ! another sphere
	      IF (.NOT.LWITHIN(1) .OR.
     &	          .NOT.LWITHIN(2) .OR.
     &	          .NOT.LWITHIN(3) .OR.
     &	          .NOT.LWITHIN(4) .OR.
     &	          .NOT.LWITHIN(5) .OR.
     &	          .NOT.LWITHIN(6) .OR.
     &	          .NOT.LWITHIN(7) .OR.
     &	          .NOT.LWITHIN(8) .OR.
     &	          .NOT.LWITHIN(9) .OR.
     &	          .NOT.LWITHIN(10)       ) THEN
                 WRITE( SOUT, '(10(A,3F10.3))')
     &' draw trinorm  { ', 
     &    INVTRI(1,1), INVTRI(2,1),  INVTRI(3,1),
     &    ' } { ',
     &    INVTRI(1,2), INVTRI(2,2),  INVTRI(3,2),
     &    ' } { ',
     &    INVTRI(1,3), INVTRI(2,3),  INVTRI(3,3),
     &    ' } { ',
     &    TRINOR(1,1,tcount), TRINOR(2,1,tcount), TRINOR(3,1,tcount),
     &    ' } { ',   
     &    TRINOR(1,2,tcount), TRINOR(2,2,tcount), TRINOR(3,2,tcount),
     &    ' } { ',
     &    TRINOR(1,3,tcount), TRINOR(2,3,tcount), TRINOR(3,3,tcount),
     &    ' }'
	      
	      ENDIF
44            CONTINUE ! jump out
	    ENDDO! go thru triangles 
	    
          ENDIF
       ENDDO
      
!-!18/11/00
!-! if the output type is sos then need to do another pass
!-! need to output end record dots to get nice sharp surface edge
!-      IF (SOSOUT) NPASS = NPASS+1
!-
!-C if doing colour mapping make 3 passes (otherwise NPASS=1)
!-      DO 45 PCOUNT= 1, NPASS
!-
!-C Change colours: as same code used in sphqpu and sphqpc (capsule)
!-C and its a bit involved put into seperate s/r at the bottom of this file
!-        CALL SPHCHC( NOUT, SOUT, NPASS, PCOUNT, SOSOUT)
!-
!-C try to avoid gaps when doing uniform output by
!-C making another pass with dmult reduced
!-99      CONTINUE
!-
!-C go through sphere centres one by one
!	DO 40 SCOUNT= 1, SPNO
C if this is not an end and radius is greater than zero
!-! 18-11-00 this line was:
!-!	  IF ( (.NOT.SPLAST(SCOUNT)) .AND.
!-!     &       (SPRAD(SCOUNT).GT.0)  .AND.
!-!     &       (SPRAD(SCOUNT).LE.RCUT(PCOUNT)).AND.
!-!     &       (SPRAD(SCOUNT).GT.RCUT(PCOUNT-1))    ) THEN
!-! but now want special output for sos type want the dots of the endrad
!-! this to be done on the additional pass thru.
!-! 11/00 replace sprad with speffr
!-	  IF (
!-     &	       ( (.NOT.SPLAST(SCOUNT)) .AND.
!-     &           (SPEFFR(SCOUNT).GT.0)  .AND.
!-     &           (SPEFFR(SCOUNT).LE.RCUT(PCOUNT)).AND.
!-     &           (SPEFFR(SCOUNT).GT.RCUT(PCOUNT-1))   ) .OR.
!-     &           (SOSOUT .AND. (PCOUNT.EQ.NPASS) .AND. SPLAST(SCOUNT))
!-     &       ) THEN
!-
!-
!-C try to improve surface by swaping x, y and z
!-C co-ords of dots on each consideration (to try to
!-C avoids dots from different spheres lining up)
!-             ISWAP1 = MOD(SCOUNT+2,3) + 1
!-             ISWAP2 = MOD(SCOUNT  ,3) + 1
!-             ISWAP3 = MOD(SCOUNT+1,3) + 1
!-
!-C go through dots one by one
!-	    DO 50 DCOUNT= 1, PTNO
!-C co-ordinates of this dot
!-	      RVEC3(1) = SPXYZ(1,SCOUNT) + 
!-     &                   SPRAD(SCOUNT)*PTXYZ(ISWAP1,DCOUNT)
!-	      RVEC3(2) = SPXYZ(2,SCOUNT) + 
!-     &                   SPRAD(SCOUNT)*PTXYZ(ISWAP2,DCOUNT)
!-	      RVEC3(3) = SPXYZ(3,SCOUNT) +
!-     &                   SPRAD(SCOUNT)*PTXYZ(ISWAP3,DCOUNT)
!-C check whether dot lies with ANY other sphere
!-C start at the records closest to the sphere centre and work
!-C backwards and forward
!-C whole idea is to save time as most dots will be rejected by
!-C thhe centres close to them
!-
!-C first do list in common
!-	      DO 60 CCOUNT = 1, MIN(SPNO-SCOUNT,SCOUNT-1)
!-C distance between dot and sphere centre scount+ccount
!-		DIST2 = (RVEC3(1)-SPXYZ(1,SCOUNT+CCOUNT))**2 +
!-     &                  (RVEC3(2)-SPXYZ(2,SCOUNT+CCOUNT))**2 +
!-     &                  (RVEC3(3)-SPXYZ(3,SCOUNT+CCOUNT))**2
!-C abort the dot dcount if the dot is within sphere
!-		IF (DIST2.LT.SPRAD(SCOUNT+CCOUNT)**2) GOTO 50
!-C same for scount-ccount
!-		DIST2 = (RVEC3(1)-SPXYZ(1,SCOUNT-CCOUNT))**2 +
!-     &                  (RVEC3(2)-SPXYZ(2,SCOUNT-CCOUNT))**2 +
!-     &                  (RVEC3(3)-SPXYZ(3,SCOUNT-CCOUNT))**2
!-		IF (DIST2.LT.SPRAD(SCOUNT-CCOUNT)**2) GOTO 50
!-60            CONTINUE
!-C go through rest of one of the routes
!-	      DO 70 CCOUNT = MIN(SPNO-SCOUNT,SCOUNT-1)+1, 
!-     &                       MAX(SPNO-SCOUNT,SCOUNT-1)
!-		IF (SCOUNT-CCOUNT.GE.1) THEN
!-		  DIST2 = (RVEC3(1)-SPXYZ(1,SCOUNT-CCOUNT))**2 +
!-     &                    (RVEC3(2)-SPXYZ(2,SCOUNT-CCOUNT))**2 +
!-     &                    (RVEC3(3)-SPXYZ(3,SCOUNT-CCOUNT))**2
!-		  IF (DIST2.LT.SPRAD(SCOUNT-CCOUNT)**2) GOTO 50
!-		  ELSE
!-		  DIST2 = (RVEC3(1)-SPXYZ(1,SCOUNT+CCOUNT))**2 +
!-     &                    (RVEC3(2)-SPXYZ(2,SCOUNT+CCOUNT))**2 +
!-     &                    (RVEC3(3)-SPXYZ(3,SCOUNT+CCOUNT))**2
!-		  IF (DIST2.LT.SPRAD(SCOUNT+CCOUNT)**2) GOTO 50
!-		ENDIF
!-70            CONTINUE
!-
!-C are we doing grid?
!-              IF (.NOT.LGRID) THEN
!-C dot should be plotted!
!-C Are we producing sos (ascii) output?
!-                IF (.NOT.SOSOUT) THEN
!-C NO - 	quanta binary record
!-                  WRITE( SOUT) 4.0, RVEC3
!-C debug produce surface normal
!-C                  write( sout) 2.0, rvec3
!-C		  rvec3(1) = rvec3(1) + PTXYZ(ISWAP1,DCOUNT)
!-C		  rvec3(2) = rvec3(2) + PTXYZ(ISWAP2,DCOUNT)
!-C		  rvec3(3) = rvec3(3) + PTXYZ(ISWAP3,DCOUNT)
!-C		  write( sout) 3.0, rvec3
!-		ELSE
!-C write out rough normal - original ptxyz record used
!-		  WRITE( SOUT, '(7F12.5)') 
!-     &              4.0, RVEC3, PTXYZ(ISWAP1,DCOUNT), 
!-     &              PTXYZ(ISWAP2,DCOUNT), PTXYZ(ISWAP3,DCOUNT) 
!-                ENDIF
!-		
!-              ELSE
!-C We are doing griding
!-C First ask is this dot to close to a line to be output?
!-C Have a store of points along lines already output
!-	        DO 100 COUNTD = 1, LINNO
!-		  DISXYZ = (RVEC3(1)-LINXYZ(1,COUNTD))**2
!-     &                    +(RVEC3(2)-LINXYZ(2,COUNTD))**2  
!-     &                    +(RVEC3(3)-LINXYZ(3,COUNTD))**2
!-C dot has to be at least 0.3 times dmult*the minimum radius
!-C of a sphere in the vicinity
!-c                  dotd2 = 0.6*dmult*min( real(linxyz(4,countd)),
!-c     &                                 sprad(scount) )
!-c 		  if (disxyz.lt.dotd2**2) goto 50
!-
!-C if new point lies within range of points along line then
!-C reject it and generate next dot. Range is sqrt(0.4)*dmult*minimum radius
!-C of the two points
!-                  IF   (DISXYZ  .LT.
!-     &  (0.6*DMULT*MIN(LINXYZ(4,COUNTD),SPRAD(SCOUNT)))**2) GOTO 50
!-
!-100             CONTINUE
!-
!-C o.k. the dot under question has be accepted as a vertex
!-C if we are sos type output only want vertices - work out the lines
!-C as in proper qpt output but only for internal purposes
!-                IF (SOSOUT) THEN
!-		  WRITE( SOUT, '(7F12.5)') 
!-     &              4.0, RVEC3, PTXYZ(ISWAP1,DCOUNT), 
!-     &              PTXYZ(ISWAP2,DCOUNT), PTXYZ(ISWAP3,DCOUNT) 
!-		ENDIF
!-C Now lets check to see whether it is in range of any other
!-C Vertex's
!-                DO 110 COUNTD = 1, VERNO
!-                  DISXYZ = (RVEC3(1)-VERXYZ(1,COUNTD))**2
!-     &                    +(RVEC3(2)-VERXYZ(2,COUNTD))**2
!-     &                    +(RVEC3(3)-VERXYZ(3,COUNTD))**2
!-C is the line in range of the other vertex? 
!-                  IF   (DISXYZ  .LT.
!-     &  (DMULT*MIN(VERXYZ(4,COUNTD),SPRAD(SCOUNT)))**2) THEN
!-
!-C draw line in file
!-C 29/7/94 - want to correct the line so that it appears more spherical
!-C use already calculated intermediate points on the line and correct
!-C them
!-C move to first point
!-                    IF (.NOT.SOSOUT) WRITE(SOUT) 2., 
!-     &                          REAL(VERXYZ(1,COUNTD)), 
!-     &                          REAL(VERXYZ(2,COUNTD)), 
!-     &                          REAL(VERXYZ(3,COUNTD))
!-
!-C store the co-ords
!-                    PREXYZ(1) = VERXYZ(1,COUNTD)
!-                    PREXYZ(2) = VERXYZ(2,COUNTD)
!-                    PREXYZ(3) = VERXYZ(3,COUNTD)
!-
!-C work out the distance between the end points of line
!-                    MAXDIS = (VERXYZ(1,COUNTD)-RVEC3(1))**2 +
!-     &                       (VERXYZ(2,COUNTD)-RVEC3(2))**2 +
!-     &                       (VERXYZ(3,COUNTD)-RVEC3(3))**2
!-C will draw lines between sections at 0.15*the distance
!-C - only draw line if the distance is less than double expected
!-                    MAXDIS = 0.3*SQRT(MAXDIS)
!-
!-C store four points along line 0.05, 0.35, 0.65, 0.95
!-C these will then be used to make sure that no vertex is output
!-C close to these
!-C do extra points at 0.20 for graphical output - but overwrite at the end
!-                    DO 111 RCOUNT = 0.05, 1.0, 0.15
!-                      LINNO = LINNO + 1
!-                      LINXYZ(1,LINNO) = RCOUNT*RVEC3(1) + 
!-     &                                  (1.-RCOUNT)*VERXYZ(1,COUNTD)
!-                      LINXYZ(2,LINNO) = RCOUNT*RVEC3(2) + 
!-     &                                  (1.-RCOUNT)*VERXYZ(2,COUNTD)
!-                      LINXYZ(3,LINNO) = RCOUNT*RVEC3(3) + 
!-     &                                  (1.-RCOUNT)*VERXYZ(3,COUNTD)
!-C correct the co-ords
!-                      CALL CORDOT( LINXYZ(1,LINNO), 
!-     &                             SPMAX, SPNO, SPXYZ, SPRAD)
!-C if routine fails to give point on surface abort the whole line
!-C this is indicated by first coord being set to big no
!-                      IF (LINXYZ(1,LINNO).GT.1E10) THEN
!-                        LINNO = LINNO - 1
!-C look at next vertex
!-			GOTO 110
!-	              ENDIF
!-
!-C work out distance between current point and previous
!-                      NOWDIS = (LINXYZ(1,LINNO)-PREXYZ(1))**2 +
!-     &                         (LINXYZ(2,LINNO)-PREXYZ(2))**2 + 
!-     &                         (LINXYZ(3,LINNO)-PREXYZ(3))**2 
!-                      NOWDIS = SQRT(NOWDIS)
!-
!-
!-C if the distance is within max then draw to point
!-                      IF (NOWDIS.LT.MAXDIS) THEN
!-                        IF (.NOT.SOSOUT) WRITE(SOUT) 3., 
!-     &                          REAL(LINXYZ(1,LINNO)), 
!-     &                          REAL(LINXYZ(2,LINNO)), 
!-     &                          REAL(LINXYZ(3,LINNO))
!-                      ELSE
!-C just move to the point
!-                        IF (.NOT.SOSOUT) WRITE(SOUT) 2.,
!-     &                          REAL(LINXYZ(1,LINNO)),
!-     &                          REAL(LINXYZ(2,LINNO)),
!-     &                          REAL(LINXYZ(3,LINNO))
!-                      ENDIF
!-
!-C store the co-ords
!-                     PREXYZ(1) = LINXYZ(1,LINNO)
!-                     PREXYZ(2) = LINXYZ(2,LINNO)
!-                     PREXYZ(3) = LINXYZ(3,LINNO)
!-
!-C store the lower of the two radii
!-                      LINXYZ(4,LINNO) = MIN( SPRAD(SCOUNT), 
!-     &				             VERXYZ(4,COUNTD))
!-
!-C make sure that we do not store the points except at 0.05, 0.35, 0.65, 0.95
!-C try just two points
!-                      IF ( 
!-C     &                     (abs(rcount-0.05).gt.1e-05) .and.
!-     &                     (ABS(RCOUNT-0.35).GT.1E-05) .AND. 
!-     &                     (ABS(RCOUNT-0.65).GT.1E-05) 
!-C      &             .and. (abs(rcount-0.95).gt.1e-05)      
!-     &                    ) LINNO = LINNO-1
!-111                 CONTINUE
!-C draw to last point
!-                    IF (.NOT.SOSOUT) WRITE(SOUT) 3., RVEC3
!-                  ENDIF
!-110             CONTINUE
!-
!-
!-C then store the vertex we have just output
!-C increment no of dots accepted by 1 and store it.
!-	        VERNO = VERNO + 1
!-		VERXYZ(1,VERNO) = RVEC3(1)
!-                VERXYZ(2,VERNO) = RVEC3(2) 
!-                VERXYZ(3,VERNO) = RVEC3(3) 
!-C also store the radius of the sphere 
!-                VERXYZ(4,VERNO) = SPRAD(SCOUNT) 
!-C vertex also counts as a line
!-                LINNO = LINNO + 1
!-		LINXYZ(1,LINNO) = RVEC3(1)
!-                LINXYZ(2,LINNO) = RVEC3(2) 
!-                LINXYZ(3,LINNO) = RVEC3(3) 
!-                LINXYZ(4,LINNO) = SPRAD(SCOUNT) 
!-
!-              ENDIF
!-
!-C end of particular dot
!-50          CONTINUE
!-C if block for whether the dots of a particular sphere should
!-C be drawn
!-          ENDIF
!-C end of considering sphere# scount
!-40      CONTINUE
!-C try to avoid gaps when doing uniform output by
!-C making another pass with dmult reduced
!-        IF (LGRID) THEN
!-          NTIME = NTIME+1
!-          WRITE( NOUT, '(A,I5,A)')
!-     &' Pass ', NTIME, ' for this colour',
!-     &'   Number of vertices output so far ', VERNO
!-          IF (NTIME.GE.2) THEN
!-C reset back to original
!-            DMULT = DMULOR
!-            NTIME = 0
!-          ELSE 
!-C reduce dmult to try to fill in any large gaps 
!-            DMULT = DMULT*0.8
!-            GOTO 99
!-          ENDIF
!-        ENDIF
!-
!-C end of loop for three colours
!-45    CONTINUE

55555 RETURN
      END
      
