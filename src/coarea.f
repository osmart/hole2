      SUBROUTINE COAREA( REQUIV, NOUT, LERR, SHORTO, ENDRAD, 
     &		     CENTRE, PERPVE, PERPVN, 
     &               SCOMAX, SCONUM, SCOXYZ, SCORAD, SCOTDO,
     &               SCOENC, SCOCIRCRAD, CVECT, SAMPLE,
     &		     LPEG, SCO_PEG_ACC, SCO_PEG_FLAG, QPTOUT,
     & 		     SCO_PEG_RAD)
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
C s/r to perform area calculation for concal - which works out the connolly
C     type slab from the hole point.
C
C possibilies with this routine - graphical output of edges of connolly
C                               - 2dmap type output
C
C Modification history:
C
C Date  Author          Modification
C 01/00 O.S.S.    	First version 
! 12/00 O.S.S. 		As well as being called by s/r concal to work out
!			areas for connolly routine now also called by
!			sphpeg (part of sph_process program) to work
!			out peg accessible areas.
! 			This means that we have to deal with spheres that are
!			not centred on the plane in question.
!			Necessitates introduction of an addition vble SCOCIRCRAD

C passed vbles *****************

C area of final assembly returned as a effective radius = sqrt(area/pi)
C return with result!
      DOUBLE PRECISION		REQUIV

C output stream number
C return unchanged
      INTEGER			NOUT

C error indicator
      LOGICAL			LERR

C 11/95 introduce new variable to reduce text output -
C if oshort =
C 0: full text output
C 1: no in run progress reported
C 2: ditto plus no graph type output - only leaving
C    Minimum radius & conductance calculations.
C 3: All text output other than input card mirroring
C    and error messages turned off
C return unchanged
      INTEGER                   SHORTO

C ENDRAD: Radius above which a point is considered to have escaped
C area goes infinite in this case - return REQUIV=1E06
C return unchanged
      DOUBLE PRECISION          ENDRAD

C The centre about which the connolly grid is to be 
C done (s/r holcal normal supplies lowcen)
C needed here to work from 3d to 2d coord frames and back again
C return unchanged
      DOUBLE PRECISION          CENTRE(3)

C unit vectors in plane normal to cvect
C needed to a number of routines - originally h2dmap but now conn
C and addend
C these are normal - first is east direction (x on plane)
C                    and second is north     (y on plane)
C return unchanged
      DOUBLE PRECISION		PERPVE(3), PERPVN(3)

C arrays for connolly calc
C maximum size of the array STRMAX
C PARAMETER in s/r concal - return unchanged
      INTEGER			SCOMAX
C number of entries stored
      INTEGER			SCONUM
C a stored entry - a point with a radius above the probe radius connr(1)
      DOUBLE PRECISION		SCOXYZ( 3, SCOMAX)
C the sphere radius of the point
      DOUBLE PRECISION		SCORAD( SCOMAX)
C
C it then becomes a store of whether point should be
C included in output/ area calculations
! n.b. vble may now be changed in calculation if sphere has no intersection
! with plane under consideration
      LOGICAL			SCOTDO( SCOMAX)
C s/r coarea needs a store for the east/north coords of
C each point on grid - these are measured relative to the
C initial point
! SCOCIRCRAD: the radius of the point in the plane under consideration
! in the case of calls from concal this we equal scocrad
      DOUBLE PRECISION		SCOENC( 2, SCOMAX),
     &				SCOCIRCRAD( SCOMAX)

! a vector in the direction of the channel
! must be a unit vector - return unchanged
      DOUBLE PRECISION          CVECT(3)

! a sampling distance - the distance between planes
      DOUBLE PRECISION		SAMPLE

! peg option vbles
      LOGICAL			LPEG ! Turn on option
      INTEGER			SCO_PEG_ACC(SCOMAX) ! 0 if not peg access 1 or -1 if so
      LOGICAL			SCO_PEG_FLAG(SCOMAX) ! working loop
! now with  SHELL_ADD need to modify the radius of some spheres
! normally this would be equal to sprad but if sp_peg_acc = +/-2
! then this radius will be lower
      DOUBLE PRECISION		SCO_PEG_RAD(SCOMAX)	

      INTEGER			QPTOUT ! stream for qpt output, -1 if none 
      
C internal variables ***********

C loop count: stored points, list stored, perimeter of square
      INTEGER			SCOUNT, LCOUNT, ECOUNT, ACOUNT
      
C number of cycles
      INTEGER			NCYCLE

C double precision loop counts
      DOUBLE PRECISION		EDCNT, NDCNT

C working 3d coords, vble
      DOUBLE PRECISION		TSTXYZ(3), WORK, CENENC(2)

C maximum and minimum east and north coord
      DOUBLE PRECISION		EMAX, EMIN, NMAX, NMIN

C stores for the squares -
C have two the current working and the new one
c these are square arrays as it allows easy swapping between the two
C in each case store the bottom left of the square coord of the square
C all squares are the same size -
      DOUBLE PRECISION		SIZE(2)

C maximum number of entries
      INTEGER			SNMAX
      PARAMETER(		SNMAX = 90000)

C the number of entries
      INTEGER			SNO(2)

C bottom left of the square in east/north coords
      DOUBLE PRECISION		SENC( 2, SNMAX, 2)

C list that we are working from (1 or 2) and that we are to use next (the other one)
      INTEGER			IFROM, ITO, IDUM

C what we want to calculate the area, that in grey, low estimate of requiv,
C tolerance of estimation
      DOUBLE PRECISION		AREA, AREAG, RLOW, TOL

C previous value of requiv
      DOUBLE PRECISION		OLDREQ

C square root of 2, pi
      DOUBLE PRECISION		ROOT2, PI

C working vbles
      DOUBLE PRECISION		DIST2, DIFFE, DIFFN

C LBLACK: one or more points on the square are black (inside a circle)
      LOGICAL			LBLACK

C edge store for a square 8 points on its perimeter
      DOUBLE PRECISION		EDGE(2,8)
C if EDBLCK(I) = 0 then point I is white - ouside all circles
C but if EDBLCK(I) = 1 then point I is black inside one circle
C by adding all 8 of the EDBLCK's together can assess square as white, black or grey
      INTEGER		  	EDBLCK(8), SUMALL

      DOUBLE PRECISION		RELCCOORD ! relative channel coord

      INTEGER			TELLN, TELLNNOT
      
C end of decs ******************

C initialize REQUIV
      REQUIV = 0D0
C initialize EMAX etc.
      EMAX = -1D10
      EMIN = +1D10
      NMAX = -1D10
      NMIN = +1D10
      ROOT2 = SQRT(2D0)
      PI = 2.*ASIN(1.)
D      WRITE( NOUT, '(A,3F8.3)') 'D-line coarea: CENTRE= ', CENTRE
D      WRITE( NOUT, '(A,3F8.3)') 'D-line coarea: ENDRAD= ', ENDRAD
D      WRITE( NOUT, '(A,3F8.3)') 'D-line coarea: SAMPLE= ', SAMPLE
D      WRITE( NOUT, '(A,3F8.3)') 'D-line coarea: CVECT= ', CVECT
D      WRITE( NOUT, '(A,I8)') 'D-line coarea: SCONUM= ', SCONUM


C first go thru all points:
C establish (1) whether any points have a radius above endrad (area then infinite!)
C           (2) the 2d (scoenc) coordinates of all the points
C           (3) the extent of the slab - maximum and minimum east and north coords
!            new:
!	    (4) the channel coordinate of each sphere 
!           (5) circular radius of each point
      TELLN = 0
      DO SCOUNT = 1, SCONUM

! dot product with the channel vector is the relative channel coord
        IF (SCOTDO(SCOUNT)) THEN
C take off centre coords.
          TSTXYZ(1) = SCOXYZ(1,SCOUNT) - CENTRE(1)
          TSTXYZ(2) = SCOXYZ(2,SCOUNT) - CENTRE(2)
          TSTXYZ(3) = SCOXYZ(3,SCOUNT) - CENTRE(3)
! so relccoord gives distance between the sphere centre under
! consideration and the plane specified
	  CALL dDOT( TSTXYZ, CVECT, RELCCOORD)
          
	  IF (ABS(RELCCOORD).LT.1E-09) THEN 
	  ! sphere is centred on the plane in consideration
	    SCOCIRCRAD( SCOUNT) = SCORAD(SCOUNT)
	    TELLN = TELLN + 1
	  ELSEIF ((ABS(RELCCOORD).LT.SCORAD(SCOUNT)) .AND.
     &            (ABS(RELCCOORD).LT.0.9*SAMPLE)          ) THEN
          ! sphere cuts the plane in consideration
	  ! use pythagorous to find radius
	    SCOCIRCRAD( SCOUNT) = SQRT(SCORAD(SCOUNT)**2 -
     &                                   RELCCOORD**2)
            TELLN = TELLN + 1
	    !write(nout,'(a)') 'debug reduced radius'
          ELSE
	  ! sphere does not cut the plane in consideration
	  ! &/or out of range
	    SCOCIRCRAD( SCOUNT) = -1E10
	    SCOTDO(SCOUNT) = .FALSE.
	    !write(nout,'(a)') 'debug no overlap'
          ENDIF
D          write( nout, *) 'debug ', scount, 
D     &                            real(relccoord), 
D     &                            real(scorad(scount)), 
D     &                            real(scocircrad(scount))
! unbounded object? - indicate by sticking REQUIV v. large
! may still continue for graphics?
          IF (SCOCIRCRAD(SCOUNT).GT.ENDRAD) THEN
	    REQUIV = 1E6
	    IF (SHORTO.LT.2) WRITE( NOUT, '(A)')
     &'   Connolly area calc - Escaped so infinite area and Requiv!'
            GOTO 55555
	  ENDIF
	ENDIF
! after this point in routine all scorad's should now be scocircrad
      ENDDO
      
      
      
      IF (SHORTO.LT.2) WRITE( NOUT, '(A,I5,A)')
     &' Area calculation have found ', TELLN, 
     &                      ' spheres on the plane in question'
      TELLN = 0
      TELLNNOT = 0
! have got list of circles that cut the plane in question
! but if doing peg calculation need to replace with list of
! peg accessible circles that contact any of these
      IF (LPEG) THEN
        DO SCOUNT = 1, SCONUM  ! go thru circles - peg loop
	  SCO_PEG_FLAG(SCOUNT) = .FALSE.
	  IF (SCO_PEG_ACC(SCOUNT).NE.0) THEN ! Q: is this a PEG circle?
	    ! A: yes have a PEG sphere
	    ! Q2: does this cut the plane in question?
            TSTXYZ(1) = SCOXYZ(1,SCOUNT) - CENTRE(1)
            TSTXYZ(2) = SCOXYZ(2,SCOUNT) - CENTRE(2)
            TSTXYZ(3) = SCOXYZ(3,SCOUNT) - CENTRE(3)
! so relccoord gives distance between the sphere centre under
! consideration and the plane specified
	    CALL dDOT( TSTXYZ, CVECT, RELCCOORD)
	    IF (ABS(RELCCOORD).LT.SCO_PEG_RAD(SCOUNT)) THEN ! Q2: does this cut the plane in question?
              ! A2: yes cuts
	      ! find its circle radius
	      SCOCIRCRAD( SCOUNT) = SQRT(SCO_PEG_RAD(SCOUNT)**2 -
     &                                   RELCCOORD**2)
              ! now to be active this must also cut one of the
	      ! existing spheres on the plane - this is to exclude
	      ! spheres from escaped side routes
              DO ACOUNT = 1, SCONUM ! another count thru
	        IF (SCOTDO(ACOUNT).AND.                  ! pick up recognized spheres
     &              (.NOT.SCO_PEG_FLAG(SCOUNT))  ) THEN  ! DO NOT COUNT TWICE
	        ! find the distance between spheres acount and scount
	          DIST2 = (SCOXYZ(1,SCOUNT)-SCOXYZ(1,ACOUNT))**2 +
     &                    (SCOXYZ(2,SCOUNT)-SCOXYZ(2,ACOUNT))**2 +
     &                    (SCOXYZ(3,SCOUNT)-SCOXYZ(3,ACOUNT))**2 
                  IF (DIST2.LT.
     &                (SCO_PEG_RAD(SCOUNT)+SCORAD(ACOUNT))**2) THEN
		    SCO_PEG_FLAG(SCOUNT) = .TRUE.
		    TELLN = TELLN + 1
		  ENDIF
		ENDIF
	      ENDDO !{another count thru}
	      IF (.NOT.SCO_PEG_FLAG(SCOUNT)) TELLNNOT = TELLNNOT + 1
	    ENDIF ! Q2: does this cut the plane in question?   
	  ENDIF! {Q: is this a PEG circle?}
        ENDDO! {go thru circles - peg loop}
        IF (SHORTO.LT.2) WRITE( NOUT, '(A/ A,I5,A/ A,I5,A)')
     &' Area calculation *** PEG variation ***',
     &'      have found ', TELLN, 
     &' PEG spheres cut the plane & touch one of the on plane spheres',
     &'      and ', TELLNNOT, 
     &' PEG spheres that cut the plane but do not touch '//
     &                                 'an on plane sphere'
        ! now lets swap in the PEG circles
        DO SCOUNT = 1, SCONUM  ! go thru circles - peg loop
	  SCOTDO(SCOUNT) = SCO_PEG_FLAG(SCOUNT)
	ENDDO

      ENDIF
      
	
      ! have list of circles to work on  
      ! find the (a) east, north coords
      ! and bounding box
      DO SCOUNT = 1, SCONUM
C only if the point active and small
C work out 2d coords
        IF (SCOTDO(SCOUNT).AND.(SCOCIRCRAD(SCOUNT).LT.ENDRAD)) THEN
C take off centre coords.
          TSTXYZ(1) = SCOXYZ(1,SCOUNT) - CENTRE(1)
          TSTXYZ(2) = SCOXYZ(2,SCOUNT) - CENTRE(2)
          TSTXYZ(3) = SCOXYZ(3,SCOUNT) - CENTRE(3)
C dot product with east vector is the east coord
          CALL dDOT( TSTXYZ, PERPVE, SCOENC(1,SCOUNT))
C dot product with north vector is the north coord
          CALL dDOT( TSTXYZ, PERPVN, SCOENC(2,SCOUNT))


C maximum/minimum EAST
	  WORK = SCOENC(1,SCOUNT)+SCOCIRCRAD(SCOUNT)
          IF (WORK.GT.EMAX) EMAX = WORK
	  WORK = SCOENC(1,SCOUNT)-SCOCIRCRAD(SCOUNT)
          IF (WORK.LT.EMIN) EMIN = WORK
C maximum/minimum NORTH
	  WORK = SCOENC(2,SCOUNT)+SCOCIRCRAD(SCOUNT)
          IF (WORK.GT.NMAX) NMAX = WORK
	  WORK = SCOENC(2,SCOUNT)-SCOCIRCRAD(SCOUNT)
          IF (WORK.LT.NMIN) NMIN = WORK
        ENDIF
      ENDDO
      
D      write( nout, '(a,f8.3)') 'D-line coarea: emax= ', emax
D      write( nout, '(a,f8.3)') 'D-line coarea: emin= ', emin
D      write( nout, '(a,f8.3)') 'D-line coarea: nmax= ', nmax
D      write( nout, '(a,f8.3)') 'D-line coarea: nmin= ', nmin
D      write( nout, '(a,f8.3)') 'D-line coarea: requiv= ', requiv
      ! trap no circles found
      IF (ABS(EMAX+1D10).LT.0.001) THEN
        IF (SHORTO.LT.2) WRITE( NOUT, '(A)')
     &' Connolly area calc: Cannot find any accessible circles'
        GOTO 55555
      ENDIF
      
C now need to go thru grid from (emin,nmin) to (emax,nmax) considering
C squares of size .25 angs. 
      SIZE(1) = 0.25D0
C initialize store #1
      SNO(1) = 0
C initialize AREA store
      AREA = 0D0
C go thru, east /west
      DO 20 EDCNT = EMIN, EMAX, SIZE(1)
        DO 30 NDCNT = NMIN, NMAX, SIZE(1)
C condsidering square bottom left (EDCNT,NDCNT)
C centre coord
          CENENC(1) = EDCNT + 0.5*SIZE(1)
          CENENC(2) = NDCNT + 0.5*SIZE(1)

D         WRITE( NOUT, '(A,2F8.3,A,2F8.3)')  
D    &'D-line considering BL=  ', EDCNT, NDCNT, ' CEN=', 
D    &                            CENENC(1), CENENC(2)

C On initial pass use crude measure - 
C a square is filled  (black) if clearly wholey within a single sphere 
C a square is unfilled (white) it has no intersection with any sphere
C squares which fufil neither are grey and must be further considered
          LBLACK = .FALSE. 
C- cuts down the total squares to be considered.
C go thru "circles" 1 by 1
          DO 40 SCOUNT = 1, SCONUM
C an active circle?
            IF (SCOTDO(SCOUNT).AND.(SCOCIRCRAD(SCOUNT).LT.ENDRAD)) THEN
C yes have an active circle
C find the distance squared between the centre of the circle and the centre of the square
              DIFFE = CENENC(1) - SCOENC(1,SCOUNT)
              DIFFN = CENENC(2) - SCOENC(2,SCOUNT)
              DIST2 = DIFFE*DIFFE + DIFFN*DIFFN
C for a black situation then
C the distance has to be less than the radius of the sphere/circle with the 
C half diagonal size of the square taken off.
              WORK = SCOCIRCRAD(SCOUNT) - 0.5D0*ROOT2*SIZE(1)
              WORK = WORK*WORK
              IF (DIST2.LT.WORK) THEN
C square is black so add it to the area
                AREA = AREA + SIZE(1)*SIZE(1)
D               WRITE( NOUT, '(A,I5)')  
D    &'D-line      square black SCOUNT= ', SCOUNT
D               CALL COARDD( NOUT, EDCNT, NDCNT, SIZE(1),
D    &                       CENTRE, PERPVE, PERPVN)
C jump out and consider next
                GOTO 30
              ENDIF 
C grey if the square could overlap
              WORK = SCOCIRCRAD(SCOUNT) + 0.5D0*ROOT2*SIZE(1)
              WORK = WORK*WORK
              IF (DIST2.LT.WORK) LBLACK = .TRUE.
            ENDIF
40        CONTINUE
C is the square grey?
          IF (LBLACK) THEN
C yes - add it to list #1
            IF (SNO(1).GE.SNMAX) THEN
	      WRITE( NOUT, '(A)')
     &' ERROR SNMAX reached in s/r coarea',
     &' this should not generally happen!',
     &' please report to Oliver Smart o.s.smart@bham.ac.uk',
     &' with input and output files'              
              LERR = .TRUE.
              GOTO 55555
            ENDIF
            SNO(1) = SNO(1) + 1
            SENC( 1, SNO(1), 1) = EDCNT
            SENC( 2, SNO(1), 1) = NDCNT
D               WRITE( NOUT, '(A,I5)')  
D    &'D-line      square grey SNO(1)= ', SNO(1)
          ELSE
C not grey but white do nothing
D               WRITE( NOUT, '(A)')  
D    &'D-line      square white'
          ENDIF
30      CONTINUE
20    CONTINUE

C grey area
      AREAG = REAL(SNO(1))*SIZE(1)*SIZE(1)
      RLOW = SQRT(AREA/PI)
      REQUIV = SQRT((AREA+0.5*AREAG)/PI)

D     IF (SHORTO.LT.2) WRITE( NOUT, '(///A,I5/ 10(A,F10.4)))') 
D    &'   Connolly area calc cycle 0  squares stored= ', SNO(1),
D    &'        black area= ', AREA,  ' grey area= ', AREAG,
D    &   ' Estimated Requiv= ',  REQUIV, ' +/- ', REQUIV-RLOW, ' angs'

C have got grey list number #1 - begin cycle
C work from list 1 to list 2
      IFROM = 1
      ITO   = 2
      NCYCLE = 0
C jump back here at the end of a cyle
777   CONTINUE
        NCYCLE = NCYCLE + 1
C clear new list
        SNO(ITO) = 0
C size of new squares - half that of old
        SIZE(ITO) = 0.5*SIZE(IFROM)

D       WRITE( NOUT, '(A,I5)')  
D    &'D-line  new cycle NCYCLE= ', NCYCLE,
D    &'D-line            IFROM=  ', IFROM,
D    &'D-line            ITO=    ',   ITO
D       WRITE( NOUT, '(A,F8.5)')  
D    &'D-line            SIZE(ITO)=   ', SIZE(ITO),
D    &'D-line            SIZE(IFROM)= ', SIZE(IFROM)


C go through list
        DO 60 LCOUNT = 1, SNO(IFROM)
C look at square size size(ifrom)*size(ifrom)
C bottom left coord senc(1,lcount,ifrom), senc(2,lcount,ifrom)
C centre coords
          CENENC(1) = SENC(1,LCOUNT,IFROM) + 0.5*SIZE(IFROM)
          CENENC(2) = SENC(2,LCOUNT,IFROM) + 0.5*SIZE(IFROM)
C edge coords 
C bottom left
          EDGE(1,1) = SENC(1,LCOUNT,IFROM)
	  EDGE(2,1) = SENC(2,LCOUNT,IFROM)
	  EDBLCK(1) = 0
C bottom middle
          EDGE(1,2) = SENC(1,LCOUNT,IFROM) + 0.5*SIZE(IFROM)
	  EDGE(2,2) = SENC(2,LCOUNT,IFROM)
	  EDBLCK(2) = 0
C bottom right
          EDGE(1,3) = SENC(1,LCOUNT,IFROM) + SIZE(IFROM)
	  EDGE(2,3) = SENC(2,LCOUNT,IFROM)
	  EDBLCK(3) = 0

C coords middle left
          EDGE(1,4) = SENC(1,LCOUNT,IFROM)
	  EDGE(2,4) = SENC(2,LCOUNT,IFROM) + 0.5*SIZE(IFROM)
	  EDBLCK(4) = 0
C no middle middle
C middle right
          EDGE(1,5) = SENC(1,LCOUNT,IFROM) + SIZE(IFROM)
	  EDGE(2,5) = SENC(2,LCOUNT,IFROM) + 0.5*SIZE(IFROM)
	  EDBLCK(5) = 0
C top left
          EDGE(1,6) = SENC(1,LCOUNT,IFROM)
	  EDGE(2,6) = SENC(2,LCOUNT,IFROM) + SIZE(IFROM)
	  EDBLCK(6) = 0
C top middle
          EDGE(1,7) = SENC(1,LCOUNT,IFROM) + 0.5*SIZE(IFROM)
	  EDGE(2,7) = SENC(2,LCOUNT,IFROM) + SIZE(IFROM)
	  EDBLCK(7) = 0
C top right
          EDGE(1,8) = SENC(1,LCOUNT,IFROM) + SIZE(IFROM)
	  EDGE(2,8) = SENC(2,LCOUNT,IFROM) + SIZE(IFROM)
	  EDBLCK(8) = 0

C look thru all active circles  
          DO 70 SCOUNT = 1, SCONUM
C an active circle?
            IF (SCOTDO(SCOUNT).AND.(SCOCIRCRAD(SCOUNT).LT.ENDRAD)) THEN
C yes have an active circle
C find the distance squared between the centre of the circle and the centre of the square
              DIFFE = CENENC(1) - SCOENC(1,SCOUNT)
              DIFFN = CENENC(2) - SCOENC(2,SCOUNT)
              DIST2 = DIFFE*DIFFE + DIFFN*DIFFN
C if the square could not overlap then consider next circle without looking at edge points
              WORK = SCOCIRCRAD(SCOUNT) + 0.5D0*ROOT2*SIZE(IFROM)
              WORK = WORK*WORK
              IF (DIST2.GT.WORK) GOTO 70
C circle and square may overlap
C so look at all eight of the edge points
              DO 80 ECOUNT = 1, 8
                DIFFE = EDGE(1,ECOUNT) - SCOENC(1,SCOUNT)
                DIFFN = EDGE(2,ECOUNT) - SCOENC(2,SCOUNT)
                DIST2 = DIFFE*DIFFE + DIFFN*DIFFN
                WORK = SCOCIRCRAD(SCOUNT)
                WORK = WORK*WORK
C if point is within circle then change flag
	        IF (DIST2.LT.WORK) EDBLCK(ECOUNT) = 1
80            CONTINUE
C are all edge points black - if so jump out to save time
               SUMALL = EDBLCK(1)+EDBLCK(2)+EDBLCK(3)+
     &                 EDBLCK(4)+EDBLCK(5)+EDBLCK(6)+
     &                 EDBLCK(7)+EDBLCK(8)
C all black SUMALL=8
               IF (SUMALL.EQ.8) GOTO 71
            ENDIF
70        CONTINUE
C Jump out prematurely here
71        CONTINUE
C add together colour of all points on edge of square
          SUMALL = EDBLCK(1)+EDBLCK(2)+EDBLCK(3)+
     &                 EDBLCK(4)+EDBLCK(5)+EDBLCK(6)+
     &                 EDBLCK(7)+EDBLCK(8)
C Q all edge points are black?
          IF (SUMALL.EQ.8) THEN
C yes!
C add its area to the total
            AREA = AREA + SIZE(IFROM)*SIZE(IFROM)
D               WRITE( NOUT, '(A,I5,A)')  
D    &'D-line      square', LCOUNT,' black'
D               CALL COARDD( NOUT, SENC(1,LCOUNT,IFROM), 
D    &                             SENC(2,LCOUNT,IFROM), SIZE(IFROM),
D    &                       CENTRE, PERPVE, PERPVN)
	  
C Q all edge points are white?
          ELSEIF (SUMALL.EQ.0) THEN
C yes! - do nothing
D               WRITE( NOUT, '(A,I5,A)')  
D    &'D-line      square', LCOUNT,' white'

	  ELSE
C edge points are neither all white nor all black - 
C so the square is grey - split it into 4 and add it to list #ITO
D               WRITE( NOUT, '(A,I5,A)')  
D    &'D-line      square', LCOUNT,' grey'
C hit array bound?
            IF (SNO(ITO)+4.GE.SNMAX) THEN
C if so - do not stop - just keep on incrementing store
               SNO(ITO) = SNO(ITO) + 4
            ELSE          
C bottom left
              SNO(ITO) = SNO(ITO) + 1
	      SENC( 1, SNO(ITO), ITO) = SENC(1,LCOUNT,IFROM)
	      SENC( 2, SNO(ITO), ITO) = SENC(2,LCOUNT,IFROM)
C bottom middle
              SNO(ITO) = SNO(ITO) + 1
	      SENC( 1, SNO(ITO), ITO) = SENC(1,LCOUNT,IFROM) + SIZE(ITO)
	      SENC( 2, SNO(ITO), ITO) = SENC(2,LCOUNT,IFROM)
C middle left
              SNO(ITO) = SNO(ITO) + 1
	      SENC( 1, SNO(ITO), ITO) = SENC(1,LCOUNT,IFROM) 
	      SENC( 2, SNO(ITO), ITO) = SENC(2,LCOUNT,IFROM) + SIZE(ITO)
C middle middle
              SNO(ITO) = SNO(ITO) + 1
	      SENC( 1, SNO(ITO), ITO) = SENC(1,LCOUNT,IFROM) + SIZE(ITO)
	      SENC( 2, SNO(ITO), ITO) = SENC(2,LCOUNT,IFROM) + SIZE(ITO)
	    ENDIF
	    
D               WRITE( NOUT, '(A,I5)')  
D    &'D-line      square grey -> to 4 SNO(ITO)= ', SNO(ITO)

	  ENDIF
	  
60      CONTINUE
C finished looking at all squares of list ifrom

C grey area
      AREAG = REAL(SNO(ITO))*SIZE(ITO)*SIZE(ITO)
C low estimate
      RLOW = SQRT(AREA/PI)
C best estimate 1/2 of grey stuff is white
      OLDREQ = REQUIV
      REQUIV = SQRT((AREA+0.5*AREAG)/PI)
      TOL = REQUIV-RLOW

      IF (SHORTO.LT.2) WRITE( NOUT, '(A,I3,A,F10.4,A,I5,10(A,F10.4)))') 
     &'   Connolly area calc cycle', NCYCLE,
     &' Requiv= ',  REQUIV, 
     &' squares stored= ', SNO(ITO),
     &'        black area= ', AREA,  ' grey area= ', AREAG,
     &   ' (tol= ', TOL, ') angs'

C tolerance within limits? 
C but do at least 3 cycles
      IF ( (ABS(REQUIV-OLDREQ).GT.0.0005) .OR.
     &     (NCYCLE.LT.4)                  )THEN
C if array bound exceeded cannot go on - flag error
        IF (SNO(ITO)+4.LE.SNMAX) THEN
C need another cycle - simply swap ifrom and ito
          IDUM = ITO
	  ITO = IFROM
	  IFROM = IDUM
C another cycle of testing
	  GOTO 777
	ELSE
	  WRITE( NOUT, '(A)')
     &'WARNING',
     &'Have reached square storage limit SNMAX in s/r coarea',
     &'before we have converged.  This means that the areas found',
     &'will be slightly inaccurate.'
	ENDIF
      ENDIF

! qptoutput option
      IF (QPTOUT.NE.-1) THEN ! qptout output option
! go thru last grey spheres stored
        DO LCOUNT = 1, SNO(IFROM)      
	  IF (MOD(LCOUNT,50).EQ.0) THEN ! only output 1 in 50
	    ! the centre of the square
	    CENENC(1) = SENC(1,LCOUNT,IFROM) + 0.5*SIZE(IFROM)
            CENENC(2) = SENC(2,LCOUNT,IFROM) + 0.5*SIZE(IFROM)
	    ! convert the 2d e/n coords to 3d normal coords
            CALL COACON( CENENC(1), CENENC(2), TSTXYZ, CENTRE, 
     &                   PERPVE, PERPVN)
            WRITE( QPTOUT) 4.0, REAL(TSTXYZ(1)), REAL(TSTXYZ(2)), 
     &                   REAL(TSTXYZ(3)) 
          ENDIF
	ENDDO ! lcount - grey spheres
      ENDIF


55555 CONTINUE
C      STOP ' Temp stop in coarea'
      RETURN
      END

      SUBROUTINE COACON( TSTE, TSTN, TSTXYZ, CENTRE, PERPVE, PERPVN)
      IMPLICIT NONE
C converts 2 coord (tste,tstn) to 3d coord tstxyz
C point in 2d, equivalent in 3d
      DOUBLE PRECISION  TSTE, TSTN, TSTXYZ(3)
C centre and vectors
      DOUBLE PRECISION CENTRE(3), PERPVE(3), PERPVN(3)
      
      TSTXYZ(1) = CENTRE(1) + TSTE*PERPVE(1) + TSTN*PERPVN(1)
      TSTXYZ(2) = CENTRE(2) + TSTE*PERPVE(2) + TSTN*PERPVN(2)
      TSTXYZ(3) = CENTRE(3) + TSTE*PERPVE(3) + TSTN*PERPVN(3)
      RETURN
      END
      
D     SUBROUTINE COARDD( NOUT, ECOORD, NCOORD, SIZE,
D    &			    CENTRE, PERPVE, PERPVN)
D     IMPLICIT NONE
C This s/r is a D (debug) routine to output a qpt type taged record (tag DCOARDD)
C to allow visual examination of progress of routine.
D     DOUBLE PRECISION ECOORD, NCOORD, SIZE, CENTRE(3), 
D    &		      PERPVE(3), PERPVN(3)
D     INTEGER	      NOUT
C point in 2d, equivalent in 3d
D     DOUBLE PRECISION  TSTE, TSTN, TSTXYZ(3)
C
C move to ECOORD, NCOORD bottom left
D     TSTE = ECOORD
D     TSTN = NCOORD
C convert to 3d
D     CALL COACON( TSTE, TSTN, TSTXYZ, CENTRE, PERPVE, PERPVN)
D     WRITE( NOUT, '(A,4F9.4)') 'DCOARDD', 2.0, TSTXYZ
C draw to bottom right
D     TSTE = ECOORD+SIZE
D     TSTN = NCOORD
D     CALL COACON( TSTE, TSTN, TSTXYZ, CENTRE, PERPVE, PERPVN)
D     WRITE( NOUT, '(A,4F9.4)') 'DCOARDD', 3.0, TSTXYZ
C draw to top right
D     TSTE = ECOORD+SIZE
D     TSTN = NCOORD+SIZE
D     CALL COACON( TSTE, TSTN, TSTXYZ, CENTRE, PERPVE, PERPVN)
D     WRITE( NOUT, '(A,4F9.4)') 'DCOARDD', 3.0, TSTXYZ
C draw to top left
D     TSTE = ECOORD
D     TSTN = NCOORD+SIZE
D     CALL COACON( TSTE, TSTN, TSTXYZ, CENTRE, PERPVE, PERPVN)
D     WRITE( NOUT, '(A,4F9.4)') 'DCOARDD', 3.0, TSTXYZ
C draw to bottom left
D     TSTE = ECOORD
D     TSTN = NCOORD
D     CALL COACON( TSTE, TSTN, TSTXYZ, CENTRE, PERPVE, PERPVN)
D     WRITE( NOUT, '(A,4F9.4)') 'DCOARDD', 3.0, TSTXYZ
C square finished - now a St Andrew's cross /
D     TSTE = ECOORD + 0.4*SIZE
D     TSTN = NCOORD + 0.4*SIZE
D     CALL COACON( TSTE, TSTN, TSTXYZ, CENTRE, PERPVE, PERPVN)
D     WRITE( NOUT, '(A,4F9.4)') 'DCOARDD', 2.0, TSTXYZ
D     TSTE = ECOORD + 0.6*SIZE
D     TSTN = NCOORD + 0.6*SIZE
D     CALL COACON( TSTE, TSTN, TSTXYZ, CENTRE, PERPVE, PERPVN)
D     WRITE( NOUT, '(A,4F9.4)') 'DCOARDD', 3.0, TSTXYZ
C square finished - now a St Andrew's cross /
D     TSTE = ECOORD + 0.4*SIZE
D     TSTN = NCOORD + 0.6*SIZE
D     CALL COACON( TSTE, TSTN, TSTXYZ, CENTRE, PERPVE, PERPVN)
D     WRITE( NOUT, '(A,4F9.4)') 'DCOARDD', 2.0, TSTXYZ
D     TSTE = ECOORD + 0.6*SIZE
D     TSTN = NCOORD + 0.4*SIZE
D     CALL COACON( TSTE, TSTN, TSTXYZ, CENTRE, PERPVE, PERPVN)
D     WRITE( NOUT, '(A,4F9.4)') 'DCOARDD', 3.0, TSTXYZ
C
D     RETURN
D     END
C test_routine |      program jiffy_test_coarea
C test_routine |      implicit none
C test_routine |c arrays for connolly calc
C test_routine |c maximum size of the array strmax
C test_routine |      integer			scomax
C test_routine |      parameter(		scomax= 10000)
C test_routine |c number of entries stored
C test_routine |      integer			sconum
C test_routine |c number of entries - that have been "checked"
C test_routine |c that is looking at the neighbouring eight points
C test_routine |c 
C test_routine |c      integer			scochk
C test_routine |c the next entry to check
C test_routine |      integer			sconxt
C test_routine |c a stored entry - a point with a radius above the probe radius connr(1)
C test_routine |      double precision		scoxyz( 3, scomax)
C test_routine |c the radius of the point
C test_routine |      double precision		scorad( scomax)
C test_routine |c logical vble whether point needs to be checked\
C test_routine |c if point is on grid then it must be if
C test_routine |c  produced by the spike procedure it need no
C test_routine |c 
C test_routine |c at the end of the procedure change meaning -
C test_routine |c it then becomes a store of whether point should be
C test_routine |c included in output/ area calculations
C test_routine |      logical			scotdo( scomax)
C test_routine |c s/r coarea needs a store for the east/north coords of
C test_routine |c each point on grid - these are measured relative to the
C test_routine |c initial point
C test_routine |      double precision		scoenc( 2, scomax)
C test_routine |      double precision reff, endrad, centre(3), perpve(3), perpvn(3),
C test_routine |     &			temp, pi
C test_routine |      logical lerr      
C test_routine |      endrad = 50.
C test_routine |      centre(1) = 0.
C test_routine |      centre(2) = 0.
C test_routine |      centre(3) = 0.
C test_routine |      perpve(1) = 1.
C test_routine |      perpve(2) = 0.
C test_routine |      perpve(3) = 0.
C test_routine |      perpvn(1) = 0.
C test_routine |      perpvn(2) = 1.
C test_routine |      perpvn(3) = 0.
C test_routine |      sconum = 2
C test_routine |      scorad(1) = 5.0
C test_routine |      scoxyz(1,1) = 0.
C test_routine |      scoxyz(2,1) = 0.
C test_routine |      scoxyz(3,1) = 0.
C test_routine |      scotdo(1) = .true.
C test_routine |      scorad(2) = 5.0
C test_routine |      scoxyz(1,2) = 2.*sqrt(12.5)
C test_routine |      scoxyz(2,2) = 0.
C test_routine |      scoxyz(3,2) = 0.
C test_routine |      scotdo(2) = .true.
C test_routine |      pi = 2d0*asin(1d0)
C test_routine |      temp=25.0*(1.5*pi+1.0) 
C test_routine |      write(*,*) 'true ans l100 area=', temp
C test_routine |      write(*,*) 'true ans l100 reqiv=', sqrt(temp/pi)
C test_routine |        call coarea( reff, 6, lerr, 0, endrad, 
C test_routine |     &		     centre, perpve, perpvn, 
C test_routine |     &               scomax, sconum, scoxyz, scorad, scotdo,
C test_routine |     &               scoenc)
C test_routine |      end


