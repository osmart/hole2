      SUBROUTINE CONCAL( NIN, NOUT, LERR, 
     &			 CVECT, CENTRE,
     &			 ATMAX, ATNO, ATBRK, ATRES, ATCHN, ATRNO,
     &			 ATXYZ, ATVDW, ATBND, 
     &                   SPDBSP, FPDBSP, 
     &			 SHORTO,
     &			 ENDRAD, CUTSIZE, CONNR,
     &			 PERPVE, PERPVN, REQUIV, PEG_WRITEALL, IREC, 
     &                   SAMPLE)
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
C s/r to perform connolly type surface calc within hole
C
C Modification history:
C
C Date  Author          Modification
C 01/00 O.S.S.    	CONN option introduced
C 10/00 O.S.S.		PEG_WRITEALL option added for PEG effect calcs

C passed vbles *****************

C input/output stream numbers
      INTEGER			NIN, NOUT

C error indicator
      LOGICAL			LERR

C a vector in the direction of the channel
C must be a unit vector - return unchanged
      DOUBLE PRECISION          CVECT(3)

C The centre about which the connolly grid is to be 
C done (s/r holcal normal supplies lowcen)
      DOUBLE PRECISION          CENTRE(3)

C maximum no. of atoms
      INTEGER                   ATMAX

C number of atoms read in from each file:
      INTEGER                   ATNO

C atom names found in Brookhaven file:
      CHARACTER*4               ATBRK(ATMAX)

C residue name in brook
      CHARACTER*3               ATRES(ATMAX)

C chain identifier in brookhaven pdb file
      CHARACTER*1               ATCHN(ATMAX)

C integer residue no.
      INTEGER                   ATRNO(ATMAX)

C co-ordinates
      DOUBLE PRECISION          ATXYZ( 3, ATMAX)

C vdw and bond radii of each atoms
      DOUBLE PRECISION          ATVDW(ATMAX), ATBND(ATMAX)


C pdb format output file 'atoms' are sphere centres
C default to none by setting FPDBSP to 'NONE'
      CHARACTER*200             FPDBSP
      INTEGER                   SPDBSP

C 11/95 introduce new variable to reduce text output -
C if oshort =
C 0: full text output
C 1: no in run progress reported
C 2: ditto plus no graph type output - only leaving
C    Minimum radius & conductance calculations.
C 3: All text output other than input card mirroring
C    and error messages turned off
      INTEGER                   SHORTO

C ENDRAD: Radius above which a point is considered to have escaped
C ENDRP3 also need to have a slightly higher value for the purposes of this s/r
      DOUBLE PRECISION          ENDRAD, ENDRP3

C cut off list control vble, -ve: no cutoff lists used in calculation
C otherwise this is the additional radius above the sphere radius
C to be used for cutoff list (see s/r holeen)
      DOUBLE PRECISION		CUTSIZE


C option CONN introduced 1/00 - 
C CONNR(1) is the connolly surface probe radius (default 1.15Angs)
C          the option is turned off by setting this radius negative
C CONNR(2) is the grid size to be used in the calculation - default .25 angs
      DOUBLE PRECISION		CONNR(2)

C unit vectors in plane normal to cvect
C needed to a number of routines - originally h2dmap but now conn
C and addend
C these are normal - first is east direction (x on plane)
C                    and second is north     (y on plane)
C return unchanged!!!
      DOUBLE PRECISION		PERPVE(3), PERPVN(3)

C area of final assembly returned as a effective radius = sqrt(area/pi)
      DOUBLE PRECISION		REQUIV

C write all the records? (for PEG calcs)
      LOGICAL 			PEG_WRITEALL

C record number for sph write
      INTEGER                   IREC

! a sampling distance - the distance between planes
      DOUBLE PRECISION		SAMPLE

C internal variables ***********

C a new point
      DOUBLE PRECISION          NEWCEN(3), NEWENG

C atom list no. with smallest dist-vdw radius, 2nd smallest
C 07/06/94 as iat's may or may not be supplied with previously 
C found numbers then we can use this to speed up the procedure
C Dec 97 add iat3 
      INTEGER			IAT1, IAT2, IAT3

C 2nd/3rd smallest distance-vdw radius (of iat2/3)
      DOUBLE PRECISION		DAT2, DAT3

C arrays for connolly calc
C maximum size of the array STRMAX
      INTEGER			SCOMAX
      PARAMETER(		SCOMAX= 10000)
C number of entries stored
      INTEGER			SCONUM
C number of entries - that have been "checked"
C that is looking at the neighbouring eight points
C 
C      INTEGER			SCOCHK
C the next entry to check
      INTEGER			SCONXT

C a stored entry - a point with a radius above the probe radius connr(1)
      DOUBLE PRECISION		SCOXYZ( 3, SCOMAX)
C the radius of the point
      DOUBLE PRECISION		SCORAD( SCOMAX)
C logical vble whether point needs to be checked\
C if point is on grid then it must be if
C  produced by the spike procedure it need no
C 
C at the end of the procedure change meaning -
C it then becomes a store of whether point should be
C included in output/ area calculations
      LOGICAL			SCOTDO( SCOMAX)
C s/r coarea needs a store for the east/north coords of
C each point on grid - these are measured relative to the
C initial point
! SCOCIRCRAD: the radius of the point in the plane under consideration
! in the case of calls from concal this we equal scocrad
      DOUBLE PRECISION		SCOENC( 2, SCOMAX),
     &				SCOCIRCRAD( SCOMAX)
      
      ! vbles needed by coarea - not used here only required
      ! for peg calls to coarea by sph_process
      INTEGER			SCO_PEG_ACC(SCOMAX) 
      LOGICAL			SCO_PEG_FLAG(SCOMAX)

CORIGC the status of the  surronding points
CORIGC if .false. then point in relevant direction has been accepted or not yet checked
CORIGC if .true. then last point in that direction has been rejected
CORIGC first variable is "East" and next is "North":
CORIGC i.e. point 0,0 is the point itself
CORIGC and directions:
CORIGC  (-1,+1) ( 0,+1) (+1,+1)
CORIGC  (-1, 0)  Cpoint (+1, 0)
CORIGC  (-1,-1) ( 0,-1) (+1,-1)
CORIG      LOGICAL			SCONEI( -1:1, -1:1, SCOMAX)
CORIG
CORIGC number of new points
CORIG INTEGER			SCONEW

C GRID SIZE, working variable - dist2
      DOUBLE PRECISION 		GRIDS, DIST2, DIFF1, DIFF2, DIFF3

C loop counts
      INTEGER			NCOUNT, ECOUNT, SCOUNT, JCOUNT, XCOUNT, 
     &				CCOUNT

C test point
      DOUBLE PRECISION		TSTXYZ(3)

C variables for spike like routine
      DOUBLE PRECISION		VDISP(3), DELTA

C max radius indicator
      DOUBLE PRECISION		MAXRAD

C number of points eliminated, elimination distance
      INTEGER			NELIM
      DOUBLE PRECISION		DELIM2

! for writing .sph file correctly
      INTEGER			WRESNO
      DOUBLE PRECISION 		WREQUIV



C end of decs ******************

C bigger value of endrad
      ENDRP3 = ENDRAD + 3D0

      IF (SHORTO.LT.2) WRITE( NOUT, '(A)')
     & ' Connolly routine for this plane'
D	WRITE( NOUT, '(//A/ A,F8.3/ A,F8.3/ A,3F8.3)') 
D    & 'D-line: Call to s/r concal',
D    &' D-line: CONNR(1) = ', CONNR(1),
D    &' D-line: CONNR(2) = ', CONNR(2),
D    &' D-line: CENTRE = ', CENTRE(1), CENTRE(2), CENTRE(3)

C find the pore radius of the initial point
      CALL HOLEEN( CENTRE, NEWENG, IAT1, IAT2, IAT3, DAT2, DAT3,
     &           ATMAX, ATNO, ATXYZ, ATVDW, CUTSIZE)

D      WRITE( NOUT, '(A,F8.3,A)')
D    & 'D-line pore radius of supplied CENTRE = ', -NEWENG, ' Angs'

C is the hole pore radius less than the probe radius - if so cannot do connolly
       IF (-NEWENG.LT.CONNR(1)) THEN
	   WRITE(NOUT, '(/ A/ A,F8.3,A,F8.3/ A)') 
     &' Connolly routine (s/r concal)',
     &'   initial point probe radius=', -NEWENG,
     &              ' less than probe radius=',CONNR(1),
     &'   So using HOLE point for calcs........'
     
C simply return the hole radius as the effective
        REQUIV = -NEWENG

      ELSE
C initial point has a pore radius above the probe radius so can check
C initialize array - one point - the initial
        SCONUM = 1
C next point to check is the first
        SCONXT = 1
        SCOXYZ(1,SCONUM) = CENTRE(1)
        SCOXYZ(2,SCONUM) = CENTRE(2)
        SCOXYZ(3,SCONUM) = CENTRE(3)
        SCORAD(SCONUM) = -NEWENG
	SCOTDO(SCONUM) = .TRUE.
C gridsize - set outside this routine 
C (n.b. neighbouring points must overlap)
        GRIDS = CONNR(2)

C jump back here to check next unchecked point
222    CONTINUE
C ignore points that have a radius above ENDRP3 - these should not spawn more
C and points that have been generated by spike procedure
        IF ( (SCORAD(SCONXT).LT.ENDRP3) .AND.
     &              SCOTDO(SCONXT)            ) THEN
C mark the point as checked
          SCOTDO(SCONXT) = .FALSE.
C check points around scoxyz(1,SCONXT), scoxyz(2,SCONXT), scoxyz(3,SCONXT)
C increment south, zero, north
          DO 15 NCOUNT = -1, 1
            DO 25 ECOUNT = -1, 1
C test point:
              TSTXYZ(1) = SCOXYZ(1,SCONXT) 
              TSTXYZ(2) = SCOXYZ(2,SCONXT) 
              TSTXYZ(3) = SCOXYZ(3,SCONXT) 
C movement east by grids
              TSTXYZ(1) = TSTXYZ(1) + REAL(ECOUNT)*GRIDS*PERPVE(1)
              TSTXYZ(2) = TSTXYZ(2) + REAL(ECOUNT)*GRIDS*PERPVE(2)
              TSTXYZ(3) = TSTXYZ(3) + REAL(ECOUNT)*GRIDS*PERPVE(3)
C movement north by grids
              TSTXYZ(1) = TSTXYZ(1) + REAL(NCOUNT)*GRIDS*PERPVN(1)
              TSTXYZ(2) = TSTXYZ(2) + REAL(NCOUNT)*GRIDS*PERPVN(2)
              TSTXYZ(3) = TSTXYZ(3) + REAL(NCOUNT)*GRIDS*PERPVN(3)
C Q: have we already produced this point?
C look through existing list
              DO 67 SCOUNT = 1, SCONUM
C find distance**2 between the points
                DIFF1 = TSTXYZ(1)-SCOXYZ(1,SCOUNT)
                DIFF2 = TSTXYZ(2)-SCOXYZ(2,SCOUNT)
                DIFF3 = TSTXYZ(3)-SCOXYZ(3,SCOUNT)
                DIST2 = DIFF1*DIFF1 + DIFF2*DIFF2 + DIFF3+DIFF3
                IF (DIST2.LT.GRIDS/1000D0) THEN
C this point already considered and accepted
CORIG                  SCONEI( ECOUNT, NCOUNT, SCONXT) = .FALSE.
D                 WRITE( NOUT, '(A,3F8.3)')
D    & 'D-line Have already considered + accepted point= ', 
D    &       TSTXYZ(1), TSTXYZ(2),TSTXYZ(3)
C jump to next point
                  GOTO 25
                ENDIF
67            CONTINUE
            
C find the radius of this point
              CALL HOLEEN( TSTXYZ, NEWENG, IAT1, IAT2, IAT3, DAT2, DAT3,
     &                   ATMAX, ATNO, ATXYZ, ATVDW, CUTSIZE)
C change sign to give +ve pore radius
              NEWENG = -NEWENG
C Q: is this an accepted point?
              IF (NEWENG.GE.CONNR(1)) THEN
C YES this is an accepted point (and its unique)
CORIGC first store information for point that produced this
CORIG                SCONEI( ECOUNT, NCOUNT, SCONXT) = .FALSE.

C now add point onto end of list to be considered
C array check here
                IF (SCONUM+1.GT.SCOMAX) THEN
                  WRITE( NOUT, '(A)')
     &' ERROR SCOMAX reached in s/r concal',
     &' this should not generally happen (unless grid set too small)!',
     &' please report to Oliver Smart o.s.smart@bham.ac.uk',
     &' with input and output files'
                  LERR = .TRUE.
                  GOTO 55555
                ENDIF
C store the extra point
                SCONUM = SCONUM + 1
                SCOXYZ(1,SCONUM) = TSTXYZ(1)
                SCOXYZ(2,SCONUM) = TSTXYZ(2)
                SCOXYZ(3,SCONUM) = TSTXYZ(3)
                SCORAD(SCONUM) = NEWENG
C this is a not spike so must be checked
		SCOTDO(SCONUM) = .TRUE.
D               WRITE( NOUT, '(A,3F8.3,A,F8.3,A)')
D    & 'D-line ACCEPT point= ', TSTXYZ(1), TSTXYZ(2),TSTXYZ(3),
D    &               ' radius= ', NEWENG, ' Angs'
C must finish as soon as a ENDRP3 point arrived at
                IF (NEWENG.GT.ENDRP3) THEN
		  IF (SHORTO.LT.2) WRITE( NOUT, '(A)')
     &' Escaped to outside at this level'
                  GOTO 333 
		ENDIF

              ELSE
C NO this point is rejected (Q: is this an accepted point?)
CORIGC first store information for point that produced this
CORIG                SCONEI( ECOUNT, NCOUNT, SCONXT) = .TRUE.
D               WRITE( NOUT, '(A,3F8.3,A,F8.3,A)')
D    & 'D-line REJECT point= ', TSTXYZ(1), TSTXYZ(2),TSTXYZ(3),
D    &               ' radius= ', NEWENG, ' Angs'
C "spike" procedure - adjust point towards originating vector until
C it gets to desired radius
C vector starts from SCOXYZ(*,SCONXT) and goes to TSTXYZ(*)
                VDISP(1) = TSTXYZ(1)-SCOXYZ(1,SCONXT)
                VDISP(2) = TSTXYZ(2)-SCOXYZ(2,SCONXT)
                VDISP(3) = TSTXYZ(3)-SCOXYZ(3,SCONXT)
C make unit
                CALL DUVEC2( VDISP)
C DISP is the proportion displacement along this vector 
C will adjust this to give a pore radius equal to the probe
C first estimate approx way along
                DELTA = 0.25D0*GRIDS
C next bit is analogous to procedure in hodotu.f for producing spikes
C do ten cycles of refinement to find sphere centre SEE f048 FOR METHOD
                DO 55 CCOUNT = 1, 100
C load up current point dum is the distance along line 
C from the sphere which is current estimate of overlap point.
                  TSTXYZ(1) = SCOXYZ(1,SCONXT) + DELTA*VDISP(1)
                  TSTXYZ(2) = SCOXYZ(2,SCONXT) + DELTA*VDISP(2)
                  TSTXYZ(3) = SCOXYZ(3,SCONXT) + DELTA*VDISP(3)
C find the radius of this point
                  CALL HOLEEN( TSTXYZ, NEWENG, IAT1, IAT2, IAT3, 
     &			       DAT2, DAT3,
     &                        ATMAX, ATNO, ATXYZ, ATVDW, CUTSIZE)
C make neweng=poreradius
                  NEWENG = -NEWENG
D               WRITE( NOUT, '(A,I5,AF8.3,A,F8.3,A)')
D    & 'D-line SPIKE cycle ',CCOUNT,' DELTA= ', DELTA,
D    &                    ' radius= ', NEWENG, ' Angs'                  
C take off the probe radius plus a very little bit
                  NEWENG =  NEWENG - (CONNR(1)+0.0001D0)
		  IF (ABS(NEWENG).LT.0.0005) GOTO 56
C if this number is greater than zero then we are too close to point SCONXT
C so add number on
                  DELTA = DELTA + NEWENG
C as we go thru this cycle the pore radius at the point should get closer to probe radius
55              CONTINUE
C at the end should have a point close to the probe radius
C store it if good enough         
C very occasionally delta goes less than zero - this can produce big jumps
C so eliminate these points  
56              CONTINUE
                IF ((ABS(NEWENG).LT.0.0005).AND.DELTA.GT.0.0) THEN
C check to see whether point is close to an already existing point
C look through existing list
                   DO 667 SCOUNT = 1, SCONUM
C find distance**2 between the points
                     DIFF1 = TSTXYZ(1)-SCOXYZ(1,SCOUNT)
                     DIFF2 = TSTXYZ(2)-SCOXYZ(2,SCOUNT)
                     DIFF3 = TSTXYZ(3)-SCOXYZ(3,SCOUNT)
                     DIST2 = DIFF1*DIFF1 + DIFF2*DIFF2 + DIFF3+DIFF3
C distance must be above 0.3 angs 
                     IF (DIST2.LT.0.09D0) THEN
C this point already considered and accepted
CORIG                  SCONEI( ECOUNT, NCOUNT, SCONXT) = .FALSE.
D                 WRITE( NOUT, '(A,3F8.3)')
D    & 'D-line Point close (.3 angs) to already stored point '
C jump to next point
                       GOTO 25
                     ENDIF
667               CONTINUE

C array check here
                  IF (SCONUM+1.GT.SCOMAX) THEN
                    WRITE( NOUT, '(A)')
     &' ERROR SCOMAX reached in s/r concal',
     &' this should not generally happen (unless grid set too small)!',
     &' please report to Oliver Smart o.s.smart@bham.ac.uk',
     &' with input and output files'
                    LERR = .TRUE.
                    GOTO 55555
                  ENDIF
C store point
                  SCONUM = SCONUM + 1
                  SCOXYZ(1,SCONUM) = TSTXYZ(1)
                  SCOXYZ(2,SCONUM) = TSTXYZ(2)
                  SCOXYZ(3,SCONUM) = TSTXYZ(3)
C get back to the pore radius
                  SCORAD(SCONUM) = NEWENG + (CONNR(1)+0.0001D0)
C this is a spike - so should not produce any new point
		  SCOTDO(SCONUM) = .FALSE.
	        ENDIF
              ENDIF
25          CONTINUE
15        CONTINUE
C finished considering point SCONXT
        ENDIF

C jump back to consider next point if necessary
C at each stage the highest radius point propogates
C this is to keep number of points down when we get escape to infinity
C initialize maximum radius array
        MAXRAD = -1D10
	SCONXT = -1000
C go thru all the point
        DO 701 SCOUNT = 1, SCONUM
          IF (SCOTDO(SCOUNT)) THEN
	     IF (SCORAD(SCOUNT).GT.MAXRAD) THEN
	       SCONXT = SCOUNT
	       MAXRAD = SCORAD(SCOUNT)
	     ENDIF
	  ENDIF
701     CONTINUE

C do we have to jump back - are there any points to check?
        IF (SCONXT.GT.0) THEN
D         	WRITE( NOUT, '(A,I5,A,F8.3)')
D    & 'D-line Jump back next point SCONXT= ', SCONXT, ' MAXRAD= ', MAXRAD
          GOTO 222
        ENDIF

C Jump out here if ENDRP3 is hit
333     CONTINUE
C tell user vectors used to define rotation
        IF (SHORTO.LT.2) WRITE( NOUT, '(A,I5,A)')
     & ' Have stored total of ', SCONUM, ' points'

C eliminate unnecessary points.
C use SCOTDO as a flag for consideration
C set all to true
        DO 702 SCOUNT = 1, SCONUM
          SCOTDO(SCOUNT) = .TRUE.
702     CONTINUE
C if we have found an end (which will allways be the last point found)
C then eliminate all points whose centres lie within it
        IF (SCORAD(SCONUM).GT.ENDRP3) THEN
	  NELIM = 0
          DELIM2 = SCORAD(SCONUM)
	  DELIM2 = DELIM2*DELIM2
C go thru points - but not the last
          DO 703 SCOUNT = 1, SCONUM-1
C find distance**2 between the point and end point
            DIFF1 = SCOXYZ(1,SCONUM)-SCOXYZ(1,SCOUNT)
            DIFF2 = SCOXYZ(2,SCONUM)-SCOXYZ(2,SCOUNT)
            DIFF3 = SCOXYZ(3,SCONUM)-SCOXYZ(3,SCOUNT)
            DIST2 = DIFF1*DIFF1 + DIFF2*DIFF2 + DIFF3+DIFF3
C within limits?
            IF (DIST2.LT.DELIM2) THEN
	      SCOTDO(SCOUNT) = .FALSE.
	      NELIM = NELIM + 1
	    ENDIF
703       CONTINUE
          IF (SHORTO.LT.2) WRITE( NOUT, '(A,I5,A)')
     & ' Have eliminated ', NELIM, 
     &    ' of these points as they are within the end found',
     & ' leaves ', SCONUM-NELIM, ' points '
	ENDIF

C 2nd cycle of elimination - points that lie well within original HOLE point
C that is with 1.5 probe radii
        IF (SCORAD(1).GT.2.5D0*CONNR(1)) THEN
	  NELIM = 0
          DELIM2 = SCORAD(1)-1.5D0*CONNR(1)
	  DELIM2 = DELIM2*DELIM2
          
C go thru points - but not the first
          DO 704 SCOUNT = 2, SCONUM
	    IF (.NOT.SCOTDO(SCOUNT)) GOTO 704
C find distance**2 between the point and original point
            DIFF1 = SCOXYZ(1,1)-SCOXYZ(1,SCOUNT)
            DIFF2 = SCOXYZ(2,1)-SCOXYZ(2,SCOUNT)
            DIFF3 = SCOXYZ(3,1)-SCOXYZ(3,SCOUNT)
            DIST2 = DIFF1*DIFF1 + DIFF2*DIFF2 + DIFF3+DIFF3
C within limits?
            IF (DIST2.LT.DELIM2) THEN
	      SCOTDO(SCOUNT) = .FALSE.
	      NELIM = NELIM + 1
	    ENDIF
704       CONTINUE
          IF (SHORTO.LT.2) WRITE( NOUT, '(A,I5,A)')
     & ' Have eliminated ', NELIM, 
     &    ' points as they are well within original HOLE centre',
     & ' leaves ', SCONUM-NELIM, ' points '          
	ENDIF

	
C now do the area calculation for this set
        CALL COAREA( REQUIV, NOUT, LERR, SHORTO, ENDRAD, 
     &		     CENTRE, PERPVE, PERPVN, 
     &               SCOMAX, SCONUM, SCOXYZ, SCORAD, SCOTDO,
     &               SCOENC, SCOCIRCRAD, CVECT, SAMPLE,
     &		     .FALSE., SCO_PEG_ACC, SCO_PEG_FLAG, -1)

C write out the records if we need to
        IF (FPDBSP.NE.'NONE') THEN
	  WRESNO = IREC ! first record is the centre supplied 
	                ! to routine write this as a normal record  
	  WREQUIV = REQUIV
	  IF (WREQUIV.GT.1000.) WREQUIV = 999.99  
	  DO 4100 SCOUNT = 1, SCONUM

C PEG_WRITEALL option do not edit out records
C set the record number to -999 for all records
             IF (       SCOTDO(SCOUNT) .OR. 
     &            (SCORAD(SCOUNT).GT.ENDRAD) .OR.
     &                    PEG_WRITEALL                ) THEN
	       WRITE( SPDBSP, '(A,I4,4X,3F8.3,2F6.2)')
     &   'ATOM      1  QSS SPH S', WRESNO,
     &       (SCOXYZ(XCOUNT,SCOUNT), XCOUNT= 1, 3),
     &       SCORAD(SCOUNT), WREQUIV   
               IF (SCORAD(SCOUNT).GT.ENDRAD) 
     &           WRITE( SPDBSP, '(A)') 'LAST-REC-END'
               WRESNO = -999 ! after first write mark as connolly type records
             ENDIF
4100      CONTINUE
	ENDIF

C end of if Q is the hole pore radius less than the probe radius? 
      ENDIF


C      STOP 'temp stop in HOLCAL'

55555 RETURN
CORIGD       DO 5010 SCOUNT = 1, SCONUM
CORIGD          WRITE( NOUT, '(A,3F8.3,A,F8.3,A/ 3L1/ 3L1/ 3L1/)')
CORIGD    & ' xyz=', (SCOXYZ(XCOUNT,SCOUNT), XCOUNT = 1,3),
CORIGD    &   '  rad=', SCORAD(SCOUNT),
CORIGD    &   '   flags=',
CORIGD    &   SCONEI( -1,  1, SCOUNT),
CORIGD    &   SCONEI(  0,  1, SCOUNT),
CORIGD    &   SCONEI( +1,  1, SCOUNT),
CORIGD    &   SCONEI( -1,  0, SCOUNT),
CORIGD    &   SCONEI(  0,  0, SCOUNT),
CORIGD    &   SCONEI( +1,  0, SCOUNT),
CORIGD    &   SCONEI( -1, -1, SCOUNT),
CORIGD    &   SCONEI(  0, -1, SCOUNT),
CORIGD    &   SCONEI( +1, -1, SCOUNT)
CORIGD5010 CONTINUE

CORIGC original procedure - involved halving gridsize but failed
CORIGC just in case leave in commented CORIG
CORIGC have finished original gridsize - halve it and then
CORIGC check the half points
CORIG        IF (GRIDS.GT.CONNR(1)/1.999) THEN
CORIG
CORIGC halve the grid size
CORIG          GRIDS = GRIDS/2D0
CORIG
CORIGD         WRITE( NOUT, '(A,F10.4)')
CORIGD    & 'D-line GRIDS reduced to = ', GRIDS
CORIG
CORIGC go thru the existing list
CORIG          SCONEW = SCONUM
CORIG          DO 89 SCOUNT = 1, SCONUM
CORIGC look at each direction of stored point
CORIGC increment south, zero, north
CORIG            DO 157 NCOUNT = -1, 1
CORIG              DO 257 ECOUNT = -1, 1
CORIGC if the flag SCONEI( ECOUNT, NCOUNT, SCOUNT) is .true. 
CORIGC then this point was rejected for previous grid size
CORIGC so check again 
CORIG                IF (SCONEI( ECOUNT, NCOUNT, SCOUNT)) THEN
CORIGC test point for this direction with new grid
CORIG                  TSTXYZ(1) = SCOXYZ(1,SCOUNT) 
CORIG                  TSTXYZ(2) = SCOXYZ(2,SCOUNT) 
CORIG                  TSTXYZ(3) = SCOXYZ(3,SCOUNT) 
CORIGC movement east by grids
CORIG                  TSTXYZ(1) = TSTXYZ(1) + REAL(ECOUNT)*GRIDS*PERPVE(1)
CORIG                  TSTXYZ(2) = TSTXYZ(2) + REAL(ECOUNT)*GRIDS*PERPVE(2)
CORIG                  TSTXYZ(3) = TSTXYZ(3) + REAL(ECOUNT)*GRIDS*PERPVE(3)
CORIGC movement north by grids
CORIG                  TSTXYZ(1) = TSTXYZ(1) + REAL(NCOUNT)*GRIDS*PERPVN(1)
CORIG                  TSTXYZ(2) = TSTXYZ(2) + REAL(NCOUNT)*GRIDS*PERPVN(2)
CORIG                  TSTXYZ(3) = TSTXYZ(3) + REAL(NCOUNT)*GRIDS*PERPVN(3)
CORIGC Q: have we already produced this point?
CORIGC look through existing list including new points
CORIG                  DO 677 JCOUNT = 1, SCONEW
CORIGC find distance**2 between the points
CORIG                    DIFF1 = TSTXYZ(1)-SCOXYZ(1,JCOUNT)
CORIG                    DIFF2 = TSTXYZ(2)-SCOXYZ(2,JCOUNT)
CORIG                    DIFF3 = TSTXYZ(3)-SCOXYZ(3,JCOUNT)
CORIG                    DIST2 = DIFF1*DIFF1 + DIFF2*DIFF2 + DIFF3+DIFF3
CORIG                    IF (DIST2.LT.GRIDS/1000D0) THEN
CORIGC this point already considered and accepted
CORIG                      SCONEI( ECOUNT, NCOUNT, SCOCHK) = .FALSE.
CORIGD                     WRITE( NOUT, '(A,3F8.3)')
CORIGD    & 'D-line 2nd loop Have already considered + accepted point= ', 
CORIGD    &       TSTXYZ(1), TSTXYZ(2),TSTXYZ(3)
CORIGC jump to next point
CORIG                     GOTO 257
CORIG                   ENDIF
CORIG677              CONTINUE
CORIG            
CORIGC find the radius of this point
CORIG                 CALL HOLEEN( TSTXYZ, NEWENG, IAT1, IAT2, IAT3, 
CORIG     &			      DAT2, DAT3,
CORIG     &                        ATMAX, ATNO, ATXYZ, ATVDW, CUTSIZE)
CORIGC change sign to give +ve pore radius
CORIG                 NEWENG = -NEWENG
CORIGC Q: is this an accepted point?
CORIG                 IF (NEWENG.GE.CONNR(1)) THEN
CORIGC YES this is an accepted point (and its unique)
CORIGC first store information for point that produced this
CORIG                   SCONEI( ECOUNT, NCOUNT, SCOUNT) = .FALSE.
CORIGC now add point onto end of list to be considered
CORIGC array check here
CORIG                   IF (SCONEW+1.GT.SCOMAX) THEN
CORIG                     WRITE( NOUT, '(A)')
CORIG     &' ERROR SCOMAX reached in s/r concal',
CORIG     &' this should not generally happen!',
CORIG     &' please report to Oliver Smart o.s.smart@bham.ac.uk',
CORIG     &' with input and output files'
CORIG                     LERR = .TRUE.
CORIG                     GOTO 55555
CORIG                  ENDIF
CORIGC store the extra point
CORIG                  SCONEW = SCONEW + 1
CORIG                  SCOXYZ(1,SCONEW) = TSTXYZ(1)
CORIG                  SCOXYZ(2,SCONEW) = TSTXYZ(2)
CORIG                  SCOXYZ(3,SCONEW) = TSTXYZ(3)
CORIG                  SCORAD(SCONEW) = NEWENG
CORIGD                 WRITE( NOUT, '(A,3F8.3,A,F8.3,A)')
CORIGD    & 'D-line 2nd loop ACCEPT point= ', TSTXYZ(1), TSTXYZ(2),TSTXYZ(3),
CORIGD    &               ' radius= ', NEWENG, ' Angs'
CORIG                  ELSE
CORIGC NO this point is rejected (Q: is this an accepted point?)
CORIGC first store information for point that produced this
CORIG                    SCONEI( ECOUNT, NCOUNT, SCOUNT) = .TRUE.
CORIGD                   WRITE( NOUT, '(A,3F8.3,A,F8.3,A)')
CORIGD    & 'D-line REJECT point= ', TSTXYZ(1), TSTXYZ(2),TSTXYZ(3),
CORIGD    &               ' radius= ', NEWENG, ' Angs'
CORIG                  ENDIF
CORIG                ENDIF
CORIG257           CONTINUE
CORIG157        CONTINUE
CORIG89        CONTINUE
CORIGC finished adding extra points for half grids
CORIG          SCONUM = SCONEW 
CORIG
CORIGC jump back to consider next point if necessary
CORIG          IF (SCOCHK.LT.SCONUM) THEN
CORIGD           WRITE( NOUT, '(A,I5,A,I5)')
CORIGD    & ' Jump back as SCOCHK= ', SCOCHK, ' SCONUM= ', SCONUM
CORIG            GOTO 222
CORIG          ENDIF
CORIG        ENDIF
      END
