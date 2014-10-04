      SUBROUTINE HODOTB( SHYDRA, DOTDEN, LDOT, LLINE, LCEN, LSPIKE,
     &                   STRMAX, STRNOP, STRNON, STRCEN, STRRAD,
     &                   STRLVC, STRLBO, STRSBO, STRBRD, CVECT,
     &                   ATMAX, ATNO, ATXYZ, ATVDW, NOUT, LERR)
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
C Date	Author		Modification
C 12/93	O.S. Smart	Original version
C
C This s/r contains code to output molecular graphics output
C (centre line and dot surface) of HOLE's result.  
C This routine does job for sphere-box's (s/r hodotu is the one for sphere).

C passed vbles ****************

C stream number for output hydra/quanta binary plot file
C return unchanged!
      INTEGER			SHYDRA

C a dot density for dot surface
C same definition as in hydra - value between 5 and 30
C default to 10
      INTEGER                   DOTDEN

C
C if are drawing graphical representation of results
C can turn off the dot surface by setting ldot false
C turn off lines joining each sphere edge to
C   the two closest atoms by setting ldot false
C can turn off line joining the sphere centres
      LOGICAL                   LDOT, LLINE, LCEN

C New option April 1993 - instead of drawing dots
C draw a line from dot to surface of atom.
C Turn on by setting lspike .true..
      LOGICAL                   LSPIKE

C store for sphere centres and radii
C as this routine only deals with spheres do not need spher-box
C store the initial point as no 0 and subseqently points in the +ve
C bit. Points below the initial point are stored in -ve bit.
C maximum number of entries
      INTEGER                   STRMAX
C the no. of entries, +Ve entries -Ve entries
      INTEGER                   STRNOP, STRNON
C the centres and radii
      DOUBLE PRECISION          STRCEN(3,-STRMAX:STRMAX),
     &                          STRRAD(-STRMAX:STRMAX)

C vbles to store the dimensions found in the spherebox
C options.
      DOUBLE PRECISION          STRLVC(3,-STRMAX:STRMAX),
     &                          STRLBO(-STRMAX:STRMAX),
     &                          STRSBO(-STRMAX:STRMAX),
     &                          STRBRD(-STRMAX:STRMAX)

C the channel vector
      DOUBLE PRECISION		CVECT(3)

C need atom numbers, co-ords & radius
C maximum no. of atoms
      INTEGER                   ATMAX
C number of atoms we have
      INTEGER                   ATNO
C co-ordinates
      DOUBLE PRECISION          ATXYZ( 3, ATMAX)
C vdw radii of each atom
      DOUBLE PRECISION          ATVDW(ATMAX)

C output stream number
      INTEGER			NOUT
C error indicator
      LOGICAL			LERR

C internal vbles **************

C program uses a sphere of isotropically distributed points
C on a sphere of unit radius. PTMAX is the array bound,
C PTNO is the number of dots (dependent on dotden)
      INTEGER                   PTMAX, PTNO
      PARAMETER(                PTMAX = 4000)
      DOUBLE PRECISION          PTXYZ( 3, PTMAX)

C for drawing move/draws/dots etc. in binary plot file
C n.b. need real rather than d.p's
      REAL                      RVEC4(0:3)

C loop counts
      INTEGER			ICOUNT, JCOUNT, CCOUNT

C centre of sphere box
      DOUBLE PRECISION                  CENTRE(3)

C unit vector giving directions of axes which define sphere box
C first axis is vector (axis(1,1),axis(2,1),axis(3,1)
C N.B. no check is made in this s/r that axes are unit
C and orthogonal - This must be made certain in calling routine.
      DOUBLE PRECISION                  AXIS(3,3)

C radius of sphere box, its additional dimensions along the three axis
      DOUBLE PRECISION                  BOXRAD, BOXDIM(3)

C cloest point to sphere box, another one, distance
      DOUBLE PRECISION			XCLOS(3), DCLOS(3), DIST 

C point on sphere
      DOUBLE PRECISION			SPOINT(3)

C maximum radius of sphere-box
      DOUBLE PRECISION			MAXRAD

C end of decs *****************
          
C line joining centres (just like uniform)
      IF (LCEN) THEN
C change to colour 2
C Change 26/10/93 do in colour 7 - quanta default bright green
        WRITE(SHYDRA) 1.0, 7.0, 0.0, 0.0
C move to first centre
	RVEC4(0) = 2.0
	RVEC4(1) = STRCEN(1,-STRNON)
	RVEC4(2) = STRCEN(2,-STRNON)
	RVEC4(3) = STRCEN(3,-STRNON)
	WRITE(SHYDRA) RVEC4
C draw between the next
	DO 50 ICOUNT = -STRNON+1, STRNOP
	  RVEC4(0) = 3.0
	  RVEC4(1) = STRCEN(1,ICOUNT)
	  RVEC4(2) = STRCEN(2,ICOUNT)
	  RVEC4(3) = STRCEN(3,ICOUNT)
	  WRITE(SHYDRA) RVEC4
50	CONTINUE
      ENDIF

      IF (LDOT) THEN
C load up the set of points =lly distributed in space
C density determined by dotden
        CALL PTGEN( DOTDEN, PTMAX, PTNO, PTXYZ, NOUT, LERR)
        IF (LERR) THEN
          WRITE( NOUT,'(A)') 
     &' Have found error in s/r PTGEN',
     &'   Presumably dotden set too high - will try to recover',
     &'   By setting it to 10'
          LERR = .FALSE.
          DOTDEN = 10
          CALL PTGEN( DOTDEN, PTMAX, PTNO, PTXYZ, NOUT, LERR)
          IF (LERR) THEN
            WRITE( NOUT, '(A)') 
     &' HAS NOT WORKED! ABORTING'
            GOTO 55555
          ENDIF
        ENDIF

C change to colour 3
C Change 26/10/93 do in colour 8 - quanta default purple
        WRITE(SHYDRA) 1.0, 8.0, 0.0, 0.0
C do not draw dots for the end points
C to avoid bone shaped object
	DO 60 ICOUNT = -STRNON+1, STRNOP-1
C do not plot if radius -ve
	  IF (STRRAD(ICOUNT).GT.0.) THEN
C go through all ptno equally distributed points
	    DO 30 JCOUNT = 1, PTNO
C load up axes for sphere-box (do for each dot
C as  use same vbles to check for overlap).
C axis #1 is cvect
              AXIS(1,1) = CVECT(1)
              AXIS(2,1) = CVECT(2)
              AXIS(3,1) = CVECT(3)
C axis#2 is the long axis
              AXIS(1,2) = STRLVC(1,ICOUNT)
              AXIS(2,2) = STRLVC(2,ICOUNT)
              AXIS(3,2) = STRLVC(3,ICOUNT)
C third axis is the cross product of the other two
              CALL DCROSS( AXIS(1,1), AXIS(1,2), AXIS(1,3))
C just check that this has length 1 - if not then stop
              IF ( ABS(1.-(AXIS(1,3)**2+AXIS(2,3)**2+AXIS(3,3)**2))
     &     .GT. 1E-06) THEN
                WRITE( NOUT, *) 'ERROR in s/r hodotb'
                WRITE( NOUT, *) 'CVECT and LVECT not orthogonal!'
                WRITE( NOUT,*) ' CVECT  = ', CVECT
                WRITE( NOUT,*) ' STRLVC  = ', STRLVC(1,ICOUNT),
     &                       STRLVC(2,ICOUNT), STRLVC(3,ICOUNT)
                LERR = .TRUE.
                GOTO 55555
              ENDIF
C load up box lengths - length along cvect (axis#1) is the long length
              BOXDIM(1) = ABS(STRLBO(ICOUNT))
              BOXDIM(2) = ABS(STRLBO(ICOUNT))
              BOXDIM(3) = ABS(STRSBO(ICOUNT))
              BOXRAD    = ABS(STRBRD(ICOUNT))
C centre of sphere-box
              CENTRE(1) =  STRCEN(1,ICOUNT)
              CENTRE(2) =  STRCEN(2,ICOUNT)
              CENTRE(3) =  STRCEN(3,ICOUNT)

C work out 'maximum' radius of sphere box
C which has box-length strsbo & strlbo twice
              MAXRAD = SQRT( 2.*STRLBO(ICOUNT)**2 + 
     &                          STRSBO(ICOUNT)**2   ) +
     &                 STRBRD(ICOUNT)

C We are doing sphere-boxes so consider point closest to
C a point on sphere radius maxrad
	      SPOINT(1) = STRCEN(1,ICOUNT) + MAXRAD* PTXYZ(1,JCOUNT)
	      SPOINT(2) = STRCEN(2,ICOUNT) + MAXRAD* PTXYZ(2,JCOUNT)
	      SPOINT(3) = STRCEN(3,ICOUNT) + MAXRAD* PTXYZ(3,JCOUNT)

C find closest point on sphere-box (XCLOS)
              CALL SBDCLO( DIST, XCLOS, SPOINT, 
     &                    AXIS, BOXRAD, BOXDIM, CENTRE)

C o.k. have prospective dot XCLOS - shall we plot it?
C only plot if point is not within neighbouring sphere-box
C have to replace old simple distance calculation with
C calls to sbdclo
C preceeding 20 centres
              DO 35 CCOUNT = ICOUNT-1, MAX(ICOUNT-20,-STRNON), -1
C load up axes for this sphere box
C axis #1 is cvect
                AXIS(1,1) = CVECT(1)
                AXIS(2,1) = CVECT(2)
                AXIS(3,1) = CVECT(3)
C axis#2 is the long axis
                AXIS(1,2) = STRLVC(1,CCOUNT)
                AXIS(2,2) = STRLVC(2,CCOUNT)
                AXIS(3,2) = STRLVC(3,CCOUNT)
C third axis is the cross product of the other two
                CALL DCROSS( AXIS(1,1), AXIS(1,2), AXIS(1,3))
C just check that this has length 1 - if not then stop
                IF ( ABS(1.-(AXIS(1,3)**2+AXIS(2,3)**2+AXIS(3,3)**2))
     &     .GT. 1E-06) THEN
                  WRITE( NOUT, *) 'ERROR in s/r hodotb'
                  WRITE( NOUT, *) 'CVECT and LVECT not orthogonal! (2)'
                  WRITE( NOUT,*) ' CVECT  = ', CVECT
                  WRITE( NOUT,*) ' STRLVC  = ', STRLVC(1,CCOUNT),
     &                       STRLVC(2,CCOUNT), STRLVC(3,CCOUNT)
                  LERR = .TRUE.
                  GOTO 55555
                ENDIF
C load up box lengths - length along cvect (axis#1) is the long length
                BOXDIM(1) = ABS(STRLBO(CCOUNT))
                BOXDIM(2) = ABS(STRLBO(CCOUNT))
                BOXDIM(3) = ABS(STRSBO(CCOUNT))
                BOXRAD =    ABS(STRBRD(CCOUNT))
C centre of sphere-box
                CENTRE(1) =  STRCEN(1,CCOUNT)
                CENTRE(2) =  STRCEN(2,CCOUNT)
                CENTRE(3) =  STRCEN(3,CCOUNT)
C use s/r sbdclo to find the distance from our prospective dot
C to the surface of sphere-box# ccount (dclos is the
C closest point - not used)
                CALL SBDCLO( DIST, DCLOS, XCLOS, 
     &                      AXIS, BOXRAD, BOXDIM, CENTRE)

C if the distance is less than zero then should not plot
                IF (DIST.LT.0.) GOTO 30
35            CONTINUE

C following 20 centres
              DO 36 CCOUNT = ICOUNT+1, MIN(ICOUNT+20,STRNOP)
C load up axes for this sphere box
C axis #1 is cvect
                AXIS(1,1) = CVECT(1)
                AXIS(2,1) = CVECT(2)
                AXIS(3,1) = CVECT(3)
C axis#2 is the long axis
                AXIS(1,2) = STRLVC(1,CCOUNT)
                AXIS(2,2) = STRLVC(2,CCOUNT)
                AXIS(3,2) = STRLVC(3,CCOUNT)
C third axis is the cross product of the other two
                CALL DCROSS( AXIS(1,1), AXIS(1,2), AXIS(1,3))
C just check that this has length 1 - if not then stop
                IF ( ABS(1.-(AXIS(1,3)**2+AXIS(2,3)**2+AXIS(3,3)**2))
     &     .GT. 1E-06) THEN
                  WRITE( NOUT, *) 'ERROR in s/r hodotb'
                  WRITE( NOUT, *) 'CVECT and LVECT not orthogonal! (3)'
                  WRITE( NOUT,*) ' CVECT  = ', CVECT
                  WRITE( NOUT,*) ' STRLVC  = ', STRLVC(1,CCOUNT),
     &                       STRLVC(2,CCOUNT), STRLVC(3,CCOUNT)
                  LERR = .TRUE.
                  GOTO 55555
                ENDIF
C load up box lengths - length along cvect (axis#1) is the long length
                BOXDIM(1) = ABS(STRLBO(CCOUNT))
                BOXDIM(2) = ABS(STRLBO(CCOUNT))
                BOXDIM(3) = ABS(STRSBO(CCOUNT))
                BOXRAD =    ABS(STRBRD(CCOUNT))
C centre of sphere-box
                CENTRE(1) =  STRCEN(1,CCOUNT)
                CENTRE(2) =  STRCEN(2,CCOUNT)
                CENTRE(3) =  STRCEN(3,CCOUNT)
C use s/r sbdclo to find the distance from our prospective dot
C to the surface of sphere-box# ccount (dclos is the
C closest point - not used)
                CALL SBDCLO( DIST, DCLOS, XCLOS, 
     &                      AXIS, BOXRAD, BOXDIM, CENTRE)

C if the distance is less than zero then should not plot
                IF (DIST.LT.0.) GOTO 30
36            CONTINUE

C write the dot - n.b. spike option not considered for
C sphere-box at present
C indicate dot by making 1st record 4.0
              RVEC4(0) = 4.0
              RVEC4(1) = XCLOS(1)
              RVEC4(2) = XCLOS(2)
              RVEC4(3) = XCLOS(3)
              WRITE(SHYDRA) RVEC4

30          CONTINUE
	  ENDIF
60	CONTINUE
      ENDIF

C sphere-box option draw the long and short axes for each sphere-box
      WRITE(SHYDRA) 1.0, 9.0, 0.0, 0.0
C go thru centre's
      DO 77 ICOUNT = -STRNON, STRNOP
C draw line for long axis
          RVEC4(0) = 2.0
          RVEC4(1) = STRCEN(1,ICOUNT) - STRLBO(ICOUNT)*STRLVC(1,ICOUNT)
          RVEC4(2) = STRCEN(2,ICOUNT) - STRLBO(ICOUNT)*STRLVC(2,ICOUNT)
          RVEC4(3) = STRCEN(3,ICOUNT) - STRLBO(ICOUNT)*STRLVC(3,ICOUNT)
          WRITE(SHYDRA) RVEC4
          RVEC4(0) = 3.0
          RVEC4(1) = STRCEN(1,ICOUNT) + STRLBO(ICOUNT)*STRLVC(1,ICOUNT)
          RVEC4(2) = STRCEN(2,ICOUNT) + STRLBO(ICOUNT)*STRLVC(2,ICOUNT)
          RVEC4(3) = STRCEN(3,ICOUNT) + STRLBO(ICOUNT)*STRLVC(3,ICOUNT)
          WRITE(SHYDRA) RVEC4
C work out short box length axis
C axis #1 is cvect
          AXIS(1,1) = CVECT(1)
          AXIS(2,1) = CVECT(2)
          AXIS(3,1) = CVECT(3)
C axis#2 is the long axis
          AXIS(1,2) = STRLVC(1,ICOUNT)
          AXIS(2,2) = STRLVC(2,ICOUNT)
          AXIS(3,2) = STRLVC(3,ICOUNT)
C third axis is the cross product of the other two
          CALL DCROSS( AXIS(1,1), AXIS(1,2), AXIS(1,3))

C draw line for this
          RVEC4(0) = 2.0
          RVEC4(1) = STRCEN(1,ICOUNT) - STRSBO(ICOUNT)*AXIS(1,3)
          RVEC4(2) = STRCEN(2,ICOUNT) - STRSBO(ICOUNT)*AXIS(2,3)
          RVEC4(3) = STRCEN(3,ICOUNT) - STRSBO(ICOUNT)*AXIS(3,3)
          WRITE(SHYDRA) RVEC4
          RVEC4(0) = 3.0
          RVEC4(1) = STRCEN(1,ICOUNT) + STRSBO(ICOUNT)*AXIS(1,3)
          RVEC4(2) = STRCEN(2,ICOUNT) + STRSBO(ICOUNT)*AXIS(2,3)
          RVEC4(3) = STRCEN(3,ICOUNT) + STRSBO(ICOUNT)*AXIS(3,3)
          WRITE(SHYDRA) RVEC4
77      CONTINUE

55555 RETURN
      END
