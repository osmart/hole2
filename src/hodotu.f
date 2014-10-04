      SUBROUTINE HODOTU( SHYDRA, DOTDEN, LDOT, LLINE, LCEN, LSPIKE,
     &                   STRMAX, STRNOP, STRNON, STRCEN, STRRAD,
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
C Upto 07/94 included in the main
C program but split of on introducing sphere-box option.


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

C need some dummy variables for s/r HOLEEN call
      INTEGER                   IAT1, IAT2, IAT3
      DOUBLE PRECISION          DAT2, DAT3
C also a test position and energy (to do spike option)
      DOUBLE PRECISION          NEWCEN(3), NEWENG

C working real
      DOUBLE PRECISION		DUM

C loop counts
      INTEGER			ICOUNT, JCOUNT, CCOUNT

C end of decs *****************
          
C line joining centres
	  IF (LCEN) THEN
C change to colour 2
C Change 26/10/93 do in colour 7 - quanta default bright green
C change 18/10/95 centre line number 4 (yellow) alt 15
            WRITE(SHYDRA) 1.0, 4.0, -55.0, 15.0
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
50	    CONTINUE
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
C change 18/10/95 dots number 7 (bright green) alt 17
            WRITE(SHYDRA) 1.0, 7.0, -55.0, 17.0
C do not draw dots for the end points
C to avoid bone shaped object
	    DO 60 ICOUNT = -STRNON+1, STRNOP-1
C do not plot if radius -ve
	      IF (STRRAD(ICOUNT).GT.0.) THEN

C go through all ptno equally distributed points
		DO 30 JCOUNT = 1, PTNO

C indicate dot by making 1st record 4.0
		  RVEC4(0) = 4.0
		  RVEC4(1) = PTXYZ(1,JCOUNT)*STRRAD(ICOUNT) +
     &			     STRCEN(1,ICOUNT)
		  RVEC4(2) = PTXYZ(2,JCOUNT)*STRRAD(ICOUNT) +
     &			     STRCEN(2,ICOUNT)
		  RVEC4(3) = PTXYZ(3,JCOUNT)*STRRAD(ICOUNT) +
     &			     STRCEN(3,ICOUNT)
C only plot if point is not within neighbouring sphere
C preceeding 20 centres
                  DO 35 CCOUNT = ICOUNT-1, MAX(ICOUNT-20,-STRNON), -1
		    DUM = (RVEC4(1)-STRCEN(1,CCOUNT))**2 +
     &			  (RVEC4(2)-STRCEN(2,CCOUNT))**2 +
     &			  (RVEC4(3)-STRCEN(3,CCOUNT))**2
C shorter? (compare squares rather than square root as quicker)
                    IF (DUM.LT.STRRAD(CCOUNT)**2) GOTO 30
35                CONTINUE

C following 20 centres
                  DO 36 CCOUNT = ICOUNT+1, MIN(ICOUNT+20,STRNOP)
		    DUM = (RVEC4(1)-STRCEN(1,CCOUNT))**2 +
     &			  (RVEC4(2)-STRCEN(2,CCOUNT))**2 +
     &			  (RVEC4(3)-STRCEN(3,CCOUNT))**2
C shorter? (compare squares rather than square root as quicker)
	  	    IF (DUM.LT.STRRAD(CCOUNT)**2) GOTO 30
36                CONTINUE

C write the dot
                  IF (.NOT.LSPIKE) THEN
		    WRITE(SHYDRA) RVEC4
                  ELSE
C new feature April 1993 instead of drawing dot lets draw
C a line from sphere surface out to atom surface

C first estimate of overlap is the dot itself
                    DUM = STRRAD(ICOUNT)
C do ten cycles of refinement to find sphere centre
C SEE f048 FOR METHOD
                    DO 55 CCOUNT = 1, 10
C load up current point dum is the distance along line 
C from the sphere which is current estimate of overlap point.
                      NEWCEN(1) = PTXYZ(1,JCOUNT)*DUM +
     &                            STRCEN(1,ICOUNT)
                      NEWCEN(2) = PTXYZ(2,JCOUNT)*DUM +
     &                            STRCEN(2,ICOUNT)
                      NEWCEN(3) = PTXYZ(3,JCOUNT)*DUM +
     &                            STRCEN(3,ICOUNT)

C find energy of point newcen
C - energy is minus radius the largest sphere
C which can be centered at this point without vdW overlap
C hard code cutsize to reasonably large
          CALL HOLEEN( NEWCEN, NEWENG, IAT1, IAT2, IAT3, DAT2, DAT3,
     &                            ATMAX, ATNO, ATXYZ, ATVDW, 3D0)
C add on the radius to the estimate of overlap point
                      DUM = DUM - NEWENG
C put a limit of six angstroms on the line length
                      IF (DUM.GT.6.0) DUM = 6.0
55                 CONTINUE
C debug tell user
C                   write( nout, *) 
C     &' debug final sphere rad', strrad(icount),' edge ', dum, 
C     &' largest sp ', -neweng

C draw line from RVEC to NEWCEN - the dot to the overlap point
C indicate moveto by making 1st record 2.0, drawto 3.0
		    RVEC4(0) = 2.0
		    WRITE(SHYDRA) RVEC4
		    RVEC4(0) = 3.0
		    RVEC4(1) = NEWCEN(1)
		    RVEC4(2) = NEWCEN(2)
		    RVEC4(3) = NEWCEN(3)
		    WRITE(SHYDRA) RVEC4
                  ENDIF
30		CONTINUE
	      ENDIF
60	    CONTINUE
	  ENDIF

55555 RETURN
      END
