      SUBROUTINE HODOTC( SHYDRA, DOTDEN, LDOT, LLINE, LCEN, LSPIKE,
     &                   STRMAX, STRNOP, STRNON, STRCEN, 
     &                   STRLVC, STRBRD,
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
C * (c) 1995 Oliver Smart & Birkbeck College, All rights reserved    *
C * (c) 1996 Oliver Smart & Birkbeck College, All rights reserved    *
C *                                                                  *
C ********************************************************************
C
C Modification history:
C
C Date	Author		Modification
C 08/95	O.S. Smart	Original version
C
C This s/r contains code to output molecular graphics output
C (centre line and dot surface) of HOLE's result.
C Version for capsule option


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
      DOUBLE PRECISION          STRCEN(3,-STRMAX:STRMAX)

C vbles need to the capsule bit  -
C strlvc is the second centre for capsule and strbrd is the
C capsule radius
C n.b. do not need strrad as this is a virtual radius
      DOUBLE PRECISION          STRLVC(3,-STRMAX:STRMAX),
     &                          STRBRD(-STRMAX:STRMAX)

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

C capsule centre
      DOUBLE PRECISION		CCENT(3)

C effective radius for a dot, xyz coord for dot, 
C distance from caps centre to dot
      DOUBLE PRECISION		EFFRAD, DOTXYZ(3), DSURF2

C end of decs *****************
          
C line joining centres
      IF (LCEN) THEN
C change to colour 2
C Change 26/10/93 do in colour 7 - quanta default bright green
C change 18/10/95 centre line number 4 (yellow) alt 15
        WRITE(SHYDRA) 1.0, 4.0, -55.0, 15.0
C move to first centre - for capsules this is mid-point between two capsule
C centres
	RVEC4(0) = 2.0
	RVEC4(1) = 0.5*(STRCEN(1,-STRNON)+STRLVC(1,-STRNON))
	RVEC4(2) = 0.5*(STRCEN(2,-STRNON)+STRLVC(2,-STRNON))
	RVEC4(3) = 0.5*(STRCEN(3,-STRNON)+STRLVC(3,-STRNON))
	WRITE(SHYDRA) RVEC4
C draw between the next
	DO 50 ICOUNT = -STRNON+1, STRNOP
	  RVEC4(0) = 3.0
	  RVEC4(1) = 0.5*(STRCEN(1,ICOUNT)+STRLVC(1,ICOUNT))
	  RVEC4(2) = 0.5*(STRCEN(2,ICOUNT)+STRLVC(2,ICOUNT))
	  RVEC4(3) = 0.5*(STRCEN(3,ICOUNT)+STRLVC(3,ICOUNT))
	  WRITE(SHYDRA) RVEC4
50	CONTINUE
      ENDIF

C do we want a dot surface?
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
C change 18/10/95 dots number 7 (bright green) alt 17
        WRITE(SHYDRA) 1.0, 7.0, -55.0, 17.0

C do not draw dots for the end points
C to avoid bone shaped object
	DO 60 ICOUNT = -STRNON+1, STRNOP-1
C centre of this capsule
	  CCENT(1) = 0.5*(STRCEN(1,ICOUNT)+STRLVC(1,ICOUNT))
	  CCENT(2) = 0.5*(STRCEN(2,ICOUNT)+STRLVC(2,ICOUNT))
	  CCENT(3) = 0.5*(STRCEN(3,ICOUNT)+STRLVC(3,ICOUNT))

C do not plot if radius -ve
	  IF (STRBRD(ICOUNT).GT.0.) THEN
C go through all ptno equally distributed points
	    DO 30 JCOUNT = 1, PTNO
C want a dot in direction ptxyz(*,jcount) from ccent
C what value should we place on its radius?
C do with s/r hcapr - result returned as effrad
              CALL HOCAPR( 
     &         STRCEN(1,ICOUNT), STRLVC(1,ICOUNT), STRBRD(ICOUNT),
     &         PTXYZ(1,JCOUNT), EFFRAD)
C coordinates for this dot
               DOTXYZ(1) = CCENT(1) + EFFRAD*PTXYZ(1,JCOUNT)
               DOTXYZ(2) = CCENT(2) + EFFRAD*PTXYZ(2,JCOUNT)
               DOTXYZ(3) = CCENT(3) + EFFRAD*PTXYZ(3,JCOUNT)
C now ask the question is this dot within any other capsule?
C like the standard sphere option only look thru surrounding 20 centres
C preceeding 20 centres (if there are 20)
               DO 35 CCOUNT = ICOUNT-1, MAX(ICOUNT-20,-STRNON), -1
C is DOTXYZ with this capsule?
C once again seperate into different s/r - 
C dsurf2 is returned as the square of the  shortest distance 
C from the capsule centre line to the point dotxyz.
                 CALL HOCAPD(
     &             STRCEN(1,CCOUNT), STRLVC(1,CCOUNT), 
     &             DOTXYZ, DSURF2, 0D0, 0D0)
C is dsurf2 less than the capsule radius squared?
C if so do not draw but jump to considering next dot
                   IF (DSURF2.LT.STRBRD(CCOUNT)*STRBRD(CCOUNT)) GOTO 30 
35             CONTINUE
C following 20 centres
               DO 36 CCOUNT = ICOUNT+1, MIN(ICOUNT+20,STRNOP)
                 CALL HOCAPD(
     &             STRCEN(1,CCOUNT), STRLVC(1,CCOUNT),
     &             DOTXYZ, DSURF2, 0D0, 0D0)
C is dsurf2 less than the capsule radius squared?
C if so do not draw but jump to considering next dot
                   IF (DSURF2.LT.STRBRD(CCOUNT)*STRBRD(CCOUNT)) GOTO 30
36             CONTINUE

C dotxyz has passed all tests write it
               WRITE(SHYDRA) 4., REAL(DOTXYZ(1)), 
     &                       REAL(DOTXYZ(2)), REAL(DOTXYZ(3))



C end of considering whether to draw dot jcount
30          CONTINUE
          ENDIF
C end of doing points for capsule #icount
60	CONTINUE
      ENDIF

C change 18/10/95 capsule vector number 8 (purple) alt 19
      WRITE(SHYDRA) 1.0, 8.0, -55.0, 19.0

C go thru all records
      DO 90 ICOUNT = -STRNON, STRNOP
C move to first centre
        RVEC4(0) = 2.
        RVEC4(1) = STRCEN(1,ICOUNT)
        RVEC4(2) = STRCEN(2,ICOUNT)
        RVEC4(3) = STRCEN(3,ICOUNT)
        WRITE(SHYDRA) RVEC4
C draw to second capsule centre
        RVEC4(0) = 3.
        RVEC4(1) = STRLVC(1,ICOUNT)
        RVEC4(2) = STRLVC(2,ICOUNT)
        RVEC4(3) = STRLVC(3,ICOUNT)
        WRITE(SHYDRA) RVEC4
90    CONTINUE

55555 RETURN
      END
