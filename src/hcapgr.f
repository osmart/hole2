      SUBROUTINE HCAPGR( NOUT, SAMPLE, PI,
     &                 STRMAX, STRNOP, STRNON, STRCEN, STRRAD,
     &                 STRLVC, STRBRD,
     &                 ATMAX, ATNO, ATBRK, ATRES,
     &                 ATCHN, ATRNO, ATXYZ, ATVDW, ATBND,
     &                 CVECT, SHORTO,
     &		       POSNO)
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
C 05/95 O.S. Smart      First version
C 11/95 O.S.S.          conductance calc and SHORTO implemented
C 06/96 O.S.S.		rationalization and vital stats - conductance
C			stuff altered to exclude mid-points (j134)
C 09/96 O.S.S.		changed definition of capsule length 1/2 previous
C 11/96 O.S.S.          Call to helefi to work out electostatic potential
C 
C This s/r contains code to output data to allow user to plot
C graphs of HOLE's result.  Capsule option.

C passed variables *************

C output stream no. (usually set to 6) RETURN UNCHANGED
      INTEGER			NOUT

C The sampling distance between planes used in hole,
C needed to work out a running volume
C (n.b. normally supplied -ve)
      DOUBLE PRECISION		SAMPLE

C will need PI (worked out in HOLE)
      DOUBLE PRECISION		PI

C store for sphere centres and radii
C store the initial point as no 0 and subseqently points in the +ve
C bit. Points below the initial point are stored in -ve bit.
C maximum number of entries
      INTEGER                   STRMAX
C the no. of entries, +Ve entries -Ve entries
      INTEGER                   STRNOP, STRNON
C the centres and radii
      DOUBLE PRECISION          STRCEN(3,-STRMAX:STRMAX),
     &                          STRRAD(-STRMAX:STRMAX)
C 08/07/94
C vbles to store the dimensions found in the spherebox
C options. Strlvc is here used to store the second centre,
C and strbrd the capsule radius.
      DOUBLE PRECISION          STRLVC(3,-STRMAX:STRMAX),
     &                          STRBRD(-STRMAX:STRMAX)

C maximum number and actual number of atoms read in from each file:
      INTEGER                   ATMAX, ATNO

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

C channel vector
      DOUBLE PRECISION		CVECT(3)

C 11/95 introduce new variable to reduce text output -
C if oshort =
C 0: full text output
C 1: no in run progress reported
C 2: ditto plus no graph type output - only leaving
C    Minimum radius & conductance calculations.
C 3: All text output other than input card mirroring
C    and error messages turned off
      INTEGER                   SHORTO

C position number to be written with info in tagged format
      INTEGER			POSNO

C internal vbles ***************

C loop count for going through stored records
      INTEGER			SCOUNT, ACOUNT, CCOUNT

C 05/94 add inverse area integration for Mark Sansom -
C just work out running total of sample/(pi*strrad^2)
      DOUBLE PRECISION          INAREA, TOAREA

C the minimum radius found, the value if only sampled not midpoints counted
      DOUBLE PRECISION          MINRAD, MINSAM

C need some dummy variables for s/r HOLEEN call
      INTEGER			IAT1, IAT2
      DOUBLE PRECISION		DAT2
C also a test centres and energy, capsule radius
      DOUBLE PRECISION		TSTCEN(3), TSTCE2(3), TSTENG, TSTBRD

C direction vector to test the capsule vector against
      DOUBLE PRECISION		TSTVEC(3)

C capsule centre, capsule centreline vector, the other version, distance between end
C angle, previous centre, distance along channel
      DOUBLE PRECISION		CAPCEN(3), CAPVEC(3), ALTVEC(3), CAPLEN, 
     &				CAPANG, PRECEN(3), DALONG
C coordinate of centre along cvect
      DOUBLE PRECISION		CAPCOR

C string to write numbers to two significant figures,
C the last character of this
      CHARACTER*8               ST2SF
      INTEGER                   LST2SF

C empirical devisor (j059)
      DOUBLE PRECISION		DIV1, DIV2, DIV3, DIV4

C for multiple calls work out mean and s.d. of
C minrad, gmax and gred
C no of calls
      INTEGER			NOCALL
      DOUBLE PRECISION		TOTR, TOTR2,
     &				TOTGM, TOTGM2,
     &				TOTGP, TOTGP2

C upper and lower coords of any atom along cvect
      DOUBLE PRECISION		UCOORD, LCOORD, COORD
C end radii
      DOUBLE PRECISION		UENDR, LENDR

C two lengths - atomic and defined by HOLE ends
      DOUBLE PRECISION		LENAT, LENHO

C average electrostatic potential as calculated by HELEFI
      DOUBLE PRECISION		AVGPOT

C macroscopic and predicted conductance
      DOUBLE PRECISION		GMACRO, GPRED
C for 2nd generation need 4 vbles!
      DOUBLE PRECISION		GPRED1, GPRED2, GPRED3, GPRED4


C sg's do not zero vbles !
      DATA NOCALL, TOTR, TOTR2, TOTGM, TOTGM2,
     &     TOTGP, TOTGP2 /0, 0D0, 0D0, 0D0, 0D0, 0D0, 0D0/

C end of decs ******************

C initialize minimum radius 
      MINRAD = 99999.
      MINSAM = 99999.

C n.b. sample must be +ve
      SAMPLE = ABS(SAMPLE)
C geometric factor to start at zero
      INAREA = 0.

C want to find rotation angle of capsule vector
C default to test against x-axis 
c BUT if the channel vector is along x then set to y-axis
      IF ( (ABS(CVECT(2)).LT.1E-06) .AND. 
     &     (ABS(CVECT(3)).LT.1E-06)       ) THEN
        IF (SHORTO.LT.2) WRITE( NOUT, '(A)')
     &' channel vector along x-axis angle quoted will be to y-axis'
        TSTVEC(1) = 0.
        TSTVEC(2) = 1.
        TSTVEC(3) = 0.
      ELSE
        TSTVEC(1) = 1.
        TSTVEC(2) = 0.
        TSTVEC(3) = 0.
      ENDIF
      
C got to the end
C want to provide graph radius found vs
C			distance from the first determined point
      IF (SHORTO.LT.2) WRITE( NOUT,'(A)') 
     &' ',
     &' Capsule graph option*****',
     &' Data:'
      IF (SHORTO.LT.2) WRITE( NOUT,'(10A12)') 
     &' cenxyz.cvect',
     &' eff.rad',
     &' area',
     &' CAP.LEN',
     &' cap.rad',
     &' angle.X-axis', 
     &' cen.line.dis',
     &' integ.s/(area)',
     &' point source'

C -ve first
      DALONG = 0.0
      DO 77 SCOUNT= 0, STRNON
C the centre point of capsule is between the two centres which define it
        CAPCEN(1) = 0.5*(STRCEN(1,-SCOUNT)+STRLVC(1,-SCOUNT))
        CAPCEN(2) = 0.5*(STRCEN(2,-SCOUNT)+STRLVC(2,-SCOUNT))
        CAPCEN(3) = 0.5*(STRCEN(3,-SCOUNT)+STRLVC(3,-SCOUNT))
C coordinate of this along cvect
        CALL DDOT( CAPCEN, CVECT, CAPCOR)
C the capsule vector is the difference between two point
        CAPVEC(1) = STRCEN(1,-SCOUNT) - STRLVC(1,-SCOUNT)
        CAPVEC(2) = STRCEN(2,-SCOUNT) - STRLVC(2,-SCOUNT)
        CAPVEC(3) = STRCEN(3,-SCOUNT) - STRLVC(3,-SCOUNT)
C the length of this vector
        CAPLEN = SQRT( CAPVEC(1)**2 + CAPVEC(2)**2 + CAPVEC(3)**2)
C find angle to test axis
C first unit capvec
        CALL DUVEC2( CAPVEC)
C then work out dot product with test vector
        CALL DDOT( CAPVEC, TSTVEC, CAPANG)
C arccos the result & convert to degrees
        CAPANG = (180./PI)*ACOS(CAPANG)
C running total for geometric factor 
C j134 28/5/96 - take out factor of 1/2
C as do not include mid-points in calc anymore
        INAREA = INAREA + SAMPLE/(PI*STRRAD(-SCOUNT)**2)
       
C distance increment, -ve as going thru negative records
        IF (SCOUNT.NE.0) 
     &     DALONG = DALONG -
     &                SQRT( (CAPCEN(1)-PRECEN(1))**2 + 
     &                      (CAPCEN(2)-PRECEN(2))**2 + 
     &			    (CAPCEN(3)-PRECEN(3))**2   )
C write out the point - centre, area, effective radius, capsule length, capsule radius,
C angle to test axis,
C NOTE CAPSULE LENGTH IS 1/2 THE DISTANCE BETWEEN CENTRES TO
C AGREE WITH BIOPHYS J. DESCRIPTION
	IF (SHORTO.LT.2) WRITE( NOUT,'(8F12.5,A12)') 
     &    CAPCOR,   STRRAD(-SCOUNT), PI*STRRAD(-SCOUNT)**2, 0.5*CAPLEN, 
     &    STRBRD(-SCOUNT), CAPANG, DALONG, INAREA, ' (sampled) '

C minimum effective radius?
        IF (STRRAD(-SCOUNT).LT.MINRAD) MINRAD = STRRAD(-SCOUNT)
        IF (STRRAD(-SCOUNT).LT.MINSAM) MINSAM = STRRAD(-SCOUNT)

C store centre for previous distance calc.
        PRECEN(1) = CAPCEN(1)
        PRECEN(2) = CAPCEN(2)
        PRECEN(3) = CAPCEN(3)

C just like in normal sphere output check the mid-point of each output interval
        IF (SCOUNT.LT.STRNON) THEN
C centre point is the average of the centre points
          CAPCEN(1) = 0.25*(STRCEN(1,-SCOUNT)   + STRLVC(1,-SCOUNT))  +
     &		      0.25*(STRCEN(1,-SCOUNT-1) + STRLVC(1,-SCOUNT-1))
          CAPCEN(2) = 0.25*(STRCEN(2,-SCOUNT)   + STRLVC(2,-SCOUNT))  +
     &		      0.25*(STRCEN(2,-SCOUNT-1) + STRLVC(2,-SCOUNT-1))
          CAPCEN(3) = 0.25*(STRCEN(3,-SCOUNT)   + STRLVC(3,-SCOUNT))  +
     &		      0.25*(STRCEN(3,-SCOUNT-1) + STRLVC(3,-SCOUNT-1))
C work out the capsule vector
C now there is a slight complication 
C How do we mix?
C work out both - take longer vector
          CAPVEC(1) = 0.5*(STRCEN(1,-SCOUNT)   - STRLVC(1,-SCOUNT)) +
     &                0.5*(STRCEN(1,-SCOUNT-1) - STRLVC(1,-SCOUNT-1)) 
          CAPVEC(2) = 0.5*(STRCEN(2,-SCOUNT)   - STRLVC(2,-SCOUNT)) +
     &                0.5*(STRCEN(2,-SCOUNT-1) - STRLVC(2,-SCOUNT-1)) 
          CAPVEC(3) = 0.5*(STRCEN(3,-SCOUNT)   - STRLVC(3,-SCOUNT)) +
     &                0.5*(STRCEN(3,-SCOUNT-1) - STRLVC(3,-SCOUNT-1))
          ALTVEC(1) = 0.5*(STRCEN(1,-SCOUNT)   - STRLVC(1,-SCOUNT)) -
     &                0.5*(STRCEN(1,-SCOUNT-1) - STRLVC(1,-SCOUNT-1)) 
          ALTVEC(2) = 0.5*(STRCEN(2,-SCOUNT)   - STRLVC(2,-SCOUNT)) -
     &                0.5*(STRCEN(2,-SCOUNT-1) - STRLVC(2,-SCOUNT-1))
          ALTVEC(3) = 0.5*(STRCEN(3,-SCOUNT)   - STRLVC(3,-SCOUNT)) -
     &                0.5*(STRCEN(3,-SCOUNT-1) - STRLVC(3,-SCOUNT-1)) 
C is altvec longer?
          IF ( (ALTVEC(1)**2+ALTVEC(2)**2+ALTVEC(3)**2).GT.
     &         (CAPVEC(1)**2+CAPVEC(2)**2+CAPVEC(3)**2)    ) THEN
            CAPVEC(1) = ALTVEC(1)
            CAPVEC(2) = ALTVEC(2)
            CAPVEC(3) = ALTVEC(3)
          ENDIF
C test points
          TSTCEN(1) = CAPCEN(1) - 0.5*CAPVEC(1)
          TSTCEN(2) = CAPCEN(2) - 0.5*CAPVEC(2)
          TSTCEN(3) = CAPCEN(3) - 0.5*CAPVEC(3)
C the second
          TSTCE2(1) = CAPCEN(1) + 0.5*CAPVEC(1)
          TSTCE2(2) = CAPCEN(2) + 0.5*CAPVEC(2)
          TSTCE2(3) = CAPCEN(3) + 0.5*CAPVEC(3)

C find out radius etc. for these new points
          CALL HCAPEN( TSTCEN, TSTENG, TSTCE2, TSTBRD,
     &           IAT1, IAT2, DAT2,
     &           ATMAX, ATNO, ATXYZ, ATVDW, PI)

C the length of this vector
          CAPLEN = SQRT( CAPVEC(1)**2 + CAPVEC(2)**2 + CAPVEC(3)**2)
C find angle to test axis
C first unit capvec
          CALL DUVEC2( CAPVEC)
C then work out dot product with test vector
          CALL DDOT( CAPVEC, TSTVEC, CAPANG)
C arccos the result & convert to degrees
          CAPANG = (180./PI)*ACOS(CAPANG)
          
C running total for geometric factor
C comment out j134 28/5/96 midpoints are no longer included
C          if (tstbrd.gt.0.) inarea = inarea + 0.5*sample/(pi*tsteng**2)

C distance increment, -ve as going thru negative records
          DALONG = DALONG -
     &                SQRT( (CAPCEN(1)-PRECEN(1))**2 +
     &                      (CAPCEN(2)-PRECEN(2))**2 +
     &                      (CAPCEN(3)-PRECEN(3))**2   )
C write out the point - centre, area, effective radius, capsule length, capsule radius,
C angle to test axis,
C coordinate of this along cvect
          CALL DDOT( CAPCEN, CVECT, CAPCOR)
          IF (SHORTO.LT.2) WRITE( NOUT,'(8F12.5,A12)')
     &      CAPCOR, -TSTENG, PI*TSTENG**2, 0.5*CAPLEN,
     &      TSTBRD, CAPANG, DALONG, INAREA, ' (mid-point)' 

C minimum effective radius?
          IF (-TSTENG.LT.MINRAD) MINRAD = -TSTENG

C store centre for previous distance calc.
          PRECEN(1) = CAPCEN(1)
          PRECEN(2) = CAPCEN(2)
          PRECEN(3) = CAPCEN(3)
C end of looking a mid-point of interval
        ENDIF
C end of going thru -ve records        
77    CONTINUE

      IF (SHORTO.LT.2) WRITE( NOUT, '(A)') ' (+ve records)**********'
C reset integrated volume 
      TOAREA = INAREA
      INAREA = 0.
      DALONG = 0.

      DO 88 SCOUNT= 0, STRNOP
C the centre point of capsule is between the two centres which define it
        CAPCEN(1) = 0.5*(STRCEN(1,SCOUNT)+STRLVC(1,SCOUNT))
        CAPCEN(2) = 0.5*(STRCEN(2,SCOUNT)+STRLVC(2,SCOUNT))
        CAPCEN(3) = 0.5*(STRCEN(3,SCOUNT)+STRLVC(3,SCOUNT))
C coordinate of this along cvect
        CALL DDOT( CAPCEN, CVECT, CAPCOR)
C the capsule vector is the difference between two point
        CAPVEC(1) = STRCEN(1,SCOUNT) - STRLVC(1,SCOUNT)
        CAPVEC(2) = STRCEN(2,SCOUNT) - STRLVC(2,SCOUNT)
        CAPVEC(3) = STRCEN(3,SCOUNT) - STRLVC(3,SCOUNT)
C the length of this vector
        CAPLEN = SQRT( CAPVEC(1)**2 + CAPVEC(2)**2 + CAPVEC(3)**2)
C find angle to test axis
C first unit capvec
        CALL DUVEC2( CAPVEC)
C then work out dot product with test vector
        CALL DDOT( CAPVEC, TSTVEC, CAPANG)
C arccos the result & convert to degrees
        CAPANG = (180./PI)*ACOS(CAPANG)
C running total for geometric factor
C j134 28/5/96 - take out factor of 1/2
C as do not include mid-points in calc anymore
        INAREA = INAREA + SAMPLE/(PI*STRRAD(SCOUNT)**2)
       
C distance increment, +ve as going thru the positive records
        IF (SCOUNT.NE.0) 
     &     DALONG = DALONG +
     &                SQRT( (CAPCEN(1)-PRECEN(1))**2 + 
     &                      (CAPCEN(2)-PRECEN(2))**2 + 
     &			    (CAPCEN(3)-PRECEN(3))**2   )
C write out the point - centre, area, effective radius, capsule length, capsule radius,
C angle to test axis,
	IF (SHORTO.LT.2) WRITE( NOUT,'(8F12.5,A)') 
     &    CAPCOR,  STRRAD(SCOUNT), PI*STRRAD(SCOUNT)**2, 0.5*CAPLEN, 
     &    STRBRD(SCOUNT), CAPANG, DALONG, INAREA, ' (sampled) '

C minimum effective radius?
        IF (STRRAD(SCOUNT).LT.MINRAD) MINRAD = STRRAD(SCOUNT)
        IF (STRRAD(SCOUNT).LT.MINSAM) MINSAM = STRRAD(SCOUNT)

C store centre for previous distance calc.
        PRECEN(1) = CAPCEN(1)
        PRECEN(2) = CAPCEN(2)
        PRECEN(3) = CAPCEN(3)

C just like in normal sphere output check the mid-point of each output interval
        IF (SCOUNT.LT.STRNOP) THEN
C centre point is the average of the centre points
          CAPCEN(1) = 0.25*(STRCEN(1,SCOUNT)   + STRLVC(1,SCOUNT))  +
     &		      0.25*(STRCEN(1,SCOUNT+1) + STRLVC(1,SCOUNT+1))
          CAPCEN(2) = 0.25*(STRCEN(2,SCOUNT)   + STRLVC(2,SCOUNT))  +
     &		      0.25*(STRCEN(2,SCOUNT+1) + STRLVC(2,SCOUNT+1))
          CAPCEN(3) = 0.25*(STRCEN(3,SCOUNT)   + STRLVC(3,SCOUNT))  +
     &		      0.25*(STRCEN(3,SCOUNT+1) + STRLVC(3,SCOUNT+1))
C work out the capsule vector
C now there is a slight complication - How do we mix?
C same sign? 
C work out both - take longer vector
          CAPVEC(1) = 0.5*(STRCEN(1,SCOUNT)   - STRLVC(1,SCOUNT)) +
     &                0.5*(STRCEN(1,SCOUNT+1) - STRLVC(1,SCOUNT+1))
          CAPVEC(2) = 0.5*(STRCEN(2,SCOUNT)   - STRLVC(2,SCOUNT)) +
     &                0.5*(STRCEN(2,SCOUNT+1) - STRLVC(2,SCOUNT+1))
          CAPVEC(3) = 0.5*(STRCEN(3,SCOUNT)   - STRLVC(3,SCOUNT)) +
     &                0.5*(STRCEN(3,SCOUNT+1) - STRLVC(3,SCOUNT+1))
          ALTVEC(1) = 0.5*(STRCEN(1,SCOUNT)   - STRLVC(1,SCOUNT)) -
     &                0.5*(STRCEN(1,SCOUNT+1) - STRLVC(1,SCOUNT+1))
          ALTVEC(2) = 0.5*(STRCEN(2,SCOUNT)   - STRLVC(2,SCOUNT)) -
     &                0.5*(STRCEN(2,SCOUNT+1) - STRLVC(2,SCOUNT+1))
          ALTVEC(3) = 0.5*(STRCEN(3,SCOUNT)   - STRLVC(3,SCOUNT)) -
     &                0.5*(STRCEN(3,SCOUNT+1) - STRLVC(3,SCOUNT+1))
C is altvec longer?
          IF ( (ALTVEC(1)**2+ALTVEC(2)**2+ALTVEC(3)**2).GT.
     &         (CAPVEC(1)**2+CAPVEC(2)**2+CAPVEC(3)**2)    ) THEN
            CAPVEC(1) = ALTVEC(1)
            CAPVEC(2) = ALTVEC(2)
            CAPVEC(3) = ALTVEC(3)
          ENDIF

C test points
          TSTCEN(1) = CAPCEN(1) - 0.5*CAPVEC(1)
          TSTCEN(2) = CAPCEN(2) - 0.5*CAPVEC(2)
          TSTCEN(3) = CAPCEN(3) - 0.5*CAPVEC(3)
C the second
          TSTCE2(1) = CAPCEN(1) + 0.5*CAPVEC(1)
          TSTCE2(2) = CAPCEN(2) + 0.5*CAPVEC(2)
          TSTCE2(3) = CAPCEN(3) + 0.5*CAPVEC(3)

C find out radius etc. for these new points
          CALL HCAPEN( TSTCEN, TSTENG, TSTCE2, TSTBRD,
     &           IAT1, IAT2, DAT2,
     &           ATMAX, ATNO, ATXYZ, ATVDW, PI)

C the length of this vector
          CAPLEN = SQRT( CAPVEC(1)**2 + CAPVEC(2)**2 + CAPVEC(3)**2)
C find angle to test axis
C first unit capvec
          CALL DUVEC2( CAPVEC)
C then work out dot product with test vector
          CALL DDOT( CAPVEC, TSTVEC, CAPANG)
C arccos the result & convert to degrees
          CAPANG = (180./PI)*ACOS(CAPANG)
          
C running total for geometric factor
C comment out j134 28/5/96 midpoints are no longer included
C          if (tstbrd.gt.0.) inarea = inarea + 0.5*sample/(pi*tsteng**2)

C distance increment, -ve as going thru negative records
          DALONG = DALONG +
     &                SQRT( (CAPCEN(1)-PRECEN(1))**2 +
     &                      (CAPCEN(2)-PRECEN(2))**2 +
     &                      (CAPCEN(3)-PRECEN(3))**2   )
C write out the point - centre, area, effective radius, capsule length, capsule radius,
C coordinate of this along cvect
          CALL DDOT( CAPCEN, CVECT, CAPCOR)
C angle to test axis,
          IF (SHORTO.LT.2) WRITE( NOUT,'(8F12.5,A12)')
     &      CAPCOR, -TSTENG, PI*TSTENG**2, 0.5*CAPLEN,
     &      TSTBRD, CAPANG, DALONG, INAREA, ' (mid-point)' 

C minimum effective radius?
          IF (-TSTENG.LT.MINRAD) MINRAD = -TSTENG

C store centre for previous distance calc.
          PRECEN(1) = CAPCEN(1)
          PRECEN(2) = CAPCEN(2)
          PRECEN(3) = CAPCEN(3)
C end of looking a mid-point of interval
        ENDIF
C end of going thru +ve records        
88    CONTINUE


C vital stats bit  ********

C length ***
C initialize coordinate comparison stores
      UCOORD = -1E20
      LCOORD =  1E20
C find the coordinates of all atom in direction cvect
      DO 13 ACOUNT = 1, ATNO
        COORD = CVECT(1)*ATXYZ(1,ACOUNT) +
     &          CVECT(2)*ATXYZ(2,ACOUNT) +
     &          CVECT(3)*ATXYZ(3,ACOUNT)
C store the highest and lowest coord
        IF (COORD.GT.UCOORD) UCOORD = COORD
        IF (COORD.LT.LCOORD) LCOORD = COORD
13    CONTINUE
      LENAT = ABS(UCOORD - LCOORD)
C for conduction prediction also need difference in channel coords 
C of the end points (to 15Angs)
C the upper end
      CAPCEN(1) = 0.5*(STRCEN(1,STRNOP)+STRLVC(1,STRNOP))
      CAPCEN(2) = 0.5*(STRCEN(2,STRNOP)+STRLVC(2,STRNOP))
      CAPCEN(3) = 0.5*(STRCEN(3,STRNOP)+STRLVC(3,STRNOP))
C coordinate of this along cvect
      CALL DDOT( CAPCEN, CVECT, UCOORD)
C lower end
      CAPCEN(1) = 0.5*(STRCEN(1,-STRNON)+STRLVC(1,-STRNON))
      CAPCEN(2) = 0.5*(STRCEN(2,-STRNON)+STRLVC(2,-STRNON))
      CAPCEN(3) = 0.5*(STRCEN(3,-STRNON)+STRLVC(3,-STRNON))
C coordinate of this along cvect
      CALL DDOT( CAPCEN, CVECT, LCOORD)
C the length in question is the difference between the ends
      LENHO = ABS(UCOORD - LCOORD)
C length ***

C end radius - need to find end  ***
C use ucoord and lcoord to find "end of channel"
C go thru all points in +ve direction
      DO 14 CCOUNT = 1, STRNOP
C find coordinate of sphere in direction cvect
        COORD = CVECT(1)*STRCEN(1,CCOUNT) +
     &          CVECT(2)*STRCEN(2,CCOUNT) +
     &          CVECT(3)*STRCEN(3,CCOUNT)
C is this bigger than ucoord?
        IF (COORD.GT.UCOORD) THEN
C store end radius
          UENDR = STRRAD(CCOUNT)
C jump out of loop
          GOTO 15
        ENDIF
14    CONTINUE
C if we get here then hole run incomplete?
      WRITE(NOUT,'(A)') 'WARNING RUN MAY BE INCOMPLETE IN +VE DIRECTION'
      UENDR = 99999.999
C jump here
15    CONTINUE 

C end radius - need to find end  ***
C use ucoord and lcoord to find "end of channel"
C go thru all points in +ve direction
      DO 24 CCOUNT = 1, STRNON
C find coordinate of sphere in direction cvect
        COORD = CVECT(1)*STRCEN(1,-CCOUNT) +
     &          CVECT(2)*STRCEN(2,-CCOUNT) +
     &          CVECT(3)*STRCEN(3,-CCOUNT)
C is this bigger than ucoord?
        IF (COORD.GT.LCOORD) THEN
C store end radius
          LENDR = STRRAD(-CCOUNT)
C jump out of loop
          GOTO 25
        ENDIF
24    CONTINUE
C if we get here then hole run incomplete?
      WRITE(NOUT,'(A)') 'WARNING RUN MAY BE INCOMPLETE IN -VE DIRECTION'
      LENDR = 99999.999
C jump here
25    CONTINUE

C before conduct calcs - first work out ele-pot - need to return average
C 11 Nov 96 - work out the electrostatic potential created along
C pore centre line by formal charges in the system
C s/r should work both with the capsule and normal call -
C hence double ref to STRCEN (capsule has two centres).
      CALL HELEFI( NOUT, AVGPOT, CVECT, SHORTO,
     &             STRMAX, STRNOP, STRNON, STRCEN, STRLVC,
     &             ATMAX, ATNO, ATBRK, ATRES,
     &             ATCHN, ATRNO, ATXYZ, ATVDW, ATBND)


      IF (SHORTO.LT.3) THEN
        WRITE( NOUT, '(/ 80(A,F10.3,A/))' )
     &' Minimum EFFECTIVE radius:     ', MINRAD, ' angstroms',
     &' "" if midpoints excluded:     ', MINSAM, ' angstroms',
     &' "Atomic" length of channel:   ', LENAT,  ' angstroms',
     &' Length defined by end points: ', LENHO,  ' angstroms',
     &' Upper end eff radius:         ', UENDR, ' angstroms',
     &' Lower end eff radius:         ', LENDR, ' angstroms',
     &' Average eff end radius:       ', 2.*UENDR*LENDR/(UENDR+LENDR),
     &						         ' angstroms'
        WRITE( NOUT, '(A/ A,F10.3,A)')
     &' Average electrostatic potential caused by formal',
     &'   charges excluding HIS:      ', AVGPOT, 
     &                     ' kcal/mol per proton charge' 
      ENDIF
      
C total integrated inverse area
      TOAREA = TOAREA + INAREA

C Area meaningless if radius goes -ve - use sample radius not midpoint
      IF (MINSAM.LT.0.) TOAREA = 1D8

C new conductivity calcs 26/11/96



C write out predicted conductivity

      IF (SHORTO.LT.3) WRITE( NOUT, '(//A,F8.3,A)')
     &' The geometric factor F= sum(ds/area) along channel is ', TOAREA,
     &                                          ' angstroms**-1'
C write out predicted conductances to two signicant figures
      CALL SF2( ST2SF, LST2SF, 100D0/TOAREA)
      IF (SHORTO.LT.3) WRITE( NOUT, '(A)')
     &' This yields a macroscopic predicted molar conductance of:',
     &'   (1/rho)*(100/F)= ('//ST2SF(1:LST2SF)//'/rho) pS,',
     &'   where rho is the conductivity of 1M permeant ion in ohm m.'

      GMACRO = 1200D0/TOAREA
      CALL SF2( ST2SF, LST2SF, GMACRO)
      IF (SHORTO.LT.3) WRITE( NOUT, '(A,A,A)')
     &' For 1M KCl rho= 1/12 (ohm m), So Gmacro= ',
     &                  ST2SF(1:LST2SF), ' pS/M.'

C empirically derived devisor - to give correct conductance for Porin
C and gramicidin - at present for simple.rad see j059 replaced j137 
C and again j141/157!
C j134 use MINSAM not MINRAD for calc. - this is first generation
      DIV1 = -0.2398*MINSAM + 5.9132
C make sure it remains above 1
      IF (DIV1.LT.1.) DIV1 = 1.
      IF (MINSAM.LT.0.) DIV1 = 1E6

C 2nd generation divisors
C (a) constant
      DIV2 = 5.5940
C (b) inverse length
      DIV3 = 1./(0.3799-0.00500*LENHO)
C (c) avg electrostatic potential
      DIV4 = 5.1080-0.00839*AVGPOT

C tell user
      IF (SHORTO.LT.3) WRITE( NOUT, '(A/ 4(A,F12.5/), A/A)')
     &' Empirically derived correction factors:',
     &'    first generation (2 system) on MINRAD= ', DIV1,
     &'    2nd generation (8 system) constant=    ', DIV2,
     &'    2nd generation on length of channel=   ', DIV3,
     &'    2nd generation on avg elect potential= ', DIV4,
     &' ',
     &' yields predictions: '
      GPRED1 = GMACRO/DIV1
      GPRED2 = GMACRO/DIV2
      GPRED3 = GMACRO/DIV3
      GPRED4 = GMACRO/DIV4

C write only 2 sf's in each case
      CALL SF2( ST2SF, LST2SF, GPRED1)
      IF (SHORTO.LT.3) WRITE( NOUT, '(A)')
     &'    first generation (2 system) on MINRAD= '//
     &                           ST2SF(1:LST2SF)//' pS/M.'
      CALL SF2( ST2SF, LST2SF, GPRED2)
      IF (SHORTO.LT.3) WRITE( NOUT, '(A)')
     &'    2nd generation (8 system) constant=    '//
     &                           ST2SF(1:LST2SF)//' pS/M.'
      CALL SF2( ST2SF, LST2SF, GPRED3)
      IF (SHORTO.LT.3) WRITE( NOUT, '(A)')
     &'    2nd generation on length of channel=   '//
     &                           ST2SF(1:LST2SF)//' pS/M.'
      CALL SF2( ST2SF, LST2SF, GPRED4)
      IF (SHORTO.LT.3) WRITE( NOUT, '(A)')
     &'    2nd generation on avg elect potential= '//
     &                           ST2SF(1:LST2SF)//' pS/M.'

C single estimate - take average on the 3 2nd generation
      GPRED = (GPRED2 + GPRED3 + GPRED4)/3.
      CALL SF2( ST2SF, LST2SF, GPRED)
      IF (SHORTO.LT.3) WRITE( NOUT, '(A)')
     &' ',
     &'  As a single estimate take average of last 3 so Gpred= '//
     &                           ST2SF(1:LST2SF)//' pS/M.'
      
      IF (SHORTO.LT.3) WRITE( NOUT, '(A)')
     &' N.B., The predicted conductances given above are only',
     &'       a crude estimate and should not be over interpreted!'

C warn if the difference between MINSAM and MINRAD is so high as to
C indicate there may be a break.
      IF (MINRAD.LT.0.8*MINSAM) 
     &   WRITE( NOUT, '(A/ 2(A,F12.3/), 3(A/))')
     &' SERIOUS WARNING ********',
     &' The minimum effective radius including mid-points= ', MINRAD,
     &' This is much smaller than for just sampled points= ', MINSAM,
     &' The difference (>20%) may mean there IS A BREAK IN HOLE RUN',
     &' If this is the case then conductance stuff invalid',
     &' SERIOUS WARNING ********'

C need to write out detailed numbers
      IF (SHORTO.LT.3) 
     &  WRITE( NOUT, '(A,I6,3(A,F12.5),4F12.5,A,F12.5,A)')
     &'  (TAG ', POSNO,
     &  '   Rmin=',  MINSAM, 
     &  '   Gmacro=',  GMACRO,
     &  '   Gpred=',  GPRED1, GPRED2, GPRED3, GPRED4, GPRED,
     &  '   Rmin-mid=',  MINRAD, ' )'
      IF (NOUT.EQ.6) CALL FLUSH6

C statistics
      NOCALL = NOCALL + 1
      TOTR   = TOTR   + MINSAM
      TOTR2  = TOTR2  + MINSAM**2
      TOTGM  = TOTGM  + GMACRO
      TOTGM2 = TOTGM2 + GMACRO**2
      TOTGP  = TOTGP  + GPRED
      TOTGP2 = TOTGP2 + GPRED**2
C if there is more than one call write means and s.d.
      IF ( (SHORTO.LT.3) .AND. (NOCALL.GT.1) ) THEN
        WRITE( NOUT, '(A)') 
     &' To date:'
        WRITE( NOUT, '(2(A,F12.5))')
     &' mean Rmin= ', TOTR/NOCALL, ' sd=', 
     &        SQRT((TOTR2/NOCALL)-(TOTR/NOCALL)**2),
     &' mean Gmacro= ', TOTGM/NOCALL, ' sd=', 
     &        SQRT((TOTGM2/NOCALL)-(TOTGM/NOCALL)**2),
     &' mean Gpred=', TOTGP/NOCALL, ' sd=', 
     &        SQRT((TOTGP2/NOCALL)-(TOTGP/NOCALL)**2)
      ENDIF


C return here
      RETURN
      END
