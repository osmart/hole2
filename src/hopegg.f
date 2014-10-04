      SUBROUTINE HOPEGG( PEGRAT, NOUT, CVECT,
     &                 STRMAX, STRNOP, STRNON, STRCEN, STRRAD,
     &                 SHORTO, SAMPLE, ENDRAD, PI,
     &                 ATMAX, ATNO, ATXYZ, ATVDW)
      IMPLICIT NONE
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
C
C Modification history:
C
C Date	Author		Modification
C 01/96 O.S. Smart      First version
C 
C 10/1/96 add yet another routine for calculate PEG radius type graph

C passed variables *************

C vbles for working peg effect on conductance -
C first number the ratio of conductance with PEG about to water
C second number is the ratio for non-penetrating PEG
      DOUBLE PRECISION          PEGRAT(2)

C output stream no. (usually set to 6) RETURN UNCHANGED
      INTEGER			NOUT

C The channel vector - already united
      DOUBLE PRECISION		CVECT(3)

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

C 11/95 introduce new variable to reduce text output -
C if oshort =
C 0: full text output
C 1: no in run progress reported
C 2: ditto plus no graph type output - only leaving
C    Minimum radius & conductance calculations.
C 3: All text output other than input card mirroring
C    and error messages turned off
      INTEGER                   SHORTO

C sampling distance used
      DOUBLE PRECISION		SAMPLE

C radius above which a point is regarded to reached free space
      DOUBLE PRECISION		ENDRAD

C pi
      DOUBLE PRECISION		PI

C atom coord store - need to find last atom
      INTEGER			ATMAX, ATNO
      DOUBLE PRECISION		ATXYZ(3,ATMAX)
C van der Waals radius for each atom
      DOUBLE PRECISION		ATVDW(ATMAX)

C internal vbles ***************

C loop counts: atoms, along channel, other chanel
C real for radius cutoff
      INTEGER			ACOUNT, CCOUNT, OCOUNT
      DOUBLE PRECISION		RCUT

C resistance sum
      DOUBLE PRECISION		RESIST

C "conductivity" for PEG and non-peg areas
      DOUBLE PRECISION		PHIPEG, PHINON

C displacement vector, a dot product (cvect.disp),
C an effective radius, distance between circle centres,
C area of overlap of two circles, maximum area of overlap
C of any circle, two resistances
      DOUBLE PRECISION		DISP(3), CDOTDP, EFFRAD, DCEN, 
     &				AREA, MAXARE, R1, R2

C displacement along cvect for an atom - upper and lower values
      DOUBLE PRECISION		COORD, UCOORD, LCOORD 

C vbles need for call to holeen to work out the radius of a point
      DOUBLE PRECISION		NEWENG, DAT2, DAT3
      INTEGER 			IAT1, IAT2, IAT3


C end of decs ******************

      write( nout, '(a)') 
     &' This routine is  a very early state of development.',
     &' Please get in touch with oss if you are interested in using.'
      do 111  acount = 1, 20
        write( nout, '(a,a)') 
     &' YOU MUST CHECK WITH OLIVER SMART BEFORE YOU', 
     &'   PUBLISH ANY RESULTS USING THIS ROUTINE.'
111   continue

C flush output stream
      IF (NOUT.EQ.6) CALL FLUSH6

C if shorto is 3 then do not want any output
      IF (SHORTO.LT.3) THEN

C initialize coordinate comparison stores
        UCOORD = -1E20
        LCOORD =  1E20

C find the coordinates of all atom in direction cvect
        DO 13 ACOUNT = 1, ATNO
          COORD = CVECT(1)*ATXYZ(1,ACOUNT) +
     &            CVECT(2)*ATXYZ(2,ACOUNT) +
     &            CVECT(3)*ATXYZ(3,ACOUNT)
C store the highest and lowest coord
          IF (COORD.GT.UCOORD) UCOORD = COORD
          IF (COORD.LT.LCOORD) LCOORD = COORD
13      CONTINUE

C use ucoord and lcoord to find "end of channel"
C go thru all points in +ve direction
        DO 14 CCOUNT = 1, STRNOP
C find coordinate of sphere in direction cvect
          COORD = CVECT(1)*STRCEN(1,CCOUNT) +
     &            CVECT(2)*STRCEN(2,CCOUNT) +
     &            CVECT(3)*STRCEN(3,CCOUNT)
C is this bigger than ucoord?
          IF (COORD.GT.UCOORD) THEN
C jump out of loop
            WRITE( NOUT, '(A/A/A,F8.3/ A,I5,A,I5,A)')
     &' ',
     &' Working out PEG graph************************',
     &' Channel end in +ve direction has radius= ', STRRAD(CCOUNT),
     &' (Reducing number of spheres stored in +ve direction from',
     &                                  STRNOP, ' to ', CCOUNT, ')'
C eliminate all points after this
            STRNOP = CCOUNT
            GOTO 15
          ENDIF
14      CONTINUE
C end is not present - jump out of routine
        WRITE( NOUT, '(A)')
     &' Warning in s/r HOPEGG - not enough +ve points',
     &' cannot proceed '
C abort
        GOTO 55555
C jump out to here
15      CONTINUE

C flush output stream
        IF (NOUT.EQ.6) CALL FLUSH6

C now find lower end
C go thru all points in -ve direction
        DO 16 CCOUNT = 1, STRNON
C find coordinate of sphere in direction cvect
          COORD = CVECT(1)*STRCEN(1,-CCOUNT) +
     &            CVECT(2)*STRCEN(2,-CCOUNT) +
     &            CVECT(3)*STRCEN(3,-CCOUNT) 
C is this bigger than ucoord?
          IF (COORD.LT.LCOORD) THEN
C jump out of loop
            WRITE( NOUT, '(A,F8.3/ A,I5,A,I5,A)')
     &' Channel end in -ve direction has radius= ', STRRAD(-CCOUNT),
     &' (Reducing number of spheres stored in -ve direction from',
     &   				STRNON, ' to ', CCOUNT, ')'
C eliminate all points after this
            STRNON = CCOUNT
            GOTO 17
          ENDIF
16      CONTINUE
C end is not present - jump out of routine
        WRITE( NOUT, '(A)')
     &' Warning in s/r HOPEGG - not enough +ve points',
     &' cannot proceed '
C abort
        GOTO 55555
C jump out to here
17      CONTINUE

C flush output stream
        IF (NOUT.EQ.6) CALL FLUSH6

C add 200 points to +ve end of channel
        IF ((STRNOP+200).GT.STRMAX) THEN
          WRITE( NOUT, '(A)')
     &' ERROR in s/r HOPEGG - not enough room to store',
     &'   the additionally required points.',
     &' Please increase STRMAX an retry'
          GOTO 55555
        ENDIF
C add 200 points to +ve end leaving strnop unaltered.
        DO 33 CCOUNT = 1, 200
C add half cvect to previous point
          STRCEN(1,STRNOP+CCOUNT)=STRCEN(1,STRNOP+CCOUNT-1)+0.5*CVECT(1)
          STRCEN(2,STRNOP+CCOUNT)=STRCEN(2,STRNOP+CCOUNT-1)+0.5*CVECT(2)
          STRCEN(3,STRNOP+CCOUNT)=STRCEN(3,STRNOP+CCOUNT-1)+0.5*CVECT(3)
C find radius of the point
          CALL HOLEEN( STRCEN(1,STRNOP+CCOUNT), 
     &                 NEWENG, IAT1, IAT2, IAT3, DAT2, DAT3,
     &                 ATMAX, ATNO, ATXYZ, ATVDW, 5D0)
C neweng is returned as minus the radius of the new point
          STRRAD(STRNOP+CCOUNT)=-NEWENG
33      CONTINUE

C do same for -ve
        IF ((STRNON+200).GT.STRMAX) THEN
          WRITE( NOUT, '(A)')
     &' ERROR in s/r HOPEGG - not enough room to store',
     &'   the additionally required points. -ve',
     &' Please increase STRMAX an retry'
          GOTO 55555
        ENDIF
C add 200 points to +ve end leaving strnop unaltered.
        DO 34 CCOUNT = 1, 200
C add half cvect to previous point
          STRCEN(1,-STRNON-CCOUNT)
     &			=STRCEN(1,-STRNON-CCOUNT+1)-0.5*CVECT(1)
          STRCEN(2,-STRNON-CCOUNT)
     &			=STRCEN(2,-STRNON-CCOUNT+1)-0.5*CVECT(2)
          STRCEN(3,-STRNON-CCOUNT)
     &			=STRCEN(3,-STRNON-CCOUNT+1)-0.5*CVECT(3)
C find radius of the point
          CALL HOLEEN( STRCEN(1,-STRNON-CCOUNT),
     &                 NEWENG, IAT1, IAT2, IAT3, DAT2, DAT3,
     &                 ATMAX, ATNO, ATXYZ, ATVDW, 5D0)
C neweng is returned as minus the radius of the new point
          STRRAD(-STRNON-CCOUNT)=-NEWENG
34      CONTINUE

C work out values for phipeg and phinon
C do conductivity integration for channel
C go thru channel
        RESIST = 0.
        DO 10 CCOUNT = -STRNON, STRNOP
C if radius goes negative abort routine
          IF (STRRAD(CCOUNT).LT.0) GOTO 55555
          RESIST = RESIST + ABS(SAMPLE)/(PI*STRRAD(CCOUNT)**2)
10      CONTINUE

C add access resistance for the ends
        RESIST = RESIST + 1./(4.*STRRAD(STRNOP))
        RESIST = RESIST + 1./(4.*STRRAD(-STRNON))

C want conductivity ratio to be PEGRAT(1) for PEG and PEGRAT(2) for NON
        PHIPEG = 1./(PEGRAT(1)*RESIST)
        PHINON = 1./(PEGRAT(2)*RESIST)

C now work out conductivity ratio 
C for radii from 0 to endrad+1. angstroms, 
C take steps of 0.25 angstroms
        DO 20 RCUT = 0., ENDRAD+1., 0.25
C if radius is above 20 angstroms only do every 1. angs 
          IF ((RCUT.GT.20.).AND.(MOD(RCUT,1D0).GT.1E-04)) GOTO 20
C if above 50 then every 10.
          IF ((RCUT.GT.50.).AND.(MOD(RCUT,10D0).GT.1E-04)) GOTO 20

C zero resistance
          RESIST = 0.

C go thru each point in channel
          DO 30 CCOUNT = -STRNON, STRNOP
C is the radius of point above the radius cutoff?
            IF (STRRAD(CCOUNT).GT.RCUT) THEN
C use PEG conductivity for complete slab
              RESIST = RESIST + 
     &                 ABS(SAMPLE*PHIPEG)/(PI*STRRAD(CCOUNT)**2)
            ELSE
C slab is "NON-PEG" but other PEG spheres may penetrate
C find largest penetrating area - initialize store
              MAXARE = -1.
C go thru all PEG spheres in channel
C MUST ALSO CONSIDER SPHERES WHICH HAVE BEEN ADDED BEFORE AND AFTER
              DO 40 OCOUNT = -STRNON-200, STRNOP+200
C other point PEG sphere?
                IF (STRRAD(OCOUNT).GT.RCUT) THEN
C yes - so does it penetrate to circle in plane of ccount?
C find vector between point ocount and ccount
                  DISP(1) = STRCEN(1,CCOUNT) - STRCEN(1,OCOUNT)
                  DISP(2) = STRCEN(2,CCOUNT) - STRCEN(2,OCOUNT)
                  DISP(3) = STRCEN(3,CCOUNT) - STRCEN(3,OCOUNT)
C find dot product between this vector and the unit channel vector
                  CALL DDOT( DISP, CVECT, CDOTDP)
C the effective radius**2 of ocount in the plane normal to cvect thru ccount
                  EFFRAD = STRRAD(OCOUNT)**2 - CDOTDP**2
C if effrad is less than zero there is no overlap so do no further
                  IF (EFFRAD.GT.0.) THEN
C find the effective radius
                    EFFRAD = SQRT(EFFRAD)
C also need distance between ccount and the effective centre of ocount
                    DCEN = (DISP(1) - CDOTDP*CVECT(1))**2 +
     &                     (DISP(2) - CDOTDP*CVECT(2))**2 + 
     &                     (DISP(3) - CDOTDP*CVECT(3))**2
                    DCEN = SQRT(DCEN)
C now find area of intersection between circle ccount and
C the effective cirle for ocount - do by special s/r
                    CALL CIROVA( STRRAD(CCOUNT), EFFRAD, DCEN, AREA, PI)
C store the maximum area of any circle
                    IF (AREA.GT.MAXARE) MAXARE = AREA
                  ENDIF
                ENDIF
40            CONTINUE
C have go thru all other spheres
C maximum area of overlap from a peg cirlce is maxare
C if still -1. then count whole slab as having resistivity
C phinon
              IF (MAXARE.LE.0.) THEN
                RESIST = RESIST + 
     &                 ABS(SAMPLE*PHINON)/(PI*STRRAD(CCOUNT)**2)
              ELSE
C have overlap area of maxare with resistivity of phipeg
C in parallel with an area pi.r**2-maxare of phinon
C end of radius below rcut bit
C have to trap case where overlap area is total
                IF ((PI*STRRAD(CCOUNT)**2-MAXARE).GT.0.) THEN
                  R1 = ABS(SAMPLE*PHIPEG)/MAXARE
                  R2 = ABS(SAMPLE*PHINON)/(PI*STRRAD(CCOUNT)**2-MAXARE)
C these are resistor in parrallel use standard formulae
                  RESIST = RESIST + (R1*R2)/(R1+R2)
                ELSE
C all the area accomodate in peg
                  RESIST = RESIST +
     &                 ABS(SAMPLE*PHIPEG)/(PI*STRRAD(CCOUNT)**2)
                ENDIF 
              ENDIF

            ENDIF
C end of going thru each point in channel
30        CONTINUE
C add access resistance for the ends - resistivity always phipeg
          RESIST = RESIST + (1.*PHIPEG)/(4.*STRRAD(STRNOP))
          RESIST = RESIST + (1.*PHIPEG)/(4.*STRRAD(-STRNON))

C write tagged output of conductivity
          WRITE( NOUT, '(A,2F12.5)')
     &'PEG-GRAPH ', RCUT, 1./RESIST
C end of looking for resistance of radius rcut
20      CONTINUE

      ENDIF

      write( nout, '(a)') 
     &' This routine is  a very early state of development.',
     &' Please get in touch with oss if you are interested in using.'
      do 112  acount = 1, 20
        write( nout, '(a,a)') 
     &' YOU MUST CHECK WITH ME BEFORE YOU', 
     &'   PUBLISH ANY RESULTS USING THIS ROUTINE.'
112   continue

C flush output stream
      IF (NOUT.EQ.6) CALL FLUSH6


C return here
55555 RETURN
      END
