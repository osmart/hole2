      SUBROUTINE H2DMAP( F2ROOT, NOUT, CVECT, 
     &                 STRMAX, STRNOP, STRNON, STRCEN, STRRAD,
     &                 SHORTO, SAMPLE, ENDRAD, PI,
     &                 ATMAX, ATNO, ATBRK, ATRES,
     &                 ATCHN, ATRNO, ATXYZ, ATVDW, ATBND)
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
C * (c) 1997 Oliver Smart                    *
C *                                                                  *
C ********************************************************************
C
C Modification history:
C
C Date	Author		Modification
C 03/96 O.S. Smart      First version
C 12/97	O.S.S.		change to call holeen
C 
C 20/3/96 Routine to calculate 2D property maps for input to surfer  

C passed variables *************

C filename root -
C e.g. if it is 'new'
C then will produce
C new_touch.grd - info for grid surface showing distance to touching atom
C new_ca.dat    - post info of CA coords
      CHARACTER*200		F2ROOT

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

C internal vbles ***************

C unit vectors used to define rotation
      DOUBLE PRECISION		PERPV1(3), PERPV2(3)

C dot product between cvect and perpv1
      DOUBLE PRECISION		CDOTP

C Displacement along cvect for a point
      DOUBLE PRECISION		CDISP, CDISP2

C max// number of divisions to make in the angle
C (73 for 5 degrees)
      INTEGER			AMAX, ADIV
      PARAMETER(		AMAX= 140)
	
C vector to store the results for the ADIV angles considered
C as we output up to 5 maps (of different properties) at once 
C need 5 seperate stores
      DOUBLE PRECISION		RSTORE( AMAX, 5)

C loop counts angles, positions, spike algorithm
C file stream number
      INTEGER			ACOUNT, SCOUNT, CCOUNT,
     &				MCOUNT

C stream numbers for output files - use vector as we have
C so many files open at any time
C number 6 is an internal hole format
      INTEGER			MOUT(6)

C value for angle, direction of spike, distance
C from sphere centre to any atoms vdw surface at that point,
C store for angle step
      DOUBLE PRECISION		ANGLE, DIRECT(3), DTOUCH,
     & 				ANSTEP

C need some dummy variables for s/r HOLEEN call
      INTEGER                   IAT1, IAT2, IAT3
      DOUBLE PRECISION          DAT2, DAT3
C also a test position and energy (to do spike option)
      DOUBLE PRECISION          NEWCEN(3), NEWENG

C the last character of the root filename
      INTEGER			LASTC
C filename for opening to:
      CHARACTER*200		FWORK

C minimum distance to sphere centre
C sphere centre number 
      DOUBLE PRECISION		CMIN
      INTEGER			CNUM

C difference vector
      DOUBLE PRECISION		DIFFV(3)

C a dot product, angle for a calpha, a working vble
      DOUBLE PRECISION		CDOTV, CANGLE, RESULT

C vble to store symbol number
      INTEGER			SYMBNO
C vble to detect when we have a change in chain
      CHARACTER*1		PCHAIN
      INTEGER			PRNO
      DOUBLE PRECISION		PANGLE
      DOUBLE PRECISION		PDISP

C function to linearly interpolate between two points
C arguements (xtest,x1,y1,x2,y2)
C function returns y value for xtest between
C two points (x1,y1) and (x2,y2)
      DOUBLE PRECISION		LINTER

C store for residue base map output
C store the residue number found along a line for a constant y
C and the next line too.  Also do the same for chain id
      INTEGER			RESSTN( 2, AMAX)
      CHARACTER*1		RESSTC( 2, AMAX)

C y value for a line (need for residue no base map) - old value
      DOUBLE PRECISION		YLINE, YOLD

C offset for chain id conversion
      INTEGER			CHNOFF


C 30 Sept 96 store for contacting residue information
C maximum number in store
      INTEGER			CRIMAX
      PARAMETER(		CRIMAX = 500)
C the actual number in the store
      INTEGER			CRINUM
C the residue number and chain id information
      INTEGER			CRIRNO(CRIMAX)
      CHARACTER*1		CRICHN(CRIMAX)
C the total number of records which have stored
      INTEGER			CRINST(CRIMAX)
C the coordinate store in angle, displacement along cvect
C (will contain sum of all coords for a particular residue)
      DOUBLE PRECISION		CRICOO( 2, CRIMAX)
      
C angle in degree
      DOUBLE PRECISION		ANGDEG
C loop count for residue tag info
      INTEGER			TCOUNT
      
C end of decs ******************

C initialize contacting residue information store
      CRINUM = 0

C symbol should start at 5 a nice open diamond (will have 2 added bef use)
      SYMBNO = 3

C find the end of the filename
      CALL CHREND( F2ROOT, LASTC)

C default number of angle sample - 73 gives 5 degree step
      ADIV = 73

C first thing to do is to define a vector at right angles
C to cvect from which the angle can be measured
      IF ( (ABS(CVECT(2)).LT.1E-06) .AND.
     &     (ABS(CVECT(3)).LT.1E-06)       ) THEN
        PERPV1(1) = 0.
        PERPV1(2) = 1.
        PERPV1(3) = 0.
      ELSE
        PERPV1(1) = 1.
        PERPV1(2) = 0.
        PERPV1(3) = 0.
      ENDIF
C now PERPV1 must be at right angles to CVECT - so find the component
C in common
      CALL DDOT( CVECT, PERPV1, CDOTP)
C as CVECT is a unit vector then can easily correct
      PERPV1(1) = PERPV1(1) - CDOTP*CVECT(1)
      PERPV1(2) = PERPV1(2) - CDOTP*CVECT(2)
      PERPV1(3) = PERPV1(3) - CDOTP*CVECT(3)
C make sure that this is a unit vector
      CALL DUVECT2( PERPV1)
C now get a vector which is normal both to CVECT and PERPV1
      CALL DCROSS( PERPV1, CVECT, PERPV2)

C tell user vectors used to define rotation
      IF (SHORTO.LT.2) WRITE( NOUT, '(A/A/ A,3F8.3/ A,3F8.3)')
     &' S/r H2DMAP will output 2D SURFER property maps ',
     &'  Angle vs distance along cvect.',
     &'  Zero degrees angle is along vector ', PERPV1,
     &'    90 degrees angle is along vector ', PERPV2

C make up filename for the touching distance from hole centreline
      FWORK = F2ROOT(1:LASTC)//'_touch.grd'
      WRITE( NOUT, '(A)')
     &' Have opened files: ',
     & FWORK(1:INDEX(FWORK,'   ')-1)//' for map of touching distance'
      IF (NOUT.EQ.6) CALL FLUSH6

C open the output .grd format file
      CALL STREAM( MOUT(1))
      OPEN( MOUT(1), FILE= FWORK, STATUS= 'UNKNOWN')
      REWIND( MOUT(1))
C write first line of file
      WRITE( MOUT(1), '(A)') 'DSAA'

C next two numbers are the number of grid lines 
C in x and number in y.
C x - we look at every 5 degrees making 73 points
C y - we have one line for every stored point (remembering zeroth)
      WRITE( MOUT(1), '(2I10)') ADIV, (STRNOP+STRNON+1)

C next line is the minimum and maximum X value
C (write in degrees)
      WRITE( MOUT(1), '(A)') '-180.0 180.0'

C next line is the minimum and maximum Y value
C This is the displacement along cvect of strxyz(*,strnop)
C and strxyz(*,-strnon)
      CALL DDOT( CVECT, STRCEN(1,STRNOP), CDISP)
      CALL DDOT( CVECT, STRCEN(1,-STRNON), CDISP2)
      WRITE( MOUT(1), '(2F12.5)') CDISP2, CDISP

C next line is the minimum and maximum z value on grid -
C will have to do calculation to really work out
C but zero/ten gives a reasonable range in practice
      WRITE( MOUT(1), '(A)') '0.0 10.0'

C go through stored points from lowest to highest
      DO 10 SCOUNT = -STRNON, STRNOP
C go through angle from -180 to 180 
        DO 20 ACOUNT = 1, ADIV
C angle starts -pi goes to +pi
          ANGLE = 2.*PI*FLOAT(ACOUNT-1)/FLOAT(ADIV-1) - PI

C the direction of the point to be considered from the
C sphere centre is given by
          DIRECT(1) = COS(ANGLE)*PERPV1(1) + 
     &                SIN(ANGLE)*PERPV2(1)
          DIRECT(2) = COS(ANGLE)*PERPV1(2) + 
     &                SIN(ANGLE)*PERPV2(2)
          DIRECT(3) = COS(ANGLE)*PERPV1(3) + 
     &                SIN(ANGLE)*PERPV2(3)


C next section adapted from spike option in hodotc
C first estimate of when we touch a surface is the radius of 
C the sphere
          DTOUCH = STRRAD(SCOUNT)
C do ten cycles of refinement to find sphere centre
C SEE f048 FOR METHOD
          DO 55 CCOUNT = 1, 10
C load up current point dum is the distance along line
C from the sphere which is current estimate of overlap point.
            NEWCEN(1) = DIRECT(1)*DTOUCH + STRCEN(1,SCOUNT)
            NEWCEN(2) = DIRECT(2)*DTOUCH + STRCEN(2,SCOUNT)
            NEWCEN(3) = DIRECT(3)*DTOUCH + STRCEN(3,SCOUNT)

C find energy of point newcen
C - energy is minus radius the largest sphere
C which can be centered at this point without vdW overlap
            CALL HOLEEN( NEWCEN, NEWENG, IAT1, IAT2, IAT3, DAT2, DAT3,
     &                         ATMAX, ATNO, ATXYZ, ATVDW, 10D0)
C add on the radius to the estimate of overlap point
            DTOUCH = DTOUCH - NEWENG
C put a limit of 50 angstroms on the line length
C            IF (DTOUCH.GT.50.0) DTOUCH = 50.0
55        CONTINUE
C store value of DTOUCH in array
          RSTORE(ACOUNT,1) = DTOUCH

C end of going thru angle
20      CONTINUE

C write this row out
        WRITE( MOUT(1), '(1000(F9.3,1X))') 
     &    (RSTORE(ACOUNT,1), ACOUNT=1, ADIV)
C end of going thru stored points (scount loop)
10    CONTINUE

C close touching file
      CLOSE( MOUT(1)) 

C make up filename for the Calpha posting info
      FWORK = F2ROOT(1:LASTC)//'_capost.dat'
      WRITE( NOUT, '(A)')
     & FWORK(1:INDEX(FWORK,'   ')-1)//' for Calpha posting info'
      CALL STREAM( MOUT(1))
      OPEN( MOUT(1), FILE= FWORK, STATUS= 'UNKNOWN')
      REWIND( MOUT(1))

C make up and open Calpha line draw file to MOUT(2)
      FWORK = F2ROOT(1:LASTC)//'_caline.bln'
      WRITE( NOUT, '(A)')
     &     FWORK(1:INDEX(FWORK,'   ')-1)//
     &   ' for Calpha connecting lines (blanking format)'
      CALL STREAM( MOUT(2))
      OPEN( MOUT(2), FILE= FWORK, STATUS= 'UNKNOWN')
      REWIND( MOUT(2))

C flush output stream
      IF (NOUT.EQ.6) CALL FLUSH6

C write header
      WRITE( MOUT(1), '(A)')
     & '"Ca angle" "Ca disp" "symbol" "label"'

C go through atoms
      DO 50 ACOUNT = 1, ATNO
C Do we have a c alpha?
        IF (ATBRK(ACOUNT)(1:2).EQ.'CA') THEN
C yes!
C is this a different chain from previous output?
C increment symbol type - change of 2 generally produces large change
          IF (ATCHN(ACOUNT).NE.PCHAIN) SYMBNO = SYMBNO + 2

C find the 2D coords of the atom in distance along
C cvect space and angle relative to the closest sphere centre
          CALL DDOT( CVECT, ATXYZ(1,ACOUNT), CDISP)  
          CMIN = 1E10
C closest sphere centre?
          DO 60 SCOUNT = -STRNON, STRNOP
	    CALL DDOT( CVECT, STRCEN(1,SCOUNT), CDISP2)
            IF (ABS(CDISP2-CDISP).LT.CMIN) THEN
              CMIN = ABS(CDISP2-CDISP)
              CNUM = SCOUNT
            ENDIF
60        CONTINUE
C difference vector from the closest centre to the Calpha in question 
          DIFFV(1) = ATXYZ(1,ACOUNT) - STRCEN(1,CNUM) 
          DIFFV(2) = ATXYZ(2,ACOUNT) - STRCEN(2,CNUM) 
          DIFFV(3) = ATXYZ(3,ACOUNT) - STRCEN(3,CNUM) 
C take out the component of this vector along cvect
          CALL DDOT( DIFFV, CVECT, CDOTV)
          DIFFV(1) = DIFFV(1) - CVECT(1)*CDOTV
          DIFFV(2) = DIFFV(2) - CVECT(2)*CDOTV
          DIFFV(3) = DIFFV(3) - CVECT(3)*CDOTV
C unit this
          CALL DUVEC2( DIFFV)
C now find the angle of this to PERPV1
          CALL DDOT( DIFFV, PERPV1, CANGLE)  
          CANGLE = (180./PI)*ACOS(CANGLE)
C cangle is now in the range 0 to 180 degree
          CALL DDOT( DIFFV, PERPV2, RESULT)
          IF (RESULT.LT.0.) CANGLE = -CANGLE
          WRITE( MOUT(1), '(2F12.3,I3,I6,A1)') 
     &       CANGLE, CDISP, SYMBNO, ATRNO(ACOUNT), ATCHN(ACOUNT)

C do lines output
C is this a different chain from previous output?
C and is the residue number one more?
          IF ( (ATCHN(ACOUNT).EQ.PCHAIN) .AND.
     &         (ATRNO(ACOUNT).EQ.(PRNO+1))     )  THEN
C yes
C draw a path
C Q: does the line change by more than 180 degrees
            IF (ABS(PANGLE-CANGLE).LT.180.) THEN
C no - draw normal line
              WRITE( MOUT(2), '(A/ 2F10.3/ 2F10.3)') 
     &          '2 1', 
     &          PANGLE, PDISP, 
     &          CANGLE, CDISP
            ELSE
c yes - go change thru 180/-180 so draw two lines ending at +/-180
              IF (PANGLE.GT.0.) THEN
                WRITE( MOUT(2), '(A/ 2F10.3/ 2F10.3)')
     &          '2 1',
     &          PANGLE, PDISP,
     &          180., 
     &          LINTER( 180D0, PANGLE, PDISP, CANGLE+360., CDISP)
                WRITE( MOUT(2), '(A/ 2F10.3/ 2F10.3)')
     &          '2 1',
     &          -180., 
     &          LINTER( -180D0, PANGLE-360, PDISP, CANGLE, CDISP),
     &          CANGLE, CDISP
              ELSE
                WRITE( MOUT(2), '(A/ 2F10.3/ 2F10.3)')
     &          '2 1',
     &          PANGLE, PDISP,
     &          -180., 
     &          LINTER( -180D0, CANGLE-360, CDISP, PANGLE, PDISP)
                WRITE( MOUT(2), '(A/ 2F10.3/ 2F10.3)')
     &          '2 1',
     &          180., 
     &          LINTER( 180D0, PANGLE+360, PDISP, CANGLE, CDISP),
     &          CANGLE, CDISP
              ENDIF
            ENDIF
          ENDIF
C store residue number and coords
          PCHAIN = ATCHN(ACOUNT)
          PRNO = ATRNO(ACOUNT)
          PANGLE = CANGLE
          PDISP  = CDISP

        ENDIF
C end of going through atoms
50    CONTINUE
C close CA symbol file and blanking file
      CLOSE( MOUT(1))
      CLOSE( MOUT(2))

C make up filename for residue base map - showing regions
C where the surface contact is with a single residue - open to MOUT(2)
      FWORK = F2ROOT(1:LASTC)//'_res.bln'
      WRITE( NOUT, '(A)')
     &     FWORK(1:INDEX(FWORK,'   ')-1)//
     &   ' for contacting residue base map (blanking format)'
      CALL STREAM( MOUT(1))
      OPEN( MOUT(1), FILE= FWORK, STATUS= 'UNKNOWN')
      REWIND( MOUT(1))

C make up filename for the O/N map
      FWORK = F2ROOT(1:LASTC)//'_on_map.grd'
      WRITE( NOUT, '(A)')
     & FWORK(1:INDEX(FWORK,'   ')-1)//
     & ' for map Oxygen/Nitrogen neighbour presence',
     &'  (value 1 for oxygen, -1 for nitrogen and zero for any other)'
      CALL STREAM( MOUT(2))
      OPEN( MOUT(2), FILE= FWORK, STATUS= 'UNKNOWN')
      REWIND( MOUT(2))

C make up filename for residue number map - open to MOUT(3)
      FWORK = F2ROOT(1:LASTC)//'_resno_map.grd'
      WRITE( NOUT, '(A)')
     & FWORK(1:INDEX(FWORK,'   ')-1)//
     & ' for map of residue number'
      CALL STREAM( MOUT(3))
      OPEN( MOUT(3), FILE= FWORK, STATUS= 'UNKNOWN')
      REWIND( MOUT(3))

C make up filename for residue polarity map - open to MOUT(4)
      FWORK = F2ROOT(1:LASTC)//'_polar_map.grd'
      WRITE( NOUT, '(A)')
     & FWORK(1:INDEX(FWORK,'   ')-1)//
     & ' for map of residue polarity ',
     &'  (1 for glu,asp; -1 for lys/arg; +0.1 tyr,ser,his,gln)'
      CALL STREAM( MOUT(4))
      OPEN( MOUT(4), FILE= FWORK, STATUS= 'UNKNOWN')
      REWIND( MOUT(4))


C make up filename for chain id map - open to MOUT(5)
      FWORK = F2ROOT(1:LASTC)//'_chain_map.grd'
      WRITE( NOUT, '(A)')
     & FWORK(1:INDEX(FWORK,'   ')-1)//
     & ' for map of chain id ',
     &'  (number is the offset ascii value for the character',
     &'   i.e. if chain id''s go A, B, C numbers will go 1,2,3)'
      CALL STREAM( MOUT(5))
      OPEN( MOUT(5), FILE= FWORK, STATUS= 'UNKNOWN')
      REWIND( MOUT(5))

C stream mout(6) is an internal hole format file which will
C be used to produce more sophisticated posting maps 
C for by program make_post_map
      FWORK = F2ROOT(1:LASTC)//'_for_make_post_map.hole'
      WRITE( NOUT, '(A)')
     & FWORK(1:INDEX(FWORK,'   ')-1)//
     & ' for file to be used with make_post_map to make',
     &' better post map '
      CALL STREAM( MOUT(6))
      OPEN( MOUT(6), FILE= FWORK, STATUS= 'UNKNOWN')
      REWIND( MOUT(6))

C flush output stream
      IF (NOUT.EQ.6) CALL FLUSH6

C find ends of channel (y-coord)
      CALL DDOT( CVECT, STRCEN(1,STRNOP), CDISP)
      CALL DDOT( CVECT, STRCEN(1,-STRNON), CDISP2)
C header write copied from top of this s/r
C do for all maps - MOUT(2) to MOUT(5)
      DO 77 MCOUNT = 2, 5
        WRITE( MOUT(MCOUNT), '(A)') 'DSAA'
        WRITE( MOUT(MCOUNT), '(2I10)') ADIV, (STRNOP+STRNON+1)
        WRITE( MOUT(MCOUNT), '(A)') '-180.0 180.0'
        WRITE( MOUT(MCOUNT) , '(2F12.5)') CDISP2, CDISP
77    CONTINUE 

C header for special hole file
      WRITE( MOUT(6), '(A)') 'HOLE FOR make_post_map ONLY'
      WRITE( MOUT(6), '(2I10)') ADIV, (STRNOP+STRNON+1)
      WRITE( MOUT(6), '(A)') '-180.0 180.0'
      WRITE( MOUT(6) , '(2F12.5)') CDISP2, CDISP

C need seperate values for each map
C ON map - We will write at 1 for oxygen, -1 for nitrogen and zero for any other
      WRITE( MOUT(2), '(A)') '-1.0 1.0'
C residue number - write first and last residue numbers of molecule
      WRITE( MOUT(3), '(2I10)') ATRNO(1), ATRNO(ATNO)
C residue polarity map from minus one to plus one
      WRITE( MOUT(4), '(A)') '-1.0 1.0'
C chain id use ascii code number of chain id - probable limits between 
C offset for chain number conversion
      CHNOFF = ICHAR(ATCHN(1))-1
      WRITE( MOUT(5), '(2I10)') 1, ICHAR(ATCHN(ATNO))-CHNOFF

C go through stored points from lowest to highest
      DO 11 SCOUNT = -STRNON, STRNOP
C work y value for this line and place in yline
        CALL DDOT( CVECT, STRCEN(1,SCOUNT), YLINE)

C step in angle used
        ANSTEP = 2.*PI/FLOAT(ADIV-1)
C go through angle from -180 to 180
        DO 21 ACOUNT = 1, ADIV
C angle starts -pi goes to +pi
          ANGLE = 2.*PI*FLOAT(ACOUNT-1)/FLOAT(ADIV-1) - PI

C the direction of the point to be considered from the
C sphere centre is given by
          DIRECT(1) = COS(ANGLE)*PERPV1(1) +
     &                SIN(ANGLE)*PERPV2(1)
          DIRECT(2) = COS(ANGLE)*PERPV1(2) +
     &                SIN(ANGLE)*PERPV2(2)
          DIRECT(3) = COS(ANGLE)*PERPV1(3) +
     &                SIN(ANGLE)*PERPV2(3)
C load up point on surface of sphere at this point
          DTOUCH = STRRAD(SCOUNT)
          NEWCEN(1) = DIRECT(1)*DTOUCH + STRCEN(1,SCOUNT)
          NEWCEN(2) = DIRECT(2)*DTOUCH + STRCEN(2,SCOUNT)
          NEWCEN(3) = DIRECT(3)*DTOUCH + STRCEN(3,SCOUNT)
C do single hole calculation
C find energy of point newcen
C - energy is minus radius the largest sphere
C which can be centered at this point without vdW overlap
          CALL HOLEEN( NEWCEN, NEWENG, IAT1, IAT2, IAT3, DAT2, DAT3,
     &                         ATMAX, ATNO, ATXYZ, ATVDW, 10D0)
C
C what we want is info in iat1/iat2 which gives the closest
C and second closest atoms
C if closest atom is a hydrogen then disregard
          IF ( (ATBRK(IAT1)(1:1).EQ.'h') .OR. 
     &         (ATBRK(IAT1)(1:1).EQ.'H')      ) IAT1 = IAT2

C store value of 1 if we have O, -1 if N and zero if other
          IF     ( (ATBRK(IAT1)(1:1).EQ.'o') .OR.
     &             (ATBRK(IAT1)(1:1).EQ.'O')      ) THEN
            RSTORE(ACOUNT,2) = 1.0

          ELSEIF ( (ATBRK(IAT1)(1:1).EQ.'n') .OR.
     &             (ATBRK(IAT1)(1:1).EQ.'N')      ) THEN
            RSTORE(ACOUNT,2) = -1.0

          ELSE
            RSTORE(ACOUNT,2) = 0.0
          ENDIF
C third store for residue number
          RSTORE(ACOUNT,3) = ATRNO(IAT1)
C fourth store for residue polarity
          IF (     (ATRES(IAT1).EQ.'GLU') .OR.
     &             (ATRES(IAT1).EQ.'ASP')      ) THEN
            RSTORE(ACOUNT,4) = +1.0
          ELSEIF ( (ATRES(IAT1).EQ.'ARG') .OR.
     &             (ATRES(IAT1).EQ.'LYS')     ) THEN
            RSTORE(ACOUNT,4) = -1.0
          ELSEIF ( (ATRES(IAT1).EQ.'TYR') .OR.
     &             (ATRES(IAT1).EQ.'HIS') .OR.      
     &             (ATRES(IAT1).EQ.'GLN') .OR.      
     &             (ATRES(IAT1).EQ.'THR') .OR.      
     &             (ATRES(IAT1).EQ.'ASN') .OR.      
     &             (ATRES(IAT1).EQ.'SER')     ) THEN
            RSTORE(ACOUNT,4) = +0.1
          ELSE
            RSTORE(ACOUNT,4) = 0.0
          ENDIF         
C fifth for chain
          RSTORE(ACOUNT,5) = ICHAR(ATCHN(IAT1))-CHNOFF

C also have to use information for residue blanking map
C first store information - residue number and chain id
          RESSTN( 1, ACOUNT) = ATRNO(IAT1)
          RESSTC( 1, ACOUNT) = ATCHN(IAT1)
C do we have a break to the left of this point?
C (ignore first point)
          IF (ACOUNT.GT.1) THEN
            IF ( (RESSTN(1,ACOUNT).NE.RESSTN(1,ACOUNT-1)) .OR.
     &           (RESSTC(1,ACOUNT).NE.RESSTC(1,ACOUNT-1)) ) THEN
C yes - we have a break draw a line in y half way between
C the two points
              WRITE( MOUT(1), '(A/ 2F10.3/ 2F10.3)')
     &  '2 1',
     &  (180./PI)*(ANGLE-0.5*ANSTEP), YLINE+(0.5*SAMPLE),
     &  (180./PI)*(ANGLE-0.5*ANSTEP), YLINE-(0.5*SAMPLE)
            ENDIF
          ENDIF
C do we have a break above the line?
C (ignore first line)
          IF (SCOUNT.GT.-STRNON) THEN
            IF ( (RESSTN(1,ACOUNT).NE.RESSTN(2,ACOUNT)) .OR.
     &           (RESSTC(1,ACOUNT).NE.RESSTC(2,ACOUNT)) ) THEN
C yes - we have a break draw a line in x half way between
C the two points
              WRITE( MOUT(1), '(A/ 2F10.3/ 2F10.3)')
     & '2 1',
     & (180./PI)*(ANGLE-0.5*ANSTEP), 0.5*(YLINE+YOLD),
     & (180./PI)*(ANGLE+0.5*ANSTEP), 0.5*(YLINE+YOLD)
            ENDIF

          ENDIF

C store information for residue number tag.
C first convert angle to degrees
          ANGDEG = (180./PI)*ANGLE
C do we have an existing matching tag?
          DO 309 TCOUNT = 1, CRINUM
            IF ( (ATRNO(IAT1).EQ.CRIRNO(TCOUNT)) .AND.
     &           (ATCHN(IAT1).EQ.CRICHN(TCOUNT))       ) THEN
C have one more match
              CRINST(TCOUNT) = CRINST(TCOUNT) + 1
C store coords y is easy
              CRICOO( 2, TCOUNT) = CRICOO( 2, TCOUNT) + YLINE
C angle is a bit more difficult 
C if the store indicates an average greater than 90 degrees and
C the angle is less than -90 then add 360
              IF ( (CRICOO(1, TCOUNT)/(CRINST(TCOUNT)-1).GT.90.) .AND.
     &             (ANGDEG.LT.-90.)    ) THEN
                CRICOO( 1, TCOUNT) = CRICOO( 1, TCOUNT) + ANGDEG +360.
C or is it the otherway around?
              ELSEIF ((CRICOO(1, TCOUNT)/(CRINST(TCOUNT)-1).LT.-90.)
     &                 .AND. (ANGDEG.GT.90.)    ) THEN
                CRICOO( 1, TCOUNT) = CRICOO( 1, TCOUNT) + ANGDEG -360.
              ELSE
C in same range so simply store
                CRICOO( 1, TCOUNT) = CRICOO( 1, TCOUNT) + ANGDEG
              ENDIF
C have stored the record - jump out of the rest of the procedure
              GOTO 310
            ENDIF
309       CONTINUE
C if have got to here no existing matching residue in store     
C have we exceed maximum number in store
          IF (CRINUM.EQ.CRIMAX) THEN
C only write error once
            CRINUM = CRINUM + 1
            WRITE( NOUT, '(A)')
     &' ERROR in h2dmap',
     &' Maximum number of elements in residue number tag store',
     &' must increase CRIMAX and recompile',
     &' ERROR in h2dmap', ' '
     
          ELSEIF (CRINUM.GT.CRIMAX) THEN
C do nothing
             CONTINUE
          ELSE
C have room so store 
            CRINUM = CRINUM + 1 
            CRIRNO(CRINUM) = ATRNO(IAT1)
            CRICHN(CRINUM) = ATCHN(IAT1)
            CRINST(CRINUM) = 1
            CRICOO(1,CRINUM) = ANGDEG
            CRICOO(2,CRINUM) = YLINE
          ENDIF
C jump to here if there was a match for this record in
C residue number tag store
310       CONTINUE

C for HOLE file simply write out the number of residue and the chain
          WRITE( MOUT(6), '(A1,I6)')   ATCHN(IAT1), ATRNO(IAT1)


C end of going thru angle (acount loop)
21      CONTINUE

C write this row out to all grid files
        DO 87 MCOUNT = 2, 5
          WRITE( MOUT(MCOUNT), '(1000(F9.3,1X))')
     &    (RSTORE(ACOUNT,MCOUNT), ACOUNT=1, ADIV)
87      CONTINUE

C store the y coordinate
        YOLD = YLINE
C store the old residue numbers and chain id's
        DO 22 ACOUNT = 1, ADIV
          RESSTN(2,ACOUNT) = RESSTN(1,ACOUNT)
          RESSTC(2,ACOUNT) = RESSTC(1,ACOUNT)
22      CONTINUE

C end of going thru stored points (scount loop)
11    CONTINUE

C close output files
      DO 8907 MCOUNT = 1, 5
        CLOSE( MOUT(MCOUNT))
8907  CONTINUE

C now write out residue tag information 
C make up filename 
      FWORK = F2ROOT(1:LASTC)//'_resid.dat'
      WRITE( NOUT, '(A)')
     & FWORK(1:INDEX(FWORK,'   ')-1)//' for residue patch identities'
      CALL STREAM( MOUT(1))
      OPEN( MOUT(1), FILE= FWORK, STATUS= 'UNKNOWN')
      REWIND( MOUT(1))

C flush output stream
      IF (NOUT.EQ.6) CALL FLUSH6
      
C write header
      WRITE( MOUT(1), '(A,A)')
     & '"Ca angle" "Ca disp" "symbol" "labRNO"',
     &           ' "labCHN" "labRNOCHN" "labelRNO:CHN"'


C go thru stored tags
      DO 930 TCOUNT = 1, CRINUM
C now write label at the centroid of stored information
C divide stores by number
          CRICOO(1,TCOUNT) = CRICOO(1,TCOUNT)/CRINST(TCOUNT)
          CRICOO(2,TCOUNT) = CRICOO(2,TCOUNT)/CRINST(TCOUNT)
C angle should be between -180 and 180
    	  IF (CRICOO(1,TCOUNT).GT.180.) 
     &      CRICOO(1,TCOUNT) = CRICOO(1,TCOUNT) - 360.
          IF (CRICOO(1,TCOUNT).LT.-180.) 
     &      CRICOO(1,TCOUNT) = CRICOO(1,TCOUNT) + 360.
C finally write label
          WRITE( MOUT(1), '(2F12.3,I3,I6,1X,A1,I6,A1,I6,A1,A1)') 
     &       CRICOO(1,TCOUNT), CRICOO(2,TCOUNT), 
     &       5, CRIRNO(TCOUNT), CRICHN(TCOUNT),
     &          CRIRNO(TCOUNT), CRICHN(TCOUNT),
     &          CRIRNO(TCOUNT), ':', CRICHN(TCOUNT)

930   CONTINUE

C close output file
      CLOSE( MOUT(1))


      RETURN
      END
