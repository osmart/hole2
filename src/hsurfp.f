      SUBROUTINE HSURFP( NOUT, CVECT, 
     &                 STRMAX, STRNOP, STRNON, STRCEN, STRRAD,
     &                 SHORTO, SAMPLE, PI,
     &                 ATMAX, ATNO, ATBRK, ATRES,
     &                 ATCHN, ATRNO, ATXYZ, ATVDW, ATBND, CUTSIZE)
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
C 11/96 O.S. Smart      First version
C 12/97 O.S.S.		cut off lists introduced to speed calculations
C			change here is vble cutsize
C 
C 5/11/96 Routine to calculate property graphs i.e. to find as function of
C coord along cvect the % of surface closest to an oxygen atom.
C Adapted from s/r H2DMAP which does a very similar thing - work out
C 2D property maps.  

C passed variables *************

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

C cut off list control vble, -ve: no cutoff lists used in calculation
C otherwise this is the additional radius above the sphere radius
C to be used for cutoff list (see s/r holeen) needed here for calls to 
C holeen
      DOUBLE PRECISION		CUTSIZE


C internal vbles ***************

C unit vectors used to define rotation
      DOUBLE PRECISION		PERPV1(3), PERPV2(3)

C dot product between cvect and perpv1
      DOUBLE PRECISION		CDOTP

C max// number of divisions to make in the angle
C (73 for 5 degrees)
      INTEGER			AMAX, ADIV
      PARAMETER(		AMAX= 140)
	
C loop counts angles, positions
      INTEGER			ACOUNT, SCOUNT

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

C y value for a line 
      DOUBLE PRECISION		YLINE

C vbles introduced in writing hsurfp
C proportion of surface contacted by
C oxygen, nitrogen, oxygen or nitrogen, other.
C acid residue, base type residue, polar type residue,
C acid base or polar type residue, other (non-polar) type
C residue. 
      INTEGER			PPO,
     &				PPN,
     &				PPOorN,
     &				PPOTH,
     &				RACID,
     &				RBASE,
     &				RAorB,
     &				RPOLAR,
     &				RABorP,
     &				RNONPO

C end of decs ******************

C do nothing if SHORTO set above 2
      IF (SHORTO.GT.2) GOTO 55555

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
      WRITE( NOUT, '(A)')
     &' S/r HSURFP will output information to draw ',
     &'  Graph of surface properties vs coordinate along cvect',
     &'  A residue is regarded as',
     &'      Acid if it has id of: GLU or ASP;',
     &'                     id of: ARG or LYS;',
     &'                     id of: HIS, HID, HIE, GLN,', 
     &'                                 THR, ASN or SER.',
     &' (This list is hard-coded in s/r HSURFP)'
     
C column titles
      WRITE( NOUT, '(20A10)') 
     &    'cvect_coord', 'radius', 
     &    'prop_O',     'prop_N',
     &    'prop_OorN',  'pro_other',
     &    'prop_Acid',  'prop_Base',
     &    'prop_AorB',  'pro_Polar',
     &    'pro_ABorP',  'prop_nonP' 

C flush output stream 
      IF (NOUT.EQ.6) CALL FLUSH6

C go through stored points from lowest to highest
      DO 11 SCOUNT = -STRNON, STRNOP
C work y value for this line and place in yline
        CALL DDOT( CVECT, STRCEN(1,SCOUNT), YLINE)

C zero the counts
        PPO = 0
        PPN = 0
        PPOorN = 0
        PPOTH = 0
        RACID = 0
        RBASE = 0
        RAorB = 0
        RPOLAR = 0
        RABorP = 0
        RNONPO = 0

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
C put in fixed large cutsize
          CALL HOLEEN( NEWCEN, NEWENG, IAT1, IAT2, IAT3, DAT2, DAT3,
     &                         ATMAX, ATNO, ATXYZ, ATVDW, 5D0*CUTSIZE)
C what we want is info in iat1/iat2 which gives the closest
C and second closest atoms
C if closest atom is a hydrogen then disregard
          IF ( (ATBRK(IAT1)(1:1).EQ.'h') .OR. 
     &         (ATBRK(IAT1)(1:1).EQ.'H')      ) IAT1 = IAT2

C store value of 1 if we have O, -1 if N and zero if other
          IF     ( (ATBRK(IAT1)(1:1).EQ.'o') .OR.
     &             (ATBRK(IAT1)(1:1).EQ.'O')      ) THEN
            PPO = PPO + 1
            PPOorN = PPOorN + 1

          ELSEIF ( (ATBRK(IAT1)(1:1).EQ.'n') .OR.
     &             (ATBRK(IAT1)(1:1).EQ.'N')      ) THEN
            PPN = PPN + 1
            PPOorN = PPOorN + 1

          ELSE
C other 
            PPOTH = PPOTH + 1

          ENDIF

C look at residue polarity
          IF (     (ATRES(IAT1).EQ.'GLU') .OR.
     &             (ATRES(IAT1).EQ.'ASP')      ) THEN
            RACID  = RACID + 1
            RAorB  = RAorB + 1
            RABorP = RABorP + 1 

          ELSEIF ( (ATRES(IAT1).EQ.'ARG') .OR.
     &             (ATRES(IAT1).EQ.'LYS')     ) THEN
            RBASE =  RBASE + 1
            RAorB  = RAorB + 1
            RABorP = RABorP + 1 
            
          ELSEIF ( (ATRES(IAT1).EQ.'TYR') .OR.
     &             (ATRES(IAT1).EQ.'HIS') .OR.      
     &             (ATRES(IAT1).EQ.'HID') .OR.      
     &             (ATRES(IAT1).EQ.'HIE') .OR.      
     &             (ATRES(IAT1).EQ.'GLN') .OR.      
     &             (ATRES(IAT1).EQ.'THR') .OR.      
     &             (ATRES(IAT1).EQ.'ASN') .OR.      
     &             (ATRES(IAT1).EQ.'SER')     ) THEN
C not acid or base but polar
            RPOLAR = RPOLAR + 1
            RABorP = RABorP + 1 
          ELSE
C other - non polar
            RNONPO = RNONPO + 1

          ENDIF         

C end of going thru angle (acount loop)
21      CONTINUE

C write out information
        WRITE( NOUT, '(20F10.4)')
     &    YLINE,                     STRRAD(SCOUNT), 
     &    FLOAT(PPO)/FLOAT(ADIV),    FLOAT(PPN)/FLOAT(ADIV),
     &    FLOAT(PPOorN)/FLOAT(ADIV), FLOAT(PPOTH)/FLOAT(ADIV),
     &    FLOAT(RACID)/FLOAT(ADIV),  FLOAT(RBASE)/FLOAT(ADIV),
     &    FLOAT(RAorB)/FLOAT(ADIV),  FLOAT(RPOLAR)/FLOAT(ADIV),
     &    FLOAT(RABorP)/FLOAT(ADIV), FLOAT(RNONPO)/FLOAT(ADIV)

C end of going thru stored points (scount loop)
11    CONTINUE

C return here      
55555 CONTINUE
C flush output stream
      IF (NOUT.EQ.6) CALL FLUSH6

      RETURN
      END
