      SUBROUTINE HOGRAP( NOUT, SAMPLE, PI,
     &                   STRMAX, STRNOP, STRNON, STRCEN, STRRAD,
     &                   STRLVC, STRLBO, STRSBO, STRBRD,
     &                   ATMAX, ATNO, ATBRK, ATRES,
     &                   ATCHN, ATRNO, ATXYZ, ATVDW, ATBND,
     &                   LSPHBX, SHORTO, POSNO, CVECT, CUTSIZE,
     &			 CONNR, LEXTRA)
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
C * (c) 1997 Oliver Smart                    *
C *                                                                  *
C ********************************************************************
C
C Modification history:
C
C Date	Author		Modification
C 07/94 O.S. Smart      First version
C 11/95 O.S.S.		conductance calc and SHORTO implemented
C 09/96 O.S.S.		Format of output radically changed
C			conductance calc removed (use CAPSULE only)
C 11/96 O.S.S. 		Call to s/r hsurfp added - produces additional
C			information like proportion of surface which 
C			is closest to O at any particular Z.
C 11/96 O.S.S.          Call to helefi to work out electostatic potential
C 12/97 O.S.S.		cut off lists introduced to speed calculations
C			change here is vble cutsize
C 01/00 O.S.S.		Connolly information output and LEXTRA keyword introduce
C 
C This s/r contains code to output data to allow user to plot
C graphs of HOLE's result.  Upto 07/94 included in the main
C program but split of on introducing sphere-box option.


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
C options.
      DOUBLE PRECISION          STRLVC(3,-STRMAX:STRMAX),
     &                          STRLBO(-STRMAX:STRMAX),
     &                          STRSBO(-STRMAX:STRMAX),
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


C pdb format output file 'atoms' are sphere centres
C default to none by setting FPDBSP to 'NONE'
C (commented out - now done in main program)
C      CHARACTER*200		FPDBSP
C      INTEGER			SPDBSP

C logical flag if .true. then spherebox option is turned on
      LOGICAL			LSPHBX

C 11/95 introduce new variable to reduce text output -
C if oshort =
C 0: full text output
C 1: no in run progress reported
C 2: ditto plus no graph type output - only leaving
C    Minimum radius & conductance calculations.
C 3: All text output other than input card mirroring
C    and error messages turned off
      INTEGER                   SHORTO

C position number for write
      INTEGER			POSNO

C channel direction unit vector
      DOUBLE PRECISION		CVECT(3)

C cut off list control vble, -ve: no cutoff lists used in calculation
C otherwise this is the additional radius above the sphere radius
C to be used for cutoff list (see s/r holeen) needed here for calls to 
C holeen
      DOUBLE PRECISION		CUTSIZE

C option CONN introduced 1/00 - 
C CONNR(1) is the connolly surface probe radius (default 1.15Angs)
C          the option is turned off by setting this radius negative
C CONNR(2) is the grid size to be used in the calculation - default .25 angs
C effective pore radius is store in STRBRD
      DOUBLE PRECISION		CONNR(2)

C extra key controls whether information as for 
C Graph of surface properties vs coordinate along cvect
C and crude electrostatic potential
C produced by calls to HSURFP and HELEFI from HOGRAP
      LOGICAL			LEXTRA


C internal vbles ***************

C loop count for going through stored records, one for xyz
      INTEGER			SCOUNT, XCOUNT

C 05/94 add inverse area integration for Mark Sansom -
C just work out running total of sample/(pi*strrad^2)
C 01/00 rename to resistance for that is waht it is - and add conn versions
      DOUBLE PRECISION          TORESI, CTORES

C working real numbers (used to work out distance along channel)
      DOUBLE PRECISION		CDIST, TOCDIS

C the minimum radius found
      DOUBLE PRECISION          MINRAD, CMINR

C need some dummy variables for s/r HOLEEN call
      INTEGER			IAT1, IAT2, IAT3
      DOUBLE PRECISION		DAT2, DAT3
C also a test position and energy
      DOUBLE PRECISION		TSTCEN(3), TSTENG

C string to write numbers to two significant figures,
C the last character of this
      CHARACTER*8		ST2SF
      INTEGER			LST2SF

C coordinate along cvect
      DOUBLE PRECISION		CVCOOR

C average potential - not needed here but has to be included
C in call to HELEFI
      DOUBLE PRECISION		AVGPOT

C macroscopic conductance - and that for connolly
      DOUBLE PRECISION		GMACRO, CGMACR

C if doing connolly - work out average requiv/rspherical
C need this to correct for slabs that have escaped
      DOUBLE PRECISION		CRATIO
      INTEGER			CRATNO

C estimated radius for escaped Connolly sections
      DOUBLE PRECISION		RESTIM

C end of decs ******************


        IF (SHORTO.LT.2) WRITE( NOUT, '(A)')
     &' ',
     &' N.b. Graph information routine (HOGRAP) differs ',
     &'        markedly from previous versions!',
     &'       Information now is written from -ve end',
     &'       to +ve in one pass'

        IF (SHORTO.LT.2) WRITE( NOUT, '(A)')
     &' ',
     &' Explanation of the meaning of each column: ',
     &' First column "cenxyz.cvec" is the coordinate in the',
     &'   direction of the channel vector i.e. if the channel',
     &'   vector is in z (0 0 1) then it is the z coordinate.',
     &' The second column "radius" is the hole pore radius ',
     &'   (as defined in the Biophys J 1993) article).',
     &' The third column "cen_line_D" is the distance ',
     &'   measured along the pore centre line - with the ',
     &'   first point found set to zero. This was the first',
     &'   thing that I used to measure the distance along a pore',
     &'   but "cenxyz.cvec" is a much better general measure. ',
     &'   However, by comparing changes in this distance to changes in ',
     &'   "cenxyz.cvec" one can get a measure of how straight a ',
     &'   channel is.',
     &' The fourth column "sum{s/(area)}" is useful in conductance',
     &'   prediction -',
     &'   basically it is the sum of the thickness of each slab',
     &'   divided by its area. HOLE uses this measure to estimate',
     &'   the macroscopic conductance of a channel.',
     &' If any of this worries you one can see the "raw" results',
     &'   of a standard hole run by producing a sphpdb file and ',
     &'    directly looking at the results.'

C initialize minimum radius 
      MINRAD = 99999.
      CMINR  = 99999.
      TORESI = 0.0 
      CTORES = 0.0 

C n.b. sample must be +ve
      SAMPLE = ABS(SAMPLE)

C if doing connolly - work out average requiv/rspherical
C need this to correct for slabs that have escaped
      IF (CONNR(1).GT.0.) THEN
        CRATIO = 0.
        CRATNO = 0
C go thru all records
        DO 1701 SCOUNT = -STRNON, STRNOP
C if the record has a connolly requiv
          IF (STRBRD(SCOUNT).LT.1E06) THEN
	    CRATIO = CRATIO + STRBRD(SCOUNT)/STRRAD(SCOUNT)
	    CRATNO = CRATNO + 1
	  ENDIF
1701    CONTINUE
        CRATIO = CRATIO/REAL(CRATNO)
        IF (SHORTO.LT.2)  WRITE( NOUT,'(A,F10.3)') 
     & ' Ratio of Connolly Requiv to HOLE spherical probe=', CRATIO

      ENDIF
 
C write column headers
      IF (CONNR(1).LT.0.) THEN
        IF (SHORTO.LT.2)  WRITE( NOUT,'(5A12)') 
     &' cenxyz.cvect',
     &' radius',
     &' cen_line_D',
     &' sum{s/(area)}',
     &' point source'
      ELSE
        IF (SHORTO.LT.2)  WRITE( NOUT,'(8A12)') 
     &' cenxyz.cvect',
     &' radius',
     &' cen.line.dis',
     &' integ.s/(area)',
     &' Requiv',
     &' Requiv_estim',
     &' Conn_s/Area',
     &' point source'
      ENDIF
      
C 18/01/00
C routine is becoming too complex - write out records from
C -strnon to strnop in one shot - previously did 
C 0 to -strnon and 0 to strnop seperately
C this allowed the cen.line.dis to go from zero 
C instead workout offset at the outset!
      TOCDIS = 0.
      DO 1702 SCOUNT = -STRNON+1, 0
C distance to previous point
	CDIST = (STRCEN(1,SCOUNT-1)-STRCEN(1,SCOUNT))**2 +
     &	        (STRCEN(2,SCOUNT-1)-STRCEN(2,SCOUNT))**2 +
     &	        (STRCEN(3,SCOUNT-1)-STRCEN(3,SCOUNT))**2
        CDIST = SQRT(CDIST)
C start TOCDIS -ve such that point zero is regarded as zero
	TOCDIS = TOCDIS - CDIST
1702  CONTINUE

C go thru all points -ve to +ve
      DO 77 SCOUNT= -STRNON, STRNOP
        IF (SCOUNT.EQ.-STRNON) THEN
C if we are at the end cannot look at previous point - there isn't any!
	  CDIST = 0.0
	ELSE
C the distance from point to the previous one
	  CDIST = (STRCEN(1,SCOUNT-1)-STRCEN(1,SCOUNT))**2 +
     &	          (STRCEN(2,SCOUNT-1)-STRCEN(2,SCOUNT))**2 +
     &	          (STRCEN(3,SCOUNT-1)-STRCEN(3,SCOUNT))**2
          CDIST = SQRT(CDIST)
C new feature April 1993
C check the sphere radius of the mid-points
C load the mid-point into newcen
          TSTCEN(1) = 0.5*(STRCEN(1,SCOUNT-1)+STRCEN(1,SCOUNT))
          TSTCEN(2) = 0.5*(STRCEN(2,SCOUNT-1)+STRCEN(2,SCOUNT))
          TSTCEN(3) = 0.5*(STRCEN(3,SCOUNT-1)+STRCEN(3,SCOUNT))
C coordinate of new point
          CALL DDOT( TSTCEN, CVECT, CVCOOR)
C find energy of point newcen
C - energy is minus radius the largest sphere
C which can be centered at this point without vdW overlap
          CALL HOLEEN( TSTCEN, TSTENG, IAT1, IAT2, IAT3, DAT2, DAT3,
     &                 ATMAX, ATNO, ATXYZ, ATVDW, CUTSIZE)
C keep a count of the integrated res
          TORESI = TORESI + 0.5*SAMPLE/(PI*(TSTENG)**2)
C the mid-point will be at distance dum2 + dum/2
C 19/07/94 do not want this info if doing sphere-box
          IF (.NOT.LSPHBX) THEN
            IF (CONNR(1).LT.0.) THEN
              IF (SHORTO.LT.2) WRITE( NOUT,'(4F12.5,A12)') 
     &                 CVCOOR, -TSTENG, 
     &                 (TOCDIS+0.5*CDIST), TORESI, ' (mid-point)'
            ELSE
              IF (SHORTO.LT.2) WRITE( NOUT,'(4F12.5,36X,A12)') 
     &                 CVCOOR, -TSTENG, 
     &                 (TOCDIS+0.5*CDIST), TORESI, ' (mid-point)'
            ENDIF
          ENDIF
C smallest sphere?
          IF  (-TSTENG.LT.MINRAD) MINRAD = -TSTENG
C add the distance between sphere centre scount and previous centre
	  TOCDIS = TOCDIS + CDIST
        ENDIF     

C running resistance - uniform probe uses 1/2 points
        TORESI = TORESI + 0.5*SAMPLE/(PI*(STRRAD(SCOUNT))**2)
C for connolly 1/2 points ignored
C if the radius is set at 1e-06 then have escaped so use approximate requiv
        IF (CONNR(1).GT.0.) THEN
          IF (STRBRD(SCOUNT).GT.0.999E6) THEN
	    RESTIM = CRATIO*STRRAD(SCOUNT)
	  ELSE
	    RESTIM = STRBRD(SCOUNT)
	  ENDIF
          CTORES = CTORES + SAMPLE/(PI*RESTIM*RESTIM)
	ENDIF

C coordinate
        CALL DDOT( STRCEN(1,SCOUNT), CVECT, CVCOOR)

C write out distance along pore centre, radius found
C and x, y, z of pore centre 
C (Oct '93 some people prefer to plot graphs against x coord etc.)
        IF (CONNR(1).LT.0.) THEN
	  IF (SHORTO.LT.2) WRITE( NOUT,'(4F12.5,A12)') 
     &                 CVCOOR, STRRAD(SCOUNT), 
     &                 TOCDIS, TORESI, ' (sampled)'
        ELSE
C connolly version - add connolly equivalent radius and the integrated resistance 
C term
	  IF (SHORTO.LT.2) WRITE( NOUT,'(4F12.5,3F12.3,A12)') 
     & CVCOOR, STRRAD(SCOUNT), TOCDIS, TORESI, 
     &         STRBRD(SCOUNT), RESTIM, CTORES, ' (sampled)'
        ENDIF
        IF  (STRRAD(SCOUNT).LT.MINRAD) MINRAD = STRRAD(SCOUNT)
        IF  (STRBRD(SCOUNT).LT.CMINR)  CMINR  = STRBRD(SCOUNT)

77    CONTINUE


C write out minimum radius if shorto < 3 (3 means no output)
      IF (SHORTO.LT.3) WRITE( NOUT, '(/A,F10.3,A/)' )
     &' Minimum radius found: ', MINRAD, ' angstroms.'
      IF ((SHORTO.LT.3).AND.CONNR(1).GT.0) 
     &                 WRITE( NOUT, '(A,F10.3,A/)' )
     &' Minimum equivR found: ', CMINR, ' angstroms.'

C Area meaningless if radius goes -ve
      IF (MINRAD.LT.0.) TORESI = 1D8
      
C write out predicted conductivity
      IF (SHORTO.LT.3) WRITE( NOUT, '(//A/A,F8.3,A)')
     &' For spherical probe approach, The geometric factor',
     &'         F= sum(ds/area) along channel is ', TORESI,
     &                                          ' angstroms**-1'
C write out predicted conductances to two signicant figures
      CALL SF2( ST2SF, LST2SF, 100D0/TORESI)
      IF (SHORTO.LT.3) WRITE( NOUT, '(A)')
     &' This yields a macroscopic predicted molar conductance of:',
     &'   (1/rho)*(100/F)= ('//ST2SF(1:LST2SF)//'/rho) pS,',
     &'   where rho is the conductivity of 1M permeant ion in ohm m.',
     &' N.B. this is a macroscopic conductance - an overestimate ',
     &' Empirically corrected conductances currently only available',
     &' With the CAPSULE option (although its not that stable!)'

      GMACRO = 1200D0/TORESI
      CALL SF2( ST2SF, LST2SF, GMACRO)
      IF (SHORTO.LT.3) WRITE( NOUT, '(A,A,A)')
     &' For 1M KCl rho= 1/12 (ohm m), So Gmacro= ',
     &                  ST2SF(1:LST2SF), ' pS/M.'
C do same for connolly numbers
      IF (CONNR(1).GT.0.) THEN
        IF (SHORTO.LT.3) WRITE( NOUT, '(//A/A,F8.3,A)')
     &' For Connolly-based approach, The geometric factor',
     &'         F= sum(ds/area) along channel is ', CTORES,
     &                                          ' angstroms**-1'
C write out predicted conductances to two signicant figures
        CALL SF2( ST2SF, LST2SF, 100D0/CTORES)
        IF (SHORTO.LT.3) WRITE( NOUT, '(A)')
     &' This yields a macroscopic predicted molar conductance of:',
     &'   (1/rho)*(100/F)= ('//ST2SF(1:LST2SF)//'/rho) pS,',
     &'   where rho is the conductivity of 2M permeant ion in ohm m.',
     &' N.B. this is a macroscopic conductance - an overestimate ',
     &' Empirically corrected conductances currently only available',
     &' With the CAPSULE option (although its not that stable!)'

        CGMACR = 1200D0/CTORES
        CALL SF2( ST2SF, LST2SF, CGMACR)
        IF (SHORTO.LT.3) WRITE( NOUT, '(A,A,A)')
     &' For 1M KCl rho= 1/12 (ohm m), So Conn_Gmacro= ',
     &                  ST2SF(1:LST2SF), ' pS/M.' 
      ELSE
C blank out detailed numbers
        CGMACR = -1.
      ENDIF
C need to write out detailed numbers
      IF (SHORTO.LT.3) WRITE( NOUT, '(A,I6,3(A,F12.5),A)')
     &'  (TAG ', POSNO,
     &  '   Rmin=', MINRAD,
     &  '   Gmacro=', GMACRO, ' Conn_Gmacro=', CGMACR, ' ).'

C sphere box option - first bodge is just to write data at the
C points worked out (leave code above)
      IF (LSPHBX) THEN
        IF (SHORTO.LT.2) WRITE( NOUT, '(A)')
     &' ',
     &' Additional sphere-box data: ',
     &' x, y & z of centre, longbox, shortbox, boxrad'
        DO 40 SCOUNT= -STRNON, STRNOP
          IF (SHORTO.LT.2) WRITE( NOUT,'(6F12.5,A)')
     &      (STRCEN(XCOUNT,SCOUNT), XCOUNT=1, 3),
     &      STRLBO(SCOUNT), STRSBO(SCOUNT), STRBRD(SCOUNT)
40      CONTINUE
      ENDIF

C additional info from hsurfp?
      IF (LEXTRA) THEN 
        CALL HSURFP( NOUT, CVECT, 
     &             STRMAX, STRNOP, STRNON, STRCEN, STRRAD,
     &             SHORTO, SAMPLE, PI,
     &             ATMAX, ATNO, ATBRK, ATRES,
     &             ATCHN, ATRNO, ATXYZ, ATVDW, ATBND, CUTSIZE)

C 11 Nov 96 - work out the electrostatic potential created along
C pore centre line by formal charges in the system
C s/r should work both with the capsule and normal call -
C hence double ref to STRCEN (capsule has two centres).
        CALL HELEFI( NOUT, AVGPOT, CVECT, SHORTO,
     &             STRMAX, STRNOP, STRNON, STRCEN, STRCEN,
     &             ATMAX, ATNO, ATBRK, ATRES,
     &             ATCHN, ATRNO, ATXYZ, ATVDW, ATBND)
      ENDIF
C return here
      RETURN
      END
