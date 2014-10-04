      SUBROUTINE HOLCAL( NIN, NOUT, LERR, CPOINT, CVECT, SAMPLE,
     &                   MCSTEP, MCKTIN, MCLEN,
     &			 ATMAX, ATNO, ATBRK, ATRES, ATCHN, ATRNO,
     &			 ATXYZ, ATVDW, ATBND, ATRAD2,
     &                   PI, LDBUG, SHYDRA, FHYDRA, SPDBSP, FPDBSP, 
     &			 LLINE, LCAPS, LSPHBX, SHORTO,
     &			 STRMAX, STRNOP, STRNON, STRCEN, STRRAD, 
     &			 STRLVC, STRLBO, STRSBO, STRBRD,
     &			 ENDRAD, CUTSIZE, CONNR, PERPVE, PERPVN,
     &			 PEG_WRITEALL)
      IMPLICIT NONE
C ********************************************************************
C *                                                                  *
C * This software is an unpublished work containing confidential and *
C * proprietary information of Birkbeck College. Use, disclosure,    *
C * reproduction and transfer of this work without the express       *
C * written consent of Birkbeck College are prohibited. This notice  *
C * must be attached to all copies or extracts of the software.      *
C *                                                                  *
C * (c) 1995 Oliver Smart & Birkbeck College,                        *
C * All rights reserved                                              *
C * (c) 1996 Oliver Smart & Birkbeck College, All rights reserved    *
C * (c) 1997 Oliver Smart                    *
C ********************************************************************
C
C Modification history:
C
C Date  Author          Modification
C 11/95 O.S. Smart      Original version
C 15/11/95 split off HOLE calculation into seperate s/r
C 10/97 O.S. Smart      CAPSULE WARNING changed to include 'plain english'
C 12/97 O.S.S.		cut off lists introduced to speed calculations
C			change here is vble cutsize
C 01/00 O.S.S.    	CONN option introduced
C

C passed vbles *****************

C input/output stream numbers
      INTEGER			NIN, NOUT

C error indicator
      LOGICAL			LERR

C an initial point in the channel
C (must be specified)
      DOUBLE PRECISION          CPOINT(3)

C a vector in the direction of the channel
C (must be specified)
      DOUBLE PRECISION          CVECT(3)

C a sampling distance - the distance between planes
C default to 0.5 angs
      DOUBLE PRECISION          SAMPLE

C the number of steps to be taken
C on each Monte Carlo application
      INTEGER                   MCSTEP

C max. length in angs of each Montecarlo displacement (angs),
C divison term in Monte Carlo probability (also angs)
C starting value and current value
      DOUBLE PRECISION          MCLEN, MCKTIN

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

C 16/6/94 - spherebox option.
C s/r ****** needs vble ATRAD as store of the
C radius squared between the centre of boxsphere and
C each atom to avoid unnecessary calc
      DOUBLE PRECISION          ATRAD2( ATMAX)

C will need PI
      DOUBLE PRECISION          PI

C logical vairable - produce debug output
      LOGICAL                   LDBUG

C hydra binary plot output file, a stream for it
C default to none - indicated by setting FHYDRA(1:4) to 'NONE'
      CHARACTER*200             FHYDRA
      INTEGER                   SHYDRA

C pdb format output file 'atoms' are sphere centres
C default to none by setting FPDBSP to 'NONE'
      CHARACTER*200             FPDBSP
      INTEGER                   SPDBSP

C vbles to turn on the sphere-box, capsule options
      LOGICAL			LSPHBX, LCAPS

C flags to turn on/off:
C the writing of lines joining sphere surfaces to the two closest atoms,
      LOGICAL			LLINE

C 11/95 introduce new variable to reduce text output -
C if oshort =
C 0: full text output
C 1: no in run progress reported
C 2: ditto plus no graph type output - only leaving
C    Minimum radius & conductance calculations.
C 3: All text output other than input card mirroring
C    and error messages turned off
      INTEGER                   SHORTO

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
C (N.B. for capsule strlvc is the second centre, strbrd capsule radius)
      DOUBLE PRECISION          STRLVC(3,-STRMAX:STRMAX),
     &                          STRLBO(-STRMAX:STRMAX),
     &                          STRSBO(-STRMAX:STRMAX),
     &                          STRBRD(-STRMAX:STRMAX)

C New option 22/11/93 - For wide channels need to
C be able to specify the radius at which an end is reached
      DOUBLE PRECISION          ENDRAD


C option CONN introduced 1/00 - 
C CONNR(1) is the connolly surface probe radius (default 1.15Angs)
C          the option is turned off by setting this radius negative
C CONNR(2) is the grid size to be used in the calculation - default .85 angs
      DOUBLE PRECISION		CONNR(2)

C unit vectors in plane normal to cvect
C needed to a number of routines - originally h2dmap but now conn
C and addend
C these are normal - first is east direction (x on plane)
C                    and second is north     (y on plane)
C return unchanged!!!
      DOUBLE PRECISION		PERPVE(3), PERPVN(3)

C PEG_WRITEALL: extra key for new separate PEG calculation - program peg_calc
C turn on with FORPEG card in control file
C this vble is to be acted on in s/r CONCAL making it 
C write all the spheres found to the .sph file without any editing       
      LOGICAL			PEG_WRITEALL

C internal variables ***********

C the current sphere centre,
C the energy of this point - energy is minus the largest sphere
C which can be centered at this point without vdW overlap
      DOUBLE PRECISION          CURCEN(3), CURENG
      INTEGER                   CURSTP

C the lowest energy point found, step number
      DOUBLE PRECISION          LOWCEN(3), LOWENG
      INTEGER                   LOWSTP

C a new point
      DOUBLE PRECISION          NEWCEN(3), NEWENG

C 17/6/94
C variables for lsphbx option
C ***LVC is the long axis vector
C ***LBO is the box dimension of the long side (direction ***LRD)
C ***SBO is the box dimension of the short side
C        n.b. the other dimension along CVECT is set to the long value ***LBO
C ***BRD is the spherebox radius
      DOUBLE PRECISION          CURLVC(3), NEWLVC(3), LOWLVC(3),
     &                          CURLBO,    NEWLBO,    LOWLBO,
     &                          CURSBO,    NEWSBO,    LOWSBO,
     &                          CURBRD,    NEWBRD,    LOWBRD

C Monte Carlo acceptance factor, starting value MCKTIN 
      DOUBLE PRECISION          MCKT

C cut off list control vble, -ve: no cutoff lists used in calculation
C otherwise this is the additional radius above the sphere radius
C to be used for cutoff list (see s/r holeen)
      DOUBLE PRECISION		CUTSIZE

C loop counts
      INTEGER                   ICOUNT

C number of rejections
      INTEGER                   NREJ
C atom list no. with smallest dist-vdw radius, 2nd smallest
C 07/06/94 as iat's may or may not be supplied with previously 
C found numbers then we can use this to speed up the procedure
C Dec 97 add iat3 
      INTEGER			IAT1, IAT2, IAT3

C 2nd/3rd smallest distance-vdw radius (of iat2/3)
      DOUBLE PRECISION		DAT2, DAT3

C a real number, a probability, the random number to test it
      DOUBLE PRECISION          HGOOD, PROB, RANDYN

C record number
      INTEGER                   IREC

C number of steps of sd min apply in spherebox option
      LOGICAL                   NOSTEP

C equivalent radius for connolly routine
      DOUBLE PRECISION		REQUIV

C end of decs ******************

! if we are doing PEG_WRITEALL then need to output vital channel statistics
! to top of sph file
      IF (PEG_WRITEALL) 
     &   CALL PEG_WRITEALL_HEADER( FPDBSP, SPDBSP, SAMPLE, 
     &		    		   CVECT, PERPVE, PERPVN,
     &				   CONNR, ENDRAD )


C the initial sphere centre is cpoint
      LOWCEN(1) = CPOINT(1)
      LOWCEN(2) = CPOINT(2)
      LOWCEN(3) = CPOINT(3)
C if doing capsule option the second point starts at the same point
      IF (LCAPS) THEN
        LOWLVC(1) = LOWCEN(1)
        LOWLVC(2) = LOWCEN(2)
        LOWLVC(3) = LOWCEN(3)
      ENDIF

C line 10 - do until the end of channel is found
10    CONTINUE

C 20/11/95 flush the output stream
        IF (NOUT.EQ.6) CALL FLUSH6


C do MCSTEP steps of Monte Carlo
      	DO 20 ICOUNT = 1, MCSTEP

C on first step new point is LOWCEN
          IF (ICOUNT.EQ.1) THEN
            NEWCEN(1) = LOWCEN(1)
            NEWCEN(2) = LOWCEN(2)
            NEWCEN(3) = LOWCEN(3)
C if doing capsule option then the second centre must also be set
            NEWLVC(1) = LOWLVC(1)
            NEWLVC(2) = LOWLVC(2)
            NEWLVC(3) = LOWLVC(3)

C the 'energy' is initially high
C so that first point is guaranteed to be accepted
            CURENG = 1D20
            LOWENG = 1D20
C temperature starts at MCKTIN
            MCKT = MCKTIN
      	    NREJ = 0
C for spherebox option then the long vector starts randomly
            IF (LSPHBX) THEN
              LOWLVC(1) = 0.
              LOWLVC(2) = 0.
              LOWLVC(3) = 0.
            ENDIF
          ELSE
C 27/05/94 put finding new point NEWCEN on the plane through
C CURCEN defined by channel vector CVECT with a maximum
C displacement from CURCEN of MCLEN into s/r HONEWP
            CALL HONEWP( NEWCEN, CURCEN, CVECT, MCLEN)
C find second random new point - put into newlvc
            IF (LCAPS) CALL HONEWP( NEWLVC, CURLVC, CVECT, MCLEN)
      	  ENDIF

C if we are doing the spherebox option we also need a new
C long vector NEWLVC N.B. this must be orthogonal to cvect
C and is biased towards LOWLVC (the vector foun on previous step)
          IF (LSPHBX) THEN
            CALL HONEWV( NEWLVC, LOWLVC, CVECT)
C 17/6/94 - spherebox option.
C Fit spherebox with the largest are normal to cvect onto point
C NEWENG is minus the 'radius' of the spherebox -
C that is minus the root of the area divided by pi      
            CALL HSBXEN( NEWCEN, NEWENG, IAT1, IAT2, DAT2,
     &        ATMAX, ATNO, ATXYZ, ATVDW, 
     &        ATRAD2, CVECT, NEWLVC, NEWLBO, NEWSBO, NEWBRD,
     &        NOUT, LERR, PI)
            IF (LERR) THEN
              WRITE( NOUT, *) 'ERROR has been found on call to '
              WRITE( NOUT, *) '  HSBXEN from HOLE (1)'
              WRITE( NOUT, *) '  Aborting ***********'
              GOTO 55555
            ENDIF
C or are we doing the capsule option?
          ELSEIF (LCAPS) THEN
            CALL HCAPEN( NEWCEN, NEWENG, NEWLVC, NEWBRD,
     &		 IAT1, IAT2, DAT2,
     &           ATMAX, ATNO, ATXYZ, ATVDW, PI)
           if (ldbug) then
	     write( nout, *) 'call to hcapen icount= ', icount
	     write( nout, *) 'newcen=', newcen
	     write( nout, *) 'newlvc=', newlvc
             write( nout, *) 'neweng= ',neweng
           endif
          ELSE
C NOT doing spherebox or capsule option
C find energy of point newcen
C - energy is minus radius the largest sphere 
C which can be centered at this point without vdW overlap
C iat1 and iat2 are the atom numbers of the nearest two atoms
C to the centre and DAT2 is the distance - vdw  radius for the 2nd atom
            CALL HOLEEN( NEWCEN, NEWENG, IAT1, IAT2, IAT3, DAT2, DAT3,
     &           ATMAX, ATNO, ATXYZ, ATVDW, CUTSIZE)

          ENDIF


C lowest energy found?
          IF ( NEWENG .LT. LOWENG ) THEN
      	    LOWSTP    = ICOUNT
            LOWENG    = NEWENG
      	    LOWCEN(1) = NEWCEN(1)
      	    LOWCEN(2) = NEWCEN(2)
      	    LOWCEN(3) = NEWCEN(3)
            if (ldbug) write(nout,*) 'debug lowest energy'
C if doing spherebox then have to store long vector,
C boxdim's & sphere radius.
            IF (LSPHBX) THEN
              LOWLVC(1) = NEWLVC(1)
              LOWLVC(2) = NEWLVC(2)
              LOWLVC(3) = NEWLVC(3)
              LOWLBO    = NEWLBO
              LOWSBO    = NEWSBO
              LOWBRD    = NEWBRD
            ELSEIF (LCAPS) THEN
C capsule -store second point
              LOWLVC(1) = NEWLVC(1)
              LOWLVC(2) = NEWLVC(2)
              LOWLVC(3) = NEWLVC(3)
            ENDIF
              
      	  ENDIF

C accept this point?
      	  HGOOD = NEWENG - CURENG
C energy down?
          IF (HGOOD.LT.0.0) THEN
C accept
      	    CURSTP    = ICOUNT
            CURENG    = NEWENG
      	    CURCEN(1) = NEWCEN(1)
      	    CURCEN(2) = NEWCEN(2)
      	    CURCEN(3) = NEWCEN(3)

C if doing spherebox then have to store long vector,
C boxdim's & sphere radius.
            IF (LSPHBX) THEN
              CURLVC(1) = NEWLVC(1)
              CURLVC(2) = NEWLVC(2)
              CURLVC(3) = NEWLVC(3)
              CURLBO    = NEWLBO
              CURSBO    = NEWSBO
              CURBRD    = NEWBRD
            ELSEIF (LCAPS) THEN
C capsule -store second point
              CURLVC(1) = NEWLVC(1)
              CURLVC(2) = NEWLVC(2)
              CURLVC(3) = NEWLVC(3)
            ENDIF

C debug output
	   if (ldbug) then
              write(nout,*) 'debug accept lower   step', icount
              write(nout,*)'newcen ',real(newcen(1)),real(newcen(2)),
     &				     real(newcen(3))
              write(nout,*)'cureng ', real(cureng)
              write(nout,*)'loweng ', real(loweng)
              write(nout,*)'at1 '//atbrk(iat1),atrno(iat1),-real(neweng)
              write(nout,*)'at2 '//atbrk(iat2),atrno(iat2), real(dat2)
              write(nout,*)'mckt ', real(mckt)
              if (lsphbx) then
                write(nout,*) 'newlvc ', newlvc
                write(nout,*) 'newlbo ', newlbo
                write(nout,*) 'newsbo ', newsbo
                write(nout,*) 'newbrd ', newbrd
              endif
      	      write(nout,*)
            endif

          ELSE
C accept according to a random number
            IF (MCKT.GT.0) THEN
C probablity that this will be accepted
      	      PROB = EXP(-HGOOD/MCKT)
C find random number between 0 and 1
      	      CALL DRAND( RANDYN)
      	      IF (RANDYN.LT.PROB) THEN
C accept
      	        CURSTP    = ICOUNT
                CURENG    = NEWENG
      	        CURCEN(1) = NEWCEN(1)
       	        CURCEN(2) = NEWCEN(2)
      	        CURCEN(3) = NEWCEN(3)
C if doing spherebox then have to store long vector,
C boxdim's & sphere radius.
                IF (LSPHBX) THEN
                  CURLVC(1) = NEWLVC(1)
                  CURLVC(2) = NEWLVC(2)
                  CURLVC(3) = NEWLVC(3)
                  CURLBO    = NEWLBO
                  CURSBO    = NEWSBO
                  CURBRD    = NEWBRD
                ELSEIF (LCAPS) THEN
C capsule -store second point
                  CURLVC(1) = NEWLVC(1)
                  CURLVC(2) = NEWLVC(2)
                  CURLVC(3) = NEWLVC(3)
                ENDIF


            if (ldbug) then
              write(nout,*) 'debug accept due to prob step', icount
              write(nout,*) 'prob rand', real(prob), real(randyn)
              write(nout,*)'newcen ',real(newcen(1)),real(newcen(2)),
     &				     real(newcen(3))
              write(nout,*)'cureng ', real(cureng)
              write(nout,*)'loweng ', real(loweng)
            write(nout,*)'at1 '//atbrk(iat1),atrno(iat1),-real(neweng)
            write(nout,*)'at2 '//atbrk(iat2),atrno(iat2), real(dat2)
              write(nout,*)'mckt ', real(mckt)
              if (lsphbx) then
                write(nout,*) 'newlvc ', newlvc
                write(nout,*) 'newlbo ', newlbo
                write(nout,*) 'newsbo ', newsbo
                write(nout,*) 'newbrd ', newbrd
              endif
      	      write(nout,*)
            endif

C reject due to prob
              ELSE 
      		NREJ = NREJ + 1
              ENDIF
C reject as temperature is zero
            ELSE 
      	      NREJ = NREJ + 1
            ENDIF
          ENDIF

C step the temperature down - down to zero at 9/10's of the step
          MCKT = MCKT - MCKTIN/(0.9*REAL(MCSTEP))
      	  IF (MCKT.LT.0) MCKT = 0.0

C finished MCSTEPS
20      CONTINUE

C 21/6/94 - problem when doing spherebox option
C is that degree of refinement due to Monte Carlo
C is insufficient so further refine lowest energy solution
C using conventional minimization.
        IF (LSPHBX) THEN
C tell user results before minimization
          IF (SHORTO.LT.1) WRITE( NOUT, '(A/ A,F8.4, 3(/A,3F8.3) )')
     &' Applying sd min to lowest energy position found.',
     &' Before min - energy (effective radius)= ', LOWENG,
     &' Centre= ', LOWCEN,
     &' Long vector= ', LOWLVC,
     &' long box length, short box length, box radius ', 
     &  LOWLBO, LOWSBO, LOWBRD
          CALL HSBXMI( LOWCEN, LOWENG, IAT1, IAT2, DAT2,
     &        ATMAX, ATNO, ATXYZ, ATVDW,
     &        ATRAD2, CVECT, LOWLVC, LOWLBO, LOWSBO, LOWBRD,
     &        NOUT, LERR, PI, NOSTEP, ENDRAD)
          IF (LERR) THEN
            WRITE( NOUT, *) 'ERROR has been found on call to '
            WRITE( NOUT, *) '  HSBXMI from HOLE (1)'
            WRITE( NOUT, *) '  Aborting ***********'
            GOTO 55555
          ENDIF
        ENDIF


C tell user lowest energy point = highest radius
C first find closest atoms
C if doing capsule use special option
        IF (LCAPS) THEN
          CALL HCAPEN( LOWCEN, NEWENG, LOWLVC, LOWBRD, IAT1, IAT2, DAT2,
     &           ATMAX, ATNO, ATXYZ, ATVDW, PI)
        ELSE
          CALL HOLEEN( LOWCEN, NEWENG, IAT1, IAT2, IAT3, DAT2, DAT3,
     &         ATMAX, ATNO, ATXYZ, ATVDW, CUTSIZE)
        ENDIF
        IF (SHORTO.LT.1) WRITE(NOUT,'(A, /A,3F8.3,
     &        2(/A,F8.3,3X,A4,A3,1X,A1,I5) /A,I5)' ) 
     & ' highest radius point found:',
     & '  at point ', LOWCEN,
     & '  closest atom surface ', -LOWENG, 
     &     ATBRK(IAT1), ATRES(IAT1), ATCHN(IAT1), ATRNO(IAT1),
     & '  2nd closest surface  ', DAT2, 
     &     ATBRK(IAT2), ATRES(IAT2), ATCHN(IAT2), ATRNO(IAT2),
     & '  found on step', LOWSTP
C tell user current point if different
        IF (CURENG.EQ.LOWENG) THEN
      	  IF (SHORTO.LT.1) write(nout,*) ' this is also current point'
        ELSE
          IF (LCAPS) THEN
            CALL HCAPEN( CURCEN, NEWENG, CURLVC, NEWBRD, 
     &		         IAT1, IAT2, DAT2, ATMAX, ATNO, ATXYZ, ATVDW, PI)
          ELSE
            CALL HOLEEN( CURCEN, NEWENG, IAT1, IAT2, IAT3, DAT2, DAT3, 
     &         ATMAX, ATNO, ATXYZ, ATVDW, CUTSIZE)
          ENDIF
          IF (SHORTO.LT.1) WRITE(NOUT,'(A, /A,3F8.3,
     &       2(/A,F8.3,3X,A4,A3,1X,A1,I5) /A,I5)' ) 
     & ' current point:',
     & '  at point ', CURCEN,
     & '  closest atom surface ', -CURENG, 
     &     ATBRK(IAT1), ATRES(IAT1), ATCHN(IAT1), ATRNO(IAT1),
     & '  2nd closest surface  ', DAT2, 
     &     ATBRK(IAT2), ATRES(IAT2), ATCHN(IAT2), ATRNO(IAT2),
     & '  found on step', CURSTP
        ENDIF

        IF (LSPHBX) THEN
C tell user extra bits of info for spherebox
          IF (SHORTO.LT.1) WRITE( NOUT, '(A,I5, 2(/ A,3F8.3) )')
     &' Number of steps of sd applied ', NOSTEP,
     &' Long vector= ', LOWLVC,
     &' long box length, short box length, box radius ',
     &  LOWLBO, LOWSBO, LOWBRD

        ELSEIF (LCAPS) THEN
C tell user extra bits of info for capsule option
          IF (SHORTO.LT.1) WRITE( NOUT, '(A,3F8.3/ A,F8.3/ A,F8.3/ A)') 
     &' Capsule option - second centre ', LOWLVC,
     &'                  distance between centres ',
     &     SQRT( (LOWCEN(1)-LOWLVC(1))**2 +
     &           (LOWCEN(2)-LOWLVC(2))**2 +
     &           (LOWCEN(3)-LOWLVC(3))**2 ),
     &' Capsule radius of lowest point ', LOWBRD,
     &'              (other radii quoted are effective)'
        ENDIF

C tell user no of rejected points
        IF (SHORTO.LT.1) WRITE(NOUT,*) ' no. of rejections ', NREJ

C have we reached the end of the channel?
C - if so radius will be high
C 22/11/93 - allow user to specify the radius at which the
C end of channel is found as ENDRAD
        IF (-LOWENG.LT.ENDRAD) THEN

	  IF (SAMPLE.GT.0) THEN
C have one more centre intial point goes to
C zero as strnop starts at -1
	    STRNOP = STRNOP + 1
	    IF (STRNOP.GT.STRMAX) THEN
	      WRITE(NOUT,*) 'ERROR'
	      WRITE(NOUT,*) 'array bound STRMAX exceeded'
	      WRITE(NOUT,*) 'increase from: ', STRMAX
	      GOTO 55555
	    ENDIF
C store the point in +ve part of the array
	    STRCEN(1,STRNOP) = LOWCEN(1)
	    STRCEN(2,STRNOP) = LOWCEN(2)
	    STRCEN(3,STRNOP) = LOWCEN(3)
	    STRRAD(STRNOP) =  -LOWENG
C store the sphere-box data
	    STRLVC(1,STRNOP) = LOWLVC(1)
	    STRLVC(2,STRNOP) = LOWLVC(2)
	    STRLVC(3,STRNOP) = LOWLVC(3)
	    STRLBO(STRNOP) =   LOWLBO
	    STRSBO(STRNOP) =   LOWSBO
	    STRBRD(STRNOP) =   LOWBRD
            IF (SHORTO.LT.1) 
     &        WRITE(NOUT, '(A,I5/)') ' stored as ', STRNOP
	  ELSE
C -ve
	    STRNON = STRNON + 1
	    IF (STRNON.GT.STRMAX) THEN
	      WRITE(NOUT,*) 'ERROR'
	      WRITE(NOUT,*) 'array bound STRMAX exceeded'
	      WRITE(NOUT,*) 'increase from: ', STRMAX
	      GOTO 55555
	    ENDIF
C store the point in -ve part of the array
	    STRCEN(1,-STRNON) = LOWCEN(1)
	    STRCEN(2,-STRNON) = LOWCEN(2)
	    STRCEN(3,-STRNON) = LOWCEN(3)
	    STRRAD(-STRNON) =  -LOWENG
C store the sphere-box data
            STRLVC(1,-STRNON) = LOWLVC(1)
            STRLVC(2,-STRNON) = LOWLVC(2)
            STRLVC(3,-STRNON) = LOWLVC(3)
            STRLBO(-STRNON) =   LOWLBO
            STRSBO(-STRNON) =   LOWSBO
            STRBRD(-STRNON) =   LOWBRD
            IF (SHORTO.LT.1)
     &        WRITE(NOUT, '(A,I5/)') ' stored as ',  -STRNON
	  ENDIF

C 18/07/94 output sphpdb file as we go along
          IF (FPDBSP.NE.'NONE') THEN
C is this a +ve or negative record?
            IF (SAMPLE.GT.0) THEN
              IREC = STRNOP
            ELSE
              IREC = -STRNON
            ENDIF
C 24/10/95 split writing sphpdb file into seperate subroutine
! 24/11/00 do not write the record if doing connolly
! do in concal instead
            IF (CONNR(1).LE.0)
     &                   CALL WPDBSP( SPDBSP, LCAPS, LSPHBX, IREC,
     &                   STRMAX, STRCEN, STRRAD, 
     &		         STRLVC, STRLBO, STRSBO, STRBRD, CVECT, ENDRAD)
          ENDIF

C Do connolly calculation after storage
          IF (CONNR(1).GT.0) THEN
C connolly routine returns requiv
            CALL CONCAL( NIN, NOUT, LERR, 
     &			 CVECT, LOWCEN,
     &			 ATMAX, ATNO, ATBRK, ATRES, ATCHN, ATRNO,
     &			 ATXYZ, ATVDW, ATBND, 
     &                   SPDBSP, FPDBSP, 
     &			 SHORTO,
     &			 ENDRAD, CUTSIZE, CONNR, PERPVE, PERPVN, REQUIV,
     &			 PEG_WRITEALL, IREC, SAMPLE)
C store requiv - in strbrd as this array is not used if connolly routine in 
C use - can tell by sample where to put
	    IF (SAMPLE.GT.0) THEN
	      STRBRD(STRNOP) = REQUIV
              IF (SHORTO.LT.1) WRITE(NOUT, '(A,I5/ A/A/)') 
     &' Requiv stored for ', STRNOP,
     &' -----',
     &' '

	    ELSE
C -ve
              STRBRD(-STRNON) = REQUIV
              IF (SHORTO.LT.1) WRITE(NOUT, '(A,I5/ A/A/)') 
     &' Requiv stored for ', -STRNON,
     &' -----',
     &' '
	    ENDIF

          ENDIF

C store the centre
C +ve or -ve?
C are we doing graphics and drawing close contact lines?
C only do if the radius found is +ve
C draw the lines connecting the closest atoms to the surface 
	  IF ((FHYDRA(1:4).NE.'NONE') .AND.
     &	      LLINE .AND. (LOWENG.LT.0.)     ) THEN
C first find closest atoms
	    CALL HOLEEN( LOWCEN, NEWENG, IAT1, IAT2, IAT3, DAT2, DAT3,
     &			 ATMAX, ATNO, ATXYZ, ATVDW, CUTSIZE)

C draw line from iat1 towards lowcen distance vdw radius of the atom
	    CALL HOLLIN( SHYDRA, LOWCEN, IAT1, ATMAX, ATXYZ, ATVDW)
C repeat for iat2
	    CALL HOLLIN( SHYDRA, LOWCEN, IAT2, ATMAX, ATXYZ, ATVDW)
	  ENDIF

C move lowcen by distance sample angs
C in direction cvect (which is a unit vector)
      	  LOWCEN(1) = LOWCEN(1) + SAMPLE*CVECT(1)
      	  LOWCEN(2) = LOWCEN(2) + SAMPLE*CVECT(2)
      	  LOWCEN(3) = LOWCEN(3) + SAMPLE*CVECT(3)
C if we are doing capsule option also have to move the second point
          IF (LCAPS) THEN
            LOWLVC(1) = LOWLVC(1) + SAMPLE*CVECT(1)
            LOWLVC(2) = LOWLVC(2) + SAMPLE*CVECT(2)
            LOWLVC(3) = LOWLVC(3) + SAMPLE*CVECT(3)
C 27/5/96 had changed routine so that started capsule
C at single point rather than moved both ends along
C but this produces very much less stable routine -
C get best of both worlds with a test as to which option is
C the better.
C Put how go this is into LOWENG
            CALL HCAPEN( LOWCEN, LOWENG, LOWLVC, NEWBRD, 
     &		         IAT1, IAT2, DAT2, ATMAX, ATNO, ATXYZ, ATVDW, PI)
C 23/11/95
C problem with using moved on capsule ends in hole - sometimes
C capsule gets too long! - so try making new capsule start
C at single point 27/5/96 - putinto CUR store rather than low
            CURLVC(1) = 0.5*(LOWLVC(1)+LOWCEN(1))
            CURLVC(2) = 0.5*(LOWLVC(2)+LOWCEN(2))
            CURLVC(3) = 0.5*(LOWLVC(3)+LOWCEN(3))
            CURCEN(1) = CURLVC(1)
            CURCEN(2) = CURLVC(2)
            CURCEN(3) = CURLVC(3)
C now work out how go this is
            CALL HCAPEN( CURCEN, CURENG, CURLVC, NEWBRD, 
     &		         IAT1, IAT2, DAT2, ATMAX, ATNO, ATXYZ, ATVDW, PI)
C as energy is minus the effective radius the largest sphere 
            IF (CURENG.LT.2.*LOWENG) THEN
C midpoint is better than moved on capsule
C so replace
              LOWLVC(1) = CURLVC(1)
              LOWLVC(2) = CURLVC(2)
              LOWLVC(3) = CURLVC(3)
              LOWCEN(1) = CURCEN(1)
              LOWCEN(2) = CURCEN(2)
              LOWCEN(3) = CURCEN(3)
              WRITE( NOUT, '(A/A/ 2(A,F12.3)/ A/ A,3F12.3)') 
     &' CAPSULE OPTION WARNING***',
     &'   Trying to move on current capsule by adding SAMPLE',
     &'   But eff radius=', -LOWENG, ' Compared to mid-point=', -CURENG,
     &'   (replace only if mid-point twice as good)',
     &'   So using midpoint coords= ', LOWCEN
              WRITE( NOUT, '(A)')
     &'   In plain english:',
     &'   The channel may be changing shape drastically at this point',
     &'   and the routine may be becoming stuck in a side passage.',
     &'   You are advised to look what is happening using molecular',
     &'   graphics (see section 6.2 of HOLE documentation',
     &'   (thanks to Peter Tieleman for suggesting clarification).', 
     &' CAPSULE OPTION WARNING*** (end)'
            ENDIF

          ENDIF
C work out new point
C END OF DO UNTIL
          GOTO 10
        ENDIF

C tell user that we have an end
	IF (SHORTO.LT.1) WRITE( NOUT,'(A,F6.1,A)') 
     &' This is an end! (radius above', ENDRAD, ' angs)'


C record the fact that previous record should be regarded as 
C an "end record" in .sph output file (if we are producing this)
C        IF (FPDBSP.NE.'NONE') WRITE( SPDBSP, '(A)') 'LAST-REC-END'
C 8/1/00 this is not sufficient - generate an array of 
C last-rec-end records - do this s/r addend
	IF (FPDBSP.NE.'NONE') THEN

! list is now done in s/r wpdbsp
!          WRITE( SPDBSP, '(A)') 'LAST-REC-END'
C move back to the last accepted point
C infact use the one before as the last is sometimes wonky!
          IF (SAMPLE.GT.0D0) THEN
C At +ve end
	    LOWCEN(1) = STRCEN(1,STRNOP-1)
	    LOWCEN(2) = STRCEN(2,STRNOP-1)
	    LOWCEN(3) = STRCEN(3,STRNOP-1)
          ELSE
C At -ve end
	    LOWCEN(1) = STRCEN(1,-STRNON+1)
	    LOWCEN(2) = STRCEN(2,-STRNON+1)
	    LOWCEN(3) = STRCEN(3,-STRNON+1)
	  ENDIF
C s/r to add the records
	  CALL ADDEND( SPDBSP, LOWCEN, SAMPLE, ENDRAD,
     &		       CVECT, PERPVE, PERPVN, 
     &                 ATMAX, ATNO, ATXYZ, ATVDW, CUTSIZE)
        ENDIF


C have we looked down from the initial point?
	IF (SAMPLE.GT.0D0) THEN
C change direction
	  SAMPLE = -SAMPLE
C back to original point
	  LOWCEN(1) = STRCEN(1,0) + SAMPLE*CVECT(1)
	  LOWCEN(2) = STRCEN(2,0) + SAMPLE*CVECT(2)
	  LOWCEN(3) = STRCEN(3,0) + SAMPLE*CVECT(3)
C if doing capsule option must also reset the second centre
          IF (LCAPS) THEN
            LOWLVC(1) = STRLVC(1,0) + SAMPLE*CVECT(1)
            LOWLVC(2) = STRLVC(2,0) + SAMPLE*CVECT(2)
            LOWLVC(3) = STRLVC(3,0) + SAMPLE*CVECT(3)
          ENDIF 
C to have continuous centre line need to now write the zeroth
C sphere record to the sphere centre pdb file
          IF (FPDBSP.NE.'NONE')
     &      CALL WPDBSP( SPDBSP, LCAPS, LSPHBX, 0,
     &                   STRMAX, STRCEN, STRRAD,
     &                   STRLVC, STRLBO, STRSBO, STRBRD, CVECT, ENDRAD)

C carry on
	  GOTO 10
	ENDIF

C 15/11/95 split off HOLE calculation into seperate s/r
C Now Calculation is finished can output results (graphical and numerical)
55555 CONTINUE
      RETURN
      END
