      SUBROUTINE RATCAL( NOUT, SHORTO, LERR, CPOINT, CVECT, SAMPLE, 
     &                   MCSTEP, 
     &                   ATMAX, ATNO, ATBRK, ATRES, ATCHN, ATRNO,
     &                   ATXYZ, ATVDW, ATBND, 
     &                   PI, ENDRAD, CUTSIZE, SPDBSP, FPDBSP,
     &			 STRMAX, STRNOP, STRNON, STRCEN, STRRAD)
      IMPLICIT NONE
C ********************************************************************
C *                                                                  *
C * This software is an unpublished work containing confidential and *
C * proprietary information of Oliver Smart. Use,           *
C * disclosure, reproduction and transfer of this work without the   *
C * express written consent of Oliver Smart are prohibited. *
C * This notice must be attached to all copies or extracts of the    *
C * software.                                                        *
C *                                                                  *
C * (c) 1997 Oliver Smart                    *
C *                                                                  *
C ********************************************************************
C
C Modification history:
C
C Date	Author		Modification
C 12/97	O.S. Smart	Original version.
C 07/98 O.S.S. 		Add penalty function (l012, vble PENDIS)
C
C passed vbles *****************

C output stream numbers
      INTEGER			NOUT

C error indicator
      LOGICAL			LERR

C variable to reduce text output, if oshort =
C 0: full text output
C 1: no in run progress reported
C 2: ditto plus no graph type output - only leaving
C    Minimum radius & conductance calculations.
C 3: All text output other than input card mirroring
C    and error messages turned off
      INTEGER                   SHORTO

C an initial point in the channel
      DOUBLE PRECISION          CPOINT(3)

C a vector in the direction of the channel
      DOUBLE PRECISION          CVECT(3)

C a sampling distance - the distance between planes
      DOUBLE PRECISION          SAMPLE

C the number of steps to be taken
C on each Monte Carlo application
      INTEGER                   MCSTEP

C array bound for maximum number of atoms and the actual maximum
      INTEGER			ATMAX, ATNO

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

C will need PI
      DOUBLE PRECISION          PI

C cut off list control vble, -ve: no cutoff lists used in calculation
C otherwise this is the additional radius above the sphere radius
C to be used for cutoff list (see s/r holeen)
      DOUBLE PRECISION          CUTSIZE

C For wide channels need to
C be able to specify the radius at which an end is reached
      DOUBLE PRECISION          ENDRAD

C pdb format output file 'atoms' are sphere centres
C default to none by setting FPDBSP to 'NONE'
      CHARACTER*200             FPDBSP
      INTEGER                   SPDBSP

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

C internal vbles ***************
C RATE is the third most distant atomic vdw surface
C minus the closest - always +VE, zero when
C have got to locally best squeeze through.
      DOUBLE PRECISION		RATE

C atom list no. with smallest dist-vdw radius, 2nd smallest
C 07/06/94 as iat's may or may not be supplied with previously 
C found numbers then we can use this to speed up the procedure
C Dec 97 add iat3 
      INTEGER			IAT1, IAT2, IAT3

C 2nd/3rd smallest distance-vdw radius (of iat2/3)
      DOUBLE PRECISION		DAT2, DAT3

C spherical polar coords
      DOUBLE PRECISION		RCOORD, PHI, THETA
C derivatives of rate wrt to phi and theta
      DOUBLE PRECISION	 	DRATEDTHE, DRATEDPHI
C cartesian coords      
      DOUBLE PRECISION		XYZCOR(3)
C step length for sd
      DOUBLE PRECISION		LAMBDA
C old value for rate
      DOUBLE PRECISION		RATOLD

C step count for sd
      INTEGER			MSTEP
C loop count for PHI, initial value on each step
      INTEGER			PCOUNT
      DOUBLE PRECISION		PHIINT
C loop count for THETA, initial value on each step
      INTEGER			TCOUNT
      DOUBLE PRECISION		THEINT
C number of samples to take for phi and theta
      INTEGER			PNUMB, TNUMB

C loop count for samples, a vble to work out distance squared
      INTEGER			SCOUNT
      DOUBLE PRECISION		DIST2      

C loop count for 2 passes
      INTEGER			FCOUNT

C minimum/maximum radius for a particular shell
      DOUBLE PRECISION		MINTHISRAD, MAXTHISRAD

C minimum maximum radius overall
      DOUBLE PRECISION		MINOVER

C penalty function cutin distance
      DOUBLE PRECISION		PENDIS

C HOLE or RAT objective function
      LOGICAL			LHORR
 
C For s/r raten cut down on the number of records needed to go 
C through to check overlap by storring the start number of this shell -
C n.b. assumes that shells are further apart than pendist
      INTEGER			SSHELL
       
C end of decs ******************

C shell starts at zero
      SSHELL = 0 
      
C do not allow spheres to get within 0.7*sample angs of one another
      PENDIS = 0.9*SAMPLE
      
      WRITE( NOUT, '(A/ A,F8.3,A)') 
     &' RAT OPTION (double pass version)',
     &' Using penalty function cutin distance of', PENDIS, ' Angs'
     
C try a single call from cpoint to raten
      CALL RATEN( CPOINT, RATE, IAT1, IAT2, IAT3, DAT2, DAT3,
     &                    ATMAX, ATNO, ATXYZ, ATVDW, CUTSIZE,
     &			PENDIS, STRMAX, STRNOP, STRNON, STRCEN, STRRAD,
     &			.FALSE., SSHELL)
      WRITE( NOUT, '(3(A,F10.3,A/))')
     &' Initial rat "energy" value is ', RATE, ' Angs',
     &' As maximum radius is ', DAT2,   ' Angs',
     &' and 3rd most distant ', DAT3,   ' Angs'

C write initial rat record to sph file
      IF (FPDBSP.NE.'NONE') 
     &   WRITE( SPDBSP, '(A,6F10.3)')
     &   'RAT', CPOINT, SAMPLE, ENDRAD, DAT2

C initialize minimum largest radius for any shell
      MINOVER = +1D06

C Use spherical coordinates from cpoint:
C rcoord distance from point, theta is angle to x-axis, phi angle to z 
C minimize RAT, keeping rcoord constant and adjusting theta & phi

C start radius at zero
      RCOORD = 0D0
C loop through rcoord 
C do until (77) all initial radii on a circle are larger than endrad
C check with minthisrad increase by sample each time
77    CONTINUE
C Initialize minthisrad
      MINTHISRAD = +1D06
      MAXTHISRAD = -1D06
      RCOORD = RCOORD + SAMPLE
      WRITE( NOUT, '(A,F10.4)') 
     &' Considering shell radius ', RCOORD
C loop through phi, want circles every 1/2 sample distance
C k137 for formulae
C half to get distance between points about sample 
        PNUMB = INT(RCOORD/SAMPLE)*2 + 1
        DO 40 PCOUNT = 1, PNUMB

C initial value to be taken for phi
          PHIINT = REAL(PCOUNT)*PI/REAL(PNUMB)
C loop through theta - the number of samples depends on phi - k137
C half to get distance between points about sample 
          TNUMB = 2*PI*RCOORD*SIN(PHIINT)/SAMPLE + 1
          DO 50 TCOUNT = 1, TNUMB
	  
C flush output stream
	    IF (NOUT.EQ.6) CALL FLUSH6

            THEINT = 2*REAL(TCOUNT)*PI/TNUMB

C 22 July 1998 l013 do 2 passes for each angle - use vble fcount
C in the first apply the hole energy function - 2nd use rat 
C idea is to have as maximizes centres as possible but still get
C side routes of these
            DO 50 FCOUNT = 1, 2
	    IF (FCOUNT.EQ.1) THEN
	      LHORR = .TRUE.
	    ELSE
	      LHORR = .FALSE.
	    ENDIF
	    
C now do crude sd in theta, phi, space
            PHI = PHIINT
	    THETA = THEINT
            LAMBDA = 1E-06
C check the radius of the initial point
            CALL RATESC( RCOORD, PHI, THETA, CPOINT, 
     &                   RATE, DRATEDTHE, DRATEDPHI, XYZCOR,
     &                   IAT1, IAT2, IAT3, DAT2, DAT3,
     &                   ATMAX, ATNO, ATXYZ, ATVDW, CUTSIZE,
     &			 PENDIS, STRMAX, STRNOP, STRNON, STRCEN, STRRAD,
     &			 LHORR, SSHELL)
C change question to still process up to endrad+1 - provides endrecords
            IF (DAT2.GT.ENDRAD+1.5D0*SAMPLE) THEN
C this is an end! Still write out point to provide cutouts
C provided radius not too large - take an additional 1.5*sample
C this should surround the molecule with a shell of "end records"
C to make sure we do not end up with dumb bells.
C do not do get toomany points!
C              IF (DAT2.LT.ENDRAD+2D0*SAMPLE) THEN
C                WRITE( SPDBSP, '(A,I4,4X,3F8.3,2F6.2)')
C     &   'ATOM      1  QSS SPH S', 1, XYZCOR, DAT2, 0.0
C                WRITE( SPDBSP, '(A)') 'LAST-REC-END' 
C	      ENDIF
	      IF (DAT2.LT.MINTHISRAD) MINTHISRAD = DAT2
	      IF (DAT2.GT.MAXTHISRAD) MAXTHISRAD = DAT2
C jump to consider next point
              GOTO 50
	    ENDIF

C maximum of 100 steps
	    DO 10 MSTEP = 1, 500
C find (x, y, z) for this set (rcoord, theta, and phi) 
C find rate, and dratdthe,dratdphi
              CALL RATESC( RCOORD, PHI, THETA, CPOINT, 
     &                   RATE, DRATEDTHE, DRATEDPHI, XYZCOR,
     &                   IAT1, IAT2, IAT3, DAT2, DAT3,
     &                   ATMAX, ATNO, ATXYZ, ATVDW, CUTSIZE,
     &			 PENDIS, STRMAX, STRNOP, STRNON, STRCEN, STRRAD,
     &			 LHORR, SSHELL)

C classify converged to true solution when DAT3-DAT2 falls below 0.0005 
              IF (DAT3-DAT2.LT.5D-04) GOTO 20
C after 100 steps reject any position with DAT3-DAT2 above 0.1 as it will never
C converge
              IF ((MSTEP.EQ.100).AND.(DAT3-DAT2.GT.0.1D0)) GOTO 15
C              if (MOD(MSTEP,20).eq.0) then
C                write( nout, *) 
C                write( nout, *) 'MSTEP= ', mstep      
C	        write( nout, *) 'lambda= ', lambda
C	        write( nout, *) 'rate= ', rate
C	        write( nout, *) 'hole radius= ', DAT2
C		write( nout, *) ' DAT3-DAT2= ', DAT3-DAT2
C     	        write( nout, *) 'DRATEDPHI= ', DRATEDPHI
C	        write( nout, *) 'DRATEDTHE= ', DRATEDTHE
C	      endif
C take a step lambda along minus the derivative
              THETA = THETA -  LAMBDA*DRATEDTHE
              PHI = PHI - LAMBDA*DRATEDPHI
C if the RATE has gone up then reduce the following step length
              IF (RATE.GT.RATOLD) THEN
	        LAMBDA = 0.5*LAMBDA
	      ELSE
C increase length slowly
	        LAMBDA = 1.2*LAMBDA
	      ENDIF
	      RATOLD = RATE
10          CONTINUE
C jump here on not converging fast enough.
15          CONTINUE
C have not converged to zero value
C still counts for minimum but not max consideration
	    IF (DAT2.LT.MINTHISRAD) MINTHISRAD = DAT2
C           WRITE( NOUT, '(3(A, F10.4))') 
C     &' HAVE NOT CONVERGED, DAT2-DAT3= ', DAT3-DAT2, 
C     &                        ' hole rad= ', DAT2, 
C     &                            ' rate= ', rate
	    GOTO 50
	    
C converged - jump to 20
20          CONTINUE
            WRITE( NOUT, '(/A,I5,A/ 3(A,F10.4,A,F10.4/),
     &                     A,F10.4/ A,3F8.3 )')
     &' Converged in', MSTEP, ' steps sd min',
     &' From theta= ', (180d0/pi)*theint, ' phi=', (180d0/pi)*phiint,
     &'   to theta= ', (180d0/pi)*theta,    ' phi=', (180d0/pi)*phi,
     &' RATE = ', RATE, ' hole radius= ', DAT2,
     &' DAT3-DAT1= ', DAT3-DAT2,
     &' Cartesian coords ', XYZCOR
     	    write( nout, *) 'DRATEDPHI= ', DRATEDPHI
	    write( nout, *) 'DRATEDTHE= ', DRATEDTHE
C max or min for this shell?
	    IF (DAT2.LT.MINTHISRAD) MINTHISRAD = DAT2
C reject because point sub optimal?
            IF (DAT3-DAT2.GT.0.001) THEN
	      WRITE( NOUT, '(A)') ' REJECT - suboptimal'
	      GOTO 50
	    ENDIF
C max this rad should only consider optimized
	    IF (DAT2.GT.MAXTHISRAD) MAXTHISRAD = DAT2
C store and output?
C only if radius not too large
            IF (DAT2.GT.ENDRAD+1.5D0*SAMPLE) THEN
              WRITE( NOUT, '(A)') 'REJECT - Too large'
	      GOTO 50
	    ENDIF   
C check to see whether this record is already in store (starts at zero)
            DO 70 SCOUNT = 0, STRNOP
C find distance squared between the stored point and the one under consideration
              DIST2 = (STRCEN(1,SCOUNT)-XYZCOR(1))**2 +
     & 	              (STRCEN(2,SCOUNT)-XYZCOR(2))**2 +      
     & 	              (STRCEN(3,SCOUNT)-XYZCOR(3))**2 
C is this short? points with 0.001angs assumed the same
              IF (DIST2.LT.1E-06) THEN
C if so consider next point
                WRITE( NOUT, '(A,I6)') 'Point already found #', SCOUNT
		GOTO 50
              ENDIF
70          CONTINUE      
C have one more centre intial point goes to
C zero as strnop starts at -1
	    STRNOP = STRNOP + 1
            WRITE( NOUT, '(A,I6)') 'Point stored as #    ', SCOUNT
C check to see whether store is full
	    IF (STRNOP.GT.STRMAX) THEN 
	      WRITE(NOUT,*) 'ERROR'
	      WRITE(NOUT,*) 'array bound STRMAX exceeded'
	      WRITE(NOUT,*) 'increase from: ', STRMAX
	      GOTO 55555
	    ENDIF
C store the point in +ve part of the array
	    STRCEN(1,STRNOP) = XYZCOR(1)
	    STRCEN(2,STRNOP) = XYZCOR(2)
	    STRCEN(3,STRNOP) = XYZCOR(3)
	    STRRAD(STRNOP)  =   DAT2

C only write if radius is +ve - infact make bigger than .01 angs
C in future introduce keyword control on this number
            IF (DAT2.GT.0.01) THEN 
              IF (FPDBSP.NE.'NONE') 
     &          WRITE( SPDBSP, '(A,I4,4X,3F8.3,2F6.2)')
     &   'ATOM      1  QSS SPH S', 1, XYZCOR, DAT2, RCOORD
C is the radius of this point above endrad?  If so mark in file
                IF (DAT2.GT.ENDRAD) WRITE( SPDBSP, '(A)') 'LAST-REC-END'
C update file to disk
                CALL DISKFF( SPDBSP)
            ENDIF
C end of TCOUNT loop going thru theta steps
50        CONTINUE
C end of PCOUNT loop going thru phi steps
40      CONTINUE

      WRITE( NOUT, '(3(A,F10.3))')
     &' Shell radius ', RCOORD, ' min rad=', MINTHISRAD,
     &' max rad= ', MAXTHISRAD

C up date shell start vble for next shell
      SSHELL = STRNOP + 1
      
C is this the minimum max radius
      IF (MAXTHISRAD.LT.MINOVER) MINOVER= MAXTHISRAD

C have finished consideration of radius rcoord
C have we finished overall?
C this is only achieved when all radii above minrad
      IF (MINTHISRAD.GT.ENDRAD) THEN
        WRITE( NOUT, '(A/ A,F10.3,A)')
     &' Have finished all radii on this shell above ENDRAD',
     &' Minimum escape radius is equal to or less than', MINOVER, 
     &							' angs'
      ELSE
C have not finished! Do next shell.
        GOTO 77
      ENDIF

55555 CONTINUE
      write( nout, *) 'TEMPORARY STOP IN S/R RATCAL'

      STOP 'TEMPORARY STOP IN S/R RATCAL'
      END
