      SUBROUTINE CGUESS( NOUT, SHORTO, LERR, CPOINT, CVECT, SAMPLE, 
     &                  ATMAX, ATNO, ATBRK, ATRES, ATCHN, ATRNO,
     &                  ATXYZ, ATVDW, ATBND, CUTSIZE)
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
C * (c) 200 Oliver Smart                    *
C *                                                                  *
C ********************************************************************
C
C Modification history:
C
C Date	Author		Modification
C 08/00 O.S.S.		First version
C			This s/r try to guess an initial point
C			and/or a channel vector for a calculation
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


C cut off list control vble, -ve: no cutoff lists used in calculation
C otherwise this is the additional radius above the sphere radius
C to be used for cutoff list (see s/r holeen)
      DOUBLE PRECISION          CUTSIZE


C internal vbles ***************



C store for all atom and ca centroids
      DOUBLE PRECISION		CENTROID_ALLATOM(3), 
     &				CENTROID_CA(3)
      INTEGER			NUMBER_CA 

C loop counts
      INTEGER			ACOUNT, XCOUNT, YCOUNT, ZCOUNT
      DOUBLE PRECISION		RCOUNT

C for call to holen
      DOUBLE PRECISION		TSTXYZ(3), TSTENG
      INTEGER			IAT1, IAT2, IAT3
      DOUBLE PRECISION		DAT2, DAT3

C store for the sum of the radii found along a particular direction
      DOUBLE PRECISION		SUM_PRAD, MAX_PRAD
      INTEGER			XYZBEST 

C vbles for grid search
      INTEGER			NCYCLE
      DOUBLE PRECISION		BESTXYZ(3), BESTRAD


C end of decs ******************

      WRITE( NOUT, '(A)')
     &' ',
     &' S/R cguess will try to guess CPOINT and/or CVECT',
     &' ',
     &' THESE GUESSES MAY WELL BE WRONG!!!!',
     &' Routine relies on having a reasonably symmetric',
     &' channel oriented along x, y or z vector',
     &' YOU ARE STRONGLY ADVISED TO CHECK THE RESULTS',
     &'                       WITH MOLECULAR GRAPHICS'



      IF (CPOINT(1).EQ.-55555.) THEN
C try to identify a suitable initial point
C either (a) the centroid of all "CA" atoms or
C        (b) the centroid of all atoms
        CENTROID_ALLATOM(1) = 0.
        CENTROID_ALLATOM(2) = 0.
        CENTROID_ALLATOM(3) = 0.
        CENTROID_CA(1) = 0.
        CENTROID_CA(2) = 0.
        CENTROID_CA(3) = 0.
	NUMBER_CA = 0
        DO ACOUNT = 1, ATNO ! go thru atoms
	  CENTROID_ALLATOM(1) = CENTROID_ALLATOM(1) + ATXYZ(1,ACOUNT)
	  CENTROID_ALLATOM(2) = CENTROID_ALLATOM(2) + ATXYZ(2,ACOUNT)
	  CENTROID_ALLATOM(3) = CENTROID_ALLATOM(3) + ATXYZ(3,ACOUNT)
	  IF (ATBRK(ACOUNT)(1:2).EQ.'CA') THEN
	    NUMBER_CA = NUMBER_CA + 1 
            CENTROID_CA(1) = CENTROID_CA(1) + ATXYZ(1,ACOUNT)	    
            CENTROID_CA(2) = CENTROID_CA(2) + ATXYZ(2,ACOUNT)	    
            CENTROID_CA(3) = CENTROID_CA(3) + ATXYZ(3,ACOUNT)	    
	  ENDIF
	ENDDO ! end of going thru atoms
        IF (NUMBER_CA.GE.3) THEN ! guess cpoint based on calpha position
	  CPOINT(1) = CENTROID_CA(1)/NUMBER_CA
	  CPOINT(2) = CENTROID_CA(2)/NUMBER_CA
	  CPOINT(3) = CENTROID_CA(3)/NUMBER_CA
	  IF (SHORTO.LE.1) WRITE( NOUT, '(A,I5,A,3F12.4 )')
     &' Initial CPOINT guess - centroid of', NUMBER_CA,
     &            ' ''CA'' atom positions', CPOINT 
        ELSE  ! guess cpoint based on all atoms
	  CPOINT(1) = CENTROID_ALLATOM(1)/ATNO
	  CPOINT(2) = CENTROID_ALLATOM(2)/ATNO
	  CPOINT(3) = CENTROID_ALLATOM(3)/ATNO
	  IF (SHORTO.LE.1) WRITE( NOUT, '(A,I5,A,3F12.4 )')
     &' Initial CPOINT guess - centroid of', ATNO,
     &            'ALL atom positions', CPOINT           	  
	ENDIF
	
C m121 original procedure does not work on 1omf!
C try a bit more sophistication	
C do a grid search on 1angs grid
        IF (SHORTO.LE.1) WRITE( NOUT, '(A)') 
     &' Now do quick grid search on cpoint'
C try 5 cycles initially
        DO NCYCLE = 1, 5
          BESTRAD = -1E10
	  DO XCOUNT = -1, 1 ! go thru x
	    TSTXYZ(1) = CPOINT(1) + REAL(XCOUNT)
            DO YCOUNT= -1, 1 ! go thru y
	      TSTXYZ(2) = CPOINT(2) + REAL(YCOUNT)
	      DO ZCOUNT= -1, 1 ! go thru z
	        TSTXYZ(3) = CPOINT(3) + REAL(ZCOUNT)
	        CALL HOLEEN( TSTXYZ, TSTENG, IAT1, IAT2, IAT3, 
     &            DAT2, DAT3,ATMAX, ATNO, ATXYZ, ATVDW, CUTSIZE)
                TSTENG = -TSTENG
	        IF (TSTENG.GT.BESTRAD) THEN !best point so far?
	          BESTRAD = TSTENG
		  BESTXYZ(1) = TSTXYZ(1)
		  BESTXYZ(2) = TSTXYZ(2)
		  BESTXYZ(3) = TSTXYZ(3)
	        ENDIF
	      ENDDO ! go thru z
	    ENDDO ! go thru y
	  ENDDO ! go thru x
          CPOINT(1) = BESTXYZ(1)
          CPOINT(2) = BESTXYZ(2)
          CPOINT(3) = BESTXYZ(3)
	  IF (SHORTO.LE.1) WRITE( NOUT, '(A,I5,A,F10.3,A,3F8.3)') 
     &' cycle ', NCYCLE, ' bestrad= ', BESTRAD, ' at ', CPOINT
	ENDDO ! number of cycles (NCYCLE)
		

	
      ENDIF  ! end of if (cpoint(1).eq.-55555.) then

C ok. now have a cpoint - try to get a cvect
      IF (CVECT(1).EQ.-55555.) THEN
        IF (SHORTO.LE.1) WRITE( NOUT, '(A/ A,16X,A)')
     &' testing points from cpoint in X, Y then Z direction',
     &'   point', '   pore radius' 
        MAX_PRAD = -1E10
	XYZBEST = -1
        DO XCOUNT = 1, 3 ! go thru x, y, and z
C vector to test 
          IF (SHORTO.LE.1) WRITE(NOUT, '(A,I5)') 'direction ', XCOUNT
          CVECT(1) = 0.
          CVECT(2) = 0.
          CVECT(3) = 0.
          CVECT(XCOUNT) = 1.
C starting at the initial point
C now look at points from -5 to +5 along proposed cvect
C find hole radius of points
          SUM_PRAD =  0.
          DO RCOUNT = -5.,5.001,1. 
	    TSTXYZ(1) = CPOINT(1) + RCOUNT*CVECT(1)
	    TSTXYZ(2) = CPOINT(2) + RCOUNT*CVECT(2)
	    TSTXYZ(3) = CPOINT(3) + RCOUNT*CVECT(3)
C for each point find hole pore radius
            CALL HOLEEN( TSTXYZ, TSTENG, IAT1, IAT2, IAT3, DAT2, DAT3,
     &           ATMAX, ATNO, ATXYZ, ATVDW, CUTSIZE)
            TSTENG = -TSTENG
	    IF (SHORTO.LE.1) WRITE(NOUT, '(3F8.3,F10.3)') TSTXYZ, TSTENG
	    SUM_PRAD = SUM_PRAD + TSTENG
          ENDDO ! end of going thru the 11 points
	  IF (SHORTO.LE.1) WRITE( NOUT, '(A,F10.3)')
     &' Average pore radius along the line= ', SUM_PRAD/11.
	  IF (SUM_PRAD.GT.MAX_PRAD) THEN
	    MAX_PRAD = SUM_PRAD
	    XYZBEST = XCOUNT
	  ENDIF 
        ENDDO ! go thru x, y, and z
C best direction?
        CVECT(1) = 0.
        CVECT(2) = 0.
        CVECT(3) = 0.
        CVECT(XYZBEST) = 1.
        IF (SHORTO.LE.1) WRITE( NOUT, '(A,I5)')
     &' Best direction is found to be ', XYZBEST, '(1=X,2=Y,3=Z)'
        
      ENDIF ! end of whether we should guess cpoint

      WRITE( NOUT, '(A/ A,3F12.4/ A,3F12.4)')
     &' s/r cguess has produced initial values for cpoint &/or cvect:',
     &'CPOINT ', CPOINT,
     &'CVECT  ', CVECT
      WRITE( NOUT, '(A)')
     &' THESE GUESSES MAY WELL BE WRONG!!!!',
     &' Routine relies on having a reasonably symmetric',
     &' channel oriented along x, y or z vector',
     &' YOU ARE STRONGLY ADVISED TO CHECK THE RESULTS',
     &'                       WITH MOLECULAR GRAPHICS',
     &' ',
     &' '

      RETURN
      END
