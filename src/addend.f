      SUBROUTINE ADDEND( SPDBSP, LASCEN, SAMPLE, ENDRAD,
     &		        CVECT, PERPVE, PERPVN, 
     &                  ATMAX, ATNO, ATXYZ, ATVDW, CUTSIZE)
           IMPLICIT NONE
C ********************************************************************
C *                                                                  *
C * This software is an unpublished work containing confidential and *
C * proprietary information of Oliver Smart. Use, disclosure,    *
C * reproduction and transfer of this work without the express       *
C * written consent of Oliver Smart are prohibited. This notice  *
C * must be attached to all copies or extracts of the software.      *
C *                                                                  *
C * (c) 2000 Oliver Smart *
C *                                                                  *
C ********************************************************************
C
C s/r add end records to spdbsp - needed to prevent dumbbells
C in graphics calcs
C
C Modification history:
C
C Date  Author          Modification
C 01/00 O.S.S.    	First version

C passed vbles *****************

C pdb format output file 'atoms' are sphere centres
C default to none by setting FPDBSP to 'NONE'
C      CHARACTER*200             FPDBSP
      INTEGER                   SPDBSP

C Last accepted HOLE centre - 
C done (s/r holcal normal supplies lowcen)
      DOUBLE PRECISION          LASCEN(3)

C a sampling distance - the distance between planes
C use here for the sign - if -ve then need to work in this direction
      DOUBLE PRECISION          SAMPLE

C a vector in the direction of the channel
C must be a unit vector - return unchanged
      DOUBLE PRECISION          CVECT(3)

C New option 22/11/93 - For wide channels need to
C be able to specify the radius at which an end is reached
C in this routine need to add a series of points that are above this radius
      DOUBLE PRECISION          ENDRAD

C unit vectors in plane normal to cvect
C needed to a number of routines - originally h2dmap but now conn
C and addend
C these are normal - first is east direction (x on plane)
C                    and second is north     (y on plane)
C return unchanged!!!
      DOUBLE PRECISION		PERPVE(3), PERPVN(3)

C maximum no. of atoms
C returned unchanged
      INTEGER			ATMAX

C number of atoms read in from each file:
C returned unchanged
      INTEGER			ATNO

C co-ordinates
C returned unchanged
      DOUBLE PRECISION		ATXYZ( 3, ATMAX)

C vdw radius of each atom
C returned unchanged
      DOUBLE PRECISION		ATVDW(ATMAX)

C cut off list control vble, -ve: no cutoff lists used in calculation
C otherwise this is the additional radius above the sphere radius
C to be used for cutoff list (see s/r holeen)
      DOUBLE PRECISION		CUTSIZE

C internals ********************

C sign of sample
      DOUBLE PRECISION		SSIGN

C loop counts Z north east, XYZ
      INTEGER			ZCOUNT, NCOUNT, ECOUNT, XCOUNT

C displacements Z north east
      DOUBLE PRECISION		ZDISP, NDISP, EDISP

C a new point
      DOUBLE PRECISION          TSTXYZ(3), NEWENG

C atom list no. with smallest dist-vdw radius, 2nd smallest
C 07/06/94 as iat's may or may not be supplied with previously 
C found numbers then we can use this to speed up the procedure
C Dec 97 add iat3 
      INTEGER			IAT1, IAT2, IAT3

C 2nd/3rd smallest distance-vdw radius (of iat2/3)
      DOUBLE PRECISION		DAT2, DAT3


C end of decs ******************

C sign of sample
      IF (SAMPLE.GT.0D0) THEN
        SSIGN = +1D0
      ELSE
        SSIGN = -1D0
      ENDIF
C produce a grid of prospective points:
C go along cvect
      DO 10 ZCOUNT = 0, 4
        ZDISP = SSIGN*REAL(ZCOUNT)*(0.5*ENDRAD)
        DO 20 NCOUNT = -2, 2
          NDISP = REAL(NCOUNT)*(0.5*ENDRAD)
	  DO 30 ECOUNT = -2, 2
            EDISP = REAL(ECOUNT)*(0.5*ENDRAD)
C the test point
             TSTXYZ(1) = LASCEN(1)
	     TSTXYZ(2) = LASCEN(2)
	     TSTXYZ(3) = LASCEN(3)
C displace Z
	     TSTXYZ(1) = TSTXYZ(1) + ZDISP*CVECT(1) 
	     TSTXYZ(2) = TSTXYZ(2) + ZDISP*CVECT(2) 
	     TSTXYZ(3) = TSTXYZ(3) + ZDISP*CVECT(3) 
C displace N
	     TSTXYZ(1) = TSTXYZ(1) + NDISP*PERPVN(1)
	     TSTXYZ(2) = TSTXYZ(2) + NDISP*PERPVN(2)
	     TSTXYZ(3) = TSTXYZ(3) + NDISP*PERPVN(3)
C displace E
	     TSTXYZ(1) = TSTXYZ(1) + EDISP*PERPVE(1)
	     TSTXYZ(2) = TSTXYZ(2) + EDISP*PERPVE(2)
	     TSTXYZ(3) = TSTXYZ(3) + EDISP*PERPVE(3)
C test point
              CALL HOLEEN( TSTXYZ, NEWENG, 
     &           IAT1, IAT2, IAT3, DAT2, DAT3,
     &           ATMAX, ATNO, ATXYZ, ATVDW, CUTSIZE)
              NEWENG = - NEWENG
C write out only points with pore radius above endrad
	      IF (NEWENG.GT.ENDRAD) THEN
	       WRITE( SPDBSP, '(A,I4,4X,3F8.3,2F6.2)')
     &   'ATOM      1  QSS SPH S', -888,
     &       (TSTXYZ(XCOUNT), XCOUNT= 1, 3),
     &       NEWENG, 0.0   
                WRITE( SPDBSP, '(A)') 'LAST-REC-END'
	      ENDIF
30        CONTINUE
20      CONTINUE
10    CONTINUE

      RETURN
      END
