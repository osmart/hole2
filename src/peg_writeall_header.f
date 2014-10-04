      SUBROUTINE PEG_WRITEALL_HEADER( FPDBSP, SPDBSP, SAMPLE, 
     &		    		   CVECT, PERPVE, PERPVN,
     &				   CONNR, ENDRAD )
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
C s/r to perform connolly type surface calc within hole
C
C Modification history:
C
C Date  Author          Modification
C 11/00 O.S.S.		First version
C
C This s/r writes out the cvect etc that are are needed for the
C forpeg calculation

C pdb format output file 'atoms' are sphere centres
C default to none by setting FPDBSP to 'NONE'
      CHARACTER*200             FPDBSP
      INTEGER                   SPDBSP

C a sampling distance - the distance between planes
      DOUBLE PRECISION          SAMPLE

C a vector in the direction of the channel
      DOUBLE PRECISION          CVECT(3)

C unit vectors in plane normal to cvect
C needed to a number of routines - originally h2dmap but now conn
C and addend
C these are normal - first is east direction (x on plane)
C                    and second is north     (y on plane)
C return unchanged!!!
      DOUBLE PRECISION		PERPVE(3), PERPVN(3)

C option CONN introduced 1/00 - 
C CONNR(1) is the connolly surface probe radius (default 1.15Angs)
C          the option is turned off by setting this radius negative
C CONNR(2) is the grid size to be used in the calculation - default .85 angs
      DOUBLE PRECISION		CONNR(2)
! the radius at which an end is reached
      DOUBLE PRECISION		ENDRAD

C internal variables ***********
C end of decs ******************

! only output sphpdb file if it is open 
! this is indicated by is name not being none
      IF (FPDBSP.NE.'NONE') THEN
        WRITE( SPDBSP, '(A, F12.6)') 'SAMPLE', SAMPLE
        WRITE( SPDBSP, '(A,3F12.6)') 'CVECT ', CVECT	
        WRITE( SPDBSP, '(A,3F12.6)') 'PERPVE', PERPVE
        WRITE( SPDBSP, '(A,3F12.6)') 'PERPVN', PERPVN
        WRITE( SPDBSP, '(A,2F12.6)') 'CONNR ', CONNR      
        WRITE( SPDBSP, '(A,2F12.6)') 'ENDRAD', ENDRAD     
      ENDIF
      RETURN
      END
