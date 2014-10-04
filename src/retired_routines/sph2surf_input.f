      PROGRAM sph2surf_input
      IMPLICIT NONE
C ********************************************************************
C *                                                                  *
C * This software is an unpublished work containing confidential and *
C * proprietary information of Birmingham University. Use, disclosure,
C * reproduction and transfer of this work without the express       *
C * written consent of Birkbeck College are prohibited. This notice  *
C * must be attached to all copies or extracts of the software.      *
C *                                                                  *
C * (c) 2002 Oliver Smart & Birmingham University, All rights reserved    *
C *                                                                  *
C ********************************************************************
C
C Modification history:
C
C Date	Author		Modification
C 05/02 O.S. Smart      Original version 
C
C program to convert .sph file for entry into surf program
C read from standard input and write to standard output
      CHARACTER*80		LINE
      
      INTEGER			SIN, SOUT, SERROR
      PARAMETER(		SIN = 5)
      PARAMETER(		SOUT = 6)
      PARAMETER(		SERROR = 0)
      CHARACTER*50              ARG_ACOUNT ! an argument  
      LOGICAL 			LERR
      DOUBLE PRECISION		SPXYZ(3), SPRAD ! center and radius
      INTEGER			SPNUM ! count
      INTEGER			XCOUNT ! loop count x y and z
	
! end of decs ******************

      LERR = .FALSE. ! set no error indicator
      SPNUM = 0


! trap a request for help 
      IF (iargc().GE.1) THEN 
        CALL GETARG( 1, ARG_ACOUNT)
        IF (ARG_ACOUNT(1:2).NE.'-h') THEN
	  WRITE( SERROR, '(A)')
     &' error unrecognized input option - only allow -h'
	ENDIF
        WRITE( SERROR, '(A)') 
     &' program sph2surf_input help message',
     &' +++++++++++++++++++++++++++++++++++',
     &' reads in a .sph file produced by hole and converts for input2',
     &' into the surf program to triangulate. Usage:'
        LERR = .TRUE.
        GOTO 55555
      ENDIF

! do until EOF
10    CONTINUE
        READ( SIN, '(A)', END=55) LINE
	IF (LINE(1:4).EQ.'ATOM') THEN
	  ! have an atom record
	  IF ((LINE(11:22).NE.'1  QSS SPH S') .AND.
     &        (LINE(11:22).NE.'1  QC1 SPH S')      ) THEN
             WRITE( SERROR, '(A)') 
     &' *** ERROR ***'//CHAR(7),
     &' record read not "QSS SPH S" atom',
     &' produced by SPHPDB option of hole',
     &' Aborting!'//CHAR(7)
            LERR = .TRUE.
	    GOTO 55555
          ELSE
	    ! have a legit hole atom
	    SPNUM = SPNUM + 1 ! increment count
	    READ( LINE(31:54),'(3F8.3)')  ! read co-ords 
     &      (SPXYZ( XCOUNT), XCOUNT= 1,3)
	    READ( LINE(55:60),'(F6.2)') SPRAD ! read radius
	    
	    WRITE( SOUT, '(I5,4F10.3)') SPNUM, SPRAD, SPXYZ
	    
	  ENDIF
	ENDIF
	
	
        GOTO 10
      
! got to the end of the file.      
55    CONTINUE    
    
55555 CONTINUE
      END
