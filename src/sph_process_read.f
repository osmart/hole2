      SUBROUTINE SPH_PROCESS_READ( NOUT, LERR, SIN, ROVER, RLOWER,
     &				   SPMAX, SPNO, SPXYZ, SPRAD, SPLAST,
     &				   SPSEC, SPCONN, SPEFFR, LCAPS,
     & 				   SAMPLE, CVECT, PERPVE, PERPVN,
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
C Modification history:
C
C Date  Author          Modification
C prior O.S.S.		part of sphqpt program
C 11/00 O.S. Smart      Original version
C    
C This s/r reads the .sph file for sph_process program

! arguments for routine
      ! output stream number - for error messages etc.
      ! return unchanged
      INTEGER                   NOUT 	    
      ! error indicator - set true if problem
      LOGICAL                   LERR
  
      ! inputfilename stream number 
      ! return unchanged
      INTEGER			SIN         
            			
      ! override radius?
      DOUBLE PRECISION		ROVER
      ! lower limit radius - any smaller than this and ignore on read
      DOUBLE PRECISION		RLOWER

      ! sphere centre storage arrays
      ! maximum number (return unchanged), actual number, co-ords and radius
      ! logical indicates whether the record is the last centre
      INTEGER                   SPMAX, SPNO
      DOUBLE PRECISION          SPXYZ( 3, SPMAX), SPRAD( SPMAX)
      LOGICAL                   SPLAST( SPMAX)
      ! capsule option needs to have the second centre
      ! n.b. SPRAD is the real capsule radius not the effective rad`
      DOUBLE PRECISION          SPSEC( 3, SPMAX)
      ! store whether point is product of connolly - pick up by -999 residue number
      LOGICAL			SPCONN( SPMAX)
      
      ! need effective radius for colouring connolly
      DOUBLE PRECISION		SPEFFR( SPMAX)

      ! logical vble capsules on
      LOGICAL			LCAPS

! vbles for forpeg option - hole channel vector etc.
      DOUBLE PRECISION		CVECT(3)
! unit vectors in plane normal to cvect
! needed to a number of routines - originally h2dmap but now conn
! and addend
! these are normal - first is east direction (x on plane)
!                    and second is north     (y on plane)
      DOUBLE PRECISION		PERPVE(3), PERPVN(3)
! a sampling distance - the distance between planes
      DOUBLE PRECISION		SAMPLE
! option CONN introduced 1/00 - 
! CONNR(1) is the connolly surface probe radius (default 1.15Angs)
!          the option is turned off by setting this radius negative
! CONNR(2) is the grid size to be used in the calculation - default 1/2 probe
      DOUBLE PRECISION		CONNR(2)
! the radius at which an end is reached
      DOUBLE PRECISION		ENDRAD

C internals
      INTEGER			XCOUNT
      INTEGER			NIGN ! number of ignored records
      ! string for text records in hydra file
      CHARACTER*80              STRING
      INTEGER			IOREAD ! for read

C end of decs ******************

C have opened input and output files
C and got initial conditions start work

C no sphere centres to start with
      SPNO = 0
      NIGN = 0


C read in input file - do until EOF reached
10    CONTINUE
	READ( SIN, '(A)', END= 20) STRING
C do we have a LAST-REC-END record?
        IF (STRING(1:12).EQ.'LAST-REC-END') THEN
          SPLAST(SPNO)   = .TRUE.
	 
        ELSEIF (STRING(1:6).EQ.'SAMPLE') THEN ! pick up for peg records
	  READ( STRING(7:18),'(F12.6)', IOSTAT=IOREAD) SAMPLE
	  IF (IOREAD.NE.0) THEN
	    LERR = .TRUE.
	    WRITE( NOUT, '(A)') 'Error trying to read SAMPLE record'
	    GOTO 55555
	  ENDIF
	  
        ELSEIF (STRING(1:6).EQ.'CVECT ') THEN ! pick up for peg records
	  READ( STRING(7:42),'(3F12.6)', IOSTAT=IOREAD)CVECT
	  IF (IOREAD.NE.0) THEN
	    LERR = .TRUE.
	    WRITE( NOUT, '(A)') 'Error trying to read CVECT record'
	    GOTO 55555
	  ENDIF
	
        ELSEIF (STRING(1:6).EQ.'PERPVE') THEN ! pick up for peg records
	  READ( STRING(7:42),'(3F12.6)', IOSTAT=IOREAD)PERPVE
	  IF (IOREAD.NE.0) THEN
	    LERR = .TRUE.
	    WRITE( NOUT, '(A)') 'Error trying to read PERPVE record'
	    GOTO 55555
	  ENDIF

        ELSEIF (STRING(1:6).EQ.'PERPVN') THEN ! pick up for peg records
	  READ( STRING(7:42),'(3F12.6)', IOSTAT=IOREAD)PERPVN
	  IF (IOREAD.NE.0) THEN
	    LERR = .TRUE.
	    WRITE( NOUT, '(A)') 'Error trying to read PERPVN record'
	    GOTO 55555
	  ENDIF
	
        ELSEIF (STRING(1:6).EQ.'CONNR ') THEN ! pick up for peg records
	  READ( STRING(7:30),'(2F12.6)', IOSTAT=IOREAD)CONNR
	  IF (IOREAD.NE.0) THEN
	    LERR = .TRUE.
	    WRITE( NOUT, '(A)') 'Error trying to read CONNR record'
	    GOTO 55555
	  ENDIF

        ELSEIF (STRING(1:6).EQ.'ENDRAD ') THEN ! pick up for peg records
	  READ( STRING(7:18),'(2F12.6)', IOSTAT=IOREAD)ENDRAD
	  IF (IOREAD.NE.0) THEN
	    LERR = .TRUE.
	    WRITE( NOUT, '(A)') 'Error trying to read ENDRAD record'
	    GOTO 55555
	  ENDIF

        ENDIF


	IF (STRING(1:4).NE.'ATOM') GOTO 10

C 'atom' read do we have a qss record?
	IF ((STRING(11:22).EQ.'1  QSS SPH S') .OR.
     &      (STRING(11:22).EQ.'1  QC1 SPH S')      ) THEN
C qss record - one more sphere centre
	  SPNO = SPNO + 1
	  SPLAST(SPNO) = .FALSE.
	  SPCONN(SPNO) = .FALSE.
	  IF (SPNO.GT.SPMAX) THEN
	    WRITE( NOUT, '(A)')
     &' *** ERROR ***'//CHAR(7),
     &' Array bound for storage of sphere centres exceeded',
     &' Please increase SPMAX in sphqpt',
     &' Aborting!'//CHAR(7)
            LERR = .TRUE.
	    GOTO 55555
	  ENDIF
C read co-ords 
	  READ( STRING(31:54),'(3F8.3)') 
     &      (SPXYZ( XCOUNT, SPNO), XCOUNT= 1,3)
C read radius
	  READ( STRING(55:60),'(F6.2)') SPRAD( SPNO)
C ignore this record?
          IF (SPRAD(SPNO).LT.RLOWER) THEN
C get rid of it 
            SPNO = SPNO - 1
	    NIGN = NIGN + 1
	    GOTO 10
	  ENDIF
C overide?
	  IF (ROVER.NE.0) SPRAD( SPNO) = ROVER
C for safety set the second centre equal to the first 
C (this is not a capsule)
	  SPSEC(1,SPNO) = SPXYZ(1,SPNO)
	  SPSEC(2,SPNO) = SPXYZ(2,SPNO)
	  SPSEC(3,SPNO) = SPXYZ(3,SPNO)

! read effective radius from 61 to 66
	  READ( STRING(61:66),'(F6.2)') SPEFFR( SPNO)
	  ! deal with old .sph format that had zero in column
          IF (ABS(SPEFFR(SPNO)).LT.1E-06) SPEFFR(SPNO) = SPRAD(SPNO) 
	  
C connolly record
          IF (STRING(23:26).EQ.'-999') SPCONN(SPNO) = .TRUE.

	  IF (STRING(11:22).EQ.'1  QC1 SPH S') THEN
C we have a capsule record
C is this first
            IF (.NOT.LCAPS) THEN
              LCAPS = .TRUE.
              WRITE( NOUT, '(A)') '      (Have capsule records)'
            ENDIF
            
C so read second centre info
340         CONTINUE
	    READ( SIN, '(A)') STRING
   	    IF (STRING(1:4).NE.'ATOM') GOTO 340

C 'atom' read do we have a correct record
	    IF (STRING(11:22).EQ.'1  QC2 SPH S') THEN
C read second centre co-ords 
	      READ( STRING(31:54),'(3F8.3)') 
     &         (SPSEC( XCOUNT, SPNO), XCOUNT= 1,3)

	    ELSE
	      WRITE( NOUT, '(A)') 
     &' *** ERROR ***'//CHAR(7),
     &' record read not "QC2 SPH S" atom',
     &' produced by SPHPDB option of hole',
     &' Aborting!'//CHAR(7)
              LERR = .TRUE.
	      GOTO 55555
	    ENDIF
	  ENDIF

C have read an unrecognized record
        ELSE
	  WRITE( NOUT, '(A)') 
     &' *** ERROR ***'//CHAR(7),
     &' record read not "QSS SPH S" atom',
     &' produced by SPHPDB option of hole',
     &' Aborting!'//CHAR(7)
          LERR = .TRUE.
	  GOTO 55555
	ENDIF

C read next record
	GOTO 10
	
20    CONTINUE
C end of do until loop end of input file reached 
C - close it
      CLOSE( SIN)

      WRITE( NOUT, '(A,I5,A)')
     &' (Have read ', SPNO, ' records)'
      IF (NIGN.NE.0) WRITE( NOUT, '(A,I5,A)')
     &' (ignored a further ', NIGN, ' as too small)'

55555 RETURN
      END
