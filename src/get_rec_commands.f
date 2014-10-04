      SUBROUTINE GET_REC_COMMANDS( NOUT, LERR,
     &			     REC_COMMANDS_HELP_MESSAGE,
     &                       REC_TOT, REC_COMMANDS, 
     &                       REC_REQUIRE_NUM, REC_REQUIRE_STRING,
     &                       REC_FOUND, REC_NUM, REC_NUM_NUM,
     &			     REC_STRING, REC_STRING_NUM,
     &			     FINPUT_ASK, FINPUT, FOUTPUT_ASK, FOUTPUT)
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
C s/r to Get command line arguements, filenames and values into a hole routine
C
C Modification history:
C
C Date  Author          Modification
C 11/00 O.S.S.		First version
C
C 
      ! passed vbles
      INTEGER			NOUT ! output streams (NO NEED FOR NIN - command
                                     ! line driven
      LOGICAL			LERR  ! success/failure indicator
				     
      ! vbles for get_rec_commands
      CHARACTER*(*) 		REC_COMMANDS_HELP_MESSAGE
      INTEGER     		REC_TOT  ! actual number of arguements
      CHARACTER*25		REC_COMMANDS(REC_TOT)
      INTEGER			REC_REQUIRE_NUM(REC_TOT)
      INTEGER			REC_REQUIRE_STRING(REC_TOT)
      LOGICAL			REC_FOUND( REC_TOT)
      DOUBLE PRECISION		REC_NUM( 10, REC_TOT)
      INTEGER			REC_NUM_NUM( REC_TOT)! the number of numbers
      CHARACTER*200		REC_STRING(10, REC_TOT)
      INTEGER			REC_STRING_NUM( REC_TOT)! the number of numbers

      LOGICAL			FINPUT_ASK ! ask for input filename
      CHARACTER*200		FINPUT     ! inputfilename (penultimate argument)
      LOGICAL			FOUTPUT_ASK ! ask for input filename
      CHARACTER*200		FOUTPUT ! outputfilename (last arguement)

 

! internals
      INTEGER			ARGNUM ! number of command line arguments
      INTEGER			RCOUNT, NCOUNT
      INTEGER			ACOUNT ! loop count args
      CHARACTER*50		ARG_ACOUNT ! an argument
      INTEGER			IOREAD
      INTEGER			NCHECK
      CHARACTER*200		ARG_NCHECK ! for command qualifiers
      INTEGER			ARG_NCHECK_LASTC
      DOUBLE PRECISION		TEST_NUM

      INTEGER			HELPEND ! last character of help message
      INTEGER			LASTC ! last character of an argument
      INTEGER			IARGC
      
! end of decs*******************! end of decs      


      DO RCOUNT = 1, REC_TOT  ! initialize found vbles
        REC_FOUND( RCOUNT) = .FALSE.
	REC_NUM_NUM(RCOUNT) = 0
	REC_STRING_NUM(RCOUNT) = 0
	DO NCOUNT = 1, 10
	  REC_NUM(NCOUNT,RCOUNT)=-1E10
	  REC_STRING(NCOUNT,RCOUNT ) = '##########'
	ENDDO
      ENDDO
      ! last character of help message
      CALL CHREND(REC_COMMANDS_HELP_MESSAGE, HELPEND) 
  
  
! Get the number of arguments on the command line
      ARGNUM = iargc() ! number of args
                       ! - will be reduced once input/output filenames picked
      
! -h for help message
      IF (ARGNUM.EQ.1) THEN
        CALL GETARG( 1, ARG_ACOUNT) 
	IF (ARG_ACOUNT(1:2).EQ.'-h') THEN
	  WRITE( NOUT, '(A)') REC_COMMANDS_HELP_MESSAGE(1:HELPEND)
          LERR = .TRUE.
          GOTO 55555
	ENDIF
      ENDIF

! pickup filenames if required
! output filename should be last
      IF (FOUTPUT_ASK) THEN ! do we need output filename
        IF (ARGNUM.LT.1) THEN
	  WRITE( NOUT, '(A)') REC_COMMANDS_HELP_MESSAGE(1:HELPEND)
          WRITE( NOUT, '(A)')
     &' ERROR ',
     &' must specify output filename and this cannot begin with a -'
 	  LERR = .TRUE.
	  GOTO 55555
	ENDIF
	CALL GETARG( ARGNUM,   FOUTPUT) ! get outputfilename
	IF (FOUTPUT(1:1).EQ.'-') THEN
	  WRITE( NOUT, '(A)') REC_COMMANDS_HELP_MESSAGE(1:HELPEND)
          WRITE( NOUT, '(A)')
     &' ERROR ',
     &' must specify output filename and this cannot begin with a -'
	  LERR = .TRUE.
	  GOTO 55555
	ENDIF
	ARGNUM = ARGNUM - 1	! reduce the arguement number by 1
      ENDIF
      
! next input
      IF (FINPUT_ASK) THEN ! do we need input filename
        IF (ARGNUM.LT.1) THEN
	  WRITE( NOUT, '(A)') REC_COMMANDS_HELP_MESSAGE(1:HELPEND)
          WRITE( NOUT, '(A)')
     &' ERROR ',
     &' must specify input filename and this cannot begin with a -'
 	  LERR = .TRUE.
	  GOTO 55555
	ENDIF
	CALL GETARG( ARGNUM,   FINPUT) ! get inputfilename
	IF (FinPUT(1:1).EQ.'-') THEN
	  WRITE( NOUT, '(A)') REC_COMMANDS_HELP_MESSAGE(1:HELPEND)
          WRITE( NOUT, '(A)')
     &' ERROR ',
     &' must specify input filename and this cannot begin with a -'
	  LERR = .TRUE.
	  GOTO 55555
	ENDIF
	ARGNUM = ARGNUM - 1	! reduce the arguement number by 1
      ENDIF


      DO ACOUNT = 1, ARGNUM ! go thru remaining arguments 
        CALL GETARG( ACOUNT, ARG_ACOUNT)  ! get the argument
	IF (ARG_ACOUNT(1:1).EQ.'-') THEN ! Q: is this a command? ANS:yes
	  ! have got an argument
	  CALL CHREND(ARG_ACOUNT, LASTC)

	  ! so go thru REC_COMMANDS to find a match
	  DO RCOUNT = 1, REC_TOT ! go the recognized list
	    ! our argument must match the whole of an command
	    IF (REC_COMMANDS(RCOUNT)(1:LASTC-1) .EQ.
     &              ARG_ACOUNT(2:LASTC)         ) THEN
              ! have a match - record it
              REC_FOUND(RCOUNT) = .TRUE.
	      ! record it in the string to pickup unrecognized
	      ARG_ACOUNT(1:1) = '*'
	      !write(*,*) 'found '//rec_commands(rcount)(1:lastc-1) !debug
              ! get subsequent strings and numbers from rest of commands
              ! go thru subsequent arguements until we get
	      ! either to the end or another commands
	      NCHECK = ACOUNT+1
15            CONTINUE ! a do-until
	      IF (NCHECK.LE.ARGNUM) THEN ! checking qualifiers
	        ! check argument ncheck
		CALL GETARG( NCHECK, ARG_NCHECK)
		IF (ARG_NCHECK(1:1).NE.'-') THEN ! there is an qualifier of the arg
		  ! have one more qualifier 
		  REC_STRING_NUM(RCOUNT) = REC_STRING_NUM(RCOUNT) + 1
		  IF (REC_STRING_NUM(RCOUNT).GT.10) THEN ! cannot cope with more than 10 qualifiers
          	    WRITE( NOUT, '(A)') 
     & REC_COMMANDS_HELP_MESSAGE(1:HELPEND)
                    WRITE( NOUT, '(A)')
     &' ERROR ',
     &' have found argument ''-'//ARG_ACOUNT(2:LASTC)//'''',
     &' has been specified with more than 10 qualifiers after it',
     &' cannot cope!'
	            LERR = .TRUE.
	            GOTO 55555	     
		  ENDIF ! (cannot cope with more than 10 qualifiers)
                  ! store string
		  REC_STRING(REC_STRING_NUM(RCOUNT),RCOUNT) = ARG_NCHECK
		  ! now lets try to read a number from (as real)
		  CALL CHREND( ARG_NCHECK, ARG_NCHECK_LASTC)
		  READ( ARG_NCHECK(1:ARG_NCHECK_LASTC), *, 
     &                  IOSTAT=IOREAD)    TEST_NUM
		  !debugwrite(*,*) '*'//arg_ncheck(1:arg_ncheck_lastc)//'*'
		  !debugwrite(*,*) test_num, ioread
		  
		  IF (IOREAD.EQ.0) THEN ! Q: has there been an error on the read?
		    ! ANS NO - store the number
		    REC_NUM_NUM(RCOUNT)= REC_NUM_NUM(RCOUNT) + 1
		    ! cannot cope with more than 10 qualifiers
		    ! but this has already been checked - each number must 
		    ! come from a string
		    REC_NUM( REC_NUM_NUM(RCOUNT), RCOUNT) = TEST_NUM ! ANS NO - store the number	    
		  ENDIF! (Q: has there been an error on the read?)
                  NCHECK = NCHECK + 1 ! check the next qualifier
		  GOTO 15 ! do until no more qualifiers		  
		ENDIF ! (there is an qualifier of the arg)
	      ENDIF ! end of checking qualifiers
   	      ! Q: have we found a number or string if we must?
              IF (REC_REQUIRE_NUM(RCOUNT).GT. REC_NUM_NUM(RCOUNT)) THEN !Q: do we have enough numbers?
	         ! ans: not enough numbers
          	 WRITE( NOUT, '(A)') 
     & REC_COMMANDS_HELP_MESSAGE(1:HELPEND)
                 WRITE( NOUT, '(A/A/A,I5,A/A)')
     &' ERROR ',
     &' have found argument ''-'//ARG_ACOUNT(2:LASTC)//'''',
     &' but this must be followed by at least ',
     &                   REC_REQUIRE_NUM(RCOUNT), ' numbers ',
     &' cannot proceed!'
	         LERR = .TRUE.
	         GOTO 55555	     
	      
              ELSEIF (REC_REQUIRE_STRING(RCOUNT).GT. 
     &                REC_STRING_NUM(RCOUNT)         ) THEN !Q: do we have enough strings?
	         ! ans: not enough strings
          	 WRITE( NOUT, '(A)') 
     & REC_COMMANDS_HELP_MESSAGE(1:HELPEND)
                 WRITE( NOUT, '(A/A/A,I5,A/A)')
     &' ERROR ',
     &' have found argument ''-'//ARG_ACOUNT(2:LASTC)//'''',
     &' but this must be followed by at least ',
     &                   REC_REQUIRE_STRING(RCOUNT), ' words ',
     &' cannot proceed!'
	         LERR = .TRUE.
	         GOTO 55555	     
	      ENDIF! (Q: do we have enough strings?)

            ENDIF	    	  
	  ENDDO ! end of going thru the recognized list
	  ! have we found a match? if so then * will be replace - as first
	  ! character of arg_acount
	  IF (ARG_ACOUNT(1:1).NE.'*') THEN ! Q: unrecognized? ANS: yes
	    WRITE( NOUT, '(A)') REC_COMMANDS_HELP_MESSAGE(1:HELPEND)
            WRITE( NOUT, '(A)')
     &' ERROR ',
     &' have found an unrecognized argument '''//ARG_ACOUNT(1:LASTC)
     & 							     //'''',
     &' Recognized commands: ',
     &('   -'//REC_COMMANDS(RCOUNT), RCOUNT = 1, REC_TOT)
	    LERR = .TRUE.
	    GOTO 55555	     
	  ENDIF ! {Q: unrecognized? ANS: yes}
        ENDIF  ! {Q: is this a command? ANS:yes}
      ENDDO ! go thru remaining arguments 



!         ! have got the filename - now go for any arguments
!        DO ACOUNT =1, ARGNUM-2 ! go thru arguments except filenames
!!	  CALL GETARG( ACOUNT, ARG_ACOUNT)
!	  IF (ARG_ACOUNT(1:4).EQ.'-qpt') THEN
!            TYPEOUT = 1 ! output to qpt
!
!          ELSEIF (ARG_ACOUNT(1:5).EQ.'-norm') THEN
!            DRAW_NORMAL_LENGTH = 0.5
!            IF (ARG_ACOUNT(6:6).EQ.'=') THEN ! a value has been specified
!              READ(ARG_ACOUNT(7:50),*,IOSTAT=IOREAD) 
!     &                          DRAW_NORMAL_LENGTH ! read the length
!              IF (IOREAD.NE.0) THEN
!                WRITE( NOUT, '(A)')
!     &'ERROR reading value from after -norm argument',
!     &' record= '//ARG_ACOUNT
!	        CALL VMD_TRIANGLES_TO_LINE_HELP_MESSAGE( NOUT) ! long message
!                LSUCCESS = .FALSE.
!                GOTO 55555
!              ENDIF
!            ENDIF
!
!           ELSEIF (ARG_ACOUNT(1:7).EQ.'-width=') THEN
!             READ(ARG_ACOUNT(8:50),*,IOSTAT=IOREAD) 
!     &                          WIDTH ! read the width
!             IF (IOREAD.NE.0) THEN
!               WRITE( NOUT, '(A)')
!     &'ERROR reading value from after -width= argument',
!     &' record= '//ARG_ACOUNT
!	       CALL VMD_TRIANGLES_TO_LINE_HELP_MESSAGE( NOUT) ! long message
!               LSUCCESS = .FALSE.
!               GOTO 55555
!            ENDIF
!
!          ELSE
!            WRITE( NOUT, '(A)')
!     &'ERROR unrecognized argument: '//ARG_ACOUNT
!	    CALL VMD_TRIANGLES_TO_LINE_HELP_MESSAGE( NOUT) ! long message
!            LSUCCESS = .FALSE.
!            GOTO 55555
!
!          ENDIF
!
!        ENDDO ! go thru arguments except filenames
!	LSUCCESS = .TRUE.
!      ENDIF

55555 RETURN
      END

