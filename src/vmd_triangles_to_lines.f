      PROGRAM VMD_TRIANGLE_TO_LINES
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
C Takes a file vmd containing triangles 
C and converts it into either (a) a qpt file (b) direct to vmd draw
C
C Modification history:
C
C Date  Author          Modification
C 11/00 O.S.S.		First version
C

      INTEGER			NOUT ! output streams (NO NEED FOR NIN - command
                                     ! line driven
      PARAMETER(		NOUT = 6)

      CHARACTER*200		FINPUT ! inputfilename (penultimate argument)
      INTEGER			SINPUT ! its stream number
      INTEGER			END_FINPUT ! last character in filename
      CHARACTER*200		FOUTPUT ! inputfilename (penultimate argument)
      INTEGER			SOUTPUT ! its stream number
      INTEGER			END_FOUTPUT ! last character in filename
     
      INTEGER			TYPEOUT ! = 0 for vmd, 1 for qpt
      DOUBLE PRECISION		DRAW_NORMAL_LENGTH ! positive if we wish to draw normals
      DOUBLE PRECISION		LEN_NORM ! vble to get norm vector to desired length
      
      LOGICAL			LERR            ! success/failure indicator
      LOGICAL			LBIN		! binary file indicator

      LOGICAL                   OPENRO	! open files readonly function

      CHARACTER*300		LINE 	! for input LONG
      INTEGER			LINE_END
      INTEGER			IOREAD, IOREAD2  ! for the iostat indicator
      INTEGER			NUMBER_CONVERTED ! number of records converted
      INTEGER			C_NO_START, C_NO_FINISH ! character positions for the picking out triplets

      DOUBLE PRECISION 		XYZ_TRIANGLE(3,3) ! triangle coords
      DOUBLE PRECISION 		XYZ_NORM(3,3)     ! normal vector for each one

      INTEGER			XCOUNT, TCOUNT
      INTEGER			WIDTH		  ! width in pixels for vmd draw

      ! vbles for get_rec_commands
      CHARACTER*2000 		REC_COMMANDS_HELP_MESSAGE
      INTEGER			REC_MAX  ! array bound
      PARAMETER(		REC_MAX = 15)
      INTEGER     		REC_TOT  ! actual number of arguements
      CHARACTER*25		REC_COMMANDS(REC_MAX)
      INTEGER			REC_REQUIRE_NUM(REC_MAX)
      INTEGER			REC_REQUIRE_STRING(REC_MAX)
      INTEGER			RCOUNT
      LOGICAL			REC_FOUND( REC_MAX)
      DOUBLE PRECISION		REC_NUM( 10, REC_MAX)
      INTEGER			REC_NUM_NUM( REC_MAX)
      CHARACTER*200		REC_STRING( 10, REC_MAX)
      INTEGER			REC_STRING_NUM( REC_MAX)
 
! end of decs*******************

      LERR = .FALSE.    ! initial values
      NUMBER_CONVERTED = 0 
      WIDTH = 1


      WRITE( NOUT,  '(A)') ! greetings
     &' *** Program vmd_triangles_to_lines ***',
     &' Copyright 2000,2004 by Oliver Smart',
     &' Copyright 2014-2015 SmartSci Limited, All rights reserved.'
      CALL VERTIM( NOUT) !  write link time of program to screen

      ! establish values for s/r get
      REC_COMMANDS_HELP_MESSAGE =
     &'                     (help message start)\n'//
     &' vmd_triangles_to_lines converts a vmd format triangle file\n'//
     &' into a series of lines (a mesh) in a qpt file or vmd file.\n'//
     &' \n'//
     &' Usage: '//
     &' vmd_triangles_to_lines (-qpt) (-norm (Y)) (-width X)'//
     &                                    ' input_file output_file\n'//
     &' options: \n'//
     &'  -qpt:  produce a qpt file rather than vmd \n'//
     &'  -norm (Y): output surface normals rather than triangles\n'//
     &'         n.b. these must be present in input file - \n'//
     &'              i.e. input must be a smoothed surface. \n'//
     &'         If Y is specified then the the normal vectors will\n'//
     &'              Y angstroms long (Y is a real - default 0.5)\n'//
     &'  -width X make the vmd draw record X pixels rather than 1'//
     &                                         ' (X is an integer)\n'//
     &'  -h: to print this help message'
!      call chrend( rec_commands_help_message, line_end)
!      write( nout, '(a)') rec_commands_help_message(1:line_end)
      REC_TOT = 3
      DO RCOUNT = 1,REC_TOT          ! initialize require store
        REC_REQUIRE_NUM(RCOUNT) = 0
	REC_REQUIRE_STRING(RCOUNT) = 0
      ENDDO 
      REC_COMMANDS(1) = 'qpt'     ! do not specify the -
      REC_COMMANDS(2) = 'norm'    ! 
      REC_COMMANDS(3) = 'width'   !
      REC_REQUIRE_NUM(3) = 1 ! require single value after width
      CALL GET_REC_COMMANDS( NOUT, LERR,
     &			     REC_COMMANDS_HELP_MESSAGE,
     &                       REC_TOT, REC_COMMANDS, 
     &                       REC_REQUIRE_NUM, REC_REQUIRE_STRING,
     &                       REC_FOUND, REC_NUM, REC_NUM_NUM,
     &			     REC_STRING, REC_STRING_NUM,
     &			     .TRUE., FINPUT, .TRUE., FOUTPUT)
      IF (LERR) GOTO 55555
      
      TYPEOUT = 0 		! default vmd output
      DRAW_NORMAL_LENGTH = -1. 	! do triangles rather than normals
      
      IF (REC_FOUND(1)) TYPEOUT = 1 ! -qpt card
      IF (REC_FOUND(2)) THEN
        DRAW_NORMAL_LENGTH = 0.5    ! turn on
	IF (REC_NUM(1,2).GT.-1E10) DRAW_NORMAL_LENGTH = REC_NUM(1,2) ! value specified
      ENDIF
      IF (REC_FOUND(3)) WIDTH = REC_NUM(1,3) ! width must have a value


      ! find last character in each filename
      CALL CHREND( FOUTPUT, END_FOUTPUT)
      CALL CHREND( FINPUT, END_FINPUT)

! open the input file as readonly
      IF (.NOT.OPENRO( SINPUT, FINPUT, NOUT)) THEN
        WRITE( NOUT, '(A,A,A)')
     &' Error cannot open input file: ''', FINPUT(1:END_FINPUT), ''''
        LERR = .TRUE. ! have failed
        GOTO 55555
      ENDIF

! open the output file as new
! use s/r newop - this keeps old version removes tilda's spaces etc. etc.
! arguments output stream, error indicator,
! reduce_output_variable (0 for full), filename, filestream, binary indicator
      LBIN = .FALSE.
      IF (TYPEOUT.EQ.1) LBIN = .TRUE. ! qpt files are binary
      CALL NEWOP( NOUT, LERR, 0,
     &                  FOUTPUT, SOUTPUT, LBIN)
      IF (LERR) THEN
        WRITE( NOUT, '(A,A,A)')
     &' Error cannot open output file: ''', FOUTPUT(1:END_FOUTPUT), ''''
        LERR = .TRUE. ! have failed
        GOTO 55555
      ENDIF
      

      WRITE( NOUT, '(/ A,A,A/ A,A,A)') 
     &'Have opened ', FINPUT(1:END_FINPUT),   ' for input ',
     &'        and ', FOUTPUT(1:END_FOUTPUT), ' for output'
      IF (TYPEOUT.EQ.0) THEN ! vmd or qpt? ans:vmd
	WRITE( NOUT, '(A)') 
     &'vmd draw output option used '
      ELSEIF (TYPEOUT.EQ.1) THEN ! vmd or qpt? ans:qpt
	WRITE( NOUT, '(A)') 
     &'qpt (quanta plot file) output option used '
      ENDIF ! vmd or qpt? ans:qpt

! normal vectors should be yellow
      IF (DRAW_NORMAL_LENGTH.GT.0) THEN ! doing normals? yes: switch to yellow
        IF (TYPEOUT.EQ.0) THEN ! vmd or qpt? ans:vmd
          WRITE(SOUTPUT,'(A)') 'draw color yellow'
        ELSEIF (TYPEOUT.EQ.1) THEN ! vmd or qpt? ans:qpt
          WRITE(SOUTPUT) 1., 4., -55., 15. ! switch to yellow
        ENDIF ! vmd or qpt? ans:qpt
      ENDIF ! doing normals? 

! everything opened and setup now lets work
      IOREAD = 0
      DO WHILE (IOREAD.EQ.0)
        READ(SINPUT, '(A)', IOSTAT=IOREAD) LINE
C no on read error is indicated by IOSTAT being zero
C                                               -1 indicates EOF
        IF (IOREAD.EQ.-1) THEN ! EOF?
          CONTINUE ! will automatically jump out of loop
        ELSEIF (IOREAD.NE.0) THEN ! error on read 
          LERR= .TRUE.
          WRITE( NOUT, '(A,I5)')
     &' Error on read - IOSTAT= ', IOREAD
          GOTO 55555 ! abort
        ELSE ! no error process line


! is the line:
!draw triangle
!draw trinorm
!12345678912345
          IF (LINE(1:8).EQ.'draw tri') THEN ! do we have a triangle
            NUMBER_CONVERTED = NUMBER_CONVERTED + 1   ! keep a count 
            IF (LINE(14:14).EQ.'e') LINE(14:14) = ' ' ! get rid of e in
                                                      ! triangle 
! now pick up the three sets of x y and z coords from the rest of             !
! line. These triplets are surrounded by { }'s             
            C_NO_START = 14 ! look for the left curly bracket from 14        
            DO TCOUNT = 1, 3 ! go thru triplet
               ! to the right of the {
               IF (INDEX(LINE(C_NO_START:300),'{').EQ.0) THEN
                 ! cannot find bracket
                 WRITE( NOUT, '(A,I5/ A,I5/ A,A,A/ A)')
     &' Error in reading triangle coords for record ', NUMBER_CONVERTED,
     &'   on trying to read triangle ', TCOUNT,
     &'   from line  ''', LINE, '''',
     &' Aborting - (please report this bug with the input file)'
                 LERR = .TRUE.
                 GOTO 55555
               ENDIF

               C_NO_START = C_NO_START + 
     &                      INDEX(LINE(C_NO_START:300),'{')                 
               ! to the left of the }
               C_NO_FINISH =        C_NO_START + 
     &                      INDEX(LINE(C_NO_START:300),'}') -2 
               ! debug           write(*,'(a)') '*'//line(c_no_start:c_no_finish)//'*'
	       
               ! read the coords in from this line segment
               READ(LINE(C_NO_START:C_NO_FINISH),*,IOSTAT=IOREAD2)     
     &               (XYZ_TRIANGLE( XCOUNT, TCOUNT), XCOUNT = 1, 3)
               IF (IOREAD2.NE.0) THEN
                 WRITE( NOUT, '(A,I5/ A,I5/ A,A,A/ A)')
     &' Error in reading triangle coords for record ', NUMBER_CONVERTED,
     &'   on trying to read triangle ', TCOUNT,
     &'   from line segment: ''', LINE(C_NO_START:C_NO_FINISH), '''',
     &' Aborting - (please report this bug with the input file)'
                LERR = .TRUE.
                GOTO 55555
              ENDIF
            ENDDO  ! go thru triplet

! now lets draw the triangle 
! - unless doing normals - when draw_normal_length will be more than zero
            IF ( (TYPEOUT.EQ.0)       .AND.        ! not doing normals
     &           (DRAW_NORMAL_LENGTH.LT.0.) ) THEN ! vmd or qpt? ans: VMD
              WRITE( SOUTPUT, '(A,3F9.3,A,3F9.3,A,I5)') ! 1->2
     &'draw line  {', 
     &     (XYZ_TRIANGLE(XCOUNT,1), XCOUNT= 1,3),
     &           '} {', 
     &     (XYZ_TRIANGLE(XCOUNT,2), XCOUNT= 1,3),
     &             '} width ', WIDTH
              WRITE( SOUTPUT, '(A,3F9.3,A,3F9.3,A,I5)') ! 2->3
     &'draw line  {', 
     &     (XYZ_TRIANGLE(XCOUNT,2), XCOUNT= 1,3),
     &           '} {', 
     &     (XYZ_TRIANGLE(XCOUNT,3), XCOUNT= 1,3),
     &             '} width ', WIDTH
              WRITE( SOUTPUT, '(A,3F9.3,A,3F9.3,A,I5)') ! 3->1
     &'draw line  {', 
     &     (XYZ_TRIANGLE(XCOUNT,3), XCOUNT= 1,3),
     &           '} {', 
     &     (XYZ_TRIANGLE(XCOUNT,1), XCOUNT= 1,3),
     &             '} width ', WIDTH

            ELSEIF ( (TYPEOUT.EQ.1)       .AND.    ! doing normals??
     &               (DRAW_NORMAL_LENGTH.LT.0.) ) THEN ! vmd or qpt? ans: QPT
              WRITE(SOUTPUT) ! move to 1
     &          2.0, (REAL(XYZ_TRIANGLE(XCOUNT,1)),XCOUNT=1,3) 
              WRITE(SOUTPUT) ! draw to 2
     &          3.0, (REAL(XYZ_TRIANGLE(XCOUNT,2)),XCOUNT=1,3)
              WRITE(SOUTPUT) ! draw to 3
     &          3.0, (REAL(XYZ_TRIANGLE(XCOUNT,3)),XCOUNT=1,3)
              WRITE(SOUTPUT) ! draw to 1
     &         3.0, (REAL(XYZ_TRIANGLE(XCOUNT,1)),XCOUNT=1,3)
            ENDIF ! vmd or qpt (now lets draw the triangle)

! are we doing normals?
            IF (DRAW_NORMAL_LENGTH.GT.0) THEN ! doing normals?
! record must be a trinorm rather than triangle
                                !123456789012
               IF (LINE(1:12).NE.'draw trinorm') THEN ! do we have a trinorm record
                  WRITE( NOUT, '(A)')
     &' ERROR ', 
     &' Have specified -norm option',
     &' but input file does contain normal vectors',
     &' Aborting '
                 LERR = .TRUE.
                 GOTO 55555
              ELSE
! read norm records - using routine above
                DO TCOUNT = 1, 3 ! go thru triplet
                  ! to the right of the {
                  IF (INDEX(LINE(C_NO_START:300),'{').EQ.0) THEN
                    ! cannot find bracket
                    WRITE( NOUT, '(A,I5/ A,I5/ A,A,A/ A)')
     &' Error in reading norm coords for record ', NUMBER_CONVERTED,
     &'   on trying to read trinorm ', TCOUNT,
     &'   from line  ''', LINE, '''',
     &' Aborting - (please report this bug with the input file)'
                    LERR = .TRUE.
                    GOTO 55555
                  ENDIF

                  C_NO_START = C_NO_START + 
     &                      INDEX(LINE(C_NO_START:300),'{')                 
                  ! to the left of the }
                  C_NO_FINISH =        C_NO_START + 
     &                      INDEX(LINE(C_NO_START:300),'}') -2 

                  ! read the coords in from this line segment
                  READ(LINE(C_NO_START:C_NO_FINISH),*,IOSTAT=IOREAD2)     
     &                (XYZ_NORM( XCOUNT, TCOUNT), XCOUNT = 1, 3)
                  IF (IOREAD2.NE.0) THEN
                    WRITE( NOUT, '(A,I5/ A,I5/ A,A,A/ A)')
     &' Error in reading triangle coords for record ', NUMBER_CONVERTED,
     &'   on trying to read triangle ', TCOUNT,
     &'   from line segment: ''', LINE(C_NO_START:C_NO_FINISH), '''',
     &' Aborting - (please report this bug with the input file)'
                    LERR = .TRUE.
                    GOTO 55555
                  ENDIF
                ENDDO  ! go thru triplet

                ! now have normal vectors in xyz_norm(xyz,1) etc.
                ! get them to the desired length DRAW_NORMAL_LENGTH
                DO TCOUNT = 1, 3 ! go thru triplet
                  LEN_NORM = XYZ_NORM(1,TCOUNT)*XYZ_NORM(1,TCOUNT) +
     &                       XYZ_NORM(2,TCOUNT)*XYZ_NORM(2,TCOUNT) + 
     &                       XYZ_NORM(3,TCOUNT)*XYZ_NORM(3,TCOUNT) 
                  LEN_NORM = SQRT( LEN_NORM)
                  IF (LEN_NORM.LT.0.0001) THEN
                    WRITE(NOUT,'(A,I5/ A/ A/)')
     &' Error in reading triangle coords for record ', NUMBER_CONVERTED,
     &'   from line segment: ''', LINE(C_NO_START:C_NO_FINISH), '''',
     &' zero length vector',
     &' Aborting - (please report this bug with the input file)'
                    LERR = .TRUE.
                    GOTO 55555
                  ENDIF
                  LEN_NORM = DRAW_NORMAL_LENGTH/LEN_NORM ! the needed multiplier
                  XYZ_NORM(1,TCOUNT) =  LEN_NORM*XYZ_NORM(1,TCOUNT)
                  XYZ_NORM(2,TCOUNT) =  LEN_NORM*XYZ_NORM(2,TCOUNT)
                  XYZ_NORM(3,TCOUNT) =  LEN_NORM*XYZ_NORM(3,TCOUNT)
                  ! now norm vector is along as it should be
                  ! draw it
                  IF  (TYPEOUT.EQ.0) THEN ! vmd or qpt? ans: VMD
                     WRITE( SOUTPUT, '(A,3F9.3,A,3F9.3,A,I5)') ! 1->2
     &'draw line  {', 
     &     (XYZ_TRIANGLE(XCOUNT,TCOUNT), XCOUNT= 1,3),
     &           '} {', 
     &     ((XYZ_TRIANGLE(XCOUNT,TCOUNT)+XYZ_NORM(XCOUNT,TCOUNT)), 
     &                                                 XCOUNT=1,3),
     &             '} width ', WIDTH
                  ELSEIF (TYPEOUT.EQ.1) THEN ! vmd or qpt? ans: QPT
                    WRITE(SOUTPUT)
     &  2.0, (REAL(XYZ_TRIANGLE(XCOUNT,TCOUNT)),XCOUNT=1,3)  ! move to point
                    WRITE(SOUTPUT) ! draw to 2
     &  3.0, (REAL(XYZ_TRIANGLE(XCOUNT,TCOUNT)+
     &             XYZ_NORM(XCOUNT,TCOUNT)      ),XCOUNT=1,3) ! draw alongvector
                  ENDIF ! vmd or qpt? (end)            
                ENDDO ! go thru triplet

              ENDIF ! do we have a trinorm record?
            ENDIF ! doing normals?

          ELSE  ! not a triangle
            IF ( (TYPEOUT.EQ.0)       .AND.        ! not doing normals
     &           (DRAW_NORMAL_LENGTH.LT.0.) ) THEN ! vmd or qpt? ans: VMD
              ! doing vmd type output - simply output the record
              CALL CHREND( LINE, LINE_END) ! find last character in the line
              WRITE( SOUTPUT, '(A)') LINE(1:LINE_END)

           ELSEIF ( (TYPEOUT.EQ.1)       .AND.    ! doing normals??
     &               (DRAW_NORMAL_LENGTH.LT.0.) ) THEN ! vmd or qpt? ans: QPT
              ! try to pickup color records and process these
	      !                 1234567890
              ! hole colours 
              IF (LINE(1:10).EQ.'draw color') THEN
                IF (INDEX(LINE,'yellow').NE.0) THEN
                  WRITE(SOUTPUT) 1., 4., -55., 15.
                ELSEIF (INDEX(LINE,'green').NE.0) THEN
                  WRITE(SOUTPUT) 1., 7., -55., 17. 
                ELSEIF (INDEX(LINE,'red').NE.0) THEN
                  WRITE(SOUTPUT) 1., 3., -55., 16.
                ELSEIF (INDEX(LINE,'blue').NE.0) THEN
                  WRITE(SOUTPUT) 1., 2., -55., 18.
                ELSE ! everyting to purple
                  WRITE(SOUTPUT) 1., 8., 0., 0.
                ENDIF

              ENDIF
            ENDIF ! qpt or vmd (for color)
          ENDIF ! do we have a triangle?

       ENDIF ! {ioread.eq.-1 statement}

      ENDDO ! end of do while ioread = 0

      WRITE( NOUT, '(A,I5,A)')
     &' Have converted ', NUMBER_CONVERTED, ' triangle records.' 
      ! have to have converted at least one triangle 
      ! to be counted successful
      IF (NUMBER_CONVERTED.EQ.0) LERR = .TRUE.

55555 CONTINUE
      IF (LERR) THEN
        CALL EXIT(0) ! indicate routine failure (cannot open files, no args,
                     !                           nothing converted)
      ELSE
        CALL EXIT(1) ! success
      ENDIF
      
      END
         
