      SUBROUTINE HOMULF( NIN, NOUT, LERR, FCOORD, 
     &			 MULMAX, MULNUM, MULNAM)
      IMPLICIT NONE
C ********************************************************************
C *                                                                  *
C * This software is an unpublished work containing confidential and *
C * proprietary information of Birkbeck College. Use, disclosure,    *
C * reproduction and transfer of this work without the express       *
C * written consent of Birkbeck College are prohibited. This notice  *
C * must be attached to all copies or extracts of the software.      *
C *                                                                  *
C * (c) 1995 Oliver Smart & Birkbeck College, 	                     *
C * All rights reserved                                              *
C * (c) 1996 Oliver Smart & Birkbeck College, All rights reserved    *
C *                                                                  *
C ********************************************************************
C
C Modification history:
C
C Date	Author		Modification
C 11/95	O.S. Smart	Original version
C 
C passed variables *************

C input, output stream, error indicator
      INTEGER			NIN, NOUT
      LOGICAL			LERR

C supplied filename - will contain wild card.
      CHARACTER*200		FCOORD

C max number of files (a parameter so dont change!), number of files, filenames
      INTEGER			MULMAX, MULNUM
      CHARACTER*200		MULNAM(MULMAX)

C local vbles ******************

C dummy string
      CHARACTER*250		DUMSTR

C input streams
      INTEGER			SIN, SIN2

C logical fn to open files readonly
      LOGICAL			OPENRO

C end of decs ******************

      WRITE( NOUT, '(A)') 
     &' ',
     &' COORD has been specified with a wildcard '''
     &       //FCOORD(1:INDEX(FCOORD,'   ')-1)//'''',
     &' So will perform a multiple HOLE run on files:'

C make sure that old temporary files do not exist
C try to open it - if you can remove it
      CALL STREAM( SIN)
      OPEN( SIN, FILE= 'hole_homulf_temp_file', 
     &      STATUS= 'OLD', ERR= 10)
      CLOSE( SIN, STATUS= 'DELETE')

10    CONTINUE
C use unix system to list files agreeing with fname
      DUMSTR = 'ls -1 '//FCOORD(1:INDEX(FCOORD,'      ')-1)//
     &                  ' > hole_homulf_temp_file'

      CALL SYSTEM( DUMSTR)

C open the file 
      CALL STREAM( SIN)
      OPEN( SIN, FILE= 'hole_homulf_temp_file', 
     &      STATUS= 'OLD', ERR= 900)
C number of files
      MULNUM = 0

C read until eof
20    CONTINUE
C check to see whether the number of files execeeded
	IF (MULNUM+1.GE.MULMAX) THEN
	  WRITE( NOUT, '(A/A,I5/A)')
     &' Problem in s/r HOMULF',
     &' Number of files specified exceeds limit MULMAX= ', MULMAX,
     &' Will continue ignoring later files'
	  GOTO 30
	ENDIF
        READ( SIN, '(A)', END= 30) MULNAM(MULNUM+1)
        MULNUM = MULNUM + 1
C check that we can open file mulnam(mulnum)
        IF (.NOT.OPENRO( SIN2, MULNAM(MULNUM), NOUT)) THEN
          WRITE( NOUT, '(A)')
     &' ERROR cannot open file: '//
     & MULNAM(MULNUM)(1:INDEX(MULNAM(MULNUM),'      ')-1),
     &' Aborting *******'
          LERR = .TRUE.
C do not forget to delete tempfile
          CLOSE( SIN, STATUS= 'DELETE')
          GOTO 55555
        ENDIF
C Close the input file as soon as it is opened
C only want to check
        CLOSE(SIN2)
C list files to screen
        WRITE( NOUT, '(I6,3X,A)') MULNUM,  
     &    ''''//MULNAM(MULNUM)(1:INDEX(MULNAM(MULNUM),'      ')-1)//''''
C read next filenames 
      GOTO 20 

C finished read
30    CONTINUE

C must have at least one file
      IF (MULNUM.EQ.0) THEN
        WRITE(NOUT, '(A)')
     &'   ERROR cannot open any of the files specified by COORD card',
     &'   Check filname='''//FCOORD(1:INDEX(FCOORD,'      ')-1)//''''
        LERR = .TRUE.
      ENDIF

C close temp file - deleting it in the process
      CLOSE( SIN, STATUS= 'DELETE')

55555 RETURN

C error routines
900   CONTINUE
        WRITE( NOUT, '(A)') 
     &'   FATAL error in homulf ',
     &'     cannot open hole_homulf_temp_file',
     &'     report to O.S.S.'
        LERR = .TRUE.
      GOTO 55555
      END
