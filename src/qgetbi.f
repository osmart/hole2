      SUBROUTINE QGETBI( NIN, NOUT, MAT3, LABORT )
      IMPLICIT NONE
C ********************************************************************
C *                                                                  *
C * This software is an unpublished work containing confidential and *
C * proprietary information of Birkbeck College. Use, disclosure,    *
C * reproduction and transfer of this work without the express       *
C * written consent of Birkbeck College are prohibited. This notice  *
C * must be attached to all copies or extracts of the software.      *
C *                                                                  *
C * (c) 1993, 1995 Oliver Smart & Birkbeck College                   *
C *  All rights reserved                                             *
C * (c) 1996 Oliver Smart & Birkbeck College, All rights reserved    *
C *                                                                  *
C ********************************************************************
C
C Modification history:
C
C Date	Author		Modification
C 12/93	O.S. Smart	Original public release in HOLE suite beta1.0
C 11/95	O.S. Smart   Changed to all allow user to choose predefined
C 					rotations xZ etc.
C
C this s/r gets a 3*3 rotation matrix to be applied to 
C move/draw & text records already input.
C The user is asked to input the name of a hydra view file -
C with the default of an identity transformation.
C Once the view file is opened then the user is asked for a view -
C if found the rotation matrix is used - if not the available views in
C file are listed. The final matrix is tested to make sure determinant
C is unity.


C passed variables

C input/output stream numbers
      INTEGER                   NIN, NOUT

C the 3*3 matrix
      REAL                      MAT3(3,3)

C an abort indicator
C if we find a problem in this s/r return true
      LOGICAL			LABORT

C variables internal to this routine 


C VNAME takes the view name from the file
C VINO takes the view name from the keyboard
      CHARACTER*10              VNAME, VINO

C translation and centering vectors
      REAL                      TRANS(3),CENTR(3)

C scale, slab
      REAL                      SCALE,SLAB

C loop variables 
      INTEGER                   JC,IC


C array of reals to store numbers found in 
C quanta view file
      REAL                      QUAREC(14)
C input file stream
      INTEGER                   SIN

C dummy filename
      CHARACTER*200             FDUM

C one character command / tag for recognizing the view file type
C two character command code
      CHARACTER*1		COM1, TAG
      CHARACTER*2       COM2 

C determinant of matrix
      REAL			DET

C **************** end of declarations ***********

C if the user does not wish to apply a rotation
C return the identity matrix
      DO 901 IC = 1, 3
        DO 902 JC = 1, 3
          MAT3(IC,JC) = 0.0
902     CONTINUE
901   CONTINUE
      MAT3(1,1) = 1.0
      MAT3(2,2) = 1.0
      MAT3(3,3) = 1.0

C asks the user whether he wants a rotation 
      CALL PROMPT( NOUT,
     & 'Do you wish to change the view with a rotation? (y/n) <no>:')
      READ( NIN, '(A1)') COM1
      CALL VTCLEAR( NOUT)
      IF ((COM1.NE.'Y') .AND. (COM1.NE.'y')) GOTO 55555

C Ask for view filename.
C Use s/r interf this has arguments 
C input stream  (usually 5) returned unchanged %
C output stream (usually 6) returned unchanged % 
C a logical variable if true file is old (existing) returned unchanged %
C The stream number the file is opened to - choosen by interf %
C The file type which is a short description of file - 
C   if this includes the string 'binary' the file will be opened 
C   as such - returned unchanged %
C The filename - this is supplied with default and returned
C   with the name which is opened %
C An abort indicator if this is supplied .true. then the routine
C   will allow the user to abort - if .false. no abort is allowed.
C   An abort is indicated by labort being returned .true. %
C choice of view files to be read
C choice - HYDRA or QUANTA file or predefined view
      WRITE( NOUT, '(A)')
     &' Do you want to use: ',
     &'    a view from a Quanta view file (*.vew) - reply q or Q',
     &'    a view from a Hydra view file - reply h or H',
     &' or a predefined view: ',
     &'    This is specified by a two letter code, the first letter',
     &'    stating which molecular axis you wich to have placed',
     &'    horizontally on the page i.e. in this -> direction.',
     &'    The second letter which axis should run up the page.',
     &'                             ^',
     &'    i.e., in this direction  |',
     &'                             |',
     &'    If letter is in UPPER CASE then this refers to the negative',
     &'    axis.',
     &'        i.e., ',
     &'        ''xy'' is the default view x along (->), y up page'//
     &                                    ' (so z out of page)',
     &'        ''xz'' has x along (->) and z up the page'//
     &                                    ' (so y into page)',
     &'        ''xZ'' has x along (->) and z DOWN page'//  
     &                                    ' (so y out of page)',
     &'        ''Yz'' has y going <- and z up page'//
     &                                    ' (so x out of page)',
     &'        For obvious reasons do not try to answer ''xx'' or'//
     &                                    ' similar!'

      CALL PROMPT(NOUT, 
     & 'Enter view code (Q,H or 2letter)? <identity>:')
      READ( NIN, '(A2)', END=55555, ERR=55555) COM2
      CALL VTCLEAR( NOUT)
C what was the reply?
      IF ( (COM2(1:1).EQ.'H') .OR. (COM2(1:1).EQ.'h') .OR.
     &     (COM2(1:1).EQ.'Q') .OR. (COM2(1:1).EQ.'q')     )  THEN
C allow abort
        LABORT = .TRUE.
C find last filename with the qualifier .vew   
        CALL LASTF( FDUM, '.vew')
        IF (FDUM(1:4).EQ.'none') FDUM = 'input'

C read from file - hydra or quanta
        IF ((TAG.EQ.'H') .OR. (TAG.EQ.'h')) THEN
          TAG = 'H'
          CALL INTERF( NIN, NOUT, .TRUE., SIN,
     &  'input binary hydra view file name', FDUM, LABORT, '.view')
C if LABORT is true, exit the function
          IF (LABORT) GOTO 55555
        ELSE
          TAG = 'Q'
          CALL INTERF( NIN, NOUT, .TRUE., SIN,
     &  'input ascii quanta view file name', FDUM, LABORT, '.vew')
C if LABORT is true, exit the function
          IF (LABORT) GOTO 55555
        
        ENDIF

704     CONTINUE
        CALL PROMPT(NOUT,
     & 'Please state the view that you want <list views in file>:')
C read reply into line - to allow default
        READ( NIN, '(A)', ERR= 55555, END= 55555) VINO
        CALL VTCLEAR( NOUT)
C has the user specified a view?
        IF (VINO(1:6).NE.'     ') THEN
C the match for viewname should be case insensitive
          CALL UCASE(VINO)
C yes start the read **************
          REWIND(SIN)
C read new view
20        CONTINUE
C reads the characters of view-name
          IF(TAG.EQ.'H') THEN
            READ( SIN, END=70) VNAME
          ELSE
            READ(SIN, '(A10)', END = 70)VNAME
          ENDIF
C reads the view parameters - mat3 is the one of interest
          IF(TAG.EQ.'H')  THEN
            READ( SIN) CENTR, MAT3, TRANS, SCALE, SLAB
          ELSE
C reads the entire row if quanta view
            READ( SIN, *) QUAREC
          ENDIF
C is this the required view? if so return after closing file
          CALL UCASE( VNAME)
          IF ( VINO.EQ.VNAME) GOTO 55
C not the required view read the rest
          GOTO 20
C end of file reached - view not found
70        CONTINUE
          WRITE(NOUT,*) 'View not found.'//CHAR(7)
        ENDIF

C user has hit return or specified a view not in file
        WRITE(NOUT,*) 'Available views in file:'

C reset the file pointer to the beginning of the file
        REWIND(SIN)
C read view name and write out
C at the end of file goto 704 and ask the user to specify
C a view again 
71      CONTINUE
        IF(TAG.EQ.'H') THEN
           READ( SIN, END=704) VNAME
        ELSE
           READ(SIN, '(A10)', END = 704)VNAME
        ENDIF
C different readings for different view files
        IF(TAG.EQ.'H') THEN
          READ( SIN) CENTR, MAT3, TRANS, SCALE, SLAB
        ELSE  
          READ (SIN, *) QUAREC
        ENDIF

C writes view names, one after another
        WRITE( NOUT,'(1X,A10)') VNAME
        GOTO 71

C a view has been specified - 
C   so check that mat3 is o.k.
C  close view file
C   and then return
55      CONTINUE
C choosing the right QUAREC elements for the matrix MAT3
        IF (TAG.EQ.'Q') THEN
          MAT3(1,1) = QUAREC(4)
          MAT3(2,1) = QUAREC(5)
          MAT3(3,1) = QUAREC(6)
          MAT3(1,2) = QUAREC(8)
          MAT3(2,2) = QUAREC(9)
          MAT3(3,2) = QUAREC(10)
          MAT3(1,3) = QUAREC(12)
          MAT3(2,3) = QUAREC(13)
          MAT3(3,3) = QUAREC(14)
        ENDIF 
        CLOSE( SIN)
C end of input of MAT3 from a files

      ELSEIF ( (COM2(1:1).EQ.'X') .OR. (COM2(1:1).EQ.'x') .OR.
     &         (COM2(1:1).EQ.'Y') .OR. (COM2(1:1).EQ.'y') .OR.
     &         (COM2(1:1).EQ.'Z') .OR. (COM2(1:1).EQ.'z')     )  THEN
C **got a two letter code**
C zero matrix
        MAT3(1,1) = 0.
        MAT3(2,1) = 0.
        MAT3(3,1) = 0.
        MAT3(1,2) = 0.
        MAT3(2,2) = 0.
        MAT3(3,2) = 0.
        MAT3(1,3) = 0.
        MAT3(2,3) = 0.
        MAT3(3,3) = 0.

c N.B. first number of mat3 is the row number - column is second
C first letter is where the x axis should end up.
        IF     (COM2(1:1).EQ.'x') THEN
          MAT3(1,1) = 1.
        ELSEIF (COM2(1:1).EQ.'X') THEN
          MAT3(1,1) = -1.
        ELSEIF (COM2(1:1).EQ.'y') THEN
          MAT3(1,2) = 1.
        ELSEIF (COM2(1:1).EQ.'Y') THEN
          MAT3(1,2) = -1.
        ELSEIF (COM2(1:1).EQ.'z') THEN
          MAT3(1,3) = 1.
        ELSEIF (COM2(1:1).EQ.'Z') THEN
          MAT3(1,3) = -1.
        ENDIF
C second letter is where the y axis goes
        IF     (COM2(2:2).EQ.'x') THEN
          MAT3(2,1) = 1.
        ELSEIF (COM2(2:2).EQ.'X') THEN
          MAT3(2,1) = -1.
        ELSEIF (COM2(2:2).EQ.'y') THEN
          MAT3(2,2) = 1.
        ELSEIF (COM2(2:2).EQ.'Y') THEN
          MAT3(2,2) = -1.
        ELSEIF (COM2(2:2).EQ.'z') THEN
          MAT3(2,3) = 1.
        ELSEIF (COM2(2:2).EQ.'Z') THEN
          MAT3(2,3) = -1.
        ELSE
          WRITE( NOUT, '(A)')
     &' ERROR '//CHAR(7),
     &' Second letter of two letter code unrecognized='''//
     &                                      COM2(2:2)//'''',
     &' Aborting************'
          LABORT = .TRUE.
          GOTO 55555
        ENDIF
C where z goes depends on x and y
C work out cross product of vector {mat(1,1), mat(1,2), mat(1,3)}
C with vector {mat(2,1), mat(2,2), mat(2,3)}
C ensures we should not get nasty volume reversal etc.
        MAT3(3,1) = MAT3(1,2)*MAT3(2,3) - MAT3(1,3)*MAT3(2,2)
        MAT3(3,2) = MAT3(1,3)*MAT3(2,1) - MAT3(1,1)*MAT3(2,3)
        MAT3(3,3) = MAT3(1,1)*MAT3(2,2) - MAT3(1,2)*MAT3(2,1)
      ENDIF

C a minimal but sufficent requirement for the matrix is
C that its determinant should be 1 so that it should
C not produce volume transformations 
C calculates the elements of the determinant
      CALL DET3( MAT3, DET)
C if the determinant is not 1 then abort with  error message
      IF (ABS(DET-1.0).GT.1E-03) THEN
        WRITE(NOUT, '( A/ A/ A,F12.4,A,E12.1/ A/ 3(3F12.3/) )')
     &' ERROR '//CHAR(7),
     &' The determinant of the rotation matrix for that view',
     &' is ', DET,'   1 - det= ', (1.0-DET),
     &' Rotation matrix: ', MAT3
        LABORT = .TRUE.
      ENDIF

55555 RETURN
      END
