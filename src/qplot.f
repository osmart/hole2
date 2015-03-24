      PROGRAM QPLOT
C ********************************************************************
C *                                                                  *
C * This software is an unpublished work containing confidential and *
C * proprietary information of Birkbeck College. Use, disclosure,    *
C * reproduction and transfer of this work without the express       *
C * written consent of Birkbeck College are prohibited. This notice  *
C * must be attached to all copies or extracts of the software.      *
C *                                                                  *
C * (c) 1993, 1996 Oliver Smart & Birkbeck College,                  *
C * All rights reserved                                              *
C *                                                                  *
C ********************************************************************
C
C Modification history:
C
C Date	Author		Modification
C 12/93	O.S. Smart	Original public release in HOLE suite beta1.0
C 10/95 O.S. Smart	Overhaul making more user friendly - 
C			   How many input files? question.
C		           Support for extra HOLE colours 17 to 20 
C			   which are z of change colour record if y -55
C 28/02/97 O.S.S.	Release HOLE2 beta001
C 11/97    O.S.S.       vt control codes
C
C
C
C This program is designed to convert the binary plot file
C produced by Hydra or Quanta to a postscript file for printing.
C Planned features include being able to rotate image (using view
C from hydra format view file), plot stereo pictures, add text, scale 
C images and possible include a simple plot interface for the vax.
C The code is written in near-standard FORTRAN77 and has been
C developed under vax-vms, iris and ibm-rs6000/aix environments.
C
C Authors Oliver Smart & Valeriu Niculae. 
C
C This software is an unpublished work containing confidential and 
C proprietary information of Birkbeck College. Use, disclosure,
C reproduction and transfer of this work without the express
C written consent of Birkbeck College are prohibited.
C
C (c) 1992 Birkbeck College,
C     University of London; London, United Kingdom.;
C     All Rights Reserved.      
C
C a note as to the format of the hydra/quanta binary file to
C be added here *****

C implicit none is a non-standard fortran statement which
C forces all variables to be declared
      IMPLICIT NONE

C Input and output stream numbers.
C Here set to 5 & 6 indicating the keyboard, the screen.
      INTEGER                   NIN
      PARAMETER(                NIN = 5)
      INTEGER                   NOUT
      PARAMETER(                NOUT= 6)
C input/output file stream
      INTEGER                   SIN, SOUT

C filenames input, output, duplicate for input filename
C to work with when requested by the switch mode
      CHARACTER*200             FINPT, FOUTPT, FINPT2

C abort indicator to be used with s/r interf
      LOGICAL                   LABORT

C store for vectors read.
C The maximum number of colours is given by MAXCOL 
C There are 16 seperate stores one for each colour.
C The maximum number of records to be stored is 
C MAXST - this number must not be adjusted in run.
C n.b. rstore(0,*,*,*) a indicates 2: move to 3:draw to etc.
      INTEGER                   MAXST
      PARAMETER(                MAXST = 20000)
      INTEGER                   MAXCOL
      PARAMETER(                MAXCOL = 20) 
      INTEGER                   ISTORE(MAXCOL)
      REAL                      RSTORE( MAXST, 0:3, MAXCOL)
C store for text strings 
C txtno             the number of strings stored
C txtlen(*)         the length of string *
C txtpos(1 to 3, *) the position of its anchor point in angs space
C txtdir(*)         an integer to indicate where the string should be
C                   placed on page 1 indicates below to the left
C                                  2 ............... centred etc.
C                   key 789
C                       456 (like a numeric keypad)
C                       123
C txtstr(*)         the string to be written 
C the maximum number of strings
      INTEGER                   MAXTXT
      PARAMETER(                MAXTXT = 500)
C the maximum numbers of characters in each text string
      INTEGER                   MAXLEN
      PARAMETER(                MAXLEN = 80)
      INTEGER                   TXTNO
      INTEGER                   TXTLEN( MAXTXT)
      REAL                      TXTPOS( 3, MAXTXT)
      INTEGER                   TXTDIR( MAXTXT)
      CHARACTER*(MAXLEN)        TXTSTR( MAXTXT) 

C takes the matrix values in double array MAT3   
      REAL                      MAT3(3,3)
C Vble which passes on the information whether
C there are "dot at" records in the file:
C -1.        means dots are processed into 3d crosses in s/r qreadi
C 0.0        no dots in file
C +ve number the point size for circles to which crosses processed
      REAL                       DOTAT
C indicator for the level of control to be used
C in forming picture:
C 'E' expert full control
C 'N' normal level of control
      CHARACTER *1              SWITCH

C a loop count
      INTEGER			ICOUNT

C one character command
      CHARACTER*1		COM1


C end of declarations ********** (declarations above, exe's below)

C turn on VT codes - but not BOLD characters after prompt
      CALL VTCON( .FALSE.)

C greet user
      WRITE( NOUT, '(A)')
     &' This is program qplot which reads quanta plot files',
     &'   and produces postscript output',
     &' Copyright 1993,1997 by Oliver Smart',
     &' Copyright 2004 by Oliver Smart ',
     &' Copyright 2014-2015 SmartSci Limited, All rights reserved.'


C write link time of program to screen
      CALL VERTIM( NOUT)


C what degree of difficulty should be applied ?
      WRITE(NOUT, '(A)')
     &' What level of questions/options do you want to be used?'
      CALL PROMPT(NOUT,
     & '  Options:-  expert (E) or normal <normal>:')

C read answer
      READ( NIN,'(A1)', END= 55555, ERR= 55555) SWITCH
      CALL VTCLEAR( NOUT)
      CALL UCASE(SWITCH)
      IF (SWITCH.NE.'E')  SWITCH ='N'

C 2/6/95 sg has problems in correctly initializing vbles
C so assign all ISTORE (number of stored move/draws) to zero
      DO 10 ICOUNT = 1, MAXCOL
        ISTORE(ICOUNT) = 0
10    CONTINUE
      TXTNO = 0

C +--------------------------------------+
C ! Open input and output streams/files  !
C +--------------------------------------+

C Ask for input filename.
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
C  
C allow abort
      LABORT = .TRUE.
C March 1993 - new routine to find latest file of
C type in the directory.  N.b. only works on unix machines
      CALL LASTF( FINPT, '.qpt') 
      IF (FINPT(1:4).EQ.'none') FINPT = 'input'
C N.B. as file_type includes 'BINARY' then will open as binary
      CALL INTERF( NIN, NOUT, .TRUE., SIN, 
     &  'input binary hydra/quanta plot', FINPT, LABORT, '.qpt')
      IF (LABORT) GOTO 55555
      FINPT2 = FINPT

C jump here if a second or subsequent file is read
101   CONTINUE

C +-------------------------------------------+
C ! Have now opened input streams  !
C !        can proceed with read              !
C +-------------------------------------------+
C s/r qreadi reads the info in file into the store
C istore( 1 to maxcol) is returned with the number of move
C    draws read for each colour
C rstore is the store for moves/draws 
C if any problem is found in the file return abort as true
      CALL QREADI( NIN, NOUT, SIN, MAXST, MAXCOL, ISTORE, RSTORE, 
     &  MAXTXT, MAXLEN, TXTNO, TXTLEN, TXTPOS, TXTDIR, TXTSTR, LABORT,
     &  DOTAT, SWITCH)
C all records now read close input
      CLOSE( SIN)
      IF (LABORT) GOTO 55555

C tell user the total number of records read
      WRITE( NOUT, '(A,I5,A)')
     &' Have read a total of ', 
     &  ISTORE( 1) + ISTORE( 2) + ISTORE( 3) + ISTORE( 4) + ISTORE( 5) +
     &  ISTORE( 6) + ISTORE( 7) + ISTORE( 8) + ISTORE( 9) + ISTORE(10) +
     &  ISTORE(11) + ISTORE(12) + ISTORE(13) + ISTORE(14) + ISTORE(15) +
     &  ISTORE(16) + ISTORE(17) + ISTORE(18) + ISTORE(19) + ISTORE(20) +
     &  TXTNO, ' records so far'

C ask whether you want another file - rather than indicating abort
C as previously - do even for simple level.
      CALL PROMPT( NOUT,
     &  'Do you want to read another input file? (y/n) <n>:')
C read answer
      READ( NIN,'(A1)', END= 55555, ERR= 55555) COM1  
      CALL VTCLEAR( NOUT)
      CALL UCASE(COM1)
C yes no question
      IF (COM1.EQ.'Y') THEN
        LABORT = .TRUE.
        FINPT = 'input'
        CALL INTERF( NIN, NOUT, .TRUE., SIN, 
     &  'input next binary hydra/quanta plot', FINPT, LABORT, '.qpt')
        IF (.NOT.LABORT) THEN 
C make a duplicate of the input filename in case
C the user aborts the input
          FINPT2 = FINPT
C read input file and ask again
          GOTO 101
        ENDIF
C end of further file question
      ENDIF

C now ask for output file - allow abort
      LABORT = .TRUE.
C the default output file is the input name (its duplicate) 
C with .ps extension
      FOUTPT = FINPT2
C third vble now false - a new file to be opened
      WRITE(NOUT, *)
      CALL INTERF( NIN, NOUT, .FALSE., SOUT, 
     &  'output postscript file', FOUTPT, LABORT, '.ps')
      IF (LABORT) GOTO 55555
        
C Does the user want to choose a different view?
C get the rotation matrix MAT3 from a hydra
C view file. 
      CALL QGETBI( NIN, NOUT, MAT3, LABORT)
      IF (LABORT) GOTO 55555
C      write( nout, '(a/3(3f8.3/)a)')
C     &'  (debug) rotation matrix = ', MAT3, ' (debug)'

C rotate all the stored move/draw and text anchor point 
C records by the matrix MAT3 read in by QGETMX
      CALL QUSEMX( NIN, NOUT, MAXST, MAXCOL, MAT3,
     &    LABORT, RSTORE, ISTORE, MAXTXT, TXTNO, TXTPOS)
      IF (LABORT) GOTO 55555

C +--------------------------------------------+
C ! Have completed all input and done rotation ! 
C !  Write postscript output file              !
C +--------------------------------------------+
      CALL QPSWR( NIN, NOUT, SOUT, MAXST, MAXCOL, ISTORE, RSTORE,
     &  MAXTXT, MAXLEN, TXTNO, TXTLEN, TXTPOS, TXTDIR, TXTSTR, 
     &  LABORT, DOTAT, SWITCH)
      CLOSE( SOUT)
      IF (LABORT) GOTO 55555

C stop here
55555 WRITE( NOUT, *) 
      STOP 'FORTRAN STOP qplot normal successful completion.'
      END
