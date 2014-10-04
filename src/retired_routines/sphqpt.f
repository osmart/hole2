      PROGRAM SPHQPT
      IMPLICIT NONE
C ********************************************************************
C *                                                                  *
C * This software is an unpublished work containing confidential and *
C * proprietary information of Birkbeck College. Use, disclosure,    *
C * reproduction and transfer of this work without the express       *
C * written consent of Birkbeck College are prohibited. This notice  *
C * must be attached to all copies or extracts of the software.      *
C *                                                                  *
C * (c) 1993, 1996 Oliver Smart & Birkbeck College,                  *
C * (c) 1998 Oliver Smart & Birmingham University                    *
C *                                                                  *
C ********************************************************************
C
C Modification history:
C
C Date  Author          Modification
C 12/93 O.S. Smart        Original public release in HOLE suite beta1.0
C 06/94 OSS & J.Neduvelil Allow the colour coding of surface according 
C                         to radius.
C 07/94 OSS & J.Neduvelil Option for more uniform spaced dots by distance
C                         search.
C 10/94 OSS 		  Support for capsule output/modifications
C			  to support "LAST-REC-END" record in .sph file.
C 28/02/97 O.S.S.	Release HOLE2 beta001
C 11/97    O.S.S.       vt control codes
C 03/98    O.S.S.	special output file for Guy Coates "solid_surf"
C 05/98	   O.S.S.	input radius filter question added (RLOWER)
C
C Reads a pdb format file produced by hole
C option SPHPDB which contains sphere centre info,
C and produces a quanta plot file output with
C dot surface and centre line (see hole.doc)

C screen, keyboard
      INTEGER                   NIN
      PARAMETER(                NIN = 5)
      INTEGER                   NOUT
      PARAMETER(                NOUT= 6)
C input/output file stream
      INTEGER                   SIN, SOUT

C filename
      CHARACTER*200             FNAME

C string for text records in hydra file
      CHARACTER*80              STRING

C abort indicator
      LOGICAL                   LABORT

C draw centre lines?
      LOGICAL                   LCEN

C override radius?
      DOUBLE PRECISION		ROVER

C lower limit radius - any smaller than this and ignore on read
      DOUBLE PRECISION		RLOWER
C number of ignored records
      INTEGER			NIGN
      
C sphere centre storage arrays
C maximum number, actual number, co-ords and radius
C logical indicates whether the record is the last centre
      INTEGER                   SPMAX, SPNO
      PARAMETER(                SPMAX = 100000)
      DOUBLE PRECISION          SPXYZ( 3, SPMAX), SPRAD( SPMAX)
      LOGICAL                   SPLAST( SPMAX)
C capsule option needs to have the second centre
C n.b. SPRAD is the real capsule radius not the effective rad`
      DOUBLE PRECISION          SPSEC( 3, SPMAX)
C store whether point is product of connolly - pick up by -999 	residue number
      LOGICAL			SPCONN( SPMAX)

C loop counts x,y&z; sphere centre count, dots count, 
C 2nd sphere centre count, colour Pass count
      INTEGER                   XCOUNT, SCOUNT

C dot density to be used in output
      INTEGER                   DOTDEN

C program uses a sphere of isotropically distributed points
C on a sphere of unit radius. PTMAX is the array bound,
C PTNO is the number of dots (dependent on dotden)
      INTEGER                   PTMAX, PTNO
      PARAMETER(                PTMAX = 10000)
      DOUBLE PRECISION          PTXYZ( 3, PTMAX)

C error indicator
      LOGICAL                   LERR

C variable to define colours
      DOUBLE PRECISION          RCUT(0:3)
C if we are doing colour NPASS=3
C if we are not doing colour NPASS=1
      INTEGER                   NPASS

C if doing a grid surface then check each new potential vertex
C to see if it lies with distance 0.6*rad*dmult (where rad is
C the pore radius at the point) of any line already output 
C- stored in linxyz.  
C If the new point is outside this range then it is accepted
C it is joined to any vertex within the range rad*dmult the
C straight line is then corrected so that it curves with the local
C pore surface
C
C within a distance DOTD2 (the square of the minimum distance apart).
C VERMAX is the array bound, VERNO is the number of accepted dots. 
C fourth number is the radius of the sphere which produced the dot
      INTEGER                   VERMAX, VERNO               
      PARAMETER(                VERMAX = 10000) 
      DOUBLE PRECISION          VERXYZ( 4, VERMAX)

C also store points taken from output lines
      INTEGER                   LINMAX, LINNO
      PARAMETER(                LINMAX = 30*VERMAX)
      DOUBLE PRECISION          LINXYZ( 4,LINMAX)

C 21/07/94 program changed so that dot is output if it is
C at least dmult*sphere-radius from any other dot
      DOUBLE PRECISION          DMULT

C control flag - if true do a grid surface
      LOGICAL			LGRID

C logical vble capsules on
      LOGICAL			LCAPS

C string for output
      CHARACTER*90		OUTSTR

C doing sos output
      LOGICAL			SOSOUT

C end of decs ******************

C turn on VT codes  with bold after prompt- 
      CALL VTCON( .TRUE.)

C initialise the no.of accepted dots to 0
      VERNO = 0

C capsule option defaults to off
      LCAPS = .FALSE.
C initialize 
      LERR = .FALSE.

C write greeting
      WRITE( NOUT, '(A)') 
     &' *** Program sphqpt ***',
     &' Reads a pdb format file produced by hole',
     &' option SPHPDB which contains sphere centre info,',
     &' and outputs a quanta plot file with the hole surface',
     &' either as a dot or grid representation',
     &' Copyright 1996,1997 by Oliver Smart and Birkbeck College',
     &' Copyright 1998 by Oliver Smart and Birmingham University',
     &' Program modification number 0003 26-03-98'

C write link time of program to screen
      CALL VERTIM( NOUT)
      WRITE( NOUT, *)

C use s/r lastf to find latest file of
C type in the directory.  N.b. only works on unix machines
      CALL LASTF( FNAME, '.sph')
      IF (FNAME(1:4).EQ.'none') FNAME = 'input'

C get input filename
      LABORT = .TRUE.
C (input stream, output, oldfile?, file_stream, file_type, name,
C  allow abort?, default extension)
C N.B. as file_type includes 'BINARY' then will open as binary
      CALL INTERF( NIN, NOUT, .TRUE., SIN, 
     &  'input pdb format', FNAME, LABORT, '.sph')
      IF (LABORT) GOTO 55555

C new option to write ascii output for input to guy coates' solid_surf
      OUTSTR = 
     &'What format for output file Qpt (quanta)'//
     &                 ' or Sos (solid_surf)? <Q>:'
      CALL PROMPT( NOUT, OUTSTR)
      READ( NIN, '(A) ', ERR=55555, END=55555) STRING
      CALL VTCLEAR( NOUT)
      
C if the string is not blank try to read value
      IF ((STRING(1:1).EQ.'S').OR.(STRING(1:1).EQ.'s')) THEN
C sos output
        SOSOUT = .TRUE.
C get output filename
        LABORT = .TRUE.
        CALL INTERF( NIN, NOUT, .FALSE., SOUT, 
     &  'output sos', FNAME, LABORT, '.sos')
        IF (LABORT) GOTO 55555	
	
      ELSE
C quanta output
        SOSOUT = .FALSE.
C get output filename
        LABORT = .TRUE.
        CALL INTERF( NIN, NOUT, .FALSE., SOUT, 
     &  'output binary hydra/quanta plot', FNAME, LABORT, '.qpt')
        IF (LABORT) GOTO 55555	
      ENDIF
      
C next question only for qpt output
      IF (SOSOUT) THEN
	LCEN = .FALSE.
      ELSE
C does the user want the centre lines drawn?
        CALL PROMPT( NOUT,
     & 'Do you want the centre lines drawn in output file? (y/n) <y>:')
        READ( NIN, '(A1)', ERR= 55555, END= 55555) STRING(1:1)
        IF (STRING(1:1).EQ.'N'.OR.STRING(1:1).EQ.'n') THEN
	  LCEN = .FALSE.
        ELSE
  	  LCEN = .TRUE.
        ENDIF
      ENDIF
      
C overide radii?
      CALL VTCLEAR( NOUT)
      WRITE( NOUT, '(A)')
     &' It is possible to enter a number to which all the sphere',
     &'   centre radii will be set regardless of value in file.'
      CALL PROMPT( NOUT,
     & 'Enter value <leave radii alone>:')
      READ( NIN, '(A)', ERR= 55555, END= 55555) STRING
C if the string is not blank try to read value
      IF (STRING(1:5).NE.'     ') THEN
	READ( STRING, '(BN,F10.0)') ROVER 
      ELSE
	ROVER = 0.
      ENDIF

C Ignore small radii or Big?
      CALL VTCLEAR( NOUT)
      WRITE( NOUT, '(A)')
     &' Do you want to ignore small radius records on read?'
      CALL PROMPT( NOUT,
     & 'Enter lower limit to be used <use all records>:')
      READ( NIN, '(A)', ERR= 55555, END= 55555) STRING
C if the string is not blank try to read value
      IF (STRING(1:5).NE.'     ') THEN
	READ( STRING, '(BN,F10.0)') RLOWER 
      ELSE
	RLOWER = -1D06
      ENDIF

C next question only for qpt output
      IF (SOSOUT) THEN
	LGRID = .FALSE.
      ELSE
      
C dot or grid surface?
        CALL VTCLEAR( NOUT)
        WRITE( NOUT, '(A)') 
     &' This program can either do a dot surface (which is fast) or',
     &'    a curving grid surface (which is must slower).'
        CALL PROMPT( NOUT,  
     & '   Do you want a dot (D) or grid (G) surface? <dot>:')
        READ( NIN, '(A) ', ERR=55555, END=55555) STRING
        CALL VTCLEAR( NOUT)
      
C if the string is not blank try to read value
        IF ((STRING(1:1).EQ.'G').OR.(STRING(1:1).EQ.'g')) THEN

C July '94 - allow more uniform dot surface by
C value for dotd2
          WRITE( NOUT, '(A)')
     &' The density of the grid surface is controlled by DMULT',
     &' A value of 2.0 will produce a rather rough surface, 1.3 a',
     &' rather dense surface - 1.8 is a reasonable compromise.',
     &' N.b. procedure takes much longer than dot surface.'
          CALL PROMPT( NOUT,
     & 'What value for DMULT? <1.8>:')
          READ( NIN, '(A) ', ERR=55555, END=55555) STRING
C if the string is not blank try to read value
          IF (STRING(1:5).NE.'     ') THEN
            READ( STRING, '(BN,F10.0)' ) DMULT
          ELSE 
            DMULT = 1.8
          ENDIF
C doing distance search - use high value for dot density
C i042 shows that 25 seems to give reasonable results
	  DOTDEN = 25
          LGRID = .TRUE.
        ELSE 

C normal quick dot surface
          LGRID = .FALSE.
        ENDIF
C end of if for qpt only
      ENDIF
	
C Doing dot surface?
      IF (.NOT.LGRID) THEN
C prompt value for dotden
        CALL VTCLEAR( NOUT)
        WRITE( NOUT, '(A)')
     &' The number of dots on the surface is controlled by DOTDEN',
     &'   The higher DOTDEN the more dots: 5 produces few dots, 20 alot'
        CALL PROMPT( NOUT,
     & 'What value for dotden? <10>:')
        READ( NIN, '(A)', ERR= 55555, END= 55555) STRING
C if the string is not blank try to read value
        IF (STRING(1:5).NE.'     ') THEN
	  READ( STRING, '(BN,I10)') DOTDEN
        ELSE
	  DOTDEN = 10
        ENDIF
      ENDIF
      
C does the user want to implement colour to illustrate the radius size?
      CALL PROMPT( NOUT, 
     &'Do you want to colour surface according to pore radius? <n>:')
      READ( NIN, '(A1)', ERR= 55555, END= 55555) STRING(1:1)
      IF ((STRING(1:1) .EQ.'Y').OR.(STRING(1:1) .EQ.'y')) THEN
	NPASS=3
	CALL PROMPT( NOUT,
     &  'What upper radius for low radius colour (red) <1.15>:')
	READ( NIN, '(A)', ERR= 55555, END= 55555) STRING   
C if the string is not blank try to read value
	IF (STRING(1:5).NE.'     ')THEN
	  READ( STRING, ' (BN,F10.0)') RCUT(1)
	ELSE
	  RCUT(1) = 1.15
	ENDIF
	CALL PROMPT( NOUT,
     &  'What upper radius for mid radius colour (green) <2.30>:')
	READ( NIN, '(A)', ERR= 55555, END= 55555) STRING   
C if the string is not blank try to read value
	IF (STRING(1:5).NE.'     ')THEN
	  READ( STRING, ' (BN,F10.0)') RCUT(2)
	ELSE
	  RCUT(2) = 2.3 
	ENDIF
	CALL PROMPT( NOUT,
     &  'What upper radius for high radius colour (blue) <999>:')
	READ( NIN, '(A)', ERR= 55555, END= 55555) STRING   
C if the string is not blank try to read value
	IF (STRING(1:5).NE.'     ')THEN
	  READ( STRING, ' (BN,F10.0)') RCUT(3)
	ELSE
	  RCUT(3) = 999.0
	ENDIF
      ELSE 
C do not colour according to radius - make only one pass
C outputing all records
	RCUT(1) = 999.0
	NPASS=1 
      ENDIF
C rcut zero must be negative 
      RCUT(0) =-1.0

C have opened input and output files
C and got initial conditions start work

C no sphere centres to start with
      SPNO = 0
      NIGN = 0

      CALL VTCLEAR( NOUT)

C read in input file - do until EOF reached
10    CONTINUE
	READ( SIN, '(A)', END= 20) STRING
C do we have a LAST-REC-END record?
        IF (STRING(1:12).EQ.'LAST-REC-END') 
     &      SPLAST(SPNO)   = .TRUE.

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

C draw centre line?
      IF (LCEN) THEN
C change to colour 7 (as in hole)
C change 18/10/95 centre line number 4 (yellow) alt 15
        WRITE( NOUT, '(A)') 
     &' Outputing centre line to quanta colour 4 (default yellow)'
        WRITE(SOUT) 1.0, 4.0, -55.0, 15.0
C moveto first point
C 10/7/96 change so that do capsule midpoints - regardless of
C whether we have capsule or not (does not matter for spheres 
C as second centre is the same as the first).
	WRITE(SOUT) 2.0, 
     &   (REAL(0.5*(SPXYZ(XCOUNT,1)+SPSEC(XCOUNT,1))), XCOUNT= 1, 3)
C go thru rest of points
	DO 30 SCOUNT = 2, SPNO
Cignore all connolly type records
          IF (SPCONN(SCOUNT)) GOTO 30
C if the previous point is an end move
	  IF (SPLAST(SCOUNT-1)) THEN
            WRITE(SOUT) 2.0, (REAL(0.5*(SPXYZ(XCOUNT,SCOUNT) +
     &                                  SPSEC(XCOUNT,SCOUNT))), 
     &                                  XCOUNT= 1, 3)           
	  ELSE
C previous not an end - draw
            WRITE(SOUT) 3.0, (REAL(0.5*(SPXYZ(XCOUNT,SCOUNT) +
     &                                  SPSEC(XCOUNT,SCOUNT))), 
     &                                  XCOUNT= 1, 3) 
	  ENDIF
30      CONTINUE
      ENDIF
C end of drawing centre lines

C 10 July 1996 split of drawing part into seperate s/r's for normal
C (uniform and capsule options) except for centre line which has just been
C done.
      IF (LCAPS) THEN
        CALL SPHQPC( NIN, NOUT, SOUT, LERR,
     &               DOTDEN, PTMAX, PTNO, PTXYZ,
     &               SPMAX, SPNO, SPXYZ, SPRAD, SPLAST, SPSEC,
     &               LGRID, NPASS, RCUT, DMULT, 
     &               VERMAX, VERNO, VERXYZ, LINMAX, LINNO, LINXYZ,
     &               SOSOUT)
      ELSE
        CALL SPHQPU( NIN, NOUT, SOUT, LERR,
     &               DOTDEN, PTMAX, PTNO, PTXYZ,
     &               SPMAX, SPNO, SPXYZ, SPRAD, SPLAST,
     &               LGRID, NPASS, RCUT, DMULT, 
     &               VERMAX, VERNO, VERXYZ, LINMAX, LINNO, LINXYZ,
     &               SOSOUT)
      ENDIF
      CLOSE( SOUT)

55555 CONTINUE
      STOP 'FORTRAN STOP - sphqpt normal completion.'
      END
