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
C * (c) 1998 Oliver Smart                    *
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
C 11/00    O.S.S.  	sphqpt (an interactive question asking routine)
C			made into a command line driven program
C
C Reads a pdb format file produced by hole
C option SPHPDB which contains sphere centre info,
C and produces a quanta plot file output with
C dot surface and centre line (see hole.doc)

C screen, keyboard
C sph_process now command line driven - comment out all
C interactive question lines with !(inter)
!(cinter)      INTEGER                   NIN
!(cinter)      PARAMETER(                NIN = 5)
!(cinter)C input/output file stream
!(cinter)      INTEGER                   SIN, SOUT
      INTEGER                   NOUT
      PARAMETER(                NOUT= 6)

      CHARACTER*200		FINPUT      ! inputfilename (penultimate argument)
      INTEGER			SIN         ! its stream number
      INTEGER			END_FINPUT  ! last character in filename
      CHARACTER*200		FOUTPUT     ! inputfilename (penultimate argument)
      INTEGER			SOUT        ! its stream number
      INTEGER			END_FOUTPUT ! last character in filename

!(cinter)C filename
!(cinter)      CHARACTER*200             FNAME

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
      PARAMETER(                SPMAX = 500000)
      DOUBLE PRECISION          SPXYZ( 3, SPMAX), SPRAD( SPMAX)
      LOGICAL                   SPLAST( SPMAX)
C capsule option needs to have the second centre
C n.b. SPRAD is the real capsule radius not the effective rad`
      DOUBLE PRECISION          SPSEC( 3, SPMAX)
C store whether point is product of connolly - pick up by -999 	residue number
      LOGICAL			SPCONN( SPMAX)
      ! need effective radius for colouring connolly
      DOUBLE PRECISION		SPEFFR( SPMAX)

! VBLES FOR s/r SPHPEG
! flag to indicate that the sphere is peg accessible
!  0: not peg accessible
!  1: accessible from +ve end
! -1: accessible from -ve end
!  2: in the shell (within distance shell_add of a peg sphere)
! -2: ditto
      INTEGER			SP_PEG_ACC( SPMAX) 

! now with  SHELL_ADD need to modify the radius of some spheres
! normally this would be equal to sprad but if sp_peg_acc = +/-2
! then this radius will be lower
      DOUBLE PRECISION		SP_PEG_RAD( SPMAX)	

! SCOTDO -> SP_TDO
! SCOENC -> SP_ENC
! SCOCIRCRAD -> SP_CIRCRAD
! n.b. vble may now be changed in calculation if sphere has no intersection
! with plane under consideration
      LOGICAL			SP_TDO( SPMAX)
C s/r coarea needs a store for the east/north coords of
C each point on grid - these are measured relative to the
C initial point
! SCOCIRCRAD: the radius of the point in the plane under consideration
! in the case of calls from concal this we equal scocrad
      DOUBLE PRECISION		SP_ENC( 2, SPMAX),
     &				SP_CIRCRAD( SPMAX)

! working flag for s/r coarea
      LOGICAL			SP_PEG_FLAG( SPMAX)
      

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

      ! vbles for get_rec_commands
      CHARACTER*3000 		REC_COMMANDS_HELP_MESSAGE
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

      LOGICAL			LBIN		! binary file indicator
      LOGICAL                   OPENRO	! open files readonly function


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

! PEG is on OR NOT
      LOGICAL 			LPEG

! vbles for working peg effect on conductance -
! first number the ratio of conductance with PEG about to water
! second number is the ratio for non-penetrating PEG
      DOUBLE PRECISION		PEGRAT(2)       

! probe radius - +ve direction and -ve
      DOUBLE PRECISION		PROBE_PLUS, PROBE_NEG

! (n037) additional shell around peg spheres where the 
! conductivity of the solvent is reduced.
      DOUBLE PRECISION		SHELL_ADD

! output of the triangle
      LOGICAL			TRIOUT


! end of decs ******************

!(cinter)C turn on VT codes  with bold after prompt- 
!(cinter)      CALL VTCON( .TRUE.)

C initialise the no.of accepted dots to 0
      VERNO = 0

C capsule option defaults to off
      LCAPS = .FALSE.
C initialize 
      LERR = .FALSE.

C write greeting
      WRITE( NOUT, '(A)') 
     &' *** Program sph_process (replacing sphqpt) ***',
     &' Reads a .sph file produced by hole',
     &' option SPHPDB which contains sphere centre info,',
     &' and processes it to a either dot surface or a number'//
     &                                     ' of other options',
     &' either as a dot or grid representation',
     &' Written by Oliver Smart at Birkbeck College from 1996.',
     &' Copyright 1996,1997 Oliver Smart ',
     &' Copyright 2004 Oliver Smart ',
     &' Copyright 2014-2015 SmartSci Limited, All rights reserved.'

C write link time of program to screen
      CALL VERTIM( NOUT)


      ! get the controlling command line arguments

      ! establish values for s/r get
      REC_COMMANDS_HELP_MESSAGE =
     &'                     (help message start)\n'//
     &' sph_process works on .sph file produced by PDBSPH '//
     &                                             'option in hole.\n'//
     &' This file is then converted either to:\n'//
     &' (a) a dot or spaghetti surface - qpt format \n'//
     &' (b) an .sos input file for the sos_triangle program to \n'//
     &'     finally make a solid surface \n'//
     &' (c) PEG option (to be written)\n'//
     &' \n'//
     &' Usage: '//
     &' sph_process (-qpt) (-nocen) (-sos) ... '//
     &                                    ' input_file output_file\n'//
     &' options: \n'//
     &'  -qpt:  produce a dot surface to qpt file (default)\n'//
     &'  -sos:  produce a .sos file for use by sos_triangle \n'//
     &'  -dotden D: use a dot density of D used by -qpt and'//
     &                                           '-sos options\n'//
     &'             D should be between 5 and 30 the higher the\n'//
     &'             value the more dots. Default is 10. \n'// 
     &'  -nocen: do not produce centre line (-qpt only) \n'//
     &'  -spaghetti (X): produce a spaghetti rather than a dot \n'//
     &'                  surface (for -qpt only \n'//
     &'                  controlled by X the grid density \n'//
     &'                  A X value of 2.0 will produce a rather\n '//
     &'                  rough surface, 1.3 a rather dense surface\n'//
     &'                  default is 1.8 a reasonable compromise.\n'//
     &'                  N.B. procedure takes much longer than '//
     &                                          ' dot surface.\n'//     
     &'  -rover Rover: use an override radius of Rover angs.\n'//
     &'              every sphere read will be assigned radius R\n'//
     &'              instead of individual radii\n'//
     &'  -ignore_small (Rignore): ignore any spheres with a radius\n'//
     &'                           less than Rignore. If argument\n'//
     &'                           found then a default 1.15angs\n'//
     &'                           used unless a value is specified\n'//
     &'  -colour (Rlow Rmid): produce a coloured surface\n'//
     &'                       surface will be \n'//
     &'                       red for pore radii < Rlow \n'//
     &'                       green for Rlow < pore radii < Rmid\n'//
     &'                       blue  for pore radii > Rmid\n'//
     &'                       default Rlow = 1.15 angs\n'//
     &'                       default Rmid = 2.30 angs\n'//
     &'                       (for connolly use effective radius\n'//
     &'                        of slab rather than pore radius\n'//
     &'  -color same as -colour for yanks\n'//
     &'  -peg Rplus Rminus ThetaPEG ThetaNON\n'//
     &'       Graphical output - specify with -sosout for .sph file\n'//
     &'                          with accessible spheres in it\n'//
     &'                          defaults to drawing sections to qpt'
      
     
     
     
     
!      call chrend( rec_commands_help_message, line_end)
!      write( nout, '(a)') rec_commands_help_message(1:line_end)
      REC_TOT = 11
      DO RCOUNT = 1,REC_TOT          ! initialize require store
        REC_REQUIRE_NUM(RCOUNT) = 0
	REC_REQUIRE_STRING(RCOUNT) = 0
      ENDDO 
      REC_COMMANDS(1) = 'qpt'     ! do not specify the -
      REC_COMMANDS(2) = 'sos'     ! 
      REC_COMMANDS(3) = 'dotden'  !
      REC_REQUIRE_NUM(3) = 1 ! require single value after dotden
      REC_COMMANDS(4) = 'nocen'
      REC_COMMANDS(5) = 'spaghetti'
      REC_COMMANDS(6) = 'rover'
      REC_REQUIRE_NUM(6) = 1 ! require a single value after rover
      REC_COMMANDS(7) = 'ignore_small'
      REC_COMMANDS(8) = 'colour'
      REC_COMMANDS(9) = 'color'
      REC_COMMANDS(10) = 'peg'
      REC_REQUIRE_STRING(10) = 4
      REC_COMMANDS(11) = 'tri'
      
      CALL GET_REC_COMMANDS( NOUT, LERR,
     &			     REC_COMMANDS_HELP_MESSAGE,
     &                       REC_TOT, REC_COMMANDS, 
     &                       REC_REQUIRE_NUM, REC_REQUIRE_STRING,
     &                       REC_FOUND, REC_NUM, REC_NUM_NUM,
     &			     REC_STRING, REC_STRING_NUM,
     &			     .TRUE., FINPUT, .TRUE., FOUTPUT)
      IF (LERR) GOTO 55555
      
            
      ! REC_COMMANDS(2) = 'sos'     ! 
      SOSOUT = .FALSE.	! default to qpt rather sos type out
      IF (REC_FOUND(2)) SOSOUT = .TRUE. ! -sos arg?
      
      ! REC_COMMANDS(3) = 'dotden'  !
      DOTDEN = 10	! dot density (dot type output)
      IF (REC_FOUND(3)) THEN ! override dotden?
        DOTDEN = REC_NUM(1,3)
	IF ((DOTDEN.LT.1).OR.(DOTDEN.GT.100)) THEN
          WRITE( NOUT, '(A,I5,A/ A)')
     &' Error dot density found after -dotden card =', DOTDEN,
     &    ' this is out of range.',
     &' -dotden value must be between 1 and 100'
        LERR = .TRUE. ! have failed
          GOTO 55555
	ENDIF
      ENDIF
      
      ! REC_COMMANDS(4) = 'nocen'
      LCEN = .TRUE. 	               ! default draw centre lines
      IF (REC_FOUND(4)) LCEN = .FALSE. ! if argument found override
      
      ! REC_COMMANDS(5) = 'spaghetti'
      LGRID = .FALSE.	               ! default do dot surface rather than sphagetti grid
      IF (REC_FOUND(5)) THEN
        LGRID = .TRUE.                ! if argument found override
	DMULT = 1.8 ! has the dmult value been specified?
        IF (REC_NUM(1,5).GT.-1E10) DMULT = REC_NUM(1,5)
      ENDIF
    
      ! REC_COMMANDS(6) = 'rover'
      ROVER  = 0.	! default no override radius
      IF (REC_FOUND(6)) ROVER = REC_NUM(1,6)
     
      
      ! REC_COMMANDS(7) = 'ignore_small'
      RLOWER = -1D06    ! default no ignore low radius records
      IF (REC_FOUND(7)) THEN
        RLOWER = 1.15
	IF (REC_NUM(1,7).GT.-1E10) RLOWER = REC_NUM(1,7)
      ENDIF 
      
      ! REC_COMMANDS(8) = 'colour'
      ! REC_COMMANDS(9) = 'color'
      ! if we are doing colour NPASS=3
      ! if we are not doing colour NPASS=1 accept any radii
      NPASS = 1 
      RCUT(0) = -1.0
      RCUT(1) = 9999.0
      IF (REC_FOUND(8).OR.REC_FOUND(9)) THEN
        NPASS = 3
        RCUT(1) = 1.15
	RCUT(2) = 2.30
	RCUT(3) = 9999.0 ! default values for colour
	! pick up the radii if specified
	IF (REC_NUM(1,8).GT.-1E10) RCUT(1) = REC_NUM(1,8)
	IF (REC_NUM(1,9).GT.-1E10) RCUT(1) = REC_NUM(1,9)
	IF (REC_NUM(2,8).GT.-1E10) RCUT(2) = REC_NUM(2,8)
	IF (REC_NUM(2,9).GT.-1E10) RCUT(2) = REC_NUM(2,9)
      ENDIF

      ! REC_COMMANDS(10) = 'peg'
      !  Rplus Rminus ThetaPEG ThetaNON
      LPEG = .FALSE.	               ! default do dot surface rather than sphagetti grid
      IF (REC_FOUND(10)) THEN
        LPEG = .TRUE.                ! if argument found override
        SAMPLE = -1E10   ! to detect lack of proper keys in .sph
	CVECT(1) = -1E10 ! all of these values should be read by
	CONNR(1) = -1E10 ! s/r sph_process_read
	PERPVE(1) = -1E10
	PERPVN(1) = -1E10
	ENDRAD = -1E10	
	PROBE_PLUS = REC_NUM(1,10)
	PROBE_NEG  = REC_NUM(2,10)
	PEGRAT(1)  = REC_NUM(3,10)
	PEGRAT(2)  = REC_NUM(4,10)
	! optional 5th arguement
	SHELL_ADD = 0.
        IF (REC_NUM(5,10).GT.-1E10) SHELL_ADD = REC_NUM(5,10)
      ENDIF

      IF (REC_FOUND(11)) THEN
        TRIOUT = .TRUE.
        SOSOUT = .TRUE.	
      ELSE
        TRIOUT = .FALSE.     
      ENDIF
      ! end of unpacking arguments
      


      ! find last character in each filename
      CALL CHREND( FOUTPUT, END_FOUTPUT)
      CALL CHREND( FINPUT, END_FINPUT)

! open the input file as readonly
      IF (.NOT.OPENRO( SIN, FINPUT, NOUT)) THEN
        WRITE( NOUT, '(A,A,A)')
     &' Error cannot open input file: ''', FINPUT(1:END_FINPUT), ''''
        LERR = .TRUE. ! have failed
        GOTO 55555
      ENDIF

! open the output file as new
! use s/r newop - this keeps old version removes tilda's spaces etc. etc.
! arguments output stream, error indicator,
! reduce_output_variable (0 for full), filename, filestream, binary indicator
      IF (FOUTPUT(1:4).NE.'none') THEN
        LBIN = .FALSE.
        IF (.NOT.SOSOUT) LBIN = .TRUE. ! qpt files are binary
        CALL NEWOP( NOUT, LERR, 0,
     &                  FOUTPUT, SOUT, LBIN)
        IF (LERR) THEN
          WRITE( NOUT, '(A,A,A)')
     &' Error cannot open output file: ''', FOUTPUT(1:END_FOUTPUT), ''''
          LERR = .TRUE. ! have failed
          GOTO 55555
        ENDIF
      ELSE
        ! no output file - only to be used for -peg option
        SOUT = -1
	WRITE( NOUT, '(A)')
     &'WARNING ''none'' specified as output file - so have not opened',
     &'        one.  This only makes sense with -peg option'
      ENDIF
      

      WRITE( NOUT, '(/ A,A,A/ A,A,A)') 
     &'Have opened ', FINPUT(1:END_FINPUT),   ' for input ',
     &'        and ', FOUTPUT(1:END_FOUTPUT), ' for output'
      IF (.NOT.SOSOUT) THEN ! qpt or sos? ans:qpt
	WRITE( NOUT, '(A)') 
     &'qpt output option used '
      ELSE 
        IF (LPEG) THEN
	  WRITE( NOUT, '(A)') 
     &'sph type output option used for PEG - next program to run ',
     &'            to process this is THIS PROGRAM AGAIN ',
     &'            with -sos option but no -peg. '
        ELSE
	WRITE( NOUT, '(A)') 
     &'sos type output option used - next program to run '//
     &                                'is ''sos_triangle'' '
	ENDIF
      ENDIF 


!(cinter)C use s/r lastf to find latest file of
!(cinter)C type in the directory.  N.b. only works on unix machines
!(cinter)      CALL LASTF( FNAME, '.sph')
!(cinter)!(cinter)      IF (FNAME(1:4).EQ.'none') FNAME = 'input'
!(cinter)
!(cinter)C get input filename
!(cinter)      LABORT = .TRUE.
!(cinter)C (input stream, output, oldfile?, file_stream, file_type, name,
!(cinter)C  allow abort?, default extension)
!(cinter)C N.B. as file_type includes 'BINARY' then will open as binary
!(cinter)      CALL INTERF( NIN, NOUT, .TRUE., SIN, 
!(cinter)     &  'input pdb format', FNAME, LABORT, '.sph')
!(cinter)      IF (LABORT) GOTO 55555
!(cinter)
!(cinter)C new option to write ascii output for input to guy coates' solid_surf
!(cinter)      OUTSTR = 
!(cinter)     &'What format for output file Qpt (quanta)'//
!(cinter)     &                 ' or Sos (solid_surf)? <Q>:'
!(cinter)      CALL PROMPT( NOUT, OUTSTR)
!(cinter)      READ( NIN, '(A) ', ERR=55555, END=55555) STRING
!(cinter)      CALL VTCLEAR( NOUT)
!(cinter)      
!(cinter)C if the string is not blank try to read value
!(cinter)      IF ((STRING(1:1).EQ.'S').OR.(STRING(1:1).EQ.'s')) THEN
!(cinter)C sos output
!(cinter)        SOSOUT = .TRUE.
!(cinter)C get output filename
!(cinter)        LABORT = .TRUE.
!(cinter)        CALL INTERF( NIN, NOUT, .FALSE., SOUT, 
!(cinter)     &  'output sos', FNAME, LABORT, '.sos')
!(cinter)        IF (LABORT) GOTO 55555	
!(cinter)	
!(cinter)      ELSE
!(cinter)C quanta output
!(cinter)        SOSOUT = .FALSE.
!(cinter)C get output filename
!(cinter)        LABORT = .TRUE.
!(cinter)        CALL INTERF( NIN, NOUT, .FALSE., SOUT, 
!(cinter)     &  'output binary hydra/quanta plot', FNAME, LABORT, '.qpt')
!(cinter)        IF (LABORT) GOTO 55555	
!(cinter)      ENDIF
!(cinter)      
!(cinter)C next question only for qpt output
!(cinter)      IF (SOSOUT) THEN
!(cinter)	LCEN = .FALSE.
!(cinter)      ELSE
!(cinter)C does the user want the centre lines drawn?
!(cinter)        CALL PROMPT( NOUT,
!(cinter)     & 'Do you want the centre lines drawn in output file? (y/n) <y>:')
!(cinter)        READ( NIN, '(A1)', ERR= 55555, END= 55555) STRING(1:1)
!(cinter)        IF (STRING(1:1).EQ.'N'.OR.STRING(1:1).EQ.'n') THEN
!(cinter)	  LCEN = .FALSE.
!(cinter)        ELSE
!(cinter)  	  LCEN = .TRUE.
!(cinter)        ENDIF
!(cinter)      ENDIF
!(cinter)      
!(cinter)C overide radii?
!(cinter)      CALL VTCLEAR( NOUT)
!(cinter)      WRITE( NOUT, '(A)')
!(cinter)     &' It is possible to enter a number to which all the sphere',
!(cinter)     &'   centre radii will be set regardless of value in file.'
!(cinter)      CALL PROMPT( NOUT,
!(cinter)     & 'Enter value <leave radii alone>:')
!(cinter)      READ( NIN, '(A)', ERR= 55555, END= 55555) STRING
!(cinter)C if the string is not blank try to read value
!(cinter)      IF (STRING(1:5).NE.'     ') THEN
!(cinter)	READ( STRING, '(BN,F10.0)') ROVER 
!(cinter)      ELSE
!(cinter)	ROVER = 0.
!(cinter)      ENDIF
!(cinter)
!(cinter)C Ignore small radii or Big?
!(cinter)      CALL VTCLEAR( NOUT)
!(cinter)      WRITE( NOUT, '(A)')
!(cinter)     &' Do you want to ignore small radius records on read?'
!(cinter)      CALL PROMPT( NOUT,
!(cinter)     & 'Enter lower limit to be used <use all records>:')
!(cinter)      READ( NIN, '(A)', ERR= 55555, END= 55555) STRING
!(cinter)C if the string is not blank try to read value
!(cinter)      IF (STRING(1:5).NE.'     ') THEN
!(cinter)	READ( STRING, '(BN,F10.0)') RLOWER 
!(cinter)      ELSE
!(cinter)	RLOWER = -1D06
!(cinter)      ENDIF
!(cinter)
!(cinter)C next question only for qpt output
!(cinter)      IF (SOSOUT) THEN
!(cinter)	LGRID = .FALSE.
!(cinter)      ELSE
!(cinter)      
!(cinter)C dot or grid surface?
!(cinter)        CALL VTCLEAR( NOUT)
!(cinter)        WRITE( NOUT, '(A)') 
!(cinter)     &' This program can either do a dot surface (which is fast) or',
!(cinter)     &'    a curving grid surface (which is must slower).'
!(cinter)        CALL PROMPT( NOUT,  
!(cinter)     & '   Do you want a dot (D) or grid (G) surface? <dot>:')
!(cinter)        READ( NIN, '(A) ', ERR=55555, END=55555) STRING
!(cinter)        CALL VTCLEAR( NOUT)
!(cinter)      
!(cinter)C if the string is not blank try to read value
!(cinter)        IF ((STRING(1:1).EQ.'G').OR.(STRING(1:1).EQ.'g')) THEN
!(cinter)
!(cinter)C July '94 - allow more uniform dot surface by
!(cinter)C value for dotd2
!(cinter)          WRITE( NOUT, '(A)')
!(cinter)     &' The density of the grid surface is controlled by DMULT',
!(cinter)     &' A value of 2.0 will produce a rather rough surface, 1.3 a',
!(cinter)     &' rather dense surface - 1.8 is a reasonable compromise.',
!(cinter)     &' N.b. procedure takes much longer than dot surface.'
!(cinter)          CALL PROMPT( NOUT,
!(cinter)     & 'What value for DMULT? <1.8>:')
!(cinter)          READ( NIN, '(A) ', ERR=55555, END=55555) STRING
!(cinter)C if the string is not blank try to read value
!(cinter)          IF (STRING(1:5).NE.'     ') THEN
!(cinter)            READ( STRING, '(BN,F10.0)' ) DMULT
!(cinter)          ELSE 
!(cinter)            DMULT = 1.8
!(cinter)          ENDIF
!(cinter)C doing distance search - use high value for dot density
!(cinter)C i042 shows that 25 seems to give reasonable results
!(cinter)	  DOTDEN = 25
!(cinter)          LGRID = .TRUE.
!(cinter)        ELSE 
!(cinter)
!(cinter)C normal quick dot surface
!(cinter)          LGRID = .FALSE.
!(cinter)        ENDIF
!(cinter)C end of if for qpt only
!(cinter)      ENDIF
!(cinter)	
!(cinter)C Doing dot surface?
!(cinter)      IF (.NOT.LGRID) THEN
!(cinter)C prompt value for dotden
!(cinter)        CALL VTCLEAR( NOUT)
!(cinter)        WRITE( NOUT, '(A)')
!(cinter)     &' The number of dots on the surface is controlled by DOTDEN',
!(cinter)     &'   The higher DOTDEN the more dots: 5 produces few dots, 20 alot'
!(cinter)        CALL PROMPT( NOUT,
!(cinter)     & 'What value for dotden? <10>:')
!(cinter)        READ( NIN, '(A)', ERR= 55555, END= 55555) STRING
!(cinter)C if the string is not blank try to read value
!(cinter)        IF (STRING(1:5).NE.'     ') THEN
!(cinter)	  READ( STRING, '(BN,I10)') DOTDEN
!(cinter)        ELSE
!(cinter)	  DOTDEN = 10
!(cinter)        ENDIF
!(cinter)      ENDIF
!(cinter)      
!(cinter)C does the user want to implement colour to illustrate the radius size?
!(cinter)      CALL PROMPT( NOUT, 
!(cinter)     &'Do you want to colour surface according to pore radius? <n>:')
!(cinter)      READ( NIN, '(A1)', ERR= 55555, END= 55555) STRING(1:1)
!(cinter)      IF ((STRING(1:1) .EQ.'Y').OR.(STRING(1:1) .EQ.'y')) THEN
!(cinter)	NPASS=3
!(cinter)	CALL PROMPT( NOUT,
!(cinter)     &  'What upper radius for low radius colour (red) <1.15>:')
!(cinter)	READ( NIN, '(A)', ERR= 55555, END= 55555) STRING   
!(cinter)C if the string is not blank try to read value
!(cinter)	IF (STRING(1:5).NE.'     ')THEN
!(cinter)	  READ( STRING, ' (BN,F10.0)') RCUT(1)
!(cinter)	ELSE
!(cinter)	  RCUT(1) = 1.15
!(cinter)	ENDIF
!(cinter)	CALL PROMPT( NOUT,
!(cinter)     &  'What upper radius for mid radius colour (green) <2.30>:')
!(cinter)	READ( NIN, '(A)', ERR= 55555, END= 55555) STRING   
!(cinter)C if the string is not blank try to read value
!(cinter)	IF (STRING(1:5).NE.'     ')THEN
!(cinter)	  READ( STRING, ' (BN,F10.0)') RCUT(2)
!(cinter)	ELSE
!(cinter)	  RCUT(2) = 2.3 
!(cinter)	ENDIF
!(cinter)	CALL PROMPT( NOUT,
!(cinter)     &  'What upper radius for high radius colour (blue) <999>:')
!(cinter)	READ( NIN, '(A)', ERR= 55555, END= 55555) STRING   
!(cinter)C if the string is not blank try to read value
!(cinter)	IF (STRING(1:5).NE.'     ')THEN
!(cinter)	  READ( STRING, ' (BN,F10.0)') RCUT(3)
!(cinter)	ELSE
!(cinter)	  RCUT(3) = 999.0
!(cinter)	ENDIF
!(cinter)      ELSE 
!(cinter)C do not colour according to radius - make only one pass
!(cinter)C outputing all records
!(cinter)	RCUT(1) = 999.0
!(cinter)	NPASS=1 
!(cinter)      ENDIF
!(cinter)C rcut zero must be negative 
!(cinter)      RCUT(0) =-1.0
!(cinter)
!(cinter)      CALL VTCLEAR( NOUT)

C have opened input and output files
C and got initial conditions start work
      CALL SPH_PROCESS_READ( NOUT, LERR, SIN, ROVER, RLOWER,
     &				   SPMAX, SPNO, SPXYZ, SPRAD, SPLAST,
     &				   SPSEC, SPCONN, SPEFFR, LCAPS,
     & 				   SAMPLE, CVECT, PERPVE, PERPVN,
     &				   CONNR, ENDRAD )
      IF (LERR) GOTO 55555

C draw centre line?
! but not if doing sos type output
      IF (LCEN.AND.(.NOT.SOSOUT).AND.(.NOT.LPEG)) THEN
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
! 4/4/04 move if either previous or this one is an end!
	  IF (SPLAST(SCOUNT-1).OR.SPLAST(SCOUNT)) THEN
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
        CALL SPHQPC( 5, NOUT, SOUT, LERR,
     &               DOTDEN, PTMAX, PTNO, PTXYZ,
     &               SPMAX, SPNO, SPXYZ, SPRAD, SPLAST, SPSEC,
     &               LGRID, NPASS, RCUT, DMULT, 
     &               VERMAX, VERNO, VERXYZ, LINMAX, LINNO, LINXYZ,
     &               SOSOUT)
     
      ELSEIF (LPEG) THEN     
        CALL SPHPEG( NOUT, SOUT, LERR,
     &               DOTDEN, PTMAX, PTNO, PTXYZ,
     &               SPMAX, SPNO, SPXYZ, SPRAD, SPEFFR, SPLAST,
     &		     SP_PEG_ACC,
     &               SOSOUT,
     & 		     SAMPLE, CVECT, PERPVE, PERPVN,
     &		     CONNR, ENDRAD, PROBE_PLUS, PROBE_NEG, PEGRAT,
     &               SP_TDO, SP_ENC, SP_CIRCRAD, SP_PEG_FLAG, 
     &		     SP_PEG_RAD, SHELL_ADD) 
      
      ELSEIF (TRIOUT) THEN
        ! direct output of triangles attempt may 2002
	CALL SPHTRI( NOUT, SOUT, LERR,
     &               DOTDEN, 
     &               SPMAX, SPNO, SPXYZ, SPRAD, SPEFFR, SPLAST,
     &               NPASS, RCUT, DMULT)

	
      ELSE
        CALL SPHQPU( NOUT, SOUT, LERR,
     &               DOTDEN, PTMAX, PTNO, PTXYZ,
     &               SPMAX, SPNO, SPXYZ, SPRAD, SPEFFR, SPLAST,
     &               LGRID, NPASS, RCUT, DMULT, 
     &               VERMAX, VERNO, VERXYZ, LINMAX, LINNO, LINXYZ,
     &               SOSOUT)
      ENDIF
      CLOSE( SOUT)

55555 CONTINUE
!      STOP 'FORTRAN STOP - sphqpt normal completion.'
      IF (LERR) THEN
        CALL EXIT(1)
      ELSE
        CALL EXIT(0)
      ENDIF
      END
