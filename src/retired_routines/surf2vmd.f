      PROGRAM Surfvmd
      IMPLICIT NONE
C ********************************************************************
C *                                                                  *
C * This software is an unpublished work containing confidential and *
C * proprietary information of Birkbeck College. Use, disclosure,    *
C * reproduction and transfer of this work without the express       *
C * written consent of Birkbeck College are prohibited. This notice  *
C * must be attached to all copies or extracts of the software.      *
C *                                                                  *
C * (c) 2002 Oliver Smart & Birmingham University                    *
C *                                                                  *
C ********************************************************************
C
C Modification history:
C
C Date  Author          Modification
C 05/02 O.S.S. 		Original version
C
C reads in the

C routine FREDA used here to decode lines include common vbles
C vbles used by freda
C col: 	c*80		the command?
C kl: 	int 		no of words inputed 0 then default
C fn:	r*4(40) 	no.'s as they occur
C kn:     int     	no. of no's
C fl:     c*1(40,6)	words?
      INCLUDE 'FREDAINC'

C
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
      CHARACTER*200 		FSPHINPUT
      INTEGER			SIN2         ! its stream number
      INTEGER			END_FSPHINPUT
      
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


      INTEGER			REC_SPHERE
      DOUBLE PRECISION		TRIA(3),TRIB(3),TRIC(3),
     &				NORMA(3),NORMB(3),NORMC(3)

      INTEGER			TCOUNT
      DOUBLE PRECISION		DIST


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
     &' Copyright 1996,1997 by Oliver Smart and Birkbeck College',
     &' Copyright 1998 by Oliver Smart and Birmingham University',
     &' Program modification number 0001 15-11-00'

C write link time of program to screen
      CALL VERTIM( NOUT)


      ! get the controlling command line arguments

      ! establish values for s/r get
      REC_COMMANDS_HELP_MESSAGE =
     &'                     (help message start)\n'//
     &' Usage: '//
     &' surf2vmd  ((options)) '//
     &        ' -sph input_file.sph input_file.tri output_file\n'//
     &' options: \n'//
     &'  -colour (Rlow Rmid): produce a coloured surface\n'//
     &'                       surface will be \n'//
     &'                       red for pore radii < Rlow \n'//
     &'                       green for Rlow < pore radii < Rmid\n'//
     &'                       blue  for pore radii > Rmid\n'//
     &'                       default Rlow = 1.15 angs\n'//
     &'                       default Rmid = 2.30 angs\n'//
     &'                       (for connolly use effective radius\n'//
     &'                        of slab rather than pore radius\n'//
     &'  -color same as -colour for yanks\n'
      
     
     
     
     
!      call chrend( rec_commands_help_message, line_end)
!      write( nout, '(a)') rec_commands_help_message(1:line_end)
      REC_TOT = 3
      DO RCOUNT = 1,REC_TOT          ! initialize require store
        REC_REQUIRE_NUM(RCOUNT) = 0
	REC_REQUIRE_STRING(RCOUNT) = 0
      ENDDO 
      REC_COMMANDS(1) = 'colour'
      REC_COMMANDS(2) = 'color'
      REC_COMMANDS(3) = 'sph'
      REC_REQUIRE_STRING(3) = 1
      
      CALL GET_REC_COMMANDS( NOUT, LERR,
     &			     REC_COMMANDS_HELP_MESSAGE,
     &                       REC_TOT, REC_COMMANDS, 
     &                       REC_REQUIRE_NUM, REC_REQUIRE_STRING,
     &                       REC_FOUND, REC_NUM, REC_NUM_NUM,
     &			     REC_STRING, REC_STRING_NUM,
     &			     .TRUE., FINPUT, .TRUE., FOUTPUT)
      IF (LERR) GOTO 55555
      
                  
      ! REC_COMMANDS(1) = 'colour'
      ! REC_COMMANDS(2) = 'color'
      ! if we are doing colour NPASS=3
      ! if we are not doing colour NPASS=1 accept any radii
      NPASS = 1 
      RCUT(0) = -1.0
      RCUT(1) = 9999.0
      IF (REC_FOUND(1).OR.REC_FOUND(2)) THEN
        NPASS = 3
        RCUT(1) = 1.15
	RCUT(2) = 2.30
	RCUT(3) = 9999.0 ! default values for colour
	! pick up the radii if specified
	IF (REC_NUM(1,1).GT.-1E10) RCUT(1) = REC_NUM(1,1)
	IF (REC_NUM(1,2).GT.-1E10) RCUT(1) = REC_NUM(1,2)
	IF (REC_NUM(2,1).GT.-1E10) RCUT(2) = REC_NUM(2,1)
	IF (REC_NUM(2,2).GT.-1E10) RCUT(2) = REC_NUM(2,2)
      ENDIF

      ! REC_COMMANDS(3) = 'sph'
      IF (REC_FOUND(3)) THEN
        FSPHINPUT = REC_STRING(1, 3)
      ELSE
        WRITE( NOUT, '(A)')
     &'Error you must specify the .sph file as an input: ',
     &'surf2vmd -sph input.sph input.tri output.vmd_plot'
        LERR = .TRUE.
	GOTO 55555
      
      ENDIF

      ! end of unpacking arguments
      
      ! find last character in each filename
      CALL CHREND( FOUTPUT, END_FOUTPUT)
      CALL CHREND( FINPUT, END_FINPUT)
      CALL CHREND( FSPHINPUT, END_FSPHINPUT)
      
! open the input sph  file as readonly
      IF (.NOT.OPENRO( SIN, FINPUT, NOUT)) THEN
        WRITE( NOUT, '(A,A,A)')
     &' Error cannot open input file: ''', FINPUT(1:END_FINPUT), ''''
        LERR = .TRUE. ! have failed
        GOTO 55555
      ENDIF
! open the input sph  file as readonly
      IF (.NOT.OPENRO( SIN2, FSPHINPUT, NOUT)) THEN
        WRITE( NOUT, '(A,A,A)')
     &' Error cannot open input file: ''', FSPHINPUT(1:END_FSPHINPUT), 
     &                                                            ''''
        LERR = .TRUE. ! have failed
        GOTO 55555
      ENDIF

! open the output file as new
! use s/r newop - this keeps old version removes tilda's spaces etc. etc.
! arguments output stream, error indicator,
! reduce_output_variable (0 for full), filename, filestream, binary indicator
      IF (FOUTPUT(1:4).NE.'none') THEN
        LBIN = .FALSE.
        CALL NEWOP( NOUT, LERR, 0,
     &                  FOUTPUT, SOUT, LBIN)
        IF (LERR) THEN
          WRITE( NOUT, '(A,A,A)')
     &' Error cannot open output file: ''', FOUTPUT(1:END_FOUTPUT), ''''
          LERR = .TRUE. ! have failed
          GOTO 55555
        ENDIF
!	WRITE( SOUT, '(A)')
!     &'draw delete all ',   ! header
!     &'draw color green '
      ELSE
        ! no output file - only to be used for -peg option
        SOUT = -1
	WRITE( NOUT, '(A)')
     &'WARNING ''none'' specified as output file - so have not opened',
     &'        one.  This only makes sense with -peg option'
      ENDIF
      

      WRITE( NOUT, '(A,A,A)') 
     &'Have opened ', FINPUT(1:END_FINPUT),   ' for surf tri input ',
     &'        and ', FSPHINPUT(1:END_FSPHINPUT), ' for sph input',
     &'        and ', FOUTPUT(1:END_FOUTPUT), ' for output'


C have opened input and output files
C and got initial conditions start work
      ROVER  = 0.
      RLOWER = -1D06
      CALL SPH_PROCESS_READ( NOUT, LERR, SIN2, ROVER, RLOWER,
     &				   SPMAX, SPNO, SPXYZ, SPRAD, SPLAST,
     &				   SPSEC, SPCONN, SPEFFR, LCAPS,
     & 				   SAMPLE, CVECT, PERPVE, PERPVN,
     &				   CONNR, ENDRAD )
      write( *, *) sprad(1)     
      IF (LERR) GOTO 55555
      DO SCOUNT = 1, SPNO
        DO TCOUNT = 1, SPNO
	  IF(SCOUNT.NE.TCOUNT) THEN ! look at the pair if different
	    ! Q: is the sphere scount inside sphere tcount
	    DIST = (SPXYZ(1,SCOUNT)-SPXYZ(1,TCOUNT))**2 +
     &             (SPXYZ(2,SCOUNT)-SPXYZ(2,TCOUNT))**2 +
     &             (SPXYZ(3,SCOUNT)-SPXYZ(3,TCOUNT))**2 
            IF (DIST.LT.SPRAD(TCOUNT)**2) THEN
	      ! could be inside - centre of scount is in tcount
	      DIST = SQRT(DIST)
	      DIST = DIST+ABS(SPRAD(SCOUNT))
	      IF (DIST.LE.ABS(SPRAD(TCOUNT))) THEN
	        ! inside - mark by chaging case of radius
		SPRAD(SCOUNT) = -ABS(SPRAD(SCOUNT))
	      ENDIF
	    ENDIF
	  
	  ENDIF
	ENDDO
      ENDDO
      DO SCOUNT = 1, SPNO
        WRITE( SOUT,'(4F10.4)')
     &   SPXYZ(1,SCOUNT), SPXYZ(2,SCOUNT), SPXYZ(3,SCOUNT),
     &   SPRAD(SCOUNT)
      ENDDO
C! now we have the .sph file in memory go through the tri file
C10    CONTINUE
C	
C        READ( SIN, '(A)', END=55) COL 
C        CALL FREDA(1,80,FL,6)
C        IF (KN.EQ.1) THEN
C 	 REC_SPHERE = FN(1)
C        ELSE
C 	 stop
C        ENDIF 

C        READ( SIN, '(A)', END=55) COL
C        CALL FREDA(1,80,FL,6)
C        IF (KN.EQ.6) THEN
C 	 TRIA(1)  = FN(1)
C 	 TRIA(2)  = FN(2)
C 	 TRIA(3)  = FN(3)
C 	 NORMA(1) = FN(4)  
C 	 NORMA(2) = FN(5)  
C 	 NORMA(3) = FN(6)  
C        ELSE
C	 stop
C        ENDIF  

C	READ( SIN, '(A)', END=55) COL
C	CALL FREDA(1,80,FL,6)
C	IF (KN.EQ.6) THEN
C	  TRIB(1)  = FN(1)
C	  TRIB(2)  = FN(2)
C	  TRIB(3)  = FN(3)
C	  NORMB(1) = FN(4)  
C	  NORMB(2) = FN(5)  
C	  NORMB(3) = FN(6)  
C	ELSE
C	  stop
C	ENDIF  
C	CALL FREDA(1,80,FL,6)
C	READ( SIN, '(A)', END=55) COL
C	IF (KN.EQ.6) THEN
C	  TRIC(1)  = FN(1)
C	  TRIC(2)  = FN(2)
C	  TRIC(3)  = FN(3)
C	  NORMC(1) = FN(4)  
C	  NORMC(2) = FN(5)  
C	  NORMC(3) = FN(6)  
C	ELSE
C	  stop
C	ENDIF  

C!	WRITE( SOUT, '(12(A,3F11.5) )')  
C!     &'draw trinorm { ', TRIA,  ' }  {', TRIB, '}  {', TRIC, ' }  {',
C!     & NORMA,  ' }  {', NORMB, ' }  {',   NORMC, ' }'
C	WRITE( SOUT, '(12(A,3F11.5) )')  
C     &'draw triange { ', TRIA,  ' }  {', TRIB, '}  {', TRIC, ' }'
C      GOTO 10
C55    CONTINUE

55555 CONTINUE
      STOP 'FORTRAN STOP - sphqpt normal completion.'
      END
