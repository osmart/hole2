      PROGRAM HOLE
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
C * All rights reserved                                              *
C * (c) 1997 Oliver Smart                    *
C *                                                                  *
C ********************************************************************
C
C Modification history:
C
C Date	Author		Modification
C 12/93	O.S. Smart	Original public release in HOLE suite beta1.0
C 05/94 O.S. Smart      Minor change - added volume integration (VOLINT)
C                       to final output for Mark Sansom (b1.1) NOW REMOVED!!
C 06/94 O.S. Smart      Major change - introduced more modularization
C			(s/r's hograp etc.) and the spherebox option.
C 05/95 O.S. Smart	Major change as spherebox option never really worked
C			introduced stripped down "capsule" option (LCAPS)
C 11/95 O.S. Smart	Modifications to colour numbers for release beta 2
C			MOLQPT option, SHORTO option, multiple input files,
C			CHARMD option
C 01/96 O.S. Smart	HOPEGG added to produce PEG polymer exclusion graph
C 03/96 O.S. Smart	H2DMAP added to produce 2D internal surface property
C			maps with the aid of Surfer.
C 28/02/97 O.S.S.	Release HOLE2 beta001
C 12/97 O.S.S.		cut off lists introduced to speed calculations
C			change here is vble cutsize
C 12/97 O.S.S.		RAT option introduced!?
C 01/00 O.S.S.    	CONN option introduced, LEXTRA key introduced
C 08/00 O.S.S.		s/r cguess Attempt to guess initial point cpoint
C			and cvect for server version introduced
C			
C This program is intended to aid analysis of holes thru' molecules,
C in particular ion channels.
C Co-ordinates are input in pdb format and the vdw radius of each atom
C is setup. The user supplies an initial point in the channel cavity
C and a vector in the rough direction of the channel. 
C Hole then proceeds to find the largest sphere which can be 
C accomodated without vdw overlap whose centre is in the plain 
C normal to the given vector through the given point.  
C This is achieved by a (Metropolis?) Monte Carlo method.
C The procedure is then repeated for plains parallel to the initial
C one until the ends of the channel are reached.
C Output is in two forms:
C (a) numerical the radius found is given vs distance along the channel
C (b) and optionally graphically in a HYDRA binary plot file,
C     a dot surface of the locus of the moving sphere is given
C     together with a couple of lines showing the nearest two atoms
C     to each sphere centre.
C 
C New option March 1993 to write sphere centres as a pdb format co-ord file
C with the radius of the sphere as occupancy and 
C distance along channel as b-factor.

C conditions to be read from unit 5 before we do any work

C input and output streams
      INTEGER			NIN, NOUT
      PARAMETER(		NIN  = 5)
      PARAMETER(		NOUT = 6)

C input coordinate filename
C (must be specified)
      CHARACTER*200		FCOORD
C input radius filename
C (must be specified)
      CHARACTER*200		FRADIU
C stream for input
      INTEGER			SIN

C hydra binary plot output file, a stream for it
C default to none - indicated by setting FHYDRA(1:4) to 'NONE'
      CHARACTER*200		FHYDRA
      INTEGER			SHYDRA

C pdb format output file 'atoms' are sphere centres
C default to none by setting FPDBSP to 'NONE'
      CHARACTER*200		FPDBSP
      INTEGER			SPDBSP

C an initial point in the channel
C (must be specified)
      DOUBLE PRECISION		CPOINT(3)

C a vector in the direction of the channel
C (must be specified)
      DOUBLE PRECISION		CVECT(3)
C unit vectors in plane normal to cvect

C needed to a number of routines - originally h2dmap but now conn
C and addend
C these are normal - first is east direction (x on plane)
C                    and second is north     (y on plane)
C calculated in s/r CALPER 
      DOUBLE PRECISION		PERPVE(3), PERPVN(3)

C a sampling distance - the distance between planes
C default to 0.5 angs
      DOUBLE PRECISION		SAMPLE

C a dot density for dot surface 
C same definition as in hydra - value between 5 and 30
C default to 10
      INTEGER			DOTDEN

C if are drawing graphical representation of results
C can turn off the dot surface by setting ldot false
C turn off lines joining each sphere edge to
C   the two closest atoms by setting ldot false
C can turn off line joining the sphere centres
      LOGICAL			LDOT, LLINE, LCEN

C New option April 1993 - instead of drawing dots
C draw a line from dot to surface of atom. 
C Turn on by setting lspike .true..
      LOGICAL			LSPIKE

C New option 22/11/93 - For wide channels need to
C be able to specify the radius at which an end is reached
      DOUBLE PRECISION		ENDRAD

C error found flag
      LOGICAL			LERR

C logical vairable - produce debug output
      LOGICAL			LDBUG

C maximum no of entries in lists
      INTEGER			MAXLST
      PARAMETER(		MAXLST = 100)

C bond radius list
      INTEGER			BNDNO
      CHARACTER*4		BNDBRK(MAXLST)
      DOUBLE PRECISION		BNDR(MAXLST)

C vdW radius list
      INTEGER			VDWNO
      CHARACTER*4		VDWBRK(MAXLST)
      CHARACTER*3		VDWRES(MAXLST)
      DOUBLE PRECISION		VDWR(MAXLST)

C logical function which opens file (readonly on vax)
      LOGICAL			OPENRO

C maximum no. of atoms
      INTEGER			ATMAX
      PARAMETER(		ATMAX =  50000)

C number of atoms read in from each file:
      INTEGER			ATNO

C atom names found in Brookhaven file:
      CHARACTER*4		ATBRK(ATMAX)

C residue name in brook
      CHARACTER*3		ATRES(ATMAX)

C chain identifier in brookhaven pdb file
      CHARACTER*1               ATCHN(ATMAX)

C integer residue no.
      INTEGER			ATRNO(ATMAX)

C co-ordinates
      DOUBLE PRECISION		ATXYZ( 3, ATMAX)

C vdw and bond radii of each atoms
      DOUBLE PRECISION		ATVDW(ATMAX), ATBND(ATMAX)

C 16/6/94 - spherebox option.
C s/r ****** needs vble ATRAD as store of the
C radius squared between the centre of boxsphere and
C each atom to avoid unnecessary calc
      DOUBLE PRECISION		ATRAD2( ATMAX)

C line for output
      CHARACTER*132		LINE

C the number of steps to be taken 
C on each Monte Carlo application
      INTEGER			MCSTEP

C max. length in angs of each Montecarlo displacement (angs),
C divison term in Monte Carlo probability (also angs)
C starting value 
      DOUBLE PRECISION		MCLEN, MCKTIN

C a displacement
      DOUBLE PRECISION		DISP(3)

C store for sphere centres and radii
C store the initial point as no 0 and subseqently points in the +ve
C bit. Points below the initial point are stored in -ve bit.
C maximum number of entries
C increased from 15000 to 50000 for rat option 27/7/98
      INTEGER			STRMAX
      PARAMETER(		STRMAX = 50000)
C the no. of entries, +Ve entries -Ve entries
      INTEGER			STRNOP, STRNON
C the centres and radii
      DOUBLE PRECISION		STRCEN(3,-STRMAX:STRMAX),
     &				STRRAD(-STRMAX:STRMAX)

C 08/07/94
C vbles to store the dimensions found in the spherebox options. 
      DOUBLE PRECISION          STRLVC(3,-STRMAX:STRMAX),
     &				STRLBO(-STRMAX:STRMAX),
     &				STRSBO(-STRMAX:STRMAX),
     &				STRBRD(-STRMAX:STRMAX)

C 18/05/95 new option for a capsule 
C turn on by setting LCAPS = true
C the capsule is a body radius strrad around line joining
C point strcen to strlvc (a second centre)
C STRCEN is used to store the effect radius of the capsule
C        which is the area normal to lvec divided by pi then rooted
C STRBRD is used to store the real capsule radius
      LOGICAL			LCAPS

C new feature 24 Oct 1993
C tell user the seed integer for the random no. generator
      INTEGER                   FSEED
C common block for first value of seed
      COMMON /CSEED/            FSEED

C will need PI
      DOUBLE PRECISION		PI

C logical flag to turn on spherebox option
      LOGICAL			LSPHBX

C 30/10/95 add facility to produce a .qpt of the molecule itself 
C for non-quanta users
      CHARACTER*200		MOLQPT
C now need to be able to setup bond radii
      LOGICAL			LNEEDB

C 11/95 introduce new variable to reduce text output -
C if oshort =
C 0: full text output
C 1: no in run progress reported
C 2: ditto plus no graph type output - only leaving 
C    Minimum radius & conductance calculations.
C 3: All text output other than input card mirroring
C    and error messages turned off
      INTEGER			SHORTO

C 11/95 introduce support for multiple pdb file read
C max number of files, number of files, filenames
      INTEGER			MULMAX, MULNUM
      PARAMETER(		MULMAX = 500)
      CHARACTER*200		MULNAM(MULMAX)
C loop count for multiple files
      INTEGER			MCOUNT

C 11/95 introduce a string which will list residues to be ignored on read
      CHARACTER*80		IGNRES

C 11/95 input multiple positions from charmm .dcd molecular dynamics file
C filename, input stream number.
C Number of datasets to skip before initial read, 
C number to skip between reads
      CHARACTER*200		FCHARM
      INTEGER			SCHARM, CHSKIP(2)

C vbles need for charmm dcd read
C need real vbles for charmm read
      REAL                      REXYZ( 3, ATMAX)
C number of free atoms & their list numbers
      INTEGER                   NFREAT, IFREAT(ATMAX)
C need to record on the initial read of the pdb file the 
C original atom numbers of the each atom from the pdb file
C - as we ignore some residues in the read.
C oatno(0) is the total number of original atoms in the pdb file
C oatno(1) is the original atom number of the stored atom#1 etc.
      INTEGER			OATNO(0:ATMAX)
C want to record the number of position we are working on
      INTEGER			IPOS
C logical function to read from dcd
      LOGICAL			HORCHR

C vbles for working peg effect on conductance -
C first number the ratio of conductance with PEG about to water
C second number is the ratio for non-penetrating PEG
      DOUBLE PRECISION		PEGRAT(2)       

C filename root for h2dmap - if not set to NONE 
C a number of files will be produced using this as basis
C appending map type and file identifier to end of this string
      CHARACTER*200		F2DMAP

C cut off list control vble, -ve: no cutoff lists used in calculation
C otherwise this is the additional radius above the sphere radius
C to be used for cutoff list (see s/r holeen)
      DOUBLE PRECISION		CUTSIZE

C option "rat" introduced 12/97 - this uses standard HOLE input but
C a completely different algorithm designed for finding escape routes from
C active sites.
      LOGICAL			LRAT

C option CONN introduced 1/00 - 
C CONNR(1) is the connolly surface probe radius (default 1.15Angs)
C          the option is turned off by setting this radius negative
C CONNR(2) is the grid size to be used in the calculation - default 1/2 probe
      DOUBLE PRECISION		CONNR(2)

C extra key controls whether information as for 
C Graph of surface properties vs coordinate along cvect
C and crude electrostatic potential
C produced by calls to HSURFP and HELEFI from HOGRAP
      LOGICAL			LEXTRA

C PEG_WRITEALL: extra key for new separate PEG calculation - program peg_calc
C turn on with FORPEG card in control file
C this vble is to be acted on in s/r CONCAL making it 
C write all the spheres found to the .sph file without any editing       
      LOGICAL			PEG_WRITEALL

C end of decs ******************

C greetings
      WRITE( NOUT,  '(A)')
     &' *** Program HOLE ***',
     &' ',
     &' If results of this run are reported or published, please', 
     &' cite the publication listed below.',
     &' For the hole program:',
     &'   Smart, O.S., Goodfellow, J.M., Wallace, B.A. (1993). ',
     &'   The pore dimensions of gramicidin A. ',
     &'   Biophys. J., 65, 2455-2460.',
!     &' For conductance prediction: ',
!     &'   Smart, O.S., Breed, J., Smith, G.R., Sansom, M.S.P. (1997).', 
!     &'   A novel method for structure-based prediction of ',
!     &'   ion channel conductance properties. ',
!     &'   Biophys. J. 72, 1109-1126.',
     &' ',
     &' Written by Oliver Smart at Birkbeck College from April 1992.',
     &' Copyright 1993,1997 by Oliver Smart and Birkbeck College',
     &' Copyright 2004,2005 by Oliver Smart ',
     &' Program modification number 2.2 002'
     
C write link time of program to screen
      CALL VERTIM( NOUT)

!      WRITE( NOUT,  '(A)')
!     &' For help on hole suite see ',
!     &' http://hole.biop.ox.ac.uk/hole/help'
C work out pi
      PI = 2.*ASIN(1.)

C default values
      SAMPLE = 0.25 ! changed from 0.5 Dec 2004
      DOTDEN = 10
      CPOINT(1) = -55555.
      CPOINT(2) = -55555.
      CPOINT(3) = -55555.
      CVECT(1)	= -55555.
      CVECT(2)	= -55555.
      CVECT(3)	= -55555.
      LDOT   = .TRUE.
C default draw dots rather than spikes
      LSPIKE = .FALSE.
      LLINE  = .FALSE.
      LCEN   = .TRUE.
      FCOORD = 'NONE'
      FRADIU = 'NONE'
      FHYDRA = 'NONE'
      FPDBSP = 'NONE'
      MOLQPT = 'NONE'
      FCHARM = 'NONE'
      F2DMAP = 'NONE'
      LNEEDB = .FALSE.
      SHORTO = 0
C Monte Carlo params see c039
      MCSTEP = 1000
      MCLEN  = 0.1
      MCKTIN = 0.1
C end of channel above 5 angs Nov 95 change to 15 angs
      ENDRAD = 15.
C strnop starts at -1 so that initial point stored as zero
C strnon @ 0 so that first -ve
      STRNOP = -1
      STRNON = 0
C default to sphere rather than spherebox or capsule option
      LSPHBX = .FALSE.
      LCAPS  = .FALSE.
      CHSKIP(1) = 0
      CHSKIP(2) = 0
      IPOS = 0
      PEGRAT(1) = 0.
      PEGRAT(2) = 0.
      CUTSIZE  = -1E09
      LRAT = .FALSE.
C default value for the connolly option set here - but
C turned off by setting -ve
      CONNR(1) = -1.15
      LEXTRA = .FALSE.
      PEG_WRITEALL = .FALSE.
      
C initial values
      LERR  = .FALSE.
      LDBUG = .FALSE.
C ignore residue string
      IGNRES = '........................................'//
     &         '........................................'

C read control variables from NIN
      CALL RCONTR( 
     &  NIN, NOUT, FCOORD, FRADIU, FHYDRA, FPDBSP, CPOINT, 
     &  CVECT, SAMPLE, DOTDEN, MCSTEP, MCLEN, MCKTIN,
     &  LDOT, LSPIKE, LLINE, LCEN, LERR, LDBUG, FSEED, ENDRAD,
     &  LSPHBX, LCAPS, MOLQPT, SHORTO, IGNRES, FCHARM, CHSKIP,
     &  PEGRAT, F2DMAP, CUTSIZE, LRAT, CONNR, LEXTRA, PEG_WRITEALL)

C if error found flag is returned true then stop
      IF (LERR) THEN
	WRITE(NOUT,*) 'ERROR found reading control vbles (s/r RCONTR)'
	GOTO 55555
      ENDIF

C have read all control values
C s/r holset
C (a) checks that everything which needs to be specified has been
C (b) mirror values back to user
C but does not open files
      CALL HOLSET( 
     &  NOUT, FCOORD, FRADIU, FHYDRA, FPDBSP, CPOINT, 
     &  CVECT, SAMPLE, DOTDEN, MCSTEP, MCLEN, MCKTIN,
     &  LDOT, LSPIKE, LLINE, LCEN, LERR, FSEED, ENDRAD, 
     &  LSPHBX, LCAPS, MOLQPT, SHORTO, IGNRES, FCHARM, 
     &  CHSKIP, PEGRAT, F2DMAP, CUTSIZE, LRAT, CONNR, PEG_WRITEALL)
C if error found flag is returned true then stop
      IF (LERR) THEN
	WRITE(NOUT,*) 'ERROR in HOLE setup (s/r HOLSET)'
	GOTO 55555
      ENDIF

C read radius records from fradiu
      CALL HORADR( NOUT, LERR, LDBUG, SHORTO, FRADIU,
     &  MAXLST, BNDNO, BNDBRK, BNDR, VDWNO, VDWBRK, VDWRES, VDWR)
      IF (LERR) THEN
	WRITE(NOUT,*) 'ERROR in vdw radius read (s/r HORADR)'
	GOTO 55555
      ENDIF

C open pdb co-ordinate file
C 15/11/95 adapt so that can set up to work with a number of files
C          fcoord will be supplied with a * wildcard.
      IF (INDEX( FCOORD, '*').NE.0) THEN 
C multiple files
        CALL HOMULF( NIN, NOUT, LERR, FCOORD, MULMAX, MULNUM, MULNAM)
        IF (LERR) GOTO 55555
C first file to work from is mulnam(1) - change fcoord to this
        FCOORD = MULNAM(1)
      ELSE
C normal single input file, indicate this
        MULNUM = 1
      ENDIF

C open first (or only) input pdb file
      IF (.NOT.OPENRO( SIN, FCOORD, NOUT)) THEN
C cannot open file
	WRITE(NOUT,*) '*** ERROR ***'
	WRITE(NOUT,*)'Cannot open pdb co-ord input file:'
	WRITE(NOUT,*) FCOORD(1:INDEX(FCOORD,'     ')-1)
	LERR = .TRUE.
	GOTO 55555
      ENDIF

C read pdb atom records and set up vdW and bond radii for each atom
C (the two logical control flags set up vdw and bond radii
C  for each atom - do not need bond radii in hole)
C If we are drawing the molecule need to setup bond radii.
      IF (MOLQPT(1:4).NE.'NONE') LNEEDB = .TRUE.
      CALL TSATR(  SIN, NOUT, LERR, LDBUG, .TRUE., LNEEDB,
     &	ATMAX, ATNO, ATBRK, ATRES, ATCHN, ATRNO, ATXYZ, ATVDW, ATBND,
     &	MAXLST, BNDNO, BNDBRK, BNDR, VDWNO, VDWBRK, VDWRES, VDWR,
     &  IGNRES, OATNO)
      IF (LERR) GOTO 55555

C tell user no. of atoms read
C tell user extact filename and no of records read
      INQUIRE( SIN, NAME = LINE)
      IF (SHORTO.LT.3) WRITE( NOUT, '(/A,I6,A/ A)') 
     &' Have read', ATNO,' atom records.',
     &' From file: '//LINE(1:INDEX(LINE,'   ')-1)
      IF ((SHORTO.LT.3) .AND. (OATNO(0).NE.ATNO)) 
     &  WRITE( NOUT, '(A,I6,A)')
     &' (Have ignored ', OATNO(0)-ATNO, ' atoms in the read)'

C close co-ord input file
      CLOSE( SIN)
C atoms read and vdw radii setup **
      IF (NOUT.EQ.6) CALL FLUSH6

C Do we have CHARMm file input?
C If so overwrite the coordinates atxyz with that of the 
C initial position from the file. File is left open to stream
C SCHARM for subsequent coordinate reads.
      IF (FCHARM(1:4).NE.'NONE') THEN
        CALL HORCHI( NOUT, LERR, SHORTO,
     &               FCHARM, SCHARM, CHSKIP(1), IPOS,
     &               ATMAX, ATNO, ATXYZ, REXYZ, NFREAT, IFREAT, OATNO)
        IF (LERR) THEN
	  WRITE(NOUT, '(A)') 
     &' ERROR in trying to read from charmm .dcd file (s/r HORCHI)'
	  GOTO 55555
        ENDIF
      ENDIF

C do initial graphics files setup 
C open/report on (a) qpt file (b) sph file and (c) molqpt
C (stick representation for the molecule is output to this file).
      CALL HOSETG( NOUT, LERR, SHORTO, 
     &             SHYDRA, FHYDRA, LLINE,
     &             SPDBSP, FPDBSP, LSPHBX,
     &             MOLQPT, ATMAX, ATNO, ATXYZ, ATBND)
      IF (LERR) THEN
	WRITE(NOUT,*) 'ERROR in graphics file open (s/r HOSETG)'
	GOTO 55555
      ENDIF

C only unit cvect if we do not have to guess it latter in s/r cguess
C detect by being set to -55555.
      IF (CVECT(1).NE.-55555.) THEN
C unit cvect
        CALL dUVEC2( CVECT)
        IF (LDBUG) WRITE(NOUT,*) 'debug unit cvect', CVECT
C calculate two vectors normal to plane of cvect
        CALL CALPER( NOUT, SHORTO, CVECT, PERPVE, PERPVN)
      ENDIF

C New feature 24 October 1993 write out seed integer used
C find one random 3d vector to kick things off
      CALL dURAN3( DISP)
      IF (SHORTO.LT.3) WRITE( NOUT, '(/A,I10/)') 
     &'  Seed integer used by ran # generator on this run ', FSEED

C ***********************************
C *** EVERYTHING IS SETUP FINALLY ***
C ***********************************


C jump back here if doing read from charmm dynamics dcd file
30    CONTINUE

C if we are doing a charmm dcd run then ipos will not be zero
      IF ((SHORTO.LT.3) .AND. (IPOS.NE.0)) 
     &  WRITE( NOUT, '(//70(''~'')/A,I5)')
     &' Starting calculation for position number ', IPOS  

C 20/11/95 flush the output stream
      IF (NOUT.EQ.6) CALL FLUSH6

C multiple pdb file support
C loop do only once if doing read from dcd file - jump back to 30
C at the end
      DO 10 MCOUNT = 1, MULNUM
C initialize vbles
C strnop starts at -1 so that initial point stored as zero
        STRNOP = -1
        STRNON = 0
        SAMPLE = ABS(SAMPLE)

C if we are not doing the first pdb file then need to reinitialize vbles
C read in coords etc.
        IF (MCOUNT.NE.1) THEN
          IF (SHORTO.LT.3) WRITE( NOUT, '(//70(''~'')/A,I5)') 
     &' Starting calculation for file number ', MCOUNT

C read atom records
          ATNO = 0
          FCOORD = MULNAM(MCOUNT)
          IF (.NOT.OPENRO( SIN, FCOORD, NOUT)) THEN
	    WRITE(NOUT,*) '*** ERROR ***'
	    WRITE(NOUT,*)'Cannot open pdb co-ord input file:'
	    WRITE(NOUT,*) FCOORD(1:INDEX(FCOORD,'     ')-1)
	    LERR = .TRUE.
	    GOTO 55555
          ENDIF
          CALL TSATR(  SIN, NOUT, LERR, LDBUG, .TRUE., LNEEDB,
     &  ATMAX, ATNO, ATBRK, ATRES, ATCHN, ATRNO, ATXYZ, ATVDW, ATBND,
     &  MAXLST, BNDNO, BNDBRK, BNDR, VDWNO, VDWBRK, VDWRES, VDWR,
     &  IGNRES, OATNO)
          IF (LERR) GOTO 55555
C tell user no. of atoms read
C tell user extact filename and no of records read
          INQUIRE( SIN, NAME = LINE)
          IF (SHORTO.LT.3) WRITE( NOUT, '(/A,I6,A/ A)') 
     &' Have read', ATNO,' atom records.',
     &' From file: '//LINE(1:INDEX(LINE,'   ')-1)
C close co-ord input file
          CLOSE( SIN)
C atoms read and vdw radii setup

C must have at leat one atom!
          IF (ATNO.EQ.0) THEN
	    LERR = .TRUE.
	    WRITE( NOUT, *) 'ERROR - no atoms!!!!!!'
	    GOTO 55555
	  ENDIF

C 20/11/95 flush the output stream
          IF (NOUT.EQ.6) CALL FLUSH6
        ENDIF


C 02/08/00 - 	guess cpoint and cvect if not specified
        IF ( (CPOINT(1).EQ.-55555.) .OR. 
     &       (CVECT(1).EQ.-55555.)      ) THEN
           CALL CGUESS( NOUT, SHORTO, LERR, CPOINT, CVECT, SAMPLE, 
     &                  ATMAX, ATNO, ATBRK, ATRES, ATCHN, ATRNO,
     &                  ATXYZ, ATVDW, ATBND, CUTSIZE)
C as cvect is make sure that it is unit-ed and normal vectors calculated
          CALL dUVEC2( CVECT) !  unit cvect
C calculate two vectors normal to plane of cvect
          CALL CALPER( NOUT, SHORTO, CVECT, PERPVE, PERPVN)
	ENDIF


C new RAT option to replace HOLCAL
        IF (LRAT) THEN
           CALL RATCAL( NOUT, SHORTO, LERR, CPOINT, CVECT, SAMPLE, 
     &			MCSTEP, 
     &                  ATMAX, ATNO, ATBRK, ATRES, ATCHN, ATRNO,
     &                  ATXYZ, ATVDW, ATBND, 
     &                  PI, ENDRAD, CUTSIZE, SPDBSP, FPDBSP,
     &			 STRMAX, STRNOP, STRNON, STRCEN, STRRAD)
          IF (LERR) GOTO 55555

        ELSE
C 15/11/95 split off HOLE calculation into seperate s/r 
          CALL HOLCAL( NIN, NOUT, LERR, CPOINT, CVECT, SAMPLE,
     &                   MCSTEP, MCKTIN, MCLEN,
     &                   ATMAX, ATNO, ATBRK, ATRES, ATCHN, ATRNO,
     &                   ATXYZ, ATVDW, ATBND, ATRAD2,
     &                   PI, LDBUG, SHYDRA, FHYDRA, SPDBSP, FPDBSP,
     &                   LLINE, LCAPS, LSPHBX, SHORTO,
     &                   STRMAX, STRNOP, STRNON, STRCEN, STRRAD,
     &                   STRLVC, STRLBO, STRSBO, STRBRD,
     &                   ENDRAD, CUTSIZE, CONNR, PERPVE, PERPVN,
     &			 PEG_WRITEALL)
          IF (LERR) GOTO 55555
        ENDIF

C Calculation finished output results (graphical and numerical)
        IF (LCAPS) THEN
C capsule output
          CALL HCAPGR( NOUT, SAMPLE, PI,
     &                 STRMAX, STRNOP, STRNON, STRCEN, STRRAD,
     &                 STRLVC, STRBRD,
     &                 ATMAX, ATNO, ATBRK, ATRES,
     &                 ATCHN, ATRNO, ATXYZ, ATVDW, ATBND,
     &                 CVECT, SHORTO,
     &		       MAX(IPOS,MCOUNT))
        ELSE
C 08/07/94 modularize the output of information for plotting graphs
          CALL HOGRAP( NOUT, SAMPLE, PI,
     &                 STRMAX, STRNOP, STRNON, STRCEN, STRRAD,
     &                 STRLVC, STRLBO, STRSBO, STRBRD,
     &                 ATMAX, ATNO, ATBRK, ATRES,
     &                 ATCHN, ATRNO, ATXYZ, ATVDW, ATBND,
     &                 LSPHBX, SHORTO,
     &		       MAX(IPOS,MCOUNT), CVECT, CUTSIZE,
     &		       CONNR, LEXTRA)
C 11/12/95 add seperate routine to calculate/write "blocking" radii
C          CALL HOBLOC( NOUT, CVECT,
C     &                 STRMAX, STRNOP, STRNON, STRCEN, STRRAD,
C     &                 SHORTO,
C     &		       MAX(IPOS,MCOUNT))
C 10/1/96 add yet another routine for calculate PEG radius type graph
          IF (PEGRAT(1).GT.0.) CALL HOPEGG( PEGRAT, NOUT, CVECT,
     &                 STRMAX, STRNOP, STRNON, STRCEN, STRRAD,
     &                 SHORTO, SAMPLE, ENDRAD, PI,
     &		       ATMAX, ATNO, ATXYZ, ATVDW)

C 20/3/96 Introduce a facility for doing 2D maps - will control
C with a filename flag eventually
          IF (F2DMAP(1:4).NE.'NONE')
     &      CALL H2DMAP( F2DMAP, NOUT, CVECT,
     &                   STRMAX, STRNOP, STRNON, STRCEN, STRRAD,
     &                   SHORTO, SAMPLE, ENDRAD, PI,
     &  	         ATMAX, ATNO, ATBRK, ATRES, 
     &		         ATCHN, ATRNO, ATXYZ, ATVDW, ATBND)


        ENDIF

C 08/07/94 modularize the output of dot surface
        IF (FHYDRA(1:4).NE.'NONE') THEN
C are we doing sphere-box?
          IF (LSPHBX) THEN
C special routine for drawing sphere-box dots etc.
            CALL HODOTB( SHYDRA, DOTDEN, LDOT, LLINE, LCEN, LSPIKE,
     &                   STRMAX, STRNOP, STRNON, STRCEN, STRRAD,
     &                   STRLVC, STRLBO, STRSBO, STRBRD, CVECT,
     &                   ATMAX, ATNO, ATXYZ, ATVDW, NOUT, LERR)
          ELSEIF (LCAPS) THEN
C special routine for drawing capsule dots etc.
            CALL HODOTC( SHYDRA, DOTDEN, LDOT, LLINE, LCEN, LSPIKE,
     &                   STRMAX, STRNOP, STRNON, STRCEN, 
     &			 STRLVC, STRBRD,
     &                   ATMAX, ATNO, ATXYZ, ATVDW, NOUT, LERR)
          ELSE
C doing normal spheres
            CALL HODOTU( SHYDRA, DOTDEN, LDOT, LLINE, LCEN, LSPIKE,
     &                   STRMAX, STRNOP, STRNON, STRCEN, STRRAD,
     &                   ATMAX, ATNO, ATXYZ, ATVDW, NOUT, LERR)
            IF (LERR) THEN
              WRITE( NOUT, *) 'ERROR found in s/r HODOTU'
              GOTO 55555
            ENDIF
          ENDIF
        ENDIF
10    CONTINUE


C if doing read from charmm dcd file then try to
C read in a further position
      IF (FCHARM(1:4).NE.'NONE') THEN
C use logical fn horchr for read - if true then have successfully
C read in further position otherwise returns false - either because 
C end of file reached or an error has been found.
C on successful read jump to 30 and do a further calculation
        IF (HORCHR( NOUT, LERR, SHORTO,
     &              FCHARM, SCHARM, CHSKIP(2), IPOS,
     &              ATMAX, ATNO, ATXYZ, REXYZ, NFREAT, IFREAT, OATNO))
     &   GOTO 30
        IF (LERR) THEN
          WRITE(NOUT, '(A)')
     &' ERROR in trying to read from charmm .dcd file (s/r HORCHR)'
          GOTO 55555
        ENDIF
      ENDIF


55555 CONTINUE
      WRITE( NOUT, '(A)') ' HOLE: normal completion'
      IF (NOUT.EQ.6) CALL FLUSH6
C use no stop - causes problems on some machines

      END
