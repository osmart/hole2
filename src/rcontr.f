      SUBROUTINE RCONTR( 
     &   NIN, NOUT, FCOORD, FRADIU, FHYDRA, FPDBSP, CPOINT,
     &   CVECT, SAMPLE, DOTDEN, MCSTEP, MCLEN, MCKTIN,
     &   LDOT, LSPIKE, LLINE, LCEN, LERR, LDBUG, FSEED, ENDRAD,
     &   LSPHBX, LCAPS, MOLQPT, SHORTO, IGNRES, FCHARM, CHSKIP,
     &   PEGRAT, F2DMAP, CUTSIZE, LRAT, CONNR, LEXTRA, PEG_WRITEALL)
      IMPLICIT NONE
C ********************************************************************
C *                                                                  *
C * This software is an unpublished work containing confidential and *
C * proprietary information of Birkbeck College. Use, disclosure,    *
C * reproduction and transfer of this work without the express       *
C * written consent of Birkbeck College are prohibited. This notice  *
C * must be attached to all copies or extracts of the software.      *
C *                                                                  *
C * (c) 1993 Oliver Smart & Birkbeck College, All rights reserved    *
C * (c) 1996 Oliver Smart & Birkbeck College, All rights reserved    *
C * (c) 1997 Oliver Smart                    *
C *                                                                  *
C ********************************************************************
C
C Modification history:
C
C Date	Author		Modification
C 12/93	O.S. Smart	Original public release in HOLE suite beta1.0
C 05/94 O.S. Smart      spherebox option flag read (LSPHBX) added
C 05/95 O.S. Smart	capsule option flag LCAPS read added
C 11/95 O.S. Smart	LINOFF card changed to LINON
C 11/95 O.S. Smart	s/r STRIPS used to strip initial blanks
C			new options: MOLQPT, SHORTO, IGNORE, CHARMD
C 01/96 O.S. Smart	new option PEGRAT added
C 02/96 O.S. Smart      new s/r dtilda added to resolve ~'s in filenames
C 12/97 O.S.S.		new vble CUTSIZE added
C 12/97 O.S.S.		RAT option introduced!?
C 01/00 O.S.S.    	CONN option introduced
C 10/00 O.S.S.    	FORPEG CARD introduced (to PEG_WRITEALL vble)

C this s/r read values for control vbles for program hole from input
C stream nin. Uses s/r freda to decode lines. N.b. no check is made
C on specification here - this is done in hole.
C conventions used in this s/r MUST be documented in hole.doc

C routine FREDA used here to decode lines include common vbles
C vbles used by freda
C col: 	c*80		the command?
C kl: 	int 		no of words inputed 0 then default
C fn:	r*4(40) 	no.'s as they occur
C kn:     int     	no. of no's
C fl:     c*1(40,6)	words?
      INCLUDE 'FREDAINC'

C input and output streams
C returned unchanged!
      INTEGER			NIN, NOUT

C input coordinate filename
      CHARACTER*200		FCOORD
C input radius filename
      CHARACTER*200		FRADIU
C hydra binary plot output file - default to none 
      CHARACTER*200		FHYDRA
C pdb format file for sphere centre info
      CHARACTER*200		FPDBSP
C 
C an initial point in the channel
      DOUBLE PRECISION		CPOINT(3)

C a vector in the direction of the channel
      DOUBLE PRECISION		CVECT(3)

C a sampling distance - the distance between planes
C default to 0.5 angs
      DOUBLE PRECISION		SAMPLE

C a dot density for dot surface 
C same definition as in hydra - value between 5 and 30
C default to 15
      INTEGER			DOTDEN

C the no. of Monte Carlo steps
      INTEGER			MCSTEP

C max. length in angs of each Montecarlo displacement (angs),
C divison term in Monte Carlo probability (also angs)
C starting value and current value
      DOUBLE PRECISION		MCLEN, MCKTIN

C
C if are drawing graphical representation of results
C can turn off the dot surface by setting ldot false
C or turn off lines joining each sphere centre to 
C the two closest atoms	by setting ldot false
      LOGICAL			LDOT, LLINE, LCEN

C if lspike is set true produce spikes from sphere surface 
C rather than dots
      LOGICAL			LSPIKE

C if error found set LERR true
      LOGICAL			LERR

C if ldbug set true then mirror input
      LOGICAL			LDBUG

C 24 October 1993 - new feature allow user specify 
C the seed integer for random number generator
      INTEGER			FSEED

C 22 Nov 1993 - allow user to specify radius above which
C result is regarded as an end
      DOUBLE PRECISION		ENDRAD

C 17 June 1994 - new flag (LSPHBX) to turn on ellipsoid option
      LOGICAL			LSPHBX

C 18/05/95 new option for a capsule
C turn on by setting LCAPS = true
      LOGICAL                   LCAPS

C 30/10/95 add facility to produce a .qpt of the molecule itself
C for non-quanta users
      CHARACTER*200             MOLQPT

C 11/95 introduce new variable to reduce text output -
C if shorto =
C 0: full text output
C 1: no in run progress reported
C 2: ditto plus no graph type output - only leaving
C    Minimum radius & conductance calculations.
C 3: All text output other than input card mirroring
C    and error messages turned off
      INTEGER                   SHORTO


C 11/95 string listing residues not be read
      CHARACTER*80		IGNRES

C 11/95 input multiple positions from charmm .dcd molecular dynamics file
C filename,  Number of datasets to skip before initial read,
C number to skip between reads
      CHARACTER*200             FCHARM
      INTEGER                   CHSKIP(2)

C vbles for working peg effect on conductance -
C first number the ratio of conductance with PEG about to water
C second number is the ratio for non-penetrating PEG
      DOUBLE PRECISION          PEGRAT(2)

C filename root for h2dmap - if not set to NONE
C a number of files will be produced using this as basis
C appending map type and file identifier to end of this string
      CHARACTER*200             F2DMAP

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
C CONNR(2) is the grid size to be used in the calculation - default .25 angs
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

C internal variables ***********

C the first word returned by freda
      CHARACTER*6		KEY

C dummy integer
      INTEGER			IDUM

C end of decs ******************

C mirror input lines
      WRITE( NOUT,  '(/1X,A)')
     & 'Control variables read:'

C read line do until EOF or STOP read and then return @55555
10    CONTINUE

	READ( NIN, '(A)', END= 55555) COL
	CALL CHREND( COL, IDUM)
	WRITE( NOUT,'(A)') COL(1:IDUM)

C remove any comment
	CALL DECOM( COL)

C if debugging mirror line back
	IF (LDBUG) WRITE( NOUT,  '(1X,A,A)')
     &	'debug dcom line: ', COL(1:IDUM)

C if line blank then read next
	IF (COL(1:5).EQ.'     ') GOTO 10

C decode line with freda
	CALL FREDA(1,80,FL,6)

C first word is key
C No word?
	IF (KL.EQ.0) GOTO 10
C fl:	  c*1(40,6)	words?
	KEY = FL(1,1)//FL(1,2)//FL(1,3)//FL(1,4)//FL(1,5)//FL(1,6)
C make upper case
	CALL UCASE( KEY)
	IF (LDBUG) WRITE( NOUT,  '(1X,A,A)')
     &		  'debug key: ', key

C key
	IF (KEY.EQ.'STOP') THEN
C stop input
	  GOTO 55555

C pdb input filename
	ELSEIF (KEY(1:5).EQ.'COORD') THEN
	  FCOORD = COL(6:80)
C strips strips off any leading blanks or tabs from the filename
          CALL STRIPS(FCOORD)
C resolve any tilda's (~) in filename
          CALL DTILDA(FCOORD)

C radius filename
	ELSEIF (KEY.EQ.'RADIUS') THEN
	  FRADIU = COL(7:80)
C strips strips off any leading blanks or tabs from the filename
          CALL STRIPS(FRADIU)
C resolve any tilda's (~) in filename
          CALL DTILDA(FRADIU)

C molqpt filename
        ELSEIF (KEY.EQ.'MOLQPT') THEN
          MOLQPT = COL(7:80)
C strips strips off any leading blanks or tabs from the filename
          CALL STRIPS(MOLQPT)
C resolve any tilda's (~) in filename
          CALL DTILDA(MOLQPT)

C binary plot filename - output
	ELSEIF (KEY.EQ.'PLTOUT') THEN
	  FHYDRA = COL(7:80)
C strips strips off any leading blanks or tabs from the filename
          CALL STRIPS(FHYDRA)
C resolve any tilda's (~) in filename
          CALL DTILDA(FHYDRA)

C pdb file for sphere info - output
	ELSEIF (KEY.EQ.'SPHPDB') THEN
	  FPDBSP = COL(7:80)
C strips strips off any leading blanks or tabs from the filename
          CALL STRIPS(FPDBSP)
C resolve any tilda's (~) in filename
          CALL DTILDA(FPDBSP)

C 2d property output filename root
	ELSEIF (KEY.EQ.'2DMAPS') THEN
	  F2DMAP = COL(7:80)
          CALL STRIPS(F2DMAP)
C resolve any tilda's (~) in filename
          CALL DTILDA(F2DMAP)

C charm dcd file
        ELSEIF (KEY.EQ.'CHARMD') THEN
          FCHARM = COL(7:80)
C strips strips off any leading blanks or tabs from the filename
          CALL STRIPS(FCHARM)
C resolve any tilda's (~) in filename
          CALL DTILDA(FCHARM)

C number of positions to skip on charmm read
        ELSEIF (KEY.EQ.'CHARMS') THEN
          IF (KN.LT.2) THEN
            LERR = .TRUE.
            WRITE( NOUT,'(/20(1X,A:))')
     &'ERROR: CHARMS (number of positions to skip on .dcd read)',
     &'must be specified with two numbers ',
     &'(the initial skip & number to skip between reads',
     &  'line read:', COL
          ENDIF
          CHSKIP(1) = FN(1)
          CHSKIP(2) = FN(2)

C peg graph ratios
        ELSEIF (KEY.EQ.'PEGRAT') THEN
          IF (KN.LT.2) THEN
            LERR = .TRUE.
            WRITE( NOUT,'(A)')
     &' ',
     &' ERROR: PEGRAT (ratio of conductances in presence and ',
     &'                with non-penetrating PEGs)',
     &' must be specified with two numbers ',
     &' line read: '//COL
          ENDIF
          PEGRAT(1) = FN(1)
          PEGRAT(2) = FN(2)

C turn dot surface off?
	ELSEIF (KEY.EQ.'DOTOFF') THEN
	  LDOT = .FALSE.
C spikes rather than dots?
	ELSEIF (KEY.EQ.'SPIKES') THEN
          LSPIKE = .TRUE.
C turn drawing of lines off?
C in old days used LINOFF card - now do LINON
	ELSEIF (KEY.EQ.'LINOFF') THEN
	  CONTINUE
	ELSEIF (KEY.EQ.'LINON ') THEN
	  LLINE = .TRUE. 
C turn drawing of centre line off?
	ELSEIF (KEY.EQ.'CENOFF') THEN
	  LCEN = .FALSE.
C dot density
	ELSEIF (KEY.EQ.'DOTDEN') THEN
	  IF (KN.LT.1) THEN
	    LERR = .TRUE.
	    WRITE( NOUT,'(/20(1X,A:))')
     &	'ERROR: DOTDEN (the dot density for surface)',
     &	'must be specified with a number!',
     &	'line read:', COL
	  ENDIF
	  DOTDEN = INT(FN(1))

C Monte Carlo step
	ELSEIF (KEY.EQ.'MCSTEP') THEN
	  IF (KN.LT.1) THEN
	    LERR = .TRUE.
	    WRITE( NOUT,'(/20(1X,A:))')
     &	'ERROR: MCSTEP (the no of Monte Carlo steps)',
     &	'must be specified with a number!',
     &	'line read:', COL
	  ENDIF
	  MCSTEP = INT(FN(1))

C Monte Carlo maximum displacement
	ELSEIF (KEY.EQ.'MCDISP') THEN
	  IF (KN.LT.1) THEN
	    LERR = .TRUE.
	    WRITE( NOUT,'(/20(1X,A:))')
     &	'ERROR: MCDISP (the max Monte Carlo displacement)',
     &	'must be specified with a number!',
     &	'line read:', COL
	  ENDIF
	  MCLEN = FN(1)

C Monte Carlo initial KT
	ELSEIF (KEY.EQ.'MCKT') THEN
	  IF (KN.LT.1) THEN
	    LERR = .TRUE.
	    WRITE( NOUT,'(/20(1X,A:))')
     &	'ERROR: MCKT (the initial Monte Carlo KT)',
     &	'must be specified with a number!',
     &	'line read:', COL
	  ENDIF
	  MCKTIN = FN(1)

C sampling distance
	ELSEIF (KEY.EQ.'SAMPLE') THEN
	  IF (KN.LT.1) THEN
	    LERR = .TRUE.
	    WRITE( NOUT,'(/20(1X,A:))')
     &	'ERROR: SAMPLE (the distance between planes)',
     &	'must be specified with a number!',
     &	'line read:', COL
	  ENDIF
	  SAMPLE = FN(1)

C seed for random number generator
        ELSEIF (KEY.EQ.'RASEED') THEN
          IF (KN.LT.1) THEN
            LERR = .TRUE.
            WRITE( NOUT,'(/20(1X,A:))')
     &  'ERROR: RASEED (seed number for random # gen)',
     &  'must be specified with a number!',
     &  'line read:', COL
          ENDIF
C get problem using freda - use read instead
          READ( COL(8:18),'(BN,I10)') FSEED

C radius for end
        ELSEIF (KEY.EQ.'ENDRAD') THEN
          IF (KN.LT.1) THEN
            LERR = .TRUE.
            WRITE( NOUT,'(/20(1X,A:))')
     &  'ERROR: ENDRAD (radius for end)',
     &  'must be specified with a number!',
     &  'line read:', COL
          ENDIF
          ENDRAD = FN(1)

C channel direction vector
	ELSEIF (KEY.EQ.'CVECT ') THEN
	  IF (KN.LT.3) THEN
	    LERR = .TRUE.
	    WRITE( NOUT,'(/20(1X,A:))')
     &	'ERROR: CVECT (the channel direction vector)',
     &	'must be specified with three no.''s',
     &	'line read:', COL
	  ENDIF
C the vector
	  CVECT(1) = FN(1)
	  CVECT(2) = FN(2)
	  CVECT(3) = FN(3)
C point in channel
	ELSEIF (KEY.EQ.'CPOINT') THEN
	  IF (KN.LT.3) THEN
	    LERR = .TRUE.
	    WRITE( NOUT,'(/20(1X,A:))')
     &	'ERROR: CPOINT (a point in the channel)',
     &	'must be specified with three no.''s',
     &	'line read:', COL
	  ENDIF
C the vector
	  CPOINT(1) = FN(1)
	  CPOINT(2) = FN(2)
	  CPOINT(3) = FN(3)

C turn on boxsphere option        
        ELSEIF (KEY.EQ.'SPHBOX') THEN
          LSPHBX = .TRUE. 

C turn on capsule option        
        ELSEIF (KEY.EQ.'CAPSUL') THEN
          LCAPS  = .TRUE. 

C turn on short output option
        ELSEIF (KEY.EQ.'SHORTO') THEN
C if no integer value specified then default to 1
          SHORTO  = 1     
          IF (KN.GE.1) SHORTO = FN(1)

C residue types to be ignore
        ELSEIF (KEY.EQ.'IGNORE') THEN
	  IGNRES = COL(7:80)
C should be upper case
          CALL UCASE(IGNRES)
C strip off leading blanks
          CALL STRIPS(IGNRES)

C cut off list control vble
        ELSEIF (KEY(1:3).EQ.'CUT') THEN
          IF (KN.LT.1) THEN
            LERR = .TRUE.
            WRITE( NOUT,'(/20(1X,A:))')
     &  'ERROR: CUTSIZE (cut off list control vble)',
     &  'must be specified with a number!',
     &  'line read:', COL
          ENDIF
          CUTSIZE = FN(1)

C turn on the rat option?
	ELSEIF (KEY(1:3).EQ.'RAT') THEN
	  LRAT = .TRUE.

C turn on the conn  option?
	ELSEIF (KEY(1:4).EQ.'CONN') THEN
C default radius  set in as -ve CONNR(1)
        CONNR(1) = -CONNR(1)
C first number if present is the probe radius, 2nd grid size
	  IF (KN.GE.1) CONNR(1) = FN(1)
C default grid size 0.7* connr(1)
          CONNR(2) = 0.7*CONNR(1)	  
	  IF (KN.GE.2) CONNR(2) = FN(2)

C turn on the extra information option?
	ELSEIF (KEY(1:5).EQ.'EXTRA') THEN
	  LEXTRA = .TRUE.

C PEG_WRITEALL: extra key for new separate PEG calculation - program peg_calc
C turn on with FORPEG card in control file
C this vble is to be acted on in s/r CONCAL making it 
C write all the spheres found to the .sph file without any editing       
	ELSEIF (KEY(1:6).EQ.'FORPEG') THEN
	  PEG_WRITEALL = .TRUE.

	ELSE
C unregonized line - tell user
	  WRITE( NOUT, '(1X,A,A)')
     &		  '***Unrecognized line read: ', COL
	ENDIF


C read next line
      GOTO 10

55555 RETURN
      END
