      SUBROUTINE HOLSET( NOUT, FCOORD, FRADIU, FHYDRA, FPDBSP, CPOINT,
     &  CVECT, SAMPLE, DOTDEN, MCSTEP, MCLEN, MCKTIN,
     &  LDOT, LSPIKE, LLINE, LCEN, LERR, FSEED, ENDRAD,
     &  LSPHBX, LCAPS, MOLQPT, SHORTO, IGNRES, FCHARM, CHSKIP, 
     &  PEGRAT, F2DMAP, CUTSIZE, LRAT, CONNR, PEG_WRITEALL)
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
C * (c) 2000 Oliver Smart                    *
C *                                                                  *
C ********************************************************************
C
C Modification history:
C
C Date	Author		Modification
C 12/93	O.S. Smart	Original public release in HOLE suite beta1.0
C 06/94 O.S. Smart	information about spherebox option added
C 05/95 O.S. Smart	information about capsule option added
C 11/95 O.S. Smart	information about MOLQPT, SHORTO, FCHARM added
C 01/96 O.S. Smart      new option PEGRAT added
C 03/96 O.S.S.		F2DMAP output added
C 12/97 O.S.S.		new vble CUTSIZE added
C 01/00 O.S.S.		CONNR added
C 08/00 O.S.S.		s/r cguess Attempt to guess initial point cpoint
C			and cvect for server version introduced

C this s/r
C (a) checks that everything which needs to be specified has been
C (b) mirror values back to user
C but does not open files

C output stream
C returned unchanged!
      INTEGER			NOUT

C input coordinate filename
      CHARACTER*200		FCOORD
C input radius filename
      CHARACTER*200		FRADIU
C hydra binary plot output file
      CHARACTER*200		FHYDRA
C file for sphere centre info to be output in pdb form
      CHARACTER*200		FPDBSP

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
      LOGICAL                   LSPIKE

C if error found set LERR true
      LOGICAL			LERR

C 05/94 flag to control whether the program does
C spherical type output or spherebox
      LOGICAL			LSPHBX

C 18/05/95 new option for a capsule
C turn on by setting LCAPS = true
C the capsule is a body radius strrad around line joining
C point strcen to strlvc (a second centre)
      LOGICAL                   LCAPS

C internal vbles

C last character in filename
      INTEGER			IEND1, IEND2

C 24 October 1993 - new feature allow user specify
C the seed integer for random number generator
      INTEGER                   FSEED

C 22 Nov 1993 - allow user to specify radius above which
C result is regarded as an end
      DOUBLE PRECISION          ENDRAD

C 30/10/95 add facility to produce a .qpt of the molecule itself
C for non-quanta users
      CHARACTER*200             MOLQPT

C 11/95 introduce new variable to reduce text output -
C if oshort =
C 0: full text output
C 1: no in run progress reported
C 2: ditto plus no graph type output - only leaving
C    Minimum radius & conductance calculations.
C 3: All text output other than input card mirroring
C    and error messages turned off
      INTEGER                   SHORTO

C string giving residue types which should be ignored (not read)
C during hole run
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

C extra key for new PEG calculation - to be acted on in
C "CONCAL" - write all the spheres without any editing
      LOGICAL			PEG_WRITEALL

C end of decs ******************


C leave a couple of lines blank
      IF (SHORTO.LT.3) THEN
        WRITE( NOUT, *)
        WRITE( NOUT, *)
      ENDIF

C check that required values have been specified
      IF (FCOORD(1:4).EQ.'NONE') THEN
	LERR = .TRUE.
	WRITE(NOUT,*) 'ERROR - have not specified COORD record'
      ENDIF
      IF (FRADIU(1:4).EQ.'NONE') THEN
	LERR = .TRUE.
	WRITE(NOUT,*) 'ERROR - have not specified RADIUS record'
      ENDIF
      IF (CVECT(1).EQ.-55555.) THEN
C 02/08/00 - turn off check on cvect cpoint - now try to guess
C if not stated
C	LERR = .TRUE.
C	WRITE(NOUT,*) 'ERROR - have not specified CVECT record'
        WRITE( NOUT, *) 
     &'WARNING have not found a CVECT record - will try to guess'
      ENDIF
      IF (CPOINT(1).EQ.-55555.) THEN
C	LERR = .TRUE.
C	CWRITE(NOUT,*) 'ERROR - have not specified CPOINT record'
        WRITE( NOUT, *) 
     &'WARNING have not found a CPOINT record - will try to guess'
      ENDIF
      IF (LERR) THEN
        WRITE(NOUT, '(A)') 
     &' see HOLE documentation in ~/hole2/doc for details!'
	GOTO 55555
      ENDIF


C rat option
      IF (LRAT) THEN
        WRITE(NOUT, '(A)') 
     &' RAT HAS BEEN TURNED ON. Many other options now irrelvant',
     &' Currently under development'
      ENDIF

C Conn option
      IF (CONNR(1).GT.0.0) THEN
        WRITE(NOUT, '(A/ A,F8.3,A/ A,F8.3,A/ A)') 
     &' CONN option turned on',
     &'   Connolly probe radius= ', CONNR(1), ' Angs',
     &'   Sample grid size= ', CONNR(2), ' Angs',
     &'  (To change specify probe radius/grid size after CONN card)'
! PEG_WRITEALL report
        IF (PEG_WRITEALL) WRITE( NOUT, '(A)')
     &' FORPEG card has been found - .sph file will contain all records'


      ENDIF


C tell user what file names will be used
C unless text output turned off
      IF (SHORTO.LT.3) THEN
        CALL CHREND( FCOORD, IEND1)
        CALL CHREND( FRADIU, IEND2)
        WRITE( NOUT, '(A)')
     &' Input pdb filename read as:    '''//FCOORD(1:IEND1)//''''
C tell user any charmm filename
        IF (FCHARM(1:4).NE.'NONE') THEN
          CALL CHREND( FCHARM, IEND1)
          WRITE( NOUT, '(1X,72(''*''))')
          WRITE( NOUT, '(A)')
     &' This pdb file will be used as a template for input from',
     &' CHARMm binary dynamics format (.dcd) file: '''//
     &                           FCHARM(1:IEND1)//''''
C any skip?
          IF ( (CHSKIP(1).EQ.0) .AND. (CHSKIP(2).EQ.0) ) THEN
            WRITE( NOUT, '(A)')
     &' All positions from this file will be analyzed'//
     &       ' (as no CHARMS card read)'
          ELSE
            WRITE( NOUT, '(A,I5,A))')
     &' Will skip', CHSKIP(1), 
     &                     ' positions before starting to analyze',
     &' and      ', CHSKIP(2), 
     &                     ' positions after each read.'
          ENDIF
          WRITE( NOUT, '(1X,72(''*''))')
        ENDIF 

        WRITE( NOUT, '(A)')
     &' Input vdW radius file read as: '''//FRADIU(1:IEND2)//''''

        WRITE( NOUT, '(A,3F8.3)') ' point in channel: ', CPOINT
        WRITE( NOUT, '(A,3F8.3)') ' channel vector:   ', CVECT
        WRITE( NOUT, '(A,F8.3)')
     &	 ' sampling distance between planes: ', SAMPLE

        WRITE(NOUT,'(/A)') ' graphics conditions:'
        CALL CHREND( FHYDRA, IEND1)
        WRITE( NOUT, '(1X,A)')
     & 'binary plot output file name: '//FHYDRA(1:IEND1)
        WRITE( NOUT,'(A,I5)') ' Dot density: ', DOTDEN
        WRITE( NOUT,'(A,L1)') 
     &' Draw dots?           ', LDOT,
     &' Draw close contacts? ', LLINE,
     &' Draw centre line?    ', LCEN,
     &' Produce spikes rather than dots? ', LSPIKE
        WRITE(NOUT,'(A,I10,A)')
     &' Seed for random number generator:', FSEED,
     &           ' (0 means value set by program)' 
        WRITE( NOUT, '(A,F8.3)')
     &' Radius above which a result is regarded as an ''end'':', ENDRAD

        IF (FPDBSP.EQ.'NONE') THEN
          WRITE( NOUT, '(1X,A)') 
     & 'Will not produce sphere centre pdb format file'
        ELSE
          CALL CHREND( FPDBSP, IEND1)
          WRITE( NOUT, '(1X,A)')
     & 'Pdb format file for sphere centres: '//FPDBSP(1:IEND1)
        ENDIF	
C tell user Monte Carlo conditions
        WRITE(NOUT,'(/A)') ' Monte Carlo conditions:'
        WRITE(NOUT,'(A,I5)')   
     &'  no of steps: ', MCSTEP
        WRITE(NOUT,'(A,F8.3,A)') 
     &'  maximum displacement', MCLEN, ' angs'
        WRITE(NOUT,'(A,F8.3,A)') 
     &'  ''kT'' initial', MCKTIN, ' angs'

C capsule option - overrides spherebox
        IF (LCAPS) THEN
          WRITE( NOUT, '(A)')
     &' *** The CAPSULE option turned ON ***'
          LSPHBX = .FALSE.  
        ELSE
          IF (LSPHBX) THEN
            WRITE( NOUT, '(A)')
     &' *** The spherebox option turned ON ***'
          ELSE
            WRITE( NOUT, '(A)')
     &' The spherebox and capsule options are turned off.'
          ENDIF
        ENDIF

C qpt of original molcules
        IF (MOLQPT.EQ.'NONE') THEN
          WRITE( NOUT, '(1X,A)')
     & '(No molqpt card so will not write qpt file of the molecule)'
        ELSE
          CALL CHREND( MOLQPT, IEND1)
          WRITE( NOUT, '(1X,A)')
     & 'Will write out stick representation of molecule to '
     &			//MOLQPT(1:IEND1)
        ENDIF    
        IF (IGNRES(1:4).NE.'....') THEN
          WRITE( NOUT, '(A)')
     &' IGNORE card read so will ignore residue types: '//
     &     IGNRES(1:INDEX(IGNRES,'    ')-1)
        ELSE
          WRITE( NOUT, '(A)')
     &' No IGNORE card read so will read all residue types'
        ENDIF
C end of if(shorto < 3) bit
      ENDIF
C write shorto level regardless of shorto level!
      IF (SHORTO.NE.0) THEN
        WRITE( NOUT, '(A/ A,I5,A)')
     &' SHORTO card read so will produce shortened textual output',
     &'        level = ', SHORTO, ' (0 for full output to 3 no further)'
      ELSE
        WRITE( NOUT, '(A)')
     &' no SHORTO card read so will produce full textual output'
      ENDIF

      IF (PEGRAT(1).NE.0.) THEN
        WRITE( NOUT, '(A, 2(/A,F8.3))')
     &' Will produce PEG polymer exclusion graph as PEGRAT card'//
     &						' has been read. ',
     &'    Ratio of conductance with PEG to normal solution= ', 
     &							PEGRAT(1),
     &'    Ratio of conductance with PEG excluded to normal= ', 
     &							PEGRAT(2)
      ELSE
        WRITE( NOUT, '(A)')
     &' As no PEGRAT card was found will not produce PEG '//
     &				'polymer exclusion graph'
      ENDIF

C 2D property map filename root
        IF (F2DMAP.EQ.'NONE') THEN
          WRITE( NOUT, '(1X,A)')
     & '(No 2DMAPS card specified so will not produce 2D property maps)'
        ELSE
C find end of filename
          CALL CHREND( F2DMAP, IEND1)
C cannot do 2D property map if capsule is on
          IF (LCAPS) THEN
            WRITE( NOUT, '(A)')
     &'ERROR***',
     &' Have specified a 2D property map output ',
     &' But capsule option is on - NOT allowed'
            LERR = .TRUE.
            GOTO 55555
          ENDIF
          WRITE( NOUT, '(1X,A)')
     & 'Will write out 2D property maps to files with name root: '
     &                  //F2DMAP(1:IEND1)
        ENDIF


C cutsize report
        IF (CUTSIZE.GT.-1e6) THEN
           WRITE( NOUT, '(A/ A,F12.3)')
     &' Have found a CUT card.  So will speed the calculation by',
     &'   using cutoff lists during run with a cutsize=', CUTSIZE,
     &'   (A negative value indicates no cutoff lists to be used)'
        ELSE
C default value - 1 for now
           CUTSIZE = 1.
           WRITE( NOUT, '(A/ A,F12.3/ A)')
     &' Have not found a CUT card.  Normal calculation using',
     &'   Cutoff lists with a cutsize=', CUTSIZE,
     &'   (if you wish to turn off option use a CUT -1 card)'
        ENDIF
        

        



55555 RETURN
      END
