      SUBROUTINE HORCHI( NOUT, LERR, SHORTO,
     &  FCHARM, SCHARM, ISKIP, IPOS,
     &  ATMAX, ATNO, ATXYZ, REXYZ, NFREAT, IFREAT, OATNO)
      IMPLICIT NONE
C ********************************************************************
C *                                                                  *
C * This software is an unpublished work containing confidential and *
C * proprietary information of Birkbeck College. Use, disclosure,    *
C * reproduction and transfer of this work without the express       *
C * written consent of Birkbeck College are prohibited. This notice  *
C * must be attached to all copies or extracts of the software.      *
C *                                                                  *
C * (c) 1995 Oliver Smart & Birkbeck College, All rights reserved    *
C * (c) 1996 Oliver Smart & Birkbeck College, All rights reserved    *
C *                                                                  *
C ********************************************************************
C
C Modification history:
C
C Date  Author          Modification
C 11/95 O.S. Smart      First version
C
C This s/r opens a binary CHARMm dynamics file (.dcd),
C it then reads in the coordinates of the molecule
C skipping ISKIP positions before the read.
C The file is left open to stream SCHARM at the end
C of the process

C output stream number , error indicator , and level of
C information feedback wanted (if shorto=3 dont write anything)
      INTEGER			NOUT
      LOGICAL			LERR
      INTEGER			SHORTO

C charmm format filename (should have been already been stripped
C of any leading blanks
      CHARACTER*200		FCHARM

C stream number for the Charmm format file
      INTEGER			SCHARM

C the number of positions to skip before read
C (zero if first position is wanted)
      INTEGER			ISKIP

C position count return with position number of the 
C coordinates given. (Should be ISKIP+1)
      INTEGER			IPOS

C the array size limit for the number of atoms,
C the actual number of atoms,
C their coordinates.
      INTEGER			ATMAX
      INTEGER			ATNO
      DOUBLE PRECISION		ATXYZ( 3, ATMAX)
C need real vbles for charmm read
      REAL			REXYZ( 3, ATMAX)

C number of free atoms & their ORIGINAL list numbers
      INTEGER                   NFREAT, IFREAT(ATMAX)

C need to record on the initial read of the pdb file the
C original atom numbers of the each atom from the pdb file
C - as we ignore some residues in the read.
C oatno(0) is the total number of original atoms in the pdb file
C oatno(1) is the original atom number of the stored atom#1 etc.
      INTEGER                   OATNO(0:ATMAX)

C internal vbles ***************

C fn to open file read only binary
      LOGICAL			OPENRB

C VBLES for charmm format binary file
C not used here:
      REAL                      HDR

C icntrl(1) no of datasets NOT SET TO CORRECT VALUE HERE
C icntrl(2) time of first step (0 here)
C icntrl(3) time step between datasets
C icntrl(9) number of fixed atoms
      INTEGER                   ICNTRL(20)

C title:
      INTEGER                   ITITL
      CHARACTER*8               TITLE(10,10)

C loop counts for atoms, title*2, skip
      INTEGER			ACOUNT, TC1, TC2, SCOUNT

C dummy number of atoms, coordinate stores
      INTEGER			DUMNAT

C end of decs ******************

C start with position 1
      IPOS = 1

C try to open file
      IF (.NOT.OPENRB( SCHARM, FCHARM, NOUT)) THEN
        LERR = .TRUE.
        WRITE( NOUT, '(A)')
     &' ERROR found in s/r HORCHI',
     &'   Cannot open input charmm binary format file ',
     &'   Filename '''//FCHARM(1:INDEX(FCHARM,'     ')-1)//'''',
     &'   (tried readonly)'
        GOTO 55555
      ENDIF

C tell user what we have done
      IF (SHORTO.LT.3) WRITE( NOUT, '(A)')
     &' Have opened charmm binary format (dcd) file '''//
     &                 FCHARM(1:INDEX(FCHARM,'     ')-1)//''''

C For successful read the number of atoms in original
C pdb/charmm dcd file must be within array limits
C (for pdb file input alone this need not be the case -
C   only have to store non-ignored atoms)
      IF (OATNO(0).GT.ATMAX) THEN
        LERR = .TRUE.
        WRITE( NOUT, '(A/ 2(A/ A,I5/), A/ A)')
     &' ERROR in s/r horchi',
     &'   The number of atoms in the original template pdb file',
     &'   (as specified by COORD card above) is ', OATNO(0),
     &'   This exceeds the current array bounds limit ATMAX',
     &'   (specified in hole.f) which currently equals ', ATMAX,
     &'   (Could work on pdb file as sufficient atoms ignored',
     &'   on read to fit in). Either increase ATMAX or contact O.S.S.'
        GOTO 55555 
      ENDIF


C next section of code adapted from s/r ssrchr
C (part of series by o.s..s)
C for format of charmm binary files see hyd_doc:dataform.doc
C read hdr (not used) and icntrl(1 to 20)
C only icntrl(9) is used the no. of fixed atoms
      READ( SCHARM, ERR= 400) HDR, ICNTRL

C title 
      READ( SCHARM, ERR= 400) ITITL, 
     & ( (TITLE(TC1,TC2), TC1= 1, 10), TC2= 1,ITITL)

C write title:
      IF (SHORTO.LT.3) WRITE( NOUT, '(/ A/ 1X,10(A8)/ )') 
     &' File has title: ',
     & ( (TITLE(TC1,TC2), TC1= 1, 10), TC2= 1,ITITL)

C read no. of atoms (into dummy vble.)
      READ( SCHARM, ERR= 400) DUMNAT

C compare this with no. of atoms read from pdb file
C MUST AGREE n.b. on reading pdb file may have ignored some
C atoms (waters etc.) of should compare the original total
      IF (DUMNAT.NE.OATNO(0)) THEN
C no of atoms in pdb file and charmm files do not agree
        WRITE( NOUT, '( A/ A,I6/ A,I6)') 
     &' ERROR',
     &' Cannot proceed original number of atoms in pdb file:', OATNO(0),
     &' does not agree with no. of atoms in charm file:', DUMNAT
C close charmm file and return
	CLOSE( SCHARM)
        LERR = .TRUE.
        GOTO 55555
      ENDIF

C no. of free ats. = total - no. fixed
      NFREAT =  OATNO(0) - ICNTRL(9)

C read list no.'s of free atoms - if there are no fixed
      IF (ICNTRL(9).NE.0) THEN
        READ( SCHARM, ERR=400) (IFREAT(ACOUNT), ACOUNT= 1, NFREAT)
      ELSE
C if all atoms are free then free atoms list no.'s 
C (n.b. deal with original atom numbers not final)
        DO 121 ACOUNT = 1, OATNO(0)
          IFREAT(ACOUNT) = ACOUNT
121	CONTINUE
      ENDIF

C read coords - n.b. they are written as reals - so read into rexyz
C (n.b. deal with original atom numbers not final)
      READ( SCHARM, ERR= 400) (REXYZ(1,ACOUNT), ACOUNT= 1, OATNO(0))
      READ( SCHARM, ERR= 400) (REXYZ(2,ACOUNT), ACOUNT= 1, OATNO(0))
      READ( SCHARM, ERR= 400) (REXYZ(3,ACOUNT), ACOUNT= 1, OATNO(0))
     
C compare coords of first atom stored - n.b. may not be first
C read as it could have been excluded
      IF ( (ABS(REXYZ(1,OATNO(1))-ATXYZ(1,1)).GT.0.005) .OR.
     &     (ABS(REXYZ(2,OATNO(1))-ATXYZ(2,1)).GT.0.005) .OR.
     &     (ABS(REXYZ(3,OATNO(1))-ATXYZ(3,1)).GT.0.005)      ) 
     &  WRITE( NOUT, '(A)')
     &' *** Warning ***',
     &' Co-ordinates of pdb file and initial position of charmm file',
     &' do not agree. Will use charmm coords for initial position.'

C copy the coordinates across to atxyz array - using
C OATNO array to check where original atoms go.
      DO 10 ACOUNT = 1, ATNO
        ATXYZ(1,ACOUNT) = REXYZ(1,OATNO(ACOUNT))
        ATXYZ(2,ACOUNT) = REXYZ(2,OATNO(ACOUNT))
        ATXYZ(3,ACOUNT) = REXYZ(3,OATNO(ACOUNT))
10    CONTINUE


C does the user want to use this position of skip a number of
C positions before initial analysis?
      IF (ISKIP.NE.0) THEN
        DO 30 SCOUNT = 1, ISKIP
C read coords for free atoms into rexyz - fixed will remain 
C unaltered
          READ( SCHARM, END=500, ERR= 400) (REXYZ(1,IFREAT(ACOUNT)), 
     & 				   ACOUNT= 1, NFREAT)
          READ( SCHARM, ERR= 400) (REXYZ(2,IFREAT(ACOUNT)), 
     & 				   ACOUNT= 1, NFREAT)
          READ( SCHARM, ERR= 400) (REXYZ(3,IFREAT(ACOUNT)), 
     & 	  			   ACOUNT= 1, NFREAT)
C end of skip read
30      CONTINUE
C copy the coordinates across to atxyz array - using
C OATNO array to check where original atoms go.
        DO 50 ACOUNT = 1, ATNO
          ATXYZ(1,ACOUNT) = REXYZ(1,OATNO(ACOUNT))
          ATXYZ(2,ACOUNT) = REXYZ(2,OATNO(ACOUNT))
          ATXYZ(3,ACOUNT) = REXYZ(3,OATNO(ACOUNT))
50      CONTINUE
C tell user what has been done?
        IF (SHORTO.LT.3) WRITE( NOUT, '(A,I5,A)')
     &' Have skipped ', ISKIP, ' positions after initial read'

C increment ipos
        IPOS = IPOS + ISKIP
C end of initial skip
      ENDIF

C return here *******
55555 RETURN

C ERRORS

C error reading from binary file
400   CONTINUE
      LERR = .TRUE.
      WRITE( NOUT, '(A)')
     &' *** error ***',
     &' On binary read from file: '//FCHARM(1:INDEX(FCHARM,'     ')),
     &' Error occured on reading the first position',
     &' Possible reasons:',
     &' (a) file not in correct charmm binary co-ord format',
     &' (b) file incomplete or corrupted',
     &' (c) file written on a machine which stores reals in',
     &'     a different order.'
      GOTO 55555

C error reading from binary file - not enough positions for initial skip
500   CONTINUE
      LERR = .TRUE.
      WRITE( NOUT, '(A/ A/ A,I8,A/ A)')
     &' *** error ***',
     &' On binary read from file: '//FCHARM(1:INDEX(FCHARM,'     ')),
     &' You have asked to skip', ISKIP, ' positions before analysis',
     &' But there are not that many positions in the file!'
      GOTO 55555

      END
C
      LOGICAL FUNCTION HORCHR( NOUT, LERR, SHORTO,
     &              FCHARM, SCHARM, ISKIP, IPOS,
     &              ATMAX, ATNO, ATXYZ, REXYZ, NFREAT, IFREAT, OATNO)
      IMPLICIT NONE
C ********************************************************************
C *                                                                  *
C * This software is an unpublished work containing confidential and *
C * proprietary information of Birkbeck College. Use, disclosure,    *
C * reproduction and transfer of this work without the express       *
C * written consent of Birkbeck College are prohibited. This notice  *
C * must be attached to all copies or extracts of the software.      *
C *                                                                  *
C * (c) 1995 Oliver Smart & Birkbeck College, All rights reserved    *
C *                                                                  *
C ********************************************************************
C
C Modification history:
C
C Date  Author          Modification
C 11/95 O.S. Smart      First version
C
C This function reads in a new position from charmm binary
C dynamics file open on unit scharm. Returns true if position
C successfully read false if error found or on end of file
C
C output stream number , error indicator , and level of
C information feedback wanted (if shorto=3 dont write anything)
      INTEGER			NOUT
      LOGICAL			LERR
      INTEGER			SHORTO

C charmm format filename (should have been already been stripped
C of any leading blanks
      CHARACTER*200		FCHARM

C stream number for the Charmm format file
      INTEGER			SCHARM

C the number of positions to skip before read
C (zero if first position is wanted)
      INTEGER			ISKIP

C position count return with position number of the 
C coordinates given. (Should be ISKIP+1)
      INTEGER			IPOS

C the array size limit for the number of atoms,
C the actual number of atoms,
C their coordinates.
      INTEGER			ATMAX
      INTEGER			ATNO
      DOUBLE PRECISION		ATXYZ( 3, ATMAX)
C need real vbles for charmm read
      REAL			REXYZ( 3, ATMAX)

C number of free atoms & their ORIGINAL list numbers
      INTEGER                   NFREAT, IFREAT(ATMAX)

C need to record on the initial read of the pdb file the
C original atom numbers of the each atom from the pdb file
C - as we ignore some residues in the read.
C oatno(0) is the total number of original atoms in the pdb file
C oatno(1) is the original atom number of the stored atom#1 etc.
      INTEGER                   OATNO(0:ATMAX)

C internal vbles ***************

C loop counts
      INTEGER			ACOUNT, SCOUNT

C end of decs ******************

C set function vble to pickup eof
      HORCHR = .FALSE.

C skip iskip positions before read 
      DO 30 SCOUNT = 1, ISKIP+1
C read coords for free atoms into rexyz - fixed will remain
C unaltered
        READ( SCHARM, END= 55555, ERR= 400) (REXYZ(1,IFREAT(ACOUNT)),
     &                             ACOUNT= 1, NFREAT)
        READ( SCHARM, ERR= 400) (REXYZ(2,IFREAT(ACOUNT)),
     &                             ACOUNT= 1, NFREAT)
        READ( SCHARM, ERR= 400) (REXYZ(3,IFREAT(ACOUNT)),
     &                             ACOUNT= 1, NFREAT)
C end of skip read
30    CONTINUE
C copy the coordinates across to atxyz array - using
C OATNO array to check where original atoms go.
      DO 50 ACOUNT = 1, ATNO
        ATXYZ(1,ACOUNT) = REXYZ(1,OATNO(ACOUNT))
        ATXYZ(2,ACOUNT) = REXYZ(2,OATNO(ACOUNT))
        ATXYZ(3,ACOUNT) = REXYZ(3,OATNO(ACOUNT))
50    CONTINUE

C ipos is the position number of the coords loaded
      IPOS = IPOS + ISKIP + 1
C record that we have successfully read a position
      HORCHR = .TRUE.

C return
55555 RETURN

C error reading from binary file
400   CONTINUE
      LERR = .TRUE.
      WRITE( NOUT, '(A)')
     &' *** error ***',
     &' On binary read from file: '//FCHARM(1:INDEX(FCHARM,'     ')),
     &' Error occured on reading the first position',
     &' Possible reasons:',
     &' (a) file not in correct charmm binary co-ord format',
     &' (b) file incomplete or corrupted',
     &' (c) file written on a machine which stores reals in',
     &'     a different order.'
      GOTO 55555

      END
