      SUBROUTINE HOSETG( NOUT, LERR, SHORTO,
     &             SHYDRA, FHYDRA, LLINE,
     &             SPDBSP, FPDBSP, LSPHBX,
     &             MOLQPT, ATMAX, ATNO, ATXYZ, ATBND)
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
C 01/00 O.S.		S/r NEWOP introduced
C
C This s/r deals with opening the graphics files at the start of
C a HOLE run.

C output stream no (to user)
C (return unchanged)
      INTEGER                   NOUT


C if an error found is found in s/r - set lerr true and program will stop, 
      LOGICAL                   LERR

C  variable to reduce text output -
C if oshort =
C 0: full text output
C 1: no in run progress reported
C 2: ditto plus no graph type output - only leaving
C    Minimum radius & conductance calculations.
C 3: All text output other than input card mirroring
C    and error messages turned off
C (return unchanged)
      INTEGER                   SHORTO


C quanta 3D plot format (.qpt) file stream, name
      INTEGER			SHYDRA
      CHARACTER*200		FHYDRA
C logical flag to indicate whether we are drawing lines from
C the HOLE sphere surfaces to closest atom
C (return unchanged)
      LOGICAL			LLINE

C sphere centre pdb format file (for use with sphqpb)
C stream number, filename
      INTEGER			SPDBSP
      CHARACTER*200		FPDBSP

C sphere-box option on?
C (return unchanged)
      LOGICAL			LSPHBX

C Output filename for .qpt format stick plot of the molecule
C (return unchanged)
      CHARACTER*200             MOLQPT

C maximum number of atoms 
C (return unchanged)
      INTEGER                   ATMAX

C the actual number of atoms
C (return unchanged)
      INTEGER                   ATNO

C atom's Cartesian coords
C (return unchanged)
      DOUBLE PRECISION          ATXYZ(3,ATMAX)

C atom's bonding radii - if two atoms are closer than
C the sum of their bonding radii they are regarded as bonded
C and a line is drawn
C (return unchanged)
      DOUBLE PRECISION          ATBND(ATMAX)

C internal vbles ***************

C end of decs ******************

C open the plot file - if we need to
      IF (FHYDRA(1:4).NE.'NONE') THEN
C open routine allows the overwriting of existing files
C (last arguement set true for binary files)
        CALL NEWOP( NOUT, LERR, SHORTO,
     &                  FHYDRA, SHYDRA, .TRUE.)
        IF (LERR) THEN
C error cannot open hydra binary plot output file
        WRITE( NOUT,  '(A)')
     &' ERROR',
     &' Cannot open hydra binary plot output file:  '//
     &   FHYDRA(1:INDEX( FHYDRA,'      ')-1),
     &' Aborting'
         GOTO 55555
        ENDIF    

C tell user:
        INQUIRE( SHYDRA, NAME = FHYDRA)
        IF (SHORTO.LT.3) WRITE( NOUT,  '(/1X,A)')
     &    'Have opened hydra binary plot output file:  '//
     &    FHYDRA(1:INDEX( FHYDRA,'      ')-1)

C colour 1 is for lines joining sphere edge to closest atoms
C Change 26/10/93 do in colour 10 with alternative qplot 20
        IF (LLINE) WRITE(SHYDRA) 1.0, 10.0, -55.0, 20.0

      ELSE
        WRITE(NOUT,*) 'Will not produce graphical output'
      ENDIF

C open the pdb format file for sphere centres - if we need to
      IF (FPDBSP(1:4).NE.'NONE') THEN
C open routine allows the overwriting of existing files
C (last arguement set true for binary files)
        CALL NEWOP( NOUT, LERR, SHORTO,
     &                  FPDBSP, SPDBSP, .FALSE.)
        IF (LERR) THEN
C error cannot open pdb sphere centre output file 
          WRITE( NOUT,  '(A)')
     &  ' ERROR',
     &  ' cannot pdb sphere centre file:  '//
     &   FPDBSP(1:INDEX( FPDBSP,'      ')-1)
          GOTO 55555
        ENDIF    
C tell user output name
        INQUIRE( SPDBSP, NAME = FPDBSP)
        IF (SHORTO.LT.3) WRITE( NOUT,  '(/1X,A)')
     &'Have opened file for pdb format output of sphere centres:  '//
     &    FPDBSP(1:INDEX( FPDBSP,'      ')-1)
C if doing sphere-box record the fact
        IF (LSPHBX) WRITE( SPDBSP, '(A)') 'SPHERE-BOX-ON'

      ELSE
        IF (SHORTO.LT.3) WRITE( NOUT, '(A)')
     &' Will not produce file with sphere centre',
     &'      information in pdb format'
      ENDIF

C 30 October 1995 molqpt can be specified to write
C a .qpt file of original molecule
      IF (MOLQPT(1:4).NE.'NONE') THEN
        CALL WMOLQP( NOUT, LERR, MOLQPT, ATMAX, ATNO, ATXYZ, ATBND,
     &		     SHORTO)
        IF (LERR) GOTO 55555
      ENDIF

55555 RETURN



      END
