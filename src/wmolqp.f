      SUBROUTINE WMOLQP( NOUT, LERR, MOLQPT, ATMAX, ATNO, ATXYZ, ATBND,
     &			 SHORTO)
      IMPLICIT NONE
C ********************************************************************
C *                                                                  *
C * This software is an unpublished work containing confidential and *
C * proprietary information of Birkbeck College. Use, disclosure,    *
C * reproduction and transfer of this work without the express       *
C * written consent of Birkbeck College are prohibited. This notice  *
C * must be attached to all copies or extracts of the software.      *
C *                                                                  *
C * (c) 1993,1995 Oliver Smart & Birkbeck College,                   *
C * All rights reserved                                              *
C * (c) 1996 Oliver Smart & Birkbeck College, All rights reserved    *
C *                                                                  *
C ********************************************************************
C
C Modification history:
C
C Date  Author          Modification
C 11/95 O.S. Smart      Original version
C 01/00 O.S.S.		NEWOP introduced
C
C This s/r writes out a binary quanta format plot file of the input
C molecule to file molqpt which should be opened in this routine.
C All the molecule is put into colour number 5 which defaults to a quanta white.


C passed variables**************

C output stream number
C (return unchanged)
      INTEGER			NOUT

C error indicator
C (set true if there is a problem)
      LOGICAL			LERR

C Output filename
C (return unchanged)
      CHARACTER*200		MOLQPT

C maximum number of atoms 
C (return unchanged)
      INTEGER			ATMAX

C the actual number of atoms
C (return unchanged)
      INTEGER			ATNO

C atom's Cartesian coords
C (return unchanged)
      DOUBLE PRECISION		ATXYZ(3,ATMAX)

C atom's bonding radii - if two atoms are closer than
C the sum of their bonding radii they are regarded as bonded
C and a line is drawn
C (return unchanged)
      DOUBLE PRECISION		ATBND(ATMAX)

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

C internal variables ***********

C output stream number
      INTEGER			SMOLQP

C two loop counts for molecules
      INTEGER			ACOUN1, ACOUN2

C distance squared
      DOUBLE PRECISION		DIST2

C logical to test to see whether an atom has been written to
C at least once
      LOGICAL 			LONCE

C end of decs ******************

C open routine allows the overwriting of existing files
C (last arguement set true for binary files)
      CALL NEWOP( NOUT, LERR, SHORTO,
     &                  MOLQPT, SMOLQP, .TRUE.)
      IF (LERR) THEN
C error cannot open .qpt file
      WRITE( NOUT,  '(A)')
     &  ' ERROR',
     &  ' cannot open MOLQPT .qpt output file:  '//
     &   MOLQPT(1:INDEX( MOLQPT,'      ')-1)
        GOTO 55555
      ENDIF
          
C tell user:
      INQUIRE( SMOLQP, NAME = MOLQPT)
      WRITE( NOUT, '(A)')
     &'  Have written stick .qpt output file:  '//
     &  MOLQPT(1:INDEX( MOLQPT,'      ')-1)

C first record change to colour 5
      WRITE(SMOLQP) 1.0, 5.0, 5.0, 5.0

C go thru all atoms
      DO 10 ACOUN1 = 1, ATNO
C make sure that we write at least one record to each atom
        LONCE = .FALSE.

C go thru every atom following acoun1
        DO 20 ACOUN2 = ACOUN1+1, ATNO
C is the atom pair acoun1, acoun2 within range
C compare the the distance with of the sum of bond radii
C Do comparison with squares as it will be quicker
          DIST2 = (ATXYZ(1,ACOUN1)-ATXYZ(1,ACOUN2))**2 +
     &            (ATXYZ(2,ACOUN1)-ATXYZ(2,ACOUN2))**2 +
     &            (ATXYZ(3,ACOUN1)-ATXYZ(3,ACOUN2))**2 
          IF (DIST2.LT.(ATBND(ACOUN1)+ATBND(ACOUN2))**2) THEN
C move to atom 1
            WRITE(SMOLQP) 2.0, REAL(ATXYZ(1,ACOUN1)),
     &                    REAL(ATXYZ(2,ACOUN1)), REAL(ATXYZ(3,ACOUN1))
C draw to atom2
            WRITE(SMOLQP) 3.0, REAL(ATXYZ(1,ACOUN2)),
     &                    REAL(ATXYZ(2,ACOUN2)), REAL(ATXYZ(3,ACOUN2))
            LONCE = .TRUE.
          ENDIF
20      CONTINUE

C has the atom had one record written to it? 
        IF (.NOT.LONCE) THEN
C may have already been written
C go thru every atom preceeding acoun1
          DO 30 ACOUN2 = 1, ACOUN1-1
            DIST2 = (ATXYZ(1,ACOUN1)-ATXYZ(1,ACOUN2))**2 +
     &             (ATXYZ(2,ACOUN1)-ATXYZ(2,ACOUN2))**2 +
     &              (ATXYZ(3,ACOUN1)-ATXYZ(3,ACOUN2))**2
            IF (DIST2.LT.(ATBND(ACOUN1)+ATBND(ACOUN2))**2) LONCE=.TRUE.
30        CONTINUE
        ENDIF

C has the atom acoun1 been written to
        IF (.NOT.LONCE) THEN
C has not been written - output small 3d cross
          WRITE(SMOLQP) 2.0, REAL(ATXYZ(1,ACOUN1))-0.1,
     &                       REAL(ATXYZ(2,ACOUN1)), 
     &                       REAL(ATXYZ(3,ACOUN1))
          WRITE(SMOLQP) 3.0, REAL(ATXYZ(1,ACOUN1)),
     &                       REAL(ATXYZ(2,ACOUN1))+0.1, 
     &                       REAL(ATXYZ(3,ACOUN1))
          WRITE(SMOLQP) 2.0, REAL(ATXYZ(1,ACOUN1)),
     &                       REAL(ATXYZ(2,ACOUN1))-0.1, 
     &                       REAL(ATXYZ(3,ACOUN1))
          WRITE(SMOLQP) 3.0, REAL(ATXYZ(1,ACOUN1)),
     &                       REAL(ATXYZ(2,ACOUN1))+0.1, 
     &                       REAL(ATXYZ(3,ACOUN1))
          WRITE(SMOLQP) 2.0, REAL(ATXYZ(1,ACOUN1)),
     &                       REAL(ATXYZ(2,ACOUN1)), 
     &                       REAL(ATXYZ(3,ACOUN1))-0.1
          WRITE(SMOLQP) 3.0, REAL(ATXYZ(1,ACOUN1)),
     &                       REAL(ATXYZ(2,ACOUN1)), 
     &                       REAL(ATXYZ(3,ACOUN1))+0.1
        ENDIF

10    CONTINUE

C close output file
      CLOSE(SMOLQP)

55555 RETURN

      END
