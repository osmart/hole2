      SUBROUTINE VDSET( NOUT, FCOORD, FRADIU, FHYDRA, 
     &             DOTDEN, DCUT, LERR)
      IMPLICIT NONE
C ********************************************************************
C *                                                                  *
C * This software is an unpublished work containing confidential and *
C * proprietary information of Birkbeck College. Use, disclosure,    *
C * reproduction and transfer of this work without the express       *
C * written consent of Birkbeck College are prohibited. This notice  *
C * must be attached to all copies or extracts of the software.      *
C *                                                                  *
C * (c) 1994 Oliver Smart & Birkbeck College, All rights reserved    *
C * (c) 1996 Oliver Smart & Birkbeck College, All rights reserved    *
C *                                                                  *
C ********************************************************************
C
C Modification history:
C
C Date	Author		Modification
C 05/94	O.S. Smart	Original version
C
C This s/r is called by program vdwdot.
C Straight adaptation of s/r HOLSET - part of HOLE
C
C this s/r
C (a) checks that everything which needs to be specified has been
C (b) mirror values back to user
C (but does not open files)

C output stream
C returned unchanged!
      INTEGER			NOUT

C input coordinate filename
      CHARACTER*200		FCOORD
C input radius filename
      CHARACTER*200		FRADIU
C hydra binary plot output file
      CHARACTER*200		FHYDRA

C a dot density for dot surface 
      INTEGER			DOTDEN

C cut off distance for calculation
      DOUBLE PRECISION		DCUT

C if error found set LERR true
      LOGICAL			LERR

C internal vbles

C last character in filename
      INTEGER			IEND1, IEND2

C end of decs ******************


C leave a couple of lines blank
      WRITE( NOUT, *)
      WRITE( NOUT, *)

C check that required values have been specified
      IF (FCOORD(1:4).EQ.'NONE') THEN
	LERR = .TRUE.
	WRITE(NOUT,*) 'ERROR - have not specified COORD record'
      ENDIF
      IF (FRADIU(1:4).EQ.'NONE') THEN
	LERR = .TRUE.
	WRITE(NOUT,*) 'ERROR - have not specified RADIUS record'
      ENDIF
      IF (FHYDRA(1:4).EQ.'NONE') THEN
        LERR = .TRUE.
        WRITE(NOUT,*) 'ERROR - have not specified PLTOUT record'
      ENDIF

      IF (LERR) THEN
	WRITE(NOUT,*) 'see hole_doc.txt for details!'
	GOTO 55555
      ENDIF

C tell user what file names will be used
      CALL CHREND( FCOORD, IEND1)
      CALL CHREND( FRADIU, IEND2)
      WRITE( NOUT, '(20(1X,A:/))')
     & 'Input pdb filename read as:    '//FCOORD(1:IEND1),
     & 'Input vdW radius file read as: '//FRADIU(1:IEND2)

      CALL CHREND( FHYDRA, IEND1)
      WRITE( NOUT, '(1X,A)')
     & 'Binary plot output (qpt) filename: '//FHYDRA(1:IEND1)
      WRITE( NOUT,'(A,I5)') 
     &' Dot density: ', DOTDEN
      WRITE( NOUT,'(A,F8.3)') 
     &' Distance cutoff to be used for calculation', DCUT

55555 RETURN
      END
