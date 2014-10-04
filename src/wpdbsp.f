      SUBROUTINE WPDBSP( SPDBSP, LCAPS, LSPHBX, IREC,
     &                   STRMAX, STRCEN, STRRAD, 
     &		         STRLVC, STRLBO, STRSBO, STRBRD, CVECT, ENDRAD)
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
C *                                                                  *
C ********************************************************************
C
C Modification history:
C
C Date	Author		Modification
C 10/95	O.S. Smart	Original version.
C 
C This s/r splits the writing of the sphere-centre record file
C which takes a PDB format from the top hole program.

C stream number of output file
      INTEGER			SPDBSP

C record number to be written to file
      INTEGER			IREC

C store for sphere centres and radii
C store the initial point as no 0 and subseqently points in the +ve
C bit. Points below the initial point are stored in -ve bit.
C maximum number of entries
      INTEGER			STRMAX

C the centres and radii
      DOUBLE PRECISION		STRCEN(3,-STRMAX:STRMAX),
     &				STRRAD(-STRMAX:STRMAX)

C 08/07/94
C vbles to store the dimensions found in the spherebox
C options. 
      DOUBLE PRECISION          STRLVC(3,-STRMAX:STRMAX),
     &				STRLBO(-STRMAX:STRMAX),
     &				STRSBO(-STRMAX:STRMAX),
     &				STRBRD(-STRMAX:STRMAX)

C channel vector
      DOUBLE PRECISION		CVECT(3)

C Capule on flag
C STRCEN is used to store the effect radius of the capsule
C        which is the area normal to lvec divided by pi then rooted
C STRBRD is used to store the real capsule radius
      LOGICAL			LCAPS

C logical flag to turn on spherebox option
      LOGICAL			LSPHBX

C New option 22/11/93 - For wide channels need to
C be able to specify the radius at which an end is reached
      DOUBLE PRECISION          ENDRAD

C loop count for xyz
      INTEGER			XCOUNT

C end of decs ******************


C 30/5/95 output capsule info as two atom records - change 
C atom name to QC1 and QC2
      IF (LCAPS) THEN
        WRITE( SPDBSP, '(A,I4,4X,3F8.3,2F6.2)')
     &   'ATOM      1  QC1 SPH S', IREC,
     &       (STRCEN(XCOUNT,IREC), XCOUNT= 1, 3),
     &       STRBRD(IREC), 0.0
C if doing capsule option simply write out second centre
        WRITE( SPDBSP, '(A,I4,4X,3F8.3,2F6.2)')
     &   'ATOM      1  QC2 SPH S', IREC,
     &       (STRLVC(XCOUNT,IREC), XCOUNT= 1, 3),
     &       STRBRD(IREC), 0.0
      ELSE 
C Not doing capsule option
C n.b. lose previously output info on distance along channel
C (last record)
        WRITE( SPDBSP, '(A,I4,4X,3F8.3,2F6.2)')
     &   'ATOM      1  QSS SPH S', IREC,
     &       (STRCEN(XCOUNT,IREC), XCOUNT= 1, 3),
     &       STRRAD(IREC), STRRAD(IREC)   
C if doing spherebox write out these details flagged
C first the three dimensions then the radius and finally
C the two axes
        IF (LSPHBX) WRITE( SPDBSP, '(A,10F9.4)')
     &   'SPHBOX ',  
     &        STRLBO(IREC), STRSBO(IREC), STRSBO(IREC), 
     &        STRBRD(IREC),
     &        (STRLVC(XCOUNT,IREC), XCOUNT= 1, 3), 
     &        (CVECT(XCOUNT), XCOUNT= 1, 3)
      ENDIF
      
! write the end radius records here
      IF (STRRAD(IREC).GT.ENDRAD) 
     &           WRITE( SPDBSP, '(A)') 'LAST-REC-END'

C update file to disk
      CALL DISKFF( SPDBSP)

55555 RETURN
      END
