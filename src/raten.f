      SUBROUTINE RATEN( CENTRE, RATE, IAT1, IAT2, IAT3, DAT2, DAT3,
     &                  ATMAX, ATNO, ATXYZ, ATVDW, CUTSIZE,
     &			PENDIS, STRMAX, STRNOP, STRNON, STRCEN, STRRAD,
     &			LHORR, SSHELL)
      IMPLICIT NONE
      SAVE
C ********************************************************************
C *                                                                  *
C * This software is an unpublished work containing confidential and *
C * proprietary information of Birkbeck College. Use, disclosure,    *
C * reproduction and transfer of this work without the express       *
C * written consent of Birkbeck College are prohibited. This notice  *
C * must be attached to all copies or extracts of the software.      *
C *                                                                  *
C * (c) 1998 Oliver Smart                    *
C *                                                                  *
C ********************************************************************
C
C Modification history:
C
C Date	Author		Modification
C 05/98 O.S.S.		First version
C 07/98 O.S.S. 		Add penalty function (l012, vble PENDIS
C
C adapted from HOLEEN - instead of returning the largest radius
C return RATE the third most distant minus the largest - always +VE, zero when
C have got to locally best squeeze through.
C uses HOLEEN and some comments maybe the same.

C passed vbles

C the point
      DOUBLE PRECISION		CENTRE(3)

C RATE is the third most distant atomic vdw surface
C minus the closest - always +VE, zero when
C have got to locally best squeeze through.
      DOUBLE PRECISION		RATE

C atom list no. with smallest dist-vdw radius, 2nd smallest
C 07/06/94 as iat's may or may not be supplied with previously 
C found numbers then we can use this to speed up the procedure
C Dec 97 add iat3 
      INTEGER			IAT1, IAT2, IAT3

C 2nd/3rd smallest distance-vdw radius (of iat2/3)
      DOUBLE PRECISION		DAT2, DAT3

C maximum no. of atoms
C returned unchanged
      INTEGER			ATMAX

C number of atoms read in from each file:
C returned unchanged
      INTEGER			ATNO

C co-ordinates
C returned unchanged
      DOUBLE PRECISION		ATXYZ( 3, ATMAX)

C vdw radius of each atom
C returned unchanged
      DOUBLE PRECISION		ATVDW(ATMAX)

C adaptation for distance list to speed calculation from Dec 97
C
C turn on cutoff procedure by setting cutsize positive 
C if not do simple all atom comparison
C
C control of size of the store vble -
C at any stage make cutdist = last rad + cutsize
C This is a passed vble - controlled by a cutsize card read
C DO NOT CHANGE as may be fixed arguement - introduce passed vble
      DOUBLE PRECISION		CUTSIZE

C penalty distance if this point is less than this distance to an already
C stored point then add on a penalty
      DOUBLE PRECISION		PENDIS
      
C store for sphere centres and radii
C store the initial point as no 0 and subseqently points in the +ve
C bit. Points below the initial point are stored in -ve bit.
C maximum number of entries
      INTEGER                   STRMAX
C the no. of entries, +Ve entries -Ve entries
      INTEGER                   STRNOP, STRNON
C the centres and radii
      DOUBLE PRECISION          STRCEN(3,-STRMAX:STRMAX),
     &                          STRRAD(-STRMAX:STRMAX)

C choice of objective function
C lmord: true - then use normal hole max radius
C        false - then use rat function third most distant
      LOGICAL			LHORR
            
      
C cut down on the number of records needed to go through to check overlap
C by storring the start number of this shell -
C n.b. assumes that shells are further apart than pendist
      INTEGER			SSHELL
      
C internal vbles

C The energy for holeen call - minus the largest radius of any sphere
      DOUBLE PRECISION		ENERGY

C loop count for going thru stored spheres, starting limit
      INTEGER			SCOUNT, START

C penalty distance squared, working vbles, distance and its square
      DOUBLE PRECISION		PEND2, WORK1, WORK2, WORK3, 
     &				DIST, DIST2

C end of decs ******************

C call to HOLEEN
C this s/r finds the largest radius for a sphere centered at
C point CENTRE without vdW overlap with the atoms supplied.
C energy is returned as minus this number.
C The procedure is accomplished by looking at the
C distance to centre-vdw radius for each atom 
C and finding the smallest value.
C n.b. the energy may become positive if centre is within
C an atoms vdw radius.
      CALL HOLEEN( CENTRE, ENERGY, IAT1, IAT2, IAT3, DAT2, DAT3,
     &                   ATMAX, ATNO, ATXYZ, ATVDW, CUTSIZE)
C store maximum radius of a sphere in DAT2 
      DAT2 = -ENERGY

C HOLE or RAT objective?
      IF (LHORR) THEN      
C 20/5/98 try changing just too hole energy
        RATE = ENERGY
      ELSE
C rat function value
        RATE = DAT3 - DAT2
      ENDIF
      
C add any penalty onto RATE
C work out penalty distance squared
      PEND2 = PENDIS*PENDIS
      START = SSHELL
      IF (START.LT.0) START = 0
      IF (START.GT.STRNOP) START = STRNOP
C go thru all centres already stored
      DO 10 SCOUNT = START, STRNOP
C find distance squared between centres
        WORK1 = CENTRE(1)-STRCEN(1,SCOUNT)
        WORK2 = CENTRE(2)-STRCEN(2,SCOUNT)
        WORK3 = CENTRE(3)-STRCEN(3,SCOUNT)
        DIST2 = WORK1*WORK1 + WORK2*WORK2 + WORK3*WORK3
	IF (DIST2.LE.PEND2) THEN
C HAVE A PENALTY - upto 10angs added on for zero dist 
          DIST = SQRT(DIST2) 
	  RATE = RATE + (10./PENDIS)*(PENDIS-DIST)
        ENDIF
10    CONTINUE
            
55555 RETURN
      END
