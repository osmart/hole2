      SUBROUTINE HOCAPR( 
     &         CCENT1, CCENT2, CAPR, DIREC, EFFRAD)
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
C * (c) 1995 Oliver Smart & Birkbeck College, All rights reserved    *
C * (c) 1996 Oliver Smart & Birkbeck College, All rights reserved    *
C *                                                                  *
C ********************************************************************
C
C Modification history:
C
C Date  Author          Modification
C 10/95 O.S. Smart      Original version
C
C This s/r works out the effective radius on a direction
C of DIREC from the mid-point of a capsule defined by
C centres CCENT1 and CCENT2 and a capsule radius of CAPR.
C All vbles other than EFFRAD must be returned unchanged
C n.b. DIREC is assumed to be supplied as a unit vector
      DOUBLE PRECISION		CCENT1(3), CCENT2(3), CAPR, 
     &				DIREC(3), EFFRAD

C internal vbles ***************

C unit capsule vector
      DOUBLE PRECISION		CAPVEC(3), CAPLEN
C critical angle, the real angle subtended to capvec
      DOUBLE PRECISION		ALPHAC, ALPHA

C pi
      DOUBLE PRECISION		PI

C end of decs ******************

C value for pi?
      IF (INT(PI).NE.3) PI = 2.*ACOS(0.)

C vector along capsule centre line
      CAPVEC(1) = CCENT1(1) - CCENT2(1)
      CAPVEC(2) = CCENT1(2) - CCENT2(2)
      CAPVEC(3) = CCENT1(3) - CCENT2(3)
C length of capsule half-vector
      CAPLEN = SQRT( CAPVEC(1)**2 + CAPVEC(2)**2 +
     &                   CAPVEC(3)**2)
C make capvec a unit
      CAPVEC(1) = CAPVEC(1)/CAPLEN
      CAPVEC(2) = CAPVEC(2)/CAPLEN
      CAPVEC(3) = CAPVEC(3)/CAPLEN
C want half vector length in rest though
      CAPLEN = 0.5*CAPLEN

C do we a sphere (special case of capsule?)
      IF (CAPLEN.LT.1E-09) THEN
        EFFRAD = CAPR
        GOTO 55555
      ENDIF

C the angle subtended by the capsule radius at the end of
C capsule
      ALPHAC = ATAN2(CAPR,CAPLEN)


C work out the angle between direct and the capsule vector
      ALPHA = ACOS( CAPVEC(1)*DIREC(1) +
     &              CAPVEC(2)*DIREC(2) +
     &              CAPVEC(3)*DIREC(3) )

C is the direction inside the "cylinder" bit of the capsule?
      IF ( (ALPHA.GT.ALPHAC) .AND. (ALPHA.LT.(PI-ALPHAC)) ) THEN
C yes - effective radius easy
        EFFRAD = CAPR/SIN(ALPHA)
      ELSE
C for angles greater than 90 then use the equivalent mirror angle
        IF (ALPHA.GT.0.5*PI) ALPHA = PI - ALPHA
C not in cylinder in one of the end caps
C got triangle with dimensions
C length of sides EFFR, CAPLEN and CAPR. The angle opposite CAPR in alpha.
C Applying cosine rule
C CAPR**2 = EFFRAD**2 - 2*CAPLEN*EFFRAD + CAPLEN**2
C only +ve solution must be real
        EFFRAD = CAPLEN*COS(ALPHA) + 
     &           SQRT( (CAPLEN*COS(ALPHA))**2-(CAPLEN**2-CAPR**2))
      ENDIF

55555 RETURN
      END
