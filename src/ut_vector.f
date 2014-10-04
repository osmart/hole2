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
C 12/93	O.S. Smart	Original public release in HOLE suite beta1.0
C 05/94 O.S. Smart      s/r MATI22 added
C
C
C contains small s/r's concerned with vectors
      SUBROUTINE CROSS(F,G,H)
      IMPLICIT NONE
      REAL 
     1        F(3),
     1        G(3),
     1        H(3)
      H(1) = F(2)*G(3) - F(3)*G(2)
      H(2) = F(3)*G(1) - F(1)*G(3)
      H(3) = F(1)*G(2) - F(2)*G(1)
      RETURN
      END
C
      REAL FUNCTION CROSSF(F,G,I1)
      IMPLICIT NONE
      REAL 
     1        F(3),
     1        G(3)
      INTEGER
     1	      I1,
     1	      I2,
     1	      I3
      I2 = (I1 + 1) - 3*INT(I1/3)
      I3 = (I2 + 1) - 3*INT(I2/3)
      CROSSF = F(I2)*G(I3) - F(I3)*G(I2)
      RETURN
      END
C
      DOUBLE PRECISION FUNCTION dCROS2(F,G,I1)
      IMPLICIT NONE
      DOUBLE PRECISION 
     1        F(3),
     1        G(3)
      INTEGER
     1        I1,
     1        I2,
     1        I3
      I2 = (I1 + 1) - 3*INT(I1/3)
      I3 = (I2 + 1) - 3*INT(I2/3)
      dCROS2 = F(I2)*G(I3) - F(I3)*G(I2)
      RETURN
      END
C
      SUBROUTINE dCROSS(F,G,H)
      IMPLICIT NONE
      DOUBLE PRECISION 
     1        F(3),
     1        G(3),
     1        H(3)
      H(1) = F(2)*G(3) - F(3)*G(2)
      H(2) = F(3)*G(1) - F(1)*G(3)
      H(3) = F(1)*G(2) - F(2)*G(1)
      RETURN
      END
C
      SUBROUTINE DOT(F,G,H)
      IMPLICIT NONE
      REAL
     1      F(3),
     1      G(3),
     1      H
      H = F(1)*G(1) + F(2)*G(2) + F(3)*G(3)
      RETURN 
      END
C
      SUBROUTINE dDOT(F,G,H)
      IMPLICIT NONE
      DOUBLE PRECISION
     1      F(3),
     1      G(3),
     1      H
      H = F(1)*G(1) + F(2)*G(2) + F(3)*G(3)
      RETURN 
      END
C
      SUBROUTINE DET3( MAT3, DET)
      IMPLICIT NONE
C work out the determinant of a 3*3 matrix MAT3
      REAL			MAT3(3,3), DET

      DET = MAT3(1,1)*(MAT3(2,2)*MAT3(3,3)-MAT3(2,3)*MAT3(3,2))
     &     - MAT3(1,2)*(MAT3(2,1)*MAT3(3,3)-MAT3(2,3)*MAT3(3,1))
     &     + MAT3(1,3)*(MAT3(2,1)*MAT3(3,2)-MAT3(2,2)*MAT3(3,1))
      RETURN
      END
C
      SUBROUTINE dUVECT(F,UF,LENF)
      IMPLICIT NONE
C returns unit vector in direction F & its length
      DOUBLE PRECISION
     1           F(3),
     1           UF(3),
     1           LENF
      INTEGER    I
      
      CALL dDOT(F,F,LENF)
      LENF = SQRT(LENF)
      
      DO 10 I = 1,3
	UF(I) = F(I)/LENF
10    CONTINUE

      RETURN
      END
C
      SUBROUTINE dUVEC2(F)
      IMPLICIT NONE
C returns unit vector in direction F
      DOUBLE PRECISION
     1           F(3),
     1		 LENF
      INTEGER    I
      
      CALL dDOT(F,F,LENF)
      LENF = SQRT(LENF)

      DO 10 I = 1,3
	F(I) = F(I)/LENF
10    CONTINUE

      RETURN
      END
C
      SUBROUTINE UVECT(F,UF,LENF)
      IMPLICIT NONE
C returns unit vector in direction F & its length
      REAL
     1      	 F(3),
     1     	 UF(3),
     1     	 LENF
      INTEGER 	 I
      
      CALL DOT(F,F,LENF)
      LENF = SQRT(LENF)
      
      DO 10 I = 1,3
        UF(I) = F(I)/LENF
10    CONTINUE

      RETURN
      END
C
      SUBROUTINE dUVECT2(F)
      IMPLICIT NONE
C returns unit vector in direction F
      DOUBLE PRECISION
     1           F(3),
     1           LENF
      INTEGER    I

      CALL dDOT(F,F,LENF)
      LENF = SQRT(LENF)

      DO 10 I = 1,3
        F(I) = F(I)/LENF
10    CONTINUE

      RETURN
      END

      SUBROUTINE UVECT2(F)
      IMPLICIT NONE
C returns unit vector in direction F
      REAL
     1      	 F(3),
     1     	 LENF
      INTEGER 	 I
      
      CALL DOT(F,F,LENF)
      LENF = SQRT(LENF)

      DO 10 I = 1,3
        F(I) = F(I)/LENF
10    CONTINUE

      RETURN
      END

      SUBROUTINE MATI22( MAT, NOUT, LERR)
      IMPLICIT NONE
C this s/r inverts a 2*2 matrix
C (c) May 1994 Oliver Smart
      DOUBLE PRECISION 			MAT(2,2), SWAP, DET
C if a matrix with a determinant of zero is supplied
C then an error is written to nout and lerr is returned true
      INTEGER				NOUT
      LOGICAL				LERR

C end of decs **************************

C work out determinant
      DET = MAT(1,1)*MAT(2,2) - MAT(1,2)*MAT(2,1)
      IF (DET.EQ.0.) THEN
        WRITE( NOUT, '(A/A/2F12.5/2F12.5)')
     &' ERROR detected in s/r MATI22 (invert a 2*2 matrix)',
     &' Supplied with matrix having a determinant of zero: ',
     & MAT
        LERR = .TRUE.
        GOTO 55555
      ENDIF

C swap left hand diagonal
      SWAP = MAT(1,1)/DET
      MAT(1,1) = MAT(2,2)/DET
      MAT(2,2) = SWAP

C change sign of right
      MAT(2,1) = -MAT(2,1)/DET
      MAT(1,2) = -MAT(1,2)/DET
        
55555 RETURN
      END
