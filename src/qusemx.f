      SUBROUTINE QUSEMX(NIN, NOUT, MAXST, MAXCOL, MAT3,
     &  LABORT,  RSTORE, ISTORE, MAXTXT, TXTNO, TXTPOS)
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
C 12/93	O.S. Smart	Original public release in HOLE suite beta1.0
C
C
C this s/r uses the rotation matrix MAT3 from the 
C hydra view file and calculates the new
C coordinates of the move/draws points accordingly

C passed variables

C user input/output streams returned unchanged
      INTEGER                   NIN, NOUT

C the maximum number of colours is stored in MAXCOL
C MAXCOL is assigned a value in qplot and MUST not be changed
C MAXST - is the maximum number of move draws to be stored
C         this is set a parameter in qplot and MUST not be changed
C istore( 1 to MAXCOL) is returned with the number of records
C    read for each colour
C rstore is the store for records
      INTEGER                   MAXST, MAXCOL
      INTEGER                   ISTORE(MAXCOL)
      REAL                      RSTORE( MAXST, 0:3, MAXCOL)

C the text records - only ancor points
C maximum number of records parameter in qplot (return unchanged) 
      INTEGER                   MAXTXT
C the number of text records stored (return unchanged)
      INTEGER                   TXTNO
C the ancor point of each text record - rotate in this routine
      REAL                      TXTPOS( 3, MAXTXT)

C an abort indicator
C if we find a problem in this s/r return true
      LOGICAL                   LABORT
C takes the matrix values in double array MAT3 (return unchanged)
      REAL                      MAT3(3,3)

C variables internal to this routine

C loop counts for colour, for records
      INTEGER                   CCOUNT, MCOUNT

C inner variables
C
C an array which will hold preliminary data for rotating s/r
C called ROTATE
      REAL                      NUM(3)

C **************** end of declarations ***********

C first rotate the move/draw records
C go thru colours
      DO 10 CCOUNT = 1, MAXCOL
C go thru records of this colour
        DO 20 MCOUNT = 1, ISTORE(CCOUNT)
C   apply matrix via subroutine ROTATE
C every coordinate point is stored in a 
C different element of the array NUM
          NUM(1) = RSTORE(MCOUNT,1,CCOUNT)
          NUM(2) = RSTORE(MCOUNT,2,CCOUNT)
          NUM(3) = RSTORE(MCOUNT,3,CCOUNT)

C this call will rotate the coordinates 
C according to the matrix values
C s/r is called with two arguments -
C an array holding the coordinates ( NUM ) and
C the actual matrix (MAT3)
          CALL ROTATE( NUM, MAT3)
C resets the coordinates with the now rotated values
          RSTORE(MCOUNT,1,CCOUNT) = NUM(1)
          RSTORE(MCOUNT,2,CCOUNT) = NUM(2)
          RSTORE(MCOUNT,3,CCOUNT) = NUM(3)
20      CONTINUE
10    CONTINUE

C rotate text ancor points
      DO 30 MCOUNT = 1, TXTNO
        CALL ROTATE( TXTPOS( 1, MCOUNT), MAT3)
30    CONTINUE

      RETURN
      END
C%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%5
C%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%5
      SUBROUTINE ROTATE(NUMVEK,MAT3)
      IMPLICIT NONE
C this s/r rotates a 3 vector by a 3x3 matrix
      REAL                  NUMVEK(3), MAT3(3,3), DUM(3)
      INTEGER               IX

C end of decs
C
C puts the initial values of array NUMVEK in array DUM
      DO 10  IX = 1,3
          DUM(IX)= NUMVEK(IX)
10    CONTINUE
C every rotated coordinate is related to the previous value
C - stored in DUM - and modelled by the value of MAT3
      DO 20   IX = 1, 3
          NUMVEK(IX) = DUM(1)*MAT3(IX,1) + DUM(2)*MAT3(IX,2) +
     &                 DUM(3)*MAT3(IX,3)
20    CONTINUE

      RETURN
      END
