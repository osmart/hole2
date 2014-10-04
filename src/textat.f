      SUBROUTINE TEXTAT ( SOUT, FSCALE, STRING, STRLEN,
     &                    XCOOR, YCOOR, ICONT)
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
C  this s/r writes a passed as argument string (STRING) at a
C position given by a control integer (ICONT) -range 1:9-,
C relative to an ancor point with coordinates XCOOR, YCOOR;
C between the actual string and the point
C there is a displacement space given by DISPLC;
C FSCALE is the size of the scale font to be applied

C passed variables
C
C output file stream
      INTEGER                   SOUT
C real vbls. for the coordinates of the ancor point
      REAL                      XCOOR, YCOOR
C variable which will hold the control number (position)
      INTEGER                   ICONT
C string to be positioned
      CHARACTER *(*)            STRING
C string length 
      INTEGER                   STRLEN
C the scale of the fonts
      REAL                      FSCALE

C internal variables
C
C variable which helps establishing the displacement size
      REAL                      DISPLC
C variables which help establishing the string position
      REAL                      ESTX, WESTX 
      REAL                      NORTHY, SOUTHY
C end of decs.

C**o****************g**********************
C displacement set to half the size of
C the font scale - around the point(XCOOR, YCOOR)
      DISPLC = FSCALE/8
C set the coordinates of the displacement position.
      ESTX =   XCOOR + DISPLC
      WESTX =  XCOOR - DISPLC
      NORTHY = YCOOR + DISPLC
      SOUTHY = YCOOR - DISPLC
C starts writing the string ABCD
      WRITE(SOUT, '(A,A,A)')
     &' (', STRING(1:STRLEN), ')'
C where should the string be positioned relative
C to the input point ?
C checking which arrow key has been passed and
C set the right position of the string;
C default is position 9

C if <4>  has been passed
      IF ( ICONT.EQ.4.0) THEN
C the height of the letter scale is adjusted
C down by 0.694 and halved so that
C the centering could take place;
C the resulted value is substracted from the
C Y coordinate in order to center the string
       YCOOR = YCOOR - (FSCALE/2)*0.694
       WRITE(SOUT, '(A)')
C function "stringwidth" needs a duplicate string
C to work on(func. "dup"); it replaces the string with the
C values of Y and X (coord. of the would be string);
C the value of Y is discarded with "pop"
     &' dup stringwidth pop '
       WRITE(SOUT, '(F10.3,A)')
C the string length must be substracted
C from the actual coordinate;
C the arguments of the substraction are inversed
C hence function "exch" is called
     & WESTX, ' exch sub '
       WRITE(SOUT, '(F10.3,A)') YCOOR,
     &' moveto show'

C if <7>  has been passed
      ELSEIF ( ICONT.EQ.7.0) THEN
C function "stringwidth" needs a duplicate string
C to work on(func. "dup"); it replaces the string with the
C values of Y and X (coord. of the would be string);
C the value of Y is discarded with "pop"
       WRITE(SOUT, '(A)')
     &' dup stringwidth pop '
       WRITE(SOUT, '(F10.3,A)')
C the string length must be substracted
C from the actual coordinate
C the arguments of the substraction are inversed
C hence function "exch" is called
     & WESTX, ' exch sub '
       WRITE(SOUT, '(F10.3,A)') NORTHY,
     &' moveto show'

C if <1>  has been passed
      ELSEIF ( ICONT.EQ.1.0) THEN
       SOUTHY = SOUTHY-FSCALE*0.694
       WRITE(SOUT, '(A)')
C function "stringwidth" needs a duplicate string
C to work on(func. "dup"); it replaces the string with the
C values of Y and X (coord. of the would be string);
C the value of Y is discarded with "pop"
     &' dup stringwidth pop '
       WRITE(SOUT, '(F10.3,A)')
C the string length must be substracted
C from the actual coordinate
C the arguments of the substraction are inversed
C hence function "exch" is called
     & WESTX, ' exch sub '
       WRITE(SOUT, '(F10.3,A)') SOUTHY,
     &' moveto show'

C if <8> has been passed
      ELSEIF (ICONT.EQ.8.0) THEN
C function "stringwidth" needs a duplicate string
C to work on(func. "dup"); it replaces the string with the
C values of Y and X (coord. of the would be string);
C the value of Y is discarded with "pop"
       WRITE(SOUT, '(A)')
     &' dup stringwidth pop '
       WRITE(SOUT, '(A,F10.3,A)')
     &' 2 div ', XCOOR,
C half the string length must be substracted
C from the actual coordinate (fnc "sub");
C the arguments of the substraction are inversed
C hence function "exch" is called
     & ' exch sub '
      WRITE(SOUT, '(F10.3,A)') NORTHY,
     &' moveto show'

C if <2> has been passed
      ELSEIF (ICONT.EQ.2.0) THEN
       SOUTHY = SOUTHY-FSCALE*0.694
C function "stringwidth" needs a duplicate string
C to work on(func. "dup"); it replaces the string with the
C values of Y and X (coord. of the would be string);
C the value of Y is discarded with "pop"
       WRITE(SOUT, '(A)')
     &' dup stringwidth pop '
       WRITE(SOUT, '(A,F10.3,A)')
     &' 2 div ', XCOOR,
C half the string length must be substracted
C from the actual coordinate (fnc "sub");
C the arguments of the substraction are inversed
C hence function "exch" is called
     & ' exch sub '
      WRITE(SOUT, '(F10.3,A)') SOUTHY,
     &' moveto show'

C if < 6 > has been passed
      ELSEIF (ICONT.EQ.6.0) THEN
       YCOOR = YCOOR - (FSCALE/2)*0.694
C the height of the letter scale is adjusted
C down by 0.694 and halved so that
C the centering could take place
C the resulted value is substracted from
C Y coordinate in order to center the string
        WRITE(SOUT, '(F10.3,A)')
     & ESTX, ' '
        WRITE(SOUT, '(F10.3,A)') YCOOR,
     &' moveto show '

C if < 3 > has been passed
      ELSEIF (ICONT.EQ.3.0) THEN
       SOUTHY = SOUTHY-FSCALE*0.694
        WRITE(SOUT, '(F10.3,A)')
     & ESTX, ' '
        WRITE(SOUT, '(F10.3,A)') SOUTHY,
     &' moveto show '

C if < 5> has been passed
      ELSEIF (ICONT.EQ.5.0) THEN
       YCOOR = YCOOR - (FSCALE/2)*0.694
C the height of the letter scale is adjusted
C down by 0.694 and halved so that
C the centering could take place
C the resulted value is substracted from
C Y coordinate in order to center the string
       WRITE(SOUT, '(A)')
C function "stringwidth" needs a duplicate string
C to work on(func. "dup"); it replaces the string with the
C values of Y and X (coord. of the would be string);
C the value of Y is discarded with "pop"
     &' dup stringwidth pop '
       WRITE(SOUT, '(A,F10.3,A)')
     &' 2 div ', XCOOR,
C half the string length must be substracted
C from the actual coordinate (fnc "sub");
C the arguments of the substraction are inversed
C hence function "exch" is called
     & ' exch sub '
       WRITE(SOUT, '(F10.3,A)') YCOOR,
     &' moveto show'

C if <9> or anything else has been passed
      ELSE
        WRITE(SOUT, '(F10.3,A)')
     & ESTX, ' '
        WRITE(SOUT, '(F10.3,A)') NORTHY,
     &' moveto show '

      ENDIF

      RETURN
      END

