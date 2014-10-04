c ********************************************************************
C *                                                                  *
C * This software is an unpublished work containing confidential and *
C * proprietary information of Birkbeck College. Use, disclosure,    *
C * reproduction and transfer of this work without the express       *
C * written consent of Birkbeck College are prohibited. This notice  *
C * must be attached to all copies or extracts of the software.      *
C *                                                                  *
C * (c) 1993 Oliver Smart & Birkbeck College, All rights reserved    *
C * (c) 1996 Oliver Smart & Birkbeck College, All rights reserved    *
C * (c) 1997 Oliver Smart                    *
C *                                                                  *
C ********************************************************************
C
C Modification history:
C
C Date	Author		Modification
C 12/93	O.S. Smart	Original public release in HOLE suite beta1.0
C 11/95 O.S. Smart	Subroutines SF2 and STRIPS added
C 11/97 O.S. Smart      PROMPT changed to use vt control codes
C 11/97 O.S. Smart      and vtclear has been added            
C
C ut_strings.f - utility routines dealing with strings
C contains s/r's chrend (find last character in string)
C                decom  (remove ! comments from lines)
C                lcase  (change string to lower case)
C                lmatch (match two strings - case insensitive & wildcard)
C                prompt (prompt the user with a string NOT STANDARD F77)
C                ucase  (change string to upper case)
C
      SUBROUTINE CHREND( STRING, IEND)
      IMPLICIT NONE
C supplied with character vble string: returned unchanged
C returns position of last non-(blank,tab,null) element of string
      CHARACTER*(*)			STRING
      INTEGER				IEND

C loop count, dummy vble
      INTEGER				IC, IDUM

      DO 10 IC = LEN(STRING), 1, -1
        IDUM = ICHAR(STRING(IC:IC))
        IF ((IDUM.EQ.32) .OR.(IDUM.EQ.9) .OR.(IDUM.EQ.0)) THEN
          CONTINUE
        ELSE
          GOTO 55555
        ENDIF
10    CONTINUE

55555 IEND = IC
      RETURN
      END
C
      SUBROUTINE DECOM( STRING)
      IMPLICIT NONE
C this s/r removes comments for imput string
C start of comment is indicated by '!'
C also removes tabs - replaces by 6 spaces
C written by Oliver Smart at Imperial College © 1990
      CHARACTER*(*)             STRING

C string length, loop count
      INTEGER                   ILEN, IC

C end of decs ******************

      ILEN = LEN(STRING)
      DO 10 IC = 1, ILEN
C remove tab
        IF (ICHAR(STRING(IC:IC)).EQ.9) THEN
          STRING(IC+6:ILEN) = STRING(IC+1:ILEN)
          STRING(IC:IC+5) = ' '
        ENDIF
        IF (STRING(IC:IC).EQ.'!') THEN
          STRING(IC:ILEN) = ' '
          GOTO 55555
        ENDIF
10    CONTINUE
55555 RETURN
      END
C
      SUBROUTINE LCASE(STRING)
      IMPLICIT NONE
      SAVE
C
C This software is an unpublished work containing confidential and 
C proprietary information of Imperial College. Use, disclosure,
C reproduction and transfer of this work without the express
C written consent of Imperial College are prohibited.
C
C (c) 1991 Oliver Smart; 
C	   Imperial College of Science, Technology and Medicine; 
C          University of London; London, United Kingdom.;
C          All Rights Reserved.	
C
C this fn changes string into lower case characters
      CHARACTER*(*) 		STRING
      INTEGER			ICHR, IC

      DO 10 IC = 1, LEN(STRING)
        ICHR = ICHAR(STRING(IC:IC))
        IF( (ICHR.GT.90) .OR. (ICHR.LT.65) ) GOTO 10
        ICHR = ICHR + 32
        STRING(IC:IC) = CHAR(ICHR)
10    CONTINUE

      RETURN
      END
C
      LOGICAL FUNCTION LMATCH( WILDC, NMATCH, STR1, STR2)
      IMPLICIT NONE
C This logical function returns true if the first NMATCH
C characters of the string STR1 match those of STR2.
C The match is not case sensitive and wildc is the wild
C character.

C wild character e.g. '?'
C return unchanged
      CHARACTER*1		    WILDC

C The no. of characters to match
C return unchanged
      INTEGER			    NMATCH

C the strings
C return unchanged
      CHARACTER*(*)		    STR1, STR2

C internal variables

C to store single characters
      CHARACTER*1		    CHAR1, CHAR2

C loop count
      INTEGER			    ICOUNT

C end of decs **********************

C check that strings have the required no. of chars
      IF ( (LEN(STR1).LT.NMATCH) .OR. (LEN(STR2).LT.NMATCH) ) THEN
	WRITE(*,*) 'LMATCH has been called with too short strings'
	WRITE(*,*) '  NMATCH: ', NMATCH
	WRITE(*,*) '  STR1: ', STR1
	WRITE(*,*) '  STR2: ', STR2
	STOP 'FORTRAN STOP - programing error in log fun LMATCH'
      ENDIF

C start with no match
      LMATCH = .FALSE.

C go thru' and match character by character
      DO 10 ICOUNT = 1, NMATCH

C characters to match
	CHAR1 = STR1(ICOUNT:ICOUNT)
	CHAR2 = STR2(ICOUNT:ICOUNT)

C make both upper case
	CALL UCASE( CHAR1)
	CALL UCASE( CHAR2)

C if either character is wild then have a match
	IF ((CHAR1.EQ.WILDC) .OR. (CHAR2.EQ.WILDC)) THEN
C so continue
	  CONTINUE
C are the characters different
	ELSEIF (CHAR1.NE.CHAR2) THEN
C no. match
	  GOTO 55555
	ENDIF

10    CONTINUE

C all characters match
      LMATCH = .TRUE.

55555 RETURN
      END
C
C
      SUBROUTINE PROMPT( NOUT, PSTR)
      IMPLICIT NONE
C this s/r prompts the user with string pstr supplied
C modified 13/11/97 to change all strings within <....>
C to reverse video
      CHARACTER*(*)			PSTR
      INTEGER				NOUT
C internal string (needed for modification of PSTR)
      CHARACTER*200			ISTR
C last non-blank character of the string
      INTEGER				IEND

C left and right angled bracket positions (< and >),
C the number of characters cleared so far
      INTEGER				WHEREA, SORTED


C control strings for VT control codes
      INCLUDE 'VTCONTROLINC'
      
C put data statements here for programs which haven't yet been updated
      DATA				VTON /.FALSE./
      DATA				VTBOLD /.FALSE./
      DATA				VTNORM /''/


C end of decs **************************


C either do straight prompt or have to much about with vt codes
      IF (.NOT.VTON) THEN
C find the last character
        CALL CHREND( PSTR, IEND)
C normal write
        WRITE( NOUT, '(1X,A,A1,$)') PSTR(1:IEND),' '

      ELSE
C copy string across
        ISTR = PSTR
C initialize sorted to start looking at the front of the string
        SORTED = 0

C jump back here after a replacement
10      CONTINUE

C look for occurence of <
        WHEREA = INDEX(ISTR(SORTED+1:200),'<')
        IF (WHEREA.NE.0) THEN
C found one - have to change string:
C change "a<CDEF" to "a<$[7mCDEF..." where $ is the <ESC> character (ascii number 27)
C           12345       12345
C correct wherea to include previously cleared bit
          WHEREA = WHEREA + SORTED
          ISTR(WHEREA+5:200) = ISTR(WHEREA+1:194)
          ISTR(WHEREA+1:WHEREA+1) =  CHAR(27)
          ISTR(WHEREA+2:WHEREA+4) = '[7m'
C record where next search should start
          SORTED = WHEREA
C        write(*,'(i5,a)') sorted, istr(1:60)
          GOTO 10
        ENDIF

C now do right hand bracket expect use different escape character
C and want it before the bracket not after it
C change "a>CDEF" to "a$[7m>CDEF..." where $ is the <ESC> character (ascii number 27)
C           12345       12345
        SORTED = 0
20      CONTINUE
        WHEREA = INDEX(ISTR(SORTED+1:200),'>')
        IF (WHEREA.NE.0) THEN
          WHEREA = WHEREA + SORTED
          ISTR(WHEREA+4:200) = ISTR(WHEREA:194)
          ISTR(WHEREA:WHEREA) = CHAR(27)
          ISTR(WHEREA+1:WHEREA+3) = '[0m'
          SORTED = WHEREA + 5
C        write(*,'(i5,a)') sorted, istr(1:60)
          GOTO 20
        ENDIF

C find the last character
        CALL CHREND( ISTR, IEND)
C 12/11/1997 new really fancy option add change to bold after prompt
C This must be turned off at start of next
C turn on/off with VTBOLD passed in common block (programs
C will need quite a large rewrite
        IF (VTBOLD) THEN
          WRITE( NOUT, '(1X,A,$)') 
     &    CHAR(27)//'[0m'//ISTR(1:IEND)//CHAR(27)//'[1m'//' '
        ELSE
C normal write
          WRITE( NOUT, '(1X,A,A1,$)') ISTR(1:IEND),' '
        ENDIF
C end of muching about with vt controls
      ENDIF

      RETURN
      END
C
      SUBROUTINE UCASE(STRING)
      IMPLICIT NONE
C this fn changes string into upper case characters
      CHARACTER*(*) 		STRING
      INTEGER			ICHR, IC

      DO 10 IC = 1, LEN(STRING)
        ICHR = ICHAR(STRING(IC:IC))
        IF( (ICHR.GT.122) .OR. (ICHR.LT.97) ) GOTO 10
        ICHR = ICHR - 32
        STRING(IC:IC) = CHAR(ICHR)
10    CONTINUE

      RETURN
      END
C
      SUBROUTINE SF2( STRING, LASTC, NUMB)
      IMPLICIT NONE
C subroutine to output numbers sensibly to two significant figures
C the number to be written
      DOUBLE PRECISION		NUMB
C string for number to be written to 
      CHARACTER*8		STRING
C last character
      INTEGER			LASTC

C exponential
      INTEGER			NEXP

C prexpontial 
      DOUBLE PRECISION		PREEXP, DUMB

C format string
      CHARACTER*6		FORM

C end of decs ******************

C clear string
      STRING = '        '

C exponential
      DUMB = LOG10(ABS(NUMB))
      IF (DUMB.GT.0) THEN
        NEXP = INT(DUMB)
      ELSE
        NEXP = INT(DUMB) - 1
      ENDIF

C the prexponential
      PREEXP = NUMB*(10D0**(-NEXP))

C trap preexp of 10.
      IF (PREEXP.EQ.10D0) THEN
        PREEXP = 1D0
        NEXP = NEXP+1
      ELSEIF (PREEXP.EQ.-10D0) THEN
        PREEXP = -1D0
        NEXP = NEXP+1
      ENDIF


C make the number two significant figures
      PREEXP = 0.1D0*NINT(PREEXP*10D0)

C big or small number? If so write in sensible exponential
      IF ((NEXP.GT.5) .OR. (NEXP.LT.-2)) THEN
C +ve or -ve number
        IF (PREEXP.LT.0.) THEN
C -ve
          WRITE( STRING(1:4),'(F4.1)') PREEXP
          STRING(5:5) = 'e'
C number of places
C -1.2e11?
          IF (NEXP.GT.9) THEN
            WRITE(STRING(6:7),'(I2)') NEXP
            LASTC = 7
C -3.4e6?
          ELSEIF (NEXP.GT.0) THEN
            WRITE(STRING(6:6),'(I1)') NEXP
            LASTC = 6
C -3.4e-9?
          ELSEIF (NEXP.GT.-10) THEN
            WRITE(STRING(6:7),'(I2)') NEXP
            LASTC = 7
          ELSE
            WRITE(STRING(6:8),'(I3)') NEXP
            LASTC = 8
          ENDIF
        ELSE
C +ve number
          WRITE( STRING(1:3),'(F3.1)') PREEXP
          STRING(4:4) = 'e'
C number of places
C -1.2e11?
          IF (NEXP.GT.9) THEN
            WRITE(STRING(5:6),'(I2)') NEXP
            LASTC = 6
C -3.4e6?
          ELSEIF (NEXP.GT.0) THEN
            WRITE(STRING(5:5),'(I1)') NEXP
            LASTC = 5
C -3.4e-9?
          ELSEIF (NEXP.GT.-10) THEN
            WRITE(STRING(5:6),'(I2)') NEXP
            LASTC = 6
          ELSE
            WRITE(STRING(5:7),'(I3)') NEXP
            LASTC = 7
          ENDIF
        ENDIF
      ELSE
C not big or small write out as proper number
        IF (NEXP.GT.0) THEN
C want number to be aligned on left 
          LASTC = NEXP + 2
C if the number is -ve then want additional character
          IF (PREEXP.LT.0.) LASTC = LASTC + 1
          WRITE( FORM, '(A2,I1,A3)') '(F', LASTC, '.0)'
          WRITE( STRING, FORM) PREEXP*(10D0**NEXP)
C do not want to write the final dot if no is 10 or above
          IF (NEXP.GT.0) LASTC = LASTC - 1
        ELSE
C make up format want one fewer place than number
C string aligned to left
          LASTC = ABS(NEXP) + 3
C if the number is -ve then want additional character
          IF (PREEXP.LT.0.) LASTC = LASTC + 1
          WRITE( FORM, '(A2,I1,A1,I1,A1)') 
     &      '(F', LASTC, '.', ABS(NEXP-1), ')'
          WRITE( STRING, FORM) PREEXP*(10D0**NEXP)
        ENDIF

      ENDIF
      
      END
C
      SUBROUTINE STRIPS( STRING)
      IMPLICIT NONE
C this s/r strips of the leading spaces from a string
C n.b. tab is also considered as a space
      CHARACTER*(*)		STRING

C internal vbles

C loop count to go thru individual characters of string
      INTEGER			CCOUNT

C end of decs ******************

C if the first character is already non-blank simply return
      IF ( (STRING(1:1).EQ.' ') .OR.
     &     (ICHAR(STRING(1:1)).EQ.9)  ) THEN


C go through characters one by one
        DO 10 CCOUNT = 1, LEN(STRING)
C is ccount blank and  ccount+1 non-blank?
C regard tab as a blank (test by seeing whether ichar returns 9)
          IF ( ( (STRING(CCOUNT:CCOUNT).EQ.' ') .OR. 
     &           (ICHAR(STRING(CCOUNT:CCOUNT)).EQ.9)  ) .AND.
     &         ( (STRING(CCOUNT+1:CCOUNT+1).NE.' ') .AND.            
     &           (ICHAR(STRING(CCOUNT+1:CCOUNT+1)).NE.9) )   ) THEN
C character number ccount is the last preceeding blank
            STRING = STRING(CCOUNT+1:LEN(STRING))
            GOTO 55555
          ENDIF
10      CONTINUE
      ENDIF

C return here
55555 CONTINUE
      RETURN
      END
C
      SUBROUTINE VTCLEAR( NOUT)
      IMPLICIT NONE
C output stream
      INTEGER			NOUT
C control strings for VT control codes
      INCLUDE 'VTCONTROLINC'
C subroutine to get a vt terminal back to normal
      IF (VTON) WRITE( NOUT,'(a,$)') CHAR(27)//'[0m'
      RETURN
      END
C
      SUBROUTINE VTCON( PBOLD)
      IMPLICIT NONE
C subroutine to turn on use of VT control codes in rest of a program
C so that terminal uses bold and reverse video
C codes to control vt
C intended to be called as first statement in any program

C only one arguement - if PBOLD is set true will turn on LBOLD
C which controls whether s/r prompt switches terminal to bold for
C command input
      LOGICAL		PBOLD
      INCLUDE 'VTCONTROLINC'

C end of decs

C turn on VT control codes
      VTON = .TRUE.

C turn on bold making
      VTBOLD = PBOLD
C vt escape code to get back to normal
      VTNORM = CHAR(27)//'[0m'

C make sure that start in normal font
      CALL VTCLEAR( 6)
      
      RETURN
      END      
