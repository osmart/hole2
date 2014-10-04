      LOGICAL FUNCTION LPAUSE( COUNT, NIN, NOUT)
      IMPLICIT NONE
      SAVE
C this fn. is intended to be used to cause periodic pauses
C in display of a "stream" of values when COUNT is 10, 30, 50 etc.
C the routine prompts stream NOUT as to whether to continue
C LPAUSE returned as false if we wish to continue 
C                 as true to quit
C 31/7/92 Modified to allow the user to respecify page length
C (returned unchanged)
      INTEGER			COUNT, NIN, NOUT

C for command
      CHARACTER*20		COM

C page length - start at 20 - new value for page
      INTEGER			PAGE, NEWP
      DATA 			PAGE /20/

C end of decs ******************


C .nb. an eof or error also indicates a quit
      IF (MOD(COUNT+10,PAGE).EQ.0) THEN
        LPAUSE = .TRUE.
C continue or quit?
        CALL PROMPT( NOUT,
     &'Continue or quit? Enter ''q'' to quit '//
     & '(no. to change page length) <continue>:')
C read answer
        READ( NIN, '(A)', ERR= 55555, END= 55555) COM
      	CALL UCASE(COM(1:1))
	IF (COM(1:1).NE.'Q') THEN
          LPAUSE = .FALSE.
C try to read page from line
          READ( COM, '(BN,I10)', ERR= 55555) NEWP
          IF (NEWP.GT.0) PAGE = NEWP
        ENDIF
      ELSE
	LPAUSE = .FALSE.
      ENDIF

55555 CONTINUE
C reset vt terminal if this is appropriate (15/11/97)
      CALL VTCLEAR( NOUT)
      RETURN
      END 
