C ********************************************************************
C *                                                                  *
C * This software is an unpublished work containing confidential and *
C * proprietary information of Birkbeck College. Use, disclosure,    *
C * reproduction and transfer of this work without the express       *
C * written consent of Birkbeck College are prohibited. This notice  *
C * must be attached to all copies or extracts of the software.      *
C *                                                                  *
C * (c) 1996 Oliver Smart & Birkbeck College, All rights reserved    *
C *                                                                  *
C ********************************************************************

C s/r readse.f is not machine dependent but infact is part of series
C program included here as it is called by s/r interf -
C on non-series application should behave like simple call to
C read
      SUBROUTINE READSE( NIN, NOUT, *, *, STRING)
      IMPLICIT NONE
C this subroutine is used to implement command record/playback
C (turned on/off by HIST option in series)
C All reads from stream nin will be replaced by this routine
C e.g. typical line:
C    READ( NIN, '(A)', END= 55555, ERR= 555555) COM
C    CALL READSE( NIN, NOUT, *55555, *55555, COM)
C all reads will now have to be done into character strings
C
C according to value of SERHIS the read will be made
C either from keyboard if SERHIS .ge. 0
C or from stream if serhis is less than zero
C
C 14/02/95 adapted so that command preceed by ! is passed to
C          system s/r (and run by a shell). n.b. should still be recorded

C passed variables *************
C input stream, output stream number
      	INTEGER			NIN, NOUT
C variable to be found in this s/r (from nin or file)
        CHARACTER*(*)		STRING

C common block /SERHIS/ ********
C control of recording/playback is by vble
C STRHIS = 0 no record/playback
C         +ve then record to stream serhis
C         -ve then playback from stream -serhis
C ISLOW - if 0 then playback normally
C            1 then delay each command
C           -1 then prompt for appoval of each command
      INTEGER                   STRHIS
      INTEGER                   ISLOW
      COMMON /SERHIS/           STRHIS, ISLOW

C internal vbles ***************

C history filename, command
      CHARACTER*200		FNAME, COM

C last character
      INTEGER			LASTC

C dummy real, loop count for slowing down
      REAL			RDUM
      REAL			RCOUNT

C dummy variable for string within routine needed for
C input of HIST_EOF and HIST_END when there are fewer than
C 8 characters in STRING
      CHARACTER*200		STRDUM

C end of decs ******************

C jump to top after shell command
30    CONTINUE

C if strhis is zero then just read from keyboard
      IF (STRHIS.GE.0) THEN
        READ( NIN, '(A)', END= 900, ERR= 800) STRDUM 

C store command if strhis says so (i.e. is +ve)
        IF (STRHIS.GT.0) THEN 
          CALL CHREND( STRDUM, LASTC)
	  WRITE( STRHIS,'(A)') STRDUM(1:LASTC) 
          CALL DISKFF( STRHIS)
        ENDIF

      ELSE
C doing play back
C read line from playback file (on end close file)
        READ( -STRHIS, '(A)', END= 700) STRDUM
C mirror output
        CALL CHREND( STRDUM, LASTC)
        CALL VTCLEAR( NOUT)
        WRITE( NOUT, '(A,A)') STRDUM(1:LASTC), 
     &                        ' ## history playback command'
C pause? just do a slow calculation (about a sec on rs6000)
        IF (ISLOW.EQ.1) THEN
          RDUM = 0.
          DO 10 RCOUNT = 1, 5E5
            RDUM = RDUM + SQRT( RCOUNT)
10        CONTINUE
C if islow is -1 
        ELSEIF (ISLOW.EQ.-1) THEN
          COM = 
     &'Approve, Replace (enter string), or stop'//
     &      ' playback (ctrl-D) <approve>: '
          CALL PROMPT( NOUT, COM)
C on ctrl-D so stop playback mode by going to line 700
          READ( NIN, '(A)', END= 700, ERR= 700) COM
C replace string by entered command if there is one
          CALL CHREND( COM, LASTC)
          IF (LASTC.NE.0) STRDUM = COM
        ENDIF

C if string is HISTEND then should regard as if END has been entered
        IF (STRDUM(1:8).EQ.'HIST_EOF') RETURN 1
        IF (STRDUM(1:8).EQ.'HIST_ERR') RETURN 2

      ENDIF

C is the command  a shell command?
      IF (STRDUM(1:1).EQ.'!') THEN
        IF (STRDUM(2:3).EQ.'cd') THEN
          CALL CHDIR( STRDUM(5:80))
          CALL VTCLEAR( NOUT)
          WRITE(NOUT,*) ' Changed directory to: '
          CALL SYSTEM( 'pwd')  
C otherwise just pass to system which starts a shell will the command
        ELSE
	  CALL SYSTEM( STRDUM(2:80) ) 
        ENDIF
C ask again
        CALL PROMPT( NOUT,
     & 'Answer previous prompt (after ! command):')
C loop to top of routine
        GOTO 30
      ENDIF

C return here
55555 CONTINUE
      CALL VTCLEAR( NOUT)
C put string vble in right place
      STRING = STRDUM
      RETURN

C EOF-ERR routines after this
900   CONTINUE
      CALL VTCLEAR( NOUT)
C end has been entered
      IF (STRHIS.GT.0) THEN
        WRITE(STRHIS,'(A)') 'HIST_EOF'
        CALL DISKFF( STRHIS)
      ENDIF

      RETURN 1

800   CONTINUE
      CALL VTCLEAR( NOUT)
C err encounted
      IF (STRHIS.GT.0) THEN
        WRITE(STRHIS,'(A)') 'HIST_ERR'
        CALL DISKFF( STRHIS)
      ENDIF

      RETURN 2

700   CONTINUE
C end of file encounter on playback
C find filename
      INQUIRE( -STRHIS, NAME= FNAME)
      CALL CHREND( FNAME, LASTC)
      WRITE( NOUT, '(A)') 
     &' '//CHAR(7),
     &' Aborted or Reached end of file on history file playback.',
     &' Closing file: '//FNAME(1:LASTC),
     &' and passing control back to keyboard'
      CALL PROMPT( NOUT,
     & 'Answer prompt above!:')
C close history file
      CLOSE( -STRHIS)
C clear playback vble
      STRHIS = 0
C read in from keyboard
      READ( NIN, '(A)', END= 900, ERR= 800) STRDUM 
      GOTO 55555

      END
