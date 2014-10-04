      SUBROUTINE WHATU( STREAM)
      IMPLICIT NONE
      SAVE
C
C This software is an unpublished work containing confidential and 
C proprietary information of Imperial College. Use, disclosure,
C reproduction and transfer of this work without the express
C written consent of Imperial Collge are prohibited.
C
C (c) 1991 Oliver Smart; 
C	   Imperial College of Science, Technology and Medicine; 
C          University of London; London, United Kingdom.;
C          All Rights Reserved.	
C
C this s/r finds out what what files are opened
C to each unit whether it is formatted or not
C n.b. as it rewinds files - should only be used at the end of a run
C with care


C for output
      INTEGER 			STREAM 

C loop count 
      INTEGER			SCOUNT

C logical for file being opened
      LOGICAL			LEX

C for filename
      CHARACTER*200		FNAME
      INTEGER			ILEN

C form
      CHARACTER*11		FFORM

C line to try to read
      CHARACTER*80		LINE

C end of decs ******************

C go through streams
      DO 10 SCOUNT = 0, 100

C do not try input or output streams
        IF ( (SCOUNT.EQ.0).OR.(SCOUNT.EQ.5).OR.(SCOUNT.EQ.6)) GOTO 10

C is a file open to this stream?
        INQUIRE( SCOUNT, OPENED= LEX)

        IF (LEX) THEN
C find name
          INQUIRE( SCOUNT, NAME= FNAME)
          CALL CHREND( FNAME, ILEN)
C find formatted or unformatted
          INQUIRE( SCOUNT, FORM= FFORM)

C tell user
          WRITE( STREAM, '(A,I3,A)')
     &' (File open on unit:', SCOUNT, ' name: '//FNAME(1:ILEN)//
     &'  form: '//FFORM//')'
C try to read from file
          IF (FFORM(1:1).EQ.'F') THEN
C tell user line read from file
            READ( SCOUNT, '(A)', END= 20, ERR= 30) LINE 
            CALL CHREND( LINE, ILEN)
            WRITE( STREAM, '(A)')
     &'   (Next line read from file: '//LINE(1:ILEN)//')'
            GOTO 10
C at end of file
20	    WRITE( STREAM, '(A)')
     &'   (At End Of File)'
            GOTO 10
C error reading from this line
30	    WRITE( STREAM, '(A)')
     &'   (Error reading from this file)'
            GOTO 10
          ENDIF

	ENDIF
10    CONTINUE
        
      RETURN
      END
