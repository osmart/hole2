      SUBROUTINE NEWOP( NOUT, LERR, SHORTO,
     &                  FNAME, ISTR, LBIN)
      IMPLICIT NONE
C ********************************************************************
C *                                                                  *
C * This software is an unpublished work containing confidential and *
C * proprietary information of Oliver Smart. Use, disclosure,    *
C * reproduction and transfer of this work without the express       *
C * written consent of Oliver Smart are prohibited. This notice  *
C * must be attached to all copies or extracts of the software.      *
C *                                                                  *
C * (c) 2000 Oliver Smart *
C *                                                                  *
C ********************************************************************
C
C Modification history:
C
C Date  Author          Modification
C 01/00 O.S. Smart      First version
C
C this s/r opens a new file name fname to a stream istr
C if the file already exists then it is renamed to .old

C output stream no (to user)
C (return unchanged)
      INTEGER                   NOUT


C if an error found is found in s/r - set lerr true and program will stop, 
      LOGICAL                   LERR

C  variable to reduce text output -
C if oshort =
C 0: full text output
C 1: no in run progress reported
C 2: ditto plus no graph type output - only leaving
C    Minimum radius & conductance calculations.
C 3: All text output other than input card mirroring
C    and error messages turned off
C (return unchanged)
      INTEGER                   SHORTO

C filename - may change if it either has (a) leading blanks or (b) a tilda to
C resolve.
      CHARACTER*200		FNAME

C stream to open to
      INTEGER			ISTR

C file is binary or ascii
      LOGICAL			LBIN


C internals ********************

C logical fn to
C to open files read only, same for readonly & binary
      LOGICAL				OPENRO, OPENRB, EITHER

C new name - unix command
      CHARACTER*200			FNEW
      CHARACTER*403			UCOMM

C last character of the string
      INTEGER				IEND, IEND2

C end of decs ******************

C strips strips off any leading blanks or tabs from the filename
      CALL STRIPS( FNAME)
C resolve any tilda's (~) in filename
      CALL DTILDA( FNAME)

C a binary file?
      IF (LBIN) THEN
C try to open readonly - either binary or ascii
        EITHER = OPENRB( ISTR, FNAME, NOUT)        
      ELSE
        EITHER = OPENRO( ISTR, FNAME, NOUT)        
      ENDIF

C Have we succeed in opening 
      IF (EITHER) THEN
C can open - so close
	CLOSE( ISTR)
C have to rename
        FNEW = FNAME
        CALL CHREND( FNEW, IEND)
	FNEW = FNEW(1:IEND)//'.old'
	UCOMM = 'mv '//FNAME(1:IEND)//' '//FNEW(1:IEND+4)
	CALL CHREND( UCOMM, IEND2)
C the mv command
	CALL SYSTEM( UCOMM(1:IEND2))
C warn user what we have done
	IF (SHORTO.LT.3) WRITE( NOUT,'(A,A,A)') 
     &' Warning have tried to open file: ''',FNAME(1:IEND), 
     &                                 ''' as NEW but',
     &'         this file already exists - old version kept as ''',
     &                                    FNEW(1:IEND+4), ''''
    
      ENDIF
C try to open file as new - if failure then silently return 
C but with error flag
      LERR = .TRUE.
      CALL STREAM( ISTR)
      IF (LBIN) THEN
        OPEN( ISTR, FILE= FNAME, STATUS= 'NEW',
     &  	  FORM= 'UNFORMATTED', ERR= 55555)
      ELSE
        OPEN( ISTR, FILE= FNAME, STATUS= 'NEW', ERR= 55555)
      ENDIF
      LERR = .FALSE.

55555 RETURN
      END
C test program for this routine:
C      logical                   lerr
C      character*200		fname
C      integer			istr
C      fname = 'temp.txt'	
C      call newop( 6, lerr, 0,
C     &            fname, istr, .false.)
C      end
