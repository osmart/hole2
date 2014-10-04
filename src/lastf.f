      SUBROUTINE LASTF( FNAME, FTYPE)
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
C This s/r returns the filename of the last file in directory
C whose name contains string ftype.  If no file is found 
C returns 'none'.
C
C n.b. This routine is specific to unix operating systems -
C      uses s/r system.
      CHARACTER*(*)		FNAME, FTYPE

C stream number for file
      INTEGER			ISTR

C line for read
      CHARACTER*80		LINE80

C end of decs ******************

C default filename
      FNAME = 'none'

C do a listing of directory by time to file:
C (assumed to be one filename per line)
      CALL SYSTEM('ls -t > sr_lastf_tempfile')
C open the file
      CALL STREAM( ISTR)
C if we cannot open file return
      OPEN( ISTR, FILE= 'sr_lastf_tempfile', STATUS= 'OLD', ERR= 55555)
C read it line by line
10    CONTINUE
        READ( ISTR, '(A)', ERR=55, END= 55) LINE80
        IF (INDEX(LINE80, FTYPE).NE.0) THEN
          FNAME = LINE80
          GOTO 55
        ENDIF
C read next line 
      GOTO 10
C successful return
55    CONTINUE
C delete temporary file
      CLOSE( ISTR, STATUS= 'DELETE')
C cannot open file return
55555 RETURN
      END
