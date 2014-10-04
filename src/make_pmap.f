      PROGRAM MAKPOS
      IMPLICIT NONE
C ********************************************************************
C *                                                                  *
C * This software is an unpublished work containing confidential and *
C * proprietary information of Birkbeck College. Use, disclosure,    *
C * reproduction and transfer of this work without the express       *
C * written consent of Birkbeck College are prohibited. This notice  *
C * must be attached to all copies or extracts of the software.      *
C *                                                                  *
C * (c) 1996 Oliver Smart & Birkbeck College,                        *
C *                                                                  *
C ********************************************************************
C
C Modification history:
C
C Date  Author          Modification
C 11/96 O.S. Smart        Original version
C 28/02/97 O.S.S.	Release HOLE2 beta001
C
C Reads a special file produced by HOLE to
C produce a pretty residue number/ chain id post map in surfer

C screen, keyboard
      INTEGER                   NIN
      PARAMETER(                NIN = 5)
      INTEGER                   NOUT
      PARAMETER(                NOUT= 6)
C input/output file stream
      INTEGER                   SIN, SOUT

C filename
      CHARACTER*200             FNAME

C abort indicator
      LOGICAL                   LABORT

C array limits for the storage of the chain id/resid matrix
      INTEGER			SAXMAX, SAYMAX
      PARAMETER(			SAXMAX = 75)
      PARAMETER(			SAYMAX = 300)

C the number of elements, x and y
      INTEGER			SAXNUM, SAYNUM

C the value of real variable (angle and channel coord)
      DOUBLE PRECISION		SAXLOW, SAXHIG,
     &  				SAYLOW, SAYHIG

C the storage arrays (n.b. must preserve edge)
      INTEGER			SARNO( 0:SAXMAX+1, 0:SAYMAX+1)
      CHARACTER*1			SACHN( 0:SAXMAX+1, 0:SAYMAX+1)
      DOUBLE PRECISION		SADIS( SAXMAX, SAYMAX)


C string for read
      CHARACTER*80		LINE

C loop count for x and y, inner loop x and y
      INTEGER			XCOUNT, YCOUNT, JCOUNT, ICOUNT
C loop count for working list and direction
      INTEGER			WCOUNT, UCOUNT

C scale factor between x and y in "distance" calc.
      DOUBLE PRECISION		YSCALE

C big store i.e store for which label is the one furthest from
C a neighbouring field
      DOUBLE PRECISION		BIGX, BIGY, BIGVAL

C vble for distance calc
      DOUBLE PRECISION		DIST

C test point
      DOUBLE PRECISION		TESTP(2)

C working list for connecting residues
C array bound limit
      INTEGER			WNMAX
      PARAMETER(		WNMAX = 2000)
      INTEGER			WORKXY(2,WNMAX), NEWPXY(2,WNMAX)
      INTEGER			WORKNO, NEWPNO

C end of decs ******************

C write greeting
      WRITE( NOUT, '(A)') 
     &' *** Program make_post_map ***',
     &' ',
     &' Copyright 1997 by Oliver Smart and Birkbeck College',
     &' Copyright 2004 by Oliver Smart ',
     &' Program modification number 2.2 001'

C write link time of program to screen
      CALL VERTIM( NOUT)
      WRITE( NOUT, *)

C use s/r lastf to find latest file of
C type in the directory.  N.b. only works on unix machines
      CALL LASTF( FNAME, '.hole')
      IF (FNAME(1:4).EQ.'none') FNAME = 'input'

C get input filename
      LABORT = .TRUE.
C (input stream, output, oldfile?, file_stream, file_type, name,
C  allow abort?, default extension)
C N.B. as file_type includes 'BINARY' then will open as binary
      CALL INTERF( NIN, NOUT, .TRUE., SIN, 
     &  'input hole format', FNAME, LABORT, '.hole')
      IF (LABORT) GOTO 55555

C get output filename
      LABORT = .TRUE.
      CALL INTERF( NIN, NOUT, .FALSE., SOUT, 
     &  'output SURFER posting format ', FNAME, LABORT, '.dat')
      IF (LABORT) GOTO 55555

C write header in output file
      WRITE( SOUT, '(A,A)')
     & '"Ca angle" "Ca disp" "labRNO"',
     &  ' "labCHN" "labRNOCHN" "labelRNO:CHN" "size-of-patch"'

C read input
C first blank lines
      READ( SIN, '(A)') LINE

C read array sizes
      READ( SIN, '(BN,2I10)') SAXNUM, SAYNUM

C check array sizes 
      IF (SAXNUM.GT.SAXMAX) THEN
        WRITE( NOUT, '(A/ A,I5/ A,I5,A/ A)') 
     &' Error in make_post_map '//CHAR(7),
     &'   Array bound SAXMAX exceeded  increase from ', SAXMAX, 
     &'   to above ', SAXNUM, ' recompile and link etc. ',
     &' Cannot proceed********************'
        GOTO 55555
      ENDIF
      IF (SAYNUM.GT.SAYMAX) THEN
        WRITE( NOUT, '(A/ A,I5/ A,I5,A/ A)') 
     &' Error in make_post_map '//CHAR(7),
     &'   Array bound SAYMAX exceeded  increase from ', SAYMAX, 
     &'   to above ', SAYNUM, ' recompile and link etc. ',
     &' Cannot proceed********************'
        GOTO 55555
      ENDIF

C read x and y limits
      READ( SIN, '(BN,2F6.1)') SAXLOW, SAXHIG
      READ( SIN, '(BN,2F12.5)') SAYLOW, SAYHIG

C tell user info
      WRITE( NOUT, '(/A,2(I6,A)/ 2(A,2F10.3:/))')
     &' input file has info for', SAXNUM, ' rows and', 
     &				SAYNUM, ' columns ',
     &' Data limits X= ', SAXLOW, SAXHIG,
     &' Data limits Y= ', SAYLOW, SAYHIG

C read in the data - go thru rows
      DO 10 YCOUNT = 1, SAYNUM
C now go thru columns
C zeroth element set to be default - different to all others
        SACHN(0,YCOUNT) = ']'
        SARNO(0,YCOUNT) = -7676
        DO 20 XCOUNT = 1, SAXNUM
C read element from input file
          READ( SIN, '(A1,I6)', END= 900, ERR=900) 
     &      SACHN(XCOUNT,YCOUNT), SARNO(XCOUNT,YCOUNT)
C end of read for column XCOUNT
20      CONTINUE
C last element set to be default - different to all others
        SACHN(SAXNUM+1,YCOUNT) = ']'
        SARNO(SAXNUM+1,YCOUNT) = -7676
C end of read for row YCOUNT
10    CONTINUE

C set upper and lower edge to different values
      DO 30 XCOUNT = 0, SAXNUM+1
        SACHN(XCOUNT,0)        = ']'
        SARNO(XCOUNT,0)        = -7676
        SACHN(XCOUNT,SAYNUM+1) = ']'
        SARNO(XCOUNT,SAYNUM+1) = -7676
30    CONTINUE

C everthing set up - shut input file
      CLOSE( SIN)

C work out scale factor between y and xcoords -
C assume final map will be square (could get scale
C factor from user but probably not worth it)
       YSCALE = FLOAT(SAXNUM)/FLOAT(SAYNUM)

C work out the closest distance matrix
      DO 40 YCOUNT = 1, SAYNUM
        DO 50 XCOUNT = 1, SAXNUM
C need to find element sadis(xcount,ycount)
C the distance of the closest element of the matrix which is
C different from this residue
C Q: do we have to be sophisticated - i.e. is it o.k. just to work out all
C distances - n**4 calc? Or do we need to be clever?
C takes about 30 sec cpu - not too bad
           SADIS(XCOUNT,YCOUNT) = 1E10
C must include edges in consideration
           DO 60 ICOUNT = 0, SAYNUM+1
             DO 70 JCOUNT = 0, SAXNUM+1
C is the element the same as (xcount,ycount)?
                IF ((SACHN(JCOUNT,ICOUNT).EQ.SACHN(XCOUNT,YCOUNT)).AND.
     &              (SARNO(JCOUNT,ICOUNT).EQ.SARNO(XCOUNT,YCOUNT)) 
     &		    					         ) THEN
C yes - do nothing
                  CONTINUE
                ELSE
C no - ask the question is it the closest different element
C work out distance between element (xcount,ycount) and (jcount,icount)
C DO NOT DO SQRT - no need just compare squares
                  DIST = (XCOUNT-JCOUNT)**2 +
     &                   (YSCALE*(YCOUNT-ICOUNT))**2
C smallest yet?
                  IF (DIST.LT.SADIS(XCOUNT,YCOUNT)) 
     &              SADIS(XCOUNT,YCOUNT) = DIST
                ENDIF
70           CONTINUE
60         CONTINUE
C have found element sadis(xcount,ycount) - look for next!

50      CONTINUE
40    CONTINUE
C have filled SADIS

C will jump back here after each label written
8989  CONTINUE

C find biggest element in sadis
        BIGVAL = -1e10
        DO 140 YCOUNT = 1, SAYNUM
          DO 150 XCOUNT = 1, SAXNUM      
            IF (SADIS(XCOUNT,YCOUNT).GT.BIGVAL) THEN
              BIGVAL = SADIS(XCOUNT,YCOUNT)
              BIGX = XCOUNT
              BIGY = YCOUNT
            ENDIF
150       CONTINUE
140     CONTINUE

C have we labelled all patches?
        IF (BIGVAL.LT.0) GOTO 55555

C for current matrix BIGVAL is the biggest value label - write it out
        WRITE( SOUT, '(2F8.3,I6,A1,A1, I6,A1, I6,A1,A1, F8.2)') 
     &    SAXLOW + ((SAXHIG-SAXLOW)*BIGX)/FLOAT(SAXNUM),
     &    SAYLOW + ((SAYHIG-SAYLOW)*BIGY)/FLOAT(SAYNUM),
     &    SARNO(BIGX,BIGY), ' ', SACHN(BIGX,BIGY),
     &    SARNO(BIGX,BIGY),  SACHN(BIGX,BIGY),
     &    SARNO(BIGX,BIGY), ':', SACHN(BIGX,BIGY), SQRT(BIGVAL)

C o.k. we have written a label for a particular - must eliminate
C whole connected patch from consideration
C first cancel the point itself
        SADIS( BIGX, BIGY) = -1e10
C
C use routine - have "working points" which we have to examine all 4 points
C which surround (above, below, left and right) to see whether
C these (a) have the same resid as bigx, bigy point AND
C       (b) have not already been canceled
C when a point is canceled should be added to  "new points"
C at the end of a cycle "new points" overwrite "working points"
C and we repeat until no "new points" are found
        WORKNO = 1
        WORKXY(1,1) = BIGX
        WORKXY(2,1) = BIGY

C do until workno=0
345     CONTINUE
        IF (WORKNO.EQ.0) THEN
          CONTINUE

        ELSE

C have some points to work on
C initialize new arrays
          NEWPNO =0
C          write(*,*) ' number of working points ', workno

C go thru the working list
          DO 880 WCOUNT = 1, WORKNO
C look above below, left and right of the working point
            DO 890 UCOUNT = 1, 4
               IF     (UCOUNT.EQ.1) THEN
C up
                 TESTP(1) = WORKXY(1,WCOUNT)
                 TESTP(2) = WORKXY(2,WCOUNT) + 1
               ELSEIF (UCOUNT.EQ.2) THEN
C down
                 TESTP(1) = WORKXY(1,WCOUNT)
                 TESTP(2) = WORKXY(2,WCOUNT) - 1
               ELSEIF (UCOUNT.EQ.3) THEN
C right
                 TESTP(1) = WORKXY(1,WCOUNT) + 1
                 TESTP(2) = WORKXY(2,WCOUNT) 
               ELSE
C left
                 TESTP(1) = WORKXY(1,WCOUNT) - 1
                 TESTP(2) = WORKXY(2,WCOUNT) 
               ENDIF
C look above the current point i.e. at point workxy(1,wcount), workxy(2,wcount)+1
C is it already cancelled? 
               IF (SADIS(TESTP(1),TESTP(2)) .GT. 0) THEN
C no!
C does it have the relevant id?
                 IF ( (SARNO(TESTP(1),TESTP(2)).EQ.SARNO(BIGX,BIGY)) 
     &                                                          .AND.
     &                (SACHN(TESTP(1),TESTP(2)).EQ.SACHN(BIGX,BIGY))  
     &						       	       ) THEN
C has the relevant id so (a) cancel it and 
C (b) add it to the new-points list
                   SADIS(TESTP(1),TESTP(2)) =  -1e10
C put a check on array bound here
                   NEWPNO = NEWPNO + 1
                   NEWPXY(1,NEWPNO) = TESTP(1)
                   NEWPXY(2,NEWPNO) = TESTP(2)
                 ENDIF  
		   ENDIF
890         CONTINUE
880       CONTINUE

C have gone through the working list - now copy new-point list to the working
          WORKNO = NEWPNO
          DO 770 WCOUNT = 1, WORKNO
             WORKXY(1,WCOUNT) = NEWPXY(1,WCOUNT)
             WORKXY(2,WCOUNT) = NEWPXY(2,WCOUNT)
770       CONTINUE

C do until workno=0 jump up to 345
          GOTO 345
        ENDIF

C do until no more labels
      GOTO 8989


55555 CONTINUE
      WRITE( NOUT, '(A)') 'STOP - make_post_map normal completion.'
      CALL EXIT(0)

C error statements
900   CONTINUE
        WRITE( NOUT, '(2(A,I5)/A)')
     &' Error reading elements row', XCOUNT, '  column', YCOUNT,
     &' Aborting**************'//CHAR(7)
      GOTO 55555

      END
