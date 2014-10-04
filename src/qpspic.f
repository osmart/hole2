      SUBROUTINE QPSPIC( NIN, NOUT, SOUT, 
     &         MAXST, MAXCOL, ISTORE, RSTORE,
     &         MAXTXT, MAXLEN, TXTNO, TXTLEN, 
     &         TXTPOS, TXTDIR, TXTSTR, PSIZE,
     &         SGREY, LWIDTH, XMIN, XMAX, YMIN, YMAX,
     &         SCALE, XPSCEN, YPSCEN, HWMANY, DOTAT, 
     &         ICOL, HSBCOL, LABORT)
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
C this s/r writes out the complete picture - 
C move/draw records & text strings.
C The picture centre {(xmin+xmax)/2, (ymin+ymax)/2} in angstroms
C is placed on postscipt co-ords {XPSCEN, YPSCEN}

C May 93 Major modifications:
C (i)   addition of colour option
C (ii)  depth cueing implemented by a simple depth sort method:
C       lines, dots and text string are written with lowest z
C       first.  For lines z co-ord taken to be its centre point.
C       Procedure should work ok  except for closly spaced lines -
C       if this is a problem then maybe could split lines up into
C       smaller sections.  
C (iii) Making the centre space of the box option white so that
C       depth cueing would be more effective.

C passed variables

C user input/output streams returned unchanged
      INTEGER                   NIN, NOUT

C output file already opened to stream sout
C return unchanged
      INTEGER                   SOUT

C the store for move draws RETURN UNCHANGED
C the maximum number of colours is stored in MAXCOL
C MAXCOL is assigned a value in qplot and MUST not be changed
C MAXST - is the maximum number of move draws to be stored
C         this is set a parameter in qplot and MUST not be changed
C istore( 1 to MAXCOL) is returned with the number of move
C    draws read for each colour
C rstore is the store for moves/draws 
      INTEGER                   MAXST
      INTEGER                   MAXCOL
      INTEGER                   ISTORE(MAXCOL)
      REAL                      RSTORE( MAXST, 0:3, MAXCOL)

C store for text strings
C n.b. return all unchanged
C txtno             the number of strings stored
C txtlen(*)         the length of string *
C txtpos(1 to 3, *) the position of its anchor point in angs space
C txtdir(*)         an integer to indicate where the string should be
C                   placed on page 1 indicates below to the left
C                                  2 ............... centred etc.
C                   key 789
C                       456 (like a numeric keypad)
C                       123
C txtstr(*)         the string to be written
C the maximum number of strings
      INTEGER                   MAXTXT
C the maximum numbers of characters in each text string
      INTEGER                   MAXLEN
      INTEGER                   TXTNO
      INTEGER                   TXTLEN( MAXTXT)
      REAL                      TXTPOS( 3, MAXTXT)
      INTEGER                   TXTDIR( MAXTXT)
      CHARACTER*1               TXTSTR( MAXLEN, MAXTXT)
C point size of text n.b. postscript specification 
C already done before this routine called 
      REAL                      PSIZE

C store for line widths/ grey levels
C return unchanged
      REAL                      LWIDTH(MAXCOL), SGREY(MAXCOL)
C a control on line type
C 1: normal lines
C 2: open box
C 3: closed box
      INTEGER                   HWMANY(MAXCOL)

C error condition indicator - if there is a fatal problem
C return true
      LOGICAL                    LABORT

C maximum & minimum x co-ord & y-co-ord in angs
C the picture centre should be at midpoints (return unchanged)
      REAL                      XMAX, XMIN, YMAX, YMIN

C postscript picture centre and scale to convert from angs
C to poscript co-ords - return unchanged
      REAL                      XPSCEN, YPSCEN, SCALE

C Vble which passes on the information whether
C there are "dot at" records in the file:
C -55555     means dots are processed into 3d crosses in s/r qreadi
C -ve no     dots should be processed into open circles (white centre)
C 0.0        no dots in file
C +ve number the point size for circles to which dots processed
C n.b. only deal with circle option here - crosses done in s/r qreadi
      REAL                       DOTAT

C 5/93 now include colour output option 
C if ICOL = 0 then no colour
C         = 1 then colour on white background
C         = 2 then colour on a black background
      INTEGER                   ICOL
C colour for colour! Use HSB colour definition ie
C hue from   0 to 360
C saturation 1 full colour 0 white.
C brightness 1 full colour 0 black
      REAL                      HSBCOL(3,MAXCOL)

C vbles internal to this routine

C May 93 store to do depth sorting
C maximum number of items in depth store
      INTEGER                   DSTMAX
      PARAMETER(                DSTMAX = 30000)
C the number of items in the store
      INTEGER                   DSTNO
C z co-ordinate of the item 
      REAL                      DSTZ( DSTMAX)
C the item in store
C first number is the list number - 
C   for a line the number of the drawto
C second number the colour of the record
C   minus one number indicates text string
      INTEGER                   DSTINF( 2, DSTMAX)

C loop counts - for colours; move/draws; within a record
      INTEGER                   CCOUNT, MCOUNT

C co-ordinates in postscript space
      REAL                      PSCX, PSCY 

C the last position of a move/draw to
      REAL                      PREVX, PREVY

C April 1993 bug found with large files have to issue
C a S command after each few thousand move to drawtos
C do a S before the first M found after each 1000 records
C keep count with istrok
      INTEGER                   ISTROK

C colour of current record, previous, record number of this record
      INTEGER			NEWCOL, OLDCOL, RECORD

C end of declarations **********

C      write(nout,*) 'debug - call to qpspic xpscen= ', xpscen


C load up depth sorting array.
C initialize the store
      DSTNO = 0
C first do draws and dots
C go thru colours
      DO 11 CCOUNT= 1, MAXCOL  
C go thru records for colour ccount
	DO 21 MCOUNT = 1, ISTORE(CCOUNT)
C What is this record?
	  IF ( (RSTORE( MCOUNT, 0, CCOUNT).EQ.3.) .OR. 
     &         (RSTORE( MCOUNT, 0, CCOUNT).EQ.4.)     ) THEN
C "draw to" or "dotat" record   
C one more to store
	    DSTNO = DSTNO + 1
	    IF (DSTNO.GT.DSTMAX) THEN
	      WRITE( NOUT, '(A)')
     &' *** ERROR ***'//CHAR(7),
     &' Depth sort array bound limit DSTMAX exceeded',
     &' in s/r qpspic - increase recompile and try again!'
	      LABORT = .TRUE.
	      GOTO 55555
	    ENDIF
C load up entry
	    DSTINF( 1, DSTNO) = MCOUNT
	    DSTINF( 2, DSTNO) = CCOUNT
C dot at record has its own z, line z of its centre point
	    IF (RSTORE( MCOUNT, 0, CCOUNT).EQ.4.) THEN
	      DSTZ( DSTNO) = RSTORE( MCOUNT, 3, CCOUNT)
	    ELSE
	      DSTZ( DSTNO) = 0.5*(RSTORE( MCOUNT,   3, CCOUNT) +
     &                            RSTORE( MCOUNT-1, 3, CCOUNT)  )
	    ENDIF
	  ENDIF
21      CONTINUE
11    CONTINUE
C now dots and lines have been do lets do text records
      DO 31 MCOUNT= 1, TXTNO
C one more to store
	DSTNO = DSTNO + 1
	IF (DSTNO.GT.DSTMAX) THEN
	  WRITE( NOUT, '(A)')
     &' *** ERROR ***'//CHAR(7),
     &' Depth sort array bound limit DSTMAX exceeded',
     &' in s/r qpspic - increase recompile and try again!'
	  LABORT = .TRUE.
	  GOTO 55555
	ENDIF
	DSTINF( 1, DSTNO) = MCOUNT
C minus one indicates a text record
	DSTINF( 2, DSTNO) = -1
	DSTZ( DSTNO) = TXTPOS( 3, MCOUNT)
31    CONTINUE

C      write( nout, '(//a)' ) 'debug pre depth sort array: '
C      do 567 mcount = 1, dstno
C567     write(nout,*) dstz(mcount),dstinf(1,mcount),dstinf(2,mcount)


C sort the records lowest dstz first
C use heap sort routine modified from Numerical Recipes
      CALL QHPS( DSTNO, DSTZ, DSTINF)

C      write( nout, '(//a)' ) 'debug post depth sort array: '
C      do 568 mcount = 1, dstno
C568     write(nout,*) dstz(mcount),dstinf(1,mcount),dstinf(2,mcount)

C now lets output the sorted records
      DO 10 MCOUNT = 1, DSTNO
C load up data item
	NEWCOL = DSTINF(2,MCOUNT)
	RECORD = DSTINF(1,MCOUNT)
C a text record?
	IF (NEWCOL.EQ.-1) THEN
C make sure characters are black - unless we have black
C background when we want white
	  IF (ICOL.EQ.2) THEN
	    WRITE( SOUT, '(A)') 'GS 1 G % text record'
	  ELSE
	    WRITE( SOUT, '(A)') 'GS 0 G % text record'
	  ENDIF
C transform x & y records from angstroms to post script coord space
	  PSCX = TXTPOS(1,RECORD) - (XMAX+XMIN)/2. 
	  PSCY = TXTPOS(2,RECORD) - (YMAX+YMIN)/2. 
	  PSCX = SCALE*PSCX + XPSCEN
	  PSCY = SCALE*PSCY + YPSCEN
C write the string specified using s/r textat
	  CALL TEXTAT( SOUT, PSIZE, 
     &        TXTSTR(1,RECORD), TXTLEN( RECORD), 
     &        PSCX, PSCY, TXTDIR( RECORD))
C restore graphics state
	  WRITE( SOUT, '(A)') 'R % end text rec'

	ELSEIF (RSTORE(RECORD,0,NEWCOL).EQ.3.) THEN
C line to
C is this colour different from the previous      
	  IF (NEWCOL.NE.OLDCOL) THEN
C process previous colour (hope this wont matter on 1st)
	    WRITE( SOUT, '(A)') 'S'
C write out this colours greyness and width
C 5/93 Colour instead of greyness
	    IF (ICOL.EQ.0) THEN 
	      WRITE( SOUT, '(F5.3,A)') SGREY(NEWCOL), ' G'
	    ELSE
C we store hue 360 times ps definition - ala quanta
	      WRITE( SOUT, '(3F7.4,A)') 
     &          HSBCOL(1,NEWCOL)/360., HSBCOL(2,NEWCOL), 
     &          HSBCOL(3,NEWCOL), ' H'
	    ENDIF

C set linewidth except if doing boxes
	    IF ((HWMANY(NEWCOL).NE.2).AND.(HWMANY(NEWCOL).NE.3)) THEN
	      WRITE( SOUT, '(F6.3,A)') LWIDTH(NEWCOL), ' W'
c	    ELSE
C want a coloured line with a thin surround either black (white background)
C or white (black background)
c              IF (ICOL.EQ.2) THEN
c                WRITE( SOUT, '(A)') '1 G'
c              ELSE
c                WRITE( SOUT, '(A)') '0 G'
c              ENDIF
c	      WRITE( SOUT, '(A)') '.25 W'
	    ENDIF
	  
C store colour 
	    OLDCOL = NEWCOL

C April 1993 bug found with large files have to issue
C a S command after each few thousand move to drawtos
C do a S before the first M found after each 1000 records
C keep count with istrok
	    ISTROK = 1000
	  
	  ENDIF
C Have setup new colours linewidth and greyness/colour.
C draw line          

C M previous position
C convert angstrom units to postscript
C first displace so that {(xmax+xmin)/2,(ymax+ymin)/2} -> {0,0}
	  PREVX = RSTORE(RECORD-1, 1, NEWCOL)-(XMAX+XMIN)/2.
	  PREVY = RSTORE(RECORD-1, 2, NEWCOL)-(YMAX+YMIN)/2.
C now scale & displace picture centre
	  PREVX = SCALE*PREVX + XPSCEN
	  PREVY = SCALE*PREVY + YPSCEN
C now for other end
	  PSCX = RSTORE(RECORD, 1, NEWCOL)-(XMAX+XMIN)/2.
	  PSCY = RSTORE(RECORD, 2, NEWCOL)-(YMAX+YMIN)/2.
	  PSCX = SCALE*PSCX + XPSCEN
	  PSCY = SCALE*PSCY + YPSCEN

C if not drawing boxes
	  IF ((HWMANY(NEWCOL).NE.2).AND.(HWMANY(NEWCOL).NE.3)) THEN
C write record
	    WRITE( SOUT, '(2(2F8.2,A))') 
     &        PREVX, PREVY, ' M ', PSCX, PSCY, ' L'
	  ELSE
C box drawing dealt with in seperate s/r
	    CALL PSBOX( SOUT, PREVX, PREVY, PSCX, PSCY, 
     &                  HWMANY(NEWCOL), LWIDTH(NEWCOL))
	    ISTROK = ISTROK - 3
	  ENDIF
C issue a S every 1000 or so lines * 4 for a box
	  ISTROK = ISTROK - 1
	  IF (ISTROK.LT.0) THEN
	    ISTROK = 1000
	    WRITE( SOUT, '(A)') 'S'
	  ENDIF
	
	ELSEIF (RSTORE(RECORD,0,NEWCOL).EQ.4.) THEN
C dot at record

C new colour?
          IF (NEWCOL.NE.OLDCOL) THEN
C process previous colour 
            WRITE( SOUT, '(A)') 'S'
C write out this colours greyness and width
            IF (ICOL.EQ.0) THEN
              WRITE( SOUT, '(F5.3,A)') SGREY(NEWCOL), ' G'
            ELSE
C we store hue 360 times ps definition - ala quanta
              WRITE( SOUT, '(3F7.4,A)')
     &          HSBCOL(1,NEWCOL)/360., HSBCOL(2,NEWCOL),
     &          HSBCOL(3,NEWCOL), ' H'
            ENDIF
C set line width if doing open circles
            IF (DOTAT.LT.0.) 
     &        WRITE( SOUT, '(F6.3,A)') LWIDTH(NEWCOL), ' W'
C store colour
            OLDCOL = NEWCOL

          ENDIF

C setup ps co-ords
          PSCX = RSTORE(RECORD, 1, NEWCOL)-(XMAX+XMIN)/2.
          PSCY = RSTORE(RECORD, 2, NEWCOL)-(YMAX+YMIN)/2.
          PSCX = SCALE*PSCX + XPSCEN
          PSCY = SCALE*PSCY + YPSCEN
C Fed circle
          IF (DOTAT.GT.0) THEN
	    WRITE( SOUT, '(A,3F8.2,A)') 
     &'N ', PSCX, PSCY, DOTAT, 
     &   ' 0 360 arc C F % closed circle'

          ELSE
C open circle
C Nov 95 change so that if doing an open circle 
C draw circle of same colour/greyness as line colour with
C a thin black surround
	    WRITE( SOUT, '(A,3F8.2, A)') 
     &' N ', PSCX, PSCY, -DOTAT, 
     &   ' 0 360 arc C F%ocirc'
C then the arc in black
            WRITE( SOUT, '(A,3F8.2,A)')
     & 'GS 0.25 W 0 G ', PSCX, PSCY, -DOTAT,
     & ' 0 360 arc S R'
          ENDIF
	
	ELSE
C unrecognized option coding error
	  STOP 'CODING ERROR IN S/R QPSPIC.F unrecog depth store item'

	ENDIF

10    CONTINUE

C issue S so that last line records processed
      WRITE( SOUT, '(A)') 'S'

C return here
55555 CONTINUE
      RETURN
      END
C
      SUBROUTINE PSBOX( SOUT, PSX1, PSY1, PSX2, PSY2, LTYPE, LWIDTH)
      IMPLICIT NONE
C This s/r write postscript command to the file open on stream sout
C which draw a box of width lwidth point centered on the line
C from (psx1,psy1) to (psx2,psy2).
C If the integer LTYPE is set to 2 only 2 line sections parallel
C to the line 1 -> 2 will be drawn; otherwise 4 lines to be output.

C passed vbles (see above for explanation)
      INTEGER                   SOUT, LTYPE
      REAL                      PSX1, PSY1, PSX2, PSY2, LWIDTH

C vbles internal to this routine

C the displacement required for 1/2 side of box
      REAL                      DELTAX, DELTAY

C end of decs ******************

C form vector at right angles to the line 1->2
      DELTAX = -(PSY2 - PSY1)
      DELTAY =   PSX2 - PSX1

C unit this
      IF ( (ABS(DELTAX).GT.0) .OR. (ABS(DELTAY).GT.0)) THEN
	DELTAX = DELTAX/SQRT( (PSX2-PSX1)**2 + (PSY2-PSY1)**2)
	DELTAY = DELTAY/SQRT( (PSX2-PSX1)**2 + (PSY2-PSY1)**2)
      ENDIF

C make lwidth/2. wide
      DELTAX = DELTAX*0.5*LWIDTH
      DELTAY = DELTAY*0.5*LWIDTH

C May 93 implementing simple depth cueing 
C Therefore want this option to draw white box with line
C around it - rather than see thru box as before
C Nov 95 change so that if doing an open circle 
C draw circle of same colour/greyness as line colour with
C a thin black surround - no longer white box
      WRITE( SOUT, '(A,4(2F8.2,A))') 
     &'N',
     &	PSX1+DELTAX, PSY1+DELTAY, ' M',
     &  PSX2+DELTAX, PSY2+DELTAY, ' L',
     &	PSX2-DELTAX, PSY2-DELTAY, ' L',
     &  PSX1-DELTAX, PSY1-DELTAY, ' L'
C F the shape then change to black
      WRITE( SOUT, '(A)') 
     & 'C F GS 0.25 W 0 G '


C start at point1 + delta
      WRITE( SOUT, '(2F8.2,A)') PSX1+DELTAX, PSY1+DELTAY, ' M'

C draw to point2 + delta
      WRITE( SOUT, '(2F8.2,A)') PSX2+DELTAX, PSY2+DELTAY, ' L'

C if we are only drawing 2 line M next point
      IF (LTYPE.EQ.2) THEN
	WRITE( SOUT, '(2F8.2,A)') 
     &    PSX2-DELTAX, PSY2-DELTAY, ' M'
      ELSE
	WRITE( SOUT, '(2F8.2,A)') 
     &    PSX2-DELTAX, PSY2-DELTAY, ' L'
      ENDIF

C draw to point1 - delta
      WRITE( SOUT, '(2F8.2,A)') PSX1-DELTAX, PSY1-DELTAY, ' L'

C move or draw to original point and process lines
      IF (LTYPE.EQ.2) THEN
	WRITE( SOUT, '(2F8.2,A)') 
     &    PSX1+DELTAX, PSY1+DELTAY, ' M S R'
      ELSE
	WRITE( SOUT, '(2(F9.3,1X),A)') 
     &    PSX1+DELTAX, PSY1+DELTAY, ' L S R'
      ENDIF

      RETURN
      END
