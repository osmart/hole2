      SUBROUTINE QPSWR( NIN, NOUT, SOUT, MAXST, MAXCOL, ISTORE, RSTORE,
     &  MAXTXT, MAXLEN, TXTNO, TXTLEN, TXTPOS, TXTDIR, TXTSTR, LABORT,
     &  DOTAT, SWITCH)
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
C this s/r writes the postscript output of the move draw
C records & text anchor points have already been rotated 
C before s/r call - just have to plot
C 
C May 93 adapted to be able to produce colour postscript output
C        and to do simple depth queueing by a depth-sort method.

C passed variables

C user input/output streams returned unchanged
      INTEGER                   NIN, NOUT

C output file already opened to stream sout
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
C for some reason character*(*) seems to fail here?
      CHARACTER*1               TXTSTR( MAXLEN, MAXTXT)

C an abort indicator
C if we find a problem in this s/r return true
      LOGICAL                   LABORT
C Vble which passes on the information whether
C there are "dot at" records in the file:
C -55555     means dots are processed into 3d crosses in s/r qreadi
C -ve no     dots should be processed into open circles (white centre)
C 0.0        no dots in file
C +ve number the point size for circles to which dots processed
C n.b. only deal with circle option here - crosses done in s/r qreadi
      REAL                       DOTAT

C variables internal to this routine 
C character for input, vble to state the degree
C of difficulty to be applied to the program
      CHARACTER *1              COM1, SWITCH
C line for input
      CHARACTER*80              LINE80

C loop counts for colour, for move/draw records
      INTEGER                   CCOUNT, MCOUNT

C LWIDTH is an array which stores the line widths
C specified by the user from keyboard;
      REAL                      LWIDTH(20)
C the number of lines to be drawn for 
C every colour (1,2 for single/double lines and 3 for rectangles)
      INTEGER                   HWMANY(20)
C variable SGREY is to take a new greyness from the keyboard;
      REAL                      SGREY(20)
C 5/93 now include colour output option 
C if ICOL = 0 then no colour
C         = 1 then colour on white background
C         = 2 then colour on a black background
      INTEGER                   ICOL
C colour for colour! Use HSB colour definition ie
C hue from   0 to 360
C saturation 1 full colour 0 white.
C brightness 1 full colour 0 black
C   0 1 1 red   / 0 0.5 1 pink
C  60 1 1 yellow
C 120 1 1 green
C 180 1 1 cyan
C 240 1 1 blue
C 300 1 1 magenta
C   0 0 1 white
C   0 1 0 black
      REAL                      HSBCOL(3,20)

C maximum/minimum x-coord & y-coord's after read and rotation
C these are need for scaling,
C scale to be applied angstroms -> postscript unit
      REAL                      XMAX, XMIN, YMAX
      REAL                      YMIN, ZMAX, ZMIN
      REAL                      SCALE

C pont size of font used to output text. 
C -55 used to indicate that text records are not to be output.
      REAL                      PSIZE

C angle of rotation read in degrees - stored in radians
      REAL                      AROT

C the number of pictures to be drawn 
C 1 - means a mono view
C 2 - normal stereo
C 3 - normal & cross eyed stereo
      INTEGER                   PICTNO

C 2x2 matrix for stereo transformation/ centre of rotation (1x2)
      REAL                      MAT(2,2), CENT(2)

C establishes the dimension (mm) of the picture box
      REAL                      PICBOX
C vble which tells whether the user wants a box
C around the picture or not, and a similar one
C for a title below the picture
      LOGICAL                   BOX
      LOGICAL                   TITLE
C character string to be positioned as title
      CHARACTER *80             STRING

C vbles to help the input of new min/max values
      CHARACTER *1              ARAXI(3)
      CHARACTER  *3             MIMA(2)
      REAL                      MIMAVA(3,2)
      INTEGER                   IC,JC  

C vbles introduced on orthogonal view implementation
C coords in pts for  various picture bits
      REAL			CENTRE, EDGE


C end of decs ******************

C write header of postscript file
	WRITE( SOUT, '(A)')
     &'%!PS-Adobe-3.0 EPSF-30.',
     &'%%BoundingBox: 0 0 612 792',
     &'%%Title: (output of program qplot)',
     &'%%Creationdate: (unknown)',
     &'%%EndComments',
C Nov 95 add some definitions to make file smaller
     &'/M { moveto } def',
     &'/L { lineto } def',
     &'/S { stroke } def',
     &'/W { setlinewidth } def',
     &'/G { setgray } def',
     &'/GS { gsave } def',
     &'/R { grestore } def',
     &'/F { fill } def',
     &'/C { closepath } def',
     &'/N { newpath } def',
     &'/H { sethsbcolor } def',
C make sure that we know the top of the picture
     &'GS /Helvetica findfont 6 scalefont setfont',
     &'0.5 G 20 780 M ([top]) show grestore'

C Ask user what line widths/greyness he/she wants
C each colour to be output to - replies stored in 
C sgrey & lwidth
      CALL WIDGRY( NIN, NOUT, ISTORE, MAXCOL, SGREY, LWIDTH,
     &        HWMANY, SWITCH, MAXST, RSTORE, ICOL, HSBCOL, DOTAT)

C if user is doing colour then he/she/it may want a black (that is
C not white) background
      IF (ICOL.EQ.2) THEN
	WRITE( SOUT, '(A)')
     &   '0 0 M 612 0 rlineto 0 792 rlineto -612 0 rlineto',
     &   'C F % this line and above for black box'
      ENDIF

C do you want to write a title for the picture?
      IF (SWITCH.EQ.'E') THEN 
        CALL PROMPT( NOUT,
     & 'Enter a title for the picture <no title> :')
C read reply into line - to allow default
        READ( NIN, '(A)', ERR= 55555) LINE80
C has the user specified a character?
        IF (LINE80(1:6).NE.'     ') THEN
	  READ( LINE80, '(A)', ERR= 55555) STRING
	  TITLE = .TRUE.
        ELSE
	  TITLE = .FALSE.
        ENDIF
      ELSE
        TITLE = .FALSE.
      ENDIF

C if there are any text records stored (or title) then set up font
      IF ((TXTNO.GT.0).OR. TITLE) THEN
        IF (SWITCH.EQ.'E') THEN
	  IF (TXTNO.GT.0) THEN
	    WRITE( NOUT, '(A,I5,A/ A)')
     &' Have stored ', TXTNO, ' text strings to plot',
     &' In what font do you want these to be written?',
     &' Enter ''none'' to not write records'
	  ENDIF
	  CALL PROMPT( NOUT,
     & 'What font? <Helvetica>: ')
	  READ( NIN, '(A)', ERR= 55555, END= 55555) LINE80
	  IF ((LINE80.NE.'none').AND.(LINE80.NE.'NONE')) THEN
C user wants to write records
C use default font?
	    IF (LINE80(1:6).EQ.'      ') LINE80= 'Helvetica'
C write out font 
	    WRITE( SOUT, '(A)')
     &'/'//LINE80(1:INDEX(LINE80,'    '))//'findfont'
C ask user for pointsize
	    PSIZE = 10.
60          CONTINUE
            CALL PROMPT( NOUT,
     & 'What font point size do you want? <10.>: ')
	    READ( NIN, '(A)', ERR= 55555, END= 55555) LINE80
	    IF (LINE80(1:6).NE.'      ') 
     &        READ( LINE80, '(BN,F10.0)', ERR= 60) PSIZE
C write to postscript file
	    WRITE( SOUT, '(F10.3,A)') 
     & PSIZE, ' scalefont setfont'
C no next output wanted make psize -55
	  ELSE
	    PSIZE = -55. 
	  ENDIF
C else, if 'switch' mode is "amateur" or "normal", 
C put the default values:
       ELSE
	WRITE(SOUT, '(A)')
     & '/Helvetica findfont 10 scalefont setfont'
C end of 'which switch mode'
	 PSIZE = 10.
       ENDIF
C end of 'if any text'
      ENDIF

C do you want a normal, or a 
C stereo picture?
      CALL PROMPT(NOUT,
     &'output Normal, Stereo or Orthogonal picture? (n/s/o) <normal>:')
      READ(NIN, '(A1)') LINE80(1:1)
      CALL UCASE(LINE80(1:1))
C a reply of 'S' indicates stereo
      IF (LINE80(1:1).EQ.'S') THEN
C default rotation angle
       AROT = 6.0
C default of number of pictures in stereo mode is 2
       PICTNO = 2
C if level is "expert", ask for a new angle and how many
C stereo views should be displayed;
       IF(SWITCH.EQ.'E') THEN
       CALL PROMPT(NOUT,
     & 'Enter the rotation angle to be used <6>:')
C read the reply in string LINE80
       READ( NIN, '(A)', ERR= 55555, END= 55555) LINE80
C has the user specified a number?
       IF (LINE80(1:6).NE.'     ') 
     &   READ( LINE80, '(BN,F10.0)', ERR= 55555) AROT

C if level is "expert", ask how many
C stereo views should be displayed;
C do you want two or three stereo views ?
	CALL PROMPT(NOUT,
     & 'Do you want  [2] or [3] stereo views? <2>:')
C read the reply in string LIN
	READ( NIN, '(A)', ERR= 55555, END= 55555) LINE80
C has the user specified a number?
	IF (LINE80(1:6).NE.'      ') THEN
	 READ( LINE80, '(BN,I6)', ERR= 55555) PICTNO
	 IF (PICTNO.NE.3) PICTNO = 2
	ENDIF
C end 'if expert mode'
       ENDIF 
C convert angle to radians
       AROT = AROT*(ASIN(1.)/90)

C 5 July 1996 new option orthogonal views indicate by setting pictno -1
      ELSEIF (LINE80(1:1).EQ.'O') THEN
        PICTNO = -1

C else - if not stereo, then 1 picture (a mono view) to be produced
      ELSE 
	PICTNO = 1
      ENDIF
C
C for the three pictures case, downsize the
C entire view with 2%
       IF (PICTNO.EQ.3) 
C writes in the file the scaling factor which dwindles
C the overall size by factor 0.98 (both for x & y)
     & WRITE( SOUT, '(A)')  '0.98 0.98 scale'
       

C this s/r finds the  max/ minimum of stored records
      CALL MINMAX( XMAX, XMIN, YMAX, YMIN, ZMAX, ZMIN, 
     &       RSTORE, ISTORE,  MCOUNT, CCOUNT, MAXST, MAXCOL)

C      write(nout, *) ' debug got to minmax call aft'
C  allow user to override max/min's here
      IF(SWITCH.EQ.'E') THEN   
	WRITE(NOUT,'(A)')
     &' Have calculated the following Min/Max values : '
	WRITE(NOUT,'(A,F10.3)') '        XMIN = ',XMIN
	WRITE(NOUT,'(A,F10.3)') '        XMAX = ',XMAX
	WRITE(NOUT,'(A,F10.3)') '        YMIN = ',YMIN
	WRITE(NOUT,'(A,F10.3)') '        YMAX = ',YMAX
	WRITE(NOUT,'(A,F10.3)') '        ZMIN = ',ZMIN
	WRITE(NOUT,'(A,F10.3)') '        ZMAX = ',ZMAX
	WRITE(NOUT,*)

	CALL PROMPT(NOUT,
     &'Do you want to override any of these values (y/n) <n> : ')
	READ(NIN, '(A1)') COM1
	CALL UCASE(COM1)
	IF (COM1.EQ.'Y') THEN
C store the character arrays to be displayed
C for every alteration of min/max values
	  ARAXI(1)='X'
	  ARAXI(2)='Y'
	  ARAXI(3)='Z'
	  MIMA(1)='MIN'
	  MIMA(2)='MAX'
C temporarily store the values into an array
C which simplifies the procedure
	  MIMAVA(1,1)=XMIN
	  MIMAVA(1,2)=XMAX
	  MIMAVA(2,1)=YMIN
	  MIMAVA(2,2)=YMAX
	  MIMAVA(3,1)=ZMIN
	  MIMAVA(3,2)=ZMAX

C do this loop for every axe coordinate (x,y & z)
	  DO 411 JC = 1,3
	    DO 511 IC = 1, 2  

	      WRITE (NOUT,'(A,A,A,A,F10.3,A)')
     & '        Override ',ARAXI(JC),MIMA(IC),
     &' =',MIMAVA(JC,IC),'  ?'
	      CALL PROMPT(NOUT,
     &' Enter a new number <the same>: ')
C read the reply in string LIN
	      READ( NIN, '(A)', ERR= 55555, END= 55555) LINE80
C has the user specified a number?
	      IF (LINE80(1:6).NE.'      ') 
     &          READ( LINE80, '(BN,F10.0)', ERR= 55555) MIMAVA(JC,IC)
511         CONTINUE
411       CONTINUE
C put back the values into x,y,z <min/max>
	  XMIN= MIMAVA(1,1)
	  XMAX= MIMAVA(1,2)
	  YMIN= MIMAVA(2,1)
	  YMAX= MIMAVA(2,2)
	  ZMIN= MIMAVA(3,1)
	  ZMAX= MIMAVA(3,2)
C end of overriding the min/max's
	ENDIF
C end of 'switch' test
      ENDIF
      
C scaling is to fit picture in a 
C (PICBOX * PICBOX) mm box - default for 
C PICBOX is 130mm for 1 or 2 pictures 
C and 195mm for 3 pictures
       PICBOX = 130.
       IF (PICTNO.EQ.3) PICBOX = 195.
C ask for a new picture box when a mono view is requested.     
C ie. PICTNO = 1(only if mode is  "expert")
C default for vble BOX is .true.
       BOX = .TRUE.
       IF (SWITCH.EQ.'E') THEN
	 IF(PICTNO.EQ.1) THEN
C do you want a new dimension for the picture box ?
	   WRITE(NOUT, '(A)')
     &' Do you want to change the size of the picture?'
	   CALL PROMPT(NOUT,
     & 'Enter size for the picture in mm <130.0> :')
C read the reply in string LIN80
	   READ( NIN, '(A)', ERR= 55555, END= 55555) LINE80
C has the user specified a number?
	   IF (LINE80(1:6).NE.'      ') THEN
	     READ( LINE80, '(BN,F10.0)', ERR= 55555) PICBOX
C if the picture approximates the size of a A4 page(210 mm)
C reset it to 190 mm.
	     IF (PICBOX.GT.190.0)  THEN
	       PICBOX = 190.
	       WRITE(NOUT, '(A)') 
     &' Exceeding the maximum permitted; reset it to 190mm'//CHAR(7)
	     ENDIF
	     IF (PICBOX.LT.0.0) PICBOX = 0.
	   ENDIF
	 ENDIF 
C as level is "expert", ask for a box;
C do you want a box around the picture or not ?
C default for BOX vble is true (BOX=.TRUE.)
	 CALL PROMPT( NOUT,
     & 'Do you want a box around the figure (y/n) <y>:')
C read the reply in string LIN80
	 READ( NIN, '(A1)', ERR= 55555, END= 55555) LINE80(1:1)
	 CALL UCASE(LINE80(1:1))
	 IF (LINE80(1:1).EQ.'N') BOX = .FALSE.
C june 93 new option - grey backround within box
         IF (ICOL.EQ.0) THEN
           CALL PROMPT( NOUT,
     & 'Do you want grey background for box (y/n) <n>:')
C read the reply in string LIN80
           READ( NIN, '(A1)', ERR= 55555, END= 55555) LINE80(1:1)
           CALL UCASE(LINE80(1:1))
C use icol to convey info to s/r box draw 
           IF (LINE80(1:1).EQ.'Y') ICOL = -1
         ENDIF

       ENDIF
C      write(6, * ) 'debug picbox is ', PICBOX
C work out scale  
C Mono picture?
      IF (PICTNO.EQ.1) THEN
C 72 postscript units approx 25.4 mm's
C 16/7/98 change scale so that only use xmax-xmin just like in stereo
C old:	SCALE = MIN( 1./(XMAX-XMIN), 1./(YMAX-YMIN))
        SCALE = 1./(XMAX-XMIN)
C 9/11/93 - reduce scale a touch to avoid picture touching box
	SCALE = (PICBOX-5.)*(72./25.4)*SCALE
      ELSEIF (PICTNO.EQ.-1) THEN
C have orthogonal view XY ZY across page
C set PICBOX to size in mm's
        PICBOX = 190.
        SCALE = (PICBOX-25.)*(72./25.4)/(XMAX-XMIN + ZMAX-ZMIN)
      ELSE
C stereo - each picture has to be 60 mm wide
C assume that it will fit with regards to y
	SCALE = 60.*(72./25.4)/(XMAX-XMIN)
      ENDIF

C new option April 1993 allow the user to override scale - 
C useful if displaying more than one molecule but need same scale
      IF (SWITCH.EQ.'E') THEN
	WRITE( NOUT, '(A,F10.3,A/ A)') 
     &' Scale set to ',SCALE, ' ps units per angstrom.',
     &' Do you want to overide this?'
	CALL PROMPT( NOUT,
     & 'Enter new value <no override>')
	READ( NIN, '(A)', ERR= 55555, END= 55555) LINE80
C has the user specified a number?
	IF (LINE80(1:6).NE.'      ')
     &    READ( LINE80, '(BN,F10.0)', ERR= 55555) SCALE
      ENDIF
C      write(*,*) '(debug info)'
C      write(*,*) 'xmin xmax ymin ymax ', xmin, xmax, ymin, ymax
C
C      write(*,*) 'pictno ', pictno
C      write(*,*) 'debug scale ', scale
C      write(*,*) '(debug info)'

C transform the mm value of PICBOX into a PS value,
C for 72 PS points equal 25.4 mm.
      PICBOX = PICBOX*72/25.4 

C produce picture

C if user wants a box around the view(s), call s/r BOXDRW
      IF (BOX) CALL BOXDRW( SOUT, YMIN, YMAX, PICTNO, 
     &                      SCALE, 298., 400., PICBOX, ICOL)

C      write(*,*) '(debug info) aft boxdrw'
C      write(*,*) '(debug info) title= ', title
C      write(*,*) '(debug info) sout= ', sout
C      write(*,*) '(debug info) ymin= ', ymin
C      write(*,*) '(debug info) ymax= ', ymax
C      write(*,*) '(debug info) scale= ', scale
C if user wants a title below the picture, call s/r TITLEX 
      IF (TITLE) CALL TITLEX( SOUT, YMIN, YMAX, SCALE, 
     &             298., 400., BOX, STRING, PSIZE)

C      write(*,*) '(debug info) aft titlex'
C mono?
      IF (PICTNO.EQ.1) THEN
C draw picture - move draws & text strings
C centre of picture at poscript co-ords 298, 400
C        write(*,*) 'debug before call to qpspic'
	CALL QPSPIC( NIN, NOUT, SOUT, 
     &         MAXST, MAXCOL, ISTORE, RSTORE,
     &         MAXTXT, MAXLEN, TXTNO, TXTLEN, 
     &         TXTPOS, TXTDIR, TXTSTR, PSIZE,
     &         SGREY, LWIDTH, XMIN, XMAX, YMIN, YMAX,
     &         SCALE, 298., 400., HWMANY, DOTAT, ICOL, HSBCOL, LABORT)
        IF (LABORT) GOTO 55555

      ELSEIF (PICTNO.EQ.-1) THEN
C Orthogonal views
C have to draw two picts total width picbox which
C has been set to 190mm to fit on page.
C first work out coords of left hand edge of 
C picture in pts
C have two pictures (a) scale*(xmax-xmin) pts wide
C                   (b) scale*(zmax-zmin) pts wide
C and need to leave 5mm = 15pts in between
C so total width is scale*(xmax-xmin + zmax-zmin) + 15 pts
C this is to be centred on 298 so
C old line:
C        edge = 298. - 0.5*(scale*(xmax-xmin + zmax-zmin) + 15.)
C do not need to worry about centre as know lefthand edge from
C picbox which is in PS units already.  Leave a 15. ps unit margin
        EDGE = 298. - 0.5*PICBOX + 15.
C centre of XY picture will be 
        CENTRE = EDGE + 0.5*SCALE*(XMAX-XMIN)

C draw XY picture - move draws & text strings
C centre of picture at poscript co-ords CENTRE, 400
C        write(*,*) 'debug before call to qpspic'
	CALL QPSPIC( NIN, NOUT, SOUT, 
     &         MAXST, MAXCOL, ISTORE, RSTORE,
     &         MAXTXT, MAXLEN, TXTNO, TXTLEN, 
     &         TXTPOS, TXTDIR, TXTSTR, PSIZE,
     &         SGREY, LWIDTH, XMIN, XMAX, YMIN, YMAX,
     &         SCALE, CENTRE, 400., HWMANY, DOTAT, ICOL, HSBCOL, LABORT)
        IF (LABORT) GOTO 55555

C now rotate everything about y axis by 90 degrees
C these lines adapted from the stereo bit below
	CENT(1)  = (XMAX + XMIN)/2.0
	CENT(2)  = (ZMAX + ZMIN)/2.0
C now we have 90 degrees rotation about y axis
C so 1 0 0 goes to 0 0 -1 
C and 0 0 1 goes to 1 0 0 (third degree projection)
	MAT(1,1) = 0.
	MAT(2,1) = -1.
	MAT(1,2) = -MAT(2,1)
	MAT(2,2) = MAT(1,1)
C apply
	DO 711 CCOUNT=1 , MAXCOL  
	  DO 721 MCOUNT = 1, ISTORE(CCOUNT)
	     CALL ROT2X2( RSTORE( MCOUNT, 1, CCOUNT), 
     &                    RSTORE( MCOUNT, 3, CCOUNT), 
     &                    MAT, CENT)
721        CONTINUE
711     CONTINUE 
	DO 731 MCOUNT = 1, TXTNO
	  CALL ROT2X2( TXTPOS(1, MCOUNT), 
     &                 TXTPOS(3,MCOUNT), 
     &                 MAT, CENT)
731      CONTINUE

C now work out centre for this picture
C which has z along the x direction of page
C       and y along y direction
C by analogy to arguement above the right hand edge will be
C old line: edge = 298. + 0.5*(scale*(xmax-xmin + zmax-zmin) + 15.)
C right hand edge.  Leave a 15. ps unit margin
        EDGE = 298. + 0.5*PICBOX - 15.
C centre of ZY picture will be 
        CENTRE = EDGE - 0.5*SCALE*(ZMAX-ZMIN)
C draw piccy
	CALL QPSPIC( NIN, NOUT, SOUT, 
     &         MAXST, MAXCOL, ISTORE, RSTORE,
     &         MAXTXT, MAXLEN, TXTNO, TXTLEN, 
     &         TXTPOS, TXTDIR, TXTSTR, PSIZE,
     &         SGREY, LWIDTH, XMIN, XMAX, YMIN, YMAX,
     &         SCALE, CENTRE, 400., HWMANY, DOTAT, ICOL, HSBCOL, LABORT)
        IF (LABORT) GOTO 55555

      ELSE
C a stereo view - rotate all records
C by minus arot/2 around y axis -  
C set the centre of rotation
	CENT(1)  = (XMAX + XMIN)/2.0
	CENT(2)  = (ZMAX + ZMIN)/2.0
C arot is stored in radians
	MAT(1,1) = COS(-AROT/2.)
	MAT(2,1) = SIN(-AROT/2.)
	MAT(1,2) = -MAT(2,1)
	MAT(2,2) = MAT(1,1)

C rotate each move/draw record
	DO 11 CCOUNT=1 , MAXCOL  
	  DO 21 MCOUNT = 1, ISTORE(CCOUNT)
	     CALL ROT2X2( RSTORE( MCOUNT, 1, CCOUNT), 
     &                    RSTORE( MCOUNT, 3, CCOUNT), 
     &                    MAT, CENT)
21        CONTINUE
11      CONTINUE 
C also rotate text ancor points
	DO 31 MCOUNT = 1, TXTNO
	  CALL ROT2X2( TXTPOS(1, MCOUNT), 
     &                 TXTPOS(3,MCOUNT), 
     &                 MAT, CENT)
31      CONTINUE

	IF (PICTNO.EQ.3) THEN
C 3 picture stereo
C two right hand pictures at x postscript
C 298 +/- 65*(72./25.4) = 113.7 or 482.2
	  CALL QPSPIC( NIN, NOUT, SOUT, 
     &         MAXST, MAXCOL, ISTORE, RSTORE,
     &         MAXTXT, MAXLEN, TXTNO, TXTLEN, 
     &         TXTPOS, TXTDIR, TXTSTR, PSIZE,
     &         SGREY, LWIDTH, XMIN, XMAX, YMIN, YMAX,
     &         SCALE, 113.7, 400., HWMANY, DOTAT, ICOL, HSBCOL, LABORT)
          IF (LABORT) GOTO 55555

	  CALL QPSPIC( NIN, NOUT, SOUT, 
     &         MAXST, MAXCOL, ISTORE, RSTORE,
     &         MAXTXT, MAXLEN, TXTNO, TXTLEN, 
     &         TXTPOS, TXTDIR, TXTSTR, PSIZE,
     &         SGREY, LWIDTH, XMIN, XMAX, YMIN, YMAX,
     &         SCALE, 482.2, 400., HWMANY, DOTAT, ICOL, HSBCOL, LABORT)
          IF (LABORT) GOTO 55555

	ELSE 
C 2 picture stereo 
C produce right hand picture
C at present rotated by (-arot/2) ;
C the two pictures should be centred with there centres 65mm
C apart - ie. picture centre (xmin+xmax)/2, ymin+ymax/2
C should be at x postscript 298 +/- 32.5*(72./25.4) = 205.8 or 390.1
	  CALL QPSPIC( NIN, NOUT, SOUT, 
     &         MAXST, MAXCOL, ISTORE, RSTORE,
     &         MAXTXT, MAXLEN, TXTNO, TXTLEN, 
     &         TXTPOS, TXTDIR, TXTSTR, PSIZE,
     &         SGREY, LWIDTH, XMIN, XMAX, YMIN, YMAX,
     &         SCALE, 205.8, 400., HWMANY, DOTAT, ICOL, HSBCOL, LABORT)
          IF (LABORT) GOTO 55555
	ENDIF

C rotate all records by arot around y axis
C (have already rotated by minus arot/2)
C set the centre of rotation
	CENT(1)  = (XMAX + XMIN)/2.0
	CENT(2)  = (ZMAX + ZMIN)/2.0
C arot is stored in radians
	MAT(1,1) = COS(AROT)
	MAT(2,1) = SIN(AROT)
	MAT(1,2) = -MAT(2,1)
	MAT(2,2) = MAT(1,1)

C rotate each move/draw record
	DO 10 CCOUNT=1 , MAXCOL  
	  DO 20 MCOUNT = 1, ISTORE(CCOUNT)
	     CALL ROT2X2( RSTORE( MCOUNT, 1, CCOUNT), 
     &                    RSTORE( MCOUNT, 3, CCOUNT), 
     &                    MAT, CENT)
20        CONTINUE
10      CONTINUE 
C also rotate text ancor points
	DO 30 MCOUNT = 1, TXTNO
	  CALL ROT2X2( TXTPOS(1, MCOUNT), 
     &                 TXTPOS(3,MCOUNT), 
     &                 MAT, CENT)
30      CONTINUE

C records have been rotated now write out right hand or
C centre stereo picture
C 3-picture stereo
	IF (PICTNO.EQ.3) THEN
C centre picture - centered at {298,400}
	  CALL QPSPIC( NIN, NOUT, SOUT, 
     &         MAXST, MAXCOL, ISTORE, RSTORE,
     &         MAXTXT, MAXLEN, TXTNO, TXTLEN, 
     &         TXTPOS, TXTDIR, TXTSTR, PSIZE,
     &         SGREY, LWIDTH, XMIN, XMAX, YMIN, YMAX,
     &         SCALE, 298., 400., HWMANY, DOTAT, ICOL, HSBCOL, LABORT) 
          IF (LABORT) GOTO 55555

	ELSE
C 2-picture stereo x postscript 390.1
	  CALL QPSPIC( NIN, NOUT, SOUT, 
     &         MAXST, MAXCOL, ISTORE, RSTORE,
     &         MAXTXT, MAXLEN, TXTNO, TXTLEN, 
     &         TXTPOS, TXTDIR, TXTSTR, PSIZE,
     &         SGREY, LWIDTH, XMIN, XMAX, YMIN, YMAX,
     &         SCALE, 390.1, 400., HWMANY, DOTAT, ICOL, HSBCOL, LABORT)
          IF (LABORT) GOTO 55555
	ENDIF

      ENDIF

C sends the completed page out to the printer
      WRITE( SOUT, '(A)')
     &'showpage'
C write end of file
      WRITE( SOUT, '(A)')
     &'%%EOF'


55555 RETURN
      END
C
      SUBROUTINE ROT2X2( VEK1, VEK2, MAT, CENT)
      IMPLICIT NONE
C This s/r rotates the passed vector elements vek1, vek2
C by the matrix MAT around the centre point CENT.
C (n.b. only change vble's VEK1 & VEK2 in this routine
C  all other return unchanged - dum is a working array)
      REAL                      VEK1, VEK2, MAT(2,2),
     &                          CENT(2), DUM(2)

C make dum equal to vek's relative to cent
      DUM(1) = VEK1 - CENT(1)
      DUM(2) = VEK2 - CENT(2)

C apply matrix
      VEK1  = MAT(1,1)*DUM(1) + MAT(1,2)*DUM(2)
      VEK2  = MAT(2,1)*DUM(1) + MAT(2,2)*DUM(2)

C displace co-ords back
      VEK1 = VEK1 + CENT(1)
      VEK2 = VEK2 + CENT(2)

      RETURN
      END
C    
      SUBROUTINE WIDGRY( NIN, NOUT, ISTORE, MAXCOL, SGREY, LWIDTH,
     &               HWMANY, SWITCH, MAXST, RSTORE, ICOL, HSBCOL, DOTAT)
      IMPLICIT NONE 
C this s/r will store new linewidths and greyness for 
C every colour found in the read file;
C defaults are given if the user presses Return

C will use freda to ease data input
C vbles used by freda
C col:  c*80            the command?
C kl:   int             no of words inputed 0 then default
C fn:   r*4(40)         no.'s as they occur
C kn:     int           no. of no's
C fl:     c*1(40,6)     words?
      INCLUDE 'FREDAINC'

C passed variables

C user input/output streams returned unchanged
      INTEGER                   NIN, NOUT

C the store for move draws RETURN UNCHANGED
C the maximum number of colours is stored in MAXCOL
C MAXCOL is assigned a value in qplot and MUST not be changed
C istore( 1 to MAXCOL) is the number of move/draws for each colour
      INTEGER                   MAXCOL
      INTEGER                   ISTORE(MAXCOL)

C LWIDTH is an array which stores the line widths in points 
C for each colour
      REAL                      LWIDTH( MAXCOL)
C array SGREY stores greyness  0 black, 1 white
      REAL                      SGREY( MAXCOL)
C number of lines to be drawn 
C 1:just a line
C 2:two lines
C 3:a rectangle
      INTEGER                   HWMANY(20) 

C switch indicates level of control to be used  
C return unchanged
      CHARACTER*1               SWITCH

C new feature April 1993 - check that at least one draw is specified
C before asking question
      INTEGER                   MAXST
      REAL                      RSTORE( MAXST, 0:3, MAXCOL)

C if ICOL = 0 then no colour
C         = 1 then colour on white background
C         = 2 then colour on a black background
      INTEGER                   ICOL
C colour for colour!
C hue from   0 to 360
C saturation 1 full colour 0 white.
C brightness 1 full colour 0 black
C   0 1 1 red   / 0 0.5 1 pink
C  60 1 1 yellow
C 120 1 1 green
C 180 1 1 cyan
C 240 1 1 blue
C 300 1 1 magenta
C   0 0 1 white
C   0 1 0 black
      REAL                      HSBCOL( 3, MAXCOL)

C Vble which passes on the information whether
C there are "dot at" records in the file:
C -55555     means dots are processed into 3d crosses in s/r qreadi
C -ve no     dots should be processed into open circles (white centre)
C 0.0        no dots in file
C +ve number the point size for circles to which dots processed
C n.b. only deal with circle option here - crosses done in s/r qreadi
      REAL                       DOTAT

C internal vbles **************

C line for input
      CHARACTER*80              LINE80
C character for input
      CHARACTER *1              COM1

C loop count for colour 
      INTEGER                   CCOUNT, RCOUNT

C only dots for a particular colour
      LOGICAL			LDOT 

C end of declarations **********

C allow colour output if advanced - change to whatever the level
      CALL PROMPT( NOUT,
     & 'Do you want colour postscript output? (y/n) <n>:')
      READ( NIN, '(A1)', END=55555, ERR= 55555) COM1
      IF ((COM1.EQ.'Y').OR.(COM1.EQ.'y')) THEN
	CALL PROMPT( NOUT,
     & 'Do you want a black or white background? (b/w) <w>:')
	READ( NIN, '(A1)', END=55555, ERR= 55555) COM1
	IF ((COM1.EQ.'B').OR.(COM1.EQ.'b')) THEN
	  ICOL = 2
        ELSE
	  ICOL = 1
	ENDIF
      ELSE 
C normal monochrome output
	ICOL = 0
      ENDIF

C set colours/ linewidths to defaults
C change 24/10/95 to sensible defaults
C all quanta line widths to 0.5pt
      DO 10 CCOUNT = 1, 14
	SGREY(CCOUNT)  = 0.0 
	LWIDTH(CCOUNT) = 0.5
	HWMANY(CCOUNT) = 1
10    CONTINUE
C set all hole objects to 1.5 pt lines
      DO 20 CCOUNT = 15, 20
        SGREY(CCOUNT)  = 0.0
        LWIDTH(CCOUNT) = 1.5
        HWMANY(CCOUNT) = 1
20    CONTINUE
C all hole surfaces get mapped to open lines
      HWMANY(16) = 2
      HWMANY(17) = 2
      HWMANY(18) = 2


C if doing colour 
      IF (ICOL.NE.0) THEN
        WRITE( NOUT, '(A)')
     &' Can either preserve default quanta colours',
     &'   or map all non-HOLE objects to cyan? '
        CALL PROMPT( NOUT,
     & '  Preserve quanta colours(P) or map to magenta? <map>:')
        READ( NIN, '(A1)', END=55555, ERR= 55555) COM1
        IF ((COM1.NE.'p').AND.(COM1.NE.'P')) THEN
C leave hole colours alone
          DO 30 CCOUNT = 1, 16
            HSBCOL(1,CCOUNT) = 300.00
            HSBCOL(2,CCOUNT) = 1.0
            HSBCOL(3,CCOUNT) = 1.0
30        CONTINUE

        ELSE 
C defaults same as quanta
C colour 1 milky green
	  HSBCOL(1,1) = 120.
	  HSBCOL(2,1) = 0.4
	  HSBCOL(3,1) = 0.9804
C 2 to light blue
	  HSBCOL(1,2) = 204.
	  HSBCOL(2,2) = 1.0
	  HSBCOL(3,2) = 1.0
C 3 to red
	  HSBCOL(1,3) = 0.0 
	  HSBCOL(2,3) = 1.0
	  HSBCOL(3,3) = 1.0
C 4 to yellow
          HSBCOL(1,4) = 54.0
          HSBCOL(2,4) = 1.0
          HSBCOL(3,4) = 1.0
C 5 to white
          HSBCOL(1,5) = 240.
          HSBCOL(2,5) = 0.0
          HSBCOL(3,5) = 1.0
C 6 to cream
          HSBCOL(1,6) = 60.0
          HSBCOL(2,6) = 0.6078
          HSBCOL(3,6) = 1.0
C 7 to bright green
          HSBCOL(1,7) = 120.0
          HSBCOL(2,7) = 1.0
          HSBCOL(3,7) = 1.0
C 8 to purple
          HSBCOL(1,8) = 288.0
          HSBCOL(2,8) = 1.0
          HSBCOL(3,8) = 1.0
C 9 to light grey/bown
          HSBCOL(1,9) = 81.00
          HSBCOL(2,9) = 0.0
          HSBCOL(3,9) = 0.7319
C 10 to salmon
          HSBCOL(1,10) = 9
          HSBCOL(2,10) = 0.6
          HSBCOL(3,10) = 0.8
C 11 to grey  
          HSBCOL(1,11) = 9
          HSBCOL(2,11) = 0.0
          HSBCOL(3,11) = 0.4
C 12 to light blue
          HSBCOL(1,12) = 180
          HSBCOL(2,12) = 0.5
          HSBCOL(3,12) = 1.0
C 13 to orange
          HSBCOL(1,13) = 14
          HSBCOL(2,13) = 1.0
          HSBCOL(3,13) = 1.0
C 14 to pink       
          HSBCOL(1,14) = 332
          HSBCOL(2,14) = 0.6078
          HSBCOL(3,14) = 1.0
        ENDIF

C colours 15 to 20 HOLE objects - all mapped the same
C 15 is hole centre line
        HSBCOL(1,15) = 60.0
        HSBCOL(2,15) = 1.0
        HSBCOL(3,15) = 1.0
C 16 is low rad surface open lines bright red
        HSBCOL(1,16) = 0.0
        HSBCOL(2,16) = 1.0
        HSBCOL(3,16) = 1.0
C 17 is mid rad surface open lines bright green
        HSBCOL(1,17) = 120.0
        HSBCOL(2,17) = 1.0
        HSBCOL(3,17) = 1.0
C 18 is high rad surface open lines bright blue 
        HSBCOL(1,18) = 240.0
        HSBCOL(2,18) = 1.0
        HSBCOL(3,18) = 1.0
C 19 is capsule vectors normal thick cyan   
        HSBCOL(1,19) = 180.0
        HSBCOL(2,19) = 1.0
        HSBCOL(3,19) = 1.0
C 20 is close atom lines normal thick grey  
        HSBCOL(1,20) = 9.0
        HSBCOL(2,20) = 0.0
        HSBCOL(3,20) = 0.4
      ELSE
C default options for monochrome picture
C make centreline dark grey
        SGREY(15)  = 0.2
C hole surface to white except low surface to light grey
        SGREY(16)  = 0.8
        SGREY(17)  = 1.0 
        SGREY(18)  = 1.0 
      ENDIF


C if not at normal level then allow user to overide
      IF (SWITCH.EQ.'E') THEN
C go thru colours
	DO 40 CCOUNT = 1, MAXCOL
C only have to ask for colours with anything stored in them!
	  IF (ISTORE(CCOUNT).GT.0) THEN
C check that at least on draw to is specified
C look thru all records for a draw if one is found then jump to question
C special feature to detect dots 
            LDOT = .FALSE.
	    DO 31 RCOUNT = 1, ISTORE(CCOUNT)
	      IF( RSTORE( RCOUNT, 0, CCOUNT).EQ. 3.0) THEN
C have a draw to - colour is not all dots then
                LDOT = .FALSE.
                GOTO 32
              ENDIF
	      IF( RSTORE( RCOUNT, 0, CCOUNT).EQ. 4.0) LDOT = .TRUE.
31          CONTINUE
C if ldot is true then we have to ask what colour and line width - but not type
            IF (LDOT) GOTO 32
C no move to found no point in asking for moveto's etc.
	    GOTO 40

C jump here if there is a moveto for this colour
32          CONTINUE

C do we have only dots?
            IF (LDOT) THEN
	      WRITE( NOUT, '(/A,I2,A,I8)')
     &' Colour ',CCOUNT,
     &        	    ' has only DOT records:',ISTORE(CCOUNT) 
            ELSE 
	      WRITE( NOUT, '(/A,I2,A,I8)')
     &' The number of moves/draws/dots read for colour ',CCOUNT,
     &        	    ' is ',ISTORE(CCOUNT) 
            ENDIF

C tell user default line width
C if not only dots or if dots are open.
            IF ((.NOT.LDOT) .OR. (DOTAT.LT.0.)) THEN 
	      WRITE( NOUT, '(A,F5.2,A)')
     &' Will output with a linewidth of ', LWIDTH(CCOUNT), ' points'
	      CALL PROMPT( NOUT, 
     & 'Do you want to change the linewidth ?'//
     &                 ' Enter a positive number <no change >: ')
C read the reply in string LINE80
	      READ( NIN, '(A)', ERR= 55555, END= 55555) LINE80 
C has the user specified a number?
	      IF (LINE80(1:6).NE.'     ') THEN 
	        READ( LINE80, '(BN,F10.0)', ERR= 55555) LWIDTH(CCOUNT)
C If the read linewidth is not positive it is reset here
	        IF  (LWIDTH(CCOUNT).LT.0) LWIDTH(CCOUNT) = 0.0
	      ENDIF
            ENDIF

C tell user what the default line type is
            IF (.NOT.LDOT) THEN 
	      WRITE( NOUT, '(A/ A/ A,I4)')
     &' There are 3 line types supported:',
     &'  1 = normal lines  2 = open rectangles  3 = closed rectangles.',
     &' Default line type is ',  HWMANY(CCOUNT)
C ask the following questions only if the 
C degree of difficulty requires this;
C do you want a single, double line or rectangle 
C do you want to change the linewidth?
	      CALL PROMPT( NOUT, 
     &'Do you want to change line type? Enter number <no change>:')
	      READ( NIN, '(A)', ERR= 55555, END= 55555) LINE80 
C has the user specified a number?
	      IF (LINE80(1:6).NE.'     ') THEN 
	        READ( LINE80, '(BN,I10)', ERR= 55555) HWMANY(CCOUNT)
C within limits?
	        IF  (HWMANY(CCOUNT).LT.1 .OR. HWMANY(CCOUNT).GT.3)
     &            HWMANY(CCOUNT) = 1
	      ENDIF
            ENDIF

C may 93 implement colour option - greyness if not
	    IF (ICOL.EQ.0) THEN
C tell user about greyness
	      WRITE( NOUT, '(A,F4.2,A)')
     &' Default greyness: ', SGREY(CCOUNT), '  (0=Black, 1=white)'
	      CALL PROMPT( NOUT,
     & 'Do you want to change this? Enter number <no change>:')
C read reply
	      READ( NIN, '(A)', ERR= 55555, END= 55555) LINE80 
	      IF (LINE80(1:6).NE.'     ') THEN 
		READ( LINE80, '(BN,F10.0)', ERR= 55555) SGREY(CCOUNT)
C within limits?
		IF (SGREY(CCOUNT).LT.0.) SGREY(CCOUNT) = 0.
		IF (SGREY(CCOUNT).GT.1.) SGREY(CCOUNT) = 1.
	      ENDIF

	    ELSE
C colour option
C jump back if colour guide asked for
123           CONTINUE
	      WRITE( NOUT, '(A)')
     &' What colour do you want?',
     &' Specify hue (0 to 360), saturation (0 to 1),'//
     &                        ' brightness (0 to 1).'
C make up prompt string
	      LINE80 =
     &'What colour? (Enter g for guide) <999 9.999 9.999>:'
C      12345678901234567890123456789012345678901234567890
C               1         2         3         4         5
C use write to place correct numbers for default
	      WRITE( LINE80(35:37), '(I3)') INT( HSBCOL(1,CCOUNT))
	      WRITE( LINE80(39:43), '(F5.3)') HSBCOL(2,CCOUNT)
	      WRITE( LINE80(45:49), '(F5.3)') HSBCOL(3,CCOUNT)
	      CALL PROMPT( NOUT, LINE80)
	      READ( NIN, '(A)', ERR= 55555, END= 55555) LINE80 
C guide to colour wanted
	      IF ((LINE80(1:1).EQ.'G').OR.(LINE80(1:1).EQ.'g')) THEN
		 WRITE( NOUT, '(A)')
     &'  ',
     &' Colour is specified by three numbers:',
     &'   (i)   hue runs from 0 to 360 - same as defined in QUANTA;',
     &'   (ii)  saturation from 1 meaning full colour 0 for white;',
     &'   (iii) brightness 1 for full colour 0 for black.',
     &' Examples: ',
     &'   0  1  1 red',   
     &'   0 0.5 1 pink',
     &'  60  1  1 yellow',
     &' 120  1  1 green',
     &' 180  1  1 cyan',
     &' 240  1  1 blue',
     &' 300  1  1 magenta',
     &'   0  0  1 white',
     &'   0  1  0 black.',
     &' Can specify just hue or all three numbers.'
		GOTO 123
	      ENDIF
C there maybe up to three numbers on line 
C use freda to decode
	      COL = LINE80

C find any numbers in reply with freda
	      CALL FREDA(1,80,FL,6)
	      IF (KN.GE.3) THEN
		HSBCOL(1,CCOUNT) = FN(1)
		HSBCOL(2,CCOUNT) = FN(2)
		HSBCOL(3,CCOUNT) = FN(3)
	      ELSEIF (KN.EQ.2) THEN
		HSBCOL(1,CCOUNT) = FN(1)
		HSBCOL(2,CCOUNT) = FN(2)
	      ELSEIF (KN.EQ.1) THEN
		HSBCOL(1,CCOUNT) = FN(1)
	      ENDIF
	      
	    ENDIF
C debug info
C	   write( nout, *) 'debug col w box grey', 
C     &        ccount, lwidth(ccount), hwmany(ccount), sgrey(ccount)
C	   write( nout, *) 'debug hue sat bright',
C     &        hsbcol(1,ccount), hsbcol(2,ccount), hsbcol(3,ccount) 

C end of 'if istore > 0'
	  ENDIF
C end of colour loop
40      CONTINUE
C end of expertness lvel
      ENDIF

55555 RETURN
      END

C this s/r finds the  max/ minimum of stored records
      SUBROUTINE MINMAX( XMAX, XMIN, YMAX, YMIN, ZMAX, ZMIN, 
     &     RSTORE, ISTORE,  MCOUNT, CCOUNT, MAXST, MAXCOL)
      IMPLICIT NONE
C this s/r calculates the min/max of the records
C found in the read file
C 5/1/93 modified so that a move to which is not followed by
C a draw is ignored

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
C values of min/max to be found out
      REAL                      XMAX, XMIN, YMAX
      REAL                      YMIN, ZMAX, ZMIN
 
C loop counts for colour, for move/draw records 
      INTEGER                   CCOUNT, MCOUNT

C end of declarations **********

C find maximum/minimum x ,y & z
      XMAX = -1E15
      XMIN =  1E15
      YMAX = -1E15
      YMIN =  1E15
      ZMAX = -1E15
      ZMIN =  1E15
C go thru colours
      DO 10 CCOUNT = 1, MAXCOL
C go thru move draw records 
	DO 20 MCOUNT = 1, ISTORE(CCOUNT)
C 5/1/93 ignore move to's which are followed by move tos
	  IF ( (RSTORE(MCOUNT,0,CCOUNT).EQ.2.) .AND.
     &         (RSTORE(MCOUNT+1,0,CCOUNT).NE.3.) ) THEN 
C            write(*,*) ' debug ', mcount, rstore(mcount,1,ccount)
	    GOTO 20
	  ENDIF
C find max/ minimum
	  IF (RSTORE(MCOUNT,1,CCOUNT).GT.XMAX)
     &                   XMAX= RSTORE(MCOUNT,1,CCOUNT)
	  IF (RSTORE(MCOUNT,1,CCOUNT).LT.XMIN)
     &                   XMIN= RSTORE(MCOUNT,1,CCOUNT)
	  IF (RSTORE(MCOUNT,2,CCOUNT).GT.YMAX)
     &                   YMAX= RSTORE(MCOUNT,2,CCOUNT)
	  IF (RSTORE(MCOUNT,2,CCOUNT).LT.YMIN)
     &                   YMIN= RSTORE(MCOUNT,2,CCOUNT)
	  IF (RSTORE(MCOUNT,3,CCOUNT).GT.ZMAX)
     &                   ZMAX= RSTORE(MCOUNT,3,CCOUNT)
	  IF (RSTORE(MCOUNT,3,CCOUNT).LT.ZMIN)
     &                   ZMIN= RSTORE(MCOUNT,3,CCOUNT)
20      CONTINUE
10    CONTINUE

      RETURN
      END

CC
C
      SUBROUTINE BOXDRW( SOUT, YMIN, YMAX,PICTNO,
     &                   SCALE, XCEN, YCEN, PICBOX, ICOL)
      IMPLICIT NONE
C this s/r places a box around the picture relative to
C the coordinatesiof the centre(XCEN,YCEN)
C  and max/min values passed as arguments;
C the latter ones are altered with regard to the SCALE
C
C June 1993 adapted so that the user can specify that a grey
C background should be used this is indicated by ICOL = -1
C ICOL is then reset in this routine.

C May 1993 colour implementation
C if ICOL = 0 then no colour
C         = 1 then colour on white background
C         = 2 then colour on a black background
      INTEGER                   ICOL

C values of min/max to be used - only for [y]
      REAL                      YMAX, YMIN
C file output stream- returned unchanged
      INTEGER                   SOUT
C the number of pictures to be drawn
      INTEGER                   PICTNO
C values of x/y for the box to be centred around
      REAL                      XCEN, YCEN
C the scale of the picture, the size of it  SUPPLIED IN PS UNITS 
      REAL                      SCALE, PICBOX
C the width and the height of the box
      REAL                      WIDTH, HEIGHT
C the start of the rectangle(lower left corner)
      REAL                      STAX, STAY
C vbles to hold the preliminary values of min/max
      REAL                      YAX, YIN
C upper right y coordinate is held by URY, and
C lower right x coordinate by LRX
      REAL                      LRX, URY
C starting point for demarkation lines inside
C the box (in stereo mode )
      REAL                      DEMARK
C loop counter
      INTEGER                   JC 
C
C  end of decs*******************

C different vbles are needed to store the [y] max/min because
C otherwise these would be overwritten and would impede 
C further calculations;  
C these vbles are yax & yin
C       
      YAX = SCALE* YMAX + YCEN 
      YIN = SCALE* YMIN + YCEN 
C the starting points of the box (low-left) are STAX(x-coor.)
C and STAY(y-coor.)
      STAX = XCEN - PICBOX/2 
C the [y] coordinate is ammended by a 2.5 mm
C margin (7.14 Ps points)
      STAY =  YCEN - ((YAX-YIN)/2) - 7.14 
C the width of the rectangle and the height of it
C are calculated here
      WIDTH = PICBOX
      HEIGHT = YAX - YIN + 2*7.14

C low-right x co-ord & up-right y co-ord; 
      LRX = STAX + WIDTH
      URY = STAY + HEIGHT
C reset the greyness to black (0) unless doing inverse colour
      IF (ICOL.EQ.2) THEN
	WRITE(SOUT, '(A)') '1 setgray'
      ELSEIF (ICOL.EQ.-1) THEN
C draw grey filled box
	WRITE(SOUT, '(A)') '0.9 setgray % grey back'
        WRITE( SOUT, '(2F10.3,A)') STAX, STAY, ' M'
        WRITE( SOUT, '(2F10.3,A)') LRX, STAY,  ' L'
        WRITE( SOUT, '(2F10.3,A)') LRX, URY,   ' L'
        WRITE( SOUT, '(2F10.3,A)') STAX, URY,  ' L'
        WRITE( SOUT, '(A)') 'closepath fill'
	WRITE(SOUT, '(A)') '0 setgray'
      ELSE
C normal black box 
	WRITE(SOUT, '(A)') '0 setgray'
      ENDIF

C set the linewidth small 
      WRITE( SOUT, '(A)') '0.25 setlinewidth newpath % box'
C write in the output file the rectangle (the box)
      WRITE( SOUT, '(2F10.3,A)') STAX, STAY, ' moveto'
      WRITE( SOUT, '(2F10.3,A)') LRX, STAY,  ' lineto'
      WRITE( SOUT, '(2F10.3,A)') LRX, URY,   ' lineto'
      WRITE( SOUT, '(2F10.3,A)') STAX, URY,  ' lineto'
      WRITE( SOUT, '(A)') 'closepath stroke'

C now stroking lines for the delimitations of the
C  different views (if stereo views, in principle)
      IF (PICTNO.NE.1) THEN
C draw the lines with regards to the no. of stereo views;
	DO 43 JC=2, PICTNO
C calculate the demarkation line with respect to the 
C number of pictures to be drawn
	  DEMARK = XCEN + (2- PICTNO)*PICBOX/6
C write in the moveto command for the first(second)
C demarkation line inside the box
	  WRITE(SOUT, '(2F10.3,A)') DEMARK, STAY, ' moveto'
C write in the lineto command for the first(second)
C demarkation line inside the box
	  WRITE(SOUT, '(2F10.3,A)') DEMARK, URY, ' lineto'
C write the stroke command
	  WRITE(SOUT, '(A)') ' stroke'
C second time , draw the demarkation line at the
C right side of the centre[ (-)*(-) = (+) ]
	  PICBOX = - PICBOX
43      CONTINUE
C end of drawing demarkation lines
      ENDIF

C return here even on error
55555 CONTINUE
      RETURN
      END
C
C
      SUBROUTINE TITLEX(SOUT, YMIN, YMAX, SCALE, 
     &             XCEN, YCEN, BOX, STRING, PSIZE)
      IMPLICIT NONE
C this s/r places a STRING passed as argument at the bottom of
C the picture relative to the centre of the page (XCEN,YCEN)
C and to a new layout given by a possible box.

C values of min/max to be used
       REAL                      YMAX,YMIN
C pont size of font used to output text. 
C -55 used to indicate that text records are not to be output.
       REAL                      PSIZE
C vble to say whether a box had been required
       LOGICAL                   BOX
C file output stream- returned unchanged
       INTEGER                   SOUT
C character string to be positioned as title
      CHARACTER *80              STRING
C values of x/y for the box to be centred around
       REAL                      XCEN, YCEN
C the scale of the picture
       REAL                      SCALE
C the start of the title (x & y)
       REAL                      STATLX, STATLY
C string length
      INTEGER                    STRLEN

C  end of decs
C the starting points of the title STATLX(x-coor.)
C and STATLY(y-coor.) ; 
      STATLX = XCEN 
C the dispalcemnt with regard to the pointsize(PSIZE)
C is cca 1.694 - (0.694 represents the height of 
C a given character) 
      STATLY = YCEN - SCALE*(YMAX-YMIN)/2 - PSIZE*1.694  
C if the picture bears a box, take the 7.14 points box margin 
C into consideration.
      IF(BOX) STATLY = STATLY - 7.14
C  calculate the string length
      STRLEN = INDEX(STRING,'      ') - 1
C      write (6, *) 'string length is ', STRLEN
C starts writing the string ;
C this sequence is particular to Post Script language
C for writing centred strings
      WRITE(SOUT, '(A,A,A)')
     &' (', STRING(1:STRLEN), ')'
      WRITE(SOUT, '(A,F10.3,A)')
     &' dup stringwidth pop 2 div ',STATLX,
     &' exch sub '
       WRITE(SOUT, '(F10.3,A)')
     & STATLY,' moveto show'


       RETURN
       END
