      SUBROUTINE QPTRAS
      IMPLICIT NONE
C ********************************************************************
C *                                                                  *
C * This software is an unpublished work containing confidential and *
C * proprietary information of Birkbeck College. Use, disclosure,    *
C * reproduction and transfer of this work without the express       *
C * written consent of Birkbeck College are prohibited. This notice  *
C * must be attached to all copies or extracts of the software.      *
C *                                                                  *
C * Copyright 1997 by Oliver Smart & Birkbeck College		     *
C *                                                                  *
C ********************************************************************
C
C Modification history:
C
C Date	Author		Modification
C 03/97	O.S. Smart	Original version
C
C
C Takes hydra/quanta binary 3D plot file and converts to
C a pdb file which rasmol can interpret.  Use atom records
C and conects to do move draws.  Different colours by different 
C element types:
C
C colours from rasmol documentation:
C CPK Colours
C 
C The RasMol cpk colour scheme is based upon the colours of the popular plastic spacefilling
C models which were developed by Corey, Pauling and later improved by Kultun. This colour
C scheme colour `atom' objects by the atom (element) type. This is the scheme conventionally used
C by chemists. The assignment of element type to colours is given below. 
C 
C     Carbon       light grey       Chlorine         green
C     Oxygen       red              Bromine, Zinc    brown
C     Hydogen      white            Sodium           blue
C     Nitrogen     light blue       Iron             purple
C     Sulphur      yellow           Calcium, Metals  dark grey
C     Phosphorous  orange           Unknown          deep pink
C
C THANKS TO ROGER SAYLE FOR POINTING OUT HOW TO DO THIS!


C the vectors
      REAL 			RVEC4(4)

C screen, keyboard
      INTEGER			NIN
      PARAMETER(		NIN = 5)
      INTEGER			NOUT
      PARAMETER(		NOUT= 6)
C input/output file stream
      INTEGER			SIN, SOUT

C input/output filename
      CHARACTER*200		FIN, FOUT

C string for text records in hydra file
      CHARACTER*80		STRING

C abort indicator
      LOGICAL			LABORT

C current colour being output
      INTEGER			CURCOL

C The atom type this is processed to
      CHARACTER*2		ATMCOL
      
C number of records written out
      INTEGER			RECPRO

C loop count for going thru records
      INTEGER			RCOUNT

C working integer
      INTEGER			IWORK

C previous point - need for draw to (must know from where!)
      REAL			PREXYZ(3)

C end of decs ***********

      WRITE( NOUT, '(A)') 
     &' S/r qptras. ',
     &'   Reads in a hydra/quanta 3D binary plot file and writes',
     &'   out a pseudo-pdb file equivalent suitable for rasmol.',
     &' ',
     &' The only way to see a hole object and the molecule which ',
     &' produced it is to concatinate the result of a MOLQPT card ',
     &' with the object - see HOLE documentation for details.',
     &' '

C routine to find latest file of
C type in the directory.  N.b. only works on unix machines
      CALL LASTF( FIN, '.qpt')
      IF (FIN(1:4).EQ.'none') FIN = 'input'

C get input filename
      LABORT = .TRUE.
C (input stream, output, oldfile?, file_stream, file_type, name,
C  allow abort?, default extension)
C N.B. as file_type includes 'BINARY' then will open as binary
      CALL INTERF( NIN, NOUT, .TRUE., SIN, 
     &  'input binary hydra/quanta plot', FIN, LABORT, '.qpt')
      IF (LABORT) GOTO 55555

C default filename
      FOUT = FIN
      IWORK = INDEX(FOUT,'.qpt')
      IF (IWORK.NE.0) FOUT = FOUT(1:IWORK-1)//'_rasmol'

      LABORT = .TRUE.
      CALL INTERF( NIN, NOUT, .FALSE., SOUT, 
     &  'output pseudo-pdb file', FOUT, LABORT, '.pdb')
      IF (LABORT) GOTO 55555

C write header for plot file n.b. wordview needed to avoid clipping
      WRITE ( SOUT, '(A)') 
     &'REMARK   Output of qptras option of qpt_conv ',
     &'REMARK   part of HOLE suite ',
     &'REMARK   Input file: '//FIN(1:INDEX(FIN,'     ')-1)

C unlike other applications do single pass - keeping store of current colour
C  Colour #                                              To atom type
C  1 to 14   Output normal quanta colours to gray            C
C  alt 15    centre line to yellow                           S
C  alt 16    low rad surf to red                             O
C  alt 17    mid rad surf to green                          CL
C  alt 18    high rad surf to blue                          NA
C  alt 19    capsule vectors to cyan-no cyan so use purple   P
C  alt 20    closest atom vectors to white                  HH
C start with atom type FE
      ATMCOL = ' C'
C initial number of records processed
      RECPRO = 0

C read line from quanta file
C at end of the file then goto line 25 read file for next colour 
10    CONTINUE
        READ( SIN, END= 25) RVEC4
C change to colour record?
        IF (RVEC4(1).EQ.1.0) THEN
C store colour number
C allow alternative colours for hole objects 15 to 20
C these are indicated by a -55 as second number and are in third
          IF (INT(RVEC4(3)).EQ.-55) THEN
	    CURCOL = INT(RVEC4(4))
          ELSE
	    CURCOL = INT(RVEC4(2))
          ENDIF

C reset ATMCOL according to table given above
          IF (CURCOL.LT.15) THEN
            ATMCOL = ' C'
          ELSEIF (CURCOL.EQ.15) THEN
            ATMCOL = ' S'
          ELSEIF (CURCOL.EQ.16) THEN
            ATMCOL = ' O'
          ELSEIF (CURCOL.EQ.17) THEN
            ATMCOL = 'CL'
          ELSEIF (CURCOL.EQ.18) THEN
            ATMCOL = 'NA'
          ELSEIF (CURCOL.EQ.19) THEN
            ATMCOL = ' P'
          ELSE
            ATMCOL = 'HH'
          ENDIF          

C a dotat record?
        ELSEIF (RVEC4(1).EQ.4.0) THEN
C draw a single line (could change to a 3D cross in future 
C note rasmol insists that there are at least as many bonds
C as atoms so must do 3 bonds and 3 atoms per move draw!
C so may as well make into little triangle
          WRITE( SOUT, '(A4,2X,I5,1X,A2,A,I4,4X,3F8.3,A)') 
     &'ATOM  ', (5000+RECPRO+1), ATMCOL,'01 HOL E', 1,
     &  RVEC4(2)-0.02, RVEC4(3), RVEC4(4), '  1.00 20.00',
     &'ATOM  ', (5000+RECPRO+2), ATMCOL,'02 HOL E', 1,
     &  RVEC4(2)+0.02, RVEC4(3), RVEC4(4), '  1.00 20.00',
     &'ATOM  ', (5000+RECPRO+3), ATMCOL,'03 HOL E', 1,
     &  RVEC4(2), RVEC4(3)+0.01, RVEC4(4)+0.01,'  1.00 20.00'

C increment RECPRO - have output 3 records
          RECPRO = RECPRO + 3
          PREXYZ(1) = RVEC4(2)
          PREXYZ(2) = RVEC4(3)
          PREXYZ(3) = RVEC4(4)

C a moveto record?
        ELSEIF (RVEC4(1).EQ.2.) THEN
          PREXYZ(1) = RVEC4(2)
          PREXYZ(2) = RVEC4(3)
          PREXYZ(3) = RVEC4(4)

C a drawto record?
        ELSEIF (RVEC4(1).EQ.3.0) THEN
C draw from prexyz to rvec4
C 1st record at rvec4
          WRITE( SOUT, '(A4,2X,I5,1X,A2,A,I4,4X,3F8.3,A)') 
     &'ATOM  ', (5000+RECPRO+1), ATMCOL,'01 HOL E', 1,
     &  RVEC4(2), RVEC4(3), RVEC4(4), '  1.00 20.00',
C 2nd at mid point     
     &'ATOM  ', (5000+RECPRO+2), ATMCOL,'02 HOL E', 1,
     &     0.5*(PREXYZ(1)+RVEC4(2)),
     &     0.5*(PREXYZ(2)+RVEC4(3)),
     &     0.5*(PREXYZ(3)+RVEC4(4)),  '  1.00 20.00',
C 3rd at end point
     &'ATOM  ', (5000+RECPRO+3), ATMCOL,'03 HOL E', 1,
     &  PREXYZ(1), PREXYZ(2), PREXYZ(3), '  1.00 20.00'
          RECPRO = RECPRO + 3
          PREXYZ(1) = RVEC4(2)
          PREXYZ(2) = RVEC4(3)
          PREXYZ(3) = RVEC4(4)

C text record? - do not use but must read to avoid error
        ELSEIF (RVEC4(1).EQ.5.0) THEN
C 2nd number read gives the number of characters
          READ( SIN)   STRING(1:INT(RVEC4(2)))
          WRITE( NOUT, '(A)')
     &' Warning - ignoring text record in .qpt file'//CHAR(7)

        ENDIF
   
C read next line of quanta file
        GOTO 10

25    CONTINUE
C jump to here on EOF of input file
C we have output RECPRO atom records each pair of 2 need 2 mutual
C connects
C go thru pairs
      DO 75 RCOUNT = 1, RECPRO, 3
C want to connect atom 5000+RCOUNT to atom 5000+RCOUNT+1
        WRITE( SOUT, '(A,2I5)') 
     &'CONECT', (5000+RCOUNT),   (5000+RCOUNT+1),
     &'CONECT', (5000+RCOUNT+1), (5000+RCOUNT+2),
     &'CONECT', (5000+RCOUNT),   (5000+RCOUNT+2)
75    CONTINUE

55555 WRITE(NOUT,*)
      RETURN
      END
