      PROGRAM QPTCON
      IMPLICIT NONE
C ********************************************************************
C *                                                                  *
C * This software is an unpublished work containing confidential and *
C * proprietary information of Birkbeck College. Use, disclosure,    *
C * reproduction and transfer of this work without the express       *
C * written consent of Birkbeck College are prohibited. This notice  *
C * must be attached to all copies or extracts of the software.      *
C *                                                                  *
C * (c) 1995 Oliver Smart & Birkbeck College, All rights reserved    *
C *                                                                  *
C ********************************************************************
C
C Modification history:
C
C Date	Author		Modification
C 10/95	O.S. Smart	First version
C 02/96 O.S. Smart	Kinemage option added
C O2/96 O.S.S.		O option added.
C 09/96 O.S.S.		VRML option added
C 02/97 O.S.S.		rasmol option added
C 28/02/97 O.S.S.	Release HOLE2 beta001
C 11/97    O.S.S.       vt control codes
C
C Converts .qpt file to:
C An ascii version of qpt (can edit)
C A .qpt file in which dots are replaced by 3D crosses
C Sybyl format
C InsightII format
C 
C Bundles together old hydasc, qptdot, qpt_sybyl and qpt_insight programs

C command
      CHARACTER*1		COM1

C screen, keyboard
      INTEGER			NIN
      PARAMETER(		NIN = 5)
      INTEGER			NOUT
      PARAMETER(		NOUT= 6)

C end of decs ******************

C turn on VT codes  with bold after prompt- 
      CALL VTCON( .TRUE.)

C greetings
      WRITE( NOUT,  '(A)')
     &' *** Program qpt_conv ***',
     &' Copyright 1996,1997 by Oliver Smart and Birkbeck College',
     &' Copyright 2004 by Oliver Smart ',
     &' Program modification number 2.2 001'


C write link time of program to screen
      CALL VERTIM( NOUT)

C initial menu
      WRITE( NOUT, '(A)')
     &' ',
     &' This program converts a .qpt file (as produced by hole) ',
     &'  to something else.',
     &' Output options',
     &' ''A'' to/from ascii version of original .qpt (can then edit)',
     &' ''C'' A .qpt file in which dots are replaced by 3D crosses',
     &' ''L'' A .qpt file with long lines split into smaller sections',
     &'       (useful for proper depth queueing in qplot)',
     &' ''I'' InsightII format',
     &' ''R'' Rasmol format',
     &' ''S'' Sybyl format',
     &' ''K'' to David C. Richardson''s kinemage format',
     &' ''O'' for use with O program',
     &' ''V'' to Virtual Reality Markup Language',
     &' ''D'' to VMD format'
    
      CALL PROMPT( NOUT, 
     &' Enter conversion option character <stop program>: ')

C read control string
      READ( NIN, '(A1)', END= 55555, ERR= 55555) COM1
      CALL VTCLEAR( NOUT)
      
C convert to upper case
      CALL UCASE( COM1)

C write a blank line
      WRITE( NOUT, *)

C option?
      IF     (COM1.EQ.'A') THEN
C use old hydasc program to convert to ascii
        CALL HYDASC

      ELSEIF (COM1.EQ.'C') THEN
C use old qptdot program to convert all dots to crosses 
        CALL QPTDOT

      ELSEIF (COM1.EQ.'L') THEN
C old qpt_split_long_line program
        CALL QPTSPL

      ELSEIF (COM1.EQ.'I') THEN
C use old qpt_insight program to convert to insight format
        CALL QPTINS

      ELSEIF (COM1.EQ.'S') THEN
C use old qpt_sybyl program to convert to insight format
        CALL QPTSYB
 
      ELSEIF (COM1.EQ.'R') THEN
C use old qpt_sybyl program to convert to insight format
        CALL QPTRAS

      ELSEIF (COM1.EQ.'K') THEN
C convert to kinemage
       CALL QPTKIN

      ELSEIF (COM1.EQ.'O') THEN
C convert to O format
       CALL QPTO

      ELSEIF (COM1.EQ.'V') THEN
C convert to O format
       CALL QPTVRM

      ELSEIF (COM1.EQ.'D') THEN
C convert to D format
       CALL QPTVMD

      ELSEIF (COM1.EQ.' ') THEN
C DO NOTHING - the default

      ELSE
        WRITE( NOUT, '(A)')
     &' Sorry unrecognized option'//CHAR(7)
      ENDIF

55555 STOP 'FORTRAN STOP qpt_conv: normal completion'
      END
