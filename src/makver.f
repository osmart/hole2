C ********************************************************************
C *                                                                  *
C * This software is an unpublished work containing confidential and *
C * proprietary information of Birkbeck College. Use, disclosure,    *
C * reproduction and transfer of this work without the express       *
C * written consent of Birkbeck College are prohibited. This notice  *
C * must be attached to all copies or extracts of the software.      *
C *                                                                  *
C * (c) 1996 Oliver Smart & Birkbeck College, All rights reserved    *
C * (c) 1997 Oliver Smart                    *
C *                                                                  *
C ********************************************************************
      program makver
      implicit none
C little program to run which writes s/r vertim giving the time of link of
C the program tic. Now used for hole as well
C Adapted 21/11/97 to give who linked it as well
      character*80		line
      call system('date> makver.temp_file')
      call system('rm vertim.f')
      open( 1, file='vertim.f', status='new')
      open( 2, file='makver.temp_file', status='old')
      read( 2, '(a)') line
      close(2)
      call system('rm makver.temp_file')
      write( 1, '(a)')
     &'      SUBROUTINE VERTIM( RESOUT)',
     &'      IMPLICIT NONE',
     &'C s/r which gives the date of linking written by program makver',
     &'      INTEGER		RESOUT',
     &'      WRITE(RESOUT, ''(A/ A,A/ A,A)'') ',
     &'     &'' HOLE release 2.2.002 (12 Jan 2005)'',', 
     &'     &'' Program linked at '//line(1:index(line,'   ')-1)//''',' 

C find out who linked it
      call system('echo $USER > makver.temp_file')
      open( 2, file='makver.temp_file', status='old')
      read( 2, '(a)') line
      close(2)
!      call system('rm makver.temp_file')

! nov 2004 take out last modified (no source dist)
      if (index(line,'osmart').ne.0) line = 'HOLE_DIST'
      write( 1, '(a)')
     &'     &'' by user '//line(1:index(line,'   ')-1)//''','
      write( 1, '(a)')
     &'     &'' For help on hole suite see'',',
     &'     &''  http://hole.biop.ox.ac.uk/hole/help'''
!      WRITE( NOUT,  '(A)')
!     &' For help on hole suite see ',
!     &' http://hole.biop.ox.ac.uk/hole/help'

!      write( 1, '(a)')
!     &'     &'' By user '//line(1:20)//''',',
!     &'     &'' Last modified .f files: '''

!C now add bit which gives the last modified fortran file
!      call system('ls -lt *.f > makver.temp_file')
!      open( 2, file='makver.temp_file', status='old')
!C n.b. the last modified fortran files will always be vertim!
!C so read second line
!      read( 2, '(a)') line
!      read( 2, '(a)') line
!      write( 1, '(a)')
!     &'      WRITE(RESOUT, ''(A)'') ',
!     &'     &''     '//line(35:70)//''''
!C give last 3 modified files
!      read( 2, '(a)') line
!      write( 1, '(a)')
!     &'      WRITE(RESOUT, ''(A)'') ',
!     &'     &''     '//line(35:70)//''''
!      read( 2, '(a)') line
!      write( 1, '(a)')
!     &'      WRITE(RESOUT, ''(A)'') ',
!     &'     &''     '//line(35:70)//''''

      write( 1, '(a)')
     &'      RETURN',
     &'      END'
      close(1)
C compile this s/r
      call system('$FC -c -O vertim.f')
C hole version
      call system('ar rv hole.a vertim.o')
      call system('rm makver.temp_file vertim.o')
      end
