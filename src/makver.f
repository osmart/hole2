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
C * (c) 2014 SmartSci Limited                *
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
     &'      WRITE(RESOUT, ''(A)'') ',
     &'     &'' '',', 
     &'     &'' For help on HOLE suite see''//',
     &'     &''  http://www.smartsci.uk/hole/'',',
     &'     &'' '',', 
     &'     &'' HOLE release 2.2.003 (07 Oct 2014)'',', 
     &'     &'' Program linked at '//line(1:index(line,'   ')-1)//''',' 

C find out who linked it
      call system('echo $USER > makver.temp_file')
      open( 2, file='makver.temp_file', status='old')
      read( 2, '(a)') line
      close(2)
!      call system('rm makver.temp_file')

! nov 2004 take out last modified (no source dist)
      if (index(line,'osmart').ne.0) then
         write( 1, '(a)')
     &'     &'' built for http://www.smartsci.uk/hole/''//',
     &'     &'' not-for-profit version binary download '',',
     &'     &'' RESTRICTED TO NOT-FOR-PROFIT USAGE ONLY. '',',
     &'     &'' RESTRICTED TO NOT-FOR-PROFIT USAGE ONLY. '',',
     &'     &'' RESTRICTED TO NOT-FOR-PROFIT USAGE ONLY. '',',
     &'     &'' RESTRICTED TO NOT-FOR-PROFIT USAGE ONLY. '''
      else
         write( 1, '(a)')
     &'     &'' by user '//line(1:index(line,'   ')-1)//''','
      endif
      write( 1, '(a)')
     &'      RETURN',
     &'      END'
      close(1)
C compile this s/r need to know the compilier for now hardcode
      call system('gfortran -c -O vertim.f')
C hole version
      call system('ar rv hole.a vertim.o')
      call system('rm makver.temp_file vertim.o')
      end
