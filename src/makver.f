! (c) 1996 Oliver Smart & Birkbeck College, All rights reserved
! (c) 1997 Oliver Smart 
! (c) 2014,2015 SmartSci Limited 
      program makver
      implicit none
! little program to run which writes s/r vertim giving 
! header information for the hole executable and other programs
      character*80 line
      CHARACTER*300 HoleVersion, HoleDate, HoleBuild, HoleBuildDetail
! build information for proper release from environment variables
      CALL GET_ENVIRONMENT_VARIABLE( "HoleVersion", HoleVersion)
      CALL GET_ENVIRONMENT_VARIABLE( "HoleDate", HoleDate)
      CALL GET_ENVIRONMENT_VARIABLE( "HoleBuild", HoleBuild)
      CALL GET_ENVIRONMENT_VARIABLE( "HoleBuildDetail", HoleBuildDetail)
      IF (LEN_TRIM(HoleVersion).LE.1) HoleVersion = "?.?.?"
      IF (LEN_TRIM(HoleDate).LE.1) HoleDate = "date unknown"
      IF (LEN_TRIM(HoleBuild).LE.1) HoleBuild = "3rd party build"
      IF (LEN_TRIM(HoleBuildDetail).LE.1) HoleBuildDetail = 
     &"3rd party build, Strictly no distribution to others\n" //
     &"3rd party build, Strictly no distribution to others\n" //
     &"3rd party build, Strictly no distribution to others\n" 

      WRITE(*,*) 'debug HoleVersion= ', HoleVersion
      WRITE(*,*) 'debug HoleDate= ', HoleDate
      WRITE(*,*) 'debug HoleBuild= ', HoleBuild
      WRITE(*,*) 'debug HoleBuildDetail= ', HoleBuildDetail


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
     &'     &'' HOLE release 2.2.004 (15 Oct 2014)'',', 
     &'     &'' Program linked at '//line(1:index(line,'   ')-1)//''',' 

! find out who linked it
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
! compile this s/r need to know the compilier for now hardcode
      call system('gfortran -c -O vertim.f')
! hole version
      call system('ar rv hole.a vertim.o')
      call system('rm makver.temp_file vertim.o')
      end
