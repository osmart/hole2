! (c) 1996 Oliver Smart & Birkbeck College, All rights reserved
! (c) 1997 Oliver Smart 
! (c) 2014,2015 SmartSci Limited 
      PROGRAM MAKVER
      IMPLICIT NONE
! little program to run which writes s/r vertim giving 
! header information for the hole executable and other programs
      CHARACTER*300 HoleVersion, HoleDate, HoleBuild, HoleBuildDetail
      CHARACTER*300 HoleRestrict
! build information for proper release from environment variables
      CALL GET_ENVIRONMENT_VARIABLE( "HoleVersion", HoleVersion)
      CALL GET_ENVIRONMENT_VARIABLE( "HoleDate", HoleDate)
      CALL GET_ENVIRONMENT_VARIABLE( "HoleBuild", HoleBuild)
      CALL GET_ENVIRONMENT_VARIABLE( "HoleBuildDetail", HoleBuildDetail)
      CALL GET_ENVIRONMENT_VARIABLE( "HoleRestrict", HoleRestrict)
      IF (LEN_TRIM(HoleVersion).LE.1) HoleVersion = 
     &"SOURCE DISTRIBUTION"
      IF (LEN_TRIM(HoleDate).LE.1) HoleDate = "?"
      IF (LEN_TRIM(HoleBuild).LE.1) HoleBuild = "3rd party build"
      IF (LEN_TRIM(HoleBuildDetail).LE.1) HoleBuildDetail = 
     &"3rd party build, Strictly no distribution to others." 
      IF (LEN_TRIM(HoleRestrict).LE.1) HoleRestrict = 
     &"Not to be used without a signed license agreement"

      CALL SYSTEM('rm -f vertim.f')
      OPEN( 1, FILE='vertim.f', STATUS='NEW')
      WRITE( 1, '(A)')
     &'      SUBROUTINE VERTIM( RESOUT)',
     &'      IMPLICIT NONE',
     &'C s/r which gives the date of linking written by program makver',
     &'      INTEGER RESOUT',
     &'      WRITE(RESOUT, ''(A)'') ',
     &'     &'' '',', 
     &'     &'' For help on HOLE suite see''//',
     &'     &''  http://www.smartsci.uk/hole/'',',
     &'     &'' '',' 

      WRITE( 1, '(A)')
     &'     &'' HOLE release '//TRIM(HoleVersion)//' ('//
     &                                  TRIM(HoleDate)//') '', '
      WRITE( 1, '(A)')
     &'     &'' '',', 
     &'     &'' '//TRIM(HoleBuildDetail)//' '', '
      WRITE( 1, '(A)')
     &'     &'' '//TRIM(HoleRestrict)//' '' '

      WRITE( 1, '(A)')
     &'      RETURN',
     &'      END'
      CLOSE(1)

! compile this s/r need to know the compilier for now hardcode
      call system('gfortran -c -O vertim.f')
! hole version
      call system('ar rv hole.a vertim.o')
      call system('rm vertim.o')
      end
