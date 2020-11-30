C Copyright 1996 Oliver Smart & Birkbeck College
C           1997 Oliver Smart
C           2014-2015 SmartSci Ltd.
C           2016 Oliver Smart
C
C Licensed under the Apache License, Version 2.0 (the "License");
C you may not use this file except in compliance with the License.
C You may obtain a copy of the License at
C
C http://www.apache.org/licenses/LICENSE-2.0
C
C Unless required by applicable law or agreed to in writing, software
C distributed under the License is distributed on an "AS IS" BASIS,
C WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
C See the License for the specific language governing permissions and
C limitations under the License.
      PROGRAM MAKVER
      IMPLICIT NONE
! little program to run which writes s/r vertim giving 
! header information for the hole executable and other programs
      CHARACTER*300 HoleVersion, HoleDate, HoleBuild, HoleBuildDetail
      CHARACTER*300 HoleRestrict
! Fortran compiler in environment variable FC      
      CHARACTER*256 FC
! build information for proper release from environment variables
      CALL GET_ENVIRONMENT_VARIABLE( "HoleVersion", HoleVersion)
      CALL GET_ENVIRONMENT_VARIABLE( "HoleDate", HoleDate)
      CALL GET_ENVIRONMENT_VARIABLE( "HoleBuild", HoleBuild)
      CALL GET_ENVIRONMENT_VARIABLE( "HoleBuildDetail", HoleBuildDetail)
      CALL GET_ENVIRONMENT_VARIABLE( "HoleRestrict", HoleRestrict)
      
      CALL GET_ENVIRONMENT_VARIABLE( "FC", FC)
      IF (LEN_TRIM(FC).LE.1) FC = "gfortran"
      
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
     &'     &''  http://www.holeprogram.org/'',',
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
      call system(TRIM(FC) // ' -c -O vertim.f')
! hole version
      call system('ar rv hole.a vertim.o')
      call system('rm vertim.o')
      end
