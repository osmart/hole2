      SUBROUTINE SPHPEG( NOUT, SOUT, LERR,
     &               DOTDEN, PTMAX, PTNO, PTXYZ,
     &               SPMAX, SPNO, SPXYZ, SPRAD, SPEFFR, SPLAST,
     &		     SP_PEG_ACC,
     &               SOSOUT,
     & 		     SAMPLE, CVECT, PERPVE, PERPVN,
     &		     CONNR, ENDRAD, PROBE_PLUS, PROBE_NEG, PEGRAT,
     &               SP_TDO, SP_ENC, SP_CIRCRAD, SP_PEG_FLAG, 
     &		     SP_PEG_RAD, SHELL_ADD) 
      IMPLICIT NONE
C ********************************************************************
C *                                                                  *
C * This software is an unpublished work containing confidential and *
C * proprietary information of Oliver Smart. Use, disclosure,    *
C * reproduction and transfer of this work without the express       *
C * written consent of Oliver Smart are prohibited. This notice  *
C * must be attached to all copies or extracts of the software.      *
C *                                                                  *
C * (c) 2000 Oliver Smart *
C *                                                                  *
C ********************************************************************
C
C s/r to do anisotropic peg calcs
C
C Modification history:
C
C Date  Author          Modification
C 12/00 O.S.S.		First version adapted from s/r SPHQPU
C

C passed vbles *****************

C screen, keyboard
      INTEGER                   NOUT

C output file stream (already opened)
      INTEGER                   SOUT

C error indicator
      LOGICAL			LERR

C dot density to be used in output
      INTEGER                   DOTDEN

C program uses a sphere of isotropically distributed points
C on a sphere of unit radius. PTMAX is the array bound,
C PTNO is the number of dots (dependent on dotden)
      INTEGER                   PTMAX, PTNO
      DOUBLE PRECISION          PTXYZ( 3, PTMAX)

C sphere centre storage arrays
C maximum number, actual number, co-ords and radius
C logical indicates whether the record is the last centre
      INTEGER                   SPMAX, SPNO
      DOUBLE PRECISION          SPXYZ( 3, SPMAX), SPRAD( SPMAX)
      LOGICAL                   SPLAST( SPMAX)

      ! need effective radius for colouring connolly
      DOUBLE PRECISION		SPEFFR( SPMAX)

! flag to indicate that the sphere is peg accessible
!  0: not peg accessible
!  1: accessible from +ve end
! -1: accessible from -ve end
! new with SHELL_ADD 
!  2: in the shell (within distance shell_add of a peg sphere)
! -2: ditto
      INTEGER			SP_PEG_ACC( SPMAX)

! new vbles
! SCOTDO -> SP_TDO
! SCOENC -> SP_ENC
! SCOCIRCRAD -> SP_CIRCRAD
! n.b. vble may now be changed in calculation if sphere has no intersection
! with plane under consideration
      LOGICAL			SP_TDO( SPMAX)
C s/r coarea needs a store for the east/north coords of
C each point on grid - these are measured relative to the
C initial point
! SCOCIRCRAD: the radius of the point in the plane under consideration
! in the case of calls from concal this we equal scocrad
      DOUBLE PRECISION		SP_ENC( 2, SPMAX),
     &				SP_CIRCRAD( SPMAX)
! working flag for s/r coarea
      LOGICAL			SP_PEG_FLAG( SPMAX)
! now with  SHELL_ADD need to modify the radius of some spheres
! normally this would be equal to sprad but if sp_peg_acc = +/-2
! then this radius will be lower
      DOUBLE PRECISION		SP_PEG_RAD( SPMAX)	

      
C doing sos output - only do dots and add on a rough surface normal
      LOGICAL			SOSOUT

! vbles for forpeg option - hole channel vector etc.
      DOUBLE PRECISION		CVECT(3)
! unit vectors in plane normal to cvect
! needed to a number of routines - originally h2dmap but now conn
! and addend
! these are normal - first is east direction (x on plane)
!                    and second is north     (y on plane)
      DOUBLE PRECISION		PERPVE(3), PERPVN(3)
! a sampling distance - the distance between planes
      DOUBLE PRECISION		SAMPLE
! option CONN introduced 1/00 - 
! CONNR(1) is the connolly surface probe radius (default 1.15Angs)
!          the option is turned off by setting this radius negative
! CONNR(2) is the grid size to be used in the calculation - default 1/2 probe
      DOUBLE PRECISION		CONNR(2)
! the radius at which an end is reached
      DOUBLE PRECISION		ENDRAD


! vbles for working peg effect on conductance -
! first number the ratio of conductance with PEG about to water
! second number is the ratio for non-penetrating PEG
      DOUBLE PRECISION		PEGRAT(2)       

! probe radius - +ve direction and -ve
      DOUBLE PRECISION		PROBE_PLUS, PROBE_NEG

! (n037) additional shell around peg spheres where the 
! conductivity of the solvent is reduced.
      DOUBLE PRECISION		SHELL_ADD


C internals ********************

      INTEGER			SCOUNT, NCOUNT, XCOUNT ! loop counts - spheres
      
      DOUBLE PRECISION		CCOORD, MAXCCOORD, MINCCOORD ! minimum & maximum channel coords
      DOUBLE PRECISION		MAXCCOORD_REFF, MINCCOORD_REFF ! effective radii of the ends
      
      ! accessible list - maximum number of entries
      INTEGER			ACC_LIST_TOT, ACC_LIST_MAX
      PARAMETER(		ACC_LIST_MAX = 150000)
      INTEGER			ACC_LIST_ENTRY(ACC_LIST_MAX)
      INTEGER			ACC_LIST_CHECK
      INTEGER 			NUM_PLUS,  NUM_MINUS, NUM_SHELL 
      INTEGER			ALIST ! An atomic list number
      DOUBLE PRECISION		RLIST ! the relevant probe radius to be used
      DOUBLE PRECISION		CCLIST
      DOUBLE PRECISION		DIST2, DIST, NEWRAD, CHECKD2     

      DOUBLE PRECISION		CC_COUNT ! Channel coord count
      DOUBLE PRECISION		CENTRE(3), CENTROID(3), CENTROID_CCOORD

      DOUBLE PRECISION 		REQUIV_ALL, REQUIV_PEG

      INTEGER			QPTOUT ! stream for binary output
      INTEGER			KEEP_COUNT
      
      DOUBLE PRECISION 		RESIST_FREE, ! resistance without peg
     &				RESIST_PEG   ! ditto with (sum sample/area)


      DOUBLE PRECISION		PI, AREA_ALL, AREA_PEG
      DOUBLE PRECISION		SLAB_FREE, SLAB_ACC, SLAB_NON, SLAB_TOT 

C end of decs ******************


      RESIST_FREE = 0. ! initialize resistance sums
      RESIST_PEG  = 0.
      PI = 2.*ASIN(1.)

      ! turn of qpt out if doing .sph output
      IF (SOSOUT) THEN
        QPTOUT = -1
      ELSE
        QPTOUT = SOUT
      ENDIF
      KEEP_COUNT = 0

      WRITE( NOUT, *)'call to sphpeg - initial vbles'
      WRITE( NOUT, *)'  spno  = ', SPNO
      WRITE( NOUT, *)'  sample= ', SAMPLE
      WRITE( NOUT, *)'  cvect= ', CVECT
      WRITE( NOUT, *)'  connr= ', CONNR
      WRITE( NOUT, *)'  endrad= ', ENDRAD
      WRITE( NOUT, *)'  pegrat= ', PEGRAT
      WRITE( NOUT, *)'  probe_plus= ', PROBE_PLUS
      WRITE( NOUT, *)'  probe_neg=  ', PROBE_NEG
      WRITE( NOUT, *)'  shell_add= ', SHELL_ADD

      ! need sample, cvect etc. to be able to work
      ! test for these
      IF ( (SAMPLE.LT.-1E9) .OR. (CVECT(1).LT.-1E9) .OR.
     &     (CVECT(1).LT.-1E9) .OR. (CONNR(1).LT.-1E9) .OR.
     &     (PERPVE(1).LT.-1E9) .OR. (PERPVN(1).LT.-1E9) .OR.
     &     (ENDRAD.LT.-1E9)                                  ) THEN
        LERR = .TRUE.
        WRITE( NOUT, '(A)')
     &' Have not found one or more of the', 
     &'    SAMPLE, CVECT, PERPVE, PERPVN, CONNR, ENDRAD',
     &' lines required in the .sph input file.',
     &' NB this file must be produced with the FORPEG card',
     &'    in the HOLE input file.'
        GOTO 55555
      ENDIF

      ! go thru the sphere records - find 
      ! (a) the maximum and minimum coords along channel vector
      ! (b) the centroid of all sphere records
      MAXCCOORD = -1D10
      MINCCOORD = +1D10
      CENTROID(1) = 0D0
      CENTROID(2) = 0D0
      CENTROID(3) = 0D0
      DO SCOUNT = 1, SPNO ! go thru spheres
        ! find channel coord of the sphere
	CCOORD = CVECT(1)*SPXYZ(1,SCOUNT) +
     &           CVECT(2)*SPXYZ(2,SCOUNT) +
     &           CVECT(3)*SPXYZ(3,SCOUNT) 
        ! do not count the end records for the coords
	! or coords with an SPEFFR greater than 999
	IF ( (.NOT.SP_PEG_FLAG(SCOUNT)) .AND.
     &       (SPEFFR(SCOUNT).LT.999.)   .AND.
     &       (SPRAD(SCOUNT).LT.ENDRAD)      ) THEN
          KEEP_COUNT = KEEP_COUNT+1
          IF (CCOORD.GT.MAXCCOORD) THEN
	    MAXCCOORD = CCOORD 
	    MAXCCOORD_REFF = SPEFFR(SCOUNT)
	  ENDIF
          IF (CCOORD.LT.MINCCOORD) THEN
	    MINCCOORD = CCOORD 
	    MINCCOORD_REFF = SPEFFR(SCOUNT)
	  ENDIF
	  CENTROID(1) = CENTROID(1) + SPXYZ(1,SCOUNT)
	  CENTROID(2) = CENTROID(2) + SPXYZ(2,SCOUNT)
	  CENTROID(3) = CENTROID(3) + SPXYZ(3,SCOUNT)
	ENDIF  
      ENDDO ! go thru spheres
      CENTROID(1) = CENTROID(1)/FLOAT(KEEP_COUNT)
      CENTROID(2) = CENTROID(2)/FLOAT(KEEP_COUNT)
      CENTROID(3) = CENTROID(3)/FLOAT(KEEP_COUNT)
      ! also need the channel coordinate of the centroid
      CENTROID_CCOORD =  CVECT(1)*CENTROID(1) +
     &                   CVECT(2)*CENTROID(2) +
     &                   CVECT(3)*CENTROID(3)



      WRITE( NOUT, '(A,I9,A)') 
     &' Have found ', KEEP_COUNT, ' records that are bounded'

      
      WRITE( NOUT, '(A,F12.3,A)') 
     &' Maximum channel coord = ', MAXCCOORD, ' angs ',
     &'    effective radius of this = ', MAXCCOORD_REFF, ' angs ',
     &' Minimum channel coord = ', MINCCOORD,' angs ',
     &'    effective radius of this = ', MINCCOORD_REFF, ' angs '
! work out the contribution to the two resistances for the ends
      RESIST_FREE = 1./(4*MAXCCOORD_REFF) + 1./(4*MAXCCOORD_REFF)
      RESIST_PEG  = RESIST_FREE/PEGRAT(1)
      WRITE( NOUT, '(A,F12.3,A)') 
     &' end resistance for peg-free = ', RESIST_FREE, ' units ',
     &' end resistance for with peg = ', RESIST_PEG,  ' units '

! now go thru the spheres and flag
! (a) all those that are at the ends with radius above
!     each the critical radius
      ACC_LIST_TOT = 0
      NUM_PLUS = 0
      NUM_MINUS = 0
      DO SCOUNT = 1, SPNO ! go thru spheres to find ends
        SP_PEG_ACC(SCOUNT) = 0
	CCOORD = CVECT(1)*SPXYZ(1,SCOUNT) +
     &           CVECT(2)*SPXYZ(2,SCOUNT) +
     &           CVECT(3)*SPXYZ(3,SCOUNT) 
        IF ((MAXCCOORD-CCOORD).LT.1E-03) THEN
	  IF (SPRAD(SCOUNT).GT.PROBE_PLUS) THEN
	    SP_PEG_ACC(SCOUNT) = +1 ! sphere is accessible to +ve end
	    ACC_LIST_TOT = ACC_LIST_TOT + 1
	    NUM_PLUS = NUM_PLUS + 1
	    IF (ACC_LIST_TOT.GT.ACC_LIST_MAX) THEN
	       WRITE( NOUT, '(A,I12)') 
     &'ERROR hit array bound ACC_LIST_MAX =', ACC_LIST_MAX,
     &'increase in s/r SPPEG'
               LERR = .TRUE.
               GOTO 55555
	    ENDIF
	    ACC_LIST_ENTRY(ACC_LIST_TOT) = SCOUNT	
	  ENDIF
        ELSEIF ((MINCCOORD-CCOORD).GT.-1E-03) THEN
	  IF (SPRAD(SCOUNT).GT.PROBE_NEG) THEN
	    SP_PEG_ACC(SCOUNT) = -1 ! sphere is accessible to -ve end
	    ACC_LIST_TOT = ACC_LIST_TOT + 1
	    NUM_MINUS = NUM_MINUS + 1
	    IF (ACC_LIST_TOT.GT.ACC_LIST_MAX) THEN
	       WRITE( NOUT, '(A,I12)') 
     &'ERROR hit array bound ACC_LIST_MAX =', ACC_LIST_MAX,
     &'increase in s/r SPPEG'
               LERR = .TRUE.
	    ENDIF
	    ACC_LIST_ENTRY(ACC_LIST_TOT) = SCOUNT	
	  ENDIF 
	ENDIF
      ENDDO ! go thru spheres to find ends

      WRITE( NOUT, '(A, I5,A)') 
     &' Have found ', ACC_LIST_TOT, ' end records ',
     &' of these   ', NUM_PLUS, ' are at positive end',
     &' and        ', NUM_MINUS, ' are at negative end'


! checking loop

      ACC_LIST_CHECK = 0
10    CONTINUE ! do while acc_list_check < ACC_LIST_TOT

        ACC_LIST_CHECK = ACC_LIST_CHECK + 1	
	! list number of the sphere to check
	ALIST =  ACC_LIST_ENTRY(ACC_LIST_CHECK) 
	! radius for the sphere to check
	IF (SP_PEG_ACC(ALIST).GT.0) THEN
	  RLIST = PROBE_PLUS  ! check sphere spawned from +ve
	ELSE
	  RLIST = PROBE_NEG ! check sphere spawned from -ve
	ENDIF
	CCLIST = CVECT(1)*SPXYZ(1,ALIST) +
     &           CVECT(2)*SPXYZ(2,ALIST) +
     &           CVECT(3)*SPXYZ(3,ALIST) 
!        write(nout, *)  ' '
!	 write(nout, *)  'debug ACC_LIST_CHECK= ', ACC_LIST_CHECK
!	 write(nout, *)  'debug ACC_LIST_TOT= ', ACC_LIST_TOT
!        write(nout, *)  'debug alist=', alist
!	 write(nout, *)  'debug SP_PEG_ACC(ALIST)= ',SP_PEG_ACC(ALIST)
     
        ! check sphere number check_list_check
	DO SCOUNT = 1, SPNO ! go thru spheres to find any with overlap
	  ! Q1: is this sphere already accessible?
	  IF (SP_PEG_ACC(SCOUNT).NE.0) THEN !Q1/2
	    ! A1: yes it is already accessed
	    CONTINUE ! no further action
            !write(nout,*) 'debug ', scount, 'already accepted'
	    
	  ! Q2: does this sphere have a relevant radius?	    
	  ELSEIF (SPRAD(SCOUNT).GT.RLIST) THEN !Q1/2
            ! Q3 is the sphere on a neighbooring plane?
	    CCOORD = CVECT(1)*SPXYZ(1,SCOUNT) +
     &               CVECT(2)*SPXYZ(2,SCOUNT) +
     &               CVECT(3)*SPXYZ(3,SCOUNT) 
            ! problem with any end type record for this
	    ! s/r addend produces unusually spaced records
            IF ( (ABS(CCLIST-CCOORD).LT.(1.1*SAMPLE)) .OR.
     &                      SPLAST(ALIST)               ) THEN ! Q3: sphere on neighbooring plane
	      ! Q4: (a) is the centre of the sphere scount within 
	      !         the sphere ALIST?
	      !     (b) is the sphere scount a connolly neighbour
	      !         use 2*gridsize used in hole connolly calc
 	      DIST2 = (SPXYZ(1,SCOUNT)-SPXYZ(1,ALIST))*
     &	              (SPXYZ(1,SCOUNT)-SPXYZ(1,ALIST))   +
     &                (SPXYZ(2,SCOUNT)-SPXYZ(2,ALIST))*
     &	              (SPXYZ(2,SCOUNT)-SPXYZ(2,ALIST))   +	  
     &                (SPXYZ(3,SCOUNT)-SPXYZ(3,ALIST))*
     &	              (SPXYZ(3,SCOUNT)-SPXYZ(3,ALIST)) 	 
	    
             IF (DIST2.LT. (SPRAD(ALIST)*SPRAD(ALIST))) THEN
	     
!	      .AND.
!     &            (DIST2.LT. (4.*CONNR(2)*CONNR(2))     )       
!     &          ) THEN ! Q4
	        ! A4: yes - sphere accepted - add to the list
	        SP_PEG_ACC(SCOUNT) = SP_PEG_ACC(ALIST)
	        ACC_LIST_TOT = ACC_LIST_TOT + 1
	        IF (SP_PEG_ACC(ALIST).GT.0) THEN
                  SP_PEG_ACC(SCOUNT) = +1 ! sphere is accessible to +ve end
	          NUM_PLUS = NUM_PLUS + 1  ! one more +ve
	        ELSE
                  SP_PEG_ACC(SCOUNT) = -1 ! sphere is accessible to -ve end
	          NUM_MINUS = NUM_MINUS + 1  ! one more -ve
	        ENDIF
	        
	        IF (ACC_LIST_TOT.GT.ACC_LIST_MAX) THEN
	          WRITE( NOUT, '(A,I12)') 
     &'ERROR hit array bound ACC_LIST_MAX =', ACC_LIST_MAX,
     &'increase in s/r SPPEG'
                  LERR = .TRUE.
	        ENDIF
	        ACC_LIST_ENTRY(ACC_LIST_TOT) = SCOUNT	
	      ENDIF !Q4
            ENDIF ! Q3: sphere on neighbooring plane
          ENDIF !Q1/2
	ENDDO ! go thru spheres to find any with overlap
      IF (ACC_LIST_CHECK.LT.ACC_LIST_TOT) GOTO 10 ! do while acc_list_check < ACC_LIST_TOT

      WRITE( NOUT, '(/ A/ 3(A, I5,A/))') 
     &
     &' After probing',
     &' Have found ', ACC_LIST_TOT, ' accessible sphere records ',
     &' of these   ', NUM_PLUS, ' are at positive end',
     &' and        ', NUM_MINUS, ' are at negative end'
 


! assign all spheres a default peg rad equal to the original
      DO SCOUNT = 1, SPNO
        SP_PEG_RAD(SCOUNT) = SPRAD(SCOUNT)
      ENDDO

      IF (SHELL_ADD.GT.0.) THEN ! add a shell if required
        NUM_SHELL = 0
      ! look for spheres that are within  shell_add of a peg sphere
        DO SCOUNT = 1, SPNO ! go thru spheres to make shell
	  ! ignore proper spheres
	  IF (SP_PEG_ACC(SCOUNT).EQ.0) THEN ! an inaccessible sphere?
	    ! then look thru all the other spheres
	    DO NCOUNT = 1, SPNO ! thru all other spheres 
	    
	      ! Q: is this a PROPER PEG ACCESSIBLE SPHERE?
	      IF (ABS(SP_PEG_ACC(NCOUNT)).EQ.1) THEN ! proper peg?
	      ! find distance squared between sphere scount and ncount
 	        DIST2 = (SPXYZ(1,SCOUNT)-SPXYZ(1,NCOUNT))*
     &	                (SPXYZ(1,SCOUNT)-SPXYZ(1,NCOUNT))   +
     &                  (SPXYZ(2,SCOUNT)-SPXYZ(2,NCOUNT))*
     &	                (SPXYZ(2,SCOUNT)-SPXYZ(2,NCOUNT))   +	  
     &                  (SPXYZ(3,SCOUNT)-SPXYZ(3,NCOUNT))*
     &	                (SPXYZ(3,SCOUNT)-SPXYZ(3,NCOUNT)) 
                ! is sphere scount in the shell of sphere ncount?	 
	        CHECKD2 = (SPRAD(NCOUNT)+SHELL_ADD)*
     &                   (SPRAD(NCOUNT)+SHELL_ADD)
                IF (DIST2.LT.CHECKD2) THEN !(in shell?)
		  DIST = SQRT(DIST2)
		  ! the radius of a sphere that extends shell_add beyond
		  ! the ncount sphere
		  NEWRAD = SPRAD(NCOUNT) + SHELL_ADD - DIST
		  ! but sphere cannot be larger than the original
		  IF (NEWRAD.GT.SPRAD(SCOUNT)) NEWRAD = SPRAD(SCOUNT)
	          IF (SP_PEG_ACC(SCOUNT).EQ.0) THEN
		    ! have one more
		    NUM_SHELL = NUM_SHELL + 1
		    ! code for access +2 from +ve -2 from -ve
		    SP_PEG_ACC(SCOUNT) = 2*SP_PEG_ACC(NCOUNT)
		    SP_PEG_RAD(SCOUNT) = NEWRAD
		  ELSE
		    ! already a shell record - but is the radius bigger
		    ! from this sphere?
		    IF (NEWRAD.GT. SP_PEG_RAD(SCOUNT)) THEN
		      ! yes then use the larger of the two
		      SP_PEG_ACC(SCOUNT) = 2*SP_PEG_ACC(NCOUNT)
		      SP_PEG_RAD(SCOUNT) = NEWRAD
		    ENDIF
		  ENDIF
	          
	        ENDIF !(in shell?)
     
	      ENDIF ! (proper peg?)
	    
	    
	    ENDDO ! thru all other spheres 
          	  
	  ENDIF! (an inaccessible sphere?)


        ENDDO  ! go thru spheres to make shell

        WRITE( NOUT, '(/ A,F10.3,A/ A,I5,A)') 
     &
     &' Have specified should use a shell of ', SHELL_ADD, ' angs',
     &' This produces another ', NUM_SHELL, ' shell spheres'


      ENDIF ! add a shell if required
 
 
 
! now have a list of accepted spheres - lets output this as an .sph file if told too
      IF (SOSOUT) THEN
! write the +ve end ones with a 0.1 effective rad - -Ve with +20.1
! so that sph_process can  draw to +ve to red, -ve to blue
! OLD
!	DO SCOUNT = 1, SPNO 
!	  IF (SP_PEG_ACC(SCOUNT).NE.0) THEN 
!           WRITE( SOUT, '(A,I4,4X,3F8.3,2F6.2)')
!    &   'ATOM      1  QSS SPH S', -999,
!    &       (SPXYZ(XCOUNT,SCOUNT), XCOUNT= 1, 3),
!    &       SPRAD(SCOUNT), 0.1-10.*REAL(SP_PEG_ACC(SCOUNT)-1) ! effective radius 0.1 for +ve 20
!           IF (SPLAST( SCOUNT)) WRITE( SOUT, '(A)') 'LAST-REC-END'
!
!         ENDIF
!       ENDDO        
	DO SCOUNT = 1, SPNO  ! +1 first to eff rad 0.1
	  IF (SP_PEG_ACC(SCOUNT).EQ.1) THEN 
            WRITE( SOUT, '(A,I4,4X,3F8.3,2F6.2)')
     &   'ATOM      1  QSS SPH S', -999,
     &       (SPXYZ(XCOUNT,SCOUNT), XCOUNT= 1, 3),
     &       SPRAD(SCOUNT), 0.1 ! effective radius 0.1 for +ve 20
            IF (SPLAST( SCOUNT)) WRITE( SOUT, '(A)') 'LAST-REC-END'
          ENDIF
        ENDDO
	DO SCOUNT = 1, SPNO  ! -1 to eff rad 20.0
	  IF (SP_PEG_ACC(SCOUNT).EQ.-1) THEN 
            WRITE( SOUT, '(A,I4,4X,3F8.3,2F6.2)')
     &   'ATOM      1  QSS SPH S', -999,
     &       (SPXYZ(XCOUNT,SCOUNT), XCOUNT= 1, 3),
     &       SPRAD(SCOUNT), 20.0 ! effective radius 0.1 for +ve 20
            IF (SPLAST( SCOUNT)) WRITE( SOUT, '(A)') 'LAST-REC-END'
          ENDIF
        ENDDO
	! write shell 
        DO SCOUNT = 1, SPNO  ! +2 to eff rad 2.0
	  IF (SP_PEG_ACC(SCOUNT).EQ.2) THEN 
            WRITE( SOUT, '(A,I4,4X,3F8.3,2F6.2)')
     &   'ATOM      1  QSS SPH S', -999,
     &       (SPXYZ(XCOUNT,SCOUNT), XCOUNT= 1, 3),
     &       SP_PEG_RAD(SCOUNT), 2.0 ! effective radius 0.1 for +ve 20
            IF (SPLAST( SCOUNT)) WRITE( SOUT, '(A)') 'LAST-REC-END'
          ENDIF
        ENDDO
        DO SCOUNT = 1, SPNO  ! -2 to eff rad 2.1	  
	IF (SP_PEG_ACC(SCOUNT).EQ.-2) THEN 
            WRITE( SOUT, '(A,I4,4X,3F8.3,2F6.2)')
     &   'ATOM      1  QSS SPH S', -999,
     &       (SPXYZ(XCOUNT,SCOUNT), XCOUNT= 1, 3),
     &       SP_PEG_RAD(SCOUNT), 2.1 ! effective radius 0.1 for +ve 20
            IF (SPLAST( SCOUNT)) WRITE( SOUT, '(A)') 'LAST-REC-END'
          ENDIF
        ENDDO
	        
        GOTO 55555
      ENDIF       

! have got input .sph files - now need to go thru planes from 
! MINCCOORD to MAXCCOORD using sample
      DO CC_COUNT= MINCCOORD, MAXCCOORD, SAMPLE
!dbug       cc_count = 21.066 
! now need to work out area of sections 
! through plane with channel_coord=CC_COUNT
! both (a) for all circles that cut through
!      (b) for just peg accessible
! need to supply a center to define plane
        CENTRE(1) = CENTROID(1) + (CC_COUNT-CENTROID_CCOORD)*CVECT(1)
	CENTRE(2) = CENTROID(2) + (CC_COUNT-CENTROID_CCOORD)*CVECT(2)
        CENTRE(3) = CENTROID(3) + (CC_COUNT-CENTROID_CCOORD)*CVECT(3)
	write( nout, *) 'debug cc_count= ', cc_count
	write( nout, *) 'debug centre= ', centre



! vbles correspondence between
!  s/r   ->  this
! coarea      s/r
!-----------------
! SCOMAX -> SPMAX
! SCONUM -> SPNO
! SCOXYZ -> SPXYZ
! SCORAD -> SPRAD
! new vbles
! SCOTDO -> SP_TDO
! SCOENC -> SP_ENC
! SCOCIRCRAD -> SP_CIRCRAD
! first work out the area of all
       ! all spheres are active (must reset every call)
        DO SCOUNT = 1, SPNO
          SP_TDO(SCOUNT) = .TRUE.
        ENDDO 
        IF (QPTOUT.NE.-1) WRITE(QPTOUT) 1.0, 7.0, -55.0, 17.0 ! change to colour 7 green
        CALL COAREA( REQUIV_ALL, NOUT, LERR, 0, ENDRAD, 
     &		     CENTRE, PERPVE, PERPVN, 
     &               SPMAX, SPNO, SPXYZ, SPRAD, SP_TDO,
     &               SP_ENC, SP_CIRCRAD, CVECT, SAMPLE,
     &		     .FALSE., SP_PEG_ACC, SP_PEG_FLAG, QPTOUT, 
     &               SP_PEG_RAD) ! first non-peg
! then just peg accessible
       ! all spheres are active (must reset every call)
        DO SCOUNT = 1, SPNO
          SP_TDO(SCOUNT) = .TRUE.
        ENDDO 
        IF (QPTOUT.NE.-1) WRITE(QPTOUT) 1.0, 3.0, -55.0, 16.0 ! change to colour 7 green
        CALL COAREA( REQUIV_PEG, NOUT, LERR, 0, ENDRAD, 
     &		     CENTRE, PERPVE, PERPVN, 
     &               SPMAX, SPNO, SPXYZ, SPRAD, SP_TDO,
     &               SP_ENC, SP_CIRCRAD, CVECT, SAMPLE,
     &		     .TRUE., SP_PEG_ACC, SP_PEG_FLAG, QPTOUT, ! now peg
     &               SP_PEG_RAD) 
     
        ! 
        IF (REQUIV_PEG.GT.REQUIV_ALL) THEN
	  WRITE(NOUT , '(A,F14.4,A,F14.4/ A)')
     &' Warning RequivPEG= ',REQUIV_PEG, '  >   RequivALL=', 
     &                                            REQUIV_ALL, 
     &' Resetting to be equal'
          REQUIV_PEG = REQUIV_ALL
	ENDIF

        WRITE( NOUT, '(A,3F14.4)')
     &' TAG-CCOORD-RequivALL-RequivPEG ', CC_COUNT, REQUIV_ALL, 
     &                                              REQUIV_PEG

! convert Requiv's to areas
        AREA_ALL = PI*REQUIV_ALL*REQUIV_ALL
        AREA_PEG = PI*REQUIV_PEG*REQUIV_PEG	

! now work out resistance contributions
        SLAB_FREE = SAMPLE/AREA_ALL
        RESIST_FREE = RESIST_FREE + SLAB_FREE       	
        IF (AREA_PEG.GT.0.) THEN
          SLAB_ACC = SAMPLE/(PEGRAT(1)*AREA_PEG)
	ELSE
	  SLAB_ACC = 9E9
	ENDIF
        IF (AREA_PEG.LT.AREA_ALL) THEN
	  SLAB_NON = SAMPLE/(PEGRAT(2)*(AREA_ALL-AREA_PEG))
	ELSE
	  SLAB_NON = 9E9
	ENDIF
        SLAB_TOT = (SLAB_ACC*SLAB_NON)/(SLAB_ACC+SLAB_NON)
        RESIST_PEG = RESIST_PEG + SLAB_TOT 

        WRITE( NOUT, '(A,3F14.4)')
     &' TAG-CCOORD-SLABfree-RESISTfree ', CC_COUNT, SLAB_FREE, 
     &                                              RESIST_FREE
        WRITE( NOUT, '(A,5F14.4)')
     &' TAG-CCOORD-SLABpeg-SLABnon-SLABtot-RESIST_PEG ', CC_COUNT, 
     &   SLAB_ACC, SLAB_NON, SLAB_TOT, RESIST_PEG
    
	WRITE( NOUT, '(A)') 
     &' '

      ENDDO !(CC_COUNT for planes)

! have gone thru all the planes output final result
      WRITE( NOUT, '(A,3F14.4)') 
     &'TAG-PROBEplus-PROBEneg-CONDrat', PROBE_PLUS, PROBE_NEG,
     &                                 RESIST_FREE/RESIST_PEG

55555 RETURN
      END
