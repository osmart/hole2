      SUBROUTINE QHPS( n, ra, rinf)
      implicit none
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
C heap sort routine for depth sorting called from s/r qpspic
C adapted (slightly) from s/r hpsort p329 Numerical Recipies in Fortran
C Press, W.H., Teukolsky, S.A., Vetterling, W.T., Flannery, B.P. 2nd ed, CUP 1992
      integer n
      real ra(n)
      integer i,ir,j,l
      real rra
c extras
      real rinf(2,n), rinfa1, rinfa2

      if (n.lt.2) return
      l = n/2+1
      ir=n
10    continue
        if (l.gt.1) then
          l = l-1
          rra = ra(l)
          RINFA1 = RINF(1,L)
          RINFA2 = RINF(2,L)
        else
          rra = ra(ir)
          RINFA1 = RINF(1,IR)
          RINFA2 = RINF(2,IR)
          ra(ir) = ra(1)
          RINF(1,IR) = RINF(1,1)
          RINF(2,IR) = RINF(2,1)
          ir = ir-1
          if (ir.eq.1) then
            ra(1) = rra
            RINF(1,1) = RINFA1
            RINF(2,1) = RINFA2
            return
          endif
        endif
        i=l
        j=l+l
20      if (j.le.ir) then
          if (j.lt.ir) then
            if (ra(j).lt.ra(j+1)) j = j+1
          endif
          if (rra.lt.ra(j))then
            ra(i)=ra(j)
            RINF(1,I) = RINF(1,j)
            RINF(2,I) = RINF(2,j)
            i=j
            j=j+j
          else
            j=ir+1
          endif
          goto 20
          endif
          ra(i)=rra
          RINF(1,I) = RINFA1
          RINF(2,I) = RINFA2
        goto 10
        END
