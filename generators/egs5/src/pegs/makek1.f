!-----------------------------makek1.f----------------------------------
! Version: 051219-1435
! Reference: SLAC-R-730/KEK-2005-8
!-----------------------------------------------------------------------
!23456789|123456789|123456789|123456789|123456789|123456789|123456789|12
!
!  get energy dependent values of k1total, the initial scattering
!  strength to take at the given media
!
      subroutine makek1(emin,emax)

!  globals

      implicit none

      real*8 emax,emin
      real*8 g1pdedx, g1ededx, sumga
      external g1pdedx, g1ededx

      include 'include/egs5_h.f'
      include 'include/egs5_mscon.f'

      include 'pegscommons/dcsstr.f'
      include 'pegscommons/dercon.f'
      include 'pegscommons/mscom.f'
      include 'pegscommons/k1spl.f'
      
!  locals

      integer i
      real*8 ekmax, ekmin, efrac, destp1, destp2, ctol
      real*8 e1, e2, ek1init(NESCPW), pk1init(NESCPW)

!  Energy spacing scheme:  ESTEPE slides from EFRAC_H to EFRAC_L

      ekmax = emax - RM
      ekmin = emin - RM
      nhinge = nmscpw

!  get the constants to compute ESTEPE as a function of E

      destp2 = (efracl-efrach) / dlog(ekmin/ekmax)
      destp1 = efrach - dlog(ekmax) * destp2

!   loop over the log-linear spaced hinges, set the energy point, then 
!   integrate over the scattering power to get the total
!   scattering strength over that interval

      e2 = ekmax
      do i=1,nhinge-1
        efrac = destp1 + destp2 * dlog(e2)
        e1 = e2 * (1.d0 - efrac)
        if(efrac.ge.0.20) then
          ctol = 1.d-3
        else if(efrac.ge.0.05) then
          ctol = 1.d-4
        else
          ctol = 1.d-5
        endif
      
        pk1init(nhinge-i+1) = sumga(g1pdedx,e1+RM,e2+RM,ctol)
        ek1init(nhinge-i+1) = sumga(g1ededx,e1+RM,e2+RM,ctol)

        ehinge(nhinge-i+1) = e2
        e2 = mscate(nhinge-i)*1.d-6
      end do

      !-->  Extrapolate to get first hinge.
      ehinge(1) = ekmin
      efrac = (ehinge(2)-ehinge(1)) / (ehinge(3)-ehinge(2))
      pk1init(1) = pk1init(2) + (pk1init(2) - pk1init(3)) * efrac
      ek1init(1) = ek1init(2) + (ek1init(2) - ek1init(3)) * efrac

      do i = 1, nhinge
        if(ehinge(i) .le. 1.d8) then
          if(ek1init(i) .lt. k1mine) k1mine = ek1init(i)
          if(ek1init(i) .gt. k1maxe) k1maxe = ek1init(i)
          if(pk1init(i) .lt. k1minp) k1minp = pk1init(i)
          if(pk1init(i) .gt. k1maxp) k1maxp = pk1init(i)
        endif
      end do

      !-->  get splines for use later in PWLF1
100   call spline(ehinge,ek1init,ak1e,bk1e,ck1e,dk1e,0.d0,0.d0,nhinge)
      call spline(ehinge,pk1init,ak1p,bk1p,ck1p,dk1p,0.d0,0.d0,nhinge)

      return
      end
!-------------------------last line of makek1.f-------------------------
