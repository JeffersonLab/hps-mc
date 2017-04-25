!-----------------------------egs5_lshell.f-----------------------------
! Version: 051219-1435
! Reference: SLAC-R-730/KEK-2005-8
!-----------------------------------------------------------------------
!23456789|123456789|123456789|123456789|123456789|123456789|123456789|12

      subroutine lshell(ll)

      implicit none

      include 'include/egs5_h.f'               ! Main EGS5 "header" file

      include 'include/egs5_bounds.f'    ! COMMONs required by EGS5 code
      include 'include/egs5_edge.f'
      include 'include/egs5_epcont.f'
      include 'include/egs5_media.f'
      include 'include/egs5_misc.f'
      include 'include/egs5_photin.f'
      include 'include/egs5_stack.f'
      include 'include/egs5_uphiot.f'
      include 'include/egs5_useful.f'

      include 'include/counters.f'       ! Additional (non-EGS5) COMMONs

      real*8 rnnow                                           ! Arguments
      integer ll

      integer ickflg                                   ! Local variables

      ilshell = ilshell + 1                ! Count entry into subroutine

      ickflg = 0
      if (ll .eq. 2) go to 1
      if (ll .eq. 3) go to 2
      call randomset(rnnow)
      ebind = eedge(2,iz)*1.E-3
      if (rnnow .gt. omegal1(iz)) then
        if (rnnow .le. omegal1(iz) + f12(iz)) then
          ickflg = 1
          go to 1
        else if (rnnow .le. omegal1(iz) + f12(iz) + f13(iz)) then
          ickflg = 1
          go to 2
        else
!         ==============
          call lauger(1)
!         ==============
        end if
      else
!       =============
        call lxray(1)
!       =============
      end if
      return

 1    continue
      call randomset(rnnow)
      if (ickflg .eq. 0) then
        ebind = eedge(3,iz)*1.E-3
      end if
      if (rnnow .gt. omegal2(iz)) then
        if (rnnow .le. omegal2(iz) + f23(iz)) then
          ickflg = 1
          go to 2
        else
!         ==============
          call lauger(2)
!         ==============
        end if
      else
!       =============
        call lxray(2)
!       =============
      end if
      return

 2    continue
      if (ickflg .eq. 0) ebind = eedge(4,iz)*1.E-3
      call randomset(rnnow)
      if (rnnow .gt. omegal3(iz)) then
!       ==============
        call lauger(3)
!       ==============
      else
!       =============
        call lxray(3)
!       =============
      end if

      return

      end

!-----------------------last line of egs5_lshell.f----------------------
