!------------------------------egs5_lxray.f-----------------------------
! Version: 051219-1435
! Reference: SLAC-R-730/KEK-2005-8
!-----------------------------------------------------------------------
!23456789|123456789|123456789|123456789|123456789|123456789|123456789|12

      subroutine lxray(ll)

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

      integer lx                                       ! Local variables

      ilxray = ilxray + 1                  ! Count entry into subroutine

      call randomset(rnnow)
      go to (1,2,3) ll

 1    continue
      if (dflx1(7,iz) .eq. 0.) return

      nxray = nxray + 1
      do lx=1,7
        if (rnnow .le. dflx1(lx,iz)) then
          exray(nxray) = elx1(lx,iz)*1.E-3
          return
        end if
      end do
      exray(nxray) = elx1(8,iz)*1.E-3
      return

 2    continue
      if (dflx2(4,iz) .eq. 0.) return

      nxray = nxray + 1
      do lx=1,4
        if (rnnow .le. dflx2(lx,iz)) then
          exray(nxray) = elx2(lx,iz)*1.E-3
          return
        end if
      end do
      exray(nxray) = elx2(5,iz)*1.E-3
      return

 3    continue
      if (dflx3(6,iz) .eq. 0.) return

      nxray = nxray + 1
      do lx=1,6
        if (rnnow .le. dflx3(lx,iz)) then
          exray(nxray) = elx3(lx,iz)*1.E-3
          return
        end if
      end do
      exray(nxray) = elx3(7,iz)*1.E-3
      return

      end

!-----------------------last line of egs5_lxray.f-----------------------
