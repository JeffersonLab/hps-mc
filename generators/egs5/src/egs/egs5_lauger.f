!-----------------------------egs5_lauger.f-----------------------------
! Version: 051219-1435
! Reference: SLAC-R-730/KEK-2005-8
!-----------------------------------------------------------------------
!23456789|123456789|123456789|123456789|123456789|123456789|123456789|12

      subroutine lauger(ll)

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

      integer laug                                     ! Local variables

      ilauger = ilauger + 1                ! Count entry into subroutine

      call randomset(rnnow)
      go to (1,2,3) ll

 1    continue
      if (dfl1aug(5,iz) .eq. 0.) return

      nauger = nauger + 1
      do laug=1,5
        if (rnnow .le. dfl1aug(laug,iz)) then
          eauger(nauger) = el1aug(laug,iz)*1.E-3
          return
        end if
      end do
      eauger(nauger) = el1aug(6,iz)*1.E-3
      return

 2    continue
      if (dfl2aug(5,iz) .eq. 0.) return

      nauger = nauger + 1
      do laug=1,5
        if (rnnow .le. dfl2aug(laug,iz)) then
          eauger(nauger) = el2aug(laug,iz)*1.E-3
          return
        end if
      end do
      eauger(nauger) = el2aug(6,iz)*1.E-3
      return

 3    continue
      if (dfl3aug(5,iz) .eq. 0.) return

      nauger = nauger + 1
      do laug=1,5
        if (rnnow .le. dfl3aug(laug,iz)) then
          eauger(nauger) = el3aug(laug,iz)*1.E-3
          return
        end if
      end do
      eauger(nauger) = el3aug(6,iz)*1.E-3
      return

      end

!-----------------------last line of egs5_lauger.f----------------------
