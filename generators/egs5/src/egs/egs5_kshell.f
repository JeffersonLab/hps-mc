!-----------------------------egs5_kshell.f-----------------------------
! Version: 051219-1435
! Reference: SLAC-R-730/KEK-2005-8
!-----------------------------------------------------------------------
!23456789|123456789|123456789|123456789|123456789|123456789|123456789|12

      subroutine kshell

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

      ikshell = ikshell + 1                ! Count entry into subroutine

      call randomset(rnnow)
      if (rnnow .gt. omegak(iz)) then
!       ===========
        call kauger
!       ===========
      else
!       ==========
        call kxray
!       ==========
      end if

      return

      end

!-----------------------last line of egs5_kshell.f----------------------
