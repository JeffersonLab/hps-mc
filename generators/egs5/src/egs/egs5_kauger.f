!-----------------------------egs5_kauger.f-----------------------------
! Version: 051219-1435
! Reference: SLAC-R-730/KEK-2005-8
!-----------------------------------------------------------------------
!23456789|123456789|123456789|123456789|123456789|123456789|123456789|12

      subroutine kauger

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

      integer kaug                                     ! Local variables

      ikauger = ikauger + 1                ! Count entry into subroutine

      if (dfkaug(13,iz) .eq. 0.) return

      nauger = nauger + 1
      call randomset(rnnow)
      do kaug=1,13
        if (rnnow .le. dfkaug(kaug,iz)) then
          eauger(nauger) = ekaug(kaug,iz)*1.E-3
          go to 1
        end if
      end do
      eauger(nauger) = ekaug(14,iz)*1.E-3

 1    continue
      if (kaug .eq. 1) then
!       ==============
        call lshell(1)
!       ==============
!       ==============
        call lshell(1)
!       ==============
      else if (kaug .eq. 2) then
!       ==============
        call lshell(1)
!       ==============
!       ==============
        call lshell(2)
!       ==============
      else if (kaug .eq. 3) then
!       ==============
        call lshell(1)
!       ==============
!       ==============
        call lshell(3)
!       ==============
      else if (kaug .eq. 4) then
!       ==============
        call lshell(2)
!       ==============
!       ==============
        call lshell(2)
!       ==============
      else if (kaug.eq.5) then
!       ==============
        call lshell(2)
!       ==============
!       ==============
        call lshell(3)
!       ==============
      else if (kaug .eq. 6) then
!       ==============
        call lshell(3)
!       ==============
!       ==============
        call lshell(3)
!       ==============
      else if (kaug .eq. 7 .or. kaug .eq. 10) then
!       ==============
        call lshell(1)
!       ==============
      else if (kaug .eq. 8 .or. kaug .eq. 11) then
!       ==============
        call lshell(2)
!       ==============
      else if (kaug .eq. 9 .or. kaug .eq. 12) then
!       ==============
        call lshell(3)
!       ==============
      else
        return
      end if

      return
      end

!-----------------------last line of egs5_kauger.f----------------------
