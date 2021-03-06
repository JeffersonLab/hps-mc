!------------------------------egs5_kxray.f-----------------------------
! Version: 051219-1435
! Reference: SLAC-R-730/KEK-2005-8
!-----------------------------------------------------------------------
!23456789|123456789|123456789|123456789|123456789|123456789|123456789|12

      subroutine kxray

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

      integer ik                                       ! Local variables

      ikxray = ikxray + 1                  ! Count entry into subroutine

      if (dfkx(9,iz) .eq. 0.) return

      nxray = nxray + 1
      call randomset(rnnow)
      do ik=1,9
        if (rnnow .le. dfkx(ik,iz)) then
          exray(nxray) = ekx(ik,iz)*1.E-3
          go to 1
        end if
      end do
      exray(nxray) = ekx(10,iz)*1.E-3

 1    continue
!                    ==============
      if (ik .eq. 1) call lshell(3)
!                    ==============
!                    ==============
      if (ik .eq. 2) call lshell(2)
!                    ==============
!                    ==============
      if (ik .eq. 3) call lshell(1)
!                    ==============
      return

      end

!-----------------------last line of egs5_kxray.f-----------------------
