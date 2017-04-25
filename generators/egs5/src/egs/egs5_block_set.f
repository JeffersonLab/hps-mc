!----------------------------egs5_block_set.f---------------------------
! Version: 070117-1205
! Auxillary to BLOCK DATA to initial elements in commons which contain
! arrays of length MXREG.  These cannot be initialized in block data.
!-----------------------------------------------------------------------
!23456789|123456789|123456789|123456789|123456789|123456789|123456789|12

      subroutine block_set

      implicit none

      include 'include/egs5_h.f'               ! Main EGS5 "header" file

      include 'include/egs5_bounds.f'    ! COMMONs required by EGS5 code
      include 'include/egs5_misc.f'
      include 'include/egs5_mscon.f'
      include 'include/egs5_usersc.f'
      include 'include/egs5_ms.f'
      include 'include/egs5_eiicom.f'
      include 'include/egs5_brempr.f'
      include 'include/egs5_userxt.f'
      include 'include/egs5_edge.f'
      include 'include/counters.f'

      integer i

      iblock = iblock + 1

! common/BOUNDS/
      vacdst = 1.d8
      do i = 1, MXREG
        ecut(i) = 0.d0
        pcut(i) = 0.d0
      end do

! common/MISC/
      kmpi = 12
      kmpo = 8
      dunit = 1.d0
      do i = 1, MXREG
        med(i) = 1
        rhor(i) = 0.d0
        k1Hscl(i) = 0.d0
        k1Lscl(i) = 0.d0
        ectrng(i) = 0.d0
        pctrng(i) = 0.d0
        iraylr(i) = 0
        lpolar(i) = 0
        incohr(i) = 0
        iprofr(i) = 0
        impacr(i) = 0
        nomsct(i) = 0
      end do
      med(1) = 0

! common/MS/
      tmxset = .true.

! common/MSCON/
      k1mine = 1.d30
      k1maxe = -1.d30
      k1minp = 1.d30
      k1maxp = -1.d30

! common/USERSC/
      emaxe = 0.d0
      do i = 1, MXREG
        estepr(i) = 0.d0
        esave(i) = 0.d0
      end do

! common/EIICOM/
      ieispl = 0
      neispl = 0
      feispl = 0.d0

! common/BREMPR/
      ibrdst = 0
      iprdst = 0
      ibrspl = 0
      nbrspl = 0
      fbrspl = 0.d0

! common/EDGE/
      do i = 1, MXREG
        iedgfl(i) = 0
        iauger(i) = 0
      end do

! common/USERXT/
      do i = 1, MXREG
        iphter(i) = 0
      end do

      return
      end

!------------------last line of egs5_block_set.f------------------------
