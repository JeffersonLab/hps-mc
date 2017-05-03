!--------------------------------chgtr.f--------------------------------
! Version: 051219-1435
!-----------------------------------------------------------------------
!23456789|123456789|123456789|123456789|123456789|123456789|123456789|12

! ----------------------------------------------------------------------
! Auxiliary (geometry) subroutine for use with the EGS5 Code System
! ----------------------------------------------------------------------
! This subroutine determines ustep is larger than tvalp and, if so,
! changes ustep to tvalp and irnew to irnewp.
! ----------------------------------------------------------------------
! Input arguments:
! ----------------
!   tvalp  = Straight trajectory distance to a boundary surface
!   irnewp = New region that particle may possibly go into next
! ----------------------------------------------------------------------

      subroutine chgtr(tvalp,irnewp)

      implicit none

      include 'include/egs5_h.f'               ! Main EGS5 "header" file

      include 'include/egs5_epcont.f'    ! COMMONs required by EGS5 code

      real*8 tvalp                                           ! Arguments
      integer irnewp

      if (tvalp .le. ustep) then
        ustep = tvalp
        irnew = irnewp
      end if

      return

      end

!---------------------------last line of chgtr.f------------------------
