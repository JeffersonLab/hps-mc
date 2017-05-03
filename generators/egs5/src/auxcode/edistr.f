!-------------------------------edistr.f--------------------------------
! Version: 051219-1435
! Reference:  Adapted from a program written by D.W.O.R. (Aug 1985)
!-----------------------------------------------------------------------
!23456789|123456789|123456789|123456789|123456789|123456789|123456789|12

! ----------------------------------------------------------------------
! Auxiliary subroutine for use with the EGS5 Code System
! ----------------------------------------------------------------------
! This routine returns an initial kinetic energy given the cumulative
! distribution function (CDF) for the source spectrum stored in ECDF,
! the energy bin tops in Ebin and the minimum energy in EbinMin, all
! of which are passed in COMMON/EDATA/.
! ----------------------------------------------------------------------

      subroutine edistr(ekedum)

      implicit none

      include 'include/egs5_h.f'               ! Main EGS5 "header" file
      include 'auxcommons/aux_h.f'        ! Auxiliary-code "header" file

      include 'auxcommons/edata.f'               ! Auxiliary-code COMMON

      real*8 ekedum,rnnow                                    ! Arguments
      real*8 elow                                      ! Local variables
      integer i

!    ----------------------------------
!    Sample to determine the energy bin
!    ----------------------------------
      call randomset(rnnow)
      i=0
 1    continue
      i = i + 1
      if(encdf(i) .le. rnnow) go to 1

      if (i .gt. 1) then
        elow = ebin(i-1)
      else
        elow = ebinmin
      end if

!     -------------------------------------
!     Sample to determine energy within bin
!     -------------------------------------
      call randomset(rnnow)
      ekedum = elow + rnnow*(ebin(i) - elow)

      return

      end

!--------------------------last line of edistr.f------------------------
