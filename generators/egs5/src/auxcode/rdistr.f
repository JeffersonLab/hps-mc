!-------------------------------rdistr.f--------------------------------
! Version: 051219-1435
! Reference:  Adapted from a program written by C. J. Huntzinger 
!             (Nov 1987), which is an adaptation from a program 
!             written by  D.W.O.R. (Aug 1985)
!-----------------------------------------------------------------------
!23456789|123456789|123456789|123456789|123456789|123456789|123456789|12

! ----------------------------------------------------------------------
! Auxiliary subroutine for use with the EGS5 Code System
! ----------------------------------------------------------------------
! This routine returns radial coordinates (ridum,xidum,yidum) given the
! cumulative distribution function (CDF) for the source radial distribu-
! tion stored in RCDF, the radial bin tops in Rbin and the minimum
! radial distance in RbinMin, all passed in COMMON/RDATA/.
! ----------------------------------------------------------------------

      subroutine rdistr(ridum,xidum,yidum)

      implicit none

!     ------------
!     EGS5 COMMONs
!     ------------
      include 'include/egs5_h.f'               ! Main EGS5 "header" file
      include 'include/egs5_uphiot.f'    ! COMMONs required by EGS5 code

!     ----------------------
!     Auxiliary-code COMMONs
!     ----------------------
      include 'auxcommons/aux_h.f'   ! Auxiliary-code "header" file
      include 'auxcommons/rdata.f'          ! Auxiliary-code COMMON

      real*8 ridum,xidum,yidum,rnnow                         ! Arguments
      real*8 rlow,angle                                ! Local variables
      integer i

!    ----------------------------------
!    Sample to determine the radius bin
!    ----------------------------------
      call randomset(rnnow)
      i=0
 1    continue
      i = i + 1
      if(rcdf(i) .le. rnnow) go to 1

      if (i .gt. 1) then
        rlow = rbin(i-1)
      else
        rlow = rbinmin
      end if

!     -------------------------------------
!     Sample to determine radius within bin
!     -------------------------------------
      call randomset(rnnow)
      ridum = rlow + rnnow*(rbin(i) - rlow)

!     -----------------------------------
!     Select the azimuthal angle randomly
!     -----------------------------------
      call randomset(rnnow)
      angle = TWOPI*rnnow
      xidum = ridum*cos(angle)
      yidum = ridum*sin(angle)

      return

      end

!--------------------------last line of rdistr.f------------------------
