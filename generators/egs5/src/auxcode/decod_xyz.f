!------------------------------decod_xyz.f-----------------------------
! Version: 051219-1435
! Reference: 030823-1730 by H. Hirayama and Y. Namito
!-----------------------------------------------------------------------
!23456789|123456789|123456789|123456789|123456789|123456789|123456789|12

! Subroutine to determine the i,j,k indices, for standard x-y-z 
! geometry EGS User Codes, given the region.  The first parameter is 
! the region number and next three are the decoded i,j,k indices.
! ----------------------------------------------------------------------
!
! -------------------
! SUBROUTINE ARGUMENT
! -------------------
!  irl   : region number
!  i     : x-position number
!  j     : y-position number
!  k     : z-position number
!  imax  : x-bin number
!  ijmax : imax*jmax jmax is y-bin number
!
! ----------------------------------------------------------------------
      subroutine decod_xyz(irl,i,j,k,imax,ijmax)

      implicit none

      integer i,ijmax,imax,irl,j,k

      i = mod (irl-1,imax)
      if (i .eq. 0) i = imax

      k = 1 + (irl -1 - i)/ijmax
      j = 1 + (irl -1 - i - (k -1 )*ijmax)/imax

      return
      end

!---------------------------- end of decodir.f ------------------------
