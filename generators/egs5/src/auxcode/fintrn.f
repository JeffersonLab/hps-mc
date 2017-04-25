!-------------------------------fintrn.f--------------------------------
! Version: 051219-1435
!-----------------------------------------------------------------------
!23456789|123456789|123456789|123456789|123456789|123456789|123456789|12

! ----------------------------------------------------------------------
! Auxiliary (geometry) subroutine for use with the EGS5 Code System
! ----------------------------------------------------------------------
! Subroutine fintrn determines the final coordinates of a particle
! (xfin,yfin,zfin) given a distance (dist) together with the initial
! coordinates of the particle (xtran,ytran,ztran) and its direction 
! cosines (u,v,w).
! ----------------------------------------------------------------------
! SPECIAL NOTE: This routine is for a sphere whose origin is translated
!               along z with respect to the usual EGS coordinate.
!               The variables xtrans, ytrans and ztrans must be defined
!               in AUSGAB and passed in common/TRNDTA/.
! ----------------------------------------------------------------------

      subroutine fintrn(dist,xfin,yfin,zfin)

      implicit none

      include 'include/egs5_h.f'               ! Main EGS5 "header" file

      include 'include/egs5_stack.f'     ! COMMONs required by EGS5 code

      include 'auxcommons/trndta.f'             ! Auxiliary-code COMMONs

      real*8                                                 ! Arguments
     * dist,
     * xfin,yfin,zfin

      xfin = xtran + dist*u(np)
      yfin = ytran + dist*v(np)
      zfin = ztran + dist*w(np)

      return

      end

!-------------------------------fintrn.f--------------------------------
