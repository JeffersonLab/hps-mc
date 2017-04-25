!-------------------------------finval.f--------------------------------
! Version: 051219-1435
! Reference:
!-----------------------------------------------------------------------
!23456789|123456789|123456789|123456789|123456789|123456789|123456789|12

! ----------------------------------------------------------------------
! Auxiliary (geometry) subroutine for use with the EGS5 Code System
! ----------------------------------------------------------------------
! Subroutine finval determines the final coordinates of a particle
! (xfin,yfin,zfin) given a distance (dist) together with the initial
! coordinates of the particle (x,y,z) and its direction cosines (u,v,w).
! ----------------------------------------------------------------------

      subroutine finval(dist,xfin,yfin,zfin)

      implicit none

      include 'include/egs5_h.f'               ! Main EGS5 "header" file

      include 'include/egs5_stack.f'     ! COMMONs required by EGS5 code

      real*8                                                 ! Arguments
     * dist,
     * xfin,yfin,zfin

      xfin = x(np) + dist*u(np)
      yfin = y(np) + dist*v(np)
      zfin = z(np) + dist*w(np)

      return

      end

!-------------------------------finval.f--------------------------------
