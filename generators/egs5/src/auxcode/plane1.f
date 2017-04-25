!-------------------------------plane1.f--------------------------------
! Version: 060116-1100
! Reference:
!-----------------------------------------------------------------------
!23456789|123456789|123456789|123456789|123456789|123456789|123456789|12

! ----------------------------------------------------------------------
! Auxiliary (geometry) subroutine for use with the EGS5 Code System
! ----------------------------------------------------------------------
! This subroutine determines whether or not a plane is intersected by a
! particle trajectory and, if so, obtains the distance to the surface.
! A plane is defined relative to a coordinate system by means of a point
! on its surface (pcoord-array) and a unit vector normal to the surface
! (pnorm-array).  Both arrays are passed in common/PLADTA/.  The user 
! must assign appropriate values to pcoord and pnorm in the User Code.
!
! Subroutine plane1 is called whenever the user wants to determine if
! the straight-line trajectory of a particle with coordinates is (X,Y,Z)
! and direction cosines (U,V,W) intersects a plane.  If it does,
! plane1 returns the distance to the plane.
! ----------------------------------------------------------------------
! Input arguments:
! ----------------
!   nplan = ID number assigned to plane
!   iside =  1 normal points away from current region
!         = -1 normal points towards current region
! Output arguments:
! -----------------
!   ihit  =  1 trajectory will strike plane
!         =  2 trajectory parallel to plane
!         =  0 trajectory moving away from plane
!   tval  = distance to plane (when ihit=1)
! ----------------------------------------------------------------------

      subroutine plane1(nplan,iside,ihit,tval)

      implicit none

      include 'include/egs5_h.f'               ! Main EGS5 "header" file

      include 'include/egs5_stack.f'     ! COMMONs required by EGS5 code

      include 'auxcommons/aux_h.f'        ! Auxiliary-code "header" file
      include 'auxcommons/pladta.f'             ! Auxiliary-code COMMONs

      real*8 tval,tnum                                       ! Arguments
      integer nplan,iside,ihit

      real*8 udota,udotap                              ! Local variables

      udota = pnorm(1,nplan)*u(np) + 
     *        pnorm(2,nplan)*v(np) +
     *        pnorm(3,nplan)*w(np)

      udotap = udota*iside

      if (udota .eq. 0.) then              ! Traveling parallel to plane
        ihit = 2
      else if (udotap .lt. 0.) then          ! Traveling away from plane
        ihit = 0
      else                ! Traveling towards plane---determine distance
        ihit = 1
        tnum = pnorm(1,nplan)*(pcoord(1,nplan) - x(np)) +
     *         pnorm(2,nplan)*(pcoord(2,nplan) - y(np)) +
     *         pnorm(3,nplan)*(pcoord(3,nplan) - z(np))
        tval = tnum/udota
      end if

      return

      end

!--------------------------last line of plane1.f------------------------
