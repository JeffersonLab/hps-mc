!-------------------------------cylndr.f--------------------------------
! Version: 051219-1435
!-----------------------------------------------------------------------
!23456789|123456789|123456789|123456789|123456789|123456789|123456789|12

! ----------------------------------------------------------------------
! Auxiliary (geometry) subroutine for use with the EGS5 Code System
! ----------------------------------------------------------------------
! This subroutine determines whether or not a right circular cylinder is 
! intersected by a particle trajectory and, if so, obtains the distance
! to the surface.
! ----------------------------------------------------------------------
! Input arguments:
! ----------------
!   icyl = cylinder ID number
!   infl = 1 means particle is inside cylinder
!        = 0 means particle is outside cylinder
! Output arguments:
! -----------------
!   ihit = 1 means particle intersects surface
!        = 0 means particle misses surface
!   tcyl = distance to surface if intersected
!
! Note: Data in the form of the cylinder-radius squared is required
!       and is transmitted via common/CYLDTA/.
! ----------------------------------------------------------------------

      subroutine cylndr(icyl,infl,ihit,tcyl)

      implicit none

      include 'include/egs5_h.f'               ! Main EGS5 "header" file
      include 'include/egs5_stack.f'     ! COMMONs required by EGS5 code

      include 'auxcommons/aux_h.f'   ! Auxiliary-code "header" file
      include 'auxcommons/cyldta.f'         ! Auxiliary-code COMMON

      real*8 tcyl                                            ! Arguments
      integer icyl,infl,ihit

      real*8 a,b,c,b2,rtarg,delcyl,rootcy              ! Local variables

      data delcyl/1.D-8/

      ihit = 1                                          ! Assume a "hit"
      tcyl = 0.

!     ----------------------------------------
!     Calculate the quadratic parameters a,b,c
!     ----------------------------------------
      a = sqrt(u(np)*u(np) + v(np)*v(np))
      if (a .eq. 0.) then                   ! Quadratic is indeterminate
        ihit = 0
        return
      end if

      b = (x(np)*u(np) + y(np)*v(np))/a
      c = x(np)*x(np) + y(np)*y(np) - cyrad2(icyl)
      b2 = b*b
      rtarg = b2 - c
      if (rtarg .lt. 0.) then                       ! Imaginary solution
        ihit = 0
        return
      end if

      if (abs(c) .lt. delcyl*cyrad2(icyl)) then
        if (infl .eq. 0 .and. b .ge. 0.) then
          ihit = 0
          return
        end if
        if (infl .eq. 1 .and. b .lt. 0.) then
          tcyl = -2.0*b/a
          return
        end if
      end if

      if ((infl .eq. 1 .and. c .ge. 0.) .or.
     *    (infl .eq. 0 .and. c .le. 0.)) then
        tcyl = delcyl*cyrad(icyl)
        return
      end if

!     ----------------------------------------------------------
!     Calculate the root(s) and choose the smallest positive one
!     ----------------------------------------------------------
      rootcy = sqrt(rtarg)
      if (c .lt. 0.) then
        tcyl = (-b + rootcy)/a
        return
      else if (b .lt. 0.) then
        tcyl = (-b - rootcy)/a
        return
      end if
      ihit = 0
      return

      end

!--------------------------last line of cylndr.f------------------------
