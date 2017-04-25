!-------------------------------sphtrn.f--------------------------------
! Version: 051219-1435
! Reference:
!-----------------------------------------------------------------------
!23456789|123456789|123456789|123456789|123456789|123456789|123456789|12

! ----------------------------------------------------------------------
! Auxiliary (geometry) subroutine for use with the EGS5 Code System
! ----------------------------------------------------------------------
! This subroutine determines whether or not a sphere is intersected by 
! a particle trajectory and, if so, obtains the distance to the surface.
! ----------------------------------------------------------------------
! Input arguments:
! ----------------
!   isph = sphere ID number
!   infl = 1 means particle is inside sphere
!        = 0 means particle is outside sphere
! Output arguments:
! -----------------
!   ihit = 1 means particle intersects surface
!        = 0 means particle misses surface
!   tsph = distance to surface if intersected
!
! Note: Data in the form of the sphere-radius squared is required
!       and is transmitted via common/SPHDTA/.
! ----------------------------------------------------------------------
! SPECIAL NOTE: This routine is for a sphere whose origin is translated
!               along z with respect to the usual EGS coordinate.
!               The variables xtrans, ytrans and ztrans must be defined
!               in AUSGAB and passed in common/TRNDTA/.
! ----------------------------------------------------------------------

      subroutine sphtrn(isph,infl,ihit,tsph)

      implicit none

      include 'include/egs5_h.f'               ! Main EGS5 "header" file

      include 'include/egs5_stack.f'     ! COMMONs required by EGS5 code

      include 'auxcommons/aux_h.f'   !      Auxiliary-code "header" file

      include 'auxcommons/sphdta.f'             ! Auxiliary-code COMMONs
      include 'auxcommons/trndta.f'

      real*8 tsph                                            ! Arguments
      integer isph,infl,ihit

      real*8 a,b,c,b2,rtarg,delsph,rootsp              ! Local variables

      data delsph/1.D-8/

      ihit = 1                                          ! Assume a "hit"
      tsph = 0.

!     ----------------------------------------
!     Calculate the quadratic parameters a,b,c
!     ----------------------------------------
      a = 1.D0
      b = (xtran*u(np) + ytran*v(np) + ztran*w(np))/a
      c = xtran*xtran + ytran*ytran + ztran*ztran - sprad2(isph)
 
      b2 = b*b
      rtarg = b2 - c
 
      if (rtarg .eq. 0.) then
        ihit = 0
        return                                      ! Imaginary solution
      end if

      if (abs(c) .lt. delsph*sprad2(isph)) then
        if(infl .eq. 0 .and. b .ge. 0.) then
          ihit = 0 
          return
        end if
        if (infl .eq. 1 .and. b .lt. 0.) then
          tsph = -2.D0*b/a
          return
        end if
      end if

      if ((infl .eq. 1 .and. c .ge. 0.) .or. 
     *    (infl .eq. 0 .and. c .le. 0.)) then
        tsph = delsph * sprad(isph)
        return
      end if
  
! ---------------------------------------------------------- 
! Calculate the root(s) and choose the smallest positive one
! ---------------------------------------------------------- 
      rootsp = sqrt(rtarg)
      if (c .lt. 0.) then
        tsph = (-b + rootsp)/a
        return
      else if (b .lt. 0.) then
        tsph = (-b - rootsp)/a
        return
      end if

      ihit = 0
      return

      end

!--------------------------last line of sphtrn.f------------------------
