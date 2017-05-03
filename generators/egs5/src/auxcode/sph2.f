!-------------------------------sph2.f--------------------------------
! Version: 051219-1435
! Reference:
!-----------------------------------------------------------------------
!23456789|123456789|123456789|123456789|123456789|123456789|123456789|12

! ----------------------------------------------------------------------
! Auxiliary (geometry) subroutine for use with the EGS5 Code System
! ----------------------------------------------------------------------
! Subroutine sph2 is generally called from subroutine howfar whenever
! a particle is in a region bounded by two spheres.
! Both subroutines sphere and chgtr are called by subroutine sph2.
! ----------------------------------------------------------------------
! Input arguments:
! ----------------
!   nsp1 = ID number assigned to sphere including current position
!   nrg1 = ID number assigned to region particle trajectory
!          will lead into next
!   nsp2 = ID number assigned to sphere inside current position
!   nrg2 = Same as above, but for second sphere
! ----------------------------------------------------------------------

      subroutine sph2(nsp1,nrg1,nsp2,nrg2)

      implicit none

      real*8 tsph                                            ! Arguments
      integer nsp1,nrg1,nsp2,nrg2,ihit

      call sphere(nsp1,0,ihit,tsph)
      if (ihit .eq. 1) then                             ! Hits 1st sphere
        call chgtr(tsph,nrg1)
      end if
      call sphere(nsp2,1,ihit,tsph)
      if (ihit .eq. 1) then                             ! Hits 2nd sphere
        call chgtr(tsph,nrg2)
      end if

      return

      end

!-------------------------------sph2.f--------------------------------
