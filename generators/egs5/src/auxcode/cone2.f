!-------------------------------cone2.f--------------------------------
! Version: 051219-1435
! Reference:
!-----------------------------------------------------------------------
!23456789|123456789|123456789|123456789|123456789|123456789|123456789|12

! ----------------------------------------------------------------------
! Auxiliary (geometry) subroutine for use with the EGS5 Code System
! ----------------------------------------------------------------------
! Subroutine cone2 is generally called from subroutine howfar whenever
! a particle is in a region bounded between two cones.
! Both subroutines cone and chgtr are called by subroutine cone2.
! ----------------------------------------------------------------------
! Input arguments:
! ----------------
!   ncone1 = ID number assigned to cone including current position
!   nrg1   = ID number assigned to region particle trajectory
!            will lead into next
!   ncone2 = ID number assigned to cone inside current position
!   nrg2   = Same as above, but for second cone
! ----------------------------------------------------------------------

      subroutine cone2(ncone1,nrg1,ncone2,nrg2)

      implicit none

      real*8 tcone                                           ! Arguments
      integer ncone1,nrg1,ncone2,nrg2,ihit

      call cone(ncone1,0,ihit,tcone)
      if (ihit .eq. 1) then                             ! Hits 1st cone
        call chgtr(tcone,nrg1)
      end if
      call cone(ncone2,1,ihit,tcone)
      if (ihit .eq. 1) then                             ! Hits 2nd cone
        call chgtr(tcone,nrg2)
      end if

      return

      end

!-------------------------------cyl2.f--------------------------------
