!-------------------------------cyl2.f--------------------------------
! Version: 051219-1435
! Reference:
!-----------------------------------------------------------------------
!23456789|123456789|123456789|123456789|123456789|123456789|123456789|12

! ----------------------------------------------------------------------
! Auxiliary (geometry) subroutine for use with the EGS5 Code System
! ----------------------------------------------------------------------
! Subroutine cyl2 is generally called from subroutine howfar whenever
! a particle is in a region bounded by two cylinder.
! Both subroutines cylinder and chgtr are called by subroutine cyl2.
! ----------------------------------------------------------------------
! Input arguments:
! ----------------
!   ncl1 = ID number assigned to cylinder including current position
!   nrg1 = ID number assigned to region particle trajectory
!          will lead into next
!   ncl2 = D number assigned to cylinder inside current position
!   nrg2 = Same as above, but for second cylinder)
! ----------------------------------------------------------------------

      subroutine cyl2(ncl1,nrg1,ncl2,nrg2)

      implicit none

      real*8 tcyl                                            ! Arguments
      integer ncl1,nrg1,ncl2,nrg2,ihit

      call cylndr(ncl1,0,ihit,tcyl)
      if (ihit .eq. 1) then                             ! Hits 1st cylinder
        call chgtr(tcyl,nrg1)
      end if
      call cylndr(ncl2,1,ihit,tcyl)
      if (ihit .eq. 1) then                             ! Hits 2nd cylinder
        call chgtr(tcyl,nrg2)
      end if

      return

      end

!-------------------------------cyl2.f--------------------------------
