!-------------------------------plan2x.f--------------------------------
! Version: 051219-1435
! Reference:
!-----------------------------------------------------------------------
!23456789|123456789|123456789|123456789|123456789|123456789|123456789|12

! ----------------------------------------------------------------------
! Auxiliary (geometry) subroutine for use with the EGS5 Code System
! ----------------------------------------------------------------------
! Subroutine plan2x is generally called from subroutine howfar whenever
! a particle is in a region bounded by two planes that are NOT parallel.
! Both subroutines plane1 and chgtr are called by subroutine plan2x.
! ----------------------------------------------------------------------
! Input arguments:
! ----------------
!   npl1 = ID number assigned to plane called first
!   nrg1 = ID number assigned to region particle trajectory
!          will lead into next
!   isd1 =  1 normal vector points towards current region
!        = -1 normal vector points away from current region
!   npl2 = Same as above, but for plane called second
!   nrg2 = Same as above, but for plane called second)
!   isd2 = Same as above, but for plane called second
! ----------------------------------------------------------------------

      subroutine plan2x(npl1,nrg1,isd1,npl2,nrg2,isd2)

      implicit none

      real*8 tval                                            ! Arguments
      integer npl1,nrg1,isd1,npl2,nrg2,isd2,ihit

      call plane1(npl1,isd1,ihit,tval)
      if (ihit .eq. 1) call chgtr(tval,nrg1)

      call plane1(npl2,isd2,ihit,tval)
      if (ihit .eq. 1) call chgtr(tval,nrg2)

      return

      end

!-------------------------------plan2x.f--------------------------------
