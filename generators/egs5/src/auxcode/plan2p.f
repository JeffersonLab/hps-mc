!-------------------------------plan2p.f--------------------------------
! Version: 051219-1435
! Reference:
!-----------------------------------------------------------------------
!23456789|123456789|123456789|123456789|123456789|123456789|123456789|12

! ----------------------------------------------------------------------
! Auxiliary (geometry) subroutine for use with the EGS5 Code System
! ----------------------------------------------------------------------
! Subroutine plan2p is generally called from subroutine howfar whenever
! a particle is in a region bounded by two planes that are PARALLEL.
! Both subroutines plane1 and chgtr are called by subroutine plan2p, but
! for efficiency reasons the second plane1 call is not made if the first
! plane is not hit, or if the trajectory is parallel.
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

      subroutine plan2p(npl1,nrg1,isd1,npl2,nrg2,isd2)

      implicit none

      real*8 tval                                            ! Arguments
      integer npl1,nrg1,isd1,npl2,nrg2,isd2,ihit

      call plane1(npl1,isd1,ihit,tval)
      if (ihit .eq. 1) then                             ! Hits 1st plane
        call chgtr(tval,nrg1)
      else if(ihit .eq. 0) then           ! Heading away from 1st plane,
        call plane1(npl2,isd2,ihit,tval)  !     but it may hit 2nd plane
        if (ihit .eq. 1) then                           ! Hits 2nd plane
          call chgtr(tval,nrg2)
        end if
      else if(ihit .ne. 2) then
        write(6,101) npl1,nrg1,npl2,nrg2,ihit
 101    FORMAT(' STOPPED IN SUBROUTINE PLAN2P WITH NPL1,NRG1,NPL2,NRG2='
     *         ,4I6,/,' AND WITH IHIT=',I6)
        stop
      end if

      return

      end

!-------------------------------plan2p.f--------------------------------
