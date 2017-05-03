!------------------------------decodeir.f-------------------------------
! Version: 051219-1435
! Reference:
!-----------------------------------------------------------------------
!23456789|123456789|123456789|123456789|123456789|123456789|123456789|12

! Subroutine to determine the i,j,k indices, for standard cylinder-slab
! geometry EGS User Codes (e.g., ucrtz.f), given the region.  The first 
! parameter is the region number and next three are the decoded i,j,k
! indices (requires /GEORTZ/ and region number must be greater than 1).
! ----------------------------------------------------------------------

      subroutine decodeir(irdum,idum,jdum,kdum)

      implicit none

      include 'auxcommons/geortz.f'             ! Auxiliary-code COMMONs

      integer                                                ! Arguments
     * irdum,idum,jdum,kdum

      if (irdum .le. 1) then
        write(6,*) '***** Program stopped because region is <= 1'
        stop
      end if

      idum = mod(irdum-1,imax)
      if (idum .eq. 0) idum = imax
      kdum = 1 + (irdum - 1 - idum)/ijmax
      jdum = 1 + (irdum - 1 - idum - (kdum - 1)*ijmax)/imax

      return

      end

!-------------------------last line of decodeir.f-----------------------
