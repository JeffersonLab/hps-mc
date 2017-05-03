!------------------------------randomset.f------------------------------
! Version: 051219-1435
! Reference: RANLUX, after James,
!            Computer Phys. Commun. 79 (1994) 111-114.
!            Subtract-and-borrow random number generator proposed by
!            Marsaglia and Zaman, implemented by F. James with the name
!            RCARRY in 1991, and later improved by Martin Luescher
!            in 1993 to produce "Luxury Pseudorandom Numbers".
!            Fortran 77 coded by F. James, 1993
!
! THIS IS THE OLD RANDOM NUMBER GENERATOR THAT USES SINGLE-DIGIT
! PRECISION. 
! 
! If you ever need to use this again, 1) BACKUP THE EXISTING
! "randomset.f" using a new name and  2) change the name of this file to
! "randomset.f"
! 
!-----------------------------------------------------------------------
!23456789|123456789|123456789|123456789|123456789|123456789|123456789|12

      subroutine randomset(rndum)
      
      implicit none

      include 'include/randomm.f'

!  global variables

      real*8 rndum

!  local variables

      integer isk
      real uni

      uni = seeds(j24) - seeds(i24) - carry 
      if (uni .lt. 0.)  then
        uni = uni + 1.0
        carry = twom24
      else
        carry = 0.
      endif
      seeds(i24) = uni
      i24 = next(i24)
      j24 = next(j24)
      rndum = uni
!  small numbers (with less than 12 "significant" bits) are "padded".
      if (uni .lt. twom12)  then
        rndum = rndum + twom24*seeds(j24)
!  and zero is forbidden in case someone takes a logarithm
        if (rndum .eq. 0.)  rndum = twom48
      endif

!     Skipping to luxury.  As proposed by Martin Luscher.
      in24 = in24 + 1
      if (in24 .eq. 24)  then
        in24 = 0
        kount = kount + nskip
        do isk = 1, nskip
          uni = seeds(j24) - seeds(i24) - carry
          if (uni .lt. 0.)  then
            uni = uni + 1.0
            carry = twom24
          else
            carry = 0.
          endif
          seeds(i24) = uni
          i24 = next(i24)
          j24 = next(j24)
        end do
      endif

      ! store for restart from number of rngs used
      kount = kount + 1
      if(kount .ge. igiga) then
        mkount = mkount + 1
        kount = kount - igiga
      endif

      return
      end

!-------------------------last line of randomset.f----------------------
