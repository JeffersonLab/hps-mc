!-------------------------------etaly3.f--------------------------------
! Version: 051219-1435
! Reference:
!-----------------------------------------------------------------------
!23456789|123456789|123456789|123456789|123456789|123456789|123456789|12

      common/ETALY3/                           ! Energy deposition tally
     * esum3(MXZTB,MXRTB),
     * rsum3(MXZTB),
     * rdel,
     * zdel,
     * nrtbin,
     * nztbin

      real*8 esum3, rsum3, rdel, zdel
      integer nrtbin, nztbin

!--------------------------last line of etaly3.f------------------------
