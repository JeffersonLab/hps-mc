!-------------------------------ntaly1.f--------------------------------
! Version: 051227-1200
!-----------------------------------------------------------------------
!23456789|123456789|123456789|123456789|123456789|123456789|123456789|12

      common/NTALY1/               ! Energy deposition event count tally
     * wdist(500),                 ! weighted distribuition in mu
     * wedist(500),                ! weighted distribuition in E
     * wback,                      ! weighted number of backscatters
     * ench,                       ! energy tally grid
     * nsum(4,MXREG,5),
     * bdist(500),                 ! distribuition in mu
     * bedist(500),                ! energy distribuition in E
     * nback,                      ! number of backscatters
     * netbin,                     ! number of energy tally bins
     * nabin                       ! number of angle tally bins

      real*8  ench
      real*8  wback, wdist, wedist
      integer*8 nsum
      integer nback, bdist, bedist, netbin, nabin

!--------------------------last line of ntaly1.f------------------------
