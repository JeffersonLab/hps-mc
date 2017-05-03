!--------------------------------nfac.f-------------------------------
! Version: 051219-1435
! Reference:
!-----------------------------------------------------------------------
!23456789|123456789|123456789|123456789|123456789|123456789|123456789|12

      common/nfac/
     * fnorm,                    ! Maximum value of x-, y- and z-dplay
     * xmin, xmax,                       ! X-direction display region
     * ymin, ymax,                       ! Y-direction display region
     * zmin, zmax,                       ! Z-direction display region
     * npreci                            ! Index to output infomation
                                  ! npreci :0 16 bits version PICT
                                  !         1 24 bits version PICT
                                  !         2 for CGVIEW
                                  
       real*8 fnorm,xmin,xmax,ymin,ymax,zmin,zmax
       integer npreci
!---------------------------last line of nfac.f-----------------------
