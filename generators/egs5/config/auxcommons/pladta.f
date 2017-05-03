!--------------------------------pladta.f-------------------------------
! Version: 051219-1435
! Reference:
!-----------------------------------------------------------------------
!23456789|123456789|123456789|123456789|123456789|123456789|123456789|12

      common/PLADTA/                                        ! Plane data
     * pcoord(3,MXPLNS),          ! (x,y,z) coordinate of point on plane
     * pnorm(3,MXPLNS)                     ! Unit normal vector of plane

      real*8 pcoord,pnorm

!---------------------------last line of pladta.f-----------------------
