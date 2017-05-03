!--------------------------------condta.f-------------------------------
! Version: 051219-1435
! Reference:
!-----------------------------------------------------------------------
!23456789|123456789|123456789|123456789|123456789|123456789|123456789|12

      common/CONDTA/                                     ! Cylinder data
     * cotal2(MXCONES),              ! Cotangent of half angle (squared)
     * cotal(MXCONES),                         ! Cotangent of half angle
     * smalll (MXCONES)                               ! z-offset of cone

      real*8 cotal2,cotal,smalll

!---------------------------last line of condta.f-----------------------
