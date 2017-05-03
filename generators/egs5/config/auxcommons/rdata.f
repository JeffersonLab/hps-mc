!--------------------------------rdata.f--------------------------------
! Version: 051219-1435
! Reference:
!-----------------------------------------------------------------------
!23456789|123456789|123456789|123456789|123456789|123456789|123456789|12

      common/RDATA/
     * rcdf(MXRBIN),            ! Cumulative distribution function (CDF)
     * rpdf(MXRBIN),                ! Probability density function (PDF)
     * rbin(MXRBIN),              ! Upper value (top) of radius bin (cm) 
     * rbinmin,                       ! Lowest value of radius bins (cm) 
     * rsam1,rsam2,delrsam,  ! Sampling range (delrsam=rsam2-rsam1) (cm)
     * nrbin                     ! Number of radius bins (set by MXRBIN) 

      real*8 rcdf,rpdf,rbin,rbinmin,rsam1,rsam2,delrsam
      integer nrbin

!---------------------------last line of rdata.f------------------------
