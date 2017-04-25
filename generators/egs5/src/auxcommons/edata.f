!--------------------------------edata.f--------------------------------
! Version: 051219-1435
! Reference:
!-----------------------------------------------------------------------
!23456789|123456789|123456789|123456789|123456789|123456789|123456789|12

      common/EDATA/
     * encdf(MXEBIN),           ! Cumulative distribution function (CDF)
     * epdf(MXEBIN),                ! Probability density function (PDF)
     * ebin(MXEBIN),             ! Upper value (top) of energy bin (MeV) 
     * ebinmin,                      ! Lowest value of energy bins (MeV) 
     * esam1,esam2,delsam,   ! Sampling range (delsam=esam2-esam1) (MeV)
     * nebin                     ! Number of energy bins (set by MXEBIN) 

      real*8 encdf,epdf,ebin,ebinmin,esam1,esam2,delsam
      integer nebin

!---------------------------last line of edata.f------------------------
