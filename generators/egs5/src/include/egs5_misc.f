!------------------------------egs5_misc.f------------------------------
! Version: 060317-1405
!-----------------------------------------------------------------------
!23456789|123456789|123456789|123456789|123456789|123456789|123456789|12

      common/MISC/                                ! Miscellaneous COMMON
     * rhor(MXREG),
     * k1Hscl(MXREG),
     * k1Lscl(MXREG),
     * ectrng(MXREG),
     * pctrng(MXREG),
     * dunit,
     * med(MXREG),
     * iraylr(MXREG),
     * lpolar(MXREG),
     * incohr(MXREG),
     * iprofr(MXREG),
     * impacr(MXREG),
     * nomsct(MXREG),  ! For turning off mult. scattering in each region
     * kmpi,kmpo,
     * nreg                          ! Number of regions in this problem

      real*8
     * rhor,
     * k1Hscl, k1Lscl,
     * ectrng, pctrng,
     * dunit

      integer
     * med,iraylr,lpolar,incohr,
     * iprofr,impacr,nomsct,
     * kmpi,kmpo,nreg

!------------------------last line of egs5_misc.f-----------------------
