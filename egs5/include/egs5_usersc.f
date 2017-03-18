!-----------------------------egs5_usersc.f-----------------------------
! Version: 060317-1100
!-----------------------------------------------------------------------
!23456789|123456789|123456789|123456789|123456789|123456789|123456789|12

      common/USERSC/                                ! User-Step-Controls
     * estepr(MXREG),  ! estepe multiplier for each region (if non zero)
     * esave(MXREG),           ! Upper limit on electron range rejection
     * emaxe                         ! maximum kinetic energy in problem

      real*8 estepr, esave, emaxe

!-----------------------last line of egs5_usersc.f----------------------
