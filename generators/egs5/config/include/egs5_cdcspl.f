!------------------------------egs5_cdcspl.f----------------------------
! Version: 051219-1435
! Reference: From source developed and provided by F. Salvat, Dec 2001
!-----------------------------------------------------------------------
!23456789|123456789|123456789|123456789|123456789|123456789|123456789|12

!  Stores spline data for current material, energy

      COMMON/CDCSPL/ 
     +       RA(NREDA),           !  Spline coefficients
     +       RB(NREDA),
     +       RC(NREDA),
     +       RD(NREDA)

      double precision RA, RB, RC, RD

!-----------------------last line of egs5_cdcspl.f----------------------
