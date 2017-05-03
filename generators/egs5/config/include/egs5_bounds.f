!----------------------------egs5_bounds.f------------------------------
! Version: 051219-1435
!-----------------------------------------------------------------------
!23456789|123456789|123456789|123456789|123456789|123456789|123456789|12

      common/BOUNDS/       ! Cutoff energies & vacuum transport distance
     * ecut(MXREG),             ! Charged particle cutoff energy (total)
     * pcut(MXREG),                       ! Photon cutoff energy (total)
     * vacdst                                ! Vacuum transport distance

      real*8 ecut,pcut,vacdst

!------------------------last line of egs5_bounds.f---------------------
