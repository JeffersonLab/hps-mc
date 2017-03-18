!------------------------------egs5_useful.f----------------------------
! Version: 051219-1435
!-----------------------------------------------------------------------
!23456789|123456789|123456789|123456789|123456789|123456789|123456789|12

      common/USEFUL/                       ! Some heavily used variables
     * RM,                                          ! Electron rest mass
     * medium,                  ! Index of current medium (0 for vacuum)
     * medold,                                ! Index of previous medium
     * iblobe         ! Flag, photon below EBINDA after PE (1=yes, 0=no)

      real*8 RM
      integer medium,medold,iblobe

!-----------------------last line of egs5_useful.f---------------------
