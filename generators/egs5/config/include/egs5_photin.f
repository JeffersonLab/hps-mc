!-----------------------------egs5_photin.f-----------------------------
! Version: 051219-1435
!-----------------------------------------------------------------------
!23456789|123456789|123456789|123456789|123456789|123456789|123456789|12

      common/PHOTIN/                             ! Photon transport data
     * ebinda(100),                      ! Average K-edge binding energy
     * ge0(MXMED),
     * ge1(MXMED),
     * gmfp0(MXGE,MXMED),                     ! Gamma Mean Free Path fit
     * gmfp1(MXGE,MXMED),                     !  coefficients
     * gbr10(MXGE,MXMED),       ! Gamma Branching Ratio fit coefficients
     * gbr11(MXGE,MXMED),       !  (pair/(pair+compton+photo)
     * gbr20(MXGE,MXMED),       ! Gamma Branching Ratio fit coefficients
     * gbr21(MXGE,MXMED),       !  (pair+compton)/(pair+compton+photo)
     * rco0(MXMED),
     * rco1(MXMED),
     * rsct0(MXRAYFF,MXMED),
     * rsct1(MXRAYFF,MXMED),
     * cohe0(MXGE,MXMED),
     * cohe1(MXGE,MXMED),
     * mpgem(MXSGE,MXMED),
     * ngr(MXMED)

      real*8 ebinda,ge0,ge1,gmfp0,gmfp1,gbr10,gbr11,gbr20,gbr21,
     *       rco0,rco1,rsct0,rsct1,cohe0,cohe1
      integer mpgem,ngr

!-----------------------last line of egs5_photin.f----------------------
