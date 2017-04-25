!----------------------------egs5_brempr.f------------------------------
! Version: 051219-1435
!-----------------------------------------------------------------------
!23456789|123456789|123456789|123456789|123456789|123456789|123456789|12

      common/BREMPR/           ! Bremsstrahlung and pair production data
     * asym(MXMED,MXEL,2),
     * dl1(6,MXMED),dl2(6,MXMED),dl3(6,MXMED),
     * dl4(6,MXMED),dl5(6,MXMED),dl6(6,MXMED),
     * alphi(2,MXMED),bpar(2,MXMED),delpos(2,MXMED),
     * wa(MXMED,MXEL),pz(MXMED,MXEL),zelem(MXMED,MXEL),rhoz(MXMED,MXEL),
     * pwr2i(MXPWR2I),delcm(MXMED),zbrang(MXMED),fbrspl,nne(MXMED),
     * ibrdst,iprdst,ibrspl,nbrspl

      real*8 dl1,dl2,dl3,dl4,dl5,dl6,alphi,bpar,delpos,wa,pz,zelem,
     *       rhoz,pwr2i,delcm,zbrang,fbrspl
      character*4 asym
      integer nne,ibrdst,iprdst,ibrspl,nbrspl

!------------------------last line of egs5_brempr.f---------------------
