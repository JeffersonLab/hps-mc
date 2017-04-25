!-----------------------------egs5_elecin.f-----------------------------
! Version: 060317-1500
!-----------------------------------------------------------------------
!23456789|123456789|123456789|123456789|123456789|123456789|123456789|12

      common/ELECIN/                          ! Electron transport input
     * ekelim,
     * eke0(MXMED),eke1(MXMED),cmfp0(MXMED),cmfp1(MXMED),
     * xr0(MXMED),teff0(MXMED),blcc(MXMED),xcc(MXMED),
     * picmp0(MXCMFP,MXMED),picmp1(MXCMFP,MXMED),
     * eicmp0(MXCMFP,MXMED),eicmp1(MXCMFP,MXMED),
     * esig0(MXEKE,MXMED),esig1(MXEKE,MXMED),
     * psig0(MXEKE,MXMED),psig1(MXEKE,MXMED),
     * ededx0(MXEKE,MXMED),ededx1(MXEKE,MXMED),
     * pdedx0(MXEKE,MXMED),pdedx1(MXEKE,MXMED),
     * erang0(MXEKE,MXMED),erang1(MXEKE,MXMED),
     * prang0(MXEKE,MXMED),prang1(MXEKE,MXMED),
     * estep0(MXEKE,MXMED),estep1(MXEKE,MXMED),
     * ebr10(MXEKE,MXMED),ebr11(MXEKE,MXMED),
     * pbr10(MXEKE,MXMED),pbr11(MXEKE,MXMED),
     * pbr20(MXEKE,MXMED),pbr21(MXEKE,MXMED),
     * tmxs0(MXEKE,MXMED),tmxs1(MXEKE,MXMED),
     * cmfpe0(MXLEKE,MXMED),cmfpe1(MXLEKE,MXMED),
     * cmfpp0(MXLEKE,MXMED),cmfpp1(MXLEKE,MXMED),
     * cxc2e0(MXLEKE,MXMED),cxc2e1(MXLEKE,MXMED),
     * cxc2p0(MXLEKE,MXMED),cxc2p1(MXLEKE,MXMED),
     * clxae0(MXLEKE,MXMED),clxae1(MXLEKE,MXMED),
     * clxap0(MXLEKE,MXMED),clxap1(MXLEKE,MXMED),
     * thr0(MXBLC,MXRNTH),thr1(MXBLC,MXRNTH),thr2(MXBLC,MXRNTH),
     * thri0(MXBLC,MXRNTHI),thri1(MXBLC,MXRNTHI),thri2(MXBLC,MXRNTHI),
     * fstep(MSSTEPS),fsqr(MSSTEPS),
     * vert1(MXVRT1),vert2(MXVRT2,MSSTEPS),
     * blc0,blc1,rthr0,rthr1,rthri0,rthri1,icomp,
     * mpeem(MXSEKE,MXMED),msmap(MXJREFF),
     * msteps,jrmax,mxv1,mxv2,nblc,nrnth,nrnthi,
     * iunrst(MXMED),epstfl(MXMED),iaprim(MXMED)

      real*8
     * ekelim,
     * eke0,eke1,cmfp0,cmfp1,xr0,teff0,blcc,xcc,
     * picmp0,picmp1,eicmp0,eicmp1,esig0,esig1,psig0,psig1,
     * ededx0,ededx1,pdedx0,pdedx1,ebr10,ebr11,pbr10,pbr11,
     * pbr20,pbr21,tmxs0,tmxs1,cmfpe0,cmfpe1,cmfpp0,cmfpp1,
     * erang0,erang1,prang0,prang1,cxc2e0,cxc2e1,cxc2p0,cxc2p1,
     * clxae0,clxae1,clxap0,clxap1,thr0,thr1,thr2,thri0,thri1,
     * thri2,fstep,fsqr,vert1,vert2,blc0,blc1,rthr0,rthr1,
     * rthri0,rthri1,estep0,estep1

      integer icomp,mpeem,msmap,msteps,jrmax,mxv1,mxv2,nblc,
     * nrnth,nrnthi,iunrst,epstfl,iaprim

!-----------------------last line of egs5_elecin.f----------------------
