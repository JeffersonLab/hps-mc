c====================================================================
c
c  Define common block containing all coupling constants and masses
c  which are used in the HELAS routines.
c
c  These include masses, widths and real/complex couplings.
c
c  This file can be built automatically from particles.dat and
c  interactions.dat
c
c====================================================================
c
c     QCD 
c
      double complex       gg(2)
      double precision     g
      common /COUPL_QCD/   g,gg   
c
c    kinematical masses
c
      double precision     hmass, wmass, zmass, amass,
     &                     tmass, bmass, lmass, cmass
      common /COUPL_MASS/  hmass, wmass, zmass, amass,
     &                     tmass, bmass, lmass, cmass
c
c   widths
c
      double precision     hwidth, wwidth, zwidth, 
     &                     twidth, lwidth, awidth
      common /COUPL_WIDTH/ hwidth, wwidth, zwidth, 
     &                     twidth, lwidth, awidth
c
c  couplings in the feynman rules
c
      double complex       gal(2), gad(2), gau(2), gwf(2),
     &                     gzn(2), gzl(2), gzd(2), gzu(2)
      double precision     gw, gwwa, gwwz, gwwzp
      common /COUPL_GAUGE/ gal   , gad   , gau   , gwf   ,
     &                     gzn   , gzl   , gzd   , gzu   ,
     &                     gw, gwwa, gwwz, gwwzp
c
      double complex       gwfc(2),  gwfs(2), gwfm(2)
      common /coupl_ckm/   gwfc,     gwfs   , gwfm	
c
      double complex       gwwh, gzzh, gwwhh, gzzhh, ghhh, ghhhh
      common /COUPL_SCAL/  gwwh, gzzh, gwwhh, gzzhh, ghhh, ghhhh
c
      double complex       ghtop(2), ghbot(2), ghtau(2), ghcha(2)
      common /COUPL_YUK/   ghtop   , ghbot   , ghtau   , ghcha

      double complex   GZPD(2),GZPU(2),GZPL(2),GZPN(2),GZPZPH,GZPZPHH
      common /DC_COUPL/ GZPD,GZPU,GZPL,GZPN,GZPZPH,GZPZPHH
      double precision ZPMASS
      double precision ZPWIDTH
      common /USR_MASS/  ZPMASS
      common /USR_DECAY/ ZPWIDTH
      double precision param1,param2,param3,param4,param5,param6,param7,param8,param9,param10,param11

