

c Define common block containing all coupling constants and masses
c  which are used in the HELAS routines.
c
c  These include masses, widths and real/complex couplings.
c
c  This file can be built automatically from particles.dat and
c  interactions.dat
c
c====================================================================
c
c
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
      double precision     gw, gwwa, gwwz
      common /COUPL_GAUGE/ gal   , gad   , gau   , gwf   ,
     &                     gzn   , gzl   , gzd   , gzu   ,
     &                     gw, gwwa, gwwz
c
      double complex       gwfc(2),  gwfs(2), gwfm(2)
      common /coupl_ckm/   gwfc,     gwfs   , gwfm	
c
      double complex       gwwh, gzzh, gwwhh, gzzhh, ghhh, ghhhh
      common /COUPL_SCAL/  gwwh, gzzh, gwwhh, gzzhh, ghhh, ghhhh
c
      double complex       ghtop(2), ghbot(2), ghtau(2), ghcha(2)
      common /COUPL_YUK/   ghtop   , ghbot   , ghtau   , ghcha


      double complex Ggrq,Ggrb,Ggrt,Ggrgg,Gs2g,Gs2q,Gs2b,Gs2t
      double complex   Gs0t(2),Gs0g(2),Gs1D(2),Gs1U(2),Gs1E(2),Gs1Ne(2),Go1D(2),Go1U(2)
     &  ,Go0t(2),Go0g(2)
     &  ,Gs1s(2),Gs1c(2),Gs1b(2),Gs1t(2),Go1s(2),Go1c(2),Go1b(2),Go1t(2)
     &  ,Gs1Mu(2),Gs1Ta(2),Gs1Nm(2),Gs1Nt(2)
      common /DC_COUPL/ Gs0t,Gs0g,Go0t,Go0g,Gs1D,Gs1U,Gs1E,Gs1Ne,Go1D,Go1U
     &                 ,Ggrq,Ggrb,Ggrt,Ggrgg,Gs2g,Gs2q,Gs2b,Gs2t
     &                 ,Gs1s,Gs1c,Gs1b,Gs1t,Go1c,Go1s,Go1b,Go1t
     &                 ,Gs1Mu,Gs1Ta,Gs1Nm,Gs1Nt
      double precision S0MASS,O0MASS,S1MASS,O1MASS,S2MASS,
     &g1mass,g2mass,g3mass,g4mass,g5mass,g6mass,g7mass,g8mass,g9mass,g0mass
      double precision S0WIDTH,O0WIDTH,S1WIDTH,O1WIDTH,S2WIDTH,
     &g1width,g2width,g3width,g4width,g5width,g6width,g7width,g8width,g9width,g0width
      common /USR_MASS/  S0MASS,O0MASS,S1MASS,O1MASS,S2MASS,
     &g1mass,g2mass,g3mass,g4mass,g5mass,g6mass,g7mass,g8mass,g9mass,g0mass
      common /USR_DECAY/ S0WIDTH,O0WIDTH,S1WIDTH,O1WIDTH,S2WIDTH,
     &g1width,g2width,g3width,g4width,g5width,g6width,g7width,g8width,g9width,g0width
      double precision s0scalarf  ,s0axialf   ,s1quleft   ,s1quright  ,s1qdleft   ,s1qdright  ,s1eleft    ,s1e
     +right   ,s1ne      ,o1quleft   ,o1quright  ,o1qdleft   ,o1qdright  ,kapMpl     ,addn       ,mstring    
     & ,s1muleft,s1muright,s1taleft,s1taright,s1qsleft,s1qcleft,s1qbleft,s1qtleft
     & ,s1qsright,s1qcright,s1qbright,s1qtright,o1qsleft,o1qsright,o1qcleft,o1qcright,o1qbleft,o1qbright
     & ,o1qtleft,o1qtright,s1nm,s1nt,o0scalarf,o0axialf
