c Define common block containing all coupling constants and masses.
c
c====================================================================

      double precision wmass,zmass
      common/boson_masses/ wmass,zmass

      double precision wwidth,zwidth,Twidth,awidth
      common/sm_width/ wwidth,zwidth,Twidth,awidth

      double precision WH1,WH2,WH3,WHC
      common/higgs_width/ WH1,WH2,WH3,WHC

      double precision MH1, MH2, MH3,MHC
      common/higgs_masses/ MH1, MH2, MH3,MHC

c     Cmasses
      double precision  MUMASS,TAMASS,CMASS,BMASS,TMASS,SMASS
      common/sm_fmasses/ MUMASS,TAMASS,CMASS,BMASS,TMASS,SMASS

      double precision g
      common/g_coupling/ g



      double complex        gad(2), gau(2), gal(2)
     +, gzd(2), gzu(2), gzl(2), gzn(2), gwfud(2),
     +gwfcs(2), gwftb(2), gwfdu(2), gwfsc(2), gwfbt(2),
     +gwfcd(2), gwfus(2), gwfsu(2), gwfbu(2), gwfdc(2), 
     +gwf(2), gg(2)

      common /coupl_FFV/    gad, gau, gal, gzd, gzu, gzl,
     + gzn, gwfud, gwfcs, gwftb, gwfdu, gwfsc, gwfbt,
     +gwfcd, gwfus, gwfsu,
     +gwfdc, gwf, gg


      double complex        gwwa, gwwz

      common /coupl_VVV/    gwwa, gwwz


      double complex gh1mumu(2), gh2mumu(2), gh3mumu(2), gh1tata(2),
     +gh2tata(2), gh3tata(2), gh1cc(2), gh2cc(2), gh3cc(2), gh1tt(2), 
     +gh2tt(2), gh3tt(2), gh1ss(2), gh2ss(2), gh3ss(2), gh1bb(2), 
     +gh2bb(2), gh3bb(2),  ghmdc(2), ghmsu(2), ghmsc(2), ghmbt(2),
     +ghpus(2),  ghpcd(2), ghpcs(2), ghptb(2), ghmmuvm(2), ghmtavt(2), 
     +ghpvmmu(2), ghpvtta(2)

      common /coupl_FFS/  gh1mumu, gh2mumu, gh3mumu, gh1tata,
     +gh2tata, gh3tata, gh1cc, gh2cc, gh3cc, gh1tt, 
     +gh2tt, gh3tt, gh1ss, gh2ss, gh3ss, gh1bb, 
     +gh2bb, gh3bb,  ghmdc, ghmsu, ghmsc, ghmbt,
     +ghpus,  ghpcd, ghpcs, ghptb, ghmmuvm, ghmtavt, 
     +ghpvmmu, ghpvtta


      double complex        gwwh1, gwwh2, gwwh3, gzzh1, 
     +gzzh2, gzzh3

      common /coupl_VVS/    gwwh1, gwwh2, gwwh3, gzzh1, 
     +gzzh2, gzzh3

      double complex        gzh1h3, gzh2h3, gzh1h2
     +, gahchc, gzhchc, gwhch1, gwh1hc, gwhch2, gwh2hc,
     +gwhch3, gwh3hc

      common /coupl_VSS/    gzh1h3, gzh2h3, gzh1h2
     +, gahchc, gzhchc, gwhch1, gwh1hc, gwhch2, gwh2hc,
     +gwhch3, gwh3hc


      double complex        gh111, gh112, gh113, g
     +h122, gh123, gh133, gh222, gh223, gh233, gh333, gh1hmhp,
     +gh2hmhp, gh3hmhp

      common /coupl_SSS/    gh111, gh112, gh113, g
     +h122, gh123, gh133, gh222, gh223, gh233, gh333, gh1hmhp,
     +gh2hmhp, gh3hmhp

      double complex        gzzh1h1, gzzh2h2, gzzh3h3,
     +gzzh1h2, gzzh1h3, gzzh2h3, gwwh1h1, gwwh2h2,
     +gwwh3h3, gwwh1h2, gwwh1h3, gwwh2h3, gaahchc, gazhchc,
     +gzzhchc, gwwhchc, gwah1hc, gwah2hc, gwah3hc, gwahch1,
     +gwahch2, gwahch3, gwzh1hc, gwzh2hc, gwzh3hc, gwzhch1,
     +gwzhch2, gwzhch3

      common /coupl_VVSS/   gzzh1h1, gzzh2h2, gzzh3h3,
     +gzzh1h2, gzzh1h3, gzzh2h3, gwwh1h1, gwwh2h2,
     +gwwh3h3, gwwh1h2, gwwh1h3, gwwh2h3, gaahchc, gazhchc,
     +gzzhchc, gwwhchc, gwah1hc, gwah2hc, gwah3hc, gwahch1,
     +gwahch2, gwahch3, gwzh1hc, gwzh2hc, gwzh3hc, gwzhch1,
     +gwzhch2, gwzhch3

C Higgs Effective couplings:
      double complex      Gggh1(2),Gggh2(2),Gggh3(2)
      common /coupl_GGH/  Gggh1,   Gggh2,   Gggh3

