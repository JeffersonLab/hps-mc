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

      double complex        gh1ee(2), gh2ee(2), gh3ee(2), gh1emu(2), gh2emu(2), gh3emu(2), gh1eta(2), gh2eta(2), gh3eta(
     +2), gh1mue(2), gh2mue(2), gh3mue(2), gh1mumu(2), gh2mumu(2), gh3mumu(2), gh1muta(2), gh2muta(2), gh3muta(2), gh1tae(2), 
     +gh2tae(2), gh3tae(2), gh1tamu(2), gh2tamu(2), gh3tamu(2), gh1tata(2), gh2tata(2), gh3tata(2), gh1uu(2), gh2uu(2), gh3uu(
     +2), gh1uc(2), gh2uc(2), gh3uc(2), gh1ut(2), gh2ut(2), gh3ut(2), gh1cu(2), gh2cu(2), gh3cu(2), gh1cc(2), gh2cc(2), gh3cc(
     +2), gh1ct(2), gh2ct(2), gh3ct(2), gh1tu(2), gh2tu(2), gh3tu(2), gh1tc(2), gh2tc(2), gh3tc(2), gh1tt(2), gh2tt(2), gh3tt(
     +2), gh1dd(2), gh2dd(2), gh3dd(2), gh1ds(2), gh2ds(2), gh3ds(2), gh1db(2), gh2db(2), gh3db(2), gh1sd(2), gh2sd(2), gh3sd(
     +2), gh1ss(2), gh2ss(2), gh3ss(2), gh1sb(2), gh2sb(2), gh3sb(2), gh1bd(2), gh2bd(2), gh3bd(2), gh1bs(2), gh2bs(2), gh3bs(
     +2), gh1bb(2), gh2bb(2), gh3bb(2), ghmdu(2), ghmdc(2), ghmdt(2), ghmsu(2), ghmsc(2), ghmst(2), ghmbu(2), ghmbc(2), ghmbt(
     +2), ghpud(2), ghpus(2), ghpub(2), ghpcd(2), ghpcs(2), ghpcb(2), ghptd(2), ghpts(2), ghptb(2), ghmeve(2), ghmevm(2), ghme
     +vt(2), ghmmuve(2), ghmmuvm(2), ghmmuvt(2), ghmtave(2), ghmtavm(2), ghmtavt(2), ghpvee(2), ghpvemu(2), ghpveta(2), ghpvme
     +(2), ghpvmmu(2), ghpvmta(2), ghpvte(2), ghpvtmu(2), ghpvtta(2)
      common /coupl_FFS/    gh1ee, gh2ee, gh3ee, gh1emu, gh2emu, gh3emu, gh1eta, gh2eta, gh3eta, gh1mue, gh2mue, gh3mue,
     + gh1mumu, gh2mumu, gh3mumu, gh1muta, gh2muta, gh3muta, gh1tae, gh2tae, gh3tae, gh1tamu, gh2tamu, gh3tamu, gh1tata, gh2ta
     +ta, gh3tata, gh1uu, gh2uu, gh3uu, gh1uc, gh2uc, gh3uc, gh1ut, gh2ut, gh3ut, gh1cu, gh2cu, gh3cu, gh1cc, gh2cc, gh3cc, gh
     +1ct, gh2ct, gh3ct, gh1tu, gh2tu, gh3tu, gh1tc, gh2tc, gh3tc, gh1tt, gh2tt, gh3tt, gh1dd, gh2dd, gh3dd, gh1ds, gh2ds, gh3
     +ds, gh1db, gh2db, gh3db, gh1sd, gh2sd, gh3sd, gh1ss, gh2ss, gh3ss, gh1sb, gh2sb, gh3sb, gh1bd, gh2bd, gh3bd, gh1bs, gh2b
     +s, gh3bs, gh1bb, gh2bb, gh3bb, ghmdu, ghmdc, ghmdt, ghmsu, ghmsc, ghmst, ghmbu, ghmbc, ghmbt, ghpud, ghpus, ghpub, ghpcd
     +, ghpcs, ghpcb, ghptd, ghpts, ghptb, ghmeve, ghmevm, ghmevt, ghmmuve, ghmmuvm, ghmmuvt, ghmtave, ghmtavm, ghmtavt, ghpve
     +e, ghpvemu, ghpveta, ghpvme, ghpvmmu, ghpvmta, ghpvte, ghpvtmu, ghpvtta



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
