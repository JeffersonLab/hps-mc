      subroutine printout
      implicit none

      include 'input.inc'
      include 'coupl.inc'

      integer i,iformat
      character*2 ab(2)
      real*8 ene
      double precision  Zero, One, Two, Three, Four, Half, Rt2
      parameter( Zero = 0.0d0, One = 1.0d0, Two = 2.0d0 )

 10   format( 1x,a,f7.3,' GeV        ',a,f7.4,' GeV' )
 11   format( 1x,a,f12.8,2x,f12.8,a,f12.8,2x,f12.8 )
 12   format( 1x,a,f6.2,a )
 13   format( 1x,a,f6.4,a )
 14   format( 1x,2(a,f10.7,', ',f10.7) )
 15   format( 1x,a,f9.5,a )
 16   format( 1x,a,f7.5 )
 17   format( 1x,a,f8.4 )
 18   format( 1x,a,f8.4,' GeV' )
 19   format( 1x,a,f6.4,a,f6.4 )
 20   format( 1x,a,f11.5,1x,f11.5 )
 21   format( 1x,a,f8.4,' GeV',1x,a,f8.4,' GeV' )
 22   format( 1x,a,f10.8,a,f6.8 )
 23   format( 1x,a,f12.8)
 24   format( 1x,a,f7.3,' GeV        ',a,f7.4,' GeV  (calc @ LO)')
 25   format( 1x,a,f15.8,a,f15.8)
 26   format( 1x,a,f16.8,2x,f16.8,a,f16.8,2x,f16.8 )


      write(*,*) '*****************************************************'
      write(*,*) '*                    MadEvent                       *'
      write(*,*) '*        --------------------------------           *'
      write(*,*) '*          http://madgraph.hep.uiuc.edu             *'	
      write(*,*) '*          http://madgraph.phys.ucl.ac.be           *'
      write(*,*) '*          http://madgraph.roma2.infn.it            *'
      write(*,*) '*        --------------------------------           *'	
      write(*,*) '*                                                   *'
      write(*,*) '*         INTEGRATION CHANNEL LOG FILE              *'
      write(*,*) '*                                                   *'
      write(*,*) '*****************************************************'
      write(6,*)
      write(*,*) '*****************************************************'
      write(*,*) '*          SUMMARY OF THE 2HDM PARAMETERS           *'
      write(*,*) '*****************************************************'
      write(6,*)
      write(6,*)  ' EW Params            '
      write(6,*)  '----------------------'
      write(6,23) ' GF (10^-5*GeV^-2) ',gfermi*1d5
      write(6,23) ' 1/alpha           ',1d0/alpha
      write(6,23) ' M_Z   (GeV)       ',zmass
      write(6,*)
      write(6,*)  'Fermion masses and widths:'
      write(6,*)  '--------------------------'
      write(6,*)
      write(6,24) 'top    mass  =  ', tmass, 'top    width  = ', twidth
      write(6,10) 'bottom mass  =  ', bmass, 'bottom width  = ', Zero
      write(6,10) 'charm  mass  =  ', cmass, 'charm  width  = ', Zero
      write(6,10) 'strange  mass  =  ', smass, 'strange  width  = ', Zero
      write(6,10) 'tau    mass  =  ', tamass, 'tau    width  = ', Zero
      write(6,10) 'mu     mass  =  ', mumass, 'mu    width  = ', Zero
      write(6,*)  'all other quark and lepton masses set to zero'
      write(6,*)
      write(6,*)  'Boson masses and widths:'
      write(6,*)  '------------------------'
      write(6,*)
      write(6,24) 'Z mass  =  ',zmass, 'Z width  = ',zwidth
      write(6,24) 'W mass  =  ',wmass, 'W width  = ',wwidth
      write(6,*)
      write(6,*)  'Higgs bosons masses and widths:'
      write(6,*)  '-------------------------------'
      write(6,*)
      write(6,24) 'H1 mass  =  ',MH1, 'H1 width  = ',WH1
      write(6,24) 'H2 mass  =  ',MH2, 'H2 width  = ',WH2
      write(6,24) 'H3 mass  =  ',MH3, 'H3 width  = ',WH3
      write(6,24) 'HC mass  =  ',MHC, 'HC width  = ',WHC
      write(6,*)
      write(6,*)  'Scalar potential parameters'
      write(6,*)  '---------------------------'
      write(6,*)
      write(6,23) ' lambda1 ',lambda1
      write(6,23) ' lambda2 ',lambda2
      write(6,23) ' lambda3 ',lambda3
      write(6,23) ' lambda4 ',lambda4
      write(6,23) ' lambda5 ',lambda5
      write(6,23) ' Rlambda6 ',Rlambda6
      write(6,23) ' Ilambda6 ',Ilambda6
      write(6,23) ' Rlambda7 ',Rlambda7
      write(6,23) ' Ilambda7 ',Ilambda7
      write(6,*)
      write(6,*)  'Scalar mixing matrix'
      write(6,*)  '--------------------'
      write(6,*)
      write(6,23) ' TH1 ',TH1
      write(6,23) ' TH2 ',TH2
      write(6,23) ' TH3 ',TH3
      write(6,23) ' TN1 ',TN1
      write(6,23) ' TN2 ',TN2
      write(6,23) ' TN3 ',TN3
      write(6,23) ' TA1 ',TA1
      write(6,23) ' TA2 ',TA2
      write(6,23) ' TA3 ',TA3
      write(6,*)
      write(6,*) 'VVV couplings:'
      write(6,*) '--------------'
      write(6,*)
      write(6,20) 'gwwa  = ', gwwa
      write(6,20) 'gwwz  = ', gwwz
      write(6,*)
      write(6,*) 'FFV couplings:'
      write(6,*) '--------------'
      write(6,*)
      write(6,11) 'gal(L)   =  ',gal(1), '    gal(R)   =  ',gal(2)
      write(6,11) 'gau(L)   =  ',gau(1), '    gau(R)   =  ',gau(2)
      write(6,11) 'gad(L)   =  ',gad(1), '    gad(R)   =  ',gad(2)
      write(6,*)
      write(6,11) 'gzn(L)   =  ',gzn(1), '    gzn(R)   =  ',gzn(2)
      write(6,11) 'gzl(L)   =  ',gzl(1), '    gzl(R)   =  ',gzl(2)
      write(6,11) 'gzu(L)   =  ',gzu(1), '    gzu(R)   =  ',gzu(2)
      write(6,11) 'gzd(L)   =  ',gzd(1), '    gzd(R)   =  ',gzd(2)
      write(6,*)
      write(6,20) 'gwfud(L)  =  ',gwfud(1)
      write(6,20) 'gwfus(L)  =  ',gwfus(1)
      write(6,20) 'gwfcd(L)  =  ',gwfcd(1)
      write(6,20) 'gwfcs(L)  =  ',gwfcs(1)
      write(6,20) 'gwftb(L)  =  ',gwftb(1)
      write(6,*)
      write(6,*) 'Strong couplings:'
      write(6,*) '-----------------'
      write(6,*)
      write(6,14) 'gg(1)    =  ',gg(1)   , '    gg(2)    =  ',gg(2)
      write(6,*)
      write(6,*) 'FFS couplings:'
      write(6,*) '-----------------'
      write(6,*)

      write(6,11) 'gh1mumu(L)  =  ',gh1mumu(1),'   gh1mumu(R)    =  ',gh1mumu(2) 
      write(6,11) 'gh2mumu(L)  =  ',gh2mumu(1),'   gh2mumu(R)    =  ',gh2mumu(2)
      write(6,11) 'gh3mumu(L)  =  ',gh3mumu(1),'   gh3mumu(R)    =  ',gh3mumu(2) 
      write(6,11) 'gh1tata(L)  =  ',gh1tata(1),'   gh1tata(R)    =  ',gh1tata(2) 
      write(6,11) 'gh2tata(L)  =  ',gh2tata(1),'   gh2tata(R)    =  ',gh2tata(2)
      write(6,11) 'gh3tata(L)  =  ',gh3tata(1),'   gh3tata(R)    =  ',gh3tata(2)
      write(6,*)

      write(6,11) 'gh1cc(L)    =  ',gh1cc(1),'   gh1cc(R)    =  ',gh1cc(2)
      write(6,11) 'gh2cc(L)    =  ',gh2cc(1),'   gh2cc(R)    =  ',gh2cc(2)
      write(6,11) 'gh3cc(L)    =  ',gh3cc(1),'   gh3cc(R)    =  ',gh3cc(2)
      write(6,11) 'gh1tt(L)    =  ',gh1tt(1),'   gh1tt(R)    =  ',gh1tt(2)
      write(6,11) 'gh2tt(L)    =  ',gh2tt(1),'   gh2tt(R)    =  ',gh2tt(2)
      write(6,11) 'gh3tt(L)    =  ',gh3tt(1),'   gh3tt(R)    =  ',gh3tt(2)
      write(6,*)

      write(6,11) 'gh1ss(L)    =  ',gh1ss(1),'   gh1ss(R)    =  ',gh1ss(2)
      write(6,11) 'gh2ss(L)    =  ',gh2ss(1),'   gh2ss(R)    =  ',gh2ss(2)
      write(6,11) 'gh3ss(L)    =  ',gh3ss(1),'   gh3ss(R)    =  ',gh3ss(2)
      write(6,11) 'gh1bb(L)    =  ',gh1bb(1),'   gh1bb(R)    =  ',gh1bb(2)
      write(6,11) 'gh2bb(L)    =  ',gh2bb(1),'   gh2bb(R)    =  ',gh2bb(2)
      write(6,11) 'gh3bb(L)    =  ',gh3bb(1),'   gh3bb(R)    =  ',gh3bb(2)
      write(6,*)
 
      write(6,11) 'ghmdc(L)    =  ',ghmdc(1),'   ghmdc(R)    =  ',ghmdc(2)
      write(6,11) 'ghmsu(L)    =  ',ghmsu(1),'   ghmsu(R)    =  ',ghmsu(2)
      write(6,11) 'ghmsc(L)    =  ',ghmsc(1),'   ghmsc(R)    =  ',ghmsc(2)
      write(6,11) 'ghmbt(L)    =  ',ghmbt(1),'   ghmbt(R)    =  ',ghmbt(2)

      write(6,11) 'ghpus(L)    =  ',ghpus(1),'   ghpus(R)    =  ',ghpus(2)
      write(6,11) 'ghpcd(L)    =  ',ghpcd(1),'   ghpcd(R)    =  ',ghpcd(2)
      write(6,11) 'ghpcs(L)    =  ',ghpcs(1),'   ghpcs(R)    =  ',ghpcs(2)
      write(6,11) 'ghptb(L)    =  ',ghptb(1),'   ghptb(R)    =  ',ghptb(2)

      write(6,11) 'ghmmuvm(L)  =  ',ghmmuvm(1),'   ghmmuvm(R)    =  ',ghmmuvm(2)
      write(6,11) 'ghmtavt(L)  =  ',ghmtavt(1),'   ghmtavt(R)    =  ',ghmtavt(2)

      write(6,11) 'ghpvmmu(L)  =  ',ghpvmmu(1),'   ghpvmmu(R)    =  ',ghpvmmu(2)
      write(6,11) 'ghpvtta(L)  =  ',ghpvtta(1),'   ghpvtta(R)    =  ',ghpvtta(2)
      write(6,*)
      write(6,*)


      write(6,*) 'VVS couplings:'
      write(6,*) '-----------------'
      write(6,*)
      write(6,11) 'gwwh1  =  ',gwwh1, '    gwwh2  =  ',gwwh2
      write(6,11) 'gwwh3  =  ',gwwh3, '    gzzh1  =  ',gzzh1
      write(6,11) 'gzzh2  =  ',gzzh2, '    gzzh3  =  ',gzzh3
      write(6,*)

      write(6,*) 'VSS couplings:'
      write(6,*) '-----------------'
      write(6,*)
      write(6,11) 'gzh1h3  =  ',gzh1h3, '   gzh2h3  = ',gzh2h3
      write(6,11) 'gzh1h2  =  ',gzh1h2, '   gahchc  = ',gahchc
      write(6,11) 'gzhchc  =  ',gzhchc, '   gwhch1  = ',gwhch1
      write(6,11) 'gwh1hc  =  ',gwh1hc, '   gwhch2  = ',gwhch2
      write(6,11) 'gwh3hc  =  ',gwh3hc, '   gwhch3  = ',gwhch3
      write(6,*)

      write(6,*) 'SSS couplings:'
      write(6,*) '-----------------'
      write(6,*)   
      write(6,26) 'gh111    =  ',gh111,  '   gh112    = ',gh112
      write(6,26) 'gh113    =  ',gh113,  '   gh122    = ',gh122
      write(6,26) 'gh123    =  ',gh123,  '   gh133    = ',gh133
      write(6,26) 'gh222    =  ',gh222,  '   gh223    = ',gh223
      write(6,26) 'gh233    =  ',gh233,  '   gh333    = ',gh333
      write(6,26) 'gh1hmhp  =  ',gh1hmhp,'   gh2hmhp  = ',gh2hmhp
      write(6,26) 'gh3hmhp  =  ',gh3hmhp
      write(6,*)

      write(6,*) 'VVSS couplings:'
      write(6,*) '-----------------'
      write(6,*)
      write(6,11) 'gzzh1h1  =  ',gzzh1h1, '    gzzh2h2  =  ',gzzh2h2
      write(6,11) 'gzzh3h3  =  ',gzzh3h3, '    gzzh1h2  =  ',gzzh1h2
      write(6,11) 'gzzh1h3  =  ',gzzh1h3, '    gzzh2h3  =  ',gzzh2h3
      write(6,11) 'gwwh1h1  =  ',gwwh1h1, '    gwwh2h2  =  ',gwwh2h2
      write(6,11) 'gwwh3h3  =  ',gwwh3h3, '    gwwh1h2  =  ',gwwh1h2
      write(6,11) 'gwwh1h3  =  ',gwwh1h3, '    gwwh2h3  =  ',gwwh2h3
      write(6,11) 'gaahchc  =  ',gaahchc, '    gazhchc  =  ',gazhchc
      write(6,11) 'gzzhchc  =  ',gzzhchc, '    gwwhchc  =  ',gwwhchc
      write(6,11) 'gwah1hc  =  ',gwah1hc, '    gwah2hc  =  ',gwah2hc
      write(6,11) 'gwah3hc  =  ',gwah3hc, '    gwahch1  =  ',gwahch1
      write(6,11) 'gwahch2  =  ',gwahch2, '    gwahch3  =  ',gwahch3
      write(6,11) 'gwzh1hc  =  ',gwzh1hc, '    gwzh2hc  =  ',gwzh2hc
      write(6,11) 'gwzh3hc  =  ',gwzh3hc, '    gwzhch1  =  ',gwzhch1
      write(6,11) 'gwzhch2  =  ',gwzhch2, '    gwzhch3  =  ',gwzhch3
      write(6,*)
 
      write(6,*)  
      write(6,*)  
      
      return
      end
