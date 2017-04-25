      subroutine printout
      implicit none
c
c     local
c
      integer i,iformat
      character*2 ab(2)
      real*8 ene
      double precision  Zero, One, Two, Three, Four, Half, Rt2
      parameter( Zero = 0.0d0, One = 1.0d0, Two = 2.0d0 )
c
c     include
c
      include 'coupl.inc'
      include 'input.inc'
c
c     Common to lh_readin and printout
c
      double precision  alpha, sin2w, gfermi, alfas
      double precision  mtMS,mbMS,mcMS,mtaMS!MSbar masses
      double precision  Vud,Vus             !CKM matrix elements
      common/values/    alpha,sin2w,gfermi,alfas,
     &                  mtMS,mbMS,mcMS,mtaMS,
     &                  Vud
c
c output all info
c
 10   format( 1x,a16,2x,f7.3,' GeV        ',a16,2x,f7.4,' GeV' )
 11   format( 1x,a13,2x,f11.5,2x,f11.5,2x,a13,2x,f11.5,2x,f11.5 )
 12   format( 1x,a13,2x,f6.2,a )
 13   format( 1x,a13,2x,f6.4,a )
 14   format( 1x,2(a13,2x,f10.7,2x,f10.7) )
 15   format( 1x,a13,2x,f9.5,a )
 16   format( 1x,a13,2x,f7.5 )
 17   format( 1x,a13,2x,f8.4 )
 18   format( 1x,a13,2x,f8.4,' GeV' )
 19   format( 1x,a13,2x,f6.4,a13,2x,f6.4 )
 20   format( 1x,a13,2x,f11.5,1x,f11.5 )
 21   format( 1x,a13,2x,f8.4,' GeV',1x,a13,2x,f8.4,' GeV' )
 22   format( 1x,a13,2x,f10.8,a13,2x,f6.4 )
 23   format( 1x,a13,2x,f8.4)
 24   format( 1x,a16,2x,f7.3,' GeV        ',a16,2x,f7.4,' GeV  (calc @ LO)')
 25   format( 1x,a16,2x,f7.3,' GeV        ',a16,2x,f7.4,' GeV')


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
      write(*,*) '*          SUMMARY OF THE SM PARAMETERS             *'
      write(*,*) '*****************************************************'
      write(6,*)
      write(6,*)  ' EW Parameters:'
      write(6,*)  '---------------'
      write(6,*)
      write(6,23) ' GF (10^-5*GeV^-2) = ',gfermi*1d5
      write(6,23) ' 1/alpha           = ',1d0/alpha
      write(6,23) ' sin^2(W)          = ',sin2w
      write(6,23) ' M_Z   (GeV)       = ',zmass
      write(6,23) ' M_W   (GeV)       = ',wmass
      write(6,*)
      write(6,*)
      write(6,*)  'Boson masses and widths:'
      write(6,*)  '------------------------'
      write(6,*)
      write(6,24) 'Z mass  =  ',zmass, 'Z width  = ',zwidth
      write(6,24) 'W mass  =  ',wmass, 'W width  = ',wwidth
      write(6,24) 'H mass  =  ',hmass, 'H width  = ',hwidth
      write(6,*)
      write(6,*)  'Fermion masses and widths:'
      write(6,*)  '--------------------------'
      write(6,*)
      write(6,24) 'top    mass  =  ', tmass, 'top    width  = ', twidth
      write(6,10) 'bottom mass  =  ', bmass, 'bottom width  = ', Zero
      write(6,10) 'charm  mass  =  ', cmass, 'charm  width  = ', Zero
      write(6,10) 'tau    mass  =  ', lmass, 'tau    width  = ', Zero
      write(6,*)  'all other quark and lepton masses set to zero'
      write(6,*)
      write(6,*)  'User Model couplings:'
      write(6,*)  '---------------------'
      write(6,*)
      write(6,11) 'Gs0t(L)  =  ', Gs0t(1),'Gs0t(R)  =  ', Gs0t(2)
      write(6,20) 'Gs0g  =  ', Gs0g
      write(6,11) 'Gs1D(L)  =  ', Gs1D(1),'Gs1D(R)  =  ', Gs1D(2)
      write(6,11) 'Gs1U(L)  =  ', Gs1U(1),'Gs1U(R)  =  ', Gs1U(2)
      write(6,11) 'Gs1S(L)  =  ', Gs1S(1),'Gs1S(R)  =  ', Gs1S(2)
      write(6,11) 'Gs1C(L)  =  ', Gs1C(1),'Gs1C(R)  =  ', Gs1C(2)
      write(6,11) 'Gs1B(L)  =  ', Gs1B(1),'Gs1B(R)  =  ', Gs1B(2)
      write(6,11) 'Gs1T(L)  =  ', Gs1T(1),'Gs1T(R)  =  ', Gs1T(2)
      write(6,11) 'Gs1E(L)  =  ', Gs1E(1),'Gs1E(R)  =  ', Gs1E(2)
      write(6,11) 'Gs1Mu(L) =  ', Gs1Mu(1),'Gs1Mu(R) =  ', Gs1Mu(2)
      write(6,11) 'Gs1Ta(L) =  ', Gs1Ta(1),'Gs1Ta(R) =  ', Gs1Ta(2)
      write(6,11) 'Gs1Ne(L) =  ', Gs1Ne(1),'Gs1Ne(R) =  ', Gs1Ne(2)
      write(6,11) 'Gs1Nm(L) =  ', Gs1Nm(1),'Gs1Nm(R) =  ', Gs1Nm(2)
      write(6,11) 'Gs1Nt(L) =  ', Gs1Nt(1),'Gs1Nt(R) =  ', Gs1Nt(2)
      write(6,11) 'Go1D(L)  =  ', Go1D(1),'Go1D(R)  =  ', Go1D(2)
      write(6,11) 'Go1U(L)  =  ', Go1U(1),'Go1U(R)  =  ', Go1U(2)
      write(6,11) 'Go1S(L)  =  ', Go1S(1),'Go1S(R)  =  ', Go1S(2)
      write(6,11) 'Go1C(L)  =  ', Go1C(1),'Go1C(R)  =  ', Go1C(2)
      write(6,11) 'Go1B(L)  =  ', Go1B(1),'Go1B(R)  =  ', Go1B(2)
      write(6,11) 'Go1T(L)  =  ', Go1T(1),'Go1T(R)  =  ', Go1T(2)
      write(6,11) 'Gs2q  =  ', Gs2q
      write(6,20) 'Gs2g  =  ', Gs2g
      write(6,20) 'Ggrq  =  ', Ggrq
      write(6,20) 'Ggrgg  =  ', Ggrgg
      write(6,*)
      write(6,*) 'Boson couplings:'
      write(6,*) '----------------'
      write(6,*)
      write(6,20) 'gwwa  = ', gwwa
      write(6,20) 'gwwz  = ', gwwz
      write(6,20) 'gwwh  = ', gwwh
      write(6,20) 'gzzh  = ', gzzh
      write(6,20) 'ghhh  = ', ghhh
      write(6,*)
      write(6,20) 'gwwhh = ', gwwhh
      write(6,20) 'gzzhh = ', gzzhh
      write(6,20) 'ghhhh = ', ghhhh
      write(6,*)
      write(6,*) 'FFV couplings:'
      write(6,*) '--------------'
      write(6,*)
      write(6,11) 'gal(L)   =  ',gal(1), 'gal(R)   =  ',gal(2)
      write(6,11) 'gau(L)   =  ',gau(1), 'gau(R)   =  ',gau(2)
      write(6,11) 'gad(L)   =  ',gad(1), 'gad(R)   =  ',gad(2)
      write(6,*)
      write(6,11) 'gwf(L)   =  ',gwf(1), 'gwf(R)   =  ',gwf(2)
      write(6,*)
      write(6,11) 'gzn(L)   =  ',gzn(1), 'gzn(R)   =  ',gzn(2)
      write(6,11) 'gzl(L)   =  ',gzl(1), 'gzl(R)   =  ',gzl(2)
      write(6,11) 'gzu(L)   =  ',gzu(1), 'gzu(R)   =  ',gzu(2)
      write(6,11) 'gzd(L)   =  ',gzd(1), 'gzd(R)   =  ',gzd(2)
      write(6,*)
      write(6,*) 'FFH couplings:'
      write(6,*) '--------------'
      write(6,*)
      write(6,14) 'gHtop(L) =  ',ghtop(1), 'gHtop(R) =  ',ghtop(2)
      write(6,14) 'gHbot(L) =  ',ghbot(1), 'gHbot(R) =  ',ghbot(2)
      write(6,14) 'gHcha(L) =  ',ghcha(1), 'gHcha(R) =  ',ghcha(2)
      write(6,14) 'gHtau(L) =  ',ghtau(1), 'gHtau(R) =  ',ghtau(2)
      write(6,*)
      write(6,*) 'Strong couplings:'
      write(6,*) '-----------------'
      write(6,*)
      write(6,14) 'gg(1)    =  ',gg(1)   , 'gg(2)    =  ',gg(2)
      write(6,*)
      write(6,*) 'User Model masses and widths:'
      write(6,*) '-----------------------------'
      write(6,*)
      write(6,24) 'S0MASS  =  ', S0MASS, 'S0WIDTH    = ', S0WIDTH
      write(6,24) 'S1MASS  =  ', S1MASS, 'S1WIDTH    = ', S1WIDTH
      write(6,24) 'O1MASS  =  ', O1MASS, 'O1WIDTH    = ', O1WIDTH
      write(6,24) 'S2MASS  =  ', S2MASS, 'S2WIDTH    = ', S2WIDTH
      write(6,24) 'g1mass  =  ', g1mass, 'g1width    = ', g1width
      write(6,24) 'g2mass  =  ', g2mass, 'g2width    = ', g2width
      write(6,24) 'g3mass  =  ', g3mass, 'g3width    = ', g3width
      write(6,24) 'g4mass  =  ', g4mass, 'g4width    = ', g4width
      write(6,24) 'g5mass  =  ', g5mass, 'g5width    = ', g5width
      write(6,24) 'g6mass  =  ', g6mass, 'g6width    = ', g6width
      write(6,24) 'g7mass  =  ', g7mass, 'g7width    = ', g7width
      write(6,24) 'g8mass  =  ', g8mass, 'g8width    = ', g8width
      write(6,24) 'g9mass  =  ', g9mass, 'g9width    = ', g9width
      write(6,24) 'g0mass  =  ', g0mass, 'g0width    = ', g0width
      write(6,*)
      write(6,*) 'User Model Parameters:'
      write(6,*) '----------------------'
      write(6,24) 's0scalarf    =  ', s0scalarf  
      write(6,24) 's0axialf     =  ', s0axialf    
      write(6,24) 'o0scalarf    =  ', o0scalarf  
      write(6,24) 'o0axialf     =  ', o0axialf   
      write(6,24) 's1quleft     =  ', s1quleft   
      write(6,24) 's1quright    =  ', s1quright  
      write(6,24) 's1qdleft     =  ', s1qdleft   
      write(6,24) 's1qdright    =  ', s1qdright  
      write(6,24) 's1qcleft     =  ', s1qcleft   
      write(6,24) 's1qcright    =  ', s1qcright  
      write(6,24) 's1qsleft     =  ', s1qsleft   
      write(6,24) 's1qsright    =  ', s1qsright  
      write(6,24) 's1qtleft     =  ', s1qtleft   
      write(6,24) 's1qtright    =  ', s1qtright  
      write(6,24) 's1qbleft     =  ', s1qbleft   
      write(6,24) 's1qbright    =  ', s1qbright  
      write(6,24) 's1eleft      =  ', s1eleft    
      write(6,24) 's1eright     =  ', s1eright   
      write(6,24) 's1muleft     =  ', s1muleft    
      write(6,24) 's1muright    =  ', s1muright   
      write(6,24) 's1taleft     =  ', s1taleft    
      write(6,24) 's1taright    =  ', s1taright   
      write(6,24) 's1ne         =  ', s1ne      
      write(6,24) 's1nm         =  ', s1nm      
      write(6,24) 's1nt         =  ', s1nt      
      write(6,24) 'o1quleft     =  ', o1quleft   
      write(6,24) 'o1quright    =  ', o1quright  
      write(6,24) 'o1qdleft     =  ', o1qdleft   
      write(6,24) 'o1qdright    =  ', o1qdright  
      write(6,24) 'o1qcleft     =  ', o1qcleft   
      write(6,24) 'o1qcright    =  ', o1qcright  
      write(6,24) 'o1qsleft     =  ', o1qsleft   
      write(6,24) 'o1qsright    =  ', o1qsright  
      write(6,24) 'o1qtleft     =  ', o1qtleft   
      write(6,24) 'o1qtright    =  ', o1qtright  
      write(6,24) 'o1qbleft     =  ', o1qbleft   
      write(6,24) 'o1qbright    =  ', o1qbright  
      write(6,24) 'kapMpl       =  ', kapMpl     
      write(6,24) 'addn         =  ', addn       
      write(6,24) 'mstring      =  ', mstring    
      write(6,*)

      return
      end

