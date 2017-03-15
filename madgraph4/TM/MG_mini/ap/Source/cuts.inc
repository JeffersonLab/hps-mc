C
C     NATALIA'S FT CUTS
C
      real*8 ef, thetafmin, mmff, xef, xthetaf, efTot, efRat
     $     , efmax, thetafmax, thetaxfmin, thetaxfmax
     $     , thetayfmin, thetayfmax, xthetaxfmin, xthetaxfmax
     $     , xthetayfmin, xthetayfmax, mmffmax
      COMMON /TO_FT_CUTS/
     $     ef, thetafmin, mmff, xef, xthetaf, efTot, efRat
     $     , efmax, thetafmax, thetaxfmin, thetaxfmax
     $     , thetayfmin, thetayfmax, xthetaxfmin, xthetaxfmax
     $     , xthetayfmin, xthetayfmax, mmffmax

C
C     AUTOMATIC CUTS
C
      REAL*8 PTJ,PTB,PTA,PTL,PTONIUM  ! ptmin
      REAL*8 EJ,EB,EA,EL,ETAONIUM      ! min energy
      REAL*8 ETAJ,ETAB,ETAA,ETAL ! min rapidity
      REAL*8 DRJJ,DRBB,DRLL,DRAA,DRBJ,DRAJ,DRJL,DRAB,DRBL,DRAL ! min distance
      REAL*8 MMJJ,MMLL,MMAA,MMBB !min inv mass
      COMMON /TO_AUTO_CUTS/
     &     PTJ ,PTB ,PTA ,PTL ,PTONIUM, ! pt
     &     EJ  ,EB  ,EA  ,EL  , ! energy
     &     ETAJ,ETAB,ETAA,ETAL,ETAONIUM, ! rapidity 
     &     DRJJ,DRAJ,DRBJ,DRJL, ! delta R
     &     DRBB,DRAB,DRBL,
     &     DRAA,DRAL,
     &     DRLL,
     &     MMJJ,MMLL,MMAA,MMBB ! invariant masses of opposite sign

C     
C     MIN/MAX cuts
C
      REAL*8 PTJmax ,PTBmax ,PTAmax ,PTLmax    ! max pt
      REAL*8 EJmax  ,EBmax  ,EAmax  ,ELmax     ! max rap
      REAL*8 ETAJmin,ETABmin,ETAAmin,ETALmin   ! min rap
      REAL*8 DRJJmax,DRBBmax,DRLLmax,DRAAmax
      REAL*8 DRBJmax,DRAJmax,DRJLmax,DRABmax,DRBLmax,DRALmax
      REAL*8 MMJJmax,MMLLmax,MMAAmax,MMBBmax !max inv mass 
      REAL*8 MMNL,MMNLMAX ! invariant mass of all leptons
      REAL*8 cutuse
      REAL*8 ptj1min,ptj2min,ptj3min,ptj4min
      REAL*8 ptj1max,ptj2max,ptj3max,ptj4max
      REAL*8 ht2min,ht3min,ht4min
      REAL*8 ht2max,ht3max,ht4max
      REAL*8 htjmin,htjmax
      REAL*8 misset,missetmax,ptheavy
      integer maxjetflavor
	
      COMMON /to_min_max_cuts/
     &     PTJmax,PTBmax,PTAmax,PTLmax,
     &     EJmax,EBmax,EAmax,ELmax,
     &     ETAJmin,ETABmin,ETAAmin,ETALmin,
     &     DRJJmax,DRBBmax,DRLLmax,DRAAmax,
     &     DRBJmax,DRAJmax,DRJLmax,DRABmax,DRBLmax,DRALmax,
     &     MMJJmax,MMLLmax,MMAAmax,MMBBmax,
     &     MMNL,MMNLmax,
     &     ptj1min,ptj1max,
     &     ptj2min,ptj2max,
     &     ptj3min,ptj3max,
     &     ptj4min,ptj4max,
     &     cutuse,
     &     ht2min,ht3min,ht4min,
     &     ht2max,ht3max,ht4max,
     &     htjmin,htjmax,
     &     misset,missetmax,ptheavy,
     &     maxjetflavor

C
C     SPECIAL CUTS
C
C-- this are somewhat old kind of cuts that where used for specific
c   purposes
c
      REAL*8 XPTJ,XPTB,XPTA,XPTL
      REAL*8 XETAMIN,XQCUT,deltaeta
      COMMON /TO_SPECXPT/XPTJ,XPTB,XPTA,XPTL,XETAMIN,XQCUT,deltaeta
