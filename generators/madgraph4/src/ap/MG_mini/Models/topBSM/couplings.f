c----------------------------------------------------------------------
C  couplings.f 
c----------------------------------------------------------------------
c  This files takes the inputs of the standard model from a Les Houches 
c  file (param_card.dat) and calculates all the couplings that end up
c  in the Feynman rules, i.e., in the HELAS routines.
c   
c  With the convention of the New Web Generation MadEvent in this
c  part no non-trivial computation is made. The SM model secondary
c  parameters have been all calculated by the SM calculator, SMCalc
c  which produced param_card.dat.
c
c  The only exception to the above rule is for alpha_S. In the case
c  where a pdf is used in the initial state, then the values as(MZ)
c  set in the les houches card is superseeded.
c  Scales and pdf's are set in the run_card.dat.
c
c This file contains the following routines:
c 
c- madgraph original call to set the parameters
c- lh_readin is called from here.
c  It talks to the lh_readin through the common block values.
c      subroutine setpara
c
c-routine to read the LH parameters in
c      subroutine lh_readin
c
c-to set
c      subroutine set_it(npara,ivalue,value,name,id,
c      subroutine case_trap(string,length)
c      subroutine no_spaces(buff,nchars)
c---------------------------------------------------------------------- 


      subroutine setpara(param_name,readlha)
c***********************************************************************
c This subroutine sets up the HELAS couplings of the STANDARD MODEL.
c***********************************************************************
      implicit none
c
c local
c
      character*(*) param_name
      logical readlha
      integer i
      real*8 dum
c
c     common file with the couplings
c
      include 'coupl.inc'
      include 'input.inc'
c
c     local
c
      double precision  v
      double precision  ee, ee2, ez, ey, sw, cw, sc2
      double precision  gwne, gwud, lambda, lam4, xt, rew, rqcd
      double precision  alphas, alfa, alfaw, mfrun
      external          alphas, alfa, alfaw, mfrun
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
c constants
c
      double complex  ci
      parameter( ci = ( 0.0d0, 1.0d0 ) )
      double precision  Zero, One, Two, Three, Four, Half, Rt2
      parameter( Zero = 0.0d0, One = 1.0d0, Two = 2.0d0 )
      parameter( Three = 3.0d0, Four = 4.0d0, Half = 0.5d0 )
      parameter( Rt2   = 1.414213562d0 )
      double precision  Pi, Fourpi
      parameter( Pi = 3.14159265358979323846d0 )
      parameter( Fourpi = Four * Pi )

c
c  Specials for this model
c

c   Zero's of BesselJ function:
      double precision x1,x2,x3,x4,x5,x6,x7,x8,x9,x0
      parameter (x1=3.8317d0)
      parameter (x2=7.0156d0)
      parameter (x3=10.1735d0)
      parameter (x4=13.3237d0)
      parameter (x5=16.4706d0)
      parameter (x6=19.6159d0)
      parameter (x7=22.7601d0)
      parameter (x8=25.9037d0)
      parameter (x9=29.0468d0)
      parameter (x0=32.1897d0)

      double precision lambda2,tau,beta
      double precision delta,p2
      double precision sumdot
      double complex f_tau, totaa1,totaa2,prop
      external sumdot


c Momenta of particles in event
      include '../genps.inc'
      double precision pp(0:3,max_particles)
      common/momenta_pp/pp

c
c     alfas and run
c************
c Uncomment the following lines in order to use alphas from the PDF
c      include '../alfas.inc'
c      include '../run.inc'
c***********
c------------------------------------------
c Start calculating the couplings for HELAS
c------------------------------------------
c
      if(readlha) then 
         call lh_readin(param_name)
         G = DSQRT(4d0*PI*ALFAS) ! use setting of the param_card.dat @ NLO
      endif
c     

      GG(1) = -G
      GG(2) = -G     
c
c auxiliary local values
c
      cw  = sqrt( One - sin2w )
      ee2 = alpha * Fourpi
      sw  = sqrt( sin2w )
      ee  = sqrt( ee2 )
      ez  = ee/(sw*cw)
      ey  = ee*(sw/cw)
      sc2 = sin2w*( One - sin2w )
      v   = Two*wmass*sw/ee   ! the wmass is used to calculate v
      lambda = hmass**2 / (Two * v**2)
c
c vector boson couplings
c
      gw   = ee/sw
      gwwa = ee
      gwwz = ee*cw/sw
c
c gauge & higgs boson coupling constants
c
      gwwh  = dcmplx( ee2/sin2w*Half*v, Zero )
      gzzh  = dcmplx( ee2/sc2*Half*v, Zero )
      ghhh  = dcmplx( -hmass**2/v*Three, Zero )
      gwwhh = dcmplx( ee2/sin2w*Half, Zero )
      gzzhh = dcmplx( ee2/sc2*Half, Zero)
      ghhhh = ghhh/v
c
c fermion-fermion-vector couplings
c
      gal(1) = dcmplx(  ee          , Zero )
      gal(2) = dcmplx(  ee          , Zero )
      gau(1) = dcmplx( -ee*Two/Three, Zero )
      gau(2) = dcmplx( -ee*Two/Three, Zero )
      gad(1) = dcmplx(  ee/Three    , Zero )
      gad(2) = dcmplx(  ee/Three    , Zero )

      gwf(1) = dcmplx( -ee/sqrt(Two*sin2w), Zero )
      gwf(2) = dcmplx(  Zero              , Zero )

      gzn(1) = dcmplx( -ez*Half                     , Zero )
      gzn(2) = dcmplx(  Zero                        , Zero )
      gzl(1) = dcmplx( -ez*(-Half + sin2w)          , Zero )
      gzl(2) = dcmplx( -ey                          , Zero )
      gzu(1) = dcmplx( -ez*( Half - sin2w*Two/Three), Zero )
      gzu(2) = dcmplx(  ey*Two/Three                , Zero )
      gzd(1) = dcmplx( -ez*(-Half + sin2w/Three)    , Zero )
      gzd(2) = dcmplx( -ey/Three                    , Zero )

c fermion-fermion-Higgs couplings (complex) hff(2)
c
c NOTE: the running mass is evaluated @ the same order 
c nloop of alpha_s set by the PDF choice
c 

      if(mtMS.gt.1d0) then
         ghtop(1) = dcmplx( -mtMS/v, Zero )
      else
         ghtop(1) = dcmplx( Zero,Zero)
      endif
      ghtop(2) = ghtop(1)

      if(mbMS.gt.1d0) then
         ghbot(1) = dcmplx( -mbMS/v, Zero )
      else
         ghbot(1) = dcmplx( Zero, Zero )
      endif
      ghbot(2) = ghbot(1)
      
      if(mcMS.gt.1d0) then
         ghcha(1) = dcmplx( -mcMS/v, Zero )
      else
         ghcha(1) = dcmplx( Zero, Zero )
      endif
      ghcha(2) = ghcha(1)

      ghtau(1) = dcmplx( -mtaMS/v, Zero )
      ghtau(2) = ghtau(1)

c
c     CKM matrix: 
c     symmetric 3x3 matrix, Vud=Vcs, Vus=Vcd Vcb=Vub=0
c
c     >>>>>>>>>>>>>>>***** NOTE****<<<<<<<<<<<<<<<<<<<<<<<<<
c     these couplings matter only when interaction_CKM.dat
c     is used to generate all the diagrams with off-diagonal
c     couplings. The default of MadEvent is a diagonal
c     CKM matrix.

	  Vus=DSQRT(1d0-Vud**2)
      do i=1,2
         gwfc(i) = gwf(i)*Vud
         gwfs(i) = gwf(i)*Vus
         gwfm(i) =-gwf(i)*Vus
      enddo

c---------------------------------------------------------
c Set Photon Width to Zero, used by symmetry optimization
c---------------------------------------------------------

      awidth = 0d0
c************************************            
c UserMode couplings
c************************************
      p2 = 2d0*(pp(0,1)*pp(0,2)-pp(1,1)*pp(1,2)-pp(2,1)*pp(2,2)-pp(3,1)*pp(3,2))

c Scalar singlet
      tau = 4D0*TMASS**2/p2
      if (tau.le.1d0)then
         beta = sqrt(1d0-tau)
         s0width = (s0scalarf**2*beta**2 + s0axialf**2)*
     &              3d0*tmass**2*s0mass*beta/(PI*8d0*v**2)
         f_tau = (dLog( (1D0+beta)/
     &                  (1D0-beta)  )-ci*pi)**2/4D0
         totaa1 = tau*(1D0-(1D0-tau)*f_tau)
         totaa2 = tau*f_tau
      else
         totaa1=(0d0,0d0)
         totaa2=(0d0,0d0)
      endif

      Gs0t(1) = dcmplx( -s0scalarf*mtMS/v, -s0axialf*mtMS/v )
      Gs0t(2) = dcmplx( -s0scalarf*mtMS/v,  s0axialf*mtMS/v )
      Gs0g(1) = dcmplx(s0scalarf,Zero) *totaa1*g**2/(8d0*pi**2*v)
      Gs0g(2) = dcmplx(s0axialf ,Zero) *totaa2*g**2/(8d0*pi**2*v)

c Scalar octet
      tau = 4D0*TMASS**2/p2
      if (tau.le.1d0)then
         beta = sqrt(1d0-tau)
         o0width = (o0scalarf**2*beta**2 + o0axialf**2)*
     &              3d0*tmass**2*o0mass*beta/(PI*8d0*v**2)
         o0width = o0width/6d0 ! difference with color singlet
         f_tau = (dLog( (1D0+beta)/
     &                  (1D0-beta)  )-ci*pi)**2/4D0
         totaa1 = tau*(1D0-(1D0-tau)*f_tau)
         totaa2 = tau*f_tau
      else
         totaa1=(0d0,0d0)
         totaa2=(0d0,0d0)
      endif
      Go0t(1) = dcmplx( -o0scalarf*mtMS/v, -o0axialf*mtMS/v )
      Go0t(2) = dcmplx( -o0scalarf*mtMS/v,  o0axialf*mtMS/v )
      Go0g(1) = dcmplx(o0scalarf,Zero) *totaa1*g**2/(8d0*pi**2*v)
      Go0g(2) = dcmplx(o0axialf ,Zero) *totaa2*g**2/(8d0*pi**2*v)


c spin-1 singlet
      Gs1D(1)=s1qdleft *dcmplx( -ez*(-Half + sin2w/Three)    , Zero )
      Gs1D(2)=s1qdright*dcmplx( -ey/Three                    , Zero )
      Gs1U(1)=s1quleft *dcmplx( -ez*( Half - sin2w*Two/Three), Zero )
      Gs1U(2)=s1quright*dcmplx(  ey*Two/Three                , Zero )
      Gs1S(1)=s1qsleft *dcmplx( -ez*(-Half + sin2w/Three)    , Zero )
      Gs1S(2)=s1qsright*dcmplx( -ey/Three                    , Zero )
      Gs1C(1)=s1qcleft *dcmplx( -ez*( Half - sin2w*Two/Three), Zero )
      Gs1C(2)=s1qcright*dcmplx(  ey*Two/Three                , Zero )
      Gs1B(1)=s1qbleft *dcmplx( -ez*(-Half + sin2w/Three)    , Zero )
      Gs1B(2)=s1qbright*dcmplx( -ey/Three                    , Zero )
      Gs1T(1)=s1qtleft *dcmplx( -ez*( Half - sin2w*Two/Three), Zero )
      Gs1T(2)=s1qtright*dcmplx(  ey*Two/Three                , Zero )
      Gs1E(1)=s1eleft  *dcmplx( -ez*(-Half + sin2w)          , Zero )
      Gs1E(2)=s1eright *dcmplx( -ey                          , Zero )
      Gs1Ne(1)=s1ne    *dcmplx( -ez*Half                     , Zero )
      Gs1Ne(2)=s1ne    *dcmplx(  Zero                        , Zero )
      Gs1Mu(1)=s1muleft  *dcmplx( -ez*(-Half + sin2w)          , Zero )
      Gs1Mu(2)=s1muright *dcmplx( -ey                          , Zero )
      Gs1Nm(1)=s1nm    *dcmplx( -ez*Half                     , Zero )
      Gs1Nm(2)=s1nm    *dcmplx(  Zero                        , Zero )
      Gs1Ta(1)=s1taleft  *dcmplx( -ez*(-Half + sin2w)          , Zero )
      Gs1Ta(2)=s1taright *dcmplx( -ey                          , Zero )
      Gs1Nt(1)=s1nt    *dcmplx( -ez*Half                     , Zero )
      Gs1Nt(2)=s1nt    *dcmplx(  Zero                        , Zero )

c spin-1 octet
      Go1D(1)=o1qdleft *dcmplx(GG(1),Zero)
      Go1D(2)=o1qdright*dcmplx(GG(2),Zero)
      Go1U(1)=o1quleft *dcmplx(GG(1),Zero)
      Go1U(2)=o1quright*dcmplx(GG(2),Zero)
      Go1S(1)=o1qsleft *dcmplx(GG(1),Zero)
      Go1S(2)=o1qsright*dcmplx(GG(2),Zero)
      Go1C(1)=o1qcleft *dcmplx(GG(1),Zero)
      Go1C(2)=o1qcright*dcmplx(GG(2),Zero)
      Go1B(1)=o1qbleft *dcmplx(GG(1),Zero)
      Go1B(2)=o1qbright*dcmplx(GG(2),Zero)
      Go1T(1)=o1qtleft *dcmplx(GG(1),Zero)
      Go1T(2)=o1qtright*dcmplx(GG(2),Zero)


c  ADD spin-2
      prop = dcmplx(p2-s2mass**2,s2mass*s2width)
     &       /(mstring**4/16d0/pi/(addn-2d0))
      s2mass = mstring
      Gs2q = dcmplx(-Rt2/8d0  ,zero)*prop
      Gs2b = dcmplx(-Rt2/8d0  ,zero)*prop
      Gs2t = dcmplx(-Rt2/8d0  ,zero)
      Gs2g = dcmplx(-Rt2/2d0  ,zero)*prop


c  RS spin-2      
      g2mass=x2*g1mass/x1
      g3mass=x3*g1mass/x1
      g4mass=x4*g1mass/x1
      g5mass=x5*g1mass/x1
      g6mass=x6*g1mass/x1
      g7mass=x7*g1mass/x1
      g8mass=x8*g1mass/x1
      g9mass=x9*g1mass/x1
      g0mass=x0*g1mass/x1

      g1width=g1mass*kapMpl**2/16d0/Pi*x1**2*delta(g1mass)
      g2width=g2mass*kapMpl**2/16d0/Pi*x2**2*delta(g2mass)
      g3width=g3mass*kapMpl**2/16d0/Pi*x3**2*delta(g3mass)
      g4width=g4mass*kapMpl**2/16d0/Pi*x4**2*delta(g4mass)
      g5width=g5mass*kapMpl**2/16d0/Pi*x5**2*delta(g5mass)
      g6width=g6mass*kapMpl**2/16d0/Pi*x6**2*delta(g6mass)
      g7width=g7mass*kapMpl**2/16d0/Pi*x7**2*delta(g7mass)
      g8width=g8mass*kapMpl**2/16d0/Pi*x8**2*delta(g8mass)
      g9width=g9mass*kapMpl**2/16d0/Pi*x9**2*delta(g9mass)
      g0width=g0mass*kapMpl**2/16d0/Pi*x0**2*delta(g0mass)

      lambda2 = g1mass/kapMpl/x1

      Ggrq  = dcmplx(-Rt2/4d0/lambda2,zero)
      Ggrb  = dcmplx(-Rt2/4d0/lambda2,zero)
      Ggrt  = dcmplx(-Rt2/4d0/lambda2,zero)
      Ggrgg = dcmplx(-Rt2/lambda2    ,zero)

c----------------------------
c end subroutine coupsm
c----------------------------


      return
      end



      

      double precision function delta(mass)
c--------------------------------------------
c calculates part of width of RS KK graviton
c--------------------------------------------
      implicit none
      double precision mass
      include "coupl.inc"

      delta=1d0/5d0
      delta=delta+8d0/5d0
      delta=delta+2d0/5d0*dsqrt(1d0-4d0*wmass**2/mass**2)*
     .(13d0/12d0+14d0/3d0*wmass**2/mass**2+4d0*wmass**4/mass**4)
      delta=delta+1d0/5d0*dsqrt(1d0-4d0*zmass**2/mass**2)*
     .(13d0/12d0+14d0/3d0*zmass**2/mass**2+4d0*zmass**4/mass**4)
      delta=delta+1d0/30d0*(1d0-4d0*hmass**2/mass**2)**(5d0/2d0)
      delta=delta+1d0/10d0                   !veve~
      delta=delta+1d0/10d0                   !vmvm~
      delta=delta+1d0/10d0                   !vtvt~
      delta=delta+1d0/10d0                   !e+e-
      delta=delta+1d0/10d0                   !mu+mu-
      delta=delta+1d0/10d0*(1d0-4d0*lmass**2/mass**2)**(3d0/2d0)*
     .(1d0+8d0/3d0*lmass**2/mass**2)         !tau+tau-
      delta=delta+3d0/10d0                   !uu~
      delta=delta+3d0/10d0                   !dd~
      delta=delta+3d0/10d0                   !ss~
      delta=delta+3d0/10d0                   !cc~
      delta=delta+3d0/10d0*(1d0-4d0*bmass**2/mass**2)**(3d0/2d0)*
     .(1d0+8d0/3d0*bmass**2/mass**2)         !bb~
      delta=delta+3d0/10d0*(1d0-4d0*tmass**2/mass**2)**(3d0/2d0)*
     .(1d0+8d0/3d0*tmass**2/mass**2)         !tt~
      

      end
