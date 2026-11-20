!***********************************************************************
! moller_v5: moller_v4 plus Moller scattering off bound target electrons
! (impulse approximation with Hartree-Fock shell Compton profiles); see
! subroutines bndini and bndmol at the end of this file.
!***********************************************************************
!------------------------------- main code -----------------------------

!-----------------------------------------------------------------------
!     Step 1. Initialization
!-----------------------------------------------------------------------

      implicit none

!     ------------
!     EGS5 COMMONs
!     ------------
      include 'include/egs5_h.f'                ! Main EGS "header" file

      include 'include/egs5_epcont.f'
      include 'include/egs5_bounds.f'
      include 'include/egs5_edge.f'
      include 'include/egs5_media.f'
      include 'include/egs5_misc.f'
      include 'include/egs5_thresh.f'
      include 'include/egs5_useful.f'
      include 'include/egs5_usersc.f'
      include 'include/egs5_userxt.f'
      include 'include/egs5_brempr.f'
      include 'include/randomm.f'

!     ----------------------
!     Auxiliary-code COMMONs
!     ----------------------
      include 'auxcommons/lines.f'

      include 'src/stdhep.inc'

      real*8 rndm, dx, dy, sigma/0.01/

      integer npart, moller, ngam, iadd(10), itrig
      real qpart, ppart, vtxpart, am
      common /tmpart/ npart,moller,qpart(10),ppart(4,10),vtxpart(3,10)

      real*8 zplane, elum, elumtot, elum30, elum35, elum40, elum45
      common /lumon/ zplane(205), elum(50)

      real*8 w
c 0.1 X0 
c      real*8 w/0.035/
c 0.01 X0 
c      real*8 w/0.0035/
c 0.005 X0
c      real*8 w/0.00175d0/
c 0.0035 X0
c      real*8 w/0.001225d0/
c 0.0025 X0
c      real*8 w/0.000875d0/
c 0.00125 X0
c      real*8 w/0.0004375d0/
c 0.001 X0 
c      real*8 w/0.00035d0/
c 0.00075 X0 
c      real*8 w/0.0002625d0/
c 0.0005 X0 
c      real*8 w/0.000175d0/
c 0.00025 X0 
c      real*8 w/0.0000875d0/

c need long integer to generate 10^9
      integer*8 ncases
c      integer*16 ncases
      real*8 ebeam
      real*8 esum, elyr
      common/totals/esum(205), elyr(20,30)

      integer lyr
      real*8 etemp(4)/4*0.0d0/

      real*8 ei,ekin,etot,totke,xi,yi,zi,   ! Arguments
     *       ui,vi,wi,wti
      real tarray(2), emax, irmax, e1, e2, e3, d

      real t0,t1,timecpu,tt              ! Local variables
      real etime
      integer i,j,k,idinc,iqi,iri
      character*24 medarr(4)

      integer istream,lok

c ==================

!     ----------
!     Open files
!     ----------
      open(UNIT= 6,FILE='egs5job.out',STATUS='unknown')

      open(51,file='moller.dat',form='FORMATTED')
      open(52,file='seed.dat',form='FORMATTED')

      call stdxwopen('moller.stdhep', ' ascii events ', 
     -     ncases, istream, lok)
      call stdxwrt(100, istream, lok)

!     ====================
      call counters_out(0)
!     ====================

!-----------------------------------------------------------------------
! Step 2: pegs5-call
!-----------------------------------------------------------------------
!     ==============
      call block_set                 ! Initialize some general variables
!     ==============

c Brems parameters
      ibrdst = 1
      iprdst = 2
c
!     ---------------------------------
!     define media before calling PEGS5
!     ---------------------------------

      nmed=4
      medarr(1)='W-RAYLEIGH              '
      medarr(2)='SI-RAYLEIGH             '
      medarr(3)='AU-RAYLEIGH             '
      medarr(4)='AIR AT NTP              '

      do j=1,nmed
        do i=1,24
          media(i,j)=medarr(j)(i:i)
        end do
      end do  

      chard(1) = 0.0001
      chard(2) = 0.0001
      chard(3) = 0.0001
      chard(4) = 0.0001
!     ------------------------------
!     Run PEGS5 before calling HATCH
!     ------------------------------
      write(6,100)
100   FORMAT(' PEGS5-call comes next')

!     =============
      call pegs5
!     =============


!-----------------------------------------------------------------------
! Step 3: Pre-hatch-call-initialization
!-----------------------------------------------------------------------

      med(1) = 0
      med(2) = 1
      med(3) = 0
!     ----------------------------------
!     Set of option flag for region 2-3
!     1: on, 0: off
!     ----------------------------------
      nreg=3

      do i=1,nreg
        ecut(i)=10.       ! egs cut off energy for electrons
        pcut(i)=10.      ! egs cut off energy for photons
        iphter(i) = 0       ! Switches for PE-angle sampling
        iedgfl(i) = 0       ! K & L-edge fluorescence
        iauger(i) = 0       ! K & L-Auger
        iraylr(i) = 1       ! Rayleigh scattering
        lpolar(i) = 0       ! Linearly-polarized photon scattering
        incohr(i) = 0       ! S/Z rejection
        iprofr(i) = 0       ! Doppler broadening
        impacr(i) = 0       ! Electron impact ionization
      end do

!     --------------------------------------------------------
!     Random number seeds.  Must be defined before call hatch.
!     ins (1- 2^31)
!     --------------------------------------------------------
c      inseed=1030701
      read(52,*) inseed,w,ebeam,ncases
 706  format(5x,i5)
      write(6,707) inseed
 707  format(2x,'seed=',i10)

      luxlev=1

!     =============
      call rluxinit   ! Initialize the Ranlux random-number generator
!     =============

!-----------------------------------------------------------------------
! Step 4:  Determination-of-incident-particle-parameters
!-----------------------------------------------------------------------
c Moller flag
c before electr
c      iausfl(9) = 1
c after electr
      iausfl(10) = 1

      iqi= -1
      xi=0.0
      yi=0.0
      zi=0.0
      ui=0.0
      vi=0.0
      wi=1.0
      iri=2
      wti=1.0
      idinc=-1
cx      ei=11000.0D0
cx      ei=6600.0D0
cx      ei=3300.0D0
cx      ei=1056.0D0
cx      ei=1100.0D0
      ei=ebeam
      ekin=ei+iqi*RM

!-----------------------------------------------------------------------
! Step 5:   hatch-call
!-----------------------------------------------------------------------
! Total energy of incident source particle must be defined before hatch
! Define posible maximum total energy of electron before hatch
      if (iqi.ne.0) then
        emaxe = ei              ! charged particle
      else
        emaxe = ei + RM         ! photon
      end if

!     ------------------------------
!     Open files (before HATCH call)
!     ------------------------------
      open(UNIT=KMPI,FILE='pgs5job.pegs5dat',STATUS='old')
      open(UNIT=KMPO,FILE='egs5job.dummy',STATUS='unknown')

      write(6,130)
130   FORMAT(/,' HATCH-call comes next',/)

!     ==========
      call hatch
!     ==========

!     ------------------------------
!     Close files (after HATCH call)
!     ------------------------------
      close(UNIT=KMPI)
      close(UNIT=KMPO)

!     ------------------------------------------
!     Bound-electron Moller tables (after HATCH)
!     ------------------------------------------
      call bndini

! ----------------------------------------------------------
! Print various data associated with each media (not region)
! ----------------------------------------------------------
      write(6,140)
140   FORMAT(/,' Quantities associated with each MEDIA:')
      do j=1,nmed
        write(6,150) (media(i,j),i=1,24)
150     FORMAT(/,1X,24A1)
        write(6,160) rhom(j),rlcm(j)
160     FORMAT(5X,' rho=',G15.7,' g/cu.cm     rlc=',G15.7,' cm')
        write(6,170) ae(j),ue(j)
170     FORMAT(5X,' ae=',G15.7,' MeV    ue=',G15.7,' MeV')
        write(6,180) ap(j),up(j)
180     FORMAT(5X,' ap=',G15.7,' MeV    up=',G15.7,' MeV',/)
      end do

!-----------------------------------------------------------------------
! Step 6:  Initialization-for-howfar
!-----------------------------------------------------------------------
      zplane(1) = 0.0
      zplane(2) = zplane(1) + w

!-----------------------------------------------------------------------
! Step 7:  Initialization-for-ausgab
!-----------------------------------------------------------------------
      do i=1,nreg
        esum(i)=0.D0
      end do

      nlines=0
      nwrite=15

!-----------------------------------------------------------------------
! Step 8:  Shower-call
!-----------------------------------------------------------------------
      tt=etime(tarray)
      t0=tarray(1)

      write(6,190)
190   format(/,' Shower Results:',///,7X,'e',14X,'z',14X,'w',10X,
     1   'iq',3X,'ir',2X,'iarg',/)

      nevhep = 0

      do i=1,ncases
         nhep = 0
         moller = 0

        call shower(iqi,ei,xi,yi,zi,ui,vi,wi,iri,wti)

        if(moller.eq.1.and.nhep.ge.2) then
           nevhep = nevhep + 1
           call stdxwrt(  1, istream, lok)
        end if
      end do

 999  continue

      tt=etime(tarray)
      t1=tarray(1)

      timecpu=t1-t0
      write(6,210) timecpu
210   format(/,' Elapsed Time (sec)=',1PE12.5)

!-----------------------------------------------------------------------
! Step 9:  Output-of-results
!-----------------------------------------------------------------------
      totke=ncases*ekin
      write(6,220) ei,ncases
220   format(//,' Incident total energy of electron=',F12.1,' MeV',/,
     * ' Number of cases in run=',I10,
     *//,' Energy deposition summary:',/)

      etot=0.D0
      do i=1,nreg
        etot = etot + esum(i)
        esum(i)=esum(i)/totke 
        write(6,230) i, esum(i)
230     format(2x,I3,F10.7)
      end do

      etot=etot/totke
      write(6,240) etot
240   FORMAT(//,' Total energy fraction in run=',G15.7,/, 
     *'   Which should be close to unity')
!     -----------
!     Close files
!     -----------
      call bndsum
      close(UNIT=6)
      call stdxwrt(200, istream, lok)
      call stdxend(istream)

      stop
      end
!-------------------------last line of main code------------------------
!-------------------------------ausgab.f--------------------------------
! Version:   050701-1615
! Reference: SLAC-R-730, KEK-2005-8 (Appendix 2)
!-----------------------------------------------------------------------
!23456789|123456789|123456789|123456789|123456789|123456789|123456789|12
! ----------------------------------------------------------------------
! Required subroutine for use with the EGS5 Code System
! ----------------------------------------------------------------------
! A simple AUSGAB to:
!
!   1) Score energy deposition
!   2) Print out stack information
!   3) Print out particle transport information (if switch is turned on)
!
! ----------------------------------------------------------------------

      subroutine ausgab(iarg)

      implicit none

      include 'include/egs5_h.f'                ! Main EGS "header" file

      include 'include/egs5_epcont.f'    ! COMMONs required by EGS5 code
      include 'include/egs5_stack.f'
      include 'auxcommons/lines.f'

      include 'src/stdhep.inc'

      real*8 esum, elyr
      common/totals/esum(205), elyr(20,30)
      
      real*8 zplane, elum
      common /lumon/ zplane(205), elum(50)

      integer npart, moller
      real qpart, ppart, vtxpart
      common /tmpart/ npart,moller,qpart(10),ppart(4,10),vtxpart(3,10)

      integer lyr, ira, nin/0/
      real dr/0.25/, am/0.511/, p

      real*8 cth, sth, et

      integer i, iarg, n, nphot, iout 

!     ----------------------
!     Add deposition energy
!     ----------------------

c brems before electr call
c      if(iarg.eq.6) then
c         write(52,777) iq(np),e(np),u(np),v(np),w(np)
c 777     format(2x,i3,f10.3,3f10.6)
c      end if
c Moller after electr call
      if(iarg.eq.9) then
         moller = 1
         call bndmol
c         if(e(np).gt.10.0.and.e(np-1).gt.10.0)
c     -     write(52,778) iq(np),e(np),u(np),v(np),w(np),
c     -                 iq(np-1),e(np-1),u(np-1),v(np-1),w(np-1)
c 778     format(2x,i3,f10.3,3f10.6,i3,f10.3,3f10.6)
      end if

      if(iarg.ne.0.and.iarg.ne.3) return

      esum(ir(np))=esum(ir(np)) + edep

      nin = nin + 1
c
cx      if(ir(np).eq.3.and.iq(np).eq.0.and.e(np).gt.20.0) then
cx         write(52,700) e(np),u(np),v(np),w(np)
cx 700     format(1x,4e14.6)
cx      end if
      if(ir(np).eq.3.and.e(np).gt.150.0.and.
     -   sqrt(u(np)**2+v(np)**2).gt.0.013) then
         nhep = nhep + 1
         p = sqrt(e(np)**2-am**2)*0.001
         if (iq(np).eq.0) then
                 idhep(nhep) = 22
                 phep(5,nhep) = 0
                 p = e(np)*0.001
         else if (iq(np).eq.-1) then
                 idhep(nhep) = 11
                 phep(5,nhep) = am*0.001
         else if (iq(np).eq.1) then
                 idhep(nhep) = -11
                 phep(5,nhep) = am*0.001
         else
                 stop
         end if

         phep(1,nhep) = p * u(np)
         phep(2,nhep) = p * v(np)
         phep(3,nhep) = p * w(np)
         phep(4,nhep) = e(np) * 0.001
         vhep(1,nhep) = x(np) * 10.
         vhep(2,nhep) = y(np) * 10.
         vhep(3,nhep) = z(np) * 10.
         vhep(4,nhep) = 0.
         isthep(nhep) = 1
         jmohep(1,nhep) = 0
         jmohep(2,nhep) = 0
         jdahep(1,nhep) = 0
         jdahep(2,nhep) = 0

c         npart = npart + 1
c         qpart(npart) = iq(np)
c         p = e(np)
c         if(iq(np).ne.0) p = sqrt(e(np)**2-am**2)
c         ppart(1,npart) = p * u(np) * 0.001
c         ppart(2,npart) = p * v(np) * 0.001
c         ppart(3,npart) = p * w(np) * 0.001
c         ppart(4,npart) = e(np) * 0.001
c         vtxpart(1,npart) = x(np)
c         vtxpart(2,npart) = y(np)
c         vtxpart(3,npart) = 0.0
      end if
!     ----------------------------------------------------------------
!     Print out stack information (for limited number cases and lines)
!     ----------------------------------------------------------------

c      if (nlines.lt.nwrite) then
c      if(ir(np).eq.1)
c     -  write(6,1240) e(np),z(np),w(np),iq(np),ir(np),iarg
1240    FORMAT(3G15.7,3I5)
c        nlines=nlines+1
c      end if

c        if(nin.le.10000)
c     -write(51,600)nin,iarg,iq(np),e(np),x(np),y(np),z(np),u(np),
c     -             v(np),w(np),ustep
c 600    format(i8,2i3,f8.4,3f10.4,4f10.5)
      
      return
      end
!--------------------------last line of ausgab.f------------------------
!-------------------------------howfar.f--------------------------------
! Version:   050701-1615
! Reference: SLAC-R-730, KEK-2005-8 (Appendix 2)
!-----------------------------------------------------------------------
!23456789|123456789|123456789|123456789|123456789|123456789|123456789|12
! ----------------------------------------------------------------------
! Required (geometry) subroutine for use with the EGS5 Code System
! ----------------------------------------------------------------------
! This is a 1-dimensional plane geometry. 
! ----------------------------------------------------------------------

      subroutine howfar

      implicit none

      include 'include/egs5_h.f'                ! Main EGS "header" file

      include 'include/egs5_epcont.f'    ! COMMONs required by EGS5 code
      include 'include/egs5_stack.f'

      real*8 zplane, elum
      common /lumon/ zplane(205), elum(50)

      real*8 deltaz                      ! Local variables

      if (ir(np).eq.1.or.ir(np).eq.3) then
        idisc = 1
        return
      end if


      if (w(np).gt.0.0) then
        deltaz=(zplane(ir(np))-z(np))/w(np)
        if(deltaz.le.0.0) then
           ustep = 0.0
           irnew=ir(np) + 1
        end if
        if(deltaz.gt.0.0.and.deltaz.le.ustep) then
           ustep=deltaz
           irnew=ir(np) + 1
        end if
      end if

      if (w(np).lt.0.0) then
        deltaz=(zplane(ir(np)-1)-z(np))/w(np)
        if(deltaz.le.0.0) then
           ustep = 0.0
           irnew=ir(np) - 1
        end if
        if(deltaz.gt.0.0.and.deltaz.le.ustep) then
           ustep=deltaz
           irnew=ir(np) - 1
        end if
      end if

      return
      end
!--------------------------last line of howfar.f------------------------
!-------------------------------bndini----------------------------------
! Moller scattering off bound atomic electrons (moller_v5).
!
! EGS5 samples Moller scattering off free electrons at rest.  In the
! target the struck electron is bound in shell i (binding energy B_i)
! and moves with momentum p distributed according to the Hartree-Fock
! shell-wise Compton profile J_i(p_z) (Biggs, Mendelsohn and Mann,
! At. Data Nucl. Data Tables 16 (1975) 201), shipped with EGS5 in
! data/shellwise_Compton_profile/zNNN.dat.  In the impulse
! approximation the struck electron has 4-momentum t = (m - B_i, p),
! the ion absorbs -p, and s = (b + t)^2 changes event by event: the CM
! boost, the opening angle and the pair mass are smeared (Levchuk
! effect, NIM A345 (1994) 496).  For E_beam >> m the rate per electron
! is independent of p (sigma ~ 1/s, flux ~ s), so shells are sampled by
! occupation and |p| from the isotropic radial density
! f(p) = -2 p dJ/dp, and the CM scattering angles are kept.  The tables
! end at 100 a.u. (373 keV/c); inner shells (W K shell: ~15% of the
! electrons) extend further, so each profile is continued with a
! hydrogen-like tail (1 + q^2/p0^2)^-3 matched to the last two points.
!
! bound.dat (optional, one line):  ibound  iz  ekmin  ndebug
!   ibound  1 = on, 0 = off (no random numbers used; identical to v4)
!   iz      atomic number of the target (profile file zNNN.dat)
!   ekmin   minimum kinetic energy [MeV] of the Moller secondary for the
!           correction; softer delta rays are left to the free model,
!           where the impulse approximation does not apply
!   ndebug  number of corrected interactions written to bound_debug.txt
! The last sampled shell and struck-electron 4-momentum are kept in
! bslast / btlast (bslast = 0 if the last call made no correction).
! Without bound.dat: on, iz = 74, ekmin = 1 MeV, ndebug = 0.
!-----------------------------------------------------------------------
      subroutine bndini

      implicit none

      include 'include/egs5_h.f'
      include 'include/egs5_useful.f'

      integer NFINE, MSH
      parameter (NFINE = 8001, MSH = 200)
      integer ibound, nshb, ndebug, nbnd, nbskp, nbsoft, bslast
      real*8 ekbmin, bocc, bbind, bpgrid, bcdf, bpau, btlast
      common /bndcom/ bocc(MSH), bbind(MSH), bpgrid(NFINE),
     *                bcdf(NFINE,MSH), ekbmin, bpau, btlast(4),
     *                ibound, nshb, ndebug, nbnd, nbskp, nbsoft, bslast

      integer mxraw, mxshel, nshell(MSH)
      double precision elecni(MSH), capin(MSH), scprof(31,MSH),
     *                 qcap(31)
      namelist /SCPRDT/ MXRAW,MXSHEL,ELECNI,NSHELL,CAPIN,SCPROF,QCAP

      integer iz, i, k, j, ios
      real*8 xj(NFINE), f(NFINE), tot, ql, qh, frac, pmax, slope,
     *       p0sq, qe
      logical there
      character*64 fname

      ibound = 1
      iz = 74
      ekbmin = 1.0d0
      ndebug = 0
      nbnd = 0
      nbskp = 0
      nbsoft = 0
      bslast = 0

      inquire(file='bound.dat', exist=there)
      if (there) then
        open(54, file='bound.dat', status='old')
        read(54, *, iostat=ios) ibound, iz, ekbmin, ndebug
        if (ios .ne. 0) stop 'bndini: cannot parse bound.dat'
        close(54)
      end if
      write(6,100) ibound, iz, ekbmin, ndebug
100   format(/,' Bound-electron Moller: ibound=',i2,' Z=',i4,
     *       ' ekmin=',f8.3,' MeV ndebug=',i8)
      if (ibound .eq. 0) return

      write(fname,'(a,i3.3,a)') 'data/shellwise_Compton_profile/z',
     *                          iz, '.dat'
      open(53, file=fname, status='old', iostat=ios)
      if (ios .ne. 0) stop 'bndini: cannot open Compton profile file'
      read(53, SCPRDT, iostat=ios)
      if (ios .ne. 0) stop 'bndini: cannot read SCPRDT namelist'
      close(53)
      nshb = mxshel

!     1 atomic unit of momentum = alpha m c, in MeV/c
      bpau = RM / 137.035999d0

!     |p| grid [a.u.], quadratic spacing: fine near 0 for the outer
!     shells, out to 4x the table end for the inner-shell tails
      qe = qcap(31)
      pmax = 4.0d0 * qe
      do k = 1, NFINE
        bpgrid(k) = pmax * (dble(k - 1) / dble(NFINE - 1))**2
      end do

      do i = 1, nshb
        bocc(i) = elecni(i)
        bbind(i) = capin(i)
!       J_i on the grid: log-linear interpolation of the table; beyond
!       the table a hydrogen-like tail J ~ (1 + q^2/p0^2)^-3, matched
!       in value and log-slope to the last two tabulated points
        slope = 0.0d0
        if (scprof(30,i) .gt. 0.0d0 .and. scprof(31,i) .gt. 0.0d0)
     *    slope = log(scprof(31,i) / scprof(30,i)) /
     *            log(qcap(31) / qcap(30))
        if (slope .lt. -0.1d0 .and. slope .gt. -6.0d0) then
          p0sq = qe**2 * (-6.0d0 / slope - 1.0d0)
        else
          p0sq = 0.0d0
        end if
        j = 1
        do k = 1, NFINE
          if (bpgrid(k) .le. qe) then
            do while (j .lt. 30 .and. bpgrid(k) .gt. qcap(j+1))
              j = j + 1
            end do
            ql = log(max(scprof(j,i), 1.0d-30))
            qh = log(max(scprof(j+1,i), 1.0d-30))
            frac = (bpgrid(k) - qcap(j)) / (qcap(j+1) - qcap(j))
            xj(k) = exp(ql + frac * (qh - ql))
          else if (p0sq .gt. 0.0d0) then
            xj(k) = scprof(31,i) * ((1.0d0 + qe**2 / p0sq) /
     *              (1.0d0 + bpgrid(k)**2 / p0sq))**3
          else
            xj(k) = 0.0d0
          end if
        end do
!       radial density f(p) = -2 p dJ/dp (isotropic shell), >= 0
        f(1) = 0.0d0
        do k = 2, NFINE - 1
          f(k) = -2.0d0 * bpgrid(k) * (xj(k+1) - xj(k-1)) /
     *           (bpgrid(k+1) - bpgrid(k-1))
          if (f(k) .lt. 0.0d0) f(k) = 0.0d0
        end do
        f(NFINE) = 0.0d0
        bcdf(1,i) = 0.0d0
        do k = 2, NFINE
          bcdf(k,i) = bcdf(k-1,i) + 0.5d0 * (f(k) + f(k-1)) *
     *                (bpgrid(k) - bpgrid(k-1))
        end do
        tot = bcdf(NFINE,i)
        if (tot .le. 0.0d0) stop 'bndini: empty Compton profile'
        do k = 1, NFINE
          bcdf(k,i) = bcdf(k,i) / tot
        end do
      end do

!     occupation numbers -> cumulative shell probabilities
      tot = 0.0d0
      do i = 1, nshb
        tot = tot + bocc(i)
      end do
      write(6,110) nshb, tot
110   format(' Bound-electron Moller: ',i3,' shells, ',f6.1,
     *       ' electrons')
      do i = 2, nshb
        bocc(i) = bocc(i) + bocc(i-1)
      end do
      do i = 1, nshb
        bocc(i) = bocc(i) / tot
      end do

      if (ndebug .gt. 0) open(55, file='bound_debug.txt',
     *                        status='unknown')
      return
      end
!-------------------------------bndmol----------------------------------
! Called from ausgab(9), right after EGS5's free Moller interaction:
! np-1 holds the scattered primary, np the secondary electron.
!  1. The free collision fixes the incoming electron b = p1 + p2 - (m,0)
!     (EGS5 Moller kinematics conserve 4-momentum exactly).
!  2. Boost to the free CM frame; record (theta*, phi*) of electron 1
!     about the beam direction.
!  3. Sample shell i and the struck electron t = (m - B_i, p).
!  4. New CM frame of b + t; same (theta*, phi*), back-to-back pair
!     with p* = sqrt(s'/4 - m^2).
!  5. Boost to the lab and overwrite the stack.  EGS5 then transports
!     both electrons through the rest of the target as usual.
!-----------------------------------------------------------------------
      subroutine bndmol

      implicit none

      include 'include/egs5_h.f'
      include 'include/egs5_stack.f'
      include 'include/egs5_useful.f'

      integer NFINE, MSH
      parameter (NFINE = 8001, MSH = 200)
      integer ibound, nshb, ndebug, nbnd, nbskp, nbsoft, bslast
      real*8 ekbmin, bocc, bbind, bpgrid, bcdf, bpau, btlast
      common /bndcom/ bocc(MSH), bbind(MSH), bpgrid(NFINE),
     *                bcdf(NFINE,MSH), ekbmin, bpau, btlast(4),
     *                ibound, nshb, ndebug, nbnd, nbskp, nbsoft, bslast

      real*8 rn, pm1, pm2, p1(4), p2(4), b(4), t(4), bk(4), q(4)
      real*8 beta(3), cth, sth, phi, ct, st, pt, e3(3), e1(3), e2(3)
      real*8 n1(3), sp, ecm, pcm, bb, twopi
      parameter (twopi = 6.283185307179586d0)
      integer i, k, lo, hi, mid, ish

      bslast = 0
      if (ibound .eq. 0) return
      if (iq(np) .ne. -1 .or. iq(np-1) .ne. -1) return
      if (e(np) - RM .lt. ekbmin) then
        nbsoft = nbsoft + 1
        return
      end if

!     1. free collision (4-vectors stored as px, py, pz, E)
      pm1 = sqrt(max(e(np-1)**2 - RM**2, 0.0d0))
      pm2 = sqrt(max(e(np)**2 - RM**2, 0.0d0))
      p1(1) = pm1 * u(np-1)
      p1(2) = pm1 * v(np-1)
      p1(3) = pm1 * w(np-1)
      p1(4) = e(np-1)
      p2(1) = pm2 * u(np)
      p2(2) = pm2 * v(np)
      p2(3) = pm2 * w(np)
      p2(4) = e(np)
      do k = 1, 3
        b(k) = p1(k) + p2(k)
      end do
      b(4) = p1(4) + p2(4) - RM

!     2. free CM frame: (theta*, phi*) of electron 1 about the beam
      do k = 1, 3
        beta(k) = (p1(k) + p2(k)) / (p1(4) + p2(4))
      end do
      call bndbst(p1, beta, q)
      call bndbst(b, beta, bk)
      call bndfrm(bk, e1, e2, e3)
      sp = sqrt(q(1)**2 + q(2)**2 + q(3)**2)
      cth = (q(1)*e3(1) + q(2)*e3(2) + q(3)*e3(3)) / sp
      phi = atan2(q(1)*e2(1) + q(2)*e2(2) + q(3)*e2(3),
     *            q(1)*e1(1) + q(2)*e1(2) + q(3)*e1(3))
      sth = sqrt(max(1.0d0 - cth**2, 0.0d0))

!     3. struck electron: shell by occupation, |p| by inverse CDF of
!        -2 p dJ/dp, isotropic direction
      call randomset(rn)
      ish = 1
      do while (ish .lt. nshb .and. rn .gt. bocc(ish))
        ish = ish + 1
      end do
      call randomset(rn)
      lo = 1
      hi = NFINE
      do while (hi - lo .gt. 1)
        mid = (lo + hi) / 2
        if (bcdf(mid,ish) .lt. rn) then
          lo = mid
        else
          hi = mid
        end if
      end do
      bb = bcdf(hi,ish) - bcdf(lo,ish)
      if (bb .gt. 0.0d0) then
        pt = bpgrid(lo) + (rn - bcdf(lo,ish)) / bb *
     *       (bpgrid(hi) - bpgrid(lo))
      else
        pt = bpgrid(lo)
      end if
      pt = pt * bpau
      call randomset(rn)
      ct = 2.0d0 * rn - 1.0d0
      st = sqrt(max(1.0d0 - ct**2, 0.0d0))
      call randomset(rn)
      t(1) = pt * st * cos(twopi * rn)
      t(2) = pt * st * sin(twopi * rn)
      t(3) = pt * ct
      t(4) = RM - bbind(ish)

!     4. new CM frame of b + t
      sp = (b(4) + t(4))**2 - (b(1) + t(1))**2 - (b(2) + t(2))**2
     *     - (b(3) + t(3))**2
      if (sp .le. 4.0d0 * RM**2) then
        nbskp = nbskp + 1
        return
      end if
      ecm = 0.5d0 * sqrt(sp)
      pcm = sqrt(ecm**2 - RM**2)
      do k = 1, 3
        beta(k) = (b(k) + t(k)) / (b(4) + t(4))
      end do
      call bndbst(b, beta, bk)
      call bndfrm(bk, e1, e2, e3)
      do k = 1, 3
        n1(k) = sth * (cos(phi) * e1(k) + sin(phi) * e2(k))
     *          + cth * e3(k)
      end do

      if (ndebug .gt. 0 .and. nbnd .lt. ndebug) then
        write(55,200) ish, bbind(ish), (t(k), k=1,3),
     *     e(np-1), u(np-1), v(np-1), w(np-1),
     *     e(np), u(np), v(np), w(np)
      end if

!     5. back to the lab; overwrite the stack
      do k = 1, 3
        q(k) = pcm * n1(k)
        beta(k) = -beta(k)
      end do
      q(4) = ecm
      call bndbst(q, beta, p1)
      do k = 1, 3
        q(k) = -pcm * n1(k)
      end do
      call bndbst(q, beta, p2)

      pm1 = sqrt(p1(1)**2 + p1(2)**2 + p1(3)**2)
      pm2 = sqrt(p2(1)**2 + p2(2)**2 + p2(3)**2)
      e(np-1) = p1(4)
      u(np-1) = p1(1) / pm1
      v(np-1) = p1(2) / pm1
      w(np-1) = p1(3) / pm1
      e(np) = p2(4)
      u(np) = p2(1) / pm2
      v(np) = p2(2) / pm2
      w(np) = p2(3) / pm2
      nbnd = nbnd + 1
      bslast = ish
      do k = 1, 4
        btlast(k) = t(k)
      end do

      if (ndebug .gt. 0 .and. nbnd .le. ndebug) then
        write(55,210) e(np-1), u(np-1), v(np-1), w(np-1),
     *                e(np), u(np), v(np), w(np)
      end if
200   format('B ',i3,1x,1p,12e24.16)
210   format('A ',1p,8e24.16)
      return
      end
!-------------------------------bndbst----------------------------------
! Lorentz boost of a (px, py, pz, E) 4-vector into the frame moving
! with velocity beta.
!-----------------------------------------------------------------------
      subroutine bndbst(a, beta, c)

      implicit none

      real*8 a(4), beta(3), c(4), b2, gam, bp, fac
      integer k

      b2 = beta(1)**2 + beta(2)**2 + beta(3)**2
      if (b2 .le. 0.0d0) then
        do k = 1, 4
          c(k) = a(k)
        end do
        return
      end if
      gam = 1.0d0 / sqrt(1.0d0 - b2)
      bp = beta(1)*a(1) + beta(2)*a(2) + beta(3)*a(3)
      fac = (gam - 1.0d0) * bp / b2 - gam * a(4)
      do k = 1, 3
        c(k) = a(k) + fac * beta(k)
      end do
      c(4) = gam * (a(4) - bp)
      return
      end
!-------------------------------bndfrm----------------------------------
! Right-handed orthonormal frame (e1, e2, e3) with e3 along the spatial
! part of a; e1 is built from the lab axis least aligned with e3, so
! the azimuth is defined the same way in every frame.
!-----------------------------------------------------------------------
      subroutine bndfrm(a, e1, e2, e3)

      implicit none

      real*8 a(4), e1(3), e2(3), e3(3), r(3), am, d
      integer k

      am = sqrt(a(1)**2 + a(2)**2 + a(3)**2)
      do k = 1, 3
        e3(k) = a(k) / am
        r(k) = 0.0d0
      end do
      if (abs(e3(1)) .le. abs(e3(2)) .and. abs(e3(1)) .le. abs(e3(3)))
     *  then
        r(1) = 1.0d0
      else if (abs(e3(2)) .le. abs(e3(3))) then
        r(2) = 1.0d0
      else
        r(3) = 1.0d0
      end if
      d = r(1)*e3(1) + r(2)*e3(2) + r(3)*e3(3)
      do k = 1, 3
        e1(k) = r(k) - d * e3(k)
      end do
      am = sqrt(e1(1)**2 + e1(2)**2 + e1(3)**2)
      do k = 1, 3
        e1(k) = e1(k) / am
      end do
      e2(1) = e3(2)*e1(3) - e3(3)*e1(2)
      e2(2) = e3(3)*e1(1) - e3(1)*e1(3)
      e2(3) = e3(1)*e1(2) - e3(2)*e1(1)
      return
      end
!--------------------------last line of bndfrm.f------------------------
!-------------------------------bndsum----------------------------------
! Summary of the bound-electron correction, written to egs5job.out.
!-----------------------------------------------------------------------
      subroutine bndsum

      implicit none

      integer NFINE, MSH
      parameter (NFINE = 8001, MSH = 200)
      integer ibound, nshb, ndebug, nbnd, nbskp, nbsoft, bslast
      real*8 ekbmin, bocc, bbind, bpgrid, bcdf, bpau, btlast
      common /bndcom/ bocc(MSH), bbind(MSH), bpgrid(NFINE),
     *                bcdf(NFINE,MSH), ekbmin, bpau, btlast(4),
     *                ibound, nshb, ndebug, nbnd, nbskp, nbsoft, bslast

      write(6,100) ibound, nbnd, nbsoft, nbskp
100   format(/,' Bound-electron Moller: ibound=',i2,/,
     *  '   corrected interactions          =',i12,/,
     *  '   left free (secondary < ekmin)   =',i12,/,
     *  '   left free (s < 4 m^2)           =',i12)
      if (ndebug .gt. 0) close(55)
      return
      end
