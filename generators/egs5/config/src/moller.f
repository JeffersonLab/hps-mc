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

      real*8 rndm, dx, dy, sigma/0.01/

      integer npart, moller, ngam, iadd(10), itrig
      real qpart, ppart, vtxpart, am
      common /tmpart/ npart,moller,qpart(10),ppart(4,10),vtxpart(3,10)

      real*8 zplane, elum, elumtot, elum30, elum35, elum40, elum45
      common /lumon/ zplane(205), elum(50)
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
      real*8 w/0.0004375d0/
c 0.001 X0 
c      real*8 w/0.00035d0/
c 0.00075 X0 
c      real*8 w/0.0002625d0/
c 0.0005 X0 
c      real*8 w/0.000175d0/
c 0.00025 X0 
c      real*8 w/0.0000875d0/

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
c need long integer to generate 10^9
      integer*8 ncases

c ==================

!     ----------
!     Open files
!     ----------
      open(UNIT= 6,FILE='egs5job.out',STATUS='unknown')

      open(51,file='moller.dat',form='FORMATTED')
      open(52,file='seed.dat',form='FORMATTED')

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
      inseed=1030701

      read(52,710) inseed
 710  format(5x,i5)
      write(6,720) inseed
 720  format(2x,'seed=',i10)

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
c      ncases=1000000000
      ncases=100000000
      idinc=-1
cx      ei=11000.0D0
cx      ei=6600.0D0
cx      ei=3300.0D0
      ei=1056.0D0
cx      ei=1100.0D0
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

      do i=1,ncases

         npart = 0

         moller = 0

        call shower(iqi,ei,xi,yi,zi,ui,vi,wi,iri,wti)

        if(moller.eq.1.and.npart.ge.2) then
           write(51,770) npart,
     -                   (qpart(j), (ppart(k,j),k=1,4),j=1,npart) 
 770       format(i10,/,(2x,f3.0,4f11.6))
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
      close(UNIT=6)

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

      integer i, iarg, n, nphot 

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

      if(ir(np).eq.3.and.e(np).gt.10.0.and.abs(v(np)).gt.0.010) then
         npart = npart + 1
         qpart(npart) = iq(np)
         p = e(np)
         if(iq(np).ne.0) p = sqrt(e(np)**2-am**2)
         ppart(1,npart) = p * u(np) * 0.001
         ppart(2,npart) = p * v(np) * 0.001
         ppart(3,npart) = p * w(np) * 0.001
         ppart(4,npart) = e(np) * 0.001
         vtxpart(1,npart) = x(np)
         vtxpart(2,npart) = y(np)
         vtxpart(3,npart) = 0.0
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
