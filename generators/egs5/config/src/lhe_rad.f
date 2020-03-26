!***********************************************************************
! Save events if
!  1) gamma or e+/e-
!  2) at least two particles with E > 1 GeV
!  3) Theta_x > 2mrad
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

      include 'include/egs5_stack.f'
!     ----------------------
!     Auxiliary-code COMMONs
!     ----------------------
      include 'auxcommons/lines.f'

      include 'src/stdhep.inc'

      real*8 rndm, dx, dy, sigma/0.01/

      integer npart, ngam, iadd(10), itrig
      real qpart, ppart, am
      common /tmpart/ npart, qpart(100), ppart(4,100)

      real*8 zplane, elum, elumtot, elum30, elum35, elum40, elum45
      common /lumon/ zplane(205), elum(50)
      
      real*8 w8

c need long integer to generate 10^9
      integer*8 ncases
      real*8 ebeam
      common /myrun/ ebeam,ncases

      real*8 esum, elyr
      common/totals/esum(205), elyr(20,30)

      integer lyr
      real*8 etemp(4)/4*0.0d0/

      real*8 ei,ekin,etot,totke,xi,yi,zi,   ! Arguments
     *       ui,vi,wi,wti,p
      real tarray(2), emax, irmax, e1, e2, e3, d

      real t0,t1,timecpu,tt              ! Local variables
      real etime
      integer i,j,k,idinc,iqi,iri,ne
      character*24 medarr(4)

      character*80 cline

      character*6 evhead / '<event' /
      integer nup,idup,istup,mothup(2),icolup(2)
      real*8 pup(5)

      integer istream,lok

c ==================

!     ----------
!     Open files
!     ----------
      open(UNIT= 6,FILE='egs5job.out',STATUS='unknown')
      open(UNIT= 4,FILE='egs5job.inp',STATUS='old')


      open(51,file='brems.dat',form='FORMATTED')
      open(52,file='seed.dat',form='FORMATTED')

      call stdxwopen('brems.stdhep', ' ascii events ', 
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
        ecut(i)=0.521       ! egs cut off energy for electrons
        pcut(i)=0.001      ! egs cut off energy for photons
        iphter(i) = 0       ! Switches for PE-angle sampling
        iedgfl(i) = 1       ! K & L-edge fluorescence
        iauger(i) = 0       ! K & L-Auger
        iraylr(i) = 1       ! Rayleigh scattering
        lpolar(i) = 0       ! Linearly-polarized photon scattering
        incohr(i) = 0       ! S/Z rejection
        iprofr(i) = 0       ! Doppler broadening
        impacr(i) = 1       ! Electron impact ionization
      end do

!     --------------------------------------------------------
!     Random number seeds.  Must be defined before call hatch.
!     ins (1- 2^31)
!     --------------------------------------------------------
c      inseed=1030701
      read(52,*) inseed,w8,ebeam,ncases
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
c Brems flag
c before electr
c      iausfl(7) = 1
c after electr
c      iausfl(8) = 1

c pair
c      iausfl(16) = 1
c      iausfl(17) = 1
c      iausfl(18) = 1

!-----------------------------------------------------------------------
! Step 5:   hatch-call
!-----------------------------------------------------------------------
! Total energy of incident source particle must be defined before hatch
! Define posible maximum total energy of electron before hatch
      if (iqi.ne.0) then
        emaxe = ebeam              ! charged particle
      else
        emaxe = ebeam + RM         ! photon
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
      zplane(2) = zplane(1) + w8

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
      nevhep=0

c     read lines looking for the event tag
 17   read(4,'(A)',end=999) cline
      if (cline(1:6).eq.evhead) goto 34
      goto 17
 34   continue

c     reset stdhep, place vertex
      nhep = 0
      nevhep = nevhep+1
      xi=0.0
      yi=0.0
      call randomset(zi)
      zi=w8*zi

      read(4,*,end=999) nup
c     loop through particles
 68   if (nup.eq.0) goto 51
      read(4,*,end=999) idup,istup,mothup,icolup,pup
      nup=nup-1

c     reject non-final particles, non-EGS particles
      if (istup.ne.1) goto 68
      if (idup.eq.11) then
              iqi=-1
              latchi = 1
      else if (idup.eq.-11) then
              iqi=1
              latchi = 1
      else if (idup.eq.13) then
              iqi=-1
              latchi = 0
      else
              goto 68
      end if
      p = sqrt(pup(4)**2-pup(5)**2)
      ei=pup(4)*1000.
c      print *,ei
      ui=pup(1)/p
      vi=pup(2)/p
      wi=pup(3)/p
      iri=2
      wti=1.0
      ekin=ei+iqi*RM
      call shower(iqi,ei,xi,yi,zi,ui,vi,wi,iri,wti)
      goto 68

 51   continue
c     write this event to stdhep
      if(nhep.eq.0) then
          nhep = 1
          idhep(1) = 12
          phep(1,1) = 0.
          phep(2,1) = 0.
          phep(3,1) = 0.1
          phep(4,1) = 0.1
          phep(5,1) = 0.
          vhep(1,1) = 0.
          vhep(2,1) = 0.
          vhep(3,1) = zplane(2) * 10.
          vhep(4,1) = 0.
          isthep(1) = 1
          jmohep(1,1) = 0
          jmohep(2,1) = 0
          jdahep(1,1) = 0
          jdahep(2,1) = 0
      end if

      call stdxwrt(  1, istream, lok)

      goto 17

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

      integer npart
      real qpart, ppart
      common /tmpart/ npart, qpart(100), ppart(4,100)

      integer*8 ncases
      real*8 ebeam
      common /myrun/ ebeam,ncases

      integer lyr, ira, nin/0/
      real dr/0.25/, am/0.511/, p

      integer i, iarg, n, nphot, iout 

!     ----------------------
!     Add deposition energy
!     ----------------------

      if(iarg.ne.0.and.iarg.ne.3) return

      esum(ir(np))=esum(ir(np)) + edep

      nin = nin + 1
c
cx      if(ir(np).eq.3.and.iq(np).eq.0.and.e(np).gt.20.0) then
cx         write(52,700) e(np),u(np),v(np),w(np)
cx 700     format(1x,4e14.6)
cx      end if

      if(ir(np).eq.3) then
         iout = 1
c     no cuts on tridents
c         if(abs(v(np)).lt.0.010) iout = 0
c         if(iq(np).ne.0.and.e(np).lt.ebeam*0.005) iout = 0
         if(iout.eq.1) then
c            npart = npart + 1
c            qpart(npart) = iq(np)
c            p = e(np)
c            if(iq(np).ne.0) p = sqrt(e(np)**2-am**2)
c            ppart(1,npart) = p * u(np) * 0.001
c            ppart(2,npart) = p * v(np) * 0.001
c            ppart(3,npart) = p * w(np) * 0.001
c            ppart(4,npart) = e(np) * 0.001

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
            jmohep(1,nhep) = latch(np)
            jmohep(2,nhep) = 0
            jdahep(1,nhep) = 0
            jdahep(2,nhep) = 0
         end if
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
