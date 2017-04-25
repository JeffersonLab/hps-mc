!***********************************************************************
!
!                          **************
!                          *            *
!                          * ucsampl5.f *
!                          *            *
!                          **************
!
! A complete example of a EGS5 user code, using a simple plane 
! geometry.  For SLAC-R-730/KEK Report 2005-8.
!
! This user code corresponds to ucsampl4.mor for egs4.
! The following shows the geometry
!***********************************************************************
!                                                                      *
!             -------------------------------------------------        *
!             1-Dimensional Plane Z Geometry (ucsampl5 example)        *
!             -------------------------------------------------        *
!                                                                      *
!                Y (X into page)                                       *
!                ^                                                     *
!                |                                                     *
!                |     |                                               *
!                | Fe  |   Air                                         *
!                |     |                                               *
!                |     |                                               *
!     1 GeV      |     |                                               *
!      ==========>+----+------------------------> Z                    *
!     electron   0    3.0                                              *
!                                                                      *
!***********************************************************************
!23456789|123456789|123456789|123456789|123456789|123456789|123456789|12
!-----------------------------------------------------------------------
!------------------------------- main code -----------------------------
!-----------------------------------------------------------------------

!-----------------------------------------------------------------------
!     Step 1. Initialization
!-----------------------------------------------------------------------

      implicit none

!     ------------
!     EGS5 COMMONs
!     ------------
      include 'include/egs5_h.f'                ! Main EGS "header" file

      include 'include/egs5_bounds.f'
      include 'include/egs5_edge.f'
      include 'include/egs5_media.f'
      include 'include/egs5_misc.f'
      include 'include/egs5_thresh.f'
      include 'include/egs5_useful.f'
      include 'include/egs5_usersc.f'
      include 'include/egs5_userxt.f'
      include 'include/randomm.f'

!     ----------------------
!     Auxiliary-code COMMONs
!     ----------------------
      include 'auxcommons/lines.f'

      common/passit/zthick
      real*8 zthick
      
      common/totals/esum(3)
      real*8 esum

      real*8 ei,ekin,etot,totke,xi,yi,zi,   ! Arguments
     *       ui,vi,wi,wti
      real tarray(2)

      real t0,t1,timecpu,tt              ! Local variables
      real etime
      integer i,idinc,iqi,iri,j,ncases
      character*24 medarr(2)

!     ----------
!     Open files
!     ----------
      open(UNIT= 6,FILE='egs5job.out',STATUS='unknown')

!     ====================
      call counters_out(0)
!     ====================

!-----------------------------------------------------------------------
! Step 2: pegs5-call
!-----------------------------------------------------------------------
!     ==============
      call block_set                 ! Initialize some general variables
!     ==============

!     ---------------------------------
!     define media before calling PEGS5
!     ---------------------------------

      nmed=2
      medarr(1)='FE-RAYLEIGH             '
      medarr(2)='AIR AT NTP              '

      do j=1,nmed
        do i=1,24
          media(i,j)=medarr(j)(i:i)
        end do
      end do  

      chard(1) = 3.0
      chard(2) = 3.0
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

      med(1)=0
      med(2)=1 
      med(3)=2

!     ----------------------------------
!     Set of option flag for region 2-3
!     1: on, 0: off
!     ----------------------------------
      nreg=3

      do i=2,nreg
        ecut(i)=100.0       ! egs cut off energy for electrons
        pcut(i)=100.0       ! egs cut off energy for photons
        iphter(i) = 0       ! Switches for PE-angle sampling
        iedgfl(i) = 0       ! K & L-edge fluorescence
        iauger(i) = 0       ! K & L-Auger
        iraylr(i) = 0       ! Rayleigh scattering
        lpolar(i) = 0       ! Linearly-polarized photon scattering
        incohr(i) = 0       ! S/Z rejection
        iprofr(i) = 0       ! Doppler broadening
        impacr(i) = 0       ! Electron impact ionization
      end do

!     --------------------------------------------------------
!     Random number seeds.  Must be defined before call hatch.
!     ins (1- 2^31)
!     --------------------------------------------------------
      inseed=1
      luxlev=1

!     =============
      call rluxinit   ! Initialize the Ranlux random-number generator
!     =============

!-----------------------------------------------------------------------
! Step 4:  Determination-of-incident-particle-parameters
!-----------------------------------------------------------------------
      iqi=-1
      xi=0.0
      yi=0.0
      zi=0.0
      ui=0.0
      vi=0.0
      wi=1.0
      iri=2
      wti=1.0
      ncases=1000
      idinc=-1
      ei=1000.D0
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
      zthick=3.0
!     plate is 3 cm thick

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

        if (nlines.lt.nwrite) then
        write(6,200) i,ei,zi,wi,iqi,iri,idinc
200     format(i2,3G15.7,3I5)
        nlines=nlines+1
        end if

        call shower(iqi,ei,xi,yi,zi,ui,vi,wi,iri,wti)

      end do

      tt=etime(tarray)
      t1=tarray(1)

      timecpu=t1-t0
      write(6,210) timecpu
210   format(/,' Elapsed Time (sec)=',1PE12.5)

!-----------------------------------------------------------------------
! Step 9:  Output-of-results
!-----------------------------------------------------------------------
      totke=ncases*ekin
      write(6,220) ei,zthick,ncases
220   format(//,' Incident total energy of electron=',F12.1,' MeV',/, '
     *Iron slab thickness=',F6.3,' cm',/, ' Number of cases in run=',I7,
     *//,' Energy deposition summary:',/)

      etot=0.D0
      do i=1,nreg
        etot=etot+esum(i)
        esum(i)=esum(i)/totke 
        write(6,230) i, esum(i)
230     format(' Fraction in region ',I3,'=',F10.7)
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

      common/totals/esum(3)
      real*8 esum

      integer iarg                                           ! Arguments

!     ----------------------
!     Add deposition energy
!     ----------------------

      esum(ir(np))=esum(ir(np)) + edep

!     ----------------------------------------------------------------
!     Print out stack information (for limited number cases and lines)
!     ----------------------------------------------------------------

      if (nlines.lt.nwrite) then
        write(6,1240) e(np),z(np),w(np),iq(np),ir(np),iarg
1240    FORMAT(3G15.7,3I5)
        nlines=nlines+1
      end if
      
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

      common/passit/zthick
      real*8 zthick

      real*8 deltaz                      ! Local variables
      integer irnxt

      if (ir(np).ne.2) then
        idisc = 1
        return
      end if

      dnear(np) = dmin1(z(np),zthick-z(np))

!-----------------------------------
! Particle going parallel to planes 
!-----------------------------------
      if(w(np).eq.0) return

!-------------------------------------------------------- 
! Check forward plane first since shower heading that way 
! most of the time
!--------------------------------------------------------
      if (w(np).gt.0.0) then
        deltaz=(zthick-z(np))/w(np)
        irnxt=3
!-----------------------------------------------------------
! Otherwise, particle must be heading in backward direction.
!-----------------------------------------------------------
      else
        deltaz=-z(np)/w(np)
        irnxt=1
      end if

      if (deltaz.le.ustep) then
        ustep=deltaz
        irnew=irnxt
      end if

      return
      end
!--------------------------last line of howfar.f------------------------
