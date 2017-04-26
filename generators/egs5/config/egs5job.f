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
      real*8 ebeam
      common /myrun/ ebeam,ncases

      real*8 esum, elyr
      common/totals/esum(205), elyr(20,30)

      integer lyr
      real*8 etemp(4)/4*0.0d0/

      real*8 ei,ekin,etot,totke,xi,yi,zi,   ! Arguments
     *       ui,vi,wi,wti
      real tarray(2), emax, irmax, e1, e2, e3, d

      real t0,t1,timecpu,tt              ! Local variables
      real etime
      integer i,j,k,idinc,iqi,iri,ne
      character*24 medarr(4)


      integer istream,lok
      integer bunchsize

c ==================

!     ----------
!     Open files
!     ----------
      open(UNIT= 6,FILE='egs5job.out',STATUS='unknown')

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
      read(52,*) inseed,w,ebeam,bunchsize,ncases
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

        nhep = 0
        nevhep = i
        
        do ne=1, bunchsize
        
           call shower(iqi,ei,xi,yi,zi,ui,vi,wi,iri,wti)

        end do

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
         if(abs(v(np)).lt.0.010) iout = 0
         if(iq(np).ne.0.and.e(np).lt.ebeam*0.005) iout = 0
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
            jmohep(1,nhep) = 0
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
! ----------------------------------------------------------------------
! 
! Stanford University Notices 
! for SLAC Manual SLAC-R-730
! and its included software known as the 
! EGS5 Code System
! 
! ----------------------------------------------------------------------
! 
! Acknowledgement of sponsorship.  
! This manual and its contents, including software, were produced in 
! part by the Stanford Linear Accelerator Center (SLAC), Stanford 
! University, under Contract DE-AC02-76SFO0515 with the U.S. Department 
! of Energy.
! 
! Use.  
! The manual and its included software should be used for non-commercial
! purposes only.  Contact SLAC regarding commercial use.
! 
! Government disclaimer of liability.  
! Neither the United States nor the United States Department of Energy, 
! nor any of their employees, makes any warranty, express or implied, 
! or assumes any legal liability or responsibility for the accuracy, 
! completeness, or usefulness of any data, apparatus, product, or 
! process disclosed, or represents that its use would not infringe 
! privately owned rights.
! 
! Stanford disclaimer of liability.  
! Stanford University makes no representations or warranties, express 
! or implied, nor assumes any liability for the use of this manual or 
! its contents, including software.
! 
! Maintenance of notices.  
! In the interest of clarity regarding the origin and status of this 
! SLAC manual and its included software, this and all the preceding 
! Stanford University notices are to: (1) remain affixed to any copy 
! or derivative of this manual or its software made or distributed by 
! the recipient of this manual or its software; and (2) be affixed to 
! any copy of a document or any software made or distributed by the 
! recipient that contains a copy or derivative of this manual or its 
! software.
! 
! ----------------------------------------------------------------------
!
! From SLAC Software Notices, Set 3
! OTT.002a, 2004 FEB 03
! 
!------------------------------counters_out.f---------------------------
! Version: 051227-1600
!-----------------------------------------------------------------------
!23456789|123456789|123456789|123456789|123456789|123456789|123456789|12

      subroutine counters_out(ioflag)
      
      implicit none

      include 'include/counters.f'

      integer ioflag                                         ! Arguments

      if (ioflag.eq.0) then       ! Open unit=99 and initialize counters
        OPEN(UNIT=99,FILE='egs5job.out99',STATUS='UNKNOWN')
        iannih           = 0
        iaphi            = 0
        ibhabha          = 0
        ibrems           = 0 
        icollis          = 0
        icompt           = 0
        iedgbin          = 0
        ieii             = 0
        ielectr          = 0
        ihatch           = 0
        ihardx           = 0
        ikauger          = 0
        ikshell          = 0
        ikxray           = 0
        ilauger          = 0
        ilshell          = 0
        ilxray           = 0
        imoller          = 0
        imscat           = 0
        ipair            = 0
        iphoto           = 0
        iphoton          = 0
        iraylei          = 0
        ishower          = 0
        iuphi            = 0
        itmxs            = 0
        noscat           = 0
        iblock           = 0
      else if (ioflag.eq.1) then
        write(99,*) 'Values for subroutine-entry counters:'
        write(99,*)
        write(99,*) 'iannih =',iannih
        write(99,*) 'iaphi  =',iaphi
        write(99,*) 'ibhabha=',ibhabha
        write(99,*) 'ibrems =',ibrems
        write(99,*) 'icollis=',icollis
        write(99,*) 'icompt =',icompt
        write(99,*) 'iedgbin=',iedgbin
        write(99,*) 'ieii   =',ieii
        write(99,*) 'ielectr=',ielectr
        write(99,*) 'ihatch =',ihatch
        write(99,*) 'ihardx =',ihardx
        write(99,*) 'ikauger=',ikauger
        write(99,*) 'ikshell=',ikshell
        write(99,*) 'ikxray =',ikxray
        write(99,*) 'ilauger=',ilauger
        write(99,*) 'ilshell=',ilshell
        write(99,*) 'ilxray =',ilxray
        write(99,*) 'imoller=',imoller
        write(99,*) 'imscat =',imscat
        write(99,*) 'ipair  =',ipair
        write(99,*) 'iphoto =',iphoto
        write(99,*) 'iphoton=',iphoton
        write(99,*) 'iraylei=',iraylei
        write(99,*) 'ishower=',ishower
        write(99,*) 'iuphi  =',iuphi
        write(99,*) 'itmxs  =',itmxs
        write(99,*) 'noscat =',noscat
        write(99,*) 'iblock =',iblock
        CLOSE(UNIT=99)
      else
        write(6,*) ' *** Error using subroutine counters_out'
        write(6,*) '       ioflag=',ioflag
        CLOSE(UNIT=99)
        write(6,*) '     Program stopped'
        stop
      end if      

      return
      end
!----------------------last line of counters_out.f----------------------
!-----------------------------egs5_annih.f------------------------------
! Version: 051219-1435
! Reference: SLAC-R-730/KEK-2005-8
!-----------------------------------------------------------------------
!23456789|123456789|123456789|123456789|123456789|123456789|123456789|12

      subroutine annih
      
      implicit none

      include 'include/egs5_h.f'               ! Main EGS5 "header" file

      include 'include/egs5_stack.f'     ! COMMONs required by EGS5 code
      include 'include/egs5_uphiot.f'
      include 'include/egs5_useful.f'

      include 'include/counters.f'       ! Additional (non-EGS5) COMMONs

      real*8 rnnow                                           ! Arguments

      real*8                                           ! Local variables
     * avip,                     ! Available energy of incident positron
     * esg1,                              ! Energy of secondary gamma #1
     * esg2,                              ! Energy of secondary gamma #2
     * a,ep0,ep,g,t,p,pot,rejf

      iannih = iannih + 1                  ! Count entry into subroutine
 
      avip = e(np) + RM
      a    = avip/RM    
      g    = a - 1.0
      t    = g - 1.0
      p    = sqrt(a*t)  
      pot  = p/t        
      ep0  = 1.0/(a + p)  

1     continue                    ! Sample 1/ep from ep = ep0 to 1 - ep0
        call randomset(rnnow)
        ep = ep0*exp(rnnow*log((1.0 - ep0)/ep0))

                                       ! Decide whether or not to accept
        call randomset(rnnow)
        rejf = 1.0 - ep + (2.0*g - 1.0/ep)/a**2
        if (rnnow .gt. rejf) go to 1

                 ! This completes sampling of a distribution which is
                 ! asymmetric about ep = 1/2, but which when symmetrized
                 ! is the symmetric annihilation distribution.

                                  ! Set up energies, place them on stack
      ep        = max(ep,1.D0 - ep)          ! Pick ep in (1/2, 1 - ep0)
      esg1      = avip*ep                 ! Energy of secondary gamma #1
      e(np)     = esg1           ! Place energy of gamma #1 on the stack
      esg2      = avip - esg1             ! Energy of secondary gamma #2
      e(np + 1) = esg2           ! Place energy of gamma #2 on the stack
      iq(np)    = 0                          ! Make particle np a photon

                             ! Set up angles for the higher energy gamma
      costhe = (esg1 - RM)*pot/esg1
      costhe = min(1.D0,costhe)                      ! Fix (860724/dwor)
      sinthe = sqrt((1.0 - costhe)*(1.0 + costhe))
      call uphi(2,1)                             ! Set direction cosines

                                             ! Set up lower energy gamma
      np     = np + 1                          ! Increase the stack size
      iq(np) = 0                                      ! Make it a photon
      costhe = (esg2 - RM)*pot/esg2
      costhe = min(1.D0,costhe)                      ! Fix (860724/dwor)
      sinthe = - sqrt((1.0 - costhe)*(1.0 + costhe))
      call uphi(3,2)                             ! Set direction cosines
                                                      ! ----------------
      return                                          ! Return to ELECTR
                                                      ! ----------------
      end
!-----------------------last line of egs5_annih.f-----------------------
!------------------------------egs5_aphi.f------------------------------
! Version: 051219-1435
! Reference: SLAC-R-730/KEK-2005-8
!-----------------------------------------------------------------------
!23456789|123456789|123456789|123456789|123456789|123456789|123456789|12

      subroutine aphi(br)

      implicit none

      include 'include/egs5_h.f'               ! Main EGS5 "header" file

      include 'include/egs5_epcont.f'    ! COMMONs required by EGS5 code
      include 'include/egs5_stack.f'
      include 'include/egs5_uphiin.f'
      include 'include/egs5_uphiot.f'

      include 'include/counters.f'       ! Additional (non-EGS5) COMMONs

      real*8 br,rnnow                                        ! Arguments
      integer iarg

      real*8                                           ! Local variables
     * sinomg,cosomg,sindel,sinpsi,sinps2,comg,enorm,
     * cosdel,omg,cosph0,cph0,val,valloc,valmax,pnorm0,
     * sinph0,ph0,coseta,ceta,anormr,anorm2,sineta,eta,
     * ufa,vfa,wfa,ufb,vfb,wfb,asav,bsav,csav
      integer ldpola

      iaphi = iaphi + 1                    ! Count entry into subroutine

      iarg = 21
!                                =================
      if (iausfl(iarg+1) .ne. 0) call ausgab(iarg)
!                                =================

      pnorm0 = sqrt(u(np)*u(np) + v(np)*v(np) + w(np)*w(np))
      u(np) = u(np)/pnorm0
      v(np) = v(np)/pnorm0
      w(np) = w(np)/pnorm0
      valmax = br + 1./br

 1    continue
        call randomset(rnnow)
        ph0 = rnnow*twopi
        sinph0 = sin(ph0)
        cph0 = pi5d2 - ph0
        cosph0 = sin(cph0)
        valloc = sqrt(sinph0*sinph0 + cosph0*cosph0)
        sinph0 = sinph0/valloc
        cosph0 = cosph0/valloc
        val = (valmax - 2.*sinthe*sinthe*cosph0*cosph0)/valmax
        call randomset(rnnow)
        if (rnnow .le. val) go to 2
      go to 1

 2    continue
      anorm2 =costhe*costhe*cosph0*cosph0 + sinph0*sinph0
      call randomset(rnnow)
      if ((valmax - 2.)/(valmax - 2. + 2.*anorm2) .gt. rnnow .or.
     *     anorm2 .lt. 1.E-10) then
        ldpola = 1
      else
        ldpola = 0
      end if

      if (ldpola .eq. 1) then
        call randomset(rnnow)
        eta = rnnow*twopi
        sineta = sin(eta)
        ceta = pi5d2 - eta
        coseta = sin(ceta)
      else
        anormr = 1./sqrt(anorm2)
        sineta = -anormr*sinph0
        coseta = anormr*costhe*cosph0
      end if

      ufa = costhe*cosph0*coseta - sinph0*sineta
      vfa = costhe*sinph0*coseta + cosph0*sineta
      wfa = -sinthe*coseta

      asav = u(np)
      bsav = v(np)
      csav = w(np)

      sinps2 = asav*asav + bsav*bsav
      if (sinps2 .lt. 1.E-20) then
        cosomg = uf(np)
        sinomg = vf(np)
      else
        sinpsi = sqrt(sinps2)
        sindel = bsav/sinpsi
        cosdel = asav/sinpsi
        cosomg = cosdel*csav*uf(np) + sindel*csav*vf(np) - sinpsi*wf(np)
        sinomg = -sindel*uf(np) + cosdel*vf(np)
      end if

      enorm = sqrt(uf(np)*uf(np) + vf(np)*vf(np) + wf(np)*wf(np))
      if (enorm .lt. 1.E-4) then
        call randomset(rnnow)
        omg = rnnow*twopi
        sinomg = sin(omg)
        comg = pi5d2 - omg
        cosomg = sin(comg)
      end if

      cosphi = cosomg*cosph0 - sinomg*sinph0
      sinphi = sinomg*cosph0 + cosomg*sinph0

      ufb = cosomg*ufa - sinomg*vfa
      vfb = sinomg*ufa + cosomg*vfa
      wfb = wfa

      if (sinps2 .lt. 1.E-20) then
        uf(np) = ufb
        vf(np) = vfb
        wf(np) = wfb
      else
        uf(np) = cosdel*csav*ufb - sindel*vfb + asav*wfb
        vf(np) = sindel*csav*ufb + cosdel*vfb + bsav*wfb
        wf(np) = -sinpsi*ufb + csav*wfb
      end if
      enorm = sqrt(uf(np)*uf(np) + vf(np)*vf(np) + wf(np)*wf(np))
      uf(np) = uf(np)/enorm
      vf(np) = vf(np)/enorm
      wf(np) = wf(np)/enorm

      return

      end

!------------------------last line of egs5_aphi.f-----------------------
!-----------------------------egs5_bhabha.f-----------------------------
! Version: 051219-1435
! Reference: SLAC-R-730/KEK-2005-8
!-----------------------------------------------------------------------
!23456789|123456789|123456789|123456789|123456789|123456789|123456789|12

      subroutine bhabha
      
      implicit none

      include 'include/egs5_h.f'               ! Main EGS5 "header" file

      include 'include/egs5_stack.f'     ! COMMONs required by EGS5 code
      include 'include/egs5_thresh.f'
      include 'include/egs5_uphiot.f'
      include 'include/egs5_useful.f'

      include 'include/counters.f'       ! Additional (non-EGS5) COMMONs

      real*8 rnnow                                           ! Arguments

      real*8                                           ! Local variables
     * eip,                          ! Total energy of incident positron
     * ekin,                       ! Kinetic energy of incident positron
     * ekse2,                  ! Kinetic energy of secondary electron #2
     * ese1,                     ! Total energy of secondary electron #1
     * ese2,                     ! Total energy of secondary electron #2
     * t0,e0,yy,e02,betai2,ep0,ep0c,y2,yp,yp2,b4,b3,b2,b1,h1,
     * br,rejf,dcosth,re1,re2,remax,ttt

      ibhabha = ibhabha + 1                ! Count entry into subroutine
 
      eip   = e(np)
      ekin  = eip - RM      
      t0    = ekin/RM
      e0    = t0 + 1.0
      yy    = 1.0/(t0 + 2.0)
      e02   = e0*e0 
      betai2= e02/(e02-1.0)
      ep0   = te(medium)/ekin
      ep0c  = 1.0 - ep0
      y2    = yy*yy
      yp    = 1.0 - 2.0*yy
      yp2   = yp*yp
      b4    = yp2*yp
      b3    = b4 + yp2
      b2    = yp*(3.0 + y2)
      b1    = 2.0 - y2
      br    = ep0
      re1   = ep0c*(betai2-br*(b1-br*(b2-br*(b3-br*b4))))
      br    = 1.0
      re2   = ep0c*(betai2-br*(b1-br*(b2-br*(b3-br*b4))))
      remax = max(re1,re2)

                                            ! Sample/reject to obtain br
1     continue
        call randomset(rnnow)
        br = ep0/(1.0 - ep0c*rnnow)

                                       ! Decide whether or not to accept
        call randomset(rnnow)
        rejf = ep0c*(betai2-br*(b1-br*(b2-br*(b3-br*b4))))
        rejf = rejf/remax

        if (rnnow .gt.rejf) go to 1

                            ! If e- got more energy than e+, move the e+
                            ! pointer and reflect br (this puts e+ on
                            ! top of stack if it has less energy)
      if (br .lt. 0.5) then
        iq(np+1) = -1
        k1step(np+1) = 0.
        k1init(np+1) = 0.
        k1rsd(np+1) = 0.

      else
        iq(np) = -1
        iq(np+1) = 1
        br = 1.0 - br
        k1step(np+1) = k1step(np)
        k1init(np+1) = k1init(np)
        k1rsd(np+1) = k1rsd(np)
        k1step(np) = 0.
        k1init(np) = 0.
        k1rsd(np) = 0.
      end if

                                                  ! Divide up the energy
      br = max(br,0.D0)    ! Avoids possible negative energy (round off)
      ekse2 = br*ekin          ! Kinetic energy of secondary electron #2
      ese1 = eip - ekse2         ! Total energy of secondary electron #1
      ese2 = ekse2 + RM          ! Total energy of secondary electron #2
      e(np) = ese1
      e(np+1) = ese2
                                           ! Bhabha angles are uniquely 
                                           ! determined by kinematics
      h1 = (eip + RM)/ekin
      dcosth = h1*(ese1 - RM)/(ese1 + RM)
      ttt = 1.0 - dcosth
      if (ttt.le.0.0) then
        sinthe = 0.0
      else
        sinthe = sqrt(1.0 - dcosth)
      end if
      costhe = sqrt(dcosth)
      call uphi(2,1)                             ! Set direction cosines
      np = np + 1
      dcosth = h1*(ese2 - RM)/(ese2 + RM)
      ttt = 1.0 - dcosth
      if (ttt.le.0.0) then
        sinthe = 0.0
      else
        sinthe = -sqrt(1.0 - dcosth)
      end if
      costhe = sqrt(dcosth)
      call uphi(3,2)                             ! Set direction cosines
                                                      ! ----------------
      return                                          ! Return to ELECTR
                                                      ! ----------------
      end

!-----------------------last line of egs5_bhabha.f----------------------
!---------------------------egs5_block_data.f---------------------------
! Version: 060802-1335
! Reference: SLAC-R-730/KEK-2005-8
!-----------------------------------------------------------------------
!23456789|123456789|123456789|123456789|123456789|123456789|123456789|12

      block data

      implicit none

      include 'include/egs5_h.f'               ! Main EGS5 "header" file

      include 'include/egs5_elecin.f'    ! COMMONs required by EGS5 code
      include 'include/egs5_epcont.f'
      include 'include/egs5_media.f'
      include 'include/egs5_mults.f'
      include 'include/egs5_thresh.f'
      include 'include/egs5_uphiot.f'
      include 'include/egs5_useful.f'
      include 'include/randomm.f'

      character*4 media1(24)                            ! Local variable
      equivalence(media1(1),media(1,1))

      data                                              ! common/ELECIN/
     * ekelim/0./,
     * icomp/1/

      data                                              ! common/EPCONT/
     * iausfl/5*1,MXAUSM5*0/,
     * rhof/1.0/

      data                                               ! common/MEDIA/
     * nmed/1/,
     * media1/'N','A','I',' ',' ',' ',' ',' ',' ',' ',' ',' ',
     *        ' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',' '/,
     * charD/MXMED*0.d0/,
     * iraylm/MXMED*0/,
     * incohm/MXMED*0/,
     * iprofm/MXMED*0/,
     * impacm/MXMED*0/,
     * useGSD/MXMED*0/

      data                                               ! common/MULTS/
     * NG21/  7/,B0G21/ 2.0000E+00/,B1G21/ 5.0000E+00/,
     * G210( 1),G211( 1),G212( 1)/-9.9140E-04, 2.7672E+00,-1.1544E+00/,
     * G210( 2),G211( 2),G212( 2)/-9.9140E-04, 2.7672E+00,-1.1544E+00/,
     * G210( 3),G211( 3),G212( 3)/-7.1017E-02, 3.4941E+00,-3.0773E+00/,
     * G210( 4),G211( 4),G212( 4)/-7.3556E-02, 3.5487E+00,-3.1989E+00/,
     * G210( 5),G211( 5),G212( 5)/ 3.6658E-01, 2.1162E+00,-2.0311E+00/,
     * G210( 6),G211( 6),G212( 6)/ 1.4498E+00,-5.9717E-01,-3.2951E-01/,
     * G210( 7),G211( 7),G212( 7)/ 1.4498E+00,-5.9717E-01,-3.2951E-01/
      data
     * NG22/  8/,B0G22/ 2.0000E+00/,B1G22/ 6.0000E+00/,
     * G220( 1),G221( 1),G222( 1)/-5.2593E-04, 1.4285E+00,-1.2670E+00/,
     * G220( 2),G221( 2),G222( 2)/-5.2593E-04, 1.4285E+00,-1.2670E+00/,
     * G220( 3),G221( 3),G222( 3)/-6.4819E-02, 2.2033E+00,-3.6399E+00/,
     * G220( 4),G221( 4),G222( 4)/ 3.7427E-02, 1.6630E+00,-2.9362E+00/,
     * G220( 5),G221( 5),G222( 5)/ 6.1955E-01,-6.2713E-01,-6.7859E-01/,
     * G220( 6),G221( 6),G222( 6)/ 1.7584E+00,-4.0390E+00, 1.8810E+00/,
     * G220( 7),G221( 7),G222( 7)/ 2.5694E+00,-6.0484E+00, 3.1256E+00/,
     * G220( 8),G221( 8),G222( 8)/ 2.5694E+00,-6.0484E+00, 3.1256E+00/
      data
     * NG31/ 11/,B0G31/ 2.0000E+00/,B1G31/ 9.0000E+00/,
     * G310( 1),G311( 1),G312( 1)/ 4.9437E-01, 1.9124E-02, 1.8375E+00/,
     * G310( 2),G311( 2),G312( 2)/ 4.9437E-01, 1.9124E-02, 1.8375E+00/,
     * G310( 3),G311( 3),G312( 3)/ 5.3251E-01,-6.1555E-01, 4.5595E+00/,
     * G310( 4),G311( 4),G312( 4)/ 6.6810E-01,-2.2056E+00, 8.9293E+00/,
     * G310( 5),G311( 5),G312( 5)/-3.8262E+00, 2.5528E+01,-3.3862E+01/,
     * G310( 6),G311( 6),G312( 6)/ 4.2335E+00,-1.0604E+01, 6.6702E+00/,
     * G310( 7),G311( 7),G312( 7)/ 5.0694E+00,-1.4208E+01, 1.0456E+01/,
     * G310( 8),G311( 8),G312( 8)/ 1.4563E+00,-3.3275E+00, 2.2601E+00/,
     * G310( 9),G311( 9),G312( 9)/-3.2852E-01, 1.2938E+00,-7.3254E-01/,
     * G310(10),G311(10),G312(10)/-2.2489E-01, 1.0713E+00,-6.1358E-01/,
     * G310(11),G311(11),G312(11)/-2.2489E-01, 1.0713E+00,-6.1358E-01/
      data
     * NG32/ 25/,B0G32/ 2.0000E+00/,B1G32/ 2.3000E+01/,
     * G320( 1),G321( 1),G322( 1)/ 2.9907E-05, 4.7318E-01, 6.5921E-01/,
     * G320( 2),G321( 2),G322( 2)/ 2.9907E-05, 4.7318E-01, 6.5921E-01/,
     * G320( 3),G321( 3),G322( 3)/ 2.5820E-03, 3.5853E-01, 1.9776E+00/,
     * G320( 4),G321( 4),G322( 4)/-5.3270E-03, 4.9418E-01, 1.4528E+00/,
     * G320( 5),G321( 5),G322( 5)/-6.6341E-02, 1.4422E+00,-2.2407E+00/,
     * G320( 6),G321( 6),G322( 6)/-3.6027E-01, 4.7190E+00,-1.1380E+01/,
     * G320( 7),G321( 7),G322( 7)/-2.7953E+00, 2.6694E+01,-6.0986E+01/,
     * G320( 8),G321( 8),G322( 8)/-3.6091E+00, 3.4125E+01,-7.7512E+01/,
     * G320( 9),G321( 9),G322( 9)/ 1.2491E+01,-7.1103E+01, 9.4496E+01/,
     * G320(10),G321(10),G322(10)/ 1.9637E+01,-1.1371E+02, 1.5794E+02/,
     * G320(11),G321(11),G322(11)/ 2.1692E+00,-2.5019E+01, 4.5340E+01/,
     * G320(12),G321(12),G322(12)/-1.6682E+01, 6.2067E+01,-5.5257E+01/,
     * G320(13),G321(13),G322(13)/-2.1539E+01, 8.2651E+01,-7.7065E+01/,
     * G320(14),G321(14),G322(14)/-1.4344E+01, 5.5193E+01,-5.0867E+01/,
     * G320(15),G321(15),G322(15)/-5.4990E+00, 2.3874E+01,-2.3140E+01/,
     * G320(16),G321(16),G322(16)/ 3.1029E+00,-4.4708E+00, 2.1318E-01/,
     * G320(17),G321(17),G322(17)/ 6.0961E+00,-1.3670E+01, 7.2823E+00/,
     * G320(18),G321(18),G322(18)/ 8.6179E+00,-2.0950E+01, 1.2536E+01/,
     * G320(19),G321(19),G322(19)/ 7.5064E+00,-1.7956E+01, 1.0520E+01/,
     * G320(20),G321(20),G322(20)/ 5.9838E+00,-1.4065E+01, 8.0342E+00/,
     * G320(21),G321(21),G322(21)/ 4.4959E+00,-1.0456E+01, 5.8462E+00/,
     * G320(22),G321(22),G322(22)/ 3.2847E+00,-7.6709E+00, 4.2445E+00/,
     * G320(23),G321(23),G322(23)/ 1.9514E+00,-4.7505E+00, 2.6452E+00/,
     * G320(24),G321(24),G322(24)/ 4.8808E-01,-1.6910E+00, 1.0459E+00/,
     * G320(25),G321(25),G322(25)/ 4.8808E-01,-1.6910E+00, 1.0459E+00/
      data
     * NBGB/  8/,B0BGB/ 1.5714E+00/,B1BGB/ 2.1429E-01/,
     * BGB0( 1),BGB1( 1),BGB2( 1)/-1.0724E+00, 2.8203E+00,-3.5669E-01/,
     * BGB0( 2),BGB1( 2),BGB2( 2)/ 3.7136E-01, 1.4560E+00,-2.8072E-02/,
     * BGB0( 3),BGB1( 3),BGB2( 3)/ 1.1396E+00, 1.1910E+00,-5.2070E-03/,
     * BGB0( 4),BGB1( 4),BGB2( 4)/ 1.4908E+00, 1.1267E+00,-2.2565E-03/,
     * BGB0( 5),BGB1( 5),BGB2( 5)/ 1.7342E+00, 1.0958E+00,-1.2705E-03/,
     * BGB0( 6),BGB1( 6),BGB2( 6)/ 1.9233E+00, 1.0773E+00,-8.1806E-04/,
     * BGB0( 7),BGB1( 7),BGB2( 7)/ 2.0791E+00, 1.0649E+00,-5.7197E-04/,
     * BGB0( 8),BGB1( 8),BGB2( 8)/ 2.0791E+00, 1.0649E+00,-5.7197E-04/
 
      data                                              ! common/THRESH/
     * RMT2/1.021997804/            ! Two times electron rest mass (MeV)
     * RMSQ/0.261119878/           ! Electron rest mass squared (MeV**2)

      data                                              ! common/UPHIOT/
     * PI/3.1415926535897932d0/,                                    ! Pi
     * TWOPI/6.2831853071795864d0/,                         ! 2 times Pi
     * PI5D2/7.853981634/                   ! Five times Pi divided by 2

      data                                              ! common/USEFUL/
     * RM/0.510998902/                        ! Electron rest mass (MeV)

      data                                             ! common/RLUXDAT/
     * twom24/1./,
     * ndskip/0,24,73,199,365/,
     * luxlev/1/,
     * inseed/0/,
     * kount/0/,
     * mkount/0/,
     * isdext/25*0/,
     * rluxset/.false./

      end
!---------------------last line of egs5_block_data.f--------------------
!------------------------egs5_block_data_atom.f-------------------------
! Version: 051219-1435
!          060619-1800   Change expression in D-type
!          070829-1530   Change precision of DFLX1(2)
! Reference: SLAC-R-730/KEK-2005-8
!-----------------------------------------------------------------------
!23456789|123456789|123456789|123456789|123456789|123456789|123456789|12

      block data atom

      implicit none

      include 'include/egs5_h.f'               ! Main EGS5 'header' file

      include 'include/egs5_edge.f'      ! COMMONs required by EGS5 code
      include 'include/egs5_photin.f'

      integer iiz                                       ! Local variable

!-------------------------------------------------------------!
!  E-K edge energy  in KeV                                    !
!            (Table 2. of Table of Isotopes, Eighth Edition)  !
!-------------------------------------------------------------!
      DATA (EEDGE(1,IIZ),IIZ=1,100)/
     *0.13600D-01,0.24600D-01,0.54800D-01,0.11210D+00,0.18800D+00,
     *0.28380D+00,0.40160D+00,0.53200D+00,0.68540D+00,0.87010D+00,
     *0.10721D+01,0.13050D+01,0.15596D+01,0.18389D+01,0.21455D+01,
     *0.24720D+01,0.28224D+01,0.32060D+01,0.36074D+01,0.40381D+01,
     *0.44928D+01,0.49664D+01,0.54651D+01,0.59892D+01,0.65390D+01,
     *0.71120D+01,0.77089D+01,0.83328D+01,0.89789D+01,0.96586D+01,
     *0.10367D+02,0.11103D+02,0.11867D+02,0.12658D+02,0.13474D+02,
     *0.14326D+02,0.15200D+02,0.16105D+02,0.17038D+02,0.17998D+02,
     *0.18986D+02,0.20000D+02,0.21044D+02,0.22117D+02,0.23220D+02,
     *0.24350D+02,0.25514D+02,0.26711D+02,0.27940D+02,0.29200D+02,
     *0.30491D+02,0.31814D+02,0.33169D+02,0.34564D+02,0.35985D+02,
     *0.37441D+02,0.38925D+02,0.40443D+02,0.41991D+02,0.43569D+02,
     *0.45184D+02,0.46834D+02,0.48519D+02,0.50239D+02,0.51996D+02,
     *0.53789D+02,0.55618D+02,0.57486D+02,0.59390D+02,0.61332D+02,
     *0.63314D+02,0.65351D+02,0.67416D+02,0.69525D+02,0.71676D+02,
     *0.73871D+02,0.76111D+02,0.78395D+02,0.80725D+02,0.83102D+02,
     *0.85530D+02,0.88005D+02,0.90526D+02,0.93100D+02,0.95724D+02,
     *0.98397D+02,0.10113D+03,0.10392D+03,0.10676D+03,0.10965D+03,
     *0.11260D+03,0.11560D+03,0.11867D+03,0.12180D+03,0.12498D+03,
     *0.12824D+03,0.13156D+03,0.13494D+03,0.13840D+03,0.14193D+03/
!--------------------------------------------------------------!
!OMEGAK is probability of X-ray emission at K-Shell absorption !
!            (Table 3. of Table of Isotopes, Eighth Edition)   !
!--------------------------------------------------------------!
      DATA OMEGAK/
     *  0.200D-04,  0.100D-03,  0.300D-03,  0.700D-03,  0.140D-02,
     *  0.260D-02,  0.430D-02,  0.690D-02,  0.100D-01,  0.150D-01,
     *  0.210D-01,  0.290D-01,  0.390D-01,  0.500D-01,  0.640D-01,
     *  0.800D-01,  0.990D-01,  0.120D+00,  0.143D+00,  0.169D+00,
     *  0.196D+00,  0.226D+00,  0.256D+00,  0.288D+00,  0.321D+00,
     *  0.355D+00,  0.388D+00,  0.421D+00,  0.454D+00,  0.486D+00,
     *  0.517D+00,  0.546D+00,  0.575D+00,  0.602D+00,  0.628D+00,
     *  0.652D+00,  0.674D+00,  0.696D+00,  0.716D+00,  0.734D+00,
     *  0.751D+00,  0.767D+00,  0.782D+00,  0.796D+00,  0.807D+00,
     *  0.820D+00,  0.831D+00,  0.842D+00,  0.851D+00,  0.860D+00,
     *  0.868D+00,  0.875D+00,  0.882D+00,  0.888D+00,  0.894D+00,
     *  0.900D+00,  0.905D+00,  0.910D+00,  0.914D+00,  0.918D+00,
     *  0.922D+00,  0.926D+00,  0.929D+00,  0.932D+00,  0.935D+00,
     *  0.938D+00,  0.940D+00,  0.942D+00,  0.945D+00,  0.947D+00,
     *  0.949D+00,  0.950D+00,  0.952D+00,  0.954D+00,  0.955D+00,
     *  0.957D+00,  0.958D+00,  0.959D+00,  0.960D+00,  0.962D+00,
     *  0.962D+00,  0.963D+00,  0.964D+00,  0.965D+00,  0.966D+00,
     *  0.967D+00,  0.967D+00,  0.968D+00,  0.969D+00,  0.969D+00,
     *  0.970D+00,  0.970D+00,  0.971D+00,  0.971D+00,  0.971D+00,
     *  0.972D+00,  0.972D+00,  0.972D+00,  0.972D+00,  0.973D+00/
!---------------------------------------------------------------------!
!E-Alpha1 photon energy (L3--> K electron transitions).               !
!                 (Table 7 Table of Isotopes Eighth Edition)          !
!---------------------------------------------------------------------!
      DATA (EKX(1,IIZ),IIZ=1,100)/
     *0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,0.18300D+00,
     *0.27700D+00,0.39200D+00,0.52500D+00,0.67700D+00,0.84900D+00,
     *0.10410D+01,0.12540D+01,0.14870D+01,0.17400D+01,0.20100D+01,
     *0.23080D+01,0.26220D+01,0.29570D+01,0.33140D+01,0.36920D+01,
     *0.40910D+01,0.45110D+01,0.49520D+01,0.54150D+01,0.58990D+01,
     *0.64040D+01,0.69300D+01,0.74780D+01,0.80480D+01,0.86390D+01,
     *0.92520D+01,0.98860D+01,0.10544D+02,0.11222D+02,0.11924D+02,
     *0.12651D+02,0.13395D+02,0.14165D+02,0.14958D+02,0.15775D+02,
     *0.16615D+02,0.17479D+02,0.18367D+02,0.19279D+02,0.20216D+02,
     *0.21177D+02,0.22163D+02,0.23174D+02,0.24210D+02,0.25271D+02,
     *0.26359D+02,0.27472D+02,0.28612D+02,0.29782D+02,0.30973D+02,
     *0.32194D+02,0.33442D+02,0.34720D+02,0.36026D+02,0.37361D+02,
     *0.38725D+02,0.40118D+02,0.41542D+02,0.42996D+02,0.44482D+02,
     *0.45998D+02,0.47547D+02,0.49128D+02,0.50742D+02,0.52389D+02,
     *0.54070D+02,0.55790D+02,0.57535D+02,0.59318D+02,0.61141D+02,
     *0.63000D+02,0.64896D+02,0.66831D+02,0.68806D+02,0.70818D+02,
     *0.72873D+02,0.74969D+02,0.77107D+02,0.79290D+02,0.81517D+02,
     *0.83787D+02,0.86105D+02,0.88471D+02,0.90886D+02,0.93350D+02,
     *0.95863D+02,0.98434D+02,0.10106D+03,0.10373D+03,0.10647D+03,
     *0.10927D+03,0.11212D+03,0.11503D+03,0.11801D+03,0.12106D+03/
!----------------------------------------------------------------------!
!E-Alpha2 photon energy (L2--> K electron transitions).                !
!                 (Table 7 Table of Isotopes Eighth Edition)           !
!----------------------------------------------------------------------!
      DATA (EKX(2,IIZ),IIZ=1,100)/
     *0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,0.18300D+00,
     *0.27700D+00,0.39200D+00,0.52500D+00,0.67700D+00,0.84800D+00,
     *0.10410D+01,0.12540D+01,0.14860D+01,0.17390D+01,0.20090D+01,
     *0.23070D+01,0.26210D+01,0.29550D+01,0.33110D+01,0.36880D+01,
     *0.40860D+01,0.45050D+01,0.49450D+01,0.54050D+01,0.58880D+01,
     *0.63910D+01,0.69150D+01,0.74610D+01,0.80280D+01,0.86160D+01,
     *0.92250D+01,0.98550D+01,0.10508D+02,0.11182D+02,0.11878D+02,
     *0.12598D+02,0.13336D+02,0.14098D+02,0.14883D+02,0.15691D+02,
     *0.16521D+02,0.17374D+02,0.18251D+02,0.19150D+02,0.20074D+02,
     *0.21020D+02,0.21990D+02,0.22984D+02,0.24002D+02,0.25044D+02,
     *0.26111D+02,0.27202D+02,0.28317D+02,0.29461D+02,0.30625D+02,
     *0.31817D+02,0.33034D+02,0.34279D+02,0.35550D+02,0.36847D+02,
     *0.38171D+02,0.39522D+02,0.40902D+02,0.42309D+02,0.43744D+02,
     *0.45208D+02,0.46700D+02,0.48221D+02,0.49773D+02,0.51354D+02,
     *0.52965D+02,0.54611D+02,0.56280D+02,0.57981D+02,0.59718D+02,
     *0.61486D+02,0.63287D+02,0.65122D+02,0.66991D+02,0.68894D+02,
     *0.70832D+02,0.72805D+02,0.74815D+02,0.76863D+02,0.78948D+02,
     *0.81069D+02,0.83231D+02,0.85431D+02,0.87675D+02,0.89957D+02,
     *0.92282D+02,0.94654D+02,0.97069D+02,0.99525D+02,0.10203D+03,
     *0.10459D+03,0.10718D+03,0.10983D+03,0.11253D+03,0.11529D+03/
!----------------------------------------------------------------------!
!E-Alpha3 photon energy (L1--> K electron transitions).                !
!                 (Table 7 Table of Isotopes Eighth Edition)           !
!----------------------------------------------------------------------!
      DATA (EKX(3,IIZ),IIZ=1,100)/
     *0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,
     *0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,
     *0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,
     *0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,
     *0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,
     *0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,
     *0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,
     *0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,
     *0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,
     *0.00000D+00,0.21708D+02,0.22693D+02,0.23702D+02,0.24735D+02,
     *0.25793D+02,0.26875D+02,0.27981D+02,0.29112D+02,0.30270D+02,
     *0.31452D+02,0.32658D+02,0.33894D+02,0.35156D+02,0.36443D+02,
     *0.37756D+02,0.39097D+02,0.40467D+02,0.41864D+02,0.43288D+02,
     *0.44743D+02,0.46224D+02,0.47734D+02,0.49274D+02,0.50846D+02,
     *0.52443D+02,0.54080D+02,0.55735D+02,0.57425D+02,0.59150D+02,
     *0.60903D+02,0.62693D+02,0.64514D+02,0.66372D+02,0.68263D+02,
     *0.70184D+02,0.72144D+02,0.74138D+02,0.76172D+02,0.78242D+02,
     *0.80349D+02,0.82496D+02,0.84683D+02,0.86910D+02,0.89178D+02,
     *0.91491D+02,0.93844D+02,0.96242D+02,0.98687D+02,0.10117D+03,
     *0.10371D+03,0.10630D+03,0.10893D+03,0.11161D+03,0.11435D+03/
!----------------------------------------------------------------------!
!E-Beta1  photon energy (M3--> K electron transitions).                !
!                 (Table 7 Table of Isotopes Eighth Edition)           !
!----------------------------------------------------------------------!
      DATA (EKX(4,IIZ),IIZ=1,100)/
     *0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,
     *0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,
     *0.00000D+00,0.00000D+00,0.15540D+01,0.18360D+01,0.21360D+01,
     *0.24640D+01,0.28160D+01,0.31900D+01,0.35900D+01,0.40130D+01,
     *0.44610D+01,0.49320D+01,0.54270D+01,0.59470D+01,0.64900D+01,
     *0.70580D+01,0.76490D+01,0.82650D+01,0.89050D+01,0.95720D+01,
     *0.10264D+02,0.10982D+02,0.11726D+02,0.12496D+02,0.13292D+02,
     *0.14111D+02,0.14961D+02,0.15836D+02,0.16738D+02,0.17667D+02,
     *0.18623D+02,0.19607D+02,0.20619D+02,0.21657D+02,0.22724D+02,
     *0.23819D+02,0.24943D+02,0.26095D+02,0.27276D+02,0.28486D+02,
     *0.29726D+02,0.30995D+02,0.32295D+02,0.33624D+02,0.34987D+02,
     *0.36378D+02,0.37801D+02,0.39258D+02,0.40748D+02,0.42272D+02,
     *0.43827D+02,0.45414D+02,0.47038D+02,0.48695D+02,0.50384D+02,
     *0.52113D+02,0.53877D+02,0.55674D+02,0.57505D+02,0.59383D+02,
     *0.61290D+02,0.63243D+02,0.65222D+02,0.67244D+02,0.69309D+02,
     *0.71414D+02,0.73560D+02,0.75749D+02,0.77982D+02,0.80255D+02,
     *0.82574D+02,0.84938D+02,0.87349D+02,0.89807D+02,0.92315D+02,
     *0.94868D+02,0.97474D+02,0.10013D+03,0.10284D+03,0.10560D+03,
     *0.10842D+03,0.11130D+03,0.11423D+03,0.11723D+03,0.12028D+03,
     *0.12340D+03,0.12658D+03,0.12982D+03,0.13314D+03,0.13652D+03/
!----------------------------------------------------------------------!
!E-Beta2  photon energy (N2+3--> K electron transitions).              !
!                 (Table 7 Table of Isotopes Eighth Edition)           !
!----------------------------------------------------------------------!
      DATA (EKX(5,IIZ),IIZ=1,100)/
     *0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,
     *0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,
     *0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,
     *0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,
     *0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,
     *0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,
     *0.10366D+02,0.11101D+02,0.11864D+02,0.12652D+02,0.13469D+02,
     *0.14311D+02,0.15185D+02,0.16085D+02,0.17013D+02,0.17969D+02,
     *0.18952D+02,0.19965D+02,0.21005D+02,0.22074D+02,0.23172D+02,
     *0.24299D+02,0.25455D+02,0.26644D+02,0.27863D+02,0.29111D+02,
     *0.30393D+02,0.31704D+02,0.33047D+02,0.34419D+02,0.35818D+02,
     *0.37255D+02,0.38726D+02,0.40228D+02,0.41764D+02,0.43335D+02,
     *0.44942D+02,0.46578D+02,0.48249D+02,0.49959D+02,0.51698D+02,
     *0.53476D+02,0.55293D+02,0.57142D+02,0.59028D+02,0.60962D+02,
     *0.62929D+02,0.64942D+02,0.66982D+02,0.69067D+02,0.71195D+02,
     *0.73363D+02,0.75575D+02,0.77831D+02,0.80130D+02,0.82473D+02,
     *0.84865D+02,0.87300D+02,0.89784D+02,0.92317D+02,0.94900D+02,
     *0.97530D+02,0.10021D+03,0.10295D+03,0.10574D+03,0.10858D+03,
     *0.11149D+03,0.11444D+03,0.11746D+03,0.12054D+03,0.12368D+03,
     *0.12689D+03,0.13015D+03,0.13348D+03,0.13689D+03,0.14036D+03/
!----------------------------------------------------------------------!
!E-Beta3  photon energy (M2--> K electron transitions).                !
!                 (Table 7 Table of Isotopes Eighth Edition)           !
!----------------------------------------------------------------------!
      DATA (EKX(6,IIZ),IIZ=1,100)/
     *0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,
     *0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,
     *0.00000D+00,0.00000D+00,0.15540D+01,0.18360D+01,0.21360D+01,
     *0.24640D+01,0.28160D+01,0.31900D+01,0.35900D+01,0.40130D+01,
     *0.44610D+01,0.49320D+01,0.54270D+01,0.59470D+01,0.64900D+01,
     *0.70580D+01,0.76490D+01,0.82650D+01,0.89050D+01,0.95720D+01,
     *0.10260D+02,0.10975D+02,0.11720D+02,0.12490D+02,0.13284D+02,
     *0.14104D+02,0.14952D+02,0.15825D+02,0.16726D+02,0.17653D+02,
     *0.18607D+02,0.19590D+02,0.20599D+02,0.21634D+02,0.22699D+02,
     *0.23791D+02,0.24912D+02,0.26060D+02,0.27238D+02,0.28444D+02,
     *0.29679D+02,0.30944D+02,0.32239D+02,0.33562D+02,0.34920D+02,
     *0.36304D+02,0.37720D+02,0.39170D+02,0.40653D+02,0.42166D+02,
     *0.43713D+02,0.45293D+02,0.46905D+02,0.48551D+02,0.50228D+02,
     *0.51947D+02,0.53695D+02,0.55480D+02,0.57300D+02,0.59159D+02,
     *0.61050D+02,0.62985D+02,0.64948D+02,0.66950D+02,0.68995D+02,
     *0.71079D+02,0.73202D+02,0.75368D+02,0.77577D+02,0.79824D+02,
     *0.82115D+02,0.84450D+02,0.86830D+02,0.89256D+02,0.91730D+02,
     *0.94247D+02,0.96815D+02,0.99432D+02,0.10210D+03,0.10482D+03,
     *0.10760D+03,0.11042D+03,0.11330D+03,0.11624D+03,0.11924D+03,
     *0.12230D+03,0.12542D+03,0.12859D+03,0.13184D+03,0.13515D+03/
!----------------------------------------------------------------------!
!E-Beta4  photon energy (N4,N5--> K electron transitions).             !
!                 (Table 7 Table of Isotopes Eighth Edition)           !
!----------------------------------------------------------------------!
      DATA (EKX(7,IIZ),IIZ=1,100)/
     *0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,
     *0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,
     *0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,
     *0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,
     *0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,
     *0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,
     *0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,
     *0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,
     *0.18982D+02,0.19998D+02,0.21042D+02,0.22115D+02,0.23217D+02,
     *0.24349D+02,0.25511D+02,0.26702D+02,0.27924D+02,0.29176D+02,
     *0.30460D+02,0.31774D+02,0.33120D+02,0.34496D+02,0.35907D+02,
     *0.37349D+02,0.38826D+02,0.40333D+02,0.41877D+02,0.43451D+02,
     *0.45064D+02,0.46705D+02,0.48386D+02,0.50099D+02,0.51849D+02,
     *0.53634D+02,0.55457D+02,0.57313D+02,0.59210D+02,0.61141D+02,
     *0.63114D+02,0.65132D+02,0.67181D+02,0.69273D+02,0.71409D+02,
     *0.73590D+02,0.75808D+02,0.78073D+02,0.80382D+02,0.82733D+02,
     *0.85134D+02,0.87580D+02,0.90074D+02,0.92618D+02,0.95211D+02,
     *0.97853D+02,0.10055D+03,0.10329D+03,0.10610D+03,0.10896D+03,
     *0.11187D+03,0.11484D+03,0.11788D+03,0.12097D+03,0.12413D+03,
     *0.12735D+03,0.13063D+03,0.13398D+03,0.13740D+03,0.14089D+03/
!----------------------------------------------------------------------!
!E-Beta5  photon energy (M4,M5--> K electron transitions).             !
!                 (Table 7 Table of Isotopes Eighth Edition)           !
!----------------------------------------------------------------------!
      DATA (EKX(8,IIZ),IIZ=1,100)/
     *0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,
     *0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,
     *0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,
     *0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,
     *0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,
     *0.71080D+01,0.77060D+01,0.83290D+01,0.89770D+01,0.96510D+01,
     *0.10350D+02,0.11074D+02,0.11826D+02,0.12601D+02,0.13404D+02,
     *0.14231D+02,0.15089D+02,0.15971D+02,0.16880D+02,0.17816D+02,
     *0.18780D+02,0.19771D+02,0.20789D+02,0.21836D+02,0.22911D+02,
     *0.24013D+02,0.25144D+02,0.26304D+02,0.27493D+02,0.28711D+02,
     *0.29959D+02,0.31236D+02,0.32544D+02,0.33881D+02,0.35252D+02,
     *0.36652D+02,0.38085D+02,0.39551D+02,0.41050D+02,0.42580D+02,
     *0.44145D+02,0.45741D+02,0.47373D+02,0.49038D+02,0.50738D+02,
     *0.52475D+02,0.54246D+02,0.56054D+02,0.57898D+02,0.59780D+02,
     *0.61700D+02,0.63662D+02,0.65652D+02,0.67685D+02,0.69760D+02,
     *0.71875D+02,0.74033D+02,0.76233D+02,0.78476D+02,0.80762D+02,
     *0.83093D+02,0.85470D+02,0.87892D+02,0.90363D+02,0.92883D+02,
     *0.95449D+02,0.98069D+02,0.10074D+03,0.10346D+03,0.10624D+03,
     *0.10907D+03,0.11196D+03,0.11491D+03,0.11792D+03,0.12099D+03,
     *0.12412D+03,0.12732D+03,0.13057D+03,0.13390D+03,0.13730D+03/
!----------------------------------------------------------------------!
!E-O       photon energy (O2O3--> K electron transitions).             !
!                 (Table 7 Table of Isotopes Eighth Edition)           !
!----------------------------------------------------------------------!
      DATA (EKX(9,IIZ),IIZ=1,100)/
     *0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,
     *0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,
     *0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,
     *0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,
     *0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,
     *0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,
     *0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,
     *0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,
     *0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,
     *0.00000D+00,0.00000D+00,0.00000D+00,0.27939D+02,0.29199D+02,
     *0.30489D+02,0.31812D+02,0.33166D+02,0.34552D+02,0.35972D+02,
     *0.37425D+02,0.38910D+02,0.40423D+02,0.41968D+02,0.43548D+02,
     *0.45162D+02,0.46813D+02,0.48497D+02,0.50219D+02,0.51970D+02,
     *0.53762D+02,0.55597D+02,0.57456D+02,0.59357D+02,0.61309D+02,
     *0.63286D+02,0.65316D+02,0.67376D+02,0.69484D+02,0.71636D+02,
     *0.73819D+02,0.76054D+02,0.78337D+02,0.80660D+02,0.83028D+02,
     *0.85444D+02,0.87911D+02,0.90421D+02,0.92983D+02,0.95595D+02,
     *0.98257D+02,0.10097D+03,0.10374D+03,0.10656D+03,0.10944D+03,
     *0.11238D+03,0.11538D+03,0.11843D+03,0.12154D+03,0.12472D+03,
     *0.12797D+03,0.13127D+03,0.13465D+03,0.13809D+03,0.14161D+03/
!----------------------------------------------------------------------!
!E-P       photon energy (P2P3--> K electron transitions).             !
!                 (Table 7 Table of Isotopes Eighth Edition)           !
!----------------------------------------------------------------------!
      DATA (EKX(10,IIZ),IIZ=1,100)/
     *0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,
     *0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,
     *0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,
     *0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,
     *0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,
     *0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,
     *0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,
     *0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,
     *0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,
     *0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,
     *0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,
     *0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,
     *0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,
     *0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,
     *0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,
     *0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,
     *0.85530D+02,0.88003D+02,0.90522D+02,0.93095D+02,0.95717D+02,
     *0.98389D+02,0.10112D+03,0.10390D+03,0.10674D+03,0.10963D+03,
     *0.11257D+03,0.11558D+03,0.11865D+03,0.12177D+03,0.12496D+03,
     *0.12821D+03,0.13152D+03,0.13491D+03,0.13836D+03,0.14189D+03/
!----------------------------------------------------------------------!
! Probability of K-alpha1 emission,                                    !
!         or Alpha1/(K-Xray total)                                     !
!                 (Table 7 Table of Isotopes Eighth Edition)           !
!  K-beta intensity are adjusted to experimental data using the data   !
!  in Table of Isotopes Seventh Edition                                !
!----------------------------------------------------------------------!
      DATA (DFKX(1,IIZ),IIZ=1,100)/
     *0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,0.66265D+00,
     *0.67857D+00,0.67308D+00,0.66265D+00,0.67669D+00,0.66667D+00,
     *0.66522D+00,0.66667D+00,0.65613D+00,0.65067D+00,0.64248D+00,
     *0.63056D+00,0.62068D+00,0.60590D+00,0.59327D+00,0.59032D+00,
     *0.58824D+00,0.58685D+00,0.58651D+00,0.58582D+00,0.58548D+00,
     *0.58514D+00,0.58480D+00,0.58411D+00,0.58309D+00,0.58207D+00,
     *0.57897D+00,0.57519D+00,0.57113D+00,0.56738D+00,0.56440D+00,
     *0.56243D+00,0.55960D+00,0.55741D+00,0.55463D+00,0.55188D+00,
     *0.54975D+00,0.54795D+00,0.54585D+00,0.54377D+00,0.54230D+00,
     *0.54054D+00,0.53966D+00,0.53821D+00,0.53618D+00,0.53446D+00,
     *0.53275D+00,0.53106D+00,0.52937D+00,0.52741D+00,0.52547D+00,
     *0.52381D+00,0.52244D+00,0.52081D+00,0.51918D+00,0.51757D+00,
     *0.51650D+00,0.51543D+00,0.51383D+00,0.51205D+00,0.51099D+00,
     *0.50993D+00,0.50888D+00,0.50733D+00,0.50603D+00,0.50475D+00,
     *0.50321D+00,0.50192D+00,0.50090D+00,0.49988D+00,0.49861D+00,
     *0.49709D+00,0.49634D+00,0.49531D+00,0.49359D+00,0.49260D+00,
     *0.49113D+00,0.48862D+00,0.48737D+00,0.48612D+00,0.48486D+00,
     *0.48362D+00,0.48216D+00,0.48070D+00,0.47923D+00,0.47823D+00,
     *0.47701D+00,0.47557D+00,0.47459D+00,0.47338D+00,0.47217D+00,
     *0.47075D+00,0.46865D+00,0.46636D+00,0.46496D+00,0.46355D+00/
!----------------------------------------------------------------------!
! Probability of K-alpha2 emission,                                    !
!         or DFKX(1)+Alpha2/(K-Xray total)                             !
!                 (Table 7 Table of Isotopes Eighth Edition)           !
!  K-beta intensity are adjusted to experimental data using the data   !
!  in Table of Isotopes Seventh Edition                                !
!----------------------------------------------------------------------!
      DATA (DFKX(2,IIZ),IIZ=1,100)/
     *0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,0.10000D+01,
     *0.10000D+01,0.10000D+01,0.10000D+01,0.10000D+01,0.10000D+01,
     *0.10000D+01,0.10000D+01,0.98168D+00,0.97403D+00,0.96216D+00,
     *0.94458D+00,0.92593D+00,0.90470D+00,0.89339D+00,0.88666D+00,
     *0.88412D+00,0.88204D+00,0.88152D+00,0.88108D+00,0.88115D+00,
     *0.88122D+00,0.88129D+00,0.88084D+00,0.87988D+00,0.87893D+00,
     *0.87482D+00,0.87026D+00,0.86469D+00,0.85959D+00,0.85563D+00,
     *0.85321D+00,0.85003D+00,0.84727D+00,0.84359D+00,0.84051D+00,
     *0.83782D+00,0.83562D+00,0.83297D+00,0.83034D+00,0.82863D+00,
     *0.82649D+00,0.82568D+00,0.82453D+00,0.82197D+00,0.81987D+00,
     *0.81831D+00,0.81623D+00,0.81417D+00,0.81168D+00,0.80975D+00,
     *0.80825D+00,0.80665D+00,0.80517D+00,0.80370D+00,0.80171D+00,
     *0.80109D+00,0.79994D+00,0.79850D+00,0.79675D+00,0.79612D+00,
     *0.79549D+00,0.79488D+00,0.79346D+00,0.79245D+00,0.79094D+00,
     *0.79004D+00,0.78902D+00,0.78842D+00,0.78781D+00,0.78681D+00,
     *0.78541D+00,0.78471D+00,0.78408D+00,0.78284D+00,0.78224D+00,
     *0.78090D+00,0.77838D+00,0.77735D+00,0.77633D+00,0.77578D+00,
     *0.77477D+00,0.77386D+00,0.77297D+00,0.77205D+00,0.77139D+00,
     *0.77085D+00,0.76994D+00,0.76978D+00,0.76924D+00,0.76870D+00,
     *0.76826D+00,0.76719D+00,0.76577D+00,0.76486D+00,0.76394D+00/
!----------------------------------------------------------------------!
! Probability of K-alpha3 emission,                                    !
!         or DFKX(2)+Alpha3/(K-Xray total)                             !
!                 (Table 7 Table of Isotopes Eighth Edition)           !
!  K-beta intensity are adjusted to experimental data using the data   !
!  in Table of Isotopes Seventh Edition                                !
!----------------------------------------------------------------------!
      DATA (DFKX(3,IIZ),IIZ=1,100)/
     *0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,0.10000D+01,
     *0.10000D+01,0.10000D+01,0.10000D+01,0.10000D+01,0.10000D+01,
     *0.10000D+01,0.10000D+01,0.98168D+00,0.97403D+00,0.96216D+00,
     *0.94458D+00,0.92593D+00,0.90470D+00,0.89339D+00,0.88666D+00,
     *0.88412D+00,0.88204D+00,0.88152D+00,0.88108D+00,0.88115D+00,
     *0.88122D+00,0.88129D+00,0.88084D+00,0.87988D+00,0.87893D+00,
     *0.87482D+00,0.87026D+00,0.86469D+00,0.85959D+00,0.85563D+00,
     *0.85321D+00,0.85003D+00,0.84727D+00,0.84359D+00,0.84051D+00,
     *0.83782D+00,0.83562D+00,0.83297D+00,0.83034D+00,0.82863D+00,
     *0.82649D+00,0.82569D+00,0.82454D+00,0.82199D+00,0.81989D+00,
     *0.81833D+00,0.81625D+00,0.81419D+00,0.81171D+00,0.80978D+00,
     *0.80828D+00,0.80670D+00,0.80522D+00,0.80375D+00,0.80177D+00,
     *0.80115D+00,0.80001D+00,0.79858D+00,0.79683D+00,0.79622D+00,
     *0.79560D+00,0.79499D+00,0.79359D+00,0.79259D+00,0.79109D+00,
     *0.79020D+00,0.78921D+00,0.78862D+00,0.78803D+00,0.78705D+00,
     *0.78566D+00,0.78499D+00,0.78440D+00,0.78318D+00,0.78262D+00,
     *0.78132D+00,0.77883D+00,0.77785D+00,0.77687D+00,0.77638D+00,
     *0.77541D+00,0.77457D+00,0.77374D+00,0.77288D+00,0.77228D+00,
     *0.77181D+00,0.77098D+00,0.77089D+00,0.77043D+00,0.76999D+00,
     *0.76965D+00,0.76871D+00,0.76743D+00,0.76666D+00,0.76587D+00/
!----------------------------------------------------------------------!
! Probability of K-beta1 emission,                                     !
!         or DFKX(3)+beta1/(K-Xray total)                              !
!                 (Table 7 Table of Isotopes Eighth Edition)           !
!  K-beta intensity are adjusted to experimental data using the data   !
!  in Table of Isotopes Seventh Edition                                !
!----------------------------------------------------------------------!
      DATA (DFKX(4,IIZ),IIZ=1,100)/
     *0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,0.10000D+01,
     *0.10000D+01,0.10000D+01,0.10000D+01,0.10000D+01,0.10000D+01,
     *0.10000D+01,0.10000D+01,0.99381D+00,0.99134D+00,0.98725D+00,
     *0.98137D+00,0.97514D+00,0.96751D+00,0.96416D+00,0.96178D+00,
     *0.96095D+00,0.96031D+00,0.95987D+00,0.95979D+00,0.95989D+00,
     *0.95972D+00,0.95970D+00,0.95950D+00,0.95922D+00,0.95886D+00,
     *0.95698D+00,0.95443D+00,0.95125D+00,0.94854D+00,0.94478D+00,
     *0.94282D+00,0.93954D+00,0.93671D+00,0.93407D+00,0.93201D+00,
     *0.93033D+00,0.92892D+00,0.92731D+00,0.92571D+00,0.92478D+00,
     *0.92324D+00,0.92224D+00,0.92079D+00,0.91962D+00,0.91851D+00,
     *0.91770D+00,0.91635D+00,0.91468D+00,0.91285D+00,0.91117D+00,
     *0.90965D+00,0.90808D+00,0.90666D+00,0.90514D+00,0.90313D+00,
     *0.90270D+00,0.90163D+00,0.90086D+00,0.89974D+00,0.89954D+00,
     *0.89934D+00,0.89947D+00,0.89891D+00,0.89889D+00,0.89837D+00,
     *0.89860D+00,0.89846D+00,0.89833D+00,0.89758D+00,0.89711D+00,
     *0.89663D+00,0.89546D+00,0.89468D+00,0.89328D+00,0.89231D+00,
     *0.89093D+00,0.89023D+00,0.88897D+00,0.88771D+00,0.88693D+00,
     *0.88568D+00,0.88498D+00,0.88430D+00,0.88358D+00,0.88323D+00,
     *0.88295D+00,0.88227D+00,0.88194D+00,0.88120D+00,0.88048D+00,
     *0.87981D+00,0.87931D+00,0.87842D+00,0.87778D+00,0.87712D+00/
!----------------------------------------------------------------------!
! Probability of K-beta2 emission,                                     !
!         or DFKX(4)+beta2/(K-Xray total)                              !
!                 (Table 7 Table of Isotopes Eighth Edition)           !
!  K-beta intensity are adjusted to experimental data using the data   !
!  in Table of Isotopes Seventh Edition                                !
!----------------------------------------------------------------------!
      DATA (DFKX(5,IIZ),IIZ=1,100)/
     *0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,0.10000D+01,
     *0.10000D+01,0.10000D+01,0.10000D+01,0.10000D+01,0.10000D+01,
     *0.10000D+01,0.10000D+01,0.99381D+00,0.99134D+00,0.98725D+00,
     *0.98137D+00,0.97514D+00,0.96751D+00,0.96416D+00,0.96178D+00,
     *0.96095D+00,0.96031D+00,0.95987D+00,0.95979D+00,0.95989D+00,
     *0.95972D+00,0.95970D+00,0.95950D+00,0.95922D+00,0.95886D+00,
     *0.95767D+00,0.95648D+00,0.95520D+00,0.95392D+00,0.95370D+00,
     *0.95351D+00,0.95353D+00,0.95343D+00,0.95293D+00,0.95243D+00,
     *0.95176D+00,0.95136D+00,0.95075D+00,0.95013D+00,0.94966D+00,
     *0.94910D+00,0.94912D+00,0.94920D+00,0.94820D+00,0.94711D+00,
     *0.94622D+00,0.94518D+00,0.94423D+00,0.94295D+00,0.94204D+00,
     *0.94140D+00,0.94070D+00,0.94085D+00,0.94064D+00,0.94037D+00,
     *0.94038D+00,0.94017D+00,0.93975D+00,0.93910D+00,0.93920D+00,
     *0.93898D+00,0.93863D+00,0.93801D+00,0.93748D+00,0.93703D+00,
     *0.93643D+00,0.93563D+00,0.93520D+00,0.93466D+00,0.93388D+00,
     *0.93363D+00,0.93318D+00,0.93297D+00,0.93253D+00,0.93215D+00,
     *0.93159D+00,0.93152D+00,0.93096D+00,0.93035D+00,0.92988D+00,
     *0.92918D+00,0.92870D+00,0.92833D+00,0.92787D+00,0.92761D+00,
     *0.92738D+00,0.92709D+00,0.92683D+00,0.92618D+00,0.92594D+00,
     *0.92518D+00,0.92504D+00,0.92466D+00,0.92446D+00,0.92389D+00/
!----------------------------------------------------------------------!
! Probability of K-beta3 emission,                                     !
!         or DFKX(5)+beta3/(K-Xray total)                              !
!                 (Table 7 Table of Isotopes Eighth Edition)           !
!  K-beta intensity are adjusted to experimental data using the data   !
!  in Table of Isotopes Seventh Edition                                !
!----------------------------------------------------------------------!
      DATA (DFKX(6,IIZ),IIZ=1,100)/
     *0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,0.10000D+01,
     *0.10000D+01,0.10000D+01,0.10000D+01,0.10000D+01,0.10000D+01,
     *0.10000D+01,0.10000D+01,0.10000D+01,0.10000D+01,0.10000D+01,
     *0.10000D+01,0.10000D+01,0.10000D+01,0.10000D+01,0.10000D+01,
     *0.10000D+01,0.10000D+01,0.10000D+01,0.10000D+01,0.10000D+01,
     *0.99996D+00,0.99994D+00,0.99993D+00,0.99991D+00,0.99988D+00,
     *0.99986D+00,0.99984D+00,0.99981D+00,0.99977D+00,0.99974D+00,
     *0.99972D+00,0.99969D+00,0.99966D+00,0.99963D+00,0.99959D+00,
     *0.99954D+00,0.99950D+00,0.99944D+00,0.99938D+00,0.99934D+00,
     *0.99926D+00,0.99920D+00,0.99913D+00,0.99882D+00,0.99834D+00,
     *0.99781D+00,0.99711D+00,0.99628D+00,0.99532D+00,0.99458D+00,
     *0.99391D+00,0.99326D+00,0.99335D+00,0.99316D+00,0.99291D+00,
     *0.99283D+00,0.99270D+00,0.99262D+00,0.99232D+00,0.99261D+00,
     *0.99258D+00,0.99259D+00,0.99255D+00,0.99257D+00,0.99265D+00,
     *0.99235D+00,0.99219D+00,0.99186D+00,0.99156D+00,0.99124D+00,
     *0.99100D+00,0.99071D+00,0.99028D+00,0.98984D+00,0.98945D+00,
     *0.98885D+00,0.98820D+00,0.98749D+00,0.98674D+00,0.98612D+00,
     *0.98528D+00,0.98463D+00,0.98409D+00,0.98346D+00,0.98308D+00,
     *0.98271D+00,0.98226D+00,0.98189D+00,0.98157D+00,0.98119D+00,
     *0.98072D+00,0.98034D+00,0.98016D+00,0.97979D+00,0.97952D+00/
!----------------------------------------------------------------------!
! Probability of K-beta4 emission,                                     !
!         or DFKX(6)+beta4/(K-Xray total)                              !
!                 (Table 7 Table of Isotopes Eighth Edition)           !
!  K-beta intensity are adjusted to experimental data using the data   !
!  in Table of Isotopes Seventh Edition                                !
!----------------------------------------------------------------------!
      DATA (DFKX(7,IIZ),IIZ=1,100)/
     *0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,0.10000D+01,
     *0.10000D+01,0.10000D+01,0.10000D+01,0.10000D+01,0.10000D+01,
     *0.10000D+01,0.10000D+01,0.10000D+01,0.10000D+01,0.10000D+01,
     *0.10000D+01,0.10000D+01,0.10000D+01,0.10000D+01,0.10000D+01,
     *0.10000D+01,0.10000D+01,0.10000D+01,0.10000D+01,0.10000D+01,
     *0.99996D+00,0.99994D+00,0.99993D+00,0.99991D+00,0.99988D+00,
     *0.99986D+00,0.99984D+00,0.99981D+00,0.99977D+00,0.99974D+00,
     *0.99972D+00,0.99969D+00,0.99966D+00,0.99963D+00,0.99959D+00,
     *0.99956D+00,0.99952D+00,0.99948D+00,0.99943D+00,0.99940D+00,
     *0.99935D+00,0.99930D+00,0.99925D+00,0.99896D+00,0.99850D+00,
     *0.99798D+00,0.99730D+00,0.99649D+00,0.99554D+00,0.99483D+00,
     *0.99417D+00,0.99355D+00,0.99368D+00,0.99350D+00,0.99329D+00,
     *0.99324D+00,0.99313D+00,0.99308D+00,0.99276D+00,0.99306D+00,
     *0.99305D+00,0.99307D+00,0.99304D+00,0.99307D+00,0.99318D+00,
     *0.99290D+00,0.99277D+00,0.99246D+00,0.99220D+00,0.99192D+00,
     *0.99171D+00,0.99147D+00,0.99108D+00,0.99068D+00,0.99034D+00,
     *0.98979D+00,0.98919D+00,0.98853D+00,0.98784D+00,0.98728D+00,
     *0.98650D+00,0.98592D+00,0.98544D+00,0.98488D+00,0.98458D+00,
     *0.98429D+00,0.98391D+00,0.98361D+00,0.98337D+00,0.98307D+00,
     *0.98268D+00,0.98237D+00,0.98225D+00,0.98196D+00,0.98176D+00/
!----------------------------------------------------------------------!
! Probability of K-beta5 emission,                                     !
!         or DFKX(7)+beta5/(K-Xray total)                              !
!                 (Table 7 Table of Isotopes Eighth Edition)           !
!  K-beta intensity are adjusted to experimental data using the data   !
!  in Table of Isotopes Seventh Edition                                !
!----------------------------------------------------------------------!
      DATA (DFKX(8,IIZ),IIZ=1,100)/
     *0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,0.10000D+01,
     *0.10000D+01,0.10000D+01,0.10000D+01,0.10000D+01,0.10000D+01,
     *0.10000D+01,0.10000D+01,0.10000D+01,0.10000D+01,0.10000D+01,
     *0.10000D+01,0.10000D+01,0.10000D+01,0.10000D+01,0.10000D+01,
     *0.10000D+01,0.10000D+01,0.10000D+01,0.10000D+01,0.10000D+01,
     *0.10000D+01,0.10000D+01,0.10000D+01,0.10000D+01,0.10000D+01,
     *0.10000D+01,0.10000D+01,0.10000D+01,0.10000D+01,0.10000D+01,
     *0.10000D+01,0.10000D+01,0.10000D+01,0.10000D+01,0.10000D+01,
     *0.10000D+01,0.10000D+01,0.10000D+01,0.10000D+01,0.10000D+01,
     *0.10000D+01,0.10000D+01,0.10000D+01,0.99977D+00,0.99936D+00,
     *0.99885D+00,0.99821D+00,0.99746D+00,0.99658D+00,0.99591D+00,
     *0.99535D+00,0.99478D+00,0.99494D+00,0.99482D+00,0.99466D+00,
     *0.99469D+00,0.99464D+00,0.99464D+00,0.99430D+00,0.99471D+00,
     *0.99480D+00,0.99492D+00,0.99499D+00,0.99512D+00,0.99531D+00,
     *0.99513D+00,0.99509D+00,0.99488D+00,0.99472D+00,0.99453D+00,
     *0.99442D+00,0.99427D+00,0.99397D+00,0.99366D+00,0.99341D+00,
     *0.99295D+00,0.99243D+00,0.99187D+00,0.99127D+00,0.99080D+00,
     *0.99012D+00,0.98960D+00,0.98919D+00,0.98873D+00,0.98853D+00,
     *0.98832D+00,0.98802D+00,0.98781D+00,0.98766D+00,0.98744D+00,
     *0.98713D+00,0.98689D+00,0.98684D+00,0.98663D+00,0.98651D+00/
!----------------------------------------------------------------------!
! Probability of K-O emission,                                         !
!         or DFKX(8)+KO/(K-Xray total)                                 !
!                 (Table 7 Table of Isotopes Eighth Edition)           !
!  K-beta intensity are adjusted to experimental data using the data   !
!  in Table of Isotopes Seventh Edition                                !
!----------------------------------------------------------------------!
      DATA (DFKX(9,IIZ),IIZ=1,100)/
     *0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,0.10000D+01,
     *0.10000D+01,0.10000D+01,0.10000D+01,0.10000D+01,0.10000D+01,
     *0.10000D+01,0.10000D+01,0.10000D+01,0.10000D+01,0.10000D+01,
     *0.10000D+01,0.10000D+01,0.10000D+01,0.10000D+01,0.10000D+01,
     *0.10000D+01,0.10000D+01,0.10000D+01,0.10000D+01,0.10000D+01,
     *0.10000D+01,0.10000D+01,0.10000D+01,0.10000D+01,0.10000D+01,
     *0.10000D+01,0.10000D+01,0.10000D+01,0.10000D+01,0.10000D+01,
     *0.10000D+01,0.10000D+01,0.10000D+01,0.10000D+01,0.10000D+01,
     *0.10000D+01,0.10000D+01,0.10000D+01,0.10000D+01,0.10000D+01,
     *0.10000D+01,0.10000D+01,0.10000D+01,0.10000D+01,0.10000D+01,
     *0.10000D+01,0.10000D+01,0.10000D+01,0.10000D+01,0.10000D+01,
     *0.10000D+01,0.10000D+01,0.10000D+01,0.10000D+01,0.10000D+01,
     *0.10000D+01,0.10000D+01,0.10000D+01,0.10000D+01,0.10000D+01,
     *0.10000D+01,0.10000D+01,0.10000D+01,0.10000D+01,0.10000D+01,
     *0.10000D+01,0.10000D+01,0.10000D+01,0.10000D+01,0.10000D+01,
     *0.10000D+01,0.10000D+01,0.10000D+01,0.10000D+01,0.10000D+01,
     *0.99994D+00,0.99983D+00,0.99967D+00,0.99947D+00,0.99924D+00,
     *0.99897D+00,0.99876D+00,0.99856D+00,0.99841D+00,0.99827D+00,
     *0.99832D+00,0.99828D+00,0.99826D+00,0.99831D+00,0.99829D+00,
     *0.99817D+00,0.99816D+00,0.99824D+00,0.99822D+00,0.99823D+00/
!---------------------------------------------------------------------!
!  E-L1 edge energy  in KeV                                           !
!            (Table 2. of Table of Isotopes, Eighth Edition)          !
!---------------------------------------------------------------------!
      DATA (EEDGE(2,IIZ),IIZ=1,100)/
     *0.00000D+00,0.00000D+00,0.53000D-02,0.80000D-02,0.12600D-01,
     *0.18000D-01,0.24400D-01,0.28500D-01,0.34000D-01,0.48500D-01,
     *0.63300D-01,0.89400D-01,0.11770D+00,0.14870D+00,0.18930D+00,
     *0.22920D+00,0.27020D+00,0.32630D+00,0.37710D+00,0.43780D+00,
     *0.50040D+00,0.56370D+00,0.62820D+00,0.69460D+00,0.76900D+00,
     *0.84610D+00,0.92560D+00,0.10081D+01,0.10961D+01,0.11936D+01,
     *0.12977D+01,0.14143D+01,0.15265D+01,0.16539D+01,0.17820D+01,
     *0.19210D+01,0.20651D+01,0.22163D+01,0.23725D+01,0.25316D+01,
     *0.26977D+01,0.28655D+01,0.30425D+01,0.32240D+01,0.34119D+01,
     *0.36043D+01,0.38058D+01,0.40180D+01,0.42375D+01,0.44647D+01,
     *0.46983D+01,0.49392D+01,0.51881D+01,0.54528D+01,0.57143D+01,
     *0.59888D+01,0.62663D+01,0.65488D+01,0.68348D+01,0.71260D+01,
     *0.74279D+01,0.77368D+01,0.80520D+01,0.83756D+01,0.87080D+01,
     *0.90458D+01,0.93942D+01,0.97513D+01,0.10116D+02,0.10486D+02,
     *0.10870D+02,0.11271D+02,0.11682D+02,0.12100D+02,0.12527D+02,
     *0.12968D+02,0.13419D+02,0.13881D+02,0.14353D+02,0.14839D+02,
     *0.15347D+02,0.15861D+02,0.16388D+02,0.16928D+02,0.17482D+02,
     *0.18048D+02,0.18634D+02,0.19232D+02,0.19846D+02,0.20472D+02,
     *0.21105D+02,0.21758D+02,0.22427D+02,0.23104D+02,0.23808D+02,
     *0.24526D+02,0.25256D+02,0.26010D+02,0.26782D+02,0.27574D+02/
!---------------------------------------------------------------------!
!  E-L2 edge energy  in KeV                                           !
!            (Table 2. of Table of Isotopes, Eighth Edition)          !
!---------------------------------------------------------------------!
      DATA (EEDGE(3,IIZ),IIZ=1,100)/
     *0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,0.47000D-02,
     *0.64000D-02,0.92000D-02,0.71000D-02,0.86000D-02,0.21700D-01,
     *0.31100D-01,0.51400D-01,0.73200D-01,0.99500D-01,0.13620D+00,
     *0.16540D+00,0.20160D+00,0.25070D+00,0.29630D+00,0.35000D+00,
     *0.40670D+00,0.46150D+00,0.52050D+00,0.58370D+00,0.65140D+00,
     *0.72110D+00,0.79360D+00,0.87190D+00,0.95100D+00,0.10428D+01,
     *0.11423D+01,0.12478D+01,0.13586D+01,0.14762D+01,0.15960D+01,
     *0.17272D+01,0.18639D+01,0.20068D+01,0.21555D+01,0.23067D+01,
     *0.24647D+01,0.26251D+01,0.27932D+01,0.29669D+01,0.31461D+01,
     *0.33303D+01,0.35237D+01,0.37270D+01,0.39380D+01,0.41561D+01,
     *0.43804D+01,0.46120D+01,0.48521D+01,0.51037D+01,0.53594D+01,
     *0.56236D+01,0.58906D+01,0.61642D+01,0.64404D+01,0.67215D+01,
     *0.70128D+01,0.73118D+01,0.76171D+01,0.79303D+01,0.82516D+01,
     *0.85806D+01,0.89178D+01,0.92643D+01,0.96169D+01,0.99782D+01,
     *0.10349D+02,0.10739D+02,0.11136D+02,0.11544D+02,0.11959D+02,
     *0.12385D+02,0.12824D+02,0.13273D+02,0.13734D+02,0.14209D+02,
     *0.14698D+02,0.15200D+02,0.15711D+02,0.16237D+02,0.16776D+02,
     *0.17328D+02,0.17899D+02,0.18484D+02,0.19081D+02,0.19693D+02,
     *0.20314D+02,0.20948D+02,0.21600D+02,0.22266D+02,0.22952D+02,
     *0.23651D+02,0.24371D+02,0.25108D+02,0.25865D+02,0.26641D+02/
!---------------------------------------------------------------------!
!  E-L3 edge energy  in KeV                                           !
!            (Table 2. of Table of Isotopes, Eighth Edition)          !
!---------------------------------------------------------------------!
      DATA (EEDGE(4,IIZ),IIZ=1,100)/
     *0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,0.47000D-02,
     *0.64000D-02,0.92000D-02,0.71000D-02,0.86000D-02,0.21600D-01,
     *0.31100D-01,0.51400D-01,0.72700D-01,0.98900D-01,0.13530D+00,
     *0.16420D+00,0.20000D+00,0.24860D+00,0.29360D+00,0.34640D+00,
     *0.40220D+00,0.45550D+00,0.51290D+00,0.57450D+00,0.64030D+00,
     *0.70810D+00,0.77860D+00,0.85470D+00,0.93110D+00,0.10197D+01,
     *0.11154D+01,0.12167D+01,0.13231D+01,0.14358D+01,0.15499D+01,
     *0.16749D+01,0.18044D+01,0.19396D+01,0.20800D+01,0.22223D+01,
     *0.23705D+01,0.25202D+01,0.26769D+01,0.28379D+01,0.30038D+01,
     *0.31733D+01,0.33511D+01,0.35375D+01,0.37301D+01,0.39288D+01,
     *0.41322D+01,0.43414D+01,0.45571D+01,0.47822D+01,0.50119D+01,
     *0.52470D+01,0.54827D+01,0.57234D+01,0.59643D+01,0.62079D+01,
     *0.64593D+01,0.67162D+01,0.69769D+01,0.72428D+01,0.75140D+01,
     *0.77901D+01,0.80711D+01,0.83579D+01,0.86480D+01,0.89436D+01,
     *0.92441D+01,0.95607D+01,0.98811D+01,0.10207D+02,0.10535D+02,
     *0.10871D+02,0.11215D+02,0.11564D+02,0.11919D+02,0.12284D+02,
     *0.12658D+02,0.13035D+02,0.13419D+02,0.13810D+02,0.14207D+02,
     *0.14610D+02,0.15025D+02,0.15444D+02,0.15870D+02,0.16300D+02,
     *0.16733D+02,0.17168D+02,0.17610D+02,0.18057D+02,0.18510D+02,
     *0.18970D+02,0.19435D+02,0.19907D+02,0.20384D+02,0.20868D+02/
!--------------------------------------------------------------------!
!L-beta3 photon energy in keV (M3--> L1 electron transitions).       !
!                (Table 7b Table of Isotopes Eighth Edition)         !
!--------------------------------------------------------------------!
      DATA (ELX1(1,IIZ),IIZ=1,100)/
     *0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,
     *0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,
     *0.00000D+00,0.00000D+00,0.11200D+00,0.14600D+00,0.17900D+00,
     *0.22100D+00,0.26300D+00,0.31000D+00,0.35900D+00,0.41200D+00,
     *0.46800D+00,0.52900D+00,0.59000D+00,0.65200D+00,0.72000D+00,
     *0.79200D+00,0.86600D+00,0.94000D+00,0.10220D+01,0.11070D+01,
     *0.11950D+01,0.12940D+01,0.13860D+01,0.14920D+01,0.16010D+01,
     *0.17070D+01,0.18270D+01,0.19470D+01,0.20720D+01,0.22010D+01,
     *0.23350D+01,0.24730D+01,0.26170D+01,0.27630D+01,0.29160D+01,
     *0.30730D+01,0.32340D+01,0.34020D+01,0.35730D+01,0.37500D+01,
     *0.39330D+01,0.41210D+01,0.43140D+01,0.45120D+01,0.47170D+01,
     *0.49270D+01,0.51430D+01,0.53630D+01,0.55930D+01,0.58290D+01,
     *0.60710D+01,0.63170D+01,0.65710D+01,0.68320D+01,0.70970D+01,
     *0.73700D+01,0.76530D+01,0.79400D+01,0.82310D+01,0.85370D+01,
     *0.88470D+01,0.91630D+01,0.94880D+01,0.98190D+01,0.10159D+02,
     *0.10511D+02,0.10868D+02,0.11235D+02,0.11610D+02,0.11992D+02,
     *0.12390D+02,0.12794D+02,0.13211D+02,0.13635D+02,0.14073D+02,
     *0.14519D+02,0.14978D+02,0.15447D+02,0.15931D+02,0.16426D+02,
     *0.16931D+02,0.17454D+02,0.17992D+02,0.18541D+02,0.19110D+02,
     *0.19688D+02,0.20280D+02,0.20894D+02,0.21523D+02,0.22169D+02/
!--------------------------------------------------------------------!
!L-beta4 photon energy in keV (M2--> L1 electron transitions).       !
!                (Table 7b Table of Isotopes Eighth Edition)         !
!--------------------------------------------------------------------!
      DATA (ELX1(2,IIZ),IIZ=1,100)/
     *0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,
     *0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,
     *0.00000D+00,0.00000D+00,0.11200D+00,0.14600D+00,0.17900D+00,
     *0.22100D+00,0.26300D+00,0.31000D+00,0.35900D+00,0.41200D+00,
     *0.46800D+00,0.52900D+00,0.59000D+00,0.65200D+00,0.72000D+00,
     *0.79200D+00,0.86600D+00,0.94000D+00,0.10220D+01,0.11070D+01,
     *0.11910D+01,0.12860D+01,0.13800D+01,0.14860D+01,0.15930D+01,
     *0.16990D+01,0.18180D+01,0.19360D+01,0.20600D+01,0.21870D+01,
     *0.23190D+01,0.24560D+01,0.25980D+01,0.27410D+01,0.28910D+01,
     *0.30450D+01,0.32030D+01,0.33670D+01,0.35350D+01,0.37080D+01,
     *0.38860D+01,0.40700D+01,0.42580D+01,0.44510D+01,0.46490D+01,
     *0.48520D+01,0.50620D+01,0.52760D+01,0.54970D+01,0.57230D+01,
     *0.59560D+01,0.61960D+01,0.64380D+01,0.66870D+01,0.69400D+01,
     *0.72040D+01,0.74710D+01,0.77460D+01,0.80260D+01,0.83130D+01,
     *0.86070D+01,0.89050D+01,0.92130D+01,0.95250D+01,0.98450D+01,
     *0.10176D+02,0.10510D+02,0.10854D+02,0.11205D+02,0.11561D+02,
     *0.11931D+02,0.12307D+02,0.12691D+02,0.13084D+02,0.13488D+02,
     *0.13898D+02,0.14319D+02,0.14749D+02,0.15191D+02,0.15641D+02,
     *0.16104D+02,0.16577D+02,0.17061D+02,0.17557D+02,0.18069D+02,
     *0.18589D+02,0.19118D+02,0.19665D+02,0.20224D+02,0.20798D+02/
!--------------------------------------------------------------------!
!L-Beta10  photon energy (M IV --> L I electron transition).         !
!                 (Table IV of Storm Israel)                         !
!--------------------------------------------------------------------!
      DATA (ELX1(3,IIZ),IIZ=1,100)/
     *0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,
     *0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,
     *0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,
     *0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,
     *0.00000D+00,0.00000D+00,0.00000D+00,0.69500D+00,0.76700D+00,
     *0.84300D+00,0.92200D+00,0.10030D+01,0.10890D+01,0.11830D+01,
     *0.12830D+01,0.13850D+01,0.14890D+01,0.15970D+01,0.17100D+01,
     *0.18300D+01,0.19530D+01,0.20810D+01,0.22150D+01,0.23510D+01,
     *0.24920D+01,0.26370D+01,0.27870D+01,0.29400D+01,0.31000D+01,
     *0.32650D+01,0.34330D+01,0.36080D+01,0.37870D+01,0.39720D+01,
     *0.41610D+01,0.43560D+01,0.45550D+01,0.47590D+01,0.49730D+01,
     *0.51930D+01,0.54180D+01,0.56480D+01,0.58840D+01,0.61270D+01,
     *0.63760D+01,0.66300D+01,0.68910D+01,0.71580D+01,0.74340D+01,
     *0.77140D+01,0.80020D+01,0.82990D+01,0.86010D+01,0.89120D+01,
     *0.92330D+01,0.95560D+01,0.98870D+01,0.10227D+02,0.10578D+02,
     *0.10938D+02,0.11303D+02,0.11678D+02,0.12062D+02,0.12457D+02,
     *0.12861D+02,0.13275D+02,0.13702D+02,0.14138D+02,0.14582D+02,
     *0.15033D+02,0.15503D+02,0.15984D+02,0.16474D+02,0.16976D+02,
     *0.17496D+02,0.18031D+02,0.18577D+02,0.19136D+02,0.19712D+02,
     *0.20305D+02,0.20911D+02,0.21528D+02,0.22159D+02,0.22804D+02/
!---------------------------------------------------------------------!
!L-Beta9/1  photon energy (M V --> L I electron transition).          !
!                 (Table IV of Storm Israel)                          !
!---------------------------------------------------------------------!
      DATA (ELX1(4,IIZ),IIZ=1,100)/
     *0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,
     *0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,
     *0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,
     *0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,
     *0.00000D+00,0.00000D+00,0.00000D+00,0.69500D+00,0.76700D+00,
     *0.84300D+00,0.92200D+00,0.10030D+01,0.10890D+01,0.11830D+01,
     *0.12830D+01,0.13850D+01,0.14890D+01,0.15970D+01,0.17110D+01,
     *0.18310D+01,0.19550D+01,0.20830D+01,0.22170D+01,0.23530D+01,
     *0.24970D+01,0.26390D+01,0.27900D+01,0.29440D+01,0.31050D+01,
     *0.32700D+01,0.34390D+01,0.36140D+01,0.37950D+01,0.39800D+01,
     *0.41700D+01,0.43670D+01,0.45670D+01,0.47730D+01,0.49870D+01,
     *0.52070D+01,0.54340D+01,0.56660D+01,0.59040D+01,0.61500D+01,
     *0.64010D+01,0.66580D+01,0.69210D+01,0.71900D+01,0.74680D+01,
     *0.77510D+01,0.80430D+01,0.83430D+01,0.86480D+01,0.89610D+01,
     *0.92830D+01,0.96100D+01,0.99450D+01,0.10289D+02,0.10645D+02,
     *0.11009D+02,0.11379D+02,0.11758D+02,0.12147D+02,0.12547D+02,
     *0.12957D+02,0.13377D+02,0.13810D+02,0.14253D+02,0.14704D+02,
     *0.15163D+02,0.15639D+02,0.16128D+02,0.16626D+02,0.17134D+02,
     *0.17663D+02,0.18207D+02,0.18763D+02,0.19331D+02,0.19918D+02,
     *0.20523D+02,0.21143D+02,0.21776D+02,0.22425D+02,0.23090D+02/
!-------------------------------------------------------------------!
!L-gama2 photon energy in keV (N2--> L1 electron transitions).      !
!                (Table 7b Table of Isotopes Eighth Edition)        !
!-------------------------------------------------------------------!
      DATA (ELX1(5,IIZ),IIZ=1,100)/
     *0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,
     *0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,
     *0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,
     *0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,
     *0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,
     *0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,
     *0.00000D+00,0.14120D+01,0.15240D+01,0.16480D+01,0.17770D+01,
     *0.19060D+01,0.20500D+01,0.21960D+01,0.23470D+01,0.25030D+01,
     *0.26640D+01,0.28310D+01,0.30040D+01,0.31810D+01,0.33640D+01,
     *0.35530D+01,0.37430D+01,0.39510D+01,0.41600D+01,0.43760D+01,
     *0.46000D+01,0.48290D+01,0.50650D+01,0.53070D+01,0.55420D+01,
     *0.57970D+01,0.60600D+01,0.63260D+01,0.65990D+01,0.68830D+01,
     *0.71860D+01,0.74710D+01,0.77680D+01,0.80870D+01,0.83980D+01,
     *0.87140D+01,0.90510D+01,0.93850D+01,0.97300D+01,0.10090D+02,
     *0.10460D+02,0.10834D+02,0.11217D+02,0.11608D+02,0.12009D+02,
     *0.12421D+02,0.12841D+02,0.13273D+02,0.13709D+02,0.14158D+02,
     *0.14625D+02,0.15097D+02,0.15582D+02,0.16077D+02,0.16585D+02,
     *0.17104D+02,0.17635D+02,0.18177D+02,0.18734D+02,0.19304D+02,
     *0.19888D+02,0.20487D+02,0.21099D+02,0.21724D+02,0.22370D+02,
     *0.23028D+02,0.23698D+02,0.24390D+02,0.25099D+02,0.25825D+02/
!--------------------------------------------------------------------!
!L-gama3 photon energy in keV (N3--> L1 electron transitions).       !
!                (Table 7b Table of Isotopes Eighth Edition)         !
!--------------------------------------------------------------------!
      DATA (ELX1(6,IIZ),IIZ=1,100)/
     *0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,
     *0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,
     *0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,
     *0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,
     *0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,
     *0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,
     *0.12970D+01,0.14120D+01,0.15240D+01,0.16480D+01,0.17770D+01,
     *0.19070D+01,0.20510D+01,0.21960D+01,0.23470D+01,0.25030D+01,
     *0.26640D+01,0.28310D+01,0.30040D+01,0.31810D+01,0.33640D+01,
     *0.35530D+01,0.37500D+01,0.39510D+01,0.41600D+01,0.43760D+01,
     *0.46000D+01,0.48290D+01,0.50650D+01,0.53070D+01,0.55530D+01,
     *0.58090D+01,0.60750D+01,0.63420D+01,0.66170D+01,0.69010D+01,
     *0.71860D+01,0.74890D+01,0.77950D+01,0.81050D+01,0.84230D+01,
     *0.87530D+01,0.90880D+01,0.94310D+01,0.97790D+01,0.10143D+02,
     *0.10511D+02,0.10890D+02,0.11277D+02,0.11675D+02,0.12082D+02,
     *0.12500D+02,0.12924D+02,0.13361D+02,0.13807D+02,0.14262D+02,
     *0.14738D+02,0.15216D+02,0.15709D+02,0.16213D+02,0.16731D+02,
     *0.17258D+02,0.17800D+02,0.18353D+02,0.18922D+02,0.19505D+02,
     *0.20101D+02,0.20715D+02,0.21342D+02,0.21981D+02,0.22643D+02,
     *0.23319D+02,0.24007D+02,0.24718D+02,0.25446D+02,0.26195D+02/
!--------------------------------------------------------------------!
!L-Gamma41 photon energy (O II --> L I electron transition).         !
!                 (Table IV of Storm Israel)                         !
!--------------------------------------------------------------------!
      DATA (ELX1(7,IIZ),IIZ=1,100)/
     *0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,
     *0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,
     *0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,
     *0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,
     *0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,
     *0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,
     *0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,
     *0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,
     *0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,
     *0.00000D+00,0.00000D+00,0.00000D+00,0.42370D+01,0.44630D+01,
     *0.46950D+01,0.49340D+01,0.51810D+01,0.54360D+01,0.57020D+01,
     *0.59730D+01,0.62510D+01,0.65330D+01,0.68170D+01,0.71090D+01,
     *0.74080D+01,0.77150D+01,0.80300D+01,0.83510D+01,0.86830D+01,
     *0.90200D+01,0.93660D+01,0.97230D+01,0.10085D+02,0.10456D+02,
     *0.10838D+02,0.11233D+02,0.11637D+02,0.12051D+02,0.12477D+02,
     *0.12914D+02,0.13359D+02,0.13815D+02,0.14281D+02,0.14762D+02,
     *0.15255D+02,0.15757D+02,0.16274D+02,0.16803D+02,0.17341D+02,
     *0.17890D+02,0.18459D+02,0.19039D+02,0.19631D+02,0.20236D+02,
     *0.20858D+02,0.21494D+02,0.22144D+02,0.22809D+02,0.23497D+02,
     *0.24204D+02,0.24928D+02,0.25667D+02,0.26425D+02,0.27201D+02/
!--------------------------------------------------------------------!
!L-Gamma42 photon energy (O III --> L I electron transition).        !
!                 (Table IV of Storm Israel)                         !
!--------------------------------------------------------------------!
      DATA (ELX1(8,IIZ),IIZ=1,100)/
     *0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,
     *0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,
     *0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,
     *0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,
     *0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,
     *0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,
     *0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,
     *0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,
     *0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,
     *0.00000D+00,0.00000D+00,0.00000D+00,0.42370D+01,0.44610D+01,
     *0.46950D+01,0.49340D+01,0.51810D+01,0.54360D+01,0.57020D+01,
     *0.59740D+01,0.62520D+01,0.65340D+01,0.68190D+01,0.71110D+01,
     *0.74110D+01,0.77180D+01,0.80330D+01,0.83550D+01,0.86870D+01,
     *0.90240D+01,0.93710D+01,0.97270D+01,0.10090D+02,0.10461D+02,
     *0.10845D+02,0.11241D+02,0.11647D+02,0.12062D+02,0.12487D+02,
     *0.12927D+02,0.13373D+02,0.13828D+02,0.14295D+02,0.14776D+02,
     *0.15271D+02,0.15775D+02,0.16293D+02,0.16828D+02,0.17371D+02,
     *0.17924D+02,0.18497D+02,0.19084D+02,0.19681D+02,0.20291D+02,
     *0.20920D+02,0.21564D+02,0.22221D+02,0.22892D+02,0.23584D+02,
     *0.24296D+02,0.25025D+02,0.25770D+02,0.26533D+02,0.27314D+02/
!--------------------------------------------------------------------!
!L-beta1 photon energy in keV (M4--> L2 electron transitions).       !
!                (Table 7c Table of Isotopes Eighth Edition)         !
!--------------------------------------------------------------------!
      DATA (ELX2(1,IIZ),IIZ=1,100)/
     *0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,
     *0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,
     *0.00000D+00,0.00000D+00,0.73000D-01,0.99000D-01,0.13600D+00,
     *0.16500D+00,0.20200D+00,0.25100D+00,0.29600D+00,0.35000D+00,
     *0.40000D+00,0.45800D+00,0.51800D+00,0.58100D+00,0.64800D+00,
     *0.71700D+00,0.79100D+00,0.86800D+00,0.94900D+00,0.10350D+01,
     *0.11250D+01,0.12190D+01,0.13170D+01,0.14200D+01,0.15260D+01,
     *0.16320D+01,0.17520D+01,0.18720D+01,0.19960D+01,0.21240D+01,
     *0.22570D+01,0.23950D+01,0.25370D+01,0.26830D+01,0.28340D+01,
     *0.29900D+01,0.31510D+01,0.33170D+01,0.34870D+01,0.36630D+01,
     *0.38430D+01,0.40290D+01,0.42210D+01,0.44140D+01,0.46200D+01,
     *0.48280D+01,0.50420D+01,0.52630D+01,0.54890D+01,0.57220D+01,
     *0.59610D+01,0.62060D+01,0.64570D+01,0.67130D+01,0.69770D+01,
     *0.72480D+01,0.75260D+01,0.78110D+01,0.81020D+01,0.84020D+01,
     *0.87090D+01,0.90230D+01,0.93430D+01,0.96720D+01,0.10010D+02,
     *0.10354D+02,0.10708D+02,0.11071D+02,0.11443D+02,0.11824D+02,
     *0.12213D+02,0.12614D+02,0.13024D+02,0.13443D+02,0.13875D+02,
     *0.14316D+02,0.14770D+02,0.15236D+02,0.15711D+02,0.16202D+02,
     *0.16708D+02,0.17222D+02,0.17751D+02,0.18296D+02,0.18856D+02,
     *0.19427D+02,0.20018D+02,0.20624D+02,0.21248D+02,0.21889D+02/
!--------------------------------------------------------------------!
!L-gamma1 photon energy in keV (N4--> L2 electron transitions).      !
!                (Table 7c Table of Isotopes Eighth Edition)         !
!--------------------------------------------------------------------!
      DATA (ELX2(2,IIZ),IIZ=1,100)/
     *0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,
     *0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,
     *0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,
     *0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,
     *0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,
     *0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,
     *0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,
     *0.00000D+00,0.00000D+00,0.00000D+00,0.21530D+01,0.23040D+01,
     *0.24620D+01,0.26230D+01,0.27910D+01,0.29650D+01,0.31440D+01,
     *0.33290D+01,0.35200D+01,0.37180D+01,0.39220D+01,0.41320D+01,
     *0.43490D+01,0.45720D+01,0.48020D+01,0.50340D+01,0.52810D+01,
     *0.55310D+01,0.57920D+01,0.60540D+01,0.63270D+01,0.66040D+01,
     *0.68920D+01,0.71830D+01,0.74840D+01,0.77900D+01,0.81050D+01,
     *0.84260D+01,0.87570D+01,0.90880D+01,0.94370D+01,0.97800D+01,
     *0.10144D+02,0.10516D+02,0.10895D+02,0.11285D+02,0.11685D+02,
     *0.12096D+02,0.12513D+02,0.12942D+02,0.13382D+02,0.13830D+02,
     *0.14291D+02,0.14765D+02,0.15248D+02,0.15742D+02,0.16249D+02,
     *0.16770D+02,0.17302D+02,0.17848D+02,0.18405D+02,0.18980D+02,
     *0.19571D+02,0.20169D+02,0.20784D+02,0.21420D+02,0.22072D+02,
     *0.22735D+02,0.23416D+02,0.24117D+02,0.24836D+02,0.25574D+02/
!--------------------------------------------------------------------!
!L-gamma5 photon energy (N I --> L II electron transition).          !
!                 (Table IV of Storm Israel)                         !
!--------------------------------------------------------------------!
      DATA (ELX2(3,IIZ),IIZ=1,100)/
     *0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,
     *0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,
     *0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,
     *0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,
     *0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,
     *0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,0.10420D+01,
     *0.11400D+01,0.12440D+01,0.13510D+01,0.14630D+01,0.15790D+01,
     *0.17030D+01,0.18320D+01,0.19690D+01,0.21110D+01,0.22560D+01,
     *0.24070D+01,0.25620D+01,0.27230D+01,0.28920D+01,0.30650D+01,
     *0.32430D+01,0.34280D+01,0.36190D+01,0.38160D+01,0.40190D+01,
     *0.42280D+01,0.44440D+01,0.46660D+01,0.48940D+01,0.51290D+01,
     *0.53700D+01,0.56200D+01,0.58770D+01,0.61380D+01,0.64050D+01,
     *0.66810D+01,0.69650D+01,0.72550D+01,0.75510D+01,0.78550D+01,
     *0.81640D+01,0.84850D+01,0.88130D+01,0.91460D+01,0.94860D+01,
     *0.98340D+01,0.10200D+02,0.10570D+02,0.10946D+02,0.11332D+02,
     *0.11729D+02,0.12136D+02,0.12549D+02,0.12972D+02,0.13408D+02,
     *0.13852D+02,0.14309D+02,0.14775D+02,0.15254D+02,0.15743D+02,
     *0.16242D+02,0.16754D+02,0.17276D+02,0.17814D+02,0.18363D+02,
     *0.18929D+02,0.19507D+02,0.20099D+02,0.20708D+02,0.21333D+02,
     *0.21974D+02,0.22630D+02,0.23303D+02,0.23992D+02,0.24698D+02/
!--------------------------------------------------------------------!
!L-gamma6 photon energy in keV (O4--> L2 electron transitions).      !
!                (Table 7c Table of Isotopes Eighth Edition)         !
!--------------------------------------------------------------------!
      DATA (ELX2(4,IIZ),IIZ=1,100)/
     *0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,
     *0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,
     *0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,
     *0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,
     *0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,
     *0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,
     *0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,
     *0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,
     *0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,
     *0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,
     *0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,
     *0.00000D+00,0.58910D+01,0.00000D+00,0.00000D+00,0.00000D+00,
     *0.00000D+00,0.00000D+00,0.00000D+00,0.79300D+01,0.00000D+00,
     *0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,
     *0.10344D+02,0.10733D+02,0.11130D+02,0.11538D+02,0.11955D+02,
     *0.12385D+02,0.12820D+02,0.13270D+02,0.13731D+02,0.14199D+02,
     *0.14683D+02,0.15178D+02,0.15685D+02,0.16203D+02,0.16735D+02,
     *0.17280D+02,0.17839D+02,0.18412D+02,0.18997D+02,0.19599D+02,
     *0.20217D+02,0.20844D+02,0.21491D+02,0.22153D+02,0.22836D+02,
     *0.23527D+02,0.24241D+02,0.24971D+02,0.25723D+02,0.26492D+02/
!--------------------------------------------------------------------!
!L-eta photon energy in keV (M1--> L2 electron transitions).         !
!                (Table 7c Table of Isotopes Eighth Edition)         !
!--------------------------------------------------------------------!
      DATA (ELX2(5,IIZ),IIZ=1,100)/
     *0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,
     *0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,
     *0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,
     *0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,
     *0.35300D+00,0.40100D+00,0.45400D+00,0.51000D+00,0.56800D+00,
     *0.62800D+00,0.69300D+00,0.76000D+00,0.83100D+00,0.90700D+00,
     *0.98400D+00,0.10680D+01,0.11550D+01,0.12450D+01,0.13390D+01,
     *0.14350D+01,0.15420D+01,0.16490D+01,0.17620D+01,0.18760D+01,
     *0.19960D+01,0.21200D+01,0.22490D+01,0.23820D+01,0.25190D+01,
     *0.26600D+01,0.28060D+01,0.29570D+01,0.31120D+01,0.32720D+01,
     *0.34370D+01,0.36060D+01,0.37800D+01,0.39550D+01,0.41420D+01,
     *0.43310D+01,0.45290D+01,0.47300D+01,0.49290D+01,0.51460D+01,
     *0.53630D+01,0.55890D+01,0.58170D+01,0.60490D+01,0.62840D+01,
     *0.65340D+01,0.67890D+01,0.70580D+01,0.73100D+01,0.75800D+01,
     *0.78570D+01,0.81390D+01,0.84280D+01,0.87240D+01,0.90270D+01,
     *0.93370D+01,0.96500D+01,0.99750D+01,0.10309D+02,0.10647D+02,
     *0.10994D+02,0.11349D+02,0.11712D+02,0.12085D+02,0.12466D+02,
     *0.12855D+02,0.13255D+02,0.13662D+02,0.14082D+02,0.14511D+02,
     *0.14953D+02,0.15400D+02,0.15861D+02,0.16333D+02,0.16819D+02,
     *0.17314D+02,0.17826D+02,0.18347D+02,0.18884D+02,0.19433D+02/
!--------------------------------------------------------------------!
!L-alpha1 photon energy in keV (M5--> L3 electron transitions).      !
!                (Table 7d Table of Isotopes Eighth Edition)         !
!--------------------------------------------------------------------!
      DATA (ELX3(1,IIZ),IIZ=1,100)/
     *0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,
     *0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,
     *0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,
     *0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,
     *0.39600D+00,0.45200D+00,0.51100D+00,0.57200D+00,0.63700D+00,
     *0.70400D+00,0.77600D+00,0.85100D+00,0.92900D+00,0.10120D+01,
     *0.10980D+01,0.11880D+01,0.12820D+01,0.13790D+01,0.14810D+01,
     *0.15810D+01,0.16940D+01,0.18060D+01,0.19230D+01,0.20420D+01,
     *0.21660D+01,0.22930D+01,0.24240D+01,0.25580D+01,0.26970D+01,
     *0.28390D+01,0.29840D+01,0.31340D+01,0.32870D+01,0.34440D+01,
     *0.36050D+01,0.37690D+01,0.39380D+01,0.41060D+01,0.42860D+01,
     *0.44660D+01,0.46510D+01,0.48400D+01,0.50330D+01,0.52300D+01,
     *0.54320D+01,0.56360D+01,0.58460D+01,0.60580D+01,0.62730D+01,
     *0.64950D+01,0.67200D+01,0.69490D+01,0.71800D+01,0.74160D+01,
     *0.76560D+01,0.78990D+01,0.81460D+01,0.83980D+01,0.86520D+01,
     *0.89110D+01,0.91750D+01,0.94430D+01,0.97130D+01,0.99890D+01,
     *0.10268D+02,0.10551D+02,0.10839D+02,0.11130D+02,0.11426D+02,
     *0.11726D+02,0.12031D+02,0.12339D+02,0.12651D+02,0.12968D+02,
     *0.13291D+02,0.13618D+02,0.13946D+02,0.14282D+02,0.14620D+02,
     *0.14961D+02,0.15308D+02,0.15660D+02,0.16016D+02,0.16377D+02/
!--------------------------------------------------------------------!
!L-alpha2 photon energy in keV (M4--> L3 electron transitions).      !
!                (Table 7d Table of Isotopes Eighth Edition)         !
!--------------------------------------------------------------------!
      DATA (ELX3(2,IIZ),IIZ=1,100)/
     *0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,
     *0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,
     *0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,
     *0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,
     *0.39600D+00,0.45200D+00,0.51100D+00,0.57200D+00,0.63700D+00,
     *0.70400D+00,0.77600D+00,0.85100D+00,0.92900D+00,0.10120D+01,
     *0.10980D+01,0.11880D+01,0.12820D+01,0.13790D+01,0.14800D+01,
     *0.15800D+01,0.16930D+01,0.18050D+01,0.19200D+01,0.20400D+01,
     *0.21630D+01,0.22900D+01,0.24200D+01,0.25540D+01,0.26920D+01,
     *0.28330D+01,0.29780D+01,0.31270D+01,0.32790D+01,0.34350D+01,
     *0.35950D+01,0.37590D+01,0.39260D+01,0.40930D+01,0.42720D+01,
     *0.44510D+01,0.46340D+01,0.48220D+01,0.50130D+01,0.52080D+01,
     *0.54080D+01,0.56100D+01,0.58160D+01,0.60260D+01,0.62390D+01,
     *0.64580D+01,0.66800D+01,0.69050D+01,0.71330D+01,0.73670D+01,
     *0.76050D+01,0.78440D+01,0.80880D+01,0.83350D+01,0.85860D+01,
     *0.88400D+01,0.90990D+01,0.93620D+01,0.96280D+01,0.98990D+01,
     *0.10172D+02,0.10450D+02,0.10731D+02,0.11016D+02,0.11306D+02,
     *0.11598D+02,0.11896D+02,0.12196D+02,0.12500D+02,0.12809D+02,
     *0.13127D+02,0.13442D+02,0.13761D+02,0.14087D+02,0.14414D+02,
     *0.14746D+02,0.15082D+02,0.15423D+02,0.15767D+02,0.16116D+02/
!--------------------------------------------------------------------!
!L-Beta2  photon energy (N5 --> L 3 electron transition).            !
!                 (Table IV of Storm Israel)                         !
!--------------------------------------------------------------------!
      DATA (ELX3(3,IIZ),IIZ=1,100)/
     *0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,
     *0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,
     *0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,
     *0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,
     *0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,
     *0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,
     *0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,
     *0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,
     *0.00000D+00,0.00000D+00,0.00000D+00,0.28370D+01,0.30020D+01,
     *0.31710D+01,0.33460D+01,0.35280D+01,0.37140D+01,0.39050D+01,
     *0.41010D+01,0.43010D+01,0.45070D+01,0.47190D+01,0.49330D+01,
     *0.51550D+01,0.53850D+01,0.56180D+01,0.58520D+01,0.60910D+01,
     *0.63370D+01,0.65890D+01,0.68430D+01,0.71030D+01,0.73670D+01,
     *0.76360D+01,0.79110D+01,0.81890D+01,0.84710D+01,0.87550D+01,
     *0.90460D+01,0.93470D+01,0.96520D+01,0.99620D+01,0.10276D+02,
     *0.10597D+02,0.10922D+02,0.11251D+02,0.11585D+02,0.11923D+02,
     *0.12270D+02,0.12622D+02,0.12980D+02,0.13341D+02,0.13708D+02,
     *0.14078D+02,0.14454D+02,0.14836D+02,0.15226D+02,0.15624D+02,
     *0.16025D+02,0.16430D+02,0.16841D+02,0.17262D+02,0.17691D+02,
     *0.18129D+02,0.18575D+02,0.19027D+02,0.19487D+02,0.19955D+02/
!--------------------------------------------------------------------!
!L-beta5 photon energy in keV (O4O5--> L3 electron transitions).     !
!                (Table 7d Table of Isotopes Eighth Edition)         !
!--------------------------------------------------------------------!
      DATA (ELX3(4,IIZ),IIZ=1,100)/
     *0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,
     *0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,
     *0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,
     *0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,
     *0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,
     *0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,
     *0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,
     *0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,
     *0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,
     *0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,
     *0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,
     *0.00000D+00,0.54830D+01,0.00000D+00,0.00000D+00,0.00000D+00,
     *0.00000D+00,0.00000D+00,0.00000D+00,0.72430D+01,0.00000D+00,
     *0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,
     *0.92400D+01,0.95540D+01,0.98750D+01,0.10201D+02,0.10532D+02,
     *0.10871D+02,0.11211D+02,0.11562D+02,0.11916D+02,0.12275D+02,
     *0.12643D+02,0.13015D+02,0.13393D+02,0.13778D+02,0.14168D+02,
     *0.14565D+02,0.14967D+02,0.15375D+02,0.15790D+02,0.16209D+02,
     *0.16639D+02,0.17069D+02,0.17505D+02,0.17950D+02,0.18399D+02,
     *0.18853D+02,0.19312D+02,0.19777D+02,0.20249D+02,0.20727D+02/
!--------------------------------------------------------------------!
!L-beta6 photon energy in keV (N1--> L3 electron transitions).       !
!                (Table 7d Table of Isotopes Eighth Edition)         !
!--------------------------------------------------------------------!
      DATA (ELX3(5,IIZ),IIZ=1,100)/
     *0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,
     *0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,
     *0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,
     *0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,
     *0.40200D+00,0.45600D+00,0.51300D+00,0.57400D+00,0.64000D+00,
     *0.70800D+00,0.77900D+00,0.85500D+00,0.93100D+00,0.10200D+01,
     *0.11140D+01,0.12120D+01,0.13150D+01,0.14240D+01,0.15230D+01,
     *0.16470D+01,0.17750D+01,0.19020D+01,0.20350D+01,0.21710D+01,
     *0.23120D+01,0.24580D+01,0.26090D+01,0.27630D+01,0.29230D+01,
     *0.30870D+01,0.32560D+01,0.34300D+01,0.36080D+01,0.37920D+01,
     *0.39800D+01,0.41730D+01,0.43710D+01,0.45690D+01,0.47810D+01,
     *0.49940D+01,0.52120D+01,0.54340D+01,0.56600D+01,0.58930D+01,
     *0.61280D+01,0.63700D+01,0.66170D+01,0.68670D+01,0.71160D+01,
     *0.73740D+01,0.76350D+01,0.79090D+01,0.81760D+01,0.84560D+01,
     *0.87380D+01,0.90230D+01,0.93160D+01,0.96120D+01,0.99100D+01,
     *0.10217D+02,0.10525D+02,0.10840D+02,0.11160D+02,0.11481D+02,
     *0.11812D+02,0.12142D+02,0.12480D+02,0.12823D+02,0.13169D+02,
     *0.13520D+02,0.13877D+02,0.14236D+02,0.14601D+02,0.14970D+02,
     *0.15350D+02,0.15727D+02,0.16109D+02,0.16498D+02,0.16890D+02,
     *0.17286D+02,0.17687D+02,0.18094D+02,0.18501D+02,0.18916D+02/
!--------------------------------------------------------------------!
!L-Beta15  photon energy (N 4 --> L 3 electron transition).          !
!                 (Table IV of Storm Israel)                         !
!--------------------------------------------------------------------!
      DATA (ELX3(6,IIZ),IIZ=1,100)/
     *0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,
     *0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,
     *0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,
     *0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,
     *0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,
     *0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,
     *0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,
     *0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,
     *0.00000D+00,0.00000D+00,0.00000D+00,0.28370D+01,0.30020D+01,
     *0.31710D+01,0.33460D+01,0.35280D+01,0.37140D+01,0.39050D+01,
     *0.41010D+01,0.43010D+01,0.45070D+01,0.47190D+01,0.49330D+01,
     *0.51550D+01,0.53850D+01,0.56180D+01,0.58520D+01,0.60910D+01,
     *0.63370D+01,0.65890D+01,0.68430D+01,0.71030D+01,0.73660D+01,
     *0.76340D+01,0.79070D+01,0.81820D+01,0.84610D+01,0.87450D+01,
     *0.90350D+01,0.93360D+01,0.96400D+01,0.99480D+01,0.10261D+02,
     *0.10580D+02,0.10904D+02,0.11233D+02,0.11566D+02,0.11903D+02,
     *0.12249D+02,0.12600D+02,0.12956D+02,0.13316D+02,0.13681D+02,
     *0.14051D+02,0.14427D+02,0.14808D+02,0.15195D+02,0.15587D+02,
     *0.15987D+02,0.16390D+02,0.16797D+02,0.17214D+02,0.17638D+02,
     *0.18070D+02,0.18509D+02,0.18956D+02,0.19413D+02,0.19880D+02/
!--------------------------------------------------------------------!
!L-l     photon energy in keV (M1--> L3 electron transitions).       !
!                (Table 7d Table of Isotopes Eighth Edition)         !
!--------------------------------------------------------------------!
      DATA (ELX3(7,IIZ),IIZ=1,100)/
     *0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,
     *0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,
     *0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,
     *0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,
     *0.34800D+00,0.39500D+00,0.44600D+00,0.50000D+00,0.55600D+00,
     *0.61500D+00,0.67800D+00,0.74300D+00,0.81100D+00,0.88400D+00,
     *0.95700D+00,0.10370D+01,0.11200D+01,0.12040D+01,0.12930D+01,
     *0.13830D+01,0.14820D+01,0.15820D+01,0.16860D+01,0.17920D+01,
     *0.19020D+01,0.20160D+01,0.21330D+01,0.22530D+01,0.23770D+01,
     *0.25030D+01,0.26340D+01,0.27670D+01,0.29050D+01,0.30450D+01,
     *0.31890D+01,0.33350D+01,0.34850D+01,0.36340D+01,0.37950D+01,
     *0.39540D+01,0.41210D+01,0.42890D+01,0.44530D+01,0.46330D+01,
     *0.48090D+01,0.49930D+01,0.51770D+01,0.53620D+01,0.55460D+01,
     *0.57430D+01,0.59430D+01,0.61510D+01,0.63410D+01,0.65450D+01,
     *0.67530D+01,0.69600D+01,0.71730D+01,0.73870D+01,0.76040D+01,
     *0.78220D+01,0.80420D+01,0.82660D+01,0.84940D+01,0.87220D+01,
     *0.89530D+01,0.91840D+01,0.94200D+01,0.96580D+01,0.98970D+01,
     *0.10137D+02,0.10381D+02,0.10622D+02,0.10871D+02,0.11118D+02,
     *0.11372D+02,0.11620D+02,0.11871D+02,0.12124D+02,0.12377D+02,
     *0.12633D+02,0.12890D+02,0.13146D+02,0.13403D+02,0.13660D+02/
!---------------------------------------------------------------------!
!PM0(K) data ln(sigma)=PM0+PM1(ln(E))+PM2(ln(E))^2+PM3(ln(E))^3       !
!    E in keV.       PHOTX                                            !
!---------------------------------------------------------------------!
      DATA (PM0(1,IIZ),IIZ=1,100)/
     *0.23503D+01,0.59186D+01,0.78158D+01,0.90023D+01,0.98828D+01,
     *0.10544D+02,0.11135D+02,0.11622D+02,0.11956D+02,0.12313D+02,
     *0.12455D+02,0.12757D+02,0.12887D+02,0.13061D+02,0.13090D+02,
     *0.13258D+02,0.13425D+02,0.13300D+02,0.13440D+02,0.13481D+02,
     *0.13500D+02,0.13605D+02,0.13605D+02,0.13551D+02,0.13450D+02,
     *0.13389D+02,0.13289D+02,0.13146D+02,0.13054D+02,0.13322D+02,
     *0.13090D+02,0.13050D+02,0.13009D+02,0.12916D+02,0.12849D+02,
     *0.12747D+02,0.12564D+02,0.12525D+02,0.12439D+02,0.12282D+02,
     *0.12308D+02,0.12083D+02,0.11964D+02,0.11813D+02,0.11848D+02,
     *0.11666D+02,0.11473D+02,0.11371D+02,0.11333D+02,0.11312D+02,
     *0.10953D+02,0.10946D+02,0.10975D+02,0.10798D+02,0.10698D+02,
     *0.10542D+02,0.10363D+02,0.10659D+02,0.10452D+02,0.97405D+01,
     *0.98486D+01,0.10127D+02,0.10067D+02,0.10033D+02,0.98017D+01,
     *0.92128D+01,0.93958D+01,0.98396D+01,0.10082D+02,0.95777D+01,
     *0.91113D+01,0.94043D+01,0.92843D+01,0.94191D+01,0.92342D+01,
     *0.99658D+01,0.95330D+01,0.90402D+01,0.93968D+01,0.98871D+01,
     *0.87164D+01,0.92839D+01,0.10329D+02,0.85480D+01,0.95395D+01,
     *0.84097D+01,0.97627D+01,0.98375D+01,0.88785D+01,0.93921D+01,
     *0.10857D+02,0.10013D+02,0.93384D+01,0.12049D+02,0.11875D+02,
     *0.11779D+02,0.10028D+02,0.12059D+02,0.11001D+02,0.12996D+02/
!---------------------------------------------------------------------!
!PM1(K) data ln(sigma)=PM0+PM1(ln(E))+PM2(ln(E))^2+PM3(ln(E))^3       !
!    E in keV.       PHOTX                                            !
!---------------------------------------------------------------------!
      DATA (PM1(1,IIZ),IIZ=1,100)/
     *-.31059D+01,-.29804D+01,-.28924D+01,-.27639D+01,-.26731D+01,
     *-.25613D+01,-.25218D+01,-.24713D+01,-.23473D+01,-.23130D+01,
     *-.20992D+01,-.20880D+01,-.19430D+01,-.18677D+01,-.16778D+01,
     *-.16365D+01,-.16087D+01,-.13585D+01,-.13201D+01,-.12258D+01,
     *-.11165D+01,-.10928D+01,-.98174D+00,-.84665D+00,-.69379D+00,
     *-.57157D+00,-.42405D+00,-.25841D+00,-.13131D+00,-.24491D+00,
     *-.32034D-01,0.49935D-01,0.13418D+00,0.24545D+00,0.33295D+00,
     *0.44300D+00,0.60430D+00,0.67230D+00,0.76539D+00,0.89714D+00,
     *0.90761D+00,0.10741D+01,0.11914D+01,0.13022D+01,0.13122D+01,
     *0.14535D+01,0.15952D+01,0.16784D+01,0.17260D+01,0.17613D+01,
     *0.19957D+01,0.20214D+01,0.20250D+01,0.21421D+01,0.22179D+01,
     *0.23294D+01,0.24527D+01,0.22968D+01,0.24320D+01,0.28436D+01,
     *0.27891D+01,0.26450D+01,0.26979D+01,0.27274D+01,0.28654D+01,
     *0.32023D+01,0.31222D+01,0.28678D+01,0.27627D+01,0.30374D+01,
     *0.33001D+01,0.31539D+01,0.32212D+01,0.31421D+01,0.32704D+01,
     *0.28559D+01,0.31065D+01,0.33796D+01,0.31784D+01,0.29445D+01,
     *0.35605D+01,0.32244D+01,0.26967D+01,0.36454D+01,0.31307D+01,
     *0.36996D+01,0.29753D+01,0.29692D+01,0.34306D+01,0.31771D+01,
     *0.23868D+01,0.28464D+01,0.31651D+01,0.17785D+01,0.18523D+01,
     *0.18955D+01,0.27876D+01,0.17598D+01,0.23171D+01,0.12794D+01/
!---------------------------------------------------------------------!
!PM2(K) data ln(sigma)=PM0+PM1(ln(E))+PM2(ln(E))^2+PM3(ln(E))^3       !
!    E in keV.       PHOTX                                            !
!---------------------------------------------------------------------!
      DATA (PM2(1,IIZ),IIZ=1,100)/
     *-.15245D+00,-.17455D+00,-.17914D+00,-.20402D+00,-.21555D+00,
     *-.23378D+00,-.23297D+00,-.23674D+00,-.26026D+00,-.25889D+00,
     *-.30863D+00,-.30218D+00,-.33217D+00,-.34302D+00,-.38528D+00,
     *-.38825D+00,-.38823D+00,-.44204D+00,-.44625D+00,-.46250D+00,
     *-.48327D+00,-.48205D+00,-.50375D+00,-.52945D+00,-.55758D+00,
     *-.57941D+00,-.60728D+00,-.63802D+00,-.66035D+00,-.63224D+00,
     *-.67266D+00,-.68556D+00,-.69935D+00,-.71873D+00,-.73194D+00,
     *-.75034D+00,-.78007D+00,-.79046D+00,-.80569D+00,-.82737D+00,
     *-.82446D+00,-.85315D+00,-.87451D+00,-.89057D+00,-.88985D+00,
     *-.91453D+00,-.93850D+00,-.95088D+00,-.95679D+00,-.95992D+00,
     *-.10016D+01,-.10037D+01,-.10012D+01,-.10197D+01,-.10303D+01,
     *-.10489D+01,-.10694D+01,-.10371D+01,-.10596D+01,-.11315D+01,
     *-.11171D+01,-.10877D+01,-.10961D+01,-.10984D+01,-.11200D+01,
     *-.11782D+01,-.11630D+01,-.11105D+01,-.10923D+01,-.11372D+01,
     *-.11814D+01,-.11535D+01,-.11621D+01,-.11435D+01,-.11670D+01,
     *-.10865D+01,-.11302D+01,-.11766D+01,-.11358D+01,-.10966D+01,
     *-.11996D+01,-.11319D+01,-.10419D+01,-.12049D+01,-.11138D+01,
     *-.12058D+01,-.10756D+01,-.10774D+01,-.11473D+01,-.11037D+01,
     *-.96128D+00,-.10409D+01,-.10883D+01,-.85188D+00,-.86017D+00,
     *-.86388D+00,-.10126D+01,-.83818D+00,-.93409D+00,-.75389D+00/
!---------------------------------------------------------------------!
!PM3(K) data ln(sigma)=PM0+PM1(ln(E))+PM2(ln(E))^2+PM3(ln(E))^3       !
!    E in keV.       PHOTX                                            !
!---------------------------------------------------------------------!
      DATA (PM3(1,IIZ),IIZ=1,100)/
     *0.18812D-01,0.20159D-01,0.19714D-01,0.21563D-01,0.21994D-01,
     *0.23017D-01,0.22575D-01,0.22577D-01,0.24150D-01,0.23683D-01,
     *0.27447D-01,0.26617D-01,0.28739D-01,0.29277D-01,0.32391D-01,
     *0.32372D-01,0.32159D-01,0.35948D-01,0.36114D-01,0.37110D-01,
     *0.38480D-01,0.38151D-01,0.39612D-01,0.41283D-01,0.43049D-01,
     *0.44403D-01,0.46204D-01,0.48134D-01,0.49488D-01,0.47438D-01,
     *0.50015D-01,0.50735D-01,0.51522D-01,0.52693D-01,0.53382D-01,
     *0.54440D-01,0.56323D-01,0.56889D-01,0.57770D-01,0.58965D-01,
     *0.58580D-01,0.60270D-01,0.61595D-01,0.62365D-01,0.62251D-01,
     *0.63722D-01,0.65107D-01,0.65755D-01,0.66008D-01,0.66060D-01,
     *0.68553D-01,0.68606D-01,0.68326D-01,0.69346D-01,0.69836D-01,
     *0.70912D-01,0.72084D-01,0.70010D-01,0.71297D-01,0.75468D-01,
     *0.74424D-01,0.72560D-01,0.73054D-01,0.73087D-01,0.74242D-01,
     *0.77596D-01,0.76713D-01,0.73295D-01,0.72327D-01,0.74791D-01,
     *0.77294D-01,0.75610D-01,0.75995D-01,0.74732D-01,0.76197D-01,
     *0.71160D-01,0.73726D-01,0.76398D-01,0.73772D-01,0.71703D-01,
     *0.77400D-01,0.73078D-01,0.68115D-01,0.77397D-01,0.72134D-01,
     *0.77104D-01,0.69480D-01,0.69801D-01,0.73299D-01,0.70900D-01,
     *0.62526D-01,0.67140D-01,0.69502D-01,0.56244D-01,0.56570D-01,
     *0.56627D-01,0.64918D-01,0.55164D-01,0.60736D-01,0.50461D-01/
!---------------------------------------------------------------------!
!PM0(L1) data ln(sigma)=PM0+PM1(ln(E))+PM2(ln(E))^2+PM3(ln(E))^3      !
!    E in keV.       PHOTX                                            !
!---------------------------------------------------------------------!
      DATA (PM0(2,IIZ),IIZ=1,100)/
     *0.00000D+00,0.00000D+00,0.35463D+01,0.56873D+01,0.67570D+01,
     *0.74942D+01,0.80556D+01,0.85054D+01,0.88830D+01,0.91951D+01,
     *0.95599D+01,0.98206D+01,0.10048D+02,0.10244D+02,0.10433D+02,
     *0.10619D+02,0.10731D+02,0.10919D+02,0.11018D+02,0.11146D+02,
     *0.11243D+02,0.11335D+02,0.11428D+02,0.11466D+02,0.11523D+02,
     *0.11618D+02,0.11657D+02,0.11724D+02,0.11739D+02,0.11783D+02,
     *0.11763D+02,0.11824D+02,0.11713D+02,0.11745D+02,0.11800D+02,
     *0.11809D+02,0.11847D+02,0.11778D+02,0.11694D+02,0.11709D+02,
     *0.11716D+02,0.11749D+02,0.11550D+02,0.11672D+02,0.11555D+02,
     *0.11570D+02,0.11581D+02,0.11516D+02,0.11489D+02,0.11525D+02,
     *0.11376D+02,0.11385D+02,0.11265D+02,0.11308D+02,0.11172D+02,
     *0.11164D+02,0.11049D+02,0.10987D+02,0.10972D+02,0.10876D+02,
     *0.10865D+02,0.10816D+02,0.10697D+02,0.10672D+02,0.10550D+02,
     *0.10490D+02,0.10438D+02,0.10386D+02,0.10282D+02,0.10190D+02,
     *0.10196D+02,0.10060D+02,0.10061D+02,0.99774D+01,0.98548D+01,
     *0.97760D+01,0.97083D+01,0.96183D+01,0.96124D+01,0.94898D+01,
     *0.94345D+01,0.93071D+01,0.92491D+01,0.91799D+01,0.90150D+01,
     *0.89785D+01,0.89466D+01,0.88739D+01,0.85317D+01,0.85761D+01,
     *0.84456D+01,0.84186D+01,0.83789D+01,0.83830D+01,0.82625D+01,
     *0.80395D+01,0.78077D+01,0.77794D+01,0.78400D+01,0.77913D+01/
!---------------------------------------------------------------------!
!PM1(L1) data ln(sigma)=PM0+PM1(ln(E))+PM2(ln(E))^2+PM3(ln(E))^3      !
!    E in keV.       PHOTX                                            !
!---------------------------------------------------------------------!
      DATA (PM1(2,IIZ),IIZ=1,100)/
     *0.00000D+00,0.00000D+00,-.28513D+01,-.27067D+01,-.25990D+01,
     *-.24934D+01,-.23968D+01,-.23127D+01,-.22355D+01,-.21556D+01,
     *-.21305D+01,-.20389D+01,-.19553D+01,-.18748D+01,-.18187D+01,
     *-.17856D+01,-.16963D+01,-.17045D+01,-.16341D+01,-.16037D+01,
     *-.15548D+01,-.15185D+01,-.14857D+01,-.14096D+01,-.13625D+01,
     *-.13537D+01,-.12935D+01,-.12732D+01,-.12081D+01,-.11748D+01,
     *-.10901D+01,-.10730D+01,-.91934D+00,-.88193D+00,-.87410D+00,
     *-.83465D+00,-.81803D+00,-.71390D+00,-.61017D+00,-.57284D+00,
     *-.54839D+00,-.53175D+00,-.34314D+00,-.40341D+00,-.28079D+00,
     *-.26583D+00,-.24905D+00,-.17203D+00,-.12368D+00,-.13033D+00,
     *0.37320D-02,0.18208D-01,0.12559D+00,0.11527D+00,0.22646D+00,
     *0.25232D+00,0.34817D+00,0.40626D+00,0.43045D+00,0.51168D+00,
     *0.52998D+00,0.57802D+00,0.66781D+00,0.68754D+00,0.78272D+00,
     *0.83196D+00,0.87792D+00,0.92117D+00,0.98983D+00,0.10583D+01,
     *0.10550D+01,0.11539D+01,0.11545D+01,0.12142D+01,0.13032D+01,
     *0.13531D+01,0.13930D+01,0.14562D+01,0.14613D+01,0.15428D+01,
     *0.15701D+01,0.16516D+01,0.16880D+01,0.17258D+01,0.18248D+01,
     *0.18473D+01,0.18645D+01,0.19079D+01,0.21079D+01,0.20813D+01,
     *0.21587D+01,0.21682D+01,0.21833D+01,0.21772D+01,0.22371D+01,
     *0.23716D+01,0.25036D+01,0.25040D+01,0.24571D+01,0.24777D+01/
!---------------------------------------------------------------------!
!PM2(L1) data ln(sigma)=PM0+PM1(ln(E))+PM2(ln(E))^2+PM3(ln(E))^3      !
!    E in keV.       PHOTX                                            !
!---------------------------------------------------------------------!
      DATA (PM2(2,IIZ),IIZ=1,100)/
     *0.00000D+00,0.00000D+00,-.19110D+00,-.21767D+00,-.23208D+00,
     *-.24834D+00,-.26111D+00,-.27000D+00,-.27934D+00,-.29030D+00,
     *-.28349D+00,-.29780D+00,-.30992D+00,-.32167D+00,-.32616D+00,
     *-.32475D+00,-.33979D+00,-.32749D+00,-.33744D+00,-.33756D+00,
     *-.34240D+00,-.34323D+00,-.34435D+00,-.35736D+00,-.36211D+00,
     *-.35773D+00,-.36741D+00,-.36585D+00,-.37651D+00,-.37870D+00,
     *-.39414D+00,-.39337D+00,-.42623D+00,-.43148D+00,-.42813D+00,
     *-.43198D+00,-.43070D+00,-.45184D+00,-.47072D+00,-.47651D+00,
     *-.47641D+00,-.47656D+00,-.51656D+00,-.49810D+00,-.52342D+00,
     *-.52141D+00,-.52061D+00,-.53494D+00,-.54216D+00,-.53601D+00,
     *-.56271D+00,-.56208D+00,-.58218D+00,-.57603D+00,-.59623D+00,
     *-.59880D+00,-.61609D+00,-.62495D+00,-.62661D+00,-.64071D+00,
     *-.64046D+00,-.64767D+00,-.66301D+00,-.66204D+00,-.67964D+00,
     *-.68661D+00,-.69364D+00,-.69920D+00,-.70876D+00,-.71996D+00,
     *-.71481D+00,-.73294D+00,-.72882D+00,-.73815D+00,-.75434D+00,
     *-.76029D+00,-.76434D+00,-.77440D+00,-.77208D+00,-.78567D+00,
     *-.78671D+00,-.79980D+00,-.80429D+00,-.80776D+00,-.82377D+00,
     *-.82551D+00,-.82566D+00,-.83102D+00,-.86646D+00,-.85858D+00,
     *-.87103D+00,-.86952D+00,-.86831D+00,-.86443D+00,-.87121D+00,
     *-.89556D+00,-.91807D+00,-.91327D+00,-.90049D+00,-.90146D+00/
!---------------------------------------------------------------------!
!PM3(L1) data ln(sigma)=PM0+PM1(ln(E))+PM2(ln(E))^2+PM3(ln(E))^3      !
!    E in keV.       PHOTX                                            !
!---------------------------------------------------------------------!
      DATA (PM3(2,IIZ),IIZ=1,100)/
     *0.00000D+00,0.00000D+00,0.20752D-01,0.22531D-01,0.23130D-01,
     *0.24086D-01,0.24632D-01,0.24841D-01,0.25244D-01,0.25859D-01,
     *0.24746D-01,0.25578D-01,0.26226D-01,0.26867D-01,0.26867D-01,
     *0.26423D-01,0.27361D-01,0.26050D-01,0.26563D-01,0.26350D-01,
     *0.26483D-01,0.26267D-01,0.26142D-01,0.26975D-01,0.27123D-01,
     *0.26596D-01,0.27194D-01,0.26879D-01,0.27541D-01,0.27538D-01,
     *0.28553D-01,0.28371D-01,0.30726D-01,0.31056D-01,0.30655D-01,
     *0.30764D-01,0.30513D-01,0.32019D-01,0.33214D-01,0.33604D-01,
     *0.33403D-01,0.33330D-01,0.36139D-01,0.34677D-01,0.36485D-01,
     *0.36131D-01,0.35937D-01,0.36911D-01,0.37320D-01,0.36747D-01,
     *0.38549D-01,0.38402D-01,0.39713D-01,0.39170D-01,0.40437D-01,
     *0.40567D-01,0.41664D-01,0.42165D-01,0.42193D-01,0.43061D-01,
     *0.42904D-01,0.43332D-01,0.44256D-01,0.43987D-01,0.45150D-01,
     *0.45526D-01,0.45961D-01,0.46227D-01,0.46687D-01,0.47363D-01,
     *0.46859D-01,0.48032D-01,0.47611D-01,0.48163D-01,0.49207D-01,
     *0.49447D-01,0.49585D-01,0.50176D-01,0.49931D-01,0.50743D-01,
     *0.50663D-01,0.51409D-01,0.51646D-01,0.51736D-01,0.52647D-01,
     *0.52708D-01,0.52626D-01,0.52876D-01,0.55019D-01,0.54460D-01,
     *0.55192D-01,0.55013D-01,0.54789D-01,0.54496D-01,0.54734D-01,
     *0.56277D-01,0.57619D-01,0.57144D-01,0.56234D-01,0.56230D-01/
!---------------------------------------------------------------------!
!PM0(L2) data ln(sigma)=PM0+PM1(ln(E))+PM2(ln(E))^2+PM3(ln(E))^3      !
!    E in keV.       PHOTX                                            !
!---------------------------------------------------------------------!
      DATA (PM0(3,IIZ),IIZ=1,100)/
     *0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,0.13821D+01,
     *0.33896D+01,0.47594D+01,0.58136D+01,0.67001D+01,0.74415D+01,
     *0.80774D+01,0.85726D+01,0.90111D+01,0.93997D+01,0.97523D+01,
     *0.10096D+02,0.10390D+02,0.10626D+02,0.10900D+02,0.11104D+02,
     *0.11337D+02,0.11535D+02,0.11726D+02,0.11909D+02,0.12049D+02,
     *0.12209D+02,0.12350D+02,0.12507D+02,0.12676D+02,0.12747D+02,
     *0.12840D+02,0.12926D+02,0.12940D+02,0.13075D+02,0.13127D+02,
     *0.13234D+02,0.13236D+02,0.13359D+02,0.13381D+02,0.13385D+02,
     *0.13456D+02,0.13562D+02,0.13500D+02,0.13500D+02,0.13656D+02,
     *0.13605D+02,0.13494D+02,0.13674D+02,0.13548D+02,0.13707D+02,
     *0.13549D+02,0.13545D+02,0.13659D+02,0.13490D+02,0.13639D+02,
     *0.13520D+02,0.13723D+02,0.13479D+02,0.13676D+02,0.13590D+02,
     *0.13634D+02,0.13470D+02,0.13547D+02,0.13770D+02,0.13454D+02,
     *0.13637D+02,0.13301D+02,0.13460D+02,0.13565D+02,0.13758D+02,
     *0.13367D+02,0.13374D+02,0.13337D+02,0.13345D+02,0.13333D+02,
     *0.13279D+02,0.13270D+02,0.13223D+02,0.13111D+02,0.13154D+02,
     *0.13074D+02,0.13010D+02,0.13078D+02,0.12982D+02,0.12967D+02,
     *0.12884D+02,0.12742D+02,0.12729D+02,0.12735D+02,0.12895D+02,
     *0.12662D+02,0.12537D+02,0.12537D+02,0.12360D+02,0.12267D+02,
     *0.12201D+02,0.12104D+02,0.12183D+02,0.11773D+02,0.11746D+02/
!---------------------------------------------------------------------!
!PM1(L2) data ln(sigma)=PM0+PM1(ln(E))+PM2(ln(E))^2+PM3(ln(E))^3      !
!    E in keV.       PHOTX                                            !
!---------------------------------------------------------------------!
      DATA (PM1(3,IIZ),IIZ=1,100)/
     *0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,-.35617D+01,
     *-.35058D+01,-.33869D+01,-.32670D+01,-.31941D+01,-.31058D+01,
     *-.30682D+01,-.29758D+01,-.29020D+01,-.28289D+01,-.27709D+01,
     *-.27453D+01,-.26947D+01,-.26148D+01,-.26049D+01,-.25321D+01,
     *-.25064D+01,-.24688D+01,-.24286D+01,-.24069D+01,-.23461D+01,
     *-.23161D+01,-.22695D+01,-.22745D+01,-.22717D+01,-.21959D+01,
     *-.21373D+01,-.20826D+01,-.19715D+01,-.19834D+01,-.19204D+01,
     *-.19000D+01,-.18024D+01,-.18073D+01,-.17365D+01,-.16514D+01,
     *-.16312D+01,-.16356D+01,-.15089D+01,-.14332D+01,-.14817D+01,
     *-.13882D+01,-.12375D+01,-.13094D+01,-.11503D+01,-.12145D+01,
     *-.10410D+01,-.99464D+00,-.10242D+01,-.84893D+00,-.91523D+00,
     *-.78576D+00,-.88110D+00,-.67059D+00,-.76315D+00,-.67436D+00,
     *-.65696D+00,-.51346D+00,-.52713D+00,-.63746D+00,-.40545D+00,
     *-.48873D+00,-.24081D+00,-.31210D+00,-.35384D+00,-.45341D+00,
     *-.16488D+00,-.14737D+00,-.99714D-01,-.86619D-01,-.56038D-01,
     *0.50655D-02,0.26636D-01,0.83624D-01,0.17797D+00,0.16449D+00,
     *0.24079D+00,0.29658D+00,0.26312D+00,0.34271D+00,0.36754D+00,
     *0.42703D+00,0.53254D+00,0.55526D+00,0.56306D+00,0.47503D+00,
     *0.63074D+00,0.71444D+00,0.72238D+00,0.84501D+00,0.89911D+00,
     *0.93728D+00,0.10088D+01,0.95997D+00,0.12155D+01,0.12368D+01/
!---------------------------------------------------------------------!
!PM2(L2) data ln(sigma)=PM0+PM1(ln(E))+PM2(ln(E))^2+PM3(ln(E))^3      !
!    E in keV.       PHOTX                                            !
!---------------------------------------------------------------------!
      DATA (PM2(3,IIZ),IIZ=1,100)/
     *0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,-.25644D+00,
     *-.25856D+00,-.27734D+00,-.29934D+00,-.30547D+00,-.31942D+00,
     *-.31547D+00,-.33010D+00,-.33816D+00,-.34805D+00,-.35271D+00,
     *-.34823D+00,-.35230D+00,-.36439D+00,-.35647D+00,-.36762D+00,
     *-.36627D+00,-.36682D+00,-.37173D+00,-.36782D+00,-.37698D+00,
     *-.37808D+00,-.38578D+00,-.37300D+00,-.36824D+00,-.38108D+00,
     *-.39159D+00,-.39972D+00,-.42279D+00,-.41241D+00,-.42250D+00,
     *-.42311D+00,-.44225D+00,-.43633D+00,-.44840D+00,-.46454D+00,
     *-.46354D+00,-.45800D+00,-.48389D+00,-.49774D+00,-.48157D+00,
     *-.49712D+00,-.52876D+00,-.50763D+00,-.54094D+00,-.52120D+00,
     *-.55702D+00,-.56169D+00,-.55121D+00,-.58727D+00,-.56696D+00,
     *-.59167D+00,-.56703D+00,-.60886D+00,-.58513D+00,-.59908D+00,
     *-.60032D+00,-.62653D+00,-.61977D+00,-.59318D+00,-.63640D+00,
     *-.61595D+00,-.66368D+00,-.64541D+00,-.63245D+00,-.60725D+00,
     *-.66500D+00,-.66398D+00,-.66949D+00,-.66749D+00,-.66966D+00,
     *-.67879D+00,-.67878D+00,-.68751D+00,-.70329D+00,-.69557D+00,
     *-.70842D+00,-.71554D+00,-.70350D+00,-.71644D+00,-.71730D+00,
     *-.72431D+00,-.74140D+00,-.74229D+00,-.73967D+00,-.71750D+00,
     *-.74456D+00,-.75651D+00,-.75370D+00,-.77413D+00,-.77929D+00,
     *-.78054D+00,-.79150D+00,-.77635D+00,-.82233D+00,-.82209D+00/
!---------------------------------------------------------------------!
!PM3(L2) data ln(sigma)=PM0+PM1(ln(E))+PM2(ln(E))^2+PM3(ln(E))^3      !
!    E in keV.       PHOTX                                            !
!---------------------------------------------------------------------!
      DATA (PM3(3,IIZ),IIZ=1,100)/
     *0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,0.31011D-01,
     *0.30980D-01,0.31890D-01,0.33427D-01,0.33519D-01,0.34431D-01,
     *0.33584D-01,0.34516D-01,0.34797D-01,0.35331D-01,0.35367D-01,
     *0.34640D-01,0.34658D-01,0.35380D-01,0.34374D-01,0.35044D-01,
     *0.34738D-01,0.34446D-01,0.34824D-01,0.34156D-01,0.34711D-01,
     *0.34658D-01,0.35260D-01,0.33755D-01,0.33279D-01,0.34091D-01,
     *0.34852D-01,0.35330D-01,0.37005D-01,0.35971D-01,0.36611D-01,
     *0.36558D-01,0.37912D-01,0.37366D-01,0.38141D-01,0.39251D-01,
     *0.39010D-01,0.38509D-01,0.40323D-01,0.41260D-01,0.39926D-01,
     *0.40867D-01,0.43136D-01,0.41483D-01,0.43833D-01,0.42256D-01,
     *0.44754D-01,0.44898D-01,0.44055D-01,0.46565D-01,0.44945D-01,
     *0.46586D-01,0.44773D-01,0.47568D-01,0.45838D-01,0.46642D-01,
     *0.46697D-01,0.48355D-01,0.47766D-01,0.45897D-01,0.48608D-01,
     *0.47181D-01,0.50267D-01,0.48962D-01,0.47956D-01,0.46117D-01,
     *0.49982D-01,0.49752D-01,0.49969D-01,0.49686D-01,0.49697D-01,
     *0.50204D-01,0.50082D-01,0.50607D-01,0.51547D-01,0.50865D-01,
     *0.51664D-01,0.52002D-01,0.51032D-01,0.51821D-01,0.51748D-01,
     *0.52039D-01,0.53003D-01,0.52971D-01,0.52680D-01,0.51078D-01,
     *0.52714D-01,0.53341D-01,0.53019D-01,0.54208D-01,0.54361D-01,
     *0.54193D-01,0.54834D-01,0.53670D-01,0.56477D-01,0.56332D-01/
!---------------------------------------------------------------------!
!PM0(L3) data ln(sigma)=PM0+PM1(ln(E))+PM2(ln(E))^2+PM3(ln(E))^3      !
!    E in keV.       PHOTX                                            !
!---------------------------------------------------------------------!
      DATA (PM0(4,IIZ),IIZ=1,100)/
     *0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,0.20442D+01,
     *0.40374D+01,0.54086D+01,0.64706D+01,0.73510D+01,0.80867D+01,
     *0.87367D+01,0.92219D+01,0.96590D+01,0.10061D+02,0.10388D+02,
     *0.10700D+02,0.10992D+02,0.11283D+02,0.11518D+02,0.11771D+02,
     *0.11984D+02,0.12165D+02,0.12363D+02,0.12501D+02,0.12696D+02,
     *0.12838D+02,0.12976D+02,0.13174D+02,0.13288D+02,0.13310D+02,
     *0.13409D+02,0.13485D+02,0.13634D+02,0.13741D+02,0.13824D+02,
     *0.13903D+02,0.13882D+02,0.13926D+02,0.13950D+02,0.14092D+02,
     *0.14086D+02,0.14068D+02,0.14112D+02,0.14209D+02,0.14158D+02,
     *0.14070D+02,0.14273D+02,0.14205D+02,0.14146D+02,0.14112D+02,
     *0.14199D+02,0.14085D+02,0.14171D+02,0.14183D+02,0.14220D+02,
     *0.14296D+02,0.14143D+02,0.14100D+02,0.14221D+02,0.14069D+02,
     *0.14124D+02,0.14038D+02,0.14094D+02,0.14149D+02,0.14109D+02,
     *0.14055D+02,0.13985D+02,0.13977D+02,0.14084D+02,0.13818D+02,
     *0.14142D+02,0.13701D+02,0.14074D+02,0.13811D+02,0.13851D+02,
     *0.13667D+02,0.14047D+02,0.13768D+02,0.13817D+02,0.14041D+02,
     *0.13672D+02,0.13788D+02,0.14060D+02,0.13684D+02,0.14024D+02,
     *0.13979D+02,0.13582D+02,0.13718D+02,0.13964D+02,0.13499D+02,
     *0.13693D+02,0.13796D+02,0.13461D+02,0.13514D+02,0.13752D+02,
     *0.13864D+02,0.13715D+02,0.13511D+02,0.13692D+02,0.13694D+02/
!---------------------------------------------------------------------!
!PM1(L3) data ln(sigma)=PM0+PM1(ln(E))+PM2(ln(E))^2+PM3(ln(E))^3      !
!    E in keV.       PHOTX                                            !
!---------------------------------------------------------------------!
      DATA (PM1(4,IIZ),IIZ=1,100)/
     *0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,-.34856D+01,
     *-.34126D+01,-.32997D+01,-.31983D+01,-.31150D+01,-.30191D+01,
     *-.29889D+01,-.28921D+01,-.28261D+01,-.27724D+01,-.26891D+01,
     *-.26321D+01,-.25829D+01,-.25690D+01,-.25112D+01,-.24956D+01,
     *-.24603D+01,-.23970D+01,-.23770D+01,-.23052D+01,-.22970D+01,
     *-.22497D+01,-.22246D+01,-.22451D+01,-.22018D+01,-.20700D+01,
     *-.20187D+01,-.19639D+01,-.19772D+01,-.19640D+01,-.19226D+01,
     *-.18811D+01,-.17605D+01,-.17113D+01,-.16436D+01,-.16619D+01,
     *-.15703D+01,-.14762D+01,-.14445D+01,-.14572D+01,-.13408D+01,
     *-.12001D+01,-.12967D+01,-.11760D+01,-.10683D+01,-.98707D+00,
     *-.10069D+01,-.86369D+00,-.86616D+00,-.83844D+00,-.81524D+00,
     *-.82505D+00,-.67470D+00,-.60223D+00,-.64735D+00,-.48848D+00,
     *-.49811D+00,-.38990D+00,-.41306D+00,-.40579D+00,-.33885D+00,
     *-.27615D+00,-.19451D+00,-.16678D+00,-.20921D+00,0.10563D-02,
     *-.17655D+00,0.12974D+00,-.78882D-01,0.10395D+00,0.11945D+00,
     *0.24121D+00,0.26119D-01,0.23479D+00,0.22300D+00,0.10335D+00,
     *0.35660D+00,0.29894D+00,0.14711D+00,0.40559D+00,0.20891D+00,
     *0.25216D+00,0.52388D+00,0.45453D+00,0.31014D+00,0.62477D+00,
     *0.51287D+00,0.46837D+00,0.69064D+00,0.67768D+00,0.53397D+00,
     *0.47927D+00,0.59476D+00,0.72818D+00,0.63096D+00,0.64353D+00/
!---------------------------------------------------------------------!
!PM2(L3) data ln(sigma)=PM0+PM1(ln(E))+PM2(ln(E))^2+PM3(ln(E))^3      !
!    E in keV.       PHOTX                                            !
!---------------------------------------------------------------------!
      DATA (PM2(4,IIZ),IIZ=1,100)/
     *0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,-.30150D+00,
     *-.30367D+00,-.32203D+00,-.33559D+00,-.34585D+00,-.36430D+00,
     *-.36037D+00,-.37458D+00,-.37767D+00,-.38149D+00,-.39497D+00,
     *-.39849D+00,-.40248D+00,-.39448D+00,-.40152D+00,-.39781D+00,
     *-.39622D+00,-.40828D+00,-.40419D+00,-.41624D+00,-.41173D+00,
     *-.41777D+00,-.41530D+00,-.40392D+00,-.40851D+00,-.43897D+00,
     *-.44728D+00,-.45502D+00,-.44569D+00,-.44261D+00,-.45095D+00,
     *-.45480D+00,-.48046D+00,-.48698D+00,-.49833D+00,-.49109D+00,
     *-.50965D+00,-.52850D+00,-.53016D+00,-.52151D+00,-.54644D+00,
     *-.57688D+00,-.54885D+00,-.57427D+00,-.59641D+00,-.61118D+00,
     *-.60102D+00,-.63139D+00,-.62857D+00,-.62945D+00,-.63141D+00,
     *-.62526D+00,-.65498D+00,-.66731D+00,-.65367D+00,-.68746D+00,
     *-.68062D+00,-.70254D+00,-.69058D+00,-.69075D+00,-.70263D+00,
     *-.71167D+00,-.72693D+00,-.72786D+00,-.71544D+00,-.75801D+00,
     *-.71850D+00,-.77766D+00,-.73308D+00,-.76585D+00,-.76840D+00,
     *-.78732D+00,-.74082D+00,-.78206D+00,-.77632D+00,-.74915D+00,
     *-.79773D+00,-.78192D+00,-.74819D+00,-.79837D+00,-.75562D+00,
     *-.76100D+00,-.81379D+00,-.79711D+00,-.76356D+00,-.82557D+00,
     *-.79921D+00,-.78861D+00,-.83044D+00,-.82568D+00,-.79224D+00,
     *-.77902D+00,-.80143D+00,-.82436D+00,-.80273D+00,-.80263D+00/
!---------------------------------------------------------------------!
!PM3(L3) data ln(sigma)=PM0+PM1(ln(E))+PM2(ln(E))^2+PM3(ln(E))^3      !
!    E in keV.       PHOTX                                            !
!---------------------------------------------------------------------!
      DATA (PM3(4,IIZ),IIZ=1,100)/
     *0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,0.36505D-01,
     *0.35766D-01,0.36752D-01,0.37308D-01,0.37717D-01,0.39232D-01,
     *0.38459D-01,0.39142D-01,0.38715D-01,0.38695D-01,0.39669D-01,
     *0.39422D-01,0.39513D-01,0.38359D-01,0.38673D-01,0.38241D-01,
     *0.37617D-01,0.38618D-01,0.37932D-01,0.38685D-01,0.38143D-01,
     *0.38437D-01,0.37922D-01,0.36848D-01,0.36996D-01,0.39348D-01,
     *0.39870D-01,0.40276D-01,0.39371D-01,0.38934D-01,0.39645D-01,
     *0.39646D-01,0.41482D-01,0.41814D-01,0.42492D-01,0.41915D-01,
     *0.43209D-01,0.44524D-01,0.44426D-01,0.43595D-01,0.45394D-01,
     *0.47585D-01,0.45326D-01,0.47114D-01,0.48682D-01,0.49614D-01,
     *0.48665D-01,0.50808D-01,0.50560D-01,0.50434D-01,0.50462D-01,
     *0.49889D-01,0.51858D-01,0.52596D-01,0.51513D-01,0.53881D-01,
     *0.53236D-01,0.54727D-01,0.53578D-01,0.53593D-01,0.54330D-01,
     *0.54776D-01,0.55774D-01,0.55618D-01,0.54652D-01,0.57496D-01,
     *0.54756D-01,0.58511D-01,0.55554D-01,0.57513D-01,0.57689D-01,
     *0.58663D-01,0.55502D-01,0.58197D-01,0.57692D-01,0.55802D-01,
     *0.58884D-01,0.57656D-01,0.55331D-01,0.58548D-01,0.55648D-01,
     *0.55854D-01,0.59226D-01,0.58054D-01,0.55650D-01,0.59661D-01,
     *0.57774D-01,0.57055D-01,0.59665D-01,0.59265D-01,0.56870D-01,
     *0.55941D-01,0.57407D-01,0.58729D-01,0.57242D-01,0.57133D-01/
!---------------------------------------------------------------------!
!PM0(M) data ln(sigma)=PM0+PM1(ln(E))+PM2(ln(E))^2+PM3(ln(E))^3       !
!    E in keV.       PHOTX                                            !
!---------------------------------------------------------------------!
      DATA (PM0(5,IIZ),IIZ=1,100)/
     *0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,
     *0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,
     *0.53383D+01,0.68335D+01,0.74775D+01,0.80060D+01,0.84739D+01,
     *0.88860D+01,0.92842D+01,0.96612D+01,0.99956D+01,0.10292D+02,
     *0.10501D+02,0.10690D+02,0.10883D+02,0.11024D+02,0.11209D+02,
     *0.11372D+02,0.11524D+02,0.11709D+02,0.11837D+02,0.12003D+02,
     *0.12142D+02,0.12284D+02,0.12419D+02,0.12544D+02,0.12675D+02,
     *0.12791D+02,0.12891D+02,0.13003D+02,0.13091D+02,0.13177D+02,
     *0.13275D+02,0.13348D+02,0.13443D+02,0.13518D+02,0.13540D+02,
     *0.13552D+02,0.13719D+02,0.13771D+02,0.13844D+02,0.13890D+02,
     *0.13945D+02,0.14023D+02,0.14054D+02,0.14099D+02,0.14137D+02,
     *0.14156D+02,0.14201D+02,0.14281D+02,0.14298D+02,0.14326D+02,
     *0.14347D+02,0.14400D+02,0.14397D+02,0.14439D+02,0.14480D+02,
     *0.14500D+02,0.14448D+02,0.14483D+02,0.14546D+02,0.14569D+02,
     *0.14583D+02,0.14615D+02,0.14651D+02,0.14584D+02,0.14609D+02,
     *0.14642D+02,0.14665D+02,0.14665D+02,0.14675D+02,0.14733D+02,
     *0.14721D+02,0.14751D+02,0.14781D+02,0.14775D+02,0.14804D+02,
     *0.14819D+02,0.14824D+02,0.14766D+02,0.14827D+02,0.14841D+02,
     *0.14849D+02,0.14933D+02,0.14944D+02,0.14933D+02,0.14892D+02,
     *0.14932D+02,0.14985D+02,0.14989D+02,0.14955D+02,0.14972D+02/
!---------------------------------------------------------------------!
!PM1(M) data ln(sigma)=PM0+PM1(ln(E))+PM2(ln(E))^2+PM3(ln(E))^3       !
!    E in keV.       PHOTX                                            !
!---------------------------------------------------------------------!
      DATA (PM1(5,IIZ),IIZ=1,100)/
     *0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,
     *0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,
     *-.21300D+01,-.20850D+01,-.20040D+01,-.20399D+01,-.20801D+01,
     *-.21365D+01,-.21759D+01,-.22282D+01,-.22350D+01,-.22266D+01,
     *-.22038D+01,-.21784D+01,-.22006D+01,-.21973D+01,-.21730D+01,
     *-.21775D+01,-.21790D+01,-.22183D+01,-.22285D+01,-.22351D+01,
     *-.22139D+01,-.22086D+01,-.21928D+01,-.21648D+01,-.21674D+01,
     *-.21418D+01,-.20988D+01,-.20840D+01,-.20533D+01,-.20208D+01,
     *-.20154D+01,-.19774D+01,-.19720D+01,-.19461D+01,-.18782D+01,
     *-.18054D+01,-.18689D+01,-.18309D+01,-.18178D+01,-.17800D+01,
     *-.17542D+01,-.17574D+01,-.17085D+01,-.16784D+01,-.16495D+01,
     *-.16053D+01,-.15751D+01,-.15945D+01,-.15518D+01,-.15269D+01,
     *-.14917D+01,-.14890D+01,-.14378D+01,-.14234D+01,-.14137D+01,
     *-.13868D+01,-.13017D+01,-.12909D+01,-.13028D+01,-.12832D+01,
     *-.12532D+01,-.12379D+01,-.12318D+01,-.11486D+01,-.11307D+01,
     *-.11259D+01,-.11078D+01,-.10759D+01,-.10525D+01,-.10638D+01,
     *-.10238D+01,-.10189D+01,-.10107D+01,-.97785D+00,-.96818D+00,
     *-.95279D+00,-.93590D+00,-.87050D+00,-.88704D+00,-.87294D+00,
     *-.85761D+00,-.89230D+00,-.87713D+00,-.84903D+00,-.79757D+00,
     *-.80635D+00,-.82335D+00,-.80532D+00,-.77204D+00,-.76438D+00/
!---------------------------------------------------------------------!
!PM2(M) data ln(sigma)=PM0+PM1(ln(E))+PM2(ln(E))^2+PM3(ln(E))^3       !
!    E in keV.       PHOTX                                            !
!---------------------------------------------------------------------!
      DATA (PM2(5,IIZ),IIZ=1,100)/
     *0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,
     *0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,
     *-.29350D+00,-.28229D+00,-.29835D+00,-.28725D+00,-.26788D+00,
     *-.25113D+00,-.23956D+00,-.22524D+00,-.22123D+00,-.21586D+00,
     *-.21976D+00,-.22557D+00,-.21293D+00,-.21265D+00,-.21607D+00,
     *-.21191D+00,-.21025D+00,-.19719D+00,-.19526D+00,-.19017D+00,
     *-.19402D+00,-.19305D+00,-.19661D+00,-.20375D+00,-.19937D+00,
     *-.20544D+00,-.21761D+00,-.22071D+00,-.22637D+00,-.23454D+00,
     *-.23370D+00,-.24359D+00,-.24320D+00,-.24871D+00,-.26510D+00,
     *-.28303D+00,-.26506D+00,-.27343D+00,-.27517D+00,-.28362D+00,
     *-.28884D+00,-.28420D+00,-.29649D+00,-.30299D+00,-.30737D+00,
     *-.31642D+00,-.32406D+00,-.31568D+00,-.32547D+00,-.32909D+00,
     *-.33660D+00,-.33475D+00,-.34582D+00,-.34787D+00,-.34863D+00,
     *-.35313D+00,-.37262D+00,-.37296D+00,-.36779D+00,-.37056D+00,
     *-.37652D+00,-.37915D+00,-.37778D+00,-.39509D+00,-.39802D+00,
     *-.39630D+00,-.39948D+00,-.40492D+00,-.40846D+00,-.40410D+00,
     *-.41176D+00,-.41040D+00,-.41079D+00,-.41639D+00,-.41758D+00,
     *-.41893D+00,-.41982D+00,-.43240D+00,-.42732D+00,-.42883D+00,
     *-.42989D+00,-.42077D+00,-.42253D+00,-.42690D+00,-.43688D+00,
     *-.43275D+00,-.42728D+00,-.42985D+00,-.43392D+00,-.43414D+00/
!---------------------------------------------------------------------!
!PM3(M) data ln(sigma)=PM0+PM1(ln(E))+PM2(ln(E))^2+PM3(ln(E))^3       !
!    E in keV.       PHOTX                                            !
!---------------------------------------------------------------------!
      DATA (PM3(5,IIZ),IIZ=1,100)/
     *0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,
     *0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,
     *0.26839D-01,0.24298D-01,0.25251D-01,0.24711D-01,0.22617D-01,
     *0.21461D-01,0.20495D-01,0.19421D-01,0.19128D-01,0.18429D-01,
     *0.18742D-01,0.19295D-01,0.18042D-01,0.18022D-01,0.18304D-01,
     *0.17894D-01,0.17842D-01,0.16732D-01,0.16718D-01,0.16263D-01,
     *0.16565D-01,0.16429D-01,0.16754D-01,0.17388D-01,0.16894D-01,
     *0.17355D-01,0.18489D-01,0.18823D-01,0.19138D-01,0.19858D-01,
     *0.19727D-01,0.20549D-01,0.20503D-01,0.20901D-01,0.22222D-01,
     *0.23640D-01,0.22200D-01,0.22848D-01,0.22967D-01,0.23620D-01,
     *0.24005D-01,0.23468D-01,0.24492D-01,0.24992D-01,0.25230D-01,
     *0.25850D-01,0.26559D-01,0.25737D-01,0.26507D-01,0.26698D-01,
     *0.27267D-01,0.27052D-01,0.27852D-01,0.28010D-01,0.28026D-01,
     *0.28311D-01,0.29779D-01,0.29732D-01,0.29269D-01,0.29437D-01,
     *0.29869D-01,0.30090D-01,0.29866D-01,0.31066D-01,0.31285D-01,
     *0.31048D-01,0.31287D-01,0.31633D-01,0.31849D-01,0.31492D-01,
     *0.32025D-01,0.31833D-01,0.31841D-01,0.32191D-01,0.32302D-01,
     *0.32322D-01,0.32287D-01,0.33126D-01,0.32752D-01,0.32842D-01,
     *0.32838D-01,0.32189D-01,0.32283D-01,0.32555D-01,0.33250D-01,
     *0.32894D-01,0.32482D-01,0.32654D-01,0.32829D-01,0.32834D-01/
!--------------------------------------------------------------------!
!OMEGAL1 is probability of X-ray emission at L1-Shell absorption     !
!            (Table 3. of Table of Isotopes, Eighth Edition)         !
!--------------------------------------------------------------------!
      DATA OMEGAL1/
     *  0.000D+00,  0.000D+00,  0.000D+00,  0.000D+00,  0.000D+00,
     *  0.000D+00,  0.000D+00,  0.000D+00,  0.000D+00,  0.000D+00,
     *  0.000D+00,  0.290D-04,  0.260D-04,  0.300D-04,  0.390D-04,
     *  0.740D-04,  0.120D-03,  0.180D-03,  0.240D-03,  0.310D-03,
     *  0.390D-03,  0.470D-03,  0.580D-03,  0.710D-03,  0.840D-03,
     *  0.100D-02,  0.120D-02,  0.140D-02,  0.160D-02,  0.180D-02,
     *  0.210D-02,  0.240D-02,  0.280D-02,  0.320D-02,  0.360D-02,
     *  0.410D-02,  0.460D-02,  0.510D-02,  0.590D-02,  0.680D-02,
     *  0.940D-02,  0.100D-01,  0.110D-01,  0.120D-01,  0.130D-01,
     *  0.140D-01,  0.160D-01,  0.180D-01,  0.200D-01,  0.370D-01,
     *  0.390D-01,  0.410D-01,  0.440D-01,  0.460D-01,  0.490D-01,
     *  0.520D-01,  0.550D-01,  0.580D-01,  0.610D-01,  0.640D-01,
     *  0.660D-01,  0.710D-01,  0.750D-01,  0.790D-01,  0.830D-01,
     *  0.890D-01,  0.940D-01,  0.100D+00,  0.106D+00,  0.112D+00,
     *  0.120D+00,  0.128D+00,  0.137D+00,  0.147D+00,  0.144D+00,
     *  0.130D+00,  0.120D+00,  0.114D+00,  0.107D+00,  0.107D+00,
     *  0.107D+00,  0.112D+00,  0.117D+00,  0.122D+00,  0.128D+00,
     *  0.134D+00,  0.139D+00,  0.146D+00,  0.153D+00,  0.161D+00,
     *  0.162D+00,  0.176D+00,  0.187D+00,  0.205D+00,  0.218D+00,
     *  0.228D+00,  0.236D+00,  0.244D+00,  0.253D+00,  0.263D+00/
!---------------------------------------------------------------!
!F12  is probability of f12 Coster-Kronig at L1-Shell absorption!
!            (Table 3. of Table of Isotopes, Eighth Edition)    !
!---------------------------------------------------------------!
      DATA F12/
     *   0.00D+00,   0.00D+00,   0.00D+00,   0.00D+00,   0.00D+00,
     *   0.00D+00,   0.00D+00,   0.00D+00,   0.00D+00,   0.00D+00,
     *   0.00D+00,   0.32D+00,   0.32D+00,   0.32D+00,   0.32D+00,
     *   0.32D+00,   0.32D+00,   0.31D+00,   0.31D+00,   0.31D+00,
     *   0.31D+00,   0.31D+00,   0.31D+00,   0.31D+00,   0.30D+00,
     *   0.30D+00,   0.30D+00,   0.30D+00,   0.30D+00,   0.29D+00,
     *   0.29D+00,   0.28D+00,   0.28D+00,   0.28D+00,   0.28D+00,
     *   0.27D+00,   0.27D+00,   0.27D+00,   0.26D+00,   0.26D+00,
     *   0.10D+00,   0.10D+00,   0.10D+00,   0.10D+00,   0.10D+00,
     *   0.10D+00,   0.10D+00,   0.10D+00,   0.10D+00,   0.17D+00,
     *   0.17D+00,   0.18D+00,   0.18D+00,   0.19D+00,   0.19D+00,
     *   0.19D+00,   0.19D+00,   0.19D+00,   0.19D+00,   0.19D+00,
     *   0.19D+00,   0.19D+00,   0.19D+00,   0.19D+00,   0.19D+00,
     *   0.19D+00,   0.19D+00,   0.19D+00,   0.19D+00,   0.19D+00,
     *   0.19D+00,   0.18D+00,   0.18D+00,   0.17D+00,   0.16D+00,
     *   0.16D+00,   0.15D+00,   0.14D+00,   0.14D+00,   0.13D+00,
     *   0.13D+00,   0.12D+00,   0.11D+00,   0.11D+00,   0.10D+00,
     *   0.10D+00,   0.10D+00,   0.90D-01,   0.90D-01,   0.90D-01,
     *   0.80D-01,   0.80D-01,   0.70D-01,   0.50D-01,   0.50D-01,
     *   0.40D-01,   0.40D-01,   0.30D-01,   0.30D-01,   0.30D-01/
!----------------------------------------------------------------------!
!F13  is probability of f13 Coster-Kronig at L1-Shell absorption       !
!            (Table 3. of Table of Isotopes, Eighth Edition)           !
!----------------------------------------------------------------------!
      DATA F13/
     *  0.000E+00,  0.000E+00,  0.000E+00,  0.000E+00,  0.000E+00,
     *  0.000E+00,  0.000E+00,  0.000E+00,  0.000E+00,  0.000E+00,
     *  0.000E+00,  0.640E+00,  0.640E+00,  0.640E+00,  0.630E+00,
     *  0.620E+00,  0.620E+00,  0.620E+00,  0.620E+00,  0.610E+00,
     *  0.600E+00,  0.590E+00,  0.580E+00,  0.570E+00,  0.580E+00,
     *  0.570E+00,  0.560E+00,  0.550E+00,  0.540E+00,  0.540E+00,
     *  0.530E+00,  0.530E+00,  0.530E+00,  0.520E+00,  0.520E+00,
     *  0.520E+00,  0.520E+00,  0.520E+00,  0.520E+00,  0.520E+00,
     *  0.610E+00,  0.610E+00,  0.610E+00,  0.610E+00,  0.600E+00,
     *  0.600E+00,  0.590E+00,  0.590E+00,  0.590E+00,  0.270E+00,
     *  0.280E+00,  0.280E+00,  0.280E+00,  0.280E+00,  0.280E+00,
     *  0.280E+00,  0.291E+00,  0.291E+00,  0.291E+00,  0.301E+00,
     *  0.301E+00,  0.301E+00,  0.301E+00,  0.301E+00,  0.301E+00,
     *  0.301E+00,  0.301E+00,  0.301E+00,  0.292E+00,  0.292E+00,
     *  0.282E+00,  0.282E+00,  0.283E+00,  0.283E+00,  0.333E+00,
     *  0.393E+00,  0.453E+00,  0.503E+00,  0.533E+00,  0.563E+00,
     *  0.573E+00,  0.584E+00,  0.584E+00,  0.584E+00,  0.595E+00,
     *  0.585E+00,  0.586E+00,  0.586E+00,  0.587E+00,  0.578E+00,
     *  0.588E+00,  0.580E+00,  0.581E+00,  0.573E+00,  0.564E+00,
     *  0.566E+00,  0.557E+00,  0.559E+00,  0.561E+00,  0.553E+00/
!----------------------------------------------------------------------!
! Probability of L-beta3 emission,                                     !
!         or Lbeta3/(L1-Xray total)                                    !
!  Intensity are adjusted to experimental data by S.I.Salem et al.     !
!  Atomic Data and Nuclear Data Table 14,91-109(1974) using the        !
!  theoretical data by J.M. Scofield. Atomic Data and Nuclear Data     !
!  Tables 14,121-137(1974).                                            !
!----------------------------------------------------------------------!
      DATA (DFLX1(1,IIZ),IIZ=1,100)/
     *0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,
     *0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,
     *0.00000D+00,0.00000D+00,0.65999D+00,0.65979D+00,0.65962D+00,
     *0.65893D+00,0.65789D+00,0.65744D+00,0.65743D+00,0.65709D+00,
     *0.65606D+00,0.65476D+00,0.65396D+00,0.65433D+00,0.65182D+00,
     *0.65070D+00,0.64941D+00,0.64802D+00,0.64631D+00,0.64498D+00,
     *0.57777D+00,0.57470D+00,0.56952D+00,0.56484D+00,0.55963D+00,
     *0.55401D+00,0.54903D+00,0.54424D+00,0.54102D+00,0.51464D+00,
     *0.51253D+00,0.49875D+00,0.50033D+00,0.50200D+00,0.50325D+00,
     *0.50504D+00,0.50534D+00,0.50581D+00,0.50461D+00,0.50317D+00,
     *0.50127D+00,0.49912D+00,0.49561D+00,0.49234D+00,0.48836D+00,
     *0.48509D+00,0.48207D+00,0.48230D+00,0.48021D+00,0.47818D+00,
     *0.47550D+00,0.47307D+00,0.47041D+00,0.46140D+00,0.45938D+00,
     *0.45675D+00,0.45309D+00,0.44989D+00,0.44570D+00,0.44178D+00,
     *0.43681D+00,0.43249D+00,0.42716D+00,0.42225D+00,0.41676D+00,
     *0.41154D+00,0.40568D+00,0.40000D+00,0.39358D+00,0.38724D+00,
     *0.38060D+00,0.37444D+00,0.36746D+00,0.36097D+00,0.35406D+00,
     *0.34756D+00,0.34025D+00,0.33341D+00,0.32635D+00,0.31964D+00,
     *0.31299D+00,0.30657D+00,0.29994D+00,0.29366D+00,0.28633D+00,
     *0.27933D+00,0.27255D+00,0.26613D+00,0.25986D+00,0.25385D+00/
!----------------------------------------------------------------------!
! Probability of L-beta4 emission,                                     !
!         or DFLX1(1)+Lbeta4/(L1-Xray total)                           !
!  Intensity are adjusted to experimental data by S.I.Salem et al.     !
!  Atomic Data and Nuclear Data Table 14,91-109(1974) using the        !
!  theoretical data by J.M. Scofield. Atomic Data and Nuclear Data     !
!  Tables 14,121-137(1974).                                            !
!  Corrected precision same with other data. Aug. 29, 2007 HH          !
!----------------------------------------------------------------------!
      DATA (DFLX1(2,IIZ),IIZ=1,100)/
     *0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,
     *0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,
     *0.00000D+00,0.00000D+00,1.00000D+00,1.00000D+00,1.00000D+00,
     *1.00000D+00,1.00000D+00,1.00000D+00,1.00000D+00,1.00000D+00,
     *0.99990D+00,0.99975D+00,0.99957D+00,0.99928D+00,0.99910D+00,
     *0.99880D+00,0.99845D+00,0.99806D+00,0.99752D+00,0.99713D+00,
     *0.99174D+00,0.89182D+00,0.88539D+00,0.87856D+00,0.87227D+00,
     *0.86542D+00,0.85899D+00,0.85343D+00,0.85007D+00,0.85383D+00,
     *0.85081D+00,0.85086D+00,0.84656D+00,0.84236D+00,0.83891D+00,
     *0.83584D+00,0.83129D+00,0.82700D+00,0.82151D+00,0.81565D+00,
     *0.80905D+00,0.80209D+00,0.79446D+00,0.78676D+00,0.77991D+00,
     *0.77372D+00,0.76841D+00,0.76783D+00,0.76497D+00,0.76223D+00,
     *0.75938D+00,0.75691D+00,0.75454D+00,0.74193D+00,0.74098D+00,
     *0.73903D+00,0.73718D+00,0.73556D+00,0.73317D+00,0.73115D+00,
     *0.72817D+00,0.72572D+00,0.72275D+00,0.71993D+00,0.71642D+00,
     *0.71279D+00,0.70953D+00,0.70600D+00,0.70214D+00,0.69819D+00,
     *0.69383D+00,0.68971D+00,0.68494D+00,0.68044D+00,0.67625D+00,
     *0.67219D+00,0.66757D+00,0.66314D+00,0.65824D+00,0.65366D+00,
     *0.64914D+00,0.64442D+00,0.63948D+00,0.63489D+00,0.62877D+00,
     *0.62291D+00,0.61705D+00,0.61157D+00,0.60598D+00,0.60062D+00/
!----------------------------------------------------------------------!
! Probability of L-beta10 emission,                                    !
!         or DFLX1(2)+Lbeta10/(L1-Xray total)                          !
!  Intensity are adjusted to experimental data by S.I.Salem et al.     !
!  Atomic Data and Nuclear Data Table 14,91-109(1974) using the        !
!  theoretical data by J.M. Scofield. Atomic Data and Nuclear Data     !
!  Tables 14,121-137(1974).                                            !
!----------------------------------------------------------------------!
      DATA (DFLX1(3,IIZ),IIZ=1,100)/
     *0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,
     *0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,
     *0.00000D+00,0.00000D+00,0.10000D+01,0.10000D+01,0.10000D+01,
     *0.10000D+01,0.10000D+01,0.10000D+01,0.10000D+01,0.10000D+01,
     *0.99994D+00,0.99985D+00,0.99975D+00,0.99957D+00,0.99946D+00,
     *0.99928D+00,0.99908D+00,0.99884D+00,0.99852D+00,0.99829D+00,
     *0.99292D+00,0.89314D+00,0.88684D+00,0.88014D+00,0.87397D+00,
     *0.86725D+00,0.86095D+00,0.85552D+00,0.85230D+00,0.85610D+00,
     *0.85322D+00,0.85336D+00,0.84922D+00,0.84518D+00,0.84191D+00,
     *0.83901D+00,0.83464D+00,0.83054D+00,0.82524D+00,0.81955D+00,
     *0.81314D+00,0.80637D+00,0.79893D+00,0.79141D+00,0.78475D+00,
     *0.77876D+00,0.77367D+00,0.77333D+00,0.77070D+00,0.76820D+00,
     *0.76559D+00,0.76338D+00,0.76127D+00,0.74882D+00,0.74816D+00,
     *0.74648D+00,0.74492D+00,0.74358D+00,0.74147D+00,0.73974D+00,
     *0.73704D+00,0.73489D+00,0.73222D+00,0.72972D+00,0.72651D+00,
     *0.72321D+00,0.72027D+00,0.71705D+00,0.71352D+00,0.70989D+00,
     *0.70586D+00,0.70209D+00,0.69766D+00,0.69351D+00,0.68968D+00,
     *0.68601D+00,0.68175D+00,0.67772D+00,0.67321D+00,0.66904D+00,
     *0.66496D+00,0.66072D+00,0.65627D+00,0.65218D+00,0.64652D+00,
     *0.64119D+00,0.63589D+00,0.63102D+00,0.62606D+00,0.62139D+00/
!----------------------------------------------------------------------!
! Probability of L-beta9/1 emission,                                   !
!         or DFLX1(3)+Lbeta9/1/(L1-Xray total)                         !
!  Intensity are adjusted to experimental data by S.I.Salem et al.     !
!  Atomic Data and Nuclear Data Table 14,91-109(1974) using the        !
!  theoretical data by J.M. Scofield. Atomic Data and Nuclear Data     !
!  Tables 14,121-137(1974).                                            !
!----------------------------------------------------------------------!
      DATA (DFLX1(4,IIZ),IIZ=1,100)/
     *0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,
     *0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,
     *0.00000D+00,0.00000D+00,0.10000D+01,0.10000D+01,0.10000D+01,
     *0.10000D+01,0.10000D+01,0.10000D+01,0.10000D+01,0.10000D+01,
     *0.10000D+01,0.10000D+01,0.10000D+01,0.10000D+01,0.10000D+01,
     *0.10000D+01,0.10000D+01,0.10000D+01,0.10000D+01,0.10000D+01,
     *0.99468D+00,0.89509D+00,0.88899D+00,0.88248D+00,0.87651D+00,
     *0.86998D+00,0.86387D+00,0.85864D+00,0.85562D+00,0.85944D+00,
     *0.85681D+00,0.85707D+00,0.85317D+00,0.84939D+00,0.84638D+00,
     *0.84375D+00,0.83964D+00,0.83582D+00,0.83080D+00,0.82537D+00,
     *0.81925D+00,0.81276D+00,0.80560D+00,0.79837D+00,0.79198D+00,
     *0.78630D+00,0.78151D+00,0.78155D+00,0.77928D+00,0.77714D+00,
     *0.77489D+00,0.77305D+00,0.77133D+00,0.75915D+00,0.75891D+00,
     *0.75764D+00,0.75649D+00,0.75559D+00,0.75391D+00,0.75260D+00,
     *0.75033D+00,0.74864D+00,0.74639D+00,0.74437D+00,0.74161D+00,
     *0.73881D+00,0.73635D+00,0.73360D+00,0.73055D+00,0.72740D+00,
     *0.72387D+00,0.72062D+00,0.71669D+00,0.71306D+00,0.70975D+00,
     *0.70668D+00,0.70292D+00,0.69948D+00,0.69557D+00,0.69199D+00,
     *0.68855D+00,0.68502D+00,0.68127D+00,0.67792D+00,0.67295D+00,
     *0.66836D+00,0.66387D+00,0.65985D+00,0.65581D+00,0.65215D+00/
!----------------------------------------------------------------------!
! Probability of L-gamma2 emission,                                    !
!         or DFLX1(4)+Lgamma2/(L1-Xray total)                          !
!  Intensity are adjusted to experimental data by S.I.Salem et al.     !
!  Atomic Data and Nuclear Data Table 14,91-109(1974) using the        !
!  theoretical data by J.M. Scofield. Atomic Data and Nuclear Data     !
!  Tables 14,121-137(1974).                                            !
!----------------------------------------------------------------------!
      DATA (DFLX1(5,IIZ),IIZ=1,100)/
     *0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,
     *0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,
     *0.00000D+00,0.00000D+00,0.10000D+01,0.10000D+01,0.10000D+01,
     *0.10000D+01,0.10000D+01,0.10000D+01,0.10000D+01,0.10000D+01,
     *0.10000D+01,0.10000D+01,0.10000D+01,0.10000D+01,0.10000D+01,
     *0.10000D+01,0.10000D+01,0.10000D+01,0.10000D+01,0.10000D+01,
     *0.99659D+00,0.90058D+00,0.89919D+00,0.89833D+00,0.89871D+00,
     *0.89917D+00,0.89843D+00,0.89768D+00,0.89775D+00,0.90222D+00,
     *0.90108D+00,0.90225D+00,0.90043D+00,0.89860D+00,0.89734D+00,
     *0.89596D+00,0.89388D+00,0.89226D+00,0.88924D+00,0.88577D+00,
     *0.88162D+00,0.87687D+00,0.87140D+00,0.86573D+00,0.86079D+00,
     *0.85670D+00,0.85351D+00,0.85420D+00,0.85285D+00,0.85155D+00,
     *0.85005D+00,0.84899D+00,0.84800D+00,0.84773D+00,0.84803D+00,
     *0.84670D+00,0.84575D+00,0.84467D+00,0.84439D+00,0.84405D+00,
     *0.84206D+00,0.84033D+00,0.83823D+00,0.83642D+00,0.83497D+00,
     *0.83347D+00,0.83290D+00,0.83160D+00,0.83052D+00,0.82925D+00,
     *0.82854D+00,0.82771D+00,0.82693D+00,0.82605D+00,0.82588D+00,
     *0.82555D+00,0.82507D+00,0.82450D+00,0.82415D+00,0.82368D+00,
     *0.82344D+00,0.82298D+00,0.82314D+00,0.82328D+00,0.82355D+00,
     *0.82394D+00,0.82412D+00,0.82458D+00,0.82472D+00,0.82502D+00/
!----------------------------------------------------------------------!
! Probability of L-gamma3 emission,                                    !
!         or DFLX1(5)+Lgamma3/(L1-Xray total)                          !
!  Intensity are adjusted to experimental data by S.I.Salem et al.     !
!  Atomic Data and Nuclear Data Table 14,91-109(1974) using the        !
!  theoretical data by J.M. Scofield. Atomic Data and Nuclear Data     !
!  Tables 14,121-137(1974).                                            !
!----------------------------------------------------------------------!
      DATA (DFLX1(6,IIZ),IIZ=1,100)/
     *0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,
     *0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,
     *0.00000D+00,0.00000D+00,0.10000D+01,0.10000D+01,0.10000D+01,
     *0.10000D+01,0.10000D+01,0.10000D+01,0.10000D+01,0.10000D+01,
     *0.10000D+01,0.10000D+01,0.10000D+01,0.10000D+01,0.10000D+01,
     *0.10000D+01,0.10000D+01,0.10000D+01,0.10000D+01,0.10000D+01,
     *0.10000D+01,0.10000D+01,0.10000D+01,0.10000D+01,0.10000D+01,
     *0.10000D+01,0.10000D+01,0.10000D+01,0.10000D+01,0.10000D+01,
     *0.10000D+01,0.10000D+01,0.10000D+01,0.10000D+01,0.10000D+01,
     *0.10000D+01,0.10000D+01,0.10000D+01,0.99874D+00,0.99647D+00,
     *0.99340D+00,0.98967D+00,0.98539D+00,0.98044D+00,0.97653D+00,
     *0.97312D+00,0.97065D+00,0.97285D+00,0.97290D+00,0.97301D+00,
     *0.97321D+00,0.97341D+00,0.97360D+00,0.97231D+00,0.97436D+00,
     *0.97459D+00,0.97488D+00,0.97513D+00,0.97542D+00,0.97570D+00,
     *0.97442D+00,0.97310D+00,0.97193D+00,0.97070D+00,0.96958D+00,
     *0.96845D+00,0.96800D+00,0.96680D+00,0.96591D+00,0.96478D+00,
     *0.96365D+00,0.96250D+00,0.96142D+00,0.96033D+00,0.95936D+00,
     *0.95832D+00,0.95743D+00,0.95653D+00,0.95567D+00,0.95473D+00,
     *0.95427D+00,0.95358D+00,0.95302D+00,0.95249D+00,0.95212D+00,
     *0.95160D+00,0.95113D+00,0.95073D+00,0.95023D+00,0.94967D+00/
!----------------------------------------------------------------------!
! Probability of L-gamma4/1 emission,                                  !
!         or DFLX1(6)+Lgamma4/1/(L1-Xray total)                        !
!  Intensity are adjusted to experimental data by S.I.Salem et al.     !
!  Atomic Data and Nuclear Data Table 14,91-109(1974) using the        !
!  theoretical data by J.M. Scofield. Atomic Data and Nuclear Data     !
!  Tables 14,121-137(1974).                                            !
!----------------------------------------------------------------------!
      DATA (DFLX1(7,IIZ),IIZ=1,100)/
     *0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,
     *0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,
     *0.00000D+00,0.00000D+00,0.10000D+01,0.10000D+01,0.10000D+01,
     *0.10000D+01,0.10000D+01,0.10000D+01,0.10000D+01,0.10000D+01,
     *0.10000D+01,0.10000D+01,0.10000D+01,0.10000D+01,0.10000D+01,
     *0.10000D+01,0.10000D+01,0.10000D+01,0.10000D+01,0.10000D+01,
     *0.10000D+01,0.10000D+01,0.10000D+01,0.10000D+01,0.10000D+01,
     *0.10000D+01,0.10000D+01,0.10000D+01,0.10000D+01,0.10000D+01,
     *0.10000D+01,0.10000D+01,0.10000D+01,0.10000D+01,0.10000D+01,
     *0.10000D+01,0.10000D+01,0.10000D+01,0.99924D+00,0.99785D+00,
     *0.99597D+00,0.99370D+00,0.99112D+00,0.98810D+00,0.98565D+00,
     *0.98355D+00,0.98208D+00,0.98352D+00,0.98362D+00,0.98375D+00,
     *0.98396D+00,0.98414D+00,0.98433D+00,0.98361D+00,0.98494D+00,
     *0.98516D+00,0.98542D+00,0.98564D+00,0.98589D+00,0.98614D+00,
     *0.98545D+00,0.98475D+00,0.98415D+00,0.98352D+00,0.98297D+00,
     *0.98243D+00,0.98232D+00,0.98174D+00,0.98136D+00,0.98084D+00,
     *0.98031D+00,0.97979D+00,0.97931D+00,0.97883D+00,0.97843D+00,
     *0.97804D+00,0.97772D+00,0.97741D+00,0.97710D+00,0.97681D+00,
     *0.97681D+00,0.97669D+00,0.97665D+00,0.97663D+00,0.97672D+00,
     *0.97674D+00,0.97681D+00,0.97695D+00,0.97704D+00,0.97712D+00/
!----------------------------------------------------------------------!
!OMEGAL2  is probability of X-ray emission at L2-Shell absorption      !
!            (Table 3. of Table of Isotopes, Eighth Edition)           !
!----------------------------------------------------------------------!
      DATA OMEGAL2/
     *  0.000D+00,  0.000D+00,  0.000D+00,  0.000D+00,  0.000D+00,
     *  0.000D+00,  0.000D+00,  0.000D+00,  0.000D+00,  0.000D+00,
     *  0.000D+00,  0.120D-02,  0.750D-03,  0.370D-03,  0.310D-03,
     *  0.260D-03,  0.240D-03,  0.220D-03,  0.270D-03,  0.330D-03,
     *  0.840D-03,  0.150D-02,  0.260D-02,  0.370D-02,  0.500D-02,
     *  0.630D-02,  0.770D-02,  0.860D-02,  0.100D-01,  0.110D-01,
     *  0.120D-01,  0.130D-01,  0.140D-01,  0.160D-01,  0.180D-01,
     *  0.200D-01,  0.220D-01,  0.240D-01,  0.260D-01,  0.280D-01,
     *  0.310D-01,  0.340D-01,  0.370D-01,  0.400D-01,  0.430D-01,
     *  0.470D-01,  0.510D-01,  0.560D-01,  0.610D-01,  0.650D-01,
     *  0.690D-01,  0.740D-01,  0.790D-01,  0.830D-01,  0.900D-01,
     *  0.960D-01,  0.103D+00,  0.110D+00,  0.117D+00,  0.124D+00,
     *  0.132D+00,  0.140D+00,  0.149D+00,  0.158D+00,  0.167D+00,
     *  0.178D+00,  0.189D+00,  0.200D+00,  0.211D+00,  0.222D+00,
     *  0.234D+00,  0.246D+00,  0.258D+00,  0.270D+00,  0.283D+00,
     *  0.295D+00,  0.308D+00,  0.321D+00,  0.334D+00,  0.347D+00,
     *  0.360D+00,  0.373D+00,  0.387D+00,  0.401D+00,  0.415D+00,
     *  0.429D+00,  0.443D+00,  0.456D+00,  0.468D+00,  0.479D+00,
     *  0.472D+00,  0.467D+00,  0.466D+00,  0.464D+00,  0.471D+00,
     *  0.479D+00,  0.485D+00,  0.490D+00,  0.497D+00,  0.506D+00/
!---------------------------------------------------------------!
!F23  is probability of f23 Coster-Kronig at L2-Shell absorption!
!            (Table 3. of Table of Isotopes, Eighth Edition)    !
!---------------------------------------------------------------!
      DATA F23/
     *  0.000D+00,  0.000D+00,  0.000D+00,  0.000D+00,  0.000D+00,
     *  0.000D+00,  0.000D+00,  0.000D+00,  0.000D+00,  0.000D+00,
     *  0.000D+00,  0.000D+00,  0.000D+00,  0.000D+00,  0.000D+00,
     *  0.000D+00,  0.000D+00,  0.000D+00,  0.000D+00,  0.000D+00,
     *  0.000D+00,  0.000D+00,  0.000D+00,  0.000D+00,  0.000D+00,
     *  0.000D+00,  0.000D+00,  0.280D-01,  0.280D-01,  0.260D-01,
     *  0.320D-01,  0.500D-01,  0.630D-01,  0.760D-01,  0.880D-01,
     *  0.100D+00,  0.109D+00,  0.117D+00,  0.126D+00,  0.132D+00,
     *  0.137D+00,  0.141D+00,  0.144D+00,  0.148D+00,  0.150D+00,
     *  0.151D+00,  0.153D+00,  0.155D+00,  0.157D+00,  0.157D+00,
     *  0.156D+00,  0.155D+00,  0.154D+00,  0.154D+00,  0.154D+00,
     *  0.153D+00,  0.153D+00,  0.153D+00,  0.153D+00,  0.152D+00,
     *  0.151D+00,  0.150D+00,  0.149D+00,  0.147D+00,  0.145D+00,
     *  0.143D+00,  0.142D+00,  0.140D+00,  0.139D+00,  0.138D+00,
     *  0.136D+00,  0.135D+00,  0.134D+00,  0.133D+00,  0.130D+00,
     *  0.128D+00,  0.126D+00,  0.124D+00,  0.122D+00,  0.120D+00,
     *  0.118D+00,  0.116D+00,  0.113D+00,  0.111D+00,  0.111D+00,
     *  0.110D+00,  0.109D+00,  0.108D+00,  0.108D+00,  0.108D+00,
     *  0.139D+00,  0.167D+00,  0.192D+00,  0.198D+00,  0.203D+00,
     *  0.200D+00,  0.198D+00,  0.197D+00,  0.196D+00,  0.194D+00/
!----------------------------------------------------------------------!
! Probability of L-beta1 emission,                                     !
!         or Lbeta1/(L2-Xray total)                                    !
!  Intensity are adjusted to experimental data by S.I.Salem et al.     !
!  Atomic Data and Nuclear Data Table 14,91-109(1974) using the        !
!  theoretical data by J.M. Scofield. Atomic Data and Nuclear Data     !
!  Tables 14,121-137(1974).                                            !
!----------------------------------------------------------------------!
      DATA (DFLX2(1,IIZ),IIZ=1,100)/
     *0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,
     *0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,
     *0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,
     *0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,
     *0.51319D+00,0.70299D+00,0.79547D+00,0.86613D+00,0.88148D+00,
     *0.90399D+00,0.92002D+00,0.92569D+00,0.93170D+00,0.93370D+00,
     *0.93543D+00,0.93743D+00,0.93935D+00,0.94134D+00,0.94322D+00,
     *0.94513D+00,0.94677D+00,0.94842D+00,0.94977D+00,0.92222D+00,
     *0.91501D+00,0.90788D+00,0.90077D+00,0.89376D+00,0.88155D+00,
     *0.86971D+00,0.87068D+00,0.87165D+00,0.86782D+00,0.86401D+00,
     *0.86117D+00,0.85834D+00,0.85425D+00,0.85020D+00,0.84889D+00,
     *0.84757D+00,0.84427D+00,0.84291D+00,0.84076D+00,0.83864D+00,
     *0.83740D+00,0.83618D+00,0.83478D+00,0.83264D+00,0.83234D+00,
     *0.83129D+00,0.83021D+00,0.82914D+00,0.82806D+00,0.82699D+00,
     *0.82562D+00,0.82399D+00,0.82167D+00,0.81777D+00,0.81279D+00,
     *0.80787D+00,0.80395D+00,0.80005D+00,0.79560D+00,0.79124D+00,
     *0.78748D+00,0.78381D+00,0.78007D+00,0.77637D+00,0.77298D+00,
     *0.76961D+00,0.76654D+00,0.76349D+00,0.76110D+00,0.75872D+00,
     *0.75633D+00,0.75396D+00,0.75281D+00,0.75167D+00,0.74949D+00,
     *0.74732D+00,0.74516D+00,0.74301D+00,0.74088D+00,0.73875D+00/
!----------------------------------------------------------------------!
! Probability of L-gamma1 emission,                                    !
!         or DFLX2(1)+Lgamma1/(L2-Xray total)                          !
!  Intensity are adjusted to experimental data by S.I.Salem et al.     !
!  Atomic Data and Nuclear Data Table 14,91-109(1974) using the        !
!  theoretical data by J.M. Scofield. Atomic Data and Nuclear Data     !
!  Tables 14,121-137(1974).                                            !
!----------------------------------------------------------------------!
      DATA (DFLX2(2,IIZ),IIZ=1,100)/
     *0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,
     *0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,
     *0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,
     *0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,
     *0.51319D+00,0.70299D+00,0.79547D+00,0.86613D+00,0.88148D+00,
     *0.90399D+00,0.92002D+00,0.92569D+00,0.93170D+00,0.93370D+00,
     *0.93543D+00,0.93743D+00,0.93935D+00,0.94134D+00,0.94322D+00,
     *0.94513D+00,0.94677D+00,0.94842D+00,0.94977D+00,0.95265D+00,
     *0.95436D+00,0.95600D+00,0.95765D+00,0.95928D+00,0.96089D+00,
     *0.96250D+00,0.96327D+00,0.96405D+00,0.96501D+00,0.96596D+00,
     *0.96666D+00,0.96734D+00,0.96829D+00,0.96923D+00,0.96986D+00,
     *0.97047D+00,0.97007D+00,0.97187D+00,0.97234D+00,0.97282D+00,
     *0.97348D+00,0.97415D+00,0.97461D+00,0.97419D+00,0.97550D+00,
     *0.97593D+00,0.97633D+00,0.97673D+00,0.97699D+00,0.97726D+00,
     *0.97671D+00,0.97585D+00,0.97462D+00,0.97151D+00,0.96779D+00,
     *0.96411D+00,0.96100D+00,0.95790D+00,0.95503D+00,0.95226D+00,
     *0.95002D+00,0.94786D+00,0.94572D+00,0.94361D+00,0.94203D+00,
     *0.94047D+00,0.93928D+00,0.93810D+00,0.93729D+00,0.93649D+00,
     *0.93607D+00,0.93566D+00,0.93537D+00,0.93507D+00,0.93487D+00,
     *0.93468D+00,0.93446D+00,0.93427D+00,0.93407D+00,0.93386D+00/
!----------------------------------------------------------------------!
! Probability of L-gamma5 emission,                                    !
!         or DFLX2(2)+Lgamma51/(L2-Xray total)                         !
!  Intensity are adjusted to experimental data by S.I.Salem et al.     !
!  Atomic Data and Nuclear Data Table 14,91-109(1974) using the        !
!  theoretical data by J.M. Scofield. Atomic Data and Nuclear Data     !
!  Tables 14,121-137(1974).                                            !
!----------------------------------------------------------------------!
      DATA (DFLX2(3,IIZ),IIZ=1,100)/
     *0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,
     *0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,
     *0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,
     *0.00000D+00,0.00000D+00,0.00000D+00,0.23840D-01,0.70940D-01,
     *0.54787D+00,0.72375D+00,0.80940D+00,0.86965D+00,0.88907D+00,
     *0.90995D+00,0.92485D+00,0.92965D+00,0.93292D+00,0.93651D+00,
     *0.93882D+00,0.94113D+00,0.94326D+00,0.94540D+00,0.94742D+00,
     *0.94944D+00,0.95134D+00,0.95324D+00,0.95474D+00,0.95758D+00,
     *0.95928D+00,0.96096D+00,0.96262D+00,0.96425D+00,0.96584D+00,
     *0.96739D+00,0.96822D+00,0.96906D+00,0.97006D+00,0.97106D+00,
     *0.97180D+00,0.97253D+00,0.97352D+00,0.97449D+00,0.97517D+00,
     *0.97584D+00,0.97547D+00,0.97724D+00,0.97772D+00,0.97820D+00,
     *0.97886D+00,0.97951D+00,0.97997D+00,0.97956D+00,0.98086D+00,
     *0.98130D+00,0.98169D+00,0.98209D+00,0.98236D+00,0.98263D+00,
     *0.98209D+00,0.98126D+00,0.98003D+00,0.97694D+00,0.97322D+00,
     *0.96954D+00,0.96644D+00,0.96336D+00,0.96050D+00,0.95767D+00,
     *0.95551D+00,0.95336D+00,0.95125D+00,0.94915D+00,0.94759D+00,
     *0.94605D+00,0.94489D+00,0.94373D+00,0.94296D+00,0.94219D+00,
     *0.94180D+00,0.94142D+00,0.94117D+00,0.94092D+00,0.94075D+00,
     *0.94059D+00,0.94042D+00,0.94026D+00,0.94010D+00,0.93994D+00/
!----------------------------------------------------------------------!
! Probability of L-gamma6 emission,                                    !
!         or DFLX2(3)+Lgamma6/(L2-Xray total)                          !
!  Intensity are adjusted to experimental data by S.I.Salem et al.     !
!  Atomic Data and Nuclear Data Table 14,91-109(1974) using the        !
!  theoretical data by J.M. Scofield. Atomic Data and Nuclear Data     !
!  Tables 14,121-137(1974).                                            !
!----------------------------------------------------------------------!
      DATA (DFLX2(4,IIZ),IIZ=1,100)/
     *0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,
     *0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,
     *0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,
     *0.00000D+00,0.00000D+00,0.00000D+00,0.23840D-01,0.70940D-01,
     *0.54787D+00,0.72375D+00,0.80940D+00,0.86965D+00,0.88907D+00,
     *0.90995D+00,0.92485D+00,0.92965D+00,0.93292D+00,0.93651D+00,
     *0.93882D+00,0.94113D+00,0.94326D+00,0.94540D+00,0.94742D+00,
     *0.94944D+00,0.95134D+00,0.95324D+00,0.95474D+00,0.95758D+00,
     *0.95928D+00,0.96096D+00,0.96262D+00,0.96425D+00,0.96584D+00,
     *0.96739D+00,0.96822D+00,0.96906D+00,0.97006D+00,0.97106D+00,
     *0.97180D+00,0.97253D+00,0.97352D+00,0.97449D+00,0.97517D+00,
     *0.97584D+00,0.97657D+00,0.97724D+00,0.97772D+00,0.97820D+00,
     *0.97886D+00,0.97951D+00,0.97997D+00,0.98043D+00,0.98086D+00,
     *0.98130D+00,0.98169D+00,0.98209D+00,0.98236D+00,0.98263D+00,
     *0.98274D+00,0.98286D+00,0.98283D+00,0.98283D+00,0.98285D+00,
     *0.98287D+00,0.98272D+00,0.98256D+00,0.98238D+00,0.98220D+00,
     *0.98208D+00,0.98197D+00,0.98167D+00,0.98137D+00,0.98122D+00,
     *0.98107D+00,0.98099D+00,0.98091D+00,0.98059D+00,0.98027D+00,
     *0.98015D+00,0.98002D+00,0.97986D+00,0.97970D+00,0.97958D+00,
     *0.97945D+00,0.97932D+00,0.97920D+00,0.97907D+00,0.97895D+00/
!----------------------------------------------------------------------!
!OMEGAL3  is probability of X-ray emission at L3-Shell absorption      !
!            (Table 3. of Table of Isotopes, Eighth Edition)           !
!----------------------------------------------------------------------!
      DATA OMEGAL3/
     *  0.000D+00,  0.000D+00,  0.000D+00,  0.000D+00,  0.000D+00,
     *  0.000D+00,  0.000D+00,  0.000D+00,  0.000D+00,  0.000D+00,
     *  0.000D+00,  0.120D-02,  0.750D-03,  0.380D-03,  0.310D-03,
     *  0.260D-03,  0.240D-03,  0.220D-03,  0.270D-03,  0.330D-03,
     *  0.840D-03,  0.150D-02,  0.260D-02,  0.370D-02,  0.500D-02,
     *  0.630D-02,  0.770D-02,  0.930D-02,  0.110D-01,  0.120D-01,
     *  0.130D-01,  0.150D-01,  0.160D-01,  0.180D-01,  0.200D-01,
     *  0.220D-01,  0.240D-01,  0.260D-01,  0.280D-01,  0.310D-01,
     *  0.340D-01,  0.370D-01,  0.400D-01,  0.430D-01,  0.460D-01,
     *  0.490D-01,  0.520D-01,  0.560D-01,  0.600D-01,  0.640D-01,
     *  0.690D-01,  0.740D-01,  0.790D-01,  0.850D-01,  0.910D-01,
     *  0.970D-01,  0.104D+00,  0.111D+00,  0.118D+00,  0.125D+00,
     *  0.132D+00,  0.139D+00,  0.147D+00,  0.155D+00,  0.164D+00,
     *  0.174D+00,  0.182D+00,  0.192D+00,  0.201D+00,  0.210D+00,
     *  0.220D+00,  0.231D+00,  0.243D+00,  0.255D+00,  0.268D+00,
     *  0.281D+00,  0.294D+00,  0.306D+00,  0.320D+00,  0.333D+00,
     *  0.347D+00,  0.360D+00,  0.373D+00,  0.386D+00,  0.399D+00,
     *  0.411D+00,  0.424D+00,  0.437D+00,  0.450D+00,  0.463D+00,
     *  0.476D+00,  0.489D+00,  0.502D+00,  0.514D+00,  0.526D+00,
     *  0.539D+00,  0.550D+00,  0.560D+00,  0.570D+00,  0.579D+00/
!----------------------------------------------------------------------!
! Probability of L-alpha1 emission,                                    !
!         or Lalpha1/(L3-Xray total)                                   !
!  Intensity are adjusted to experimental data by S.I.Salem et al.     !
!  Atomic Data and Nuclear Data Table 14,91-109(1974) using the        !
!  theoretical data by J.M. Scofield. Atomic Data and Nuclear Data     !
!  Tables 14,121-137(1974).                                            !
!----------------------------------------------------------------------!
      DATA (DFLX3(1,IIZ),IIZ=1,100)/
     *0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,
     *0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,
     *0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,
     *0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,
     *0.45307D+00,0.62426D+00,0.70836D+00,0.77277D+00,0.78685D+00,
     *0.80487D+00,0.81662D+00,0.82828D+00,0.83607D+00,0.84040D+00,
     *0.84313D+00,0.84605D+00,0.84833D+00,0.85067D+00,0.85225D+00,
     *0.85387D+00,0.85491D+00,0.85585D+00,0.85255D+00,0.85427D+00,
     *0.83897D+00,0.82416D+00,0.81042D+00,0.79713D+00,0.78971D+00,
     *0.78245D+00,0.77482D+00,0.76732D+00,0.76250D+00,0.75773D+00,
     *0.75192D+00,0.74620D+00,0.74221D+00,0.73826D+00,0.73464D+00,
     *0.73106D+00,0.72912D+00,0.72875D+00,0.72768D+00,0.72738D+00,
     *0.72788D+00,0.72838D+00,0.72872D+00,0.72849D+00,0.72964D+00,
     *0.73021D+00,0.73102D+00,0.73182D+00,0.73305D+00,0.73428D+00,
     *0.72827D+00,0.72211D+00,0.71688D+00,0.71221D+00,0.70770D+00,
     *0.70325D+00,0.69951D+00,0.69582D+00,0.69234D+00,0.68890D+00,
     *0.68608D+00,0.68329D+00,0.68073D+00,0.67818D+00,0.67530D+00,
     *0.67243D+00,0.66988D+00,0.66734D+00,0.66503D+00,0.66275D+00,
     *0.66065D+00,0.65857D+00,0.65667D+00,0.65477D+00,0.65265D+00,
     *0.65054D+00,0.64845D+00,0.64636D+00,0.64430D+00,0.64224D+00/
!----------------------------------------------------------------------!
! Probability of L-alpha2 emission,                                    !
!         or DFLX3(1)+Lalpha2/(L3-Xray total)                          !
!  Intensity are adjusted to experimental data by S.I.Salem et al.     !
!  Atomic Data and Nuclear Data Table 14,91-109(1974) using the        !
!  theoretical data by J.M. Scofield. Atomic Data and Nuclear Data     !
!  Tables 14,121-137(1974).                                            !
!----------------------------------------------------------------------!
      DATA (DFLX3(2,IIZ),IIZ=1,100)/
     *0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,
     *0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,
     *0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,
     *0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,
     *0.50371D+00,0.69416D+00,0.78767D+00,0.85955D+00,0.87507D+00,
     *0.89521D+00,0.90837D+00,0.92156D+00,0.93051D+00,0.93519D+00,
     *0.93809D+00,0.94130D+00,0.94381D+00,0.94639D+00,0.94816D+00,
     *0.94995D+00,0.95108D+00,0.95223D+00,0.94846D+00,0.94823D+00,
     *0.93126D+00,0.91482D+00,0.90005D+00,0.88577D+00,0.87753D+00,
     *0.86946D+00,0.86098D+00,0.85265D+00,0.84732D+00,0.84206D+00,
     *0.83561D+00,0.82925D+00,0.82482D+00,0.82043D+00,0.81641D+00,
     *0.81243D+00,0.81031D+00,0.80993D+00,0.80874D+00,0.80841D+00,
     *0.80896D+00,0.80952D+00,0.80990D+00,0.80965D+00,0.81092D+00,
     *0.81156D+00,0.81249D+00,0.81342D+00,0.81478D+00,0.81615D+00,
     *0.80947D+00,0.80242D+00,0.79684D+00,0.79169D+00,0.78668D+00,
     *0.78173D+00,0.77762D+00,0.77354D+00,0.76967D+00,0.76585D+00,
     *0.76272D+00,0.75961D+00,0.75676D+00,0.75393D+00,0.75076D+00,
     *0.74761D+00,0.74477D+00,0.74194D+00,0.73938D+00,0.73684D+00,
     *0.73451D+00,0.73220D+00,0.73008D+00,0.72798D+00,0.72562D+00,
     *0.72327D+00,0.72094D+00,0.71863D+00,0.71633D+00,0.71404D+00/
!----------------------------------------------------------------------!
! Probability of L-beta2 emission,                                     !
!         or DFLX3(2)+Lbeta2/(L3-Xray total)                           !
!  Intensity are adjusted to experimental data by S.I.Salem et al.     !
!  Atomic Data and Nuclear Data Table 14,91-109(1974) using the        !
!  theoretical data by J.M. Scofield. Atomic Data and Nuclear Data     !
!  Tables 14,121-137(1974).                                            !
!----------------------------------------------------------------------!
      DATA (DFLX3(3,IIZ),IIZ=1,100)/
     *0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,
     *0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,
     *0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,
     *0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,
     *0.50371D+00,0.69416D+00,0.78767D+00,0.85955D+00,0.87507D+00,
     *0.89521D+00,0.90837D+00,0.92156D+00,0.93051D+00,0.93519D+00,
     *0.93809D+00,0.94130D+00,0.94381D+00,0.94639D+00,0.94816D+00,
     *0.94995D+00,0.95108D+00,0.95223D+00,0.95281D+00,0.95361D+00,
     *0.95337D+00,0.95309D+00,0.95272D+00,0.95236D+00,0.95236D+00,
     *0.95212D+00,0.95188D+00,0.95143D+00,0.95123D+00,0.95102D+00,
     *0.95051D+00,0.95000D+00,0.94960D+00,0.94922D+00,0.94877D+00,
     *0.94833D+00,0.94694D+00,0.94755D+00,0.94725D+00,0.94796D+00,
     *0.94775D+00,0.94755D+00,0.94723D+00,0.94614D+00,0.94654D+00,
     *0.94619D+00,0.94576D+00,0.94533D+00,0.94481D+00,0.94428D+00,
     *0.94288D+00,0.94063D+00,0.93895D+00,0.93741D+00,0.93359D+00,
     *0.92982D+00,0.92682D+00,0.92384D+00,0.92080D+00,0.91779D+00,
     *0.91507D+00,0.91238D+00,0.90989D+00,0.90740D+00,0.90505D+00,
     *0.90268D+00,0.90022D+00,0.89782D+00,0.89548D+00,0.89316D+00,
     *0.89104D+00,0.88897D+00,0.88718D+00,0.88544D+00,0.88335D+00,
     *0.88130D+00,0.87927D+00,0.87720D+00,0.87599D+00,0.87315D+00/
!----------------------------------------------------------------------!
! Probability of L-beta5 emission,                                     !
!         or DFLX3(3)+Lbeta5/(L3-Xray total)                           !
!  Intensity are adjusted to experimental data by S.I.Salem et al.     !
!  Atomic Data and Nuclear Data Table 14,91-109(1974) using the        !
!  theoretical data by J.M. Scofield. Atomic Data and Nuclear Data     !
!  Tables 14,121-137(1974).                                            !
!----------------------------------------------------------------------!
      DATA (DFLX3(4,IIZ),IIZ=1,100)/
     *0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,
     *0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,
     *0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,
     *0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,
     *0.50371D+00,0.69416D+00,0.78767D+00,0.85955D+00,0.87507D+00,
     *0.89521D+00,0.90837D+00,0.92156D+00,0.93051D+00,0.93519D+00,
     *0.93809D+00,0.94130D+00,0.94381D+00,0.94639D+00,0.94816D+00,
     *0.94995D+00,0.95108D+00,0.95223D+00,0.95281D+00,0.95361D+00,
     *0.95337D+00,0.95309D+00,0.95272D+00,0.95236D+00,0.95236D+00,
     *0.95212D+00,0.95188D+00,0.95143D+00,0.95123D+00,0.95102D+00,
     *0.95051D+00,0.95000D+00,0.94960D+00,0.94922D+00,0.94877D+00,
     *0.94833D+00,0.94796D+00,0.94755D+00,0.94725D+00,0.94796D+00,
     *0.94775D+00,0.94755D+00,0.94723D+00,0.94693D+00,0.94654D+00,
     *0.94619D+00,0.94576D+00,0.94533D+00,0.94481D+00,0.94428D+00,
     *0.94345D+00,0.94279D+00,0.94182D+00,0.94098D+00,0.94003D+00,
     *0.93910D+00,0.93836D+00,0.93762D+00,0.93672D+00,0.93584D+00,
     *0.93507D+00,0.93431D+00,0.93351D+00,0.93270D+00,0.93200D+00,
     *0.93126D+00,0.93030D+00,0.92938D+00,0.92843D+00,0.92749D+00,
     *0.92658D+00,0.92571D+00,0.92494D+00,0.92420D+00,0.92310D+00,
     *0.92203D+00,0.92096D+00,0.91986D+00,0.91961D+00,0.91772D+00/
!----------------------------------------------------------------------!
! Probability of L-beta6 emission,                                     !
!         or DFLX3(4)+Lbeta6/(L3-Xray total)                           !
!  Intensity are adjusted to experimental data by S.I.Salem et al.     !
!  Atomic Data and Nuclear Data Table 14,91-109(1974) using the        !
!  theoretical data by J.M. Scofield. Atomic Data and Nuclear Data     !
!  Tables 14,121-137(1974).                                            !
!----------------------------------------------------------------------!
      DATA (DFLX3(5,IIZ),IIZ=1,100)/
     *0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,
     *0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,
     *0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,
     *0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,
     *0.53900D+00,0.71552D+00,0.80212D+00,0.86324D+00,0.88310D+00,
     *0.90153D+00,0.91348D+00,0.92581D+00,0.93184D+00,0.93828D+00,
     *0.94184D+00,0.94540D+00,0.94818D+00,0.95098D+00,0.95292D+00,
     *0.95488D+00,0.95635D+00,0.95782D+00,0.95858D+00,0.95957D+00,
     *0.95931D+00,0.95907D+00,0.95870D+00,0.95835D+00,0.95839D+00,
     *0.95816D+00,0.95799D+00,0.95762D+00,0.95751D+00,0.95741D+00,
     *0.95700D+00,0.95658D+00,0.95630D+00,0.95603D+00,0.95570D+00,
     *0.95538D+00,0.95515D+00,0.95478D+00,0.95457D+00,0.95432D+00,
     *0.95431D+00,0.95429D+00,0.95421D+00,0.95414D+00,0.95398D+00,
     *0.95386D+00,0.95369D+00,0.95353D+00,0.95320D+00,0.95287D+00,
     *0.95211D+00,0.95151D+00,0.95064D+00,0.94988D+00,0.94930D+00,
     *0.94874D+00,0.94815D+00,0.94757D+00,0.94687D+00,0.94617D+00,
     *0.94557D+00,0.94497D+00,0.94434D+00,0.94368D+00,0.94314D+00,
     *0.94255D+00,0.94182D+00,0.94113D+00,0.94034D+00,0.93955D+00,
     *0.93884D+00,0.93816D+00,0.93755D+00,0.93697D+00,0.93602D+00,
     *0.93510D+00,0.93419D+00,0.93324D+00,0.93314D+00,0.93140D+00/
!----------------------------------------------------------------------!
! Probability of L-beta15 emission,                                    !
!         or DFLX3(5)+Lbeta15/(L3-Xray total)                          !
!  Intensity are adjusted to experimental data by S.I.Salem et al.     !
!  Atomic Data and Nuclear Data Table 14,91-109(1974) using the        !
!  theoretical data by J.M. Scofield. Atomic Data and Nuclear Data     !
!  Tables 14,121-137(1974).                                            !
!----------------------------------------------------------------------!
      DATA (DFLX3(6,IIZ),IIZ=1,100)/
     *0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,
     *0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,
     *0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,
     *0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,
     *0.53900D+00,0.71552D+00,0.80212D+00,0.86324D+00,0.88310D+00,
     *0.90153D+00,0.91348D+00,0.92581D+00,0.93184D+00,0.93828D+00,
     *0.94184D+00,0.94540D+00,0.94818D+00,0.95098D+00,0.95292D+00,
     *0.95488D+00,0.95635D+00,0.95782D+00,0.95907D+00,0.96017D+00,
     *0.96182D+00,0.96341D+00,0.96467D+00,0.96590D+00,0.96687D+00,
     *0.96783D+00,0.96832D+00,0.96879D+00,0.96924D+00,0.96969D+00,
     *0.96992D+00,0.97015D+00,0.97031D+00,0.97046D+00,0.97053D+00,
     *0.97059D+00,0.97042D+00,0.97019D+00,0.97008D+00,0.96993D+00,
     *0.96983D+00,0.96972D+00,0.96955D+00,0.96940D+00,0.96914D+00,
     *0.96892D+00,0.96860D+00,0.96828D+00,0.96773D+00,0.96719D+00,
     *0.96701D+00,0.96694D+00,0.96649D+00,0.96612D+00,0.96566D+00,
     *0.96521D+00,0.96474D+00,0.96426D+00,0.96363D+00,0.96301D+00,
     *0.96244D+00,0.96187D+00,0.96125D+00,0.96064D+00,0.96013D+00,
     *0.95963D+00,0.95893D+00,0.95823D+00,0.95745D+00,0.95667D+00,
     *0.95597D+00,0.95526D+00,0.95470D+00,0.95414D+00,0.95320D+00,
     *0.95226D+00,0.95134D+00,0.95041D+00,0.94950D+00,0.94859D+00/
!-------------------------------------------------------------!
!  E-M average edge energy                                    !
!            (Calculated by ln M_ab=(sum n_i ln M_i)/sum n_i  !
!             by using M_i in Table 2. of Table of Isotopes,  !
!             Eighth  Edition)                                !
!-------------------------------------------------------------!
      DATA EMBIND/
     *0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,
     *0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,
     *0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,
     *0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,
     *0.14100D-01,0.10600D-01,0.83000D-02,0.89000D-02,0.11600D-01,
     *0.12700D-01,0.11800D-01,0.14100D-01,0.93000D-02,0.24400D-01,
     *0.40400D-01,0.57200D-01,0.74400D-01,0.94400D-01,0.11110D+00,
     *0.14110D+00,0.16180D+00,0.18920D+00,0.21780D+00,0.24460D+00,
     *0.27360D+00,0.30470D+00,0.33010D+00,0.36150D+00,0.39350D+00,
     *0.42560D+00,0.46240D+00,0.50440D+00,0.54890D+00,0.59580D+00,
     *0.64380D+00,0.69380D+00,0.74680D+00,0.80990D+00,0.86440D+00,
     *0.92590D+00,0.98300D+00,0.10411D+01,0.10957D+01,0.11485D+01,
     *0.12049D+01,0.12642D+01,0.13229D+01,0.13843D+01,0.14485D+01,
     *0.15100D+01,0.15740D+01,0.16401D+01,0.17087D+01,0.17757D+01,
     *0.18459D+01,0.19288D+01,0.20121D+01,0.20968D+01,0.21811D+01,
     *0.22693D+01,0.23616D+01,0.24543D+01,0.25506D+01,0.26528D+01,
     *0.27608D+01,0.28694D+01,0.29794D+01,0.30943D+01,0.32102D+01,
     *0.33297D+01,0.34563D+01,0.35849D+01,0.37160D+01,0.38475D+01,
     *0.39749D+01,0.41046D+01,0.42384D+01,0.43698D+01,0.45069D+01,
     *0.46479D+01,0.47889D+01,0.49329D+01,0.50791D+01,0.52282D+01/
!-------------------------------------------------------------!
!  K-L1L1 Energy  Calculated neglecting correction term       !
!  by using Atomic-Electron Binding Energy in Table 2. of     !
!  Table of Isotopes, Eighth  Edition.                        !
!-------------------------------------------------------------!
      DATA (EKAUG(1,IIZ),IIZ=1,100)/
     *0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,0.16280D+00,
     *0.24780D+00,0.35280D+00,0.47500D+00,0.61740D+00,0.77310D+00,
     *0.94550D+00,0.11262D+01,0.13242D+01,0.15415D+01,0.17669D+01,
     *0.20136D+01,0.22820D+01,0.25534D+01,0.28532D+01,0.31625D+01,
     *0.34920D+01,0.38390D+01,0.42087D+01,0.46000D+01,0.50010D+01,
     *0.54198D+01,0.58577D+01,0.63166D+01,0.67867D+01,0.72714D+01,
     *0.77717D+01,0.82745D+01,0.88137D+01,0.93500D+01,0.99097D+01,
     *0.10484D+02,0.11070D+02,0.11672D+02,0.12293D+02,0.12934D+02,
     *0.13590D+02,0.14269D+02,0.14959D+02,0.15669D+02,0.16396D+02,
     *0.17142D+02,0.17902D+02,0.18675D+02,0.19465D+02,0.20271D+02,
     *0.21095D+02,0.21935D+02,0.22793D+02,0.23659D+02,0.24556D+02,
     *0.25463D+02,0.26392D+02,0.27345D+02,0.28321D+02,0.29317D+02,
     *0.30328D+02,0.31361D+02,0.32415D+02,0.33488D+02,0.34580D+02,
     *0.35697D+02,0.36829D+02,0.37983D+02,0.39158D+02,0.40360D+02,
     *0.41573D+02,0.42809D+02,0.44053D+02,0.45325D+02,0.46623D+02,
     *0.47935D+02,0.49274D+02,0.50634D+02,0.52019D+02,0.53424D+02,
     *0.54837D+02,0.56283D+02,0.57751D+02,0.59244D+02,0.60760D+02,
     *0.62301D+02,0.63862D+02,0.65451D+02,0.67064D+02,0.68706D+02,
     *0.70386D+02,0.72086D+02,0.73815D+02,0.75583D+02,0.77366D+02,
     *0.79189D+02,0.81044D+02,0.82919D+02,0.84832D+02,0.86778D+02/
!-------------------------------------------------------------!
!  K-L1L2 Energy  Calculated neglecting correction term       !
!  by using Atomic-Electron Binding Energy in Table 2. of     !
!  Table of Isotopes, Eighth  Edition.                        !
!-------------------------------------------------------------!
      DATA (EKAUG(2,IIZ),IIZ=1,100)/
     *0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,0.17070D+00,
     *0.25940D+00,0.36800D+00,0.49640D+00,0.64280D+00,0.79990D+00,
     *0.97770D+00,0.11642D+01,0.13687D+01,0.15907D+01,0.18200D+01,
     *0.20774D+01,0.23506D+01,0.26290D+01,0.29340D+01,0.32503D+01,
     *0.35857D+01,0.39412D+01,0.43164D+01,0.47109D+01,0.51186D+01,
     *0.55448D+01,0.59897D+01,0.64528D+01,0.69318D+01,0.74222D+01,
     *0.79271D+01,0.84410D+01,0.89816D+01,0.95277D+01,0.10096D+02,
     *0.10677D+02,0.11271D+02,0.11882D+02,0.12510D+02,0.13159D+02,
     *0.13823D+02,0.14509D+02,0.15208D+02,0.15926D+02,0.16662D+02,
     *0.17416D+02,0.18185D+02,0.18966D+02,0.19764D+02,0.20579D+02,
     *0.21413D+02,0.22263D+02,0.23129D+02,0.24008D+02,0.24911D+02,
     *0.25828D+02,0.26768D+02,0.27730D+02,0.28715D+02,0.29721D+02,
     *0.30743D+02,0.31786D+02,0.32850D+02,0.33933D+02,0.35036D+02,
     *0.36162D+02,0.37306D+02,0.38470D+02,0.39657D+02,0.40868D+02,
     *0.42095D+02,0.43341D+02,0.44599D+02,0.45881D+02,0.47191D+02,
     *0.48518D+02,0.49868D+02,0.51242D+02,0.52639D+02,0.54054D+02,
     *0.55486D+02,0.56944D+02,0.58427D+02,0.59935D+02,0.61466D+02,
     *0.63021D+02,0.64597D+02,0.66199D+02,0.67829D+02,0.69485D+02,
     *0.71177D+02,0.72896D+02,0.74642D+02,0.76421D+02,0.78222D+02,
     *0.80064D+02,0.81929D+02,0.83821D+02,0.85749D+02,0.87711D+02/
!-------------------------------------------------------------!
!  K-L1L3 Energy  Calculated neglecting correction term       !
!  by using Atomic-Electron Binding Energy in Table 2. of     !
!  Table of Isotopes, Eighth  Edition.                        !
!-------------------------------------------------------------!
      DATA (EKAUG(3,IIZ),IIZ=1,100)/
     *0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,0.17070D+00,
     *0.25940D+00,0.36800D+00,0.49640D+00,0.64280D+00,0.80000D+00,
     *0.97770D+00,0.11642D+01,0.13692D+01,0.15913D+01,0.18209D+01,
     *0.20786D+01,0.23522D+01,0.26311D+01,0.29367D+01,0.32539D+01,
     *0.35902D+01,0.39472D+01,0.43240D+01,0.47201D+01,0.51297D+01,
     *0.55578D+01,0.60047D+01,0.64700D+01,0.69517D+01,0.74453D+01,
     *0.79540D+01,0.84721D+01,0.90171D+01,0.95681D+01,0.10142D+02,
     *0.10730D+02,0.11330D+02,0.11949D+02,0.12586D+02,0.13244D+02,
     *0.13917D+02,0.14614D+02,0.15325D+02,0.16055D+02,0.16804D+02,
     *0.17573D+02,0.18357D+02,0.19156D+02,0.19972D+02,0.20807D+02,
     *0.21661D+02,0.22533D+02,0.23424D+02,0.24329D+02,0.25258D+02,
     *0.26205D+02,0.27176D+02,0.28171D+02,0.29192D+02,0.30235D+02,
     *0.31297D+02,0.32381D+02,0.33490D+02,0.34621D+02,0.35774D+02,
     *0.36953D+02,0.38152D+02,0.39376D+02,0.40626D+02,0.41902D+02,
     *0.43199D+02,0.44519D+02,0.45854D+02,0.47218D+02,0.48614D+02,
     *0.50032D+02,0.51477D+02,0.52951D+02,0.54453D+02,0.55979D+02,
     *0.57526D+02,0.59109D+02,0.60720D+02,0.62362D+02,0.64035D+02,
     *0.65739D+02,0.67471D+02,0.69239D+02,0.71040D+02,0.72878D+02,
     *0.74758D+02,0.76676D+02,0.78632D+02,0.80630D+02,0.82664D+02,
     *0.84745D+02,0.86865D+02,0.89022D+02,0.91230D+02,0.93484D+02/
!-------------------------------------------------------------!
!  K-L2L2 Energy  Calculated neglecting correction term       !
!  by using Atomic-Electron Binding Energy in Table 2. of     !
!  Table of Isotopes, Eighth  Edition.                        !
!-------------------------------------------------------------!
      DATA (EKAUG(4,IIZ),IIZ=1,100)/
     *0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,0.17860D+00,
     *0.27100D+00,0.38320D+00,0.51780D+00,0.66820D+00,0.82670D+00,
     *0.10099D+01,0.12022D+01,0.14132D+01,0.16399D+01,0.18731D+01,
     *0.21412D+01,0.24192D+01,0.27046D+01,0.30148D+01,0.33381D+01,
     *0.36794D+01,0.40434D+01,0.44241D+01,0.48218D+01,0.52362D+01,
     *0.56698D+01,0.61217D+01,0.65890D+01,0.70769D+01,0.75730D+01,
     *0.80825D+01,0.86075D+01,0.91495D+01,0.97054D+01,0.10282D+02,
     *0.10871D+02,0.11472D+02,0.12091D+02,0.12727D+02,0.13384D+02,
     *0.14056D+02,0.14749D+02,0.15458D+02,0.16183D+02,0.16928D+02,
     *0.17690D+02,0.18467D+02,0.19257D+02,0.20064D+02,0.20888D+02,
     *0.21730D+02,0.22590D+02,0.23465D+02,0.24357D+02,0.25266D+02,
     *0.26193D+02,0.27143D+02,0.28115D+02,0.29110D+02,0.30126D+02,
     *0.31158D+02,0.32211D+02,0.33285D+02,0.34379D+02,0.35493D+02,
     *0.36627D+02,0.37782D+02,0.38957D+02,0.40156D+02,0.41376D+02,
     *0.42617D+02,0.43872D+02,0.45144D+02,0.46437D+02,0.47759D+02,
     *0.49101D+02,0.50463D+02,0.51850D+02,0.53258D+02,0.54685D+02,
     *0.56135D+02,0.57605D+02,0.59104D+02,0.60626D+02,0.62172D+02,
     *0.63741D+02,0.65332D+02,0.66947D+02,0.68594D+02,0.70264D+02,
     *0.71968D+02,0.73706D+02,0.75469D+02,0.77259D+02,0.79078D+02,
     *0.80939D+02,0.82814D+02,0.84723D+02,0.86666D+02,0.88644D+02/
!-------------------------------------------------------------!
!  K-L2L3 Energy  Calculated neglecting correction term       !
!  by using Atomic-Electron Binding Energy in Table 2. of     !
!  Table of Isotopes, Eighth  Edition.                        !
!-------------------------------------------------------------!
      DATA (EKAUG(5,IIZ),IIZ=1,100)/
     *0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,0.17860D+00,
     *0.27100D+00,0.38320D+00,0.51780D+00,0.66820D+00,0.82680D+00,
     *0.10099D+01,0.12022D+01,0.14137D+01,0.16405D+01,0.18740D+01,
     *0.21424D+01,0.24208D+01,0.27067D+01,0.30175D+01,0.33417D+01,
     *0.36839D+01,0.40494D+01,0.44317D+01,0.48310D+01,0.52473D+01,
     *0.56828D+01,0.61367D+01,0.66062D+01,0.70968D+01,0.75961D+01,
     *0.81094D+01,0.86386D+01,0.91850D+01,0.97458D+01,0.10328D+02,
     *0.10924D+02,0.11531D+02,0.12158D+02,0.12803D+02,0.13469D+02,
     *0.14150D+02,0.14854D+02,0.15574D+02,0.16312D+02,0.17070D+02,
     *0.17847D+02,0.18639D+02,0.19447D+02,0.20272D+02,0.21115D+02,
     *0.21979D+02,0.22860D+02,0.23760D+02,0.24679D+02,0.25613D+02,
     *0.26570D+02,0.27551D+02,0.28555D+02,0.29586D+02,0.30640D+02,
     *0.31712D+02,0.32806D+02,0.33925D+02,0.35066D+02,0.36230D+02,
     *0.37418D+02,0.38629D+02,0.39863D+02,0.41125D+02,0.42411D+02,
     *0.43721D+02,0.45051D+02,0.46399D+02,0.47774D+02,0.49182D+02,
     *0.50615D+02,0.52072D+02,0.53558D+02,0.55073D+02,0.56610D+02,
     *0.58175D+02,0.59769D+02,0.61396D+02,0.63053D+02,0.64741D+02,
     *0.66459D+02,0.68206D+02,0.69987D+02,0.71805D+02,0.73657D+02,
     *0.75549D+02,0.77486D+02,0.79459D+02,0.81468D+02,0.83520D+02,
     *0.85620D+02,0.87750D+02,0.89924D+02,0.92147D+02,0.94417D+02/
!-------------------------------------------------------------!
!  K-L3L3 Energy  Calculated neglecting correction term       !
!  by using Atomic-Electron Binding Energy in Table 2. of     !
!  Table of Isotopes, Eighth  Edition.                        !
!-------------------------------------------------------------!
      DATA (EKAUG(6,IIZ),IIZ=1,100)/
     *0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,0.17860D+00,
     *0.27100D+00,0.38320D+00,0.51780D+00,0.66820D+00,0.82690D+00,
     *0.10099D+01,0.12022D+01,0.14142D+01,0.16411D+01,0.18749D+01,
     *0.21436D+01,0.24224D+01,0.27088D+01,0.30202D+01,0.33453D+01,
     *0.36884D+01,0.40554D+01,0.44393D+01,0.48402D+01,0.52584D+01,
     *0.56958D+01,0.61517D+01,0.66234D+01,0.71167D+01,0.76192D+01,
     *0.81363D+01,0.86697D+01,0.92205D+01,0.97862D+01,0.10374D+02,
     *0.10976D+02,0.11591D+02,0.12225D+02,0.12878D+02,0.13553D+02,
     *0.14245D+02,0.14959D+02,0.15690D+02,0.16441D+02,0.17212D+02,
     *0.18004D+02,0.18812D+02,0.19636D+02,0.20480D+02,0.21343D+02,
     *0.22227D+02,0.23131D+02,0.24055D+02,0.25000D+02,0.25961D+02,
     *0.26947D+02,0.27959D+02,0.28996D+02,0.30062D+02,0.31153D+02,
     *0.32265D+02,0.33402D+02,0.34565D+02,0.35754D+02,0.36968D+02,
     *0.38208D+02,0.39476D+02,0.40770D+02,0.42094D+02,0.43445D+02,
     *0.44826D+02,0.46229D+02,0.47654D+02,0.49111D+02,0.50606D+02,
     *0.52129D+02,0.53681D+02,0.55267D+02,0.56888D+02,0.58535D+02,
     *0.60215D+02,0.61934D+02,0.63689D+02,0.65480D+02,0.67310D+02,
     *0.69177D+02,0.71080D+02,0.73027D+02,0.75016D+02,0.77050D+02,
     *0.79130D+02,0.81266D+02,0.83449D+02,0.85677D+02,0.87962D+02,
     *0.90301D+02,0.92686D+02,0.95125D+02,0.97628D+02,0.10019D+03/
!-------------------------------------------------------------!
!  K-L1M Energy  Calculated neglecting correction term        !
!  by using Atomic-Electron Binding Energy in Table 2. of     !
!  Table of Isotopes, Eighth  Edition.                        !
!-------------------------------------------------------------!
      DATA (EKAUG(7,IIZ),IIZ=1,100)/
     *0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,
     *0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,
     *0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,
     *0.00000D+00,0.00000D+00,0.28797D+01,0.32303D+01,0.36003D+01,
     *0.39783D+01,0.43921D+01,0.48286D+01,0.52857D+01,0.57584D+01,
     *0.62532D+01,0.67715D+01,0.73106D+01,0.78735D+01,0.84406D+01,
     *0.90290D+01,0.96316D+01,0.10266D+02,0.10910D+02,0.11581D+02,
     *0.12264D+02,0.12973D+02,0.13699D+02,0.14448D+02,0.15221D+02,
     *0.16014D+02,0.16829D+02,0.17671D+02,0.18532D+02,0.19415D+02,
     *0.20320D+02,0.21246D+02,0.22189D+02,0.23154D+02,0.24140D+02,
     *0.25149D+02,0.26181D+02,0.27235D+02,0.28302D+02,0.29406D+02,
     *0.30526D+02,0.31675D+02,0.32853D+02,0.34060D+02,0.35294D+02,
     *0.36551D+02,0.37833D+02,0.39144D+02,0.40479D+02,0.41839D+02,
     *0.43233D+02,0.44650D+02,0.46094D+02,0.47565D+02,0.49070D+02,
     *0.50598D+02,0.52151D+02,0.53723D+02,0.55328D+02,0.56969D+02,
     *0.58634D+02,0.60331D+02,0.62060D+02,0.63822D+02,0.65610D+02,
     *0.67423D+02,0.69274D+02,0.71159D+02,0.73078D+02,0.75032D+02,
     *0.77019D+02,0.79040D+02,0.81098D+02,0.83194D+02,0.85331D+02,
     *0.87516D+02,0.89739D+02,0.92004D+02,0.94317D+02,0.96667D+02,
     *0.99067D+02,0.10151D+03,0.10400D+03,0.10653D+03,0.10912D+03/
!-------------------------------------------------------------!
!  K-L2M Energy  Calculated neglecting correction term        !
!  by using Atomic-Electron Binding Energy in Table 2. of     !
!  Table of Isotopes, Eighth  Edition.                        !
!-------------------------------------------------------------!
      DATA (EKAUG(8,IIZ),IIZ=1,100)/
     *0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,
     *0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,
     *0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,
     *0.00000D+00,0.00000D+00,0.29553D+01,0.33111D+01,0.36881D+01,
     *0.40720D+01,0.44943D+01,0.49363D+01,0.53966D+01,0.58760D+01,
     *0.63782D+01,0.69035D+01,0.74468D+01,0.80186D+01,0.85914D+01,
     *0.91844D+01,0.97981D+01,0.10434D+02,0.11087D+02,0.11767D+02,
     *0.12457D+02,0.13174D+02,0.13909D+02,0.14665D+02,0.15446D+02,
     *0.16247D+02,0.17070D+02,0.17921D+02,0.18789D+02,0.19680D+02,
     *0.20594D+02,0.21528D+02,0.22480D+02,0.23453D+02,0.24448D+02,
     *0.25467D+02,0.26508D+02,0.27571D+02,0.28651D+02,0.29761D+02,
     *0.30891D+02,0.32051D+02,0.33238D+02,0.34455D+02,0.35699D+02,
     *0.36966D+02,0.38258D+02,0.39579D+02,0.40925D+02,0.42296D+02,
     *0.43698D+02,0.45126D+02,0.46581D+02,0.48064D+02,0.49578D+02,
     *0.51119D+02,0.52683D+02,0.54268D+02,0.55884D+02,0.57537D+02,
     *0.59216D+02,0.60925D+02,0.62668D+02,0.64441D+02,0.66241D+02,
     *0.68072D+02,0.69935D+02,0.71835D+02,0.73769D+02,0.75739D+02,
     *0.77739D+02,0.79775D+02,0.81846D+02,0.83959D+02,0.86110D+02,
     *0.88307D+02,0.90549D+02,0.92831D+02,0.95155D+02,0.97523D+02,
     *0.99942D+02,0.10240D+03,0.10490D+03,0.10745D+03,0.11006D+03/
!-------------------------------------------------------------!
!  K-L3M Energy  Calculated neglecting correction term        !
!  by using Atomic-Electron Binding Energy in Table 2. of     !
!  Table of Isotopes, Eighth  Edition.                        !
!-------------------------------------------------------------!
      DATA (EKAUG(9,IIZ),IIZ=1,100)/
     *0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,
     *0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,
     *0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,
     *0.00000D+00,0.00000D+00,0.29574D+01,0.33138D+01,0.36917D+01,
     *0.40765D+01,0.45003D+01,0.49439D+01,0.54058D+01,0.58871D+01,
     *0.63912D+01,0.69185D+01,0.74640D+01,0.80385D+01,0.86145D+01,
     *0.92113D+01,0.98292D+01,0.10469D+02,0.11128D+02,0.11813D+02,
     *0.12510D+02,0.13234D+02,0.13976D+02,0.14741D+02,0.15531D+02,
     *0.16342D+02,0.17175D+02,0.18037D+02,0.18918D+02,0.19823D+02,
     *0.20751D+02,0.21701D+02,0.22669D+02,0.23661D+02,0.24676D+02,
     *0.25715D+02,0.26779D+02,0.27866D+02,0.28972D+02,0.30108D+02,
     *0.31268D+02,0.32459D+02,0.33679D+02,0.34931D+02,0.36213D+02,
     *0.37520D+02,0.38854D+02,0.40219D+02,0.41612D+02,0.43033D+02,
     *0.44488D+02,0.45973D+02,0.47488D+02,0.49033D+02,0.50613D+02,
     *0.52224D+02,0.53861D+02,0.55523D+02,0.57221D+02,0.58960D+02,
     *0.60731D+02,0.62534D+02,0.64377D+02,0.66256D+02,0.68166D+02,
     *0.70112D+02,0.72100D+02,0.74128D+02,0.76196D+02,0.78307D+02,
     *0.80457D+02,0.82649D+02,0.84886D+02,0.87170D+02,0.89503D+02,
     *0.91888D+02,0.94329D+02,0.96821D+02,0.99364D+02,0.10197D+03,
     *0.10462D+03,0.10733D+03,0.11010D+03,0.11293D+03,0.11583D+03/
!-------------------------------------------------------------!
!  K-L1N Energy  Calculated neglecting correction term        !
!  by using Atomic-Electron Binding Energy in Table 2. of     !
!  Table of Isotopes, Eighth  Edition. Using average N        !
!  binding energy.                                            !
!-------------------------------------------------------------!
      DATA (EKAUG(10,IIZ),IIZ=1,100)/
     *0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,
     *0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,
     *0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,
     *0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,0.36003D+01,
     *0.39924D+01,0.44027D+01,0.48369D+01,0.52946D+01,0.57700D+01,
     *0.62659D+01,0.67833D+01,0.73247D+01,0.78828D+01,0.84650D+01,
     *0.90682D+01,0.96854D+01,0.10336D+02,0.10997D+02,0.11684D+02,
     *0.12388D+02,0.13118D+02,0.13865D+02,0.14621D+02,0.15428D+02,
     *0.16261D+02,0.17122D+02,0.17988D+02,0.18882D+02,0.19796D+02,
     *0.20738D+02,0.21696D+02,0.22670D+02,0.23668D+02,0.24691D+02,
     *0.25738D+02,0.26809D+02,0.27904D+02,0.29012D+02,0.30158D+02,
     *0.31323D+02,0.32519D+02,0.33790D+02,0.35071D+02,0.36373D+02,
     *0.37681D+02,0.39022D+02,0.40280D+02,0.41840D+02,0.43240D+02,
     *0.44690D+02,0.46176D+02,0.47686D+02,0.49224D+02,0.50795D+02,
     *0.52389D+02,0.53995D+02,0.55629D+02,0.57298D+02,0.59004D+02,
     *0.60746D+02,0.62516D+02,0.64320D+02,0.66156D+02,0.68021D+02,
     *0.69914D+02,0.71847D+02,0.73812D+02,0.75816D+02,0.77855D+02,
     *0.79931D+02,0.82042D+02,0.84189D+02,0.86379D+02,0.88610D+02,
     *0.90894D+02,0.93216D+02,0.95581D+02,0.97997D+02,0.10045D+03,
     *0.10296D+03,0.10551D+03,0.10811D+03,0.11076D+03,0.11346D+03/
!-------------------------------------------------------------!
!  K-L2N Energy  Calculated neglecting correction term        !
!  by using Atomic-Electron Binding Energy in Table 2. of     !
!  Table of Isotopes, Eighth  Edition. Using average N        !
!  binding energy.                                            !
!-------------------------------------------------------------!
       DATA (EKAUG(11,IIZ),IIZ=1,100)/
     *0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,
     *0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,
     *0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,
     *0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,0.36881D+01,
     *0.40861D+01,0.45049D+01,0.49446D+01,0.54055D+01,0.58876D+01,
     *0.63909D+01,0.69153D+01,0.74609D+01,0.80279D+01,0.86158D+01,
     *0.92236D+01,0.98519D+01,0.10504D+02,0.11174D+02,0.11870D+02,
     *0.12582D+02,0.13319D+02,0.14074D+02,0.14838D+02,0.15653D+02,
     *0.16494D+02,0.17362D+02,0.18237D+02,0.19139D+02,0.20062D+02,
     *0.21012D+02,0.21978D+02,0.22961D+02,0.23968D+02,0.24999D+02,
     *0.26056D+02,0.27136D+02,0.28240D+02,0.29361D+02,0.30513D+02,
     *0.31688D+02,0.32895D+02,0.34175D+02,0.35465D+02,0.36777D+02,
     *0.38096D+02,0.39447D+02,0.40715D+02,0.42285D+02,0.43696D+02,
     *0.45155D+02,0.46652D+02,0.48173D+02,0.49723D+02,0.51303D+02,
     *0.52911D+02,0.54527D+02,0.56174D+02,0.57854D+02,0.59572D+02,
     *0.61329D+02,0.63111D+02,0.64928D+02,0.66775D+02,0.68652D+02,
     *0.70562D+02,0.72507D+02,0.74488D+02,0.76507D+02,0.78561D+02,
     *0.80651D+02,0.82776D+02,0.84937D+02,0.87144D+02,0.89389D+02,
     *0.91685D+02,0.94026D+02,0.96408D+02,0.98835D+02,0.10131D+03,
     *0.10384D+03,0.10640D+03,0.10901D+03,0.11167D+03,0.11439D+03/
!-------------------------------------------------------------!
!  K-L3N Energy  Calculated neglecting correction term        !
!  by using Atomic-Electron Binding Energy in Table 2. of     !
!  Table of Isotopes, Eighth  Edition.   Using average N      !
!  binding energy.                                            !
!-------------------------------------------------------------!
      DATA (EKAUG(12,IIZ),IIZ=1,100)/
     *0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,
     *0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,
     *0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,
     *0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,0.36917D+01,
     *0.40906D+01,0.45109D+01,0.49522D+01,0.54147D+01,0.58987D+01,
     *0.64039D+01,0.69303D+01,0.74781D+01,0.80478D+01,0.86389D+01,
     *0.92505D+01,0.98830D+01,0.10536D+02,0.11215D+02,0.11916D+02,
     *0.12634D+02,0.13378D+02,0.14142D+02,0.14914D+02,0.15738D+02,
     *0.16588D+02,0.17467D+02,0.18354D+02,0.19268D+02,0.20204D+02,
     *0.21169D+02,0.22150D+02,0.23150D+02,0.24176D+02,0.25226D+02,
     *0.26304D+02,0.27407D+02,0.28535D+02,0.29682D+02,0.30860D+02,
     *0.32065D+02,0.33304D+02,0.34616D+02,0.35941D+02,0.37291D+02,
     *0.38649D+02,0.40043D+02,0.41355D+02,0.42973D+02,0.44434D+02,
     *0.45945D+02,0.47499D+02,0.49080D+02,0.50692D+02,0.52338D+02,
     *0.54015D+02,0.55705D+02,0.57429D+02,0.59191D+02,0.60995D+02,
     *0.62843D+02,0.64719D+02,0.66636D+02,0.68590D+02,0.70577D+02,
     *0.72603D+02,0.74672D+02,0.76781D+02,0.78934D+02,0.81130D+02,
     *0.83369D+02,0.85650D+02,0.87977D+02,0.90355D+02,0.92782D+02,
     *0.95266D+02,0.97806D+02,0.10040D+03,0.10304D+03,0.10575D+03,
     *0.10852D+03,0.11133D+03,0.11421D+03,0.11715D+03,0.12017D+03/
!-------------------------------------------------------------!
!  K-MM Energy  Calculated neglecting correction term         !
!  by using Atomic-Electron Binding Energy in Table 2. of     !
!  Table of Isotopes, Eighth  Edition.  Using average M       !
!  binding energy.                                            !
!-------------------------------------------------------------!
      DATA (EKAUG(13,IIZ),IIZ=1,100)/
     *0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,
     *0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,
     *0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,
     *0.00000D+00,0.00000D+00,0.31690D+01,0.35656D+01,0.39799D+01,
     *0.44646D+01,0.49452D+01,0.54485D+01,0.59714D+01,0.65158D+01,
     *0.70866D+01,0.76853D+01,0.83046D+01,0.89603D+01,0.96098D+01,
     *0.10286D+02,0.10989D+02,0.11718D+02,0.12469D+02,0.13252D+02,
     *0.14043D+02,0.14876D+02,0.15726D+02,0.16603D+02,0.17508D+02,
     *0.18438D+02,0.19390D+02,0.20384D+02,0.21394D+02,0.22433D+02,
     *0.23499D+02,0.24589D+02,0.25702D+02,0.26842D+02,0.28009D+02,
     *0.29204D+02,0.30426D+02,0.31676D+02,0.32945D+02,0.34256D+02,
     *0.35589D+02,0.36959D+02,0.38361D+02,0.39799D+02,0.41272D+02,
     *0.42774D+02,0.44306D+02,0.45873D+02,0.47471D+02,0.49099D+02,
     *0.50769D+02,0.52470D+02,0.54205D+02,0.55972D+02,0.57781D+02,
     *0.59622D+02,0.61493D+02,0.63392D+02,0.65331D+02,0.67314D+02,
     *0.69332D+02,0.71388D+02,0.73486D+02,0.75624D+02,0.77797D+02,
     *0.80009D+02,0.82266D+02,0.84567D+02,0.86911D+02,0.89304D+02,
     *0.91738D+02,0.94217D+02,0.96745D+02,0.99324D+02,0.10196D+03,
     *0.10465D+03,0.10739D+03,0.11019D+03,0.11305D+03,0.11597D+03,
     *0.11895D+03,0.12198D+03,0.12507D+03,0.12824D+03,0.13147D+03/
!-------------------------------------------------------------!
!  K-MN Energy  Calculated neglecting correction term         !
!  by using Atomic-Electron Binding Energy in Table 2. of     !
!  Table of Isotopes, Eighth  Edition.   Using average M, N   !
!  binding energy.                                            !
!-------------------------------------------------------------!
      DATA (EKAUG(14,IIZ),IIZ=1,100)/
     *0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,
     *0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,
     *0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,
     *0.00000D+00,0.00000D+00,0.31875D+01,0.35865D+01,0.40090D+01,
     *0.44787D+01,0.49558D+01,0.54568D+01,0.59803D+01,0.65274D+01,
     *0.70993D+01,0.76971D+01,0.83187D+01,0.89696D+01,0.96342D+01,
     *0.10325D+02,0.11043D+02,0.11788D+02,0.12556D+02,0.13355D+02,
     *0.14168D+02,0.15021D+02,0.15892D+02,0.16776D+02,0.17715D+02,
     *0.18685D+02,0.19683D+02,0.20700D+02,0.21745D+02,0.22815D+02,
     *0.23917D+02,0.25039D+02,0.26183D+02,0.27357D+02,0.28559D+02,
     *0.29793D+02,0.31054D+02,0.32345D+02,0.33655D+02,0.35008D+02,
     *0.36386D+02,0.37803D+02,0.39298D+02,0.40810D+02,0.42350D+02,
     *0.43904D+02,0.45495D+02,0.47009D+02,0.48831D+02,0.50499D+02,
     *0.52226D+02,0.53996D+02,0.55798D+02,0.57631D+02,0.59505D+02,
     *0.61413D+02,0.63337D+02,0.65298D+02,0.67301D+02,0.69350D+02,
     *0.71444D+02,0.73573D+02,0.75746D+02,0.77958D+02,0.80208D+02,
     *0.82500D+02,0.84838D+02,0.87220D+02,0.89649D+02,0.92127D+02,
     *0.94649D+02,0.97218D+02,0.99836D+02,0.10251D+03,0.10523D+03,
     *0.10802D+03,0.11087D+03,0.11377D+03,0.11673D+03,0.11975D+03,
     *0.12284D+03,0.12598D+03,0.12918D+03,0.13246D+03,0.13580D+03/
!-------------------------------------------------------------!
!  L1-MM Energy  Calculated neglecting correction term        !
!  by using Atomic-Electron Binding Energy in Table 2. of     !
!  Table of Isotopes, Eighth  Edition.   Using average M      !
!  binding energy.                                            !
!-------------------------------------------------------------!
      DATA (EL1AUG(1,IIZ),IIZ=1,100)/
     *0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,
     *0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,
     *0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,
     *0.00000D+00,0.00000D+00,0.28930D+00,0.33530D+00,0.37960D+00,
     *0.47220D+00,0.54250D+00,0.61160D+00,0.67680D+00,0.74580D+00,
     *0.82070D+00,0.90200D+00,0.97990D+00,0.10775D+01,0.11448D+01,
     *0.12169D+01,0.12999D+01,0.13777D+01,0.14651D+01,0.15598D+01,
     *0.16388D+01,0.17415D+01,0.18379D+01,0.19369D+01,0.20424D+01,
     *0.21505D+01,0.22561D+01,0.23823D+01,0.25010D+01,0.26249D+01,
     *0.27531D+01,0.28810D+01,0.30092D+01,0.31397D+01,0.32731D+01,
     *0.34107D+01,0.35516D+01,0.36945D+01,0.38330D+01,0.39855D+01,
     *0.41370D+01,0.43003D+01,0.44666D+01,0.46434D+01,0.48290D+01,
     *0.50181D+01,0.52084D+01,0.54062D+01,0.56070D+01,0.58110D+01,
     *0.60258D+01,0.62462D+01,0.64711D+01,0.66983D+01,0.69350D+01,
     *0.71786D+01,0.74131D+01,0.76573D+01,0.79062D+01,0.81645D+01,
     *0.84294D+01,0.86953D+01,0.89719D+01,0.92516D+01,0.95337D+01,
     *0.98251D+01,0.10122D+02,0.10429D+02,0.10739D+02,0.11062D+02,
     *0.11389D+02,0.11721D+02,0.12062D+02,0.12414D+02,0.12777D+02,
     *0.13155D+02,0.13549D+02,0.13950D+02,0.14364D+02,0.14794D+02,
     *0.15230D+02,0.15678D+02,0.16144D+02,0.16624D+02,0.17118D+02/
!-------------------------------------------------------------!
!  L1-MN Energy  Calculated neglecting correction term        !
!  by using Atomic-Electron Binding Energy in Table 2. of     !
!  Table of Isotopes, Eighth  Edition.   Using average M, N   !
!  binding energy.                                            !
!-------------------------------------------------------------!
      DATA (EL1AUG(2,IIZ),IIZ=1,100)/
     *0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,
     *0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,
     *0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,
     *0.00000D+00,0.00000D+00,0.30780D+00,0.35620D+00,0.40870D+00,
     *0.48630D+00,0.55310D+00,0.61990D+00,0.68570D+00,0.75740D+00,
     *0.83340D+00,0.91380D+00,0.99400D+00,0.10868D+01,0.11692D+01,
     *0.12561D+01,0.13537D+01,0.14480D+01,0.15523D+01,0.16633D+01,
     *0.17633D+01,0.18862D+01,0.20038D+01,0.21098D+01,0.22495D+01,
     *0.23967D+01,0.25486D+01,0.26989D+01,0.28514D+01,0.30067D+01,
     *0.31711D+01,0.33309D+01,0.34900D+01,0.36545D+01,0.38240D+01,
     *0.39998D+01,0.41798D+01,0.43636D+01,0.45432D+01,0.47374D+01,
     *0.49341D+01,0.51444D+01,0.54040D+01,0.56542D+01,0.59076D+01,
     *0.61478D+01,0.63973D+01,0.65419D+01,0.69678D+01,0.72114D+01,
     *0.74829D+01,0.77727D+01,0.80634D+01,0.83573D+01,0.86595D+01,
     *0.89699D+01,0.92571D+01,0.95630D+01,0.98758D+01,0.10200D+02,
     *0.10541D+02,0.10881D+02,0.11232D+02,0.11586D+02,0.11945D+02,
     *0.12316D+02,0.12694D+02,0.13082D+02,0.13477D+02,0.13885D+02,
     *0.14300D+02,0.14722D+02,0.15153D+02,0.15599D+02,0.16056D+02,
     *0.16533D+02,0.17025D+02,0.17527D+02,0.18044D+02,0.18579D+02,
     *0.19124D+02,0.19678D+02,0.20254D+02,0.20845D+02,0.21453D+02/
!-------------------------------------------------------------!
!  L1-MO Energy  Calculated neglecting correction term        !
!  by using Atomic-Electron Binding Energy in Table 2. of     !
!  Table of Isotopes, Eighth  Edition.   Using average M      !
!  binding energy.  Neglect O binding energy.                 !
!-------------------------------------------------------------!
      DATA (EL1AUG(3,IIZ),IIZ=1,100)/
     *0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,
     *0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,
     *0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,
     *0.00000D+00,0.00000D+00,0.30780D+00,0.35620D+00,0.40870D+00,
     *0.48630D+00,0.55310D+00,0.61990D+00,0.68570D+00,0.75740D+00,
     *0.83340D+00,0.91380D+00,0.99400D+00,0.10868D+01,0.11692D+01,
     *0.12573D+01,0.13571D+01,0.14521D+01,0.15595D+01,0.16709D+01,
     *0.17799D+01,0.19033D+01,0.20271D+01,0.21547D+01,0.22870D+01,
     *0.24241D+01,0.25608D+01,0.27124D+01,0.28625D+01,0.30184D+01,
     *0.31787D+01,0.33434D+01,0.35136D+01,0.36886D+01,0.38689D+01,
     *0.40545D+01,0.42454D+01,0.44413D+01,0.46429D+01,0.48499D+01,
     *0.50629D+01,0.52833D+01,0.55077D+01,0.57391D+01,0.59775D+01,
     *0.62230D+01,0.64726D+01,0.67291D+01,0.69913D+01,0.72595D+01,
     *0.75358D+01,0.78202D+01,0.81112D+01,0.84070D+01,0.87107D+01,
     *0.90245D+01,0.93419D+01,0.96694D+01,0.10003D+02,0.10346D+02,
     *0.10699D+02,0.11057D+02,0.11426D+02,0.11802D+02,0.12187D+02,
     *0.12586D+02,0.12991D+02,0.13408D+02,0.13834D+02,0.14272D+02,
     *0.14718D+02,0.15178D+02,0.15647D+02,0.16130D+02,0.16625D+02,
     *0.17130D+02,0.17653D+02,0.18189D+02,0.18734D+02,0.19301D+02,
     *0.19878D+02,0.20467D+02,0.21077D+02,0.21703D+02,0.22346D+02/
!-------------------------------------------------------------!
!  L1-NN Energy  Calculated neglecting correction term        !
!  by using Atomic-Electron Binding Energy in Table 2. of     !
!  Table of Isotopes, Eighth  Edition.   Using average N      !
!  binding energy.                                            !
!-------------------------------------------------------------!
      DATA (EL1AUG(4,IIZ),IIZ=1,100)/
     *0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,
     *0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,
     *0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,
     *0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,
     *0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,
     *0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,
     *0.12953D+01,0.14075D+01,0.15183D+01,0.16395D+01,0.17667D+01,
     *0.18879D+01,0.20310D+01,0.21696D+01,0.22827D+01,0.24565D+01,
     *0.26429D+01,0.28412D+01,0.30154D+01,0.32019D+01,0.33885D+01,
     *0.35890D+01,0.37809D+01,0.39709D+01,0.41692D+01,0.43749D+01,
     *0.45888D+01,0.48080D+01,0.50327D+01,0.52533D+01,0.54893D+01,
     *0.57312D+01,0.59884D+01,0.63414D+01,0.66650D+01,0.69862D+01,
     *0.72774D+01,0.75861D+01,0.76776D+01,0.83286D+01,0.86119D+01,
     *0.89400D+01,0.92993D+01,0.96557D+01,0.10016D+02,0.10384D+02,
     *0.10761D+02,0.11101D+02,0.11469D+02,0.11845D+02,0.12235D+02,
     *0.12653D+02,0.13066D+02,0.13491D+02,0.13921D+02,0.14356D+02,
     *0.14807D+02,0.15267D+02,0.15735D+02,0.16215D+02,0.16709D+02,
     *0.17212D+02,0.17723D+02,0.18244D+02,0.18783D+02,0.19336D+02,
     *0.19911D+02,0.20502D+02,0.21104D+02,0.21724D+02,0.22365D+02,
     *0.23018D+02,0.23678D+02,0.24364D+02,0.25067D+02,0.25788D+02/
!-------------------------------------------------------------!
!  L1-NO Energy  Calculated neglecting correction term        !
!  by using Atomic-Electron Binding Energy in Table 2. of     !
!  Table of Isotopes, Eighth  Edition.   Using average N      !
!  binding energy.  Neglect O binding energy.                 !
!-------------------------------------------------------------!
      DATA (EL1AUG(5,IIZ),IIZ=1,100)/
     *0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,
     *0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,
     *0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,
     *0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,
     *0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,
     *0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,
     *0.12965D+01,0.14109D+01,0.15224D+01,0.16467D+01,0.17744D+01,
     *0.19044D+01,0.20480D+01,0.21930D+01,0.23276D+01,0.24941D+01,
     *0.26703D+01,0.28533D+01,0.30290D+01,0.32129D+01,0.34002D+01,
     *0.35967D+01,0.37933D+01,0.39944D+01,0.42034D+01,0.44198D+01,
     *0.46436D+01,0.48736D+01,0.51104D+01,0.53531D+01,0.56018D+01,
     *0.58600D+01,0.61274D+01,0.64451D+01,0.67499D+01,0.70561D+01,
     *0.73527D+01,0.76615D+01,0.78648D+01,0.83521D+01,0.86599D+01,
     *0.89929D+01,0.93467D+01,0.97035D+01,0.10066D+02,0.10435D+02,
     *0.10816D+02,0.11186D+02,0.11575D+02,0.11973D+02,0.12381D+02,
     *0.12811D+02,0.13242D+02,0.13686D+02,0.14137D+02,0.14598D+02,
     *0.15077D+02,0.15564D+02,0.16061D+02,0.16572D+02,0.17095D+02,
     *0.17630D+02,0.18179D+02,0.18738D+02,0.19315D+02,0.19904D+02,
     *0.20508D+02,0.21130D+02,0.21766D+02,0.22414D+02,0.23086D+02,
     *0.23772D+02,0.24467D+02,0.25187D+02,0.25924D+02,0.26681D+02/
!-------------------------------------------------------------!
!  L1-OO Energy  Calculated neglecting correction term        !
!  by using Atomic-Electron Binding Energy in Table 2. of     !
!  Table of Isotopes, Eighth  Edition.   Neglect O binding    !
!  energy.                                                    !
!-------------------------------------------------------------!
      DATA (EL1AUG(6,IIZ),IIZ=1,100)/
     *0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,
     *0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,
     *0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,
     *0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,
     *0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,
     *0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,
     *0.12977D+01,0.14143D+01,0.15265D+01,0.16539D+01,0.17820D+01,
     *0.19210D+01,0.20651D+01,0.22163D+01,0.23725D+01,0.25316D+01,
     *0.26977D+01,0.28655D+01,0.30425D+01,0.32240D+01,0.34119D+01,
     *0.36043D+01,0.38058D+01,0.40180D+01,0.42375D+01,0.44647D+01,
     *0.46983D+01,0.49392D+01,0.51881D+01,0.54528D+01,0.57143D+01,
     *0.59888D+01,0.62663D+01,0.65488D+01,0.68348D+01,0.71260D+01,
     *0.74279D+01,0.77368D+01,0.80520D+01,0.83756D+01,0.87080D+01,
     *0.90458D+01,0.93942D+01,0.97513D+01,0.10116D+02,0.10486D+02,
     *0.10870D+02,0.11271D+02,0.11682D+02,0.12100D+02,0.12527D+02,
     *0.12968D+02,0.13419D+02,0.13881D+02,0.14353D+02,0.14839D+02,
     *0.15347D+02,0.15861D+02,0.16388D+02,0.16928D+02,0.17482D+02,
     *0.18048D+02,0.18634D+02,0.19232D+02,0.19846D+02,0.20472D+02,
     *0.21105D+02,0.21758D+02,0.22427D+02,0.23104D+02,0.23808D+02,
     *0.24526D+02,0.25256D+02,0.26010D+02,0.26782D+02,0.27574D+02/
!-------------------------------------------------------------!
!  L2-MM Energy  Calculated neglecting correction term        !
!  by using Atomic-Electron Binding Energy in Table 2. of     !
!  Table of Isotopes, Eighth  Edition.   Using average M      !
!  binding energy.                                            !
!-------------------------------------------------------------!
      DATA (EL2AUG(1,IIZ),IIZ=1,100)/
     *0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,
     *0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,
     *0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,
     *0.00000D+00,0.00000D+00,0.21370D+00,0.25450D+00,0.29180D+00,
     *0.37850D+00,0.44030D+00,0.50390D+00,0.56590D+00,0.62820D+00,
     *0.69570D+00,0.77000D+00,0.84370D+00,0.93240D+00,0.99400D+00,
     *0.10615D+01,0.11334D+01,0.12098D+01,0.12874D+01,0.13738D+01,
     *0.14450D+01,0.15403D+01,0.16284D+01,0.17199D+01,0.18175D+01,
     *0.19175D+01,0.20157D+01,0.21330D+01,0.22439D+01,0.23591D+01,
     *0.24791D+01,0.25989D+01,0.27182D+01,0.28402D+01,0.29645D+01,
     *0.30928D+01,0.32244D+01,0.33585D+01,0.34839D+01,0.36306D+01,
     *0.37718D+01,0.39246D+01,0.40820D+01,0.42490D+01,0.44245D+01,
     *0.46030D+01,0.47834D+01,0.49713D+01,0.51617D+01,0.53546D+01,
     *0.55606D+01,0.57698D+01,0.59841D+01,0.61995D+01,0.64268D+01,
     *0.66568D+01,0.68818D+01,0.71119D+01,0.73504D+01,0.75965D+01,
     *0.78464D+01,0.81009D+01,0.83640D+01,0.86324D+01,0.89031D+01,
     *0.91763D+01,0.94612D+01,0.97523D+01,0.10048D+02,0.10356D+02,
     *0.10669D+02,0.10986D+02,0.11314D+02,0.11649D+02,0.11998D+02,
     *0.12364D+02,0.12739D+02,0.13123D+02,0.13526D+02,0.13938D+02,
     *0.14355D+02,0.14793D+02,0.15242D+02,0.15707D+02,0.16185D+02/
!-------------------------------------------------------------!
!  L2-MN Energy  Calculated neglecting correction term        !
!  by using Atomic-Electron Binding Energy in Table 2. of     !
!  Table of Isotopes, Eighth  Edition.   Using average M, N   !
!  binding energy.                                            !
!-------------------------------------------------------------!
      DATA (EL2AUG(2,IIZ),IIZ=1,100)/
     *0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,
     *0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,
     *0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,
     *0.00000D+00,0.00000D+00,0.23220D+00,0.27540D+00,0.32090D+00,
     *0.39260D+00,0.45090D+00,0.51220D+00,0.57480D+00,0.63980D+00,
     *0.70840D+00,0.78180D+00,0.85780D+00,0.94170D+00,0.10184D+01,
     *0.11007D+01,0.11872D+01,0.12801D+01,0.13746D+01,0.14773D+01,
     *0.15695D+01,0.16850D+01,0.17943D+01,0.18928D+01,0.20246D+01,
     *0.21637D+01,0.23082D+01,0.24496D+01,0.25943D+01,0.27409D+01,
     *0.28971D+01,0.30488D+01,0.31990D+01,0.33550D+01,0.35154D+01,
     *0.36819D+01,0.38526D+01,0.40276D+01,0.41941D+01,0.43825D+01,
     *0.45689D+01,0.47687D+01,0.50194D+01,0.52598D+01,0.55031D+01,
     *0.57327D+01,0.59723D+01,0.61070D+01,0.65225D+01,0.67550D+01,
     *0.70177D+01,0.72963D+01,0.75764D+01,0.78585D+01,0.81513D+01,
     *0.84481D+01,0.87258D+01,0.90176D+01,0.93200D+01,0.96320D+01,
     *0.99584D+01,0.10286D+02,0.10624D+02,0.10967D+02,0.11314D+02,
     *0.11667D+02,0.12034D+02,0.12405D+02,0.12786D+02,0.13179D+02,
     *0.13580D+02,0.13987D+02,0.14405D+02,0.14834D+02,0.15277D+02,
     *0.15742D+02,0.16215D+02,0.16700D+02,0.17206D+02,0.17723D+02,
     *0.18249D+02,0.18793D+02,0.19352D+02,0.19928D+02,0.20520D+02/
!-------------------------------------------------------------!
!  L2-MO Energy  Calculated neglecting correction term        !
!  by using Atomic-Electron Binding Energy in Table 2. of     !
!  Table of Isotopes, Eighth  Edition.   Using average M      !
!  binding energy.  Neglect O binding energy.                 !
!-------------------------------------------------------------!
      DATA (EL2AUG(3,IIZ),IIZ=1,100)/
     *0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,
     *0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,
     *0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,
     *0.00000D+00,0.00000D+00,0.23220D+00,0.27540D+00,0.32090D+00,
     *0.39260D+00,0.45090D+00,0.51220D+00,0.57480D+00,0.63980D+00,
     *0.70840D+00,0.78180D+00,0.85780D+00,0.94170D+00,0.10184D+01,
     *0.11019D+01,0.11906D+01,0.12842D+01,0.13818D+01,0.14849D+01,
     *0.15861D+01,0.17021D+01,0.18176D+01,0.19377D+01,0.20621D+01,
     *0.21911D+01,0.23204D+01,0.24631D+01,0.26054D+01,0.27526D+01,
     *0.29047D+01,0.30613D+01,0.32226D+01,0.33891D+01,0.35603D+01,
     *0.37366D+01,0.39182D+01,0.41053D+01,0.42938D+01,0.44950D+01,
     *0.46977D+01,0.49076D+01,0.51231D+01,0.53447D+01,0.55730D+01,
     *0.58079D+01,0.60476D+01,0.62942D+01,0.65460D+01,0.68031D+01,
     *0.70706D+01,0.73438D+01,0.76242D+01,0.79082D+01,0.82025D+01,
     *0.85027D+01,0.88106D+01,0.91240D+01,0.94472D+01,0.97776D+01,
     *0.10116D+02,0.10463D+02,0.10818D+02,0.11183D+02,0.11556D+02,
     *0.11937D+02,0.12331D+02,0.12732D+02,0.13143D+02,0.13566D+02,
     *0.13998D+02,0.14443D+02,0.14899D+02,0.15365D+02,0.15846D+02,
     *0.16339D+02,0.16843D+02,0.17362D+02,0.17896D+02,0.18445D+02,
     *0.19003D+02,0.19582D+02,0.20175D+02,0.20786D+02,0.21413D+02/
!-------------------------------------------------------------!
!  L2-NN Energy  Calculated neglecting correction term        !
!  by using Atomic-Electron Binding Energy in Table 2. of     !
!  Table of Isotopes, Eighth  Edition.   Using average N      !
!  binding energy.                                            !
!-------------------------------------------------------------!
      DATA (EL2AUG(4,IIZ),IIZ=1,100)/
     *0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,
     *0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,
     *0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,
     *0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,
     *0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,
     *0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,
     *0.11398D+01,0.12410D+01,0.13504D+01,0.14618D+01,0.15807D+01,
     *0.16941D+01,0.18298D+01,0.19601D+01,0.20657D+01,0.22316D+01,
     *0.24099D+01,0.26008D+01,0.27661D+01,0.29448D+01,0.31227D+01,
     *0.33150D+01,0.34988D+01,0.36799D+01,0.38697D+01,0.40663D+01,
     *0.42709D+01,0.44808D+01,0.46967D+01,0.49042D+01,0.51344D+01,
     *0.53660D+01,0.56127D+01,0.59568D+01,0.62706D+01,0.65817D+01,
     *0.68623D+01,0.71611D+01,0.72427D+01,0.78833D+01,0.81555D+01,
     *0.84748D+01,0.88229D+01,0.91687D+01,0.95175D+01,0.98758D+01,
     *0.10239D+02,0.10570D+02,0.10923D+02,0.11290D+02,0.11667D+02,
     *0.12070D+02,0.12471D+02,0.12883D+02,0.13301D+02,0.13725D+02,
     *0.14158D+02,0.14606D+02,0.15058D+02,0.15524D+02,0.16003D+02,
     *0.16492D+02,0.16988D+02,0.17496D+02,0.18018D+02,0.18557D+02,
     *0.19120D+02,0.19692D+02,0.20277D+02,0.20886D+02,0.21509D+02,
     *0.22143D+02,0.22793D+02,0.23462D+02,0.24150D+02,0.24855D+02/
!-------------------------------------------------------------!
!  L2-NO Energy  Calculated neglecting correction term        !
!  by using Atomic-Electron Binding Energy in Table 2. of     !
!  Table of Isotopes, Eighth  Edition.   Using average N      !
!  binding energy.  Neglect O binding energy.                 !
!-------------------------------------------------------------!
      DATA (EL2AUG(5,IIZ),IIZ=1,100)/
     *0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,
     *0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,
     *0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,
     *0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,
     *0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,
     *0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,
     *0.11411D+01,0.12444D+01,0.13545D+01,0.14690D+01,0.15884D+01,
     *0.17106D+01,0.18468D+01,0.19835D+01,0.21106D+01,0.22692D+01,
     *0.24373D+01,0.26129D+01,0.27797D+01,0.29558D+01,0.31344D+01,
     *0.33227D+01,0.35112D+01,0.37034D+01,0.39039D+01,0.41112D+01,
     *0.43257D+01,0.45464D+01,0.47744D+01,0.50040D+01,0.52469D+01,
     *0.54948D+01,0.57517D+01,0.60605D+01,0.63555D+01,0.66516D+01,
     *0.69376D+01,0.72365D+01,0.74299D+01,0.79068D+01,0.82035D+01,
     *0.85277D+01,0.88703D+01,0.92165D+01,0.95672D+01,0.99270D+01,
     *0.10294D+02,0.10655D+02,0.11030D+02,0.11417D+02,0.11813D+02,
     *0.12228D+02,0.12648D+02,0.13078D+02,0.13517D+02,0.13967D+02,
     *0.14428D+02,0.14903D+02,0.15385D+02,0.15881D+02,0.16389D+02,
     *0.16910D+02,0.17444D+02,0.17990D+02,0.18550D+02,0.19125D+02,
     *0.19717D+02,0.20320D+02,0.20939D+02,0.21576D+02,0.22230D+02,
     *0.22897D+02,0.23582D+02,0.24285D+02,0.25007D+02,0.25748D+02/
!-------------------------------------------------------------!
!  L2-OO Energy  Calculated neglecting correction term        !
!  by using Atomic-Electron Binding Energy in Table 2. of     !
!  Table of Isotopes, Eighth  Edition.   Neglect O binding    !
!  energy.                                                    !
!-------------------------------------------------------------!
      DATA (EL2AUG(6,IIZ),IIZ=1,100)/
     *0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,
     *0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,
     *0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,
     *0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,
     *0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,
     *0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,
     *0.11423D+01,0.12478D+01,0.13586D+01,0.14762D+01,0.15960D+01,
     *0.17272D+01,0.18639D+01,0.20068D+01,0.21555D+01,0.23067D+01,
     *0.24647D+01,0.26251D+01,0.27932D+01,0.29669D+01,0.31461D+01,
     *0.33303D+01,0.35237D+01,0.37270D+01,0.39380D+01,0.41561D+01,
     *0.43804D+01,0.46120D+01,0.48521D+01,0.51037D+01,0.53594D+01,
     *0.56236D+01,0.58906D+01,0.61642D+01,0.64404D+01,0.67215D+01,
     *0.70128D+01,0.73118D+01,0.76171D+01,0.79303D+01,0.82516D+01,
     *0.85806D+01,0.89178D+01,0.92643D+01,0.96169D+01,0.99782D+01,
     *0.10349D+02,0.10739D+02,0.11136D+02,0.11544D+02,0.11959D+02,
     *0.12385D+02,0.12824D+02,0.13273D+02,0.13734D+02,0.14209D+02,
     *0.14698D+02,0.15200D+02,0.15711D+02,0.16237D+02,0.16776D+02,
     *0.17328D+02,0.17899D+02,0.18484D+02,0.19081D+02,0.19693D+02,
     *0.20314D+02,0.20948D+02,0.21600D+02,0.22266D+02,0.22952D+02,
     *0.23651D+02,0.24371D+02,0.25108D+02,0.25865D+02,0.26641D+02/
!-------------------------------------------------------------!
!  L3-MM Energy  Calculated neglecting correction term        !
!  by using Atomic-Electron Binding Energy in Table 2. of     !
!  Table of Isotopes, Eighth  Edition.   Using average M      !
!  binding energy.                                            !
!-------------------------------------------------------------!
      DATA (EL3AUG(1,IIZ),IIZ=1,100)/
     *0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,
     *0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,
     *0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,
     *0.00000D+00,0.00000D+00,0.21160D+00,0.25180D+00,0.28820D+00,
     *0.37400D+00,0.43430D+00,0.49630D+00,0.55670D+00,0.61710D+00,
     *0.68270D+00,0.75500D+00,0.82650D+00,0.91250D+00,0.97090D+00,
     *0.10346D+01,0.11023D+01,0.11743D+01,0.12470D+01,0.13277D+01,
     *0.13927D+01,0.14808D+01,0.15612D+01,0.16444D+01,0.17331D+01,
     *0.18233D+01,0.19108D+01,0.20167D+01,0.21149D+01,0.22168D+01,
     *0.23221D+01,0.24263D+01,0.25287D+01,0.26323D+01,0.27372D+01,
     *0.28446D+01,0.29538D+01,0.30635D+01,0.31624D+01,0.32831D+01,
     *0.33952D+01,0.35167D+01,0.36412D+01,0.37729D+01,0.39109D+01,
     *0.40495D+01,0.41878D+01,0.43311D+01,0.44742D+01,0.46170D+01,
     *0.47701D+01,0.49231D+01,0.50777D+01,0.52306D+01,0.53922D+01,
     *0.55523D+01,0.57031D+01,0.58569D+01,0.60132D+01,0.61731D+01,
     *0.63323D+01,0.64920D+01,0.66552D+01,0.68175D+01,0.69783D+01,
     *0.71359D+01,0.72964D+01,0.74598D+01,0.76214D+01,0.77866D+01,
     *0.79506D+01,0.81124D+01,0.82742D+01,0.84380D+01,0.86050D+01,
     *0.87832D+01,0.89588D+01,0.91332D+01,0.93174D+01,0.94962D+01,
     *0.96742D+01,0.98570D+01,0.10041D+02,0.10226D+02,0.10412D+02/
!-------------------------------------------------------------!
!  L3-MN Energy  Calculated neglecting correction term        !
!  by using Atomic-Electron Binding Energy in Table 2. of     !
!  Table of Isotopes, Eighth  Edition.   Using average M, N   !
!  binding energy.                                            !
!-------------------------------------------------------------!
      DATA (EL3AUG(2,IIZ),IIZ=1,100)/
     *0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,
     *0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,
     *0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,
     *0.00000D+00,0.00000D+00,0.23010D+00,0.27270D+00,0.31790D+00,
     *0.38810D+00,0.44490D+00,0.50460D+00,0.56560D+00,0.62870D+00,
     *0.69540D+00,0.76680D+00,0.84060D+00,0.92180D+00,0.99530D+00,
     *0.10738D+01,0.11561D+01,0.12446D+01,0.13342D+01,0.14312D+01,
     *0.15172D+01,0.16255D+01,0.17271D+01,0.18173D+01,0.19402D+01,
     *0.20695D+01,0.22033D+01,0.23333D+01,0.24653D+01,0.25986D+01,
     *0.27401D+01,0.28762D+01,0.30095D+01,0.31471D+01,0.32881D+01,
     *0.34337D+01,0.35820D+01,0.37326D+01,0.38726D+01,0.40350D+01,
     *0.41923D+01,0.43608D+01,0.45786D+01,0.47837D+01,0.49895D+01,
     *0.51792D+01,0.53767D+01,0.54668D+01,0.58350D+01,0.60174D+01,
     *0.62272D+01,0.64496D+01,0.66700D+01,0.68896D+01,0.71167D+01,
     *0.73436D+01,0.75471D+01,0.77626D+01,0.79828D+01,0.82086D+01,
     *0.84443D+01,0.86772D+01,0.89149D+01,0.91520D+01,0.93894D+01,
     *0.96266D+01,0.98688D+01,0.10113D+02,0.10359D+02,0.10610D+02,
     *0.10862D+02,0.11113D+02,0.11365D+02,0.11623D+02,0.11884D+02,
     *0.12161D+02,0.12435D+02,0.12710D+02,0.12997D+02,0.13281D+02,
     *0.13568D+02,0.13857D+02,0.14151D+02,0.14447D+02,0.14747D+02/
!-------------------------------------------------------------!
!  L3-MO Energy  Calculated neglecting correction term        !
!  by using Atomic-Electron Binding Energy in Table 2. of     !
!  Table of Isotopes, Eighth  Edition.   Using average M      !
!  binding energy.   Neglect O binding energy                 !
!-------------------------------------------------------------!
      DATA (EL3AUG(3,IIZ),IIZ=1,100)/
     *0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,
     *0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,
     *0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,
     *0.00000D+00,0.00000D+00,0.23010D+00,0.27270D+00,0.31730D+00,
     *0.38810D+00,0.44490D+00,0.50460D+00,0.56560D+00,0.62870D+00,
     *0.69540D+00,0.76680D+00,0.84060D+00,0.92180D+00,0.99530D+00,
     *0.10750D+01,0.11595D+01,0.12487D+01,0.13414D+01,0.14388D+01,
     *0.15338D+01,0.16426D+01,0.17504D+01,0.18622D+01,0.19777D+01,
     *0.20969D+01,0.22155D+01,0.23468D+01,0.24764D+01,0.26103D+01,
     *0.27477D+01,0.28887D+01,0.30331D+01,0.31812D+01,0.33330D+01,
     *0.34884D+01,0.36476D+01,0.38103D+01,0.39723D+01,0.41475D+01,
     *0.43211D+01,0.44997D+01,0.46823D+01,0.48686D+01,0.50594D+01,
     *0.52544D+01,0.54520D+01,0.56540D+01,0.58585D+01,0.60655D+01,
     *0.62801D+01,0.64971D+01,0.67178D+01,0.69393D+01,0.71679D+01,
     *0.73982D+01,0.76319D+01,0.78690D+01,0.81100D+01,0.83542D+01,
     *0.86016D+01,0.88536D+01,0.91095D+01,0.93681D+01,0.96311D+01,
     *0.98967D+01,0.10166D+02,0.10439D+02,0.10716D+02,0.10997D+02,
     *0.11280D+02,0.11569D+02,0.11859D+02,0.12154D+02,0.12453D+02,
     *0.12758D+02,0.13063D+02,0.13372D+02,0.13687D+02,0.14003D+02,
     *0.14322D+02,0.14646D+02,0.14974D+02,0.15305D+02,0.15640D+02/
!-------------------------------------------------------------!
!  L3-NN Energy  Calculated neglecting correction term        !
!  by using Atomic-Electron Binding Energy in Table 2. of     !
!  Table of Isotopes, Eighth  Edition.   Using average N      !
!  binding energy.                                            !
!-------------------------------------------------------------!
      DATA (EL3AUG(4,IIZ),IIZ=1,100)/
     *0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,
     *0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,
     *0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,
     *0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,
     *0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,
     *0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,
     *0.11130D+01,0.12099D+01,0.13149D+01,0.14214D+01,0.15346D+01,
     *0.16418D+01,0.17703D+01,0.18929D+01,0.19902D+01,0.21472D+01,
     *0.23157D+01,0.24959D+01,0.26498D+01,0.28158D+01,0.29804D+01,
     *0.31580D+01,0.33262D+01,0.34904D+01,0.36618D+01,0.38390D+01,
     *0.40227D+01,0.42102D+01,0.44017D+01,0.45827D+01,0.47869D+01,
     *0.49894D+01,0.52048D+01,0.55160D+01,0.57945D+01,0.60681D+01,
     *0.63088D+01,0.65655D+01,0.66025D+01,0.71958D+01,0.74179D+01,
     *0.76843D+01,0.79762D+01,0.82623D+01,0.85486D+01,0.88412D+01,
     *0.91349D+01,0.93912D+01,0.96683D+01,0.99524D+01,0.10244D+02,
     *0.10556D+02,0.10862D+02,0.11175D+02,0.11486D+02,0.11801D+02,
     *0.12117D+02,0.12441D+02,0.12766D+02,0.13097D+02,0.13434D+02,
     *0.13774D+02,0.14114D+02,0.14456D+02,0.14807D+02,0.15164D+02,
     *0.15539D+02,0.15912D+02,0.16287D+02,0.16677D+02,0.17067D+02,
     *0.17462D+02,0.17857D+02,0.18261D+02,0.18669D+02,0.19082D+02/
!-------------------------------------------------------------!
!  L3-NO Energy  Calculated neglecting correction term        !
!  by using Atomic-Electron Binding Energy in Table 2. of     !
!  Table of Isotopes, Eighth  Edition.   Using average N      !
!  binding energy.  Neglect O binding energy.                 !
!-------------------------------------------------------------!
      DATA (EL3AUG(5,IIZ),IIZ=1,100)/
     *0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,
     *0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,
     *0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,
     *0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,
     *0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,
     *0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,
     *0.11142D+01,0.12133D+01,0.13190D+01,0.14286D+01,0.15423D+01,
     *0.16583D+01,0.17873D+01,0.19163D+01,0.20351D+01,0.21848D+01,
     *0.23431D+01,0.25080D+01,0.26634D+01,0.28268D+01,0.29921D+01,
     *0.31657D+01,0.33386D+01,0.35139D+01,0.36960D+01,0.38839D+01,
     *0.40775D+01,0.42758D+01,0.44794D+01,0.46825D+01,0.48994D+01,
     *0.51182D+01,0.53438D+01,0.56197D+01,0.58794D+01,0.61380D+01,
     *0.63841D+01,0.66409D+01,0.67897D+01,0.72193D+01,0.74659D+01,
     *0.77372D+01,0.80236D+01,0.83101D+01,0.85983D+01,0.88924D+01,
     *0.91895D+01,0.94759D+01,0.97747D+01,0.10080D+02,0.10390D+02,
     *0.10714D+02,0.11039D+02,0.11369D+02,0.11703D+02,0.12042D+02,
     *0.12387D+02,0.12738D+02,0.13092D+02,0.13454D+02,0.13820D+02,
     *0.14192D+02,0.14570D+02,0.14950D+02,0.15339D+02,0.15732D+02,
     *0.16136D+02,0.16540D+02,0.16949D+02,0.17367D+02,0.17788D+02,
     *0.18216D+02,0.18646D+02,0.19084D+02,0.19526D+02,0.19975D+02/
!-------------------------------------------------------------!
!  L3-OO Energy  Calculated neglecting correction term        !
!  by using Atomic-Electron Binding Energy in Table 2. of     !
!  Table of Isotopes, Eighth  Edition.   Neglect O binding    !
!  energy.                                                    !
!-------------------------------------------------------------!
      DATA (EL3AUG(6,IIZ),IIZ=1,100)/
     *0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,
     *0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,
     *0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,
     *0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,
     *0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,
     *0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,
     *0.11154D+01,0.12167D+01,0.13231D+01,0.14358D+01,0.15499D+01,
     *0.16749D+01,0.18044D+01,0.19396D+01,0.20800D+01,0.22223D+01,
     *0.23705D+01,0.25202D+01,0.26769D+01,0.28379D+01,0.30038D+01,
     *0.31733D+01,0.33511D+01,0.35375D+01,0.37301D+01,0.39288D+01,
     *0.41322D+01,0.43414D+01,0.45571D+01,0.47822D+01,0.50119D+01,
     *0.52470D+01,0.54827D+01,0.57234D+01,0.59643D+01,0.62079D+01,
     *0.64593D+01,0.67162D+01,0.69769D+01,0.72428D+01,0.75140D+01,
     *0.77901D+01,0.80711D+01,0.83579D+01,0.86480D+01,0.89436D+01,
     *0.92441D+01,0.95607D+01,0.98811D+01,0.10207D+02,0.10535D+02,
     *0.10871D+02,0.11215D+02,0.11564D+02,0.11919D+02,0.12284D+02,
     *0.12658D+02,0.13035D+02,0.13419D+02,0.13810D+02,0.14207D+02,
     *0.14610D+02,0.15025D+02,0.15444D+02,0.15870D+02,0.16300D+02,
     *0.16733D+02,0.17168D+02,0.17610D+02,0.18057D+02,0.18510D+02,
     *0.18970D+02,0.19435D+02,0.19907D+02,0.20384D+02,0.20868D+02/
!-------------------------------------------------------------!
!  K-L1L1 Auger Intensity                                     !
!     Z=12-17 Table 1 of W.N.Assad, Nucl. Phys. 44(1963)399.  !
!     Z>17  Table 8. of  Table of Isotopes, Eighth  Edition.  !
!-------------------------------------------------------------!
      DATA (DFKAUG(1,IIZ),IIZ=1,100)/
     *0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,
     *0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,
     *0.00000D+00,0.30930D+00,0.27650D+00,0.25230D+00,0.23370D+00,
     *0.21980D+00,0.20600D+00,0.69400D-01,0.67600D-01,0.66700D-01,
     *0.66200D-01,0.66100D-01,0.66000D-01,0.66200D-01,0.66500D-01,
     *0.66700D-01,0.67000D-01,0.67500D-01,0.67900D-01,0.68400D-01,
     *0.69100D-01,0.70900D-01,0.70600D-01,0.70500D-01,0.68100D-01,
     *0.67300D-01,0.68200D-01,0.68700D-01,0.69700D-01,0.70500D-01,
     *0.70900D-01,0.70800D-01,0.71800D-01,0.72100D-01,0.73100D-01,
     *0.73600D-01,0.74100D-01,0.74700D-01,0.75700D-01,0.75900D-01,
     *0.77100D-01,0.77200D-01,0.78400D-01,0.78800D-01,0.79600D-01,
     *0.80500D-01,0.81600D-01,0.83000D-01,0.83100D-01,0.84700D-01,
     *0.85400D-01,0.86000D-01,0.88100D-01,0.89200D-01,0.90200D-01,
     *0.91500D-01,0.91100D-01,0.92400D-01,0.94000D-01,0.95800D-01,
     *0.95600D-01,0.97700D-01,0.97800D-01,0.10000D+00,0.10010D+00,
     *0.10240D+00,0.10250D+00,0.10550D+00,0.10550D+00,0.10570D+00,
     *0.10890D+00,0.10900D+00,0.10930D+00,0.10970D+00,0.11280D+00,
     *0.11290D+00,0.11320D+00,0.11630D+00,0.11720D+00,0.11750D+00,
     *0.11780D+00,0.11780D+00,0.11850D+00,0.12220D+00,0.12300D+00,
     *0.12320D+00,0.12400D+00,0.12440D+00,0.12420D+00,0.12520D+00/
!-------------------------------------------------------------!
!  K-L1L2 Auger Intensity                                     !
!     Z=12-17 Table 1 of W.N.Assad, Nucl. Phys. 44(1963)399.  !
!     Z>17  Table 8. of  Table of Isotopes, Eighth  Edition.  !
!-------------------------------------------------------------!
      DATA (DFKAUG(2,IIZ),IIZ=1,100)/
     *0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,
     *0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,
     *0.00000D+00,0.67310D+00,0.63990D+00,0.61250D+00,0.59030D+00,
     *0.57340D+00,0.55400D+00,0.14560D+00,0.14210D+00,0.14050D+00,
     *0.13980D+00,0.13990D+00,0.14000D+00,0.14060D+00,0.14020D+00,
     *0.14100D+00,0.14200D+00,0.14340D+00,0.14480D+00,0.14430D+00,
     *0.14620D+00,0.15040D+00,0.15040D+00,0.14830D+00,0.14660D+00,
     *0.14300D+00,0.14480D+00,0.14620D+00,0.14840D+00,0.15030D+00,
     *0.15140D+00,0.15140D+00,0.15360D+00,0.15480D+00,0.15710D+00,
     *0.15880D+00,0.16000D+00,0.16220D+00,0.16430D+00,0.16550D+00,
     *0.16800D+00,0.16990D+00,0.17220D+00,0.17410D+00,0.17660D+00,
     *0.17940D+00,0.18260D+00,0.18540D+00,0.18800D+00,0.19220D+00,
     *0.19490D+00,0.19730D+00,0.20270D+00,0.20620D+00,0.20930D+00,
     *0.21360D+00,0.21620D+00,0.22070D+00,0.22530D+00,0.23030D+00,
     *0.23360D+00,0.23770D+00,0.24220D+00,0.24760D+00,0.25140D+00,
     *0.25610D+00,0.26030D+00,0.26770D+00,0.27200D+00,0.27430D+00,
     *0.28240D+00,0.28460D+00,0.29050D+00,0.29460D+00,0.30300D+00,
     *0.30650D+00,0.30980D+00,0.31910D+00,0.32390D+00,0.32830D+00,
     *0.33550D+00,0.33930D+00,0.34440D+00,0.35180D+00,0.35740D+00,
     *0.36190D+00,0.36810D+00,0.37330D+00,0.38070D+00,0.38390D+00/
!-------------------------------------------------------------!
!  K-L1L3 Auger Intensity                                     !
!     Z=12-17 Table 1 of W.N.Assad, Nucl. Phys. 44(1963)399.  !
!     Z>17  Table 8. of  Table of Isotopes, Eighth  Edition.  !
!-------------------------------------------------------------!
      DATA (DFKAUG(3,IIZ),IIZ=1,100)/
     *0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,
     *0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,
     *0.00000D+00,0.77300D+00,0.73570D+00,0.70340D+00,0.67800D+00,
     *0.65790D+00,0.63590D+00,0.29130D+00,0.28420D+00,0.27980D+00,
     *0.27840D+00,0.27850D+00,0.27740D+00,0.27840D+00,0.27600D+00,
     *0.27740D+00,0.27760D+00,0.27840D+00,0.27880D+00,0.27910D+00,
     *0.28230D+00,0.28570D+00,0.28490D+00,0.28200D+00,0.27490D+00,
     *0.26640D+00,0.26790D+00,0.27150D+00,0.27260D+00,0.27280D+00,
     *0.27430D+00,0.27440D+00,0.27630D+00,0.27520D+00,0.27760D+00,
     *0.27860D+00,0.27900D+00,0.28030D+00,0.28230D+00,0.28190D+00,
     *0.28440D+00,0.28540D+00,0.28670D+00,0.28770D+00,0.29010D+00,
     *0.29150D+00,0.29440D+00,0.29680D+00,0.29880D+00,0.30210D+00,
     *0.30430D+00,0.30730D+00,0.31140D+00,0.31380D+00,0.31720D+00,
     *0.32040D+00,0.32340D+00,0.32630D+00,0.33110D+00,0.33430D+00,
     *0.33770D+00,0.33990D+00,0.34470D+00,0.35000D+00,0.35140D+00,
     *0.35590D+00,0.36020D+00,0.36510D+00,0.36910D+00,0.37140D+00,
     *0.37660D+00,0.37850D+00,0.38420D+00,0.38540D+00,0.39320D+00,
     *0.39690D+00,0.39970D+00,0.40550D+00,0.41010D+00,0.41330D+00,
     *0.41900D+00,0.42180D+00,0.42550D+00,0.43170D+00,0.43580D+00,
     *0.43930D+00,0.44410D+00,0.44790D+00,0.45400D+00,0.45610D+00/
!-------------------------------------------------------------!
!  K-L2L2 Auger Intensity                                     !
!     Z=12-17 Table 1 of W.N.Assad, Nucl. Phys. 44(1963)399.  !
!     Z>17  Table 8. of  Table of Isotopes, Eighth  Edition.  !
!-------------------------------------------------------------!
      DATA (DFKAUG(4,IIZ),IIZ=1,100)/
     *0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,
     *0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,
     *0.00000D+00,0.78870D+00,0.75420D+00,0.72420D+00,0.70080D+00,
     *0.68250D+00,0.66190D+00,0.30530D+00,0.29790D+00,0.29320D+00,
     *0.29190D+00,0.29210D+00,0.29100D+00,0.29210D+00,0.28960D+00,
     *0.29100D+00,0.29110D+00,0.29210D+00,0.29240D+00,0.29280D+00,
     *0.29610D+00,0.29950D+00,0.29850D+00,0.29560D+00,0.28790D+00,
     *0.27930D+00,0.28080D+00,0.28430D+00,0.28540D+00,0.28540D+00,
     *0.28700D+00,0.28720D+00,0.28900D+00,0.28780D+00,0.29010D+00,
     *0.29110D+00,0.29140D+00,0.29260D+00,0.29450D+00,0.29400D+00,
     *0.29640D+00,0.29730D+00,0.29860D+00,0.29940D+00,0.30180D+00,
     *0.30310D+00,0.30600D+00,0.30830D+00,0.31030D+00,0.31350D+00,
     *0.31570D+00,0.31860D+00,0.32250D+00,0.32490D+00,0.32820D+00,
     *0.33140D+00,0.33430D+00,0.33710D+00,0.34190D+00,0.34490D+00,
     *0.34830D+00,0.35030D+00,0.35520D+00,0.36020D+00,0.36170D+00,
     *0.36590D+00,0.37020D+00,0.37480D+00,0.37880D+00,0.38110D+00,
     *0.38600D+00,0.38780D+00,0.39360D+00,0.39450D+00,0.40230D+00,
     *0.40590D+00,0.40840D+00,0.41420D+00,0.41860D+00,0.42160D+00,
     *0.42720D+00,0.42980D+00,0.43340D+00,0.43950D+00,0.44350D+00,
     *0.44680D+00,0.45150D+00,0.45520D+00,0.46110D+00,0.46310D+00/
!-------------------------------------------------------------!
!  K-L2L3 Auger Intensity                                     !
!     Z=12-17 Table 1 of W.N.Assad, Nucl. Phys. 44(1963)399.  !
!     Z>17  Table 8. of  Table of Isotopes, Eighth  Edition.  !
!-------------------------------------------------------------!
      DATA (DFKAUG(5,IIZ),IIZ=1,100)/
     *0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,
     *0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,
     *0.00000D+00,0.99990D+00,0.99990D+00,0.99980D+00,0.99950D+00,
     *0.99910D+00,0.99850D+00,0.64660D+00,0.63570D+00,0.62660D+00,
     *0.62310D+00,0.62280D+00,0.62000D+00,0.62010D+00,0.61470D+00,
     *0.61540D+00,0.61490D+00,0.61430D+00,0.61400D+00,0.61170D+00,
     *0.61700D+00,0.61960D+00,0.61530D+00,0.60930D+00,0.58900D+00,
     *0.57100D+00,0.57210D+00,0.57340D+00,0.57180D+00,0.57110D+00,
     *0.56840D+00,0.56710D+00,0.56620D+00,0.56340D+00,0.56140D+00,
     *0.55850D+00,0.55800D+00,0.55440D+00,0.55350D+00,0.55420D+00,
     *0.55080D+00,0.54940D+00,0.54830D+00,0.54680D+00,0.54430D+00,
     *0.54270D+00,0.54340D+00,0.54260D+00,0.54160D+00,0.54230D+00,
     *0.54260D+00,0.54270D+00,0.54280D+00,0.54340D+00,0.54400D+00,
     *0.54320D+00,0.54340D+00,0.54460D+00,0.54570D+00,0.54670D+00,
     *0.54580D+00,0.54580D+00,0.54620D+00,0.54830D+00,0.54710D+00,
     *0.54770D+00,0.54900D+00,0.55060D+00,0.55090D+00,0.54970D+00,
     *0.55070D+00,0.55130D+00,0.55280D+00,0.55120D+00,0.55380D+00,
     *0.55430D+00,0.55490D+00,0.55710D+00,0.55640D+00,0.55640D+00,
     *0.55930D+00,0.55840D+00,0.55930D+00,0.56170D+00,0.56260D+00,
     *0.56230D+00,0.56350D+00,0.56360D+00,0.56530D+00,0.56610D+00/
!-------------------------------------------------------------!
!  K-L3L3 Auger Intensity                                     !
!     Z=12-17 Table 1 of W.N.Assad, Nucl. Phys. 44(1963)399.  !
!     Z>17  Table 8. of  Table of Isotopes, Eighth  Edition.  !
!-------------------------------------------------------------!
      DATA (DFKAUG(6,IIZ),IIZ=1,100)/
     *0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,
     *0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,
     *0.00000D+00,0.10000D+01,0.10000D+01,0.10000D+01,0.10000D+01,
     *0.10000D+01,0.10000D+01,0.84230D+00,0.82790D+00,0.81350D+00,
     *0.80960D+00,0.80850D+00,0.80490D+00,0.80340D+00,0.79680D+00,
     *0.79730D+00,0.79520D+00,0.79320D+00,0.79100D+00,0.78830D+00,
     *0.79170D+00,0.79150D+00,0.78620D+00,0.77700D+00,0.75390D+00,
     *0.73090D+00,0.73130D+00,0.73090D+00,0.72710D+00,0.72330D+00,
     *0.71900D+00,0.71560D+00,0.71170D+00,0.70850D+00,0.70230D+00,
     *0.69950D+00,0.69660D+00,0.69110D+00,0.68790D+00,0.68580D+00,
     *0.68110D+00,0.67700D+00,0.67400D+00,0.67040D+00,0.66560D+00,
     *0.66190D+00,0.66160D+00,0.65850D+00,0.65610D+00,0.65480D+00,
     *0.65340D+00,0.65120D+00,0.65010D+00,0.64800D+00,0.64700D+00,
     *0.64490D+00,0.64350D+00,0.64270D+00,0.64170D+00,0.64050D+00,
     *0.63920D+00,0.63690D+00,0.63470D+00,0.63400D+00,0.63250D+00,
     *0.63220D+00,0.63050D+00,0.62900D+00,0.62860D+00,0.62680D+00,
     *0.62520D+00,0.62400D+00,0.62380D+00,0.62040D+00,0.62110D+00,
     *0.61980D+00,0.61890D+00,0.61930D+00,0.61670D+00,0.61510D+00,
     *0.61640D+00,0.61410D+00,0.61340D+00,0.61390D+00,0.61340D+00,
     *0.61160D+00,0.61110D+00,0.60970D+00,0.60970D+00,0.60910D+00/
!-------------------------------------------------------------!
!  K-L1M Auger Intensity                                      !
!      Table 8. of  Table of Isotopes, Eighth  Edition.       !
!-------------------------------------------------------------!
      DATA (DFKAUG(7,IIZ),IIZ=1,100)/
     *0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,
     *0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,
     *0.00000D+00,0.10000D+01,0.10000D+01,0.10000D+01,0.10000D+01,
     *0.10000D+01,0.10000D+01,0.87850D+00,0.86730D+00,0.85480D+00,
     *0.85160D+00,0.85140D+00,0.84830D+00,0.84750D+00,0.84170D+00,
     *0.84230D+00,0.84070D+00,0.83910D+00,0.83750D+00,0.83560D+00,
     *0.84090D+00,0.84210D+00,0.83820D+00,0.82950D+00,0.80640D+00,
     *0.78280D+00,0.78450D+00,0.78540D+00,0.78320D+00,0.78100D+00,
     *0.77730D+00,0.77500D+00,0.77200D+00,0.76970D+00,0.76510D+00,
     *0.76310D+00,0.76110D+00,0.75710D+00,0.75480D+00,0.75310D+00,
     *0.74980D+00,0.74720D+00,0.74490D+00,0.74240D+00,0.73880D+00,
     *0.73630D+00,0.73710D+00,0.73540D+00,0.73420D+00,0.73360D+00,
     *0.73360D+00,0.73280D+00,0.73260D+00,0.73200D+00,0.73210D+00,
     *0.73160D+00,0.73140D+00,0.73200D+00,0.73230D+00,0.73240D+00,
     *0.73250D+00,0.73160D+00,0.73080D+00,0.73130D+00,0.73090D+00,
     *0.73130D+00,0.73120D+00,0.73100D+00,0.73150D+00,0.73130D+00,
     *0.73140D+00,0.73170D+00,0.73250D+00,0.73110D+00,0.73260D+00,
     *0.73290D+00,0.73320D+00,0.73430D+00,0.73370D+00,0.73350D+00,
     *0.73560D+00,0.73480D+00,0.73540D+00,0.73660D+00,0.73740D+00,
     *0.73700D+00,0.73770D+00,0.73830D+00,0.73920D+00,0.73950D+00/
!-------------------------------------------------------------!
!  K-L2M Auger Intensity                                      !
!      Table 8. of  Table of Isotopes, Eighth  Edition.       !
!-------------------------------------------------------------!
      DATA (DFKAUG(8,IIZ),IIZ=1,100)/
     *0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,
     *0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,
     *0.00000D+00,0.10000D+01,0.10000D+01,0.10000D+01,0.10000D+01,
     *0.10000D+01,0.10000D+01,0.91720D+00,0.90930D+00,0.89900D+00,
     *0.89710D+00,0.89710D+00,0.89550D+00,0.89500D+00,0.89010D+00,
     *0.89100D+00,0.89000D+00,0.88910D+00,0.88820D+00,0.88690D+00,
     *0.89430D+00,0.89670D+00,0.89390D+00,0.88580D+00,0.86220D+00,
     *0.83810D+00,0.84080D+00,0.84270D+00,0.84200D+00,0.84040D+00,
     *0.83770D+00,0.83600D+00,0.83340D+00,0.83150D+00,0.82740D+00,
     *0.82630D+00,0.82490D+00,0.82140D+00,0.81940D+00,0.81750D+00,
     *0.81490D+00,0.81240D+00,0.81030D+00,0.80800D+00,0.80510D+00,
     *0.80290D+00,0.80310D+00,0.80180D+00,0.80090D+00,0.80150D+00,
     *0.80140D+00,0.80060D+00,0.80090D+00,0.80060D+00,0.80090D+00,
     *0.80070D+00,0.80090D+00,0.80160D+00,0.80220D+00,0.80240D+00,
     *0.80260D+00,0.80200D+00,0.80170D+00,0.80220D+00,0.80200D+00,
     *0.80250D+00,0.80240D+00,0.80260D+00,0.80310D+00,0.80310D+00,
     *0.80320D+00,0.80380D+00,0.80460D+00,0.80360D+00,0.80500D+00,
     *0.80560D+00,0.80610D+00,0.80690D+00,0.80680D+00,0.80690D+00,
     *0.80880D+00,0.80850D+00,0.80910D+00,0.81050D+00,0.81140D+00,
     *0.81170D+00,0.81240D+00,0.81300D+00,0.81400D+00,0.81490D+00/
!-------------------------------------------------------------!
!  K-L3M Auger Intensity                                      !
!      Table 8. of  Table of Isotopes, Eighth  Edition.       !
!-------------------------------------------------------------!
      DATA (DFKAUG(9,IIZ),IIZ=1,100)/
     *0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,
     *0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,
     *0.00000D+00,0.10000D+01,0.10000D+01,0.10000D+01,0.10000D+01,
     *0.10000D+01,0.10000D+01,0.99320D+00,0.99180D+00,0.98740D+00,
     *0.98710D+00,0.98700D+00,0.98670D+00,0.98670D+00,0.98500D+00,
     *0.98540D+00,0.98560D+00,0.98560D+00,0.98530D+00,0.98490D+00,
     *0.98670D+00,0.98710D+00,0.98600D+00,0.98350D+00,0.96720D+00,
     *0.95310D+00,0.95310D+00,0.95280D+00,0.95140D+00,0.94980D+00,
     *0.94770D+00,0.94590D+00,0.94380D+00,0.94210D+00,0.93920D+00,
     *0.93830D+00,0.93680D+00,0.93340D+00,0.93120D+00,0.92840D+00,
     *0.92600D+00,0.92300D+00,0.92040D+00,0.91770D+00,0.91460D+00,
     *0.91240D+00,0.91150D+00,0.91020D+00,0.90920D+00,0.90840D+00,
     *0.90730D+00,0.90680D+00,0.90590D+00,0.90520D+00,0.90460D+00,
     *0.90370D+00,0.90310D+00,0.90290D+00,0.90230D+00,0.90170D+00,
     *0.90090D+00,0.89930D+00,0.89810D+00,0.89710D+00,0.89590D+00,
     *0.89490D+00,0.89360D+00,0.89260D+00,0.89170D+00,0.89040D+00,
     *0.88930D+00,0.88830D+00,0.88740D+00,0.88590D+00,0.88530D+00,
     *0.88410D+00,0.88330D+00,0.88240D+00,0.88100D+00,0.88010D+00,
     *0.87960D+00,0.87850D+00,0.87740D+00,0.87680D+00,0.87660D+00,
     *0.87520D+00,0.87430D+00,0.87370D+00,0.87320D+00,0.87270D+00/
!-------------------------------------------------------------!
!  K-L1N Auger Intensity                                      !
!      Table 8. of  Table of Isotopes, Eighth  Edition.       !
!-------------------------------------------------------------!
      DATA (DFKAUG(10,IIZ),IIZ=1,100)/
     *0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,
     *0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,
     *0.00000D+00,0.10000D+01,0.10000D+01,0.10000D+01,0.10000D+01,
     *0.10000D+01,0.10000D+01,0.99320D+00,0.99180D+00,0.98930D+00,
     *0.98890D+00,0.98870D+00,0.98850D+00,0.98840D+00,0.98660D+00,
     *0.98680D+00,0.98700D+00,0.98690D+00,0.98670D+00,0.98630D+00,
     *0.98840D+00,0.98900D+00,0.98810D+00,0.98580D+00,0.97190D+00,
     *0.95870D+00,0.95940D+00,0.95980D+00,0.95920D+00,0.95810D+00,
     *0.95640D+00,0.95500D+00,0.95330D+00,0.95190D+00,0.94980D+00,
     *0.94930D+00,0.94810D+00,0.94540D+00,0.94390D+00,0.94190D+00,
     *0.94010D+00,0.93790D+00,0.93600D+00,0.93410D+00,0.93190D+00,
     *0.93050D+00,0.92990D+00,0.92910D+00,0.92850D+00,0.92800D+00,
     *0.92740D+00,0.92730D+00,0.92680D+00,0.92630D+00,0.92610D+00,
     *0.92570D+00,0.92540D+00,0.92550D+00,0.92540D+00,0.92520D+00,
     *0.92480D+00,0.92380D+00,0.92340D+00,0.92300D+00,0.92240D+00,
     *0.92220D+00,0.92170D+00,0.92110D+00,0.92090D+00,0.92040D+00,
     *0.91990D+00,0.91970D+00,0.91940D+00,0.91880D+00,0.91870D+00,
     *0.91840D+00,0.91810D+00,0.91810D+00,0.91760D+00,0.91730D+00,
     *0.91750D+00,0.91720D+00,0.91700D+00,0.91680D+00,0.91710D+00,
     *0.91670D+00,0.91660D+00,0.91630D+00,0.91670D+00,0.91680D+00/
!-------------------------------------------------------------!
!  K-L2N Auger Intensity                                      !
!      Table 8. of  Table of Isotopes, Eighth  Edition.       !
!-------------------------------------------------------------!
      DATA (DFKAUG(11,IIZ),IIZ=1,100)/
     *0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,
     *0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,
     *0.00000D+00,0.10000D+01,0.10000D+01,0.10000D+01,0.10000D+01,
     *0.10000D+01,0.10000D+01,0.99320D+00,0.99180D+00,0.99020D+00,
     *0.98980D+00,0.98960D+00,0.98930D+00,0.98920D+00,0.98740D+00,
     *0.98750D+00,0.98760D+00,0.98750D+00,0.98730D+00,0.98700D+00,
     *0.98920D+00,0.98990D+00,0.98910D+00,0.98690D+00,0.97600D+00,
     *0.96380D+00,0.96530D+00,0.96640D+00,0.96630D+00,0.96570D+00,
     *0.96430D+00,0.96320D+00,0.96180D+00,0.96070D+00,0.95940D+00,
     *0.95930D+00,0.95840D+00,0.95620D+00,0.95520D+00,0.95350D+00,
     *0.95220D+00,0.95070D+00,0.94950D+00,0.94830D+00,0.94670D+00,
     *0.94580D+00,0.94530D+00,0.94460D+00,0.94430D+00,0.94390D+00,
     *0.94350D+00,0.94340D+00,0.94300D+00,0.94260D+00,0.94250D+00,
     *0.94230D+00,0.94220D+00,0.94230D+00,0.94220D+00,0.94210D+00,
     *0.94180D+00,0.94120D+00,0.94100D+00,0.94090D+00,0.94050D+00,
     *0.94060D+00,0.94040D+00,0.94020D+00,0.94010D+00,0.93990D+00,
     *0.93960D+00,0.93960D+00,0.93960D+00,0.93920D+00,0.93940D+00,
     *0.93930D+00,0.93930D+00,0.93950D+00,0.93930D+00,0.93940D+00,
     *0.93950D+00,0.93970D+00,0.93980D+00,0.94000D+00,0.94020D+00,
     *0.94020D+00,0.94050D+00,0.94050D+00,0.94090D+00,0.94140D+00/
!-------------------------------------------------------------!
!  K-L3N Auger Intensity                                      !
!      Table 8. of  Table of Isotopes, Eighth  Edition.       !
!-------------------------------------------------------------!
      DATA (DFKAUG(12,IIZ),IIZ=1,100)/
     *0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,
     *0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,
     *0.00000D+00,0.10000D+01,0.10000D+01,0.10000D+01,0.10000D+01,
     *0.10000D+01,0.10000D+01,0.99320D+00,0.99180D+00,0.99020D+00,
     *0.98980D+00,0.98960D+00,0.98930D+00,0.98920D+00,0.98880D+00,
     *0.98880D+00,0.98880D+00,0.98860D+00,0.98840D+00,0.98810D+00,
     *0.98990D+00,0.99050D+00,0.98990D+00,0.98810D+00,0.98410D+00,
     *0.97910D+00,0.97980D+00,0.98030D+00,0.98030D+00,0.98010D+00,
     *0.97940D+00,0.97890D+00,0.97820D+00,0.97770D+00,0.97710D+00,
     *0.97650D+00,0.97600D+00,0.97540D+00,0.97490D+00,0.97440D+00,
     *0.97390D+00,0.97340D+00,0.97300D+00,0.97250D+00,0.97190D+00,
     *0.97150D+00,0.97120D+00,0.97050D+00,0.97030D+00,0.96990D+00,
     *0.96950D+00,0.96910D+00,0.96870D+00,0.96840D+00,0.96810D+00,
     *0.96770D+00,0.96740D+00,0.96730D+00,0.96690D+00,0.96660D+00,
     *0.96640D+00,0.96600D+00,0.96580D+00,0.96560D+00,0.96510D+00,
     *0.96490D+00,0.96460D+00,0.96440D+00,0.96410D+00,0.96390D+00,
     *0.96360D+00,0.96340D+00,0.96330D+00,0.96280D+00,0.96270D+00,
     *0.96240D+00,0.96230D+00,0.96210D+00,0.96180D+00,0.96170D+00,
     *0.96150D+00,0.96130D+00,0.96110D+00,0.96100D+00,0.96080D+00,
     *0.96050D+00,0.96040D+00,0.96020D+00,0.96010D+00,0.96010D+00/
!-------------------------------------------------------------!
!  K-MM Auger Intensity                                       !
!      Table 8. of  Table of Isotopes, Eighth  Edition.       !
!-------------------------------------------------------------!
      DATA (DFKAUG(13,IIZ),IIZ=1,100)/
     *0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,
     *0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,
     *0.00000D+00,0.10000D+01,0.10000D+01,0.10000D+01,0.10000D+01,
     *0.10000D+01,0.10000D+01,0.10000D+01,0.10000D+01,0.99940D+00,
     *0.99940D+00,0.99940D+00,0.99950D+00,0.99950D+00,0.99950D+00,
     *0.99950D+00,0.99960D+00,0.99960D+00,0.99960D+00,0.99950D+00,
     *0.99980D+00,0.99990D+00,0.99980D+00,0.99960D+00,0.99780D+00,
     *0.99560D+00,0.99580D+00,0.99590D+00,0.99590D+00,0.99580D+00,
     *0.99560D+00,0.99540D+00,0.99510D+00,0.99490D+00,0.99460D+00,
     *0.99440D+00,0.99420D+00,0.99400D+00,0.99370D+00,0.99350D+00,
     *0.99320D+00,0.99290D+00,0.99270D+00,0.99240D+00,0.99220D+00,
     *0.99200D+00,0.99190D+00,0.99170D+00,0.99150D+00,0.99140D+00,
     *0.99120D+00,0.99110D+00,0.99090D+00,0.99080D+00,0.99070D+00,
     *0.99060D+00,0.99050D+00,0.99040D+00,0.99020D+00,0.99020D+00,
     *0.99000D+00,0.98990D+00,0.98970D+00,0.98960D+00,0.98940D+00,
     *0.98930D+00,0.98910D+00,0.98900D+00,0.98890D+00,0.98880D+00,
     *0.98860D+00,0.98850D+00,0.98840D+00,0.98820D+00,0.98800D+00,
     *0.98790D+00,0.98780D+00,0.98770D+00,0.98760D+00,0.98750D+00,
     *0.98740D+00,0.98730D+00,0.98710D+00,0.98700D+00,0.98690D+00,
     *0.98680D+00,0.98670D+00,0.98660D+00,0.98660D+00,0.98660D+00/
!---------------------------------------------------------------------!
!  L1-MM Auger Intensity                                              !
!      Table 2. E. J. McGuire, Phy. Rev. A, 3(1971)587.               !
!      Linear interpolation is applied for element which is not       !
!      included in this table.                                        !
!---------------------------------------------------------------------!
      DATA (DFL1AUG(1,IIZ),IIZ=1,100)/
     *0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,
     *0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,
     *0.00000D+00,0.10000D+01,0.10000D+01,0.10000D+01,0.10000D+01,
     *0.10000D+01,0.10000D+01,0.10000D+01,0.95685D+00,0.91307D+00,
     *0.91429D+00,0.91550D+00,0.93018D+00,0.94487D+00,0.94278D+00,
     *0.94068D+00,0.94688D+00,0.95308D+00,0.95534D+00,0.95760D+00,
     *0.95061D+00,0.94363D+00,0.93993D+00,0.93623D+00,0.92272D+00,
     *0.90922D+00,0.90308D+00,0.89695D+00,0.88510D+00,0.87324D+00,
     *0.85927D+00,0.84531D+00,0.84074D+00,0.83617D+00,0.82345D+00,
     *0.81072D+00,0.79800D+00,0.78832D+00,0.77865D+00,0.76897D+00,
     *0.76315D+00,0.75732D+00,0.75150D+00,0.74568D+00,0.74571D+00,
     *0.74573D+00,0.74576D+00,0.74579D+00,0.74581D+00,0.74584D+00,
     *0.74028D+00,0.73472D+00,0.72916D+00,0.72359D+00,0.71803D+00,
     *0.71247D+00,0.70691D+00,0.70473D+00,0.70256D+00,0.70038D+00,
     *0.69820D+00,0.69602D+00,0.69385D+00,0.69167D+00,0.69359D+00,
     *0.69550D+00,0.69742D+00,0.69933D+00,0.70125D+00,0.68993D+00,
     *0.67861D+00,0.66730D+00,0.65598D+00,0.65185D+00,0.64771D+00,
     *0.64358D+00,0.63944D+00,0.63531D+00,0.63117D+00,0.62704D+00,
     *0.62291D+00,0.61877D+00,0.61464D+00,0.61050D+00,0.60637D+00,
     *0.60223D+00,0.59810D+00,0.59397D+00,0.58983D+00,0.58570D+00/
!---------------------------------------------------------------------!
!  L1-MN Auger Intensity                                              !
!      Table 2. E. J. McGuire, Phy. Rev. A, 3(1971)587.               !
!      Linear interpolation is applied for element which is not       !
!      included in this table.                                        !
!---------------------------------------------------------------------!
      DATA (DFL1AUG(2,IIZ),IIZ=1,100)/
     *0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,
     *0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,
     *0.00000D+00,0.10000D+01,0.10000D+01,0.10000D+01,0.10000D+01,
     *0.10000D+01,0.10000D+01,0.10000D+01,0.10000D+01,0.99903D+00,
     *0.99903D+00,0.99903D+00,0.99951D+00,0.10000D+01,0.99981D+00,
     *0.99962D+00,0.99969D+00,0.99975D+00,0.99978D+00,0.99981D+00,
     *0.99949D+00,0.99916D+00,0.99884D+00,0.99853D+00,0.99795D+00,
     *0.99737D+00,0.99340D+00,0.98944D+00,0.98832D+00,0.98721D+00,
     *0.98886D+00,0.99050D+00,0.99019D+00,0.98987D+00,0.98856D+00,
     *0.98726D+00,0.98595D+00,0.98191D+00,0.97788D+00,0.97384D+00,
     *0.97091D+00,0.96797D+00,0.96504D+00,0.96210D+00,0.96124D+00,
     *0.96039D+00,0.95953D+00,0.95867D+00,0.95782D+00,0.95696D+00,
     *0.95708D+00,0.95721D+00,0.95733D+00,0.95746D+00,0.95758D+00,
     *0.95771D+00,0.95783D+00,0.95696D+00,0.95609D+00,0.95522D+00,
     *0.95435D+00,0.95348D+00,0.95261D+00,0.95174D+00,0.94977D+00,
     *0.94780D+00,0.94582D+00,0.94385D+00,0.94188D+00,0.93893D+00,
     *0.93599D+00,0.93305D+00,0.93010D+00,0.92789D+00,0.92568D+00,
     *0.92347D+00,0.92126D+00,0.91905D+00,0.91684D+00,0.91463D+00,
     *0.91242D+00,0.91021D+00,0.90800D+00,0.90579D+00,0.90358D+00,
     *0.90137D+00,0.89916D+00,0.89695D+00,0.89474D+00,0.89253D+00/
!---------------------------------------------------------------------!
!  L1-MO Auger Intensity                                              !
!      Table 2. E. J. McGuire, Phy. Rev. A, 3(1971)587.               !
!      Linear interpolation is applied for element which is not       !
!      included in this table.                                        !
!---------------------------------------------------------------------!
      DATA (DFL1AUG(3,IIZ),IIZ=1,100)/
     *0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,
     *0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,
     *0.00000D+00,0.10000D+01,0.10000D+01,0.10000D+01,0.10000D+01,
     *0.10000D+01,0.10000D+01,0.10000D+01,0.10000D+01,0.99903D+00,
     *0.99903D+00,0.99903D+00,0.99951D+00,0.10000D+01,0.99981D+00,
     *0.99962D+00,0.99969D+00,0.99975D+00,0.99978D+00,0.99981D+00,
     *0.99949D+00,0.99916D+00,0.99884D+00,0.99853D+00,0.99795D+00,
     *0.99737D+00,0.99695D+00,0.99652D+00,0.99586D+00,0.99520D+00,
     *0.99441D+00,0.99361D+00,0.99292D+00,0.99223D+00,0.99086D+00,
     *0.98950D+00,0.98813D+00,0.98649D+00,0.98486D+00,0.98322D+00,
     *0.98265D+00,0.98207D+00,0.98150D+00,0.98093D+00,0.98035D+00,
     *0.97976D+00,0.97918D+00,0.97860D+00,0.97801D+00,0.97743D+00,
     *0.97716D+00,0.97690D+00,0.97663D+00,0.97637D+00,0.97610D+00,
     *0.97584D+00,0.97557D+00,0.97538D+00,0.97518D+00,0.97499D+00,
     *0.97480D+00,0.97461D+00,0.97441D+00,0.97422D+00,0.97436D+00,
     *0.97451D+00,0.97465D+00,0.97480D+00,0.97494D+00,0.97299D+00,
     *0.97103D+00,0.96907D+00,0.96712D+00,0.96663D+00,0.96614D+00,
     *0.96565D+00,0.96516D+00,0.96467D+00,0.96418D+00,0.96369D+00,
     *0.96320D+00,0.96271D+00,0.96222D+00,0.96173D+00,0.96124D+00,
     *0.96075D+00,0.96026D+00,0.95977D+00,0.95928D+00,0.95879D+00/
!---------------------------------------------------------------------!
!  L1-NN Auger Intensity                                              !
!      Table 2. E. J. McGuire, Phy. Rev. A, 3(1971)587.               !
!      Linear interpolation is applied for element which is not       !
!      included in this table.                                        !
!---------------------------------------------------------------------!
      DATA (DFL1AUG(4,IIZ),IIZ=1,100)/
     *0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,
     *0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,
     *0.00000D+00,0.10000D+01,0.10000D+01,0.10000D+01,0.10000D+01,
     *0.10000D+01,0.10000D+01,0.10000D+01,0.10000D+01,0.10000D+01,
     *0.10000D+01,0.10000D+01,0.10000D+01,0.10000D+01,0.10000D+01,
     *0.10000D+01,0.10000D+01,0.10000D+01,0.10000D+01,0.10000D+01,
     *0.10000D+01,0.10000D+01,0.10000D+01,0.10000D+01,0.10000D+01,
     *0.10000D+01,0.99977D+00,0.99953D+00,0.99950D+00,0.99946D+00,
     *0.99958D+00,0.99970D+00,0.99970D+00,0.99969D+00,0.99969D+00,
     *0.99968D+00,0.99968D+00,0.99924D+00,0.99880D+00,0.99836D+00,
     *0.99806D+00,0.99777D+00,0.99747D+00,0.99718D+00,0.99702D+00,
     *0.99687D+00,0.99671D+00,0.99655D+00,0.99640D+00,0.99624D+00,
     *0.99628D+00,0.99632D+00,0.99636D+00,0.99641D+00,0.99645D+00,
     *0.99649D+00,0.99653D+00,0.99636D+00,0.99618D+00,0.99601D+00,
     *0.99584D+00,0.99567D+00,0.99549D+00,0.99532D+00,0.99513D+00,
     *0.99495D+00,0.99476D+00,0.99458D+00,0.99439D+00,0.99389D+00,
     *0.99339D+00,0.99289D+00,0.99239D+00,0.99202D+00,0.99166D+00,
     *0.99129D+00,0.99092D+00,0.99055D+00,0.99019D+00,0.98982D+00,
     *0.98945D+00,0.98909D+00,0.98872D+00,0.98835D+00,0.98798D+00,
     *0.98762D+00,0.98725D+00,0.98688D+00,0.98652D+00,0.98615D+00/
!---------------------------------------------------------------------!
!  L1-NO Auger Intensity                                              !
!      Table 2. E. J. McGuire, Phy. Rev. A, 3(1971)587.               !
!      Linear interpolation is applied for element which is not       !
!      included in this table.                                        !
!---------------------------------------------------------------------!
      DATA (DFL1AUG(5,IIZ),IIZ=1,100)/
     *0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,
     *0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,
     *0.00000D+00,0.10000D+01,0.10000D+01,0.10000D+01,0.10000D+01,
     *0.10000D+01,0.10000D+01,0.10000D+01,0.10000D+01,0.10000D+01,
     *0.10000D+01,0.10000D+01,0.10000D+01,0.10000D+01,0.10000D+01,
     *0.10000D+01,0.10000D+01,0.10000D+01,0.10000D+01,0.10000D+01,
     *0.10000D+01,0.10000D+01,0.10000D+01,0.10000D+01,0.10000D+01,
     *0.10000D+01,0.99999D+00,0.99999D+00,0.99999D+00,0.99999D+00,
     *0.99999D+00,0.10000D+01,0.10000D+01,0.10000D+01,0.10000D+01,
     *0.10000D+01,0.10000D+01,0.99999D+00,0.99998D+00,0.99997D+00,
     *0.99995D+00,0.99993D+00,0.99990D+00,0.99988D+00,0.99987D+00,
     *0.99986D+00,0.99985D+00,0.99984D+00,0.99983D+00,0.99982D+00,
     *0.99982D+00,0.99983D+00,0.99983D+00,0.99984D+00,0.99984D+00,
     *0.99985D+00,0.99985D+00,0.99984D+00,0.99983D+00,0.99982D+00,
     *0.99981D+00,0.99980D+00,0.99979D+00,0.99978D+00,0.99975D+00,
     *0.99972D+00,0.99969D+00,0.99966D+00,0.99963D+00,0.99959D+00,
     *0.99955D+00,0.99951D+00,0.99947D+00,0.99942D+00,0.99938D+00,
     *0.99933D+00,0.99928D+00,0.99923D+00,0.99919D+00,0.99914D+00,
     *0.99909D+00,0.99905D+00,0.99900D+00,0.99895D+00,0.99890D+00,
     *0.99886D+00,0.99881D+00,0.99876D+00,0.99872D+00,0.99867D+00/
!---------------------------------------------------------------------!
!  L2-MM Auger Intensity                                              !
!      Table 3. E. J. McGuire, Phy. Rev. A, 3(1971)587.               !
!      Linear interpolation is applied for element which is not       !
!      included in this table.                                        !
!---------------------------------------------------------------------!
      DATA (DFL2AUG(1,IIZ),IIZ=1,100)/
     *0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,
     *0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,
     *0.00000D+00,0.10000D+01,0.10000D+01,0.10000D+01,0.10000D+01,
     *0.10000D+01,0.10000D+01,0.10000D+01,0.99074D+00,0.97985D+00,
     *0.97989D+00,0.97994D+00,0.98742D+00,0.99491D+00,0.99303D+00,
     *0.99115D+00,0.99234D+00,0.99352D+00,0.99447D+00,0.99542D+00,
     *0.98803D+00,0.98065D+00,0.96849D+00,0.95633D+00,0.94335D+00,
     *0.93036D+00,0.92330D+00,0.91623D+00,0.89902D+00,0.88181D+00,
     *0.87808D+00,0.87435D+00,0.86712D+00,0.85988D+00,0.84055D+00,
     *0.82122D+00,0.80189D+00,0.79148D+00,0.78108D+00,0.77067D+00,
     *0.76426D+00,0.75785D+00,0.75144D+00,0.74503D+00,0.74308D+00,
     *0.74112D+00,0.73917D+00,0.73722D+00,0.73526D+00,0.73331D+00,
     *0.73077D+00,0.72823D+00,0.72569D+00,0.72316D+00,0.72062D+00,
     *0.71808D+00,0.71554D+00,0.71442D+00,0.71330D+00,0.71218D+00,
     *0.71106D+00,0.70994D+00,0.70882D+00,0.70770D+00,0.70290D+00,
     *0.69810D+00,0.69330D+00,0.68850D+00,0.68370D+00,0.68053D+00,
     *0.67737D+00,0.67420D+00,0.67103D+00,0.65478D+00,0.63854D+00,
     *0.62229D+00,0.60605D+00,0.58980D+00,0.57356D+00,0.55731D+00,
     *0.54106D+00,0.52482D+00,0.50857D+00,0.49233D+00,0.47608D+00,
     *0.45984D+00,0.44359D+00,0.42734D+00,0.41110D+00,0.39485D+00/
!---------------------------------------------------------------------!
!  L2-MN Auger Intensity. DFL2AUG(1)+L2-MN                            !
!      Table 3. E. J. McGuire, Phy. Rev. A, 3(1971)587.               !
!      Linear interpolation is applied for element which is not       !
!      included in this table.                                        !
!---------------------------------------------------------------------!
      DATA (DFL2AUG(2,IIZ),IIZ=1,100)/
     *0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,
     *0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,
     *0.00000D+00,0.10000D+01,0.10000D+01,0.10000D+01,0.10000D+01,
     *0.10000D+01,0.10000D+01,0.10000D+01,0.10000D+01,0.99990D+00,
     *0.99991D+00,0.99992D+00,0.99996D+00,0.10000D+01,0.99998D+00,
     *0.99996D+00,0.99997D+00,0.99998D+00,0.99998D+00,0.99999D+00,
     *0.99999D+00,0.99999D+00,0.99964D+00,0.99930D+00,0.99866D+00,
     *0.99803D+00,0.99727D+00,0.99651D+00,0.97854D+00,0.96056D+00,
     *0.97775D+00,0.99494D+00,0.99397D+00,0.99300D+00,0.99157D+00,
     *0.99015D+00,0.98872D+00,0.98620D+00,0.98367D+00,0.98115D+00,
     *0.97799D+00,0.97484D+00,0.97168D+00,0.96853D+00,0.96774D+00,
     *0.96695D+00,0.96616D+00,0.96537D+00,0.96458D+00,0.96379D+00,
     *0.96364D+00,0.96349D+00,0.96334D+00,0.96318D+00,0.96303D+00,
     *0.96288D+00,0.96273D+00,0.96155D+00,0.96037D+00,0.95919D+00,
     *0.95800D+00,0.95682D+00,0.95564D+00,0.95446D+00,0.95194D+00,
     *0.94942D+00,0.94691D+00,0.94439D+00,0.94187D+00,0.93898D+00,
     *0.93609D+00,0.93321D+00,0.93032D+00,0.92507D+00,0.91983D+00,
     *0.91458D+00,0.90933D+00,0.90408D+00,0.89884D+00,0.89359D+00,
     *0.88834D+00,0.88310D+00,0.87785D+00,0.87260D+00,0.86735D+00,
     *0.86211D+00,0.85686D+00,0.85161D+00,0.84637D+00,0.84112D+00/
!---------------------------------------------------------------------!
!  L2-MO Auger Intensity.  DFL2AUG(2)+L2-MO                           !
!      Table 3. E. J. McGuire, Phy. Rev. A, 3(1971)587.               !
!      Linear interpolation is applied for element which is not       !
!      included in this table.                                        !
!---------------------------------------------------------------------!
      DATA (DFL2AUG(3,IIZ),IIZ=1,100)/
     *0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,
     *0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,
     *0.00000D+00,0.10000D+01,0.10000D+01,0.10000D+01,0.10000D+01,
     *0.10000D+01,0.10000D+01,0.10000D+01,0.10000D+01,0.99990D+00,
     *0.99991D+00,0.99992D+00,0.99996D+00,0.10000D+01,0.99998D+00,
     *0.99996D+00,0.99997D+00,0.99998D+00,0.99998D+00,0.99999D+00,
     *0.99999D+00,0.99999D+00,0.99964D+00,0.99930D+00,0.99866D+00,
     *0.99803D+00,0.99757D+00,0.99710D+00,0.98091D+00,0.96472D+00,
     *0.97996D+00,0.99521D+00,0.99424D+00,0.99327D+00,0.99182D+00,
     *0.99038D+00,0.98893D+00,0.98753D+00,0.98612D+00,0.98472D+00,
     *0.98381D+00,0.98290D+00,0.98199D+00,0.98108D+00,0.98077D+00,
     *0.98045D+00,0.98014D+00,0.97983D+00,0.97951D+00,0.97920D+00,
     *0.97868D+00,0.97816D+00,0.97764D+00,0.97712D+00,0.97660D+00,
     *0.97608D+00,0.97556D+00,0.97532D+00,0.97507D+00,0.97483D+00,
     *0.97459D+00,0.97435D+00,0.97410D+00,0.97386D+00,0.97380D+00,
     *0.97374D+00,0.97369D+00,0.97363D+00,0.97357D+00,0.97225D+00,
     *0.97092D+00,0.96960D+00,0.96828D+00,0.96659D+00,0.96490D+00,
     *0.96321D+00,0.96153D+00,0.95984D+00,0.95815D+00,0.95646D+00,
     *0.95477D+00,0.95308D+00,0.95139D+00,0.94971D+00,0.94802D+00,
     *0.94633D+00,0.94464D+00,0.94295D+00,0.94126D+00,0.93957D+00/
!---------------------------------------------------------------------!
!  L2-NN Auger Intensity. DFL2AUG(3)+L2-NN                            !
!      Table 3. E. J. McGuire, Phy. Rev. A, 3(1971)587.               !
!      Linear interpolation is applied for element which is not       !
!      included in this table.                                        !
!---------------------------------------------------------------------!
      DATA (DFL2AUG(4,IIZ),IIZ=1,100)/
     *0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,
     *0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,
     *0.00000D+00,0.10000D+01,0.10000D+01,0.10000D+01,0.10000D+01,
     *0.10000D+01,0.10000D+01,0.10000D+01,0.10000D+01,0.10000D+01,
     *0.10000D+01,0.10000D+01,0.10000D+01,0.10000D+01,0.10000D+01,
     *0.10000D+01,0.10000D+01,0.10000D+01,0.10000D+01,0.10000D+01,
     *0.10000D+01,0.10000D+01,0.10000D+01,0.10000D+01,0.10000D+01,
     *0.10000D+01,0.99997D+00,0.99993D+00,0.99962D+00,0.99932D+00,
     *0.99964D+00,0.99997D+00,0.99997D+00,0.99997D+00,0.99997D+00,
     *0.99998D+00,0.99998D+00,0.99981D+00,0.99964D+00,0.99947D+00,
     *0.99912D+00,0.99877D+00,0.99842D+00,0.99807D+00,0.99794D+00,
     *0.99781D+00,0.99769D+00,0.99756D+00,0.99743D+00,0.99730D+00,
     *0.99717D+00,0.99704D+00,0.99691D+00,0.99678D+00,0.99665D+00,
     *0.99652D+00,0.99639D+00,0.99636D+00,0.99634D+00,0.99631D+00,
     *0.99629D+00,0.99626D+00,0.99624D+00,0.99621D+00,0.99567D+00,
     *0.99513D+00,0.99458D+00,0.99404D+00,0.99350D+00,0.99328D+00,
     *0.99306D+00,0.99285D+00,0.99263D+00,0.99185D+00,0.99107D+00,
     *0.99029D+00,0.98950D+00,0.98872D+00,0.98794D+00,0.98716D+00,
     *0.98638D+00,0.98560D+00,0.98482D+00,0.98403D+00,0.98325D+00,
     *0.98247D+00,0.98169D+00,0.98091D+00,0.98013D+00,0.97935D+00/
!---------------------------------------------------------------------!
!  L2-NO Auger Intensity. DFL2AUG(4)+L2-NO                            !
!      Table 3. E. J. McGuire, Phy. Rev. A, 3(1971)587.               !
!      Linear interpolation is applied for element which is not       !
!      included in this table.                                        !
!---------------------------------------------------------------------!
      DATA (DFL2AUG(5,IIZ),IIZ=1,100)/
     *0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,
     *0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,
     *0.00000D+00,0.10000D+01,0.10000D+01,0.10000D+01,0.10000D+01,
     *0.10000D+01,0.10000D+01,0.10000D+01,0.10000D+01,0.10000D+01,
     *0.10000D+01,0.10000D+01,0.10000D+01,0.10000D+01,0.10000D+01,
     *0.10000D+01,0.10000D+01,0.10000D+01,0.10000D+01,0.10000D+01,
     *0.10000D+01,0.10000D+01,0.10000D+01,0.10000D+01,0.10000D+01,
     *0.10000D+01,0.10000D+01,0.10000D+01,0.10000D+01,0.10000D+01,
     *0.10000D+01,0.10000D+01,0.10000D+01,0.10000D+01,0.10000D+01,
     *0.10000D+01,0.10000D+01,0.10000D+01,0.99999D+00,0.99999D+00,
     *0.99997D+00,0.99995D+00,0.99993D+00,0.99991D+00,0.99990D+00,
     *0.99990D+00,0.99989D+00,0.99988D+00,0.99988D+00,0.99987D+00,
     *0.99987D+00,0.99988D+00,0.99988D+00,0.99989D+00,0.99989D+00,
     *0.99990D+00,0.99990D+00,0.99989D+00,0.99987D+00,0.99986D+00,
     *0.99985D+00,0.99984D+00,0.99982D+00,0.99981D+00,0.99978D+00,
     *0.99976D+00,0.99973D+00,0.99971D+00,0.99968D+00,0.99964D+00,
     *0.99959D+00,0.99955D+00,0.99951D+00,0.99942D+00,0.99932D+00,
     *0.99923D+00,0.99914D+00,0.99905D+00,0.99895D+00,0.99886D+00,
     *0.99877D+00,0.99867D+00,0.99858D+00,0.99849D+00,0.99840D+00,
     *0.99830D+00,0.99821D+00,0.99812D+00,0.99802D+00,0.99793D+00/
!---------------------------------------------------------------------!
!  L3-MM Auger Intensity                                              !
!      Table 3. E. J. McGuire, Phy. Rev. A, 3(1971)587.               !
!      Linear interpolation is applied for element which is not       !
!      included in this table.                                        !
!---------------------------------------------------------------------!
      DATA (DFL3AUG(1,IIZ),IIZ=1,100)/
     *0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,
     *0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,
     *0.00000D+00,0.10000D+01,0.10000D+01,0.10000D+01,0.10000D+01,
     *0.10000D+01,0.10000D+01,0.10000D+01,0.99074D+00,0.97985D+00,
     *0.97989D+00,0.97994D+00,0.98742D+00,0.99491D+00,0.99303D+00,
     *0.99115D+00,0.99234D+00,0.99352D+00,0.99447D+00,0.99542D+00,
     *0.98803D+00,0.98065D+00,0.96849D+00,0.95633D+00,0.94335D+00,
     *0.93036D+00,0.92330D+00,0.91623D+00,0.89902D+00,0.88181D+00,
     *0.87808D+00,0.87435D+00,0.86712D+00,0.85988D+00,0.84055D+00,
     *0.82122D+00,0.80189D+00,0.79148D+00,0.78108D+00,0.77067D+00,
     *0.76426D+00,0.75785D+00,0.75144D+00,0.74503D+00,0.74308D+00,
     *0.74112D+00,0.73917D+00,0.73722D+00,0.73526D+00,0.73331D+00,
     *0.73077D+00,0.72823D+00,0.72569D+00,0.72316D+00,0.72062D+00,
     *0.71808D+00,0.71554D+00,0.71442D+00,0.71330D+00,0.71218D+00,
     *0.71106D+00,0.70994D+00,0.70882D+00,0.70770D+00,0.70290D+00,
     *0.69810D+00,0.69330D+00,0.68850D+00,0.68370D+00,0.68053D+00,
     *0.67737D+00,0.67420D+00,0.67103D+00,0.65478D+00,0.63854D+00,
     *0.62229D+00,0.60605D+00,0.58980D+00,0.57356D+00,0.55731D+00,
     *0.54106D+00,0.52482D+00,0.50857D+00,0.49233D+00,0.47608D+00,
     *0.45984D+00,0.44359D+00,0.42734D+00,0.41110D+00,0.39485D+00/
!---------------------------------------------------------------------!
!  L3-MN Auger Intensity. DFL3AUG(1)+L3-MN                            !
!      Table 3. E. J. McGuire, Phy. Rev. A, 3(1971)587.               !
!      Linear interpolation is applied for element which is not       !
!      included in this table.                                        !
!---------------------------------------------------------------------!
      DATA (DFL3AUG(2,IIZ),IIZ=1,100)/
     *0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,
     *0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,
     *0.00000D+00,0.10000D+01,0.10000D+01,0.10000D+01,0.10000D+01,
     *0.10000D+01,0.10000D+01,0.10000D+01,0.10000D+01,0.99990D+00,
     *0.99991D+00,0.99992D+00,0.99996D+00,0.10000D+01,0.99998D+00,
     *0.99996D+00,0.99997D+00,0.99998D+00,0.99998D+00,0.99999D+00,
     *0.99999D+00,0.99999D+00,0.99964D+00,0.99930D+00,0.99866D+00,
     *0.99803D+00,0.99727D+00,0.99651D+00,0.97854D+00,0.96056D+00,
     *0.97775D+00,0.99494D+00,0.99397D+00,0.99300D+00,0.99157D+00,
     *0.99015D+00,0.98872D+00,0.98620D+00,0.98367D+00,0.98115D+00,
     *0.97799D+00,0.97484D+00,0.97168D+00,0.96853D+00,0.96774D+00,
     *0.96695D+00,0.96616D+00,0.96537D+00,0.96458D+00,0.96379D+00,
     *0.96364D+00,0.96349D+00,0.96334D+00,0.96318D+00,0.96303D+00,
     *0.96288D+00,0.96273D+00,0.96155D+00,0.96037D+00,0.95919D+00,
     *0.95800D+00,0.95682D+00,0.95564D+00,0.95446D+00,0.95194D+00,
     *0.94942D+00,0.94691D+00,0.94439D+00,0.94187D+00,0.93898D+00,
     *0.93609D+00,0.93321D+00,0.93032D+00,0.92507D+00,0.91983D+00,
     *0.91458D+00,0.90933D+00,0.90408D+00,0.89884D+00,0.89359D+00,
     *0.88834D+00,0.88310D+00,0.87785D+00,0.87260D+00,0.86735D+00,
     *0.86211D+00,0.85686D+00,0.85161D+00,0.84637D+00,0.84112D+00/
!---------------------------------------------------------------------!
!  L3-MO Auger Intensity.  DFL3AUG(2)+L3-MO                           !
!      Table 3. E. J. McGuire, Phy. Rev. A, 3(1971)587.               !
!      Linear interpolation is applied for element which is not       !
!      included in this table.                                        !
!---------------------------------------------------------------------!
      DATA (DFL3AUG(3,IIZ),IIZ=1,100)/
     *0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,
     *0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,
     *0.00000D+00,0.10000D+01,0.10000D+01,0.10000D+01,0.10000D+01,
     *0.10000D+01,0.10000D+01,0.10000D+01,0.10000D+01,0.99990D+00,
     *0.99991D+00,0.99992D+00,0.99996D+00,0.10000D+01,0.99998D+00,
     *0.99996D+00,0.99997D+00,0.99998D+00,0.99998D+00,0.99999D+00,
     *0.99999D+00,0.99999D+00,0.99964D+00,0.99930D+00,0.99866D+00,
     *0.99803D+00,0.99757D+00,0.99710D+00,0.98091D+00,0.96472D+00,
     *0.97996D+00,0.99521D+00,0.99424D+00,0.99327D+00,0.99182D+00,
     *0.99038D+00,0.98893D+00,0.98753D+00,0.98612D+00,0.98472D+00,
     *0.98381D+00,0.98290D+00,0.98199D+00,0.98108D+00,0.98077D+00,
     *0.98045D+00,0.98014D+00,0.97983D+00,0.97951D+00,0.97920D+00,
     *0.97868D+00,0.97816D+00,0.97764D+00,0.97712D+00,0.97660D+00,
     *0.97608D+00,0.97556D+00,0.97532D+00,0.97507D+00,0.97483D+00,
     *0.97459D+00,0.97435D+00,0.97410D+00,0.97386D+00,0.97380D+00,
     *0.97374D+00,0.97369D+00,0.97363D+00,0.97357D+00,0.97225D+00,
     *0.97092D+00,0.96960D+00,0.96828D+00,0.96659D+00,0.96490D+00,
     *0.96321D+00,0.96153D+00,0.95984D+00,0.95815D+00,0.95646D+00,
     *0.95477D+00,0.95308D+00,0.95139D+00,0.94971D+00,0.94802D+00,
     *0.94633D+00,0.94464D+00,0.94295D+00,0.94126D+00,0.93957D+00/
!---------------------------------------------------------------------!
!  L3-NN Auger Intensity. DFL3AUG(3)+L3-NN                            !
!      Table 3. E. J. McGuire, Phy. Rev. A, 3(1971)587.               !
!      Linear interpolation is applied for element which is not       !
!      included in this table.                                        !
!---------------------------------------------------------------------!
      DATA (DFL3AUG(4,IIZ),IIZ=1,100)/
     *0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,
     *0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,
     *0.00000D+00,0.10000D+01,0.10000D+01,0.10000D+01,0.10000D+01,
     *0.10000D+01,0.10000D+01,0.10000D+01,0.10000D+01,0.10000D+01,
     *0.10000D+01,0.10000D+01,0.10000D+01,0.10000D+01,0.10000D+01,
     *0.10000D+01,0.10000D+01,0.10000D+01,0.10000D+01,0.10000D+01,
     *0.10000D+01,0.10000D+01,0.10000D+01,0.10000D+01,0.10000D+01,
     *0.10000D+01,0.99997D+00,0.99993D+00,0.99962D+00,0.99932D+00,
     *0.99964D+00,0.99997D+00,0.99997D+00,0.99997D+00,0.99997D+00,
     *0.99998D+00,0.99998D+00,0.99981D+00,0.99964D+00,0.99947D+00,
     *0.99912D+00,0.99877D+00,0.99842D+00,0.99807D+00,0.99794D+00,
     *0.99781D+00,0.99769D+00,0.99756D+00,0.99743D+00,0.99730D+00,
     *0.99717D+00,0.99704D+00,0.99691D+00,0.99678D+00,0.99665D+00,
     *0.99652D+00,0.99639D+00,0.99636D+00,0.99634D+00,0.99631D+00,
     *0.99629D+00,0.99626D+00,0.99624D+00,0.99621D+00,0.99567D+00,
     *0.99513D+00,0.99458D+00,0.99404D+00,0.99350D+00,0.99328D+00,
     *0.99306D+00,0.99285D+00,0.99263D+00,0.99185D+00,0.99107D+00,
     *0.99029D+00,0.98950D+00,0.98872D+00,0.98794D+00,0.98716D+00,
     *0.98638D+00,0.98560D+00,0.98482D+00,0.98403D+00,0.98325D+00,
     *0.98247D+00,0.98169D+00,0.98091D+00,0.98013D+00,0.97935D+00/
!---------------------------------------------------------------------!
!  L3-NO Auger Intensity. DFL3AUG(4)+L3-NO                            !
!      Table 3. E. J. McGuire, Phy. Rev. A, 3(1971)587.               !
!      Linear interpolation is applied for element which is not       !
!      included in this table.                                        !
!---------------------------------------------------------------------!
      DATA (DFL3AUG(5,IIZ),IIZ=1,100)/
     *0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,
     *0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,
     *0.00000D+00,0.10000D+01,0.10000D+01,0.10000D+01,0.10000D+01,
     *0.10000D+01,0.10000D+01,0.10000D+01,0.10000D+01,0.10000D+01,
     *0.10000D+01,0.10000D+01,0.10000D+01,0.10000D+01,0.10000D+01,
     *0.10000D+01,0.10000D+01,0.10000D+01,0.10000D+01,0.10000D+01,
     *0.10000D+01,0.10000D+01,0.10000D+01,0.10000D+01,0.10000D+01,
     *0.10000D+01,0.10000D+01,0.10000D+01,0.10000D+01,0.10000D+01,
     *0.10000D+01,0.10000D+01,0.10000D+01,0.10000D+01,0.10000D+01,
     *0.10000D+01,0.10000D+01,0.10000D+01,0.99999D+00,0.99999D+00,
     *0.99997D+00,0.99995D+00,0.99993D+00,0.99991D+00,0.99990D+00,
     *0.99990D+00,0.99989D+00,0.99988D+00,0.99988D+00,0.99987D+00,
     *0.99987D+00,0.99988D+00,0.99988D+00,0.99989D+00,0.99989D+00,
     *0.99990D+00,0.99990D+00,0.99989D+00,0.99987D+00,0.99986D+00,
     *0.99985D+00,0.99984D+00,0.99982D+00,0.99981D+00,0.99978D+00,
     *0.99976D+00,0.99973D+00,0.99971D+00,0.99968D+00,0.99964D+00,
     *0.99959D+00,0.99955D+00,0.99951D+00,0.99942D+00,0.99932D+00,
     *0.99923D+00,0.99914D+00,0.99905D+00,0.99895D+00,0.99886D+00,
     *0.99877D+00,0.99867D+00,0.99858D+00,0.99849D+00,0.99840D+00,
     *0.99830D+00,0.99821D+00,0.99812D+00,0.99802D+00,0.99793D+00/

      data NEPM/MXEPERMED*1/

      end

!------------------last line of egs5_block_data_atom.f------------------
!----------------------------egs5_block_set.f---------------------------
! Version: 070117-1205
! Auxillary to BLOCK DATA to initial elements in commons which contain
! arrays of length MXREG.  These cannot be initialized in block data.
!-----------------------------------------------------------------------
!23456789|123456789|123456789|123456789|123456789|123456789|123456789|12

      subroutine block_set

      implicit none

      include 'include/egs5_h.f'               ! Main EGS5 "header" file

      include 'include/egs5_bounds.f'    ! COMMONs required by EGS5 code
      include 'include/egs5_misc.f'
      include 'include/egs5_mscon.f'
      include 'include/egs5_usersc.f'
      include 'include/egs5_ms.f'
      include 'include/egs5_eiicom.f'
      include 'include/egs5_brempr.f'
      include 'include/egs5_userxt.f'
      include 'include/egs5_edge.f'
      include 'include/counters.f'

      integer i

      iblock = iblock + 1

! common/BOUNDS/
      vacdst = 1.d8
      do i = 1, MXREG
        ecut(i) = 0.d0
        pcut(i) = 0.d0
      end do

! common/MISC/
      kmpi = 12
      kmpo = 8
      dunit = 1.d0
      do i = 1, MXREG
        med(i) = 1
        rhor(i) = 0.d0
        k1Hscl(i) = 0.d0
        k1Lscl(i) = 0.d0
        ectrng(i) = 0.d0
        pctrng(i) = 0.d0
        iraylr(i) = 0
        lpolar(i) = 0
        incohr(i) = 0
        iprofr(i) = 0
        impacr(i) = 0
        nomsct(i) = 0
      end do
      med(1) = 0

! common/MS/
      tmxset = .true.

! common/MSCON/
      k1mine = 1.d30
      k1maxe = -1.d30
      k1minp = 1.d30
      k1maxp = -1.d30

! common/USERSC/
      emaxe = 0.d0
      do i = 1, MXREG
        estepr(i) = 0.d0
        esave(i) = 0.d0
      end do

! common/EIICOM/
      ieispl = 0
      neispl = 0
      feispl = 0.d0

! common/BREMPR/
      ibrdst = 0
      iprdst = 0
      ibrspl = 0
      nbrspl = 0
      fbrspl = 0.d0

! common/EDGE/
      do i = 1, MXREG
        iedgfl(i) = 0
        iauger(i) = 0
      end do

! common/USERXT/
      do i = 1, MXREG
        iphter(i) = 0
      end do

      return
      end

!------------------last line of egs5_block_set.f------------------------
!------------------------------egs5_brems.f-----------------------------
! Version: 051219-1435
! Reference: SLAC-R-730/KEK-2005-8
!-----------------------------------------------------------------------
!23456789|123456789|123456789|123456789|123456789|123456789|123456789|12

      subroutine brems
      
      implicit none

      include 'include/egs5_h.f'               ! Main EGS5 "header" file

      include 'include/egs5_brempr.f'    ! COMMONs required by EGS5 code
      include 'include/egs5_stack.f'
      include 'include/egs5_thresh.f'
      include 'include/egs5_uphiot.f'
      include 'include/egs5_useful.f'

      include 'include/counters.f'       ! Additional (non-EGS5) COMMONs

      real*8 rnnow,rnnow1,rnnow2                             ! Arguments

      real*8                                           ! Local variables
     * eie,                          ! Total energy of incident electron
     * esg,                                 ! Energy of secondary photon
     * ese,                         ! Total energy of secondary electron
     * abrems,p,h,br,del,delta,rejf,ztarg,tteie,ttese,esedei,y2max,
     * rjarg1,rjarg2,rjarg3,y2tst,y2tst1,rejmin,rejmid,rejmax,rejtop,
     * rejtst,t

      integer lvx,lvl0,lvl,idistr

      real*8 AILN2,AI2LN2                             ! Local parameters
      data
     * AILN2/1.44269E0/,                                         ! 1/ln2
     * AI2LN2/0.7213475E0/                                      ! 1/2ln2

      ibrems = ibrems + 1                  ! Count entry into subroutine
 
      eie = e(np)
      np = np + 1
      if (eie .lt. 50.0) then        ! Choose Bethe-Heitler distribution
        lvx = 1
        lvl0 = 0      
      else         ! Choose Coulomb-corrected Bethe Heitler distribution
        lvx = 2
        lvl0 = 3
      end if
      abrems = float(int(AILN2*log(eie/ap(medium))))

                                 ! Start of main sampling-rejection loop
1     continue
        call randomset(rnnow)
                                              ! Start of (1-br)/br
                                              ! subdistribution sampling
        if (0.5 .lt. (abrems*alphi(lvx,medium) + 0.5)*rnnow) then
          call randomset(rnnow)
          idistr = abrems*rnnow
          p = pwr2i(idistr+1)
          lvl = lvl0 + 1 
          call randomset(rnnow)
          if (rnnow .ge. AI2LN2) then
2           continue
              call randomset(rnnow)
              call randomset(rnnow1)
              call randomset(rnnow2)
              h = max(rnnow1,rnnow2)
              br = 1.0 - 0.5*h
              if (rnnow .gt. 0.5/br) go to 2
          else
            call randomset(rnnow)
            br = rnnow*0.5
          end if
          br = br*p
                                 ! Start of 2br subdistribution sampling
        else
          call randomset(rnnow1)
          call randomset(rnnow2)
          br = max(rnnow1,rnnow2)
          lvl = lvl0 + 2
        end if

        esg = eie*br
        if (esg .lt. ap(medium)) go to 1

        ese = eie - esg
        if (ese .lt. RM) go to 1
        del = br/ese
                                                     ! Check that Adelta
                                                     ! and Bdelta > 0
        if (del .ge. delpos(lvx,medium)) go to 1
        delta = delcm(medium)*del
        if (delta .lt. 1.0) then
          rejf = dl1(lvl,medium) + delta*(dl2(lvl,medium) + 
     *           delta*dl3(lvl,medium))
        else
          rejf = dl4(lvl,medium) + dl5(lvl,medium)*
     *           log(delta + dl6(lvl,medium))
        end if
        call randomset(rnnow)
        if (rnnow .gt. rejf) go to 1
                                                     ! Set up new photon
      if (ibrdst .ne. 1) then             ! Polar angle is m/E (default)
        theta = RM/eie
      else                                          ! Sample polar angle
        ztarg = zbrang(medium)
        tteie = eie/RM
        ttese = ese/RM
        esedei = ttese/tteie
        y2max = (PI*tteie)**2
        rjarg1 = 1.0 + esedei**2
        rjarg2 = 3.0*rjarg1 - 2.0*esedei
        rjarg3 = ((1.0 - esedei)/(2.0*tteie*esedei))**2
        y2tst1 = (1.0 + 0.0e0)**2
        rejmin = (4.0 + log(rjarg3 + ztarg/y2tst1))*
     *           (4.0*esedei*0.0e0/y2tst1 - rjarg1) + rjarg2
        y2tst1 = (1.0 + 1.0e0)**2
        rejmid = (4.0 + log(rjarg3 + ztarg/y2tst1))*
     *           (4.0*esedei*1.0e0/y2tst1 - rjarg1) + rjarg2
        y2tst1 = (1.0 + y2max)**2
        rejmax = (4.0 + log(rjarg3 + ztarg/y2tst1))*
     *           (4.0*esedei*y2max/y2tst1 - rjarg1) + rjarg2
        rejtop = max(rejmin,rejmid,rejmax)

5       continue
          call randomset(rnnow)
          y2tst =rnnow/(1.0 - rnnow + 1.0/y2max)
          y2tst1 = (1.0 + y2tst)**2
          rejtst = (4.0 + log(rjarg3 + ztarg/y2tst1))*
     *             (4.0*esedei*y2tst/y2tst1 - rjarg1) + rjarg2
          call randomset(rnnow)
          if (rnnow .gt. (rejtst/rejtop)) go to 5
        theta = sqrt(y2tst)/tteie
      end if

      call uphi(1,3)                  ! Set direction cosines for photon
                            ! Put lowest energy particle on top of stack
      if (esg .le. ese) then
        iq(np) = 0
        e(np) = esg
        e(np-1) = ese
        k1step(np) = 0.
        k1init(np) = 0.
        k1rsd(np) = 0.
      else
        iq(np) = iq(np-1)
        iq(np-1) = 0
        e(np) = ese
        e(np-1) = esg
        t = u(np)
        u(np) = u(np-1)
        u(np-1) = t
        t = v(np)
        v(np) = v(np-1)
        v(np-1) = t
        t = w(np)
        w(np) = w(np-1)
        w(np-1) = t
        k1step(np) = k1step(np-1)
        k1step(np-1) = 0.
        k1init(np) = k1init(np-1)
        k1init(np-1) = 0.
        k1rsd(np) = k1rsd(np-1)
        k1rsd(np-1) = 0.

      end if
                                                      ! ----------------
      return                                          ! Return to ELECTR
                                                      ! ----------------
      end

!------------------------last line of egs5_brems.f----------------------
!-----------------------------egs5_collis.f-----------------------------
! Version: 070808-1230
! Determines collision type for hard electron collisions
!-----------------------------------------------------------------------
!23456789|123456789|123456789|123456789|123456789|123456789|123456789|12

      subroutine collis(lelec,irl,sig0,go1)

      implicit none

      include 'include/egs5_h.f'               ! Main EGS5 "header" file

      include 'include/egs5_bounds.f'    ! COMMONs required by EGS5 code
      include 'include/egs5_brempr.f'
      include 'include/egs5_edge.f'
      include 'include/egs5_elecin.f'
      include 'include/egs5_epcont.f'
      include 'include/egs5_media.f'
      include 'include/egs5_misc.f'
      include 'include/egs5_stack.f'
      include 'include/egs5_thresh.f'
      include 'include/egs5_uphiin.f'
      include 'include/egs5_uphiot.f'
      include 'include/egs5_useful.f'
      include 'include/egs5_userxt.f'

      include 'include/counters.f'       ! Additional (non-EGS5) COMMONs

      real*8 rnnow                                           ! Arguments
      integer iarg

      integer lelec
      integer irl
      logical go1
      real*8 sig0

!  locals

      real*8 ebr1, pbr1,pbr2,frstbr,fdummy
      integer idummy,npstrt,icsplt,lelke

      icollis = icollis + 1                ! Count entry into subroutine

      go1 = .false.

      eke = e(np) - RM
      elke = log(eke)
      lelke = eke1(medium)*elke + eke0(medium)

!       ----------------------------------------------------------------
!       It is finally time to interact --- determine type of interaction
!       ----------------------------------------------------------------

1     continue
      if (lelec .lt. 0) then                                      ! e-
        ebr1 = ebr11(lelke+iextp,medium)*elke +
     *           ebr10(lelke+iextp,medium)
        if (eke .le. ap(med(irl))) ebr1 = 0.
        call randomset(rnnow)
        if (rnnow .lt. ebr1) then                              ! Brems
         go to 9
        else                                         ! Probably Moller
          if (e(np) .le. thmoll(medium)) then             ! Not Moller
            if (ebr1 .le. 0.) then                  ! Not Brems either
              go1 = .true.
              return
            end if
            go to 9                                     ! Forced Brems
          end if

          iarg = 8                                ! BEFORE call moller
!                                    =================
          if (iausfl(iarg+1) .ne. 0) call ausgab(iarg)
!                                    =================
!         ===========
          call moller         ! To determine energies and polar angles
!         ===========

          iarg = 9                                 ! AFTER call moller
!                                    =================
          if (iausfl(iarg+1) .ne. 0) call ausgab(iarg)
!                                    =================
          if(iq(np) .eq. 0) then
            return              ! KEK addition to prevent EII
                                ! K-Xray from being discarded
          endif                 ! in ELECTRA

        end if
        go1 = .true.
        return
      end if

!       ------------------------
!       Must be e+ to reach here
!       ------------------------
      pbr1 = pbr11(lelke,medium)*elke + pbr10(lelke,medium)
      if (eke .le. ap(med(irl))) pbr1 = 0.
      call randomset(rnnow)
      if (rnnow .lt. pbr1) then                                ! Brems
        go to 9
      end if
!     --------------------------------------------------
!     Otherwise, either Bhabha or Annihilation-in-Flight
!     --------------------------------------------------
      pbr2 = pbr21(lelke,medium)*elke + pbr20(lelke,medium)
      if (rnnow .lt. pbr2) then                               ! Bhabha

        iarg = 10                                 ! BEFORE call bhabha
!                                  =================
        if (iausfl(iarg+1) .ne. 0) call ausgab(iarg)
!                                  =================
!       ===========
        call bhabha           ! To determine energies and polar angles
!       ===========

        iarg = 11                                  ! AFTER call bhabha
!                                  =================
        if (iausfl(iarg+1) .ne. 0) call ausgab(iarg)
!                                  =================

      else                                    ! Annihilation-in-flight

        iarg = 12                                  ! BEFORE call annih
!                                  =================
        if (iausfl(iarg+1) .ne. 0) call ausgab(iarg)
!                                  =================
!       ==========
        call annih            ! To determine energies and polar angles
!       ==========

        uf(np) = 0.
        uf(np+1) = 0.
        vf(np) = 0.
        vf(np+1) = 0.
        wf(np) = 0.
        wf(np+1) = 0.

        iarg = 13                                   ! AFTER call annih
!                                  =================
        if (iausfl(iarg+1) .ne. 0) call ausgab(iarg)
!                                  =================
        go to 8
      end if
      go1 = .true.
      return

 8    continue                                        ! ----------------
      return                                          ! Return to SHOWER
                                                      ! ----------------
 9    continue
!     ----------------------
!     Bremsstrahlung section
!     ----------------------

      iarg = 6                                       ! BEFORE call brems
!                                =================
      if (iausfl(iarg+1) .ne. 0) call ausgab(iarg)
!                                =================
!     ==========
      call brems                ! To determine energies and polar angles
!     ==========

      uf(np) = 0.
      uf(np-1) = 0.
      vf(np) = 0.
      vf(np-1) = 0.
      wf(np) = 0.
      wf(np-1) = 0.

!     ------------------------------------------------------------------
!     The following "splitting" scheme places additional bremsstrahlung
!     photons on the stack, resetting particle weights to make the game
!     fair.  Two user inputs are required:
!     ibrspl = 0 => no additional bremsstrahlung photons (default)
!            = 1 => perform bremsstrahlung splitting
!     nbrspl = number of bremsstrahlung photons created/interaction
!     A third variable is set here:
!     fbrspl = 1/nbrspl (used to adjust the particle weights)
!     nbrspl and fbrspl change dynamically if stack overflow might occur
!     ------------------------------------------------------------------

      if (ibrspl .eq. 1) then             ! Splitting has been requested
!       -------------------
!       Set fbrspl for user
!       -------------------
        if(fbrspl.eq.0.d0) then
          if(nbrspl.gt.0) then
            fbrspl = 1.d0/float(nbrspl)
          else
            write(6,105) 
 105        FORMAT(' *** ERROR ***.  Brems splitting requested but',
     *      ' number of splits .le. 0.  Stopping')
           stop
          endif
        endif

!       ----------------------------------------------------
!       Check for stack overflow and take appropriate action
!       ----------------------------------------------------
        if (nbrspl .gt. 1 .and.
     *      (np + nbrspl) .ge. MXSTACK) then
 10       continue
            write(6,106) MXSTACK,nbrspl,(2*nbrspl + 1)/3
 106        FORMAT(' *** WARNING ***. STACK SIZE = ',I4,
     *             ' MIGHT OVERFLOW',/,
     *             '                 NBRSPL BEING REDUCED, ',
     *             I4,'-->',I4,/)
            nbrspl = (2*nbrspl + 1)/3
            fbrspl = 1./float(nbrspl)

            if (nbrspl .eq. 1) then
              write(6,107) MXSTACK
 107          FORMAT(' *** WARNING ***. STACK SIZE = ',I4,
     *               ' IS TOO SMALL',/,
     *        '                 BREMSSTRAHLUNG SPLITTING NOW SHUT OFF'/)
              ibrspl=0
            end if

            if((np+nbrspl) .lt. MXSTACK) go to 11
          go to 10
 11       continue
        end if

!       ----------------------------------------------------------------
!       Shuffle electron to the top of the stack (npstrt is a pointer
!       to original location of the electron).
!       ----------------------------------------------------------------
        if (iq(np).eq.0) then
          npstrt = np - 1
          fdummy = u(np-1)
          u(np-1) = u(np)
          u(np) = fdummy
          fdummy = v(np-1)
          v(np-1) = v(np)
          v(np) = fdummy
          fdummy = w(np-1)
          w(np-1) = w(np)
          w(np) = fdummy
          fdummy = e(np-1)
          e(np-1) = e(np)
          e(np) = fdummy
          fdummy = wt(np-1)
          wt(np-1) = wt(np)
          wt(np) = fdummy
          idummy = iq(np-1)
          iq(np-1) = iq(np)
          iq(np) = idummy
          idummy = latch(np-1)
          latch(np-1) = latch(np)
          latch(np) = idummy
          fdummy = uf(np-1)
          uf(np-1) = uf(np)
          uf(np) = fdummy
          fdummy = vf(np-1)
          vf(np-1) = vf(np)
          vf(np) = fdummy
          fdummy = wf(np-1)
          wf(np-1) = wf(np)
          wf(np) = fdummy
          k1step(np) = k1step(np-1)
          k1step(np-1) = 0.
          k1init(np) = k1init(np-1)
          k1init(np-1) = 0.
          k1rsd(np) = k1rsd(np-1)
          k1rsd(np-1) = 0.
        else
          npstrt = np
        end if
        wt(np-1) = wt(np-1)*fbrspl     ! Adjust weight of initial photon
        frstbr = e(np-1)                ! Store energy of initial photon

        e(np) = e(np) + e(np-1)      ! Restore electron's initial energy
                                     !  (because interaction reduced it)

        iarg = 7                                      ! AFTER call brems
!                                  =================
        if (iausfl(iarg+1) .ne. 0) call ausgab(iarg)
!                                  =================
        icsplt = 1                        ! Initialize splitting counter

 12     continue
        if (icsplt .ge. nbrspl) go to 13
          icsplt = icsplt+1

          iarg = 6                                   ! BEFORE call brems
!                                    =================
          if (iausfl(iarg+1) .ne. 0) call ausgab(iarg)
!                                    =================
!         ==========
          call brems            ! To determine energies and polar angles
!         ==========

!         --------------------------------------------------------------
!         Shuffle electron to the top of the stack (npstrt is a pointer
!         to original location of the electron).
!         --------------------------------------------------------------
          if (iq(np) .eq. 0) then
            fdummy = u(np-1)
            u(np-1) = u(np)
            u(np) = fdummy
            fdummy = v(np-1)
            v(np-1) = v(np)
            v(np) = fdummy
            fdummy = w(np-1)
            w(np-1) = w(np)
            w(np) = fdummy
            fdummy = e(np-1)
            e(np-1) = e(np)
            e(np) = fdummy
            fdummy = wt(np-1)
            wt(np-1) = wt(np)
            wt(np) = fdummy
            idummy = iq(np-1)
            iq(np-1) = iq(np)
            iq(np) = idummy
            idummy = latch(np-1)
            latch(np-1) = latch(np)
            latch(np) = idummy
            fdummy = uf(np-1)
            uf(np-1) = uf(np)
            uf(np) = fdummy
            fdummy = vf(np-1)
            vf(np-1) = vf(np)
            vf(np) = fdummy
            fdummy = wf(np-1)
            wf(np-1) = wf(np)
            wf(np) = fdummy
            k1step(np) = k1step(np-1)
            k1step(np-1) = 0.
            k1init(np) = k1init(np-1)
            k1init(np-1) = 0.
            k1rsd(np) = k1rsd(np-1)
            k1rsd(np-1) = 0.
          end if
          wt(np-1) = wt(np-1)*fbrspl           ! Adjust weight of photon
          e(np) = e(np) + e(np-1)    ! Restore electron's initial energy

          iarg = 7                                    ! AFTER call brems
!                                    =================
          if (iausfl(iarg+1) .ne. 0) call ausgab(iarg)
!                                    =================
        go to 12

 13     continue
!       ----------------------------------------------------------------
!       Restore the electron's energy to what it had after the first
!       interaction and put the electron back to it's original stack
!       location (this will prevent overflow because usually the photon
!       has lower energy).
!       ----------------------------------------------------------------
        e(np) = e(np) - frstbr
        fdummy = u(np)
        u(np) = u(npstrt)
        u(npstrt) = fdummy
        fdummy = v(np)
        v(np) = v(npstrt)
        v(npstrt) = fdummy
        fdummy = w(np)
        w(np) = w(npstrt)
        w(npstrt) = fdummy
        fdummy = e(np)
        e(np) = e(npstrt)
        e(npstrt) = fdummy
        fdummy = wt(np)
        wt(np) = wt(npstrt)
        wt(npstrt) = fdummy
        idummy = iq(np)
        iq(np) = iq(npstrt)
        iq(npstrt) = idummy
        idummy = latch(np)
        latch(np) = latch(npstrt)
        latch(npstrt) = idummy
        fdummy = uf(np)
        uf(np) = uf(npstrt)
        uf(npstrt) = fdummy
        fdummy = vf(np)
        vf(np) = vf(npstrt)
        vf(npstrt) = fdummy
        fdummy = wf(np)
        wf(np) = wf(npstrt)
        wf(npstrt) = fdummy
        fdummy = k1step(np) 
        k1step(np) = k1step(npstrt)
        k1step(npstrt) = fdummy
        fdummy = k1init(np) 
        k1init(np) = k1init(npstrt)
        k1init(npstrt) = fdummy
        fdummy = k1rsd(np) 
        k1rsd(np) = k1rsd(npstrt)
        k1rsd(npstrt) = fdummy
      end if

      iarg = 7                                        ! AFTER call brems
!                                =================
      if (iausfl(iarg+1) .ne. 0) call ausgab(iarg)
!                                =================
      if (iq(np) .eq. 0) then                         ! ----------------
        return                                        ! Return to SHOWER
      else                                            ! ----------------
        go1 = .true.
        return
      end if
      end

!-----------------------last line of egs5_collis.f----------------------
!-----------------------------egs5_compt.f------------------------------
! Version: 051219-1435
! Reference: SLAC-R-730/KEK-2005-8
!-----------------------------------------------------------------------
!23456789|123456789|123456789|123456789|123456789|123456789|123456789|12

      subroutine compt
      
      implicit none

      include 'include/egs5_h.f'               ! Main EGS5 "header" file

      include 'include/egs5_bcomp.f'     ! COMMONs required by EGS5 code
      include 'include/egs5_epcont.f'
      include 'include/egs5_misc.f'
      include 'include/egs5_stack.f'
      include 'include/egs5_thresh.f'
      include 'include/egs5_uphiot.f'
      include 'include/egs5_useful.f'

      include 'include/counters.f'       ! Additional (non-EGS5) COMMONs

      real*8 rnnow,rnnow1,rnnow2,rnnow3                      ! Arguments
      integer iarg

      real*8                                           ! Local variables
     * eig,                                  ! Energy of incident photon
     * esg,                                 ! Energy of secondary photon
     * ese,                         ! Total energy of secondary electron
     * egp,br0i,alph1,alph2,sumalp,
     * br,a1mibr,temp,rejf3,psq,t,
     * f1,f2,f3,f4,f5,eps1,cpr,esg1,esg2,
     * etmp,xval,xlv,alamb,esedef,valloc,
     * qvalmx,esgmax,sxz
      integer ishell,iqtmp,lvallc,irloc,lxlv

      icompt = icompt + 1                  ! Count entry into subroutine

      irloc = ir(np)
      if (incohr(irloc) .ne. 1 .and. iprofr(irloc) .eq. 1) then
        write(6,101)
 101    FORMAT(' STOPPED IN SUBROUTINE COMPT',/, ' INCOHR(IR(NP)) should
     * be 1 whenever IPROFR(IR(NP)) = 1')
        stop
      end if

      eig = e(np)
      egp = eig/RM
      br0i =1. + 2.*egp
      alph1 = log(br0i)
      alph2 = egp*(br0i + 1.)/(br0i*br0i)
      sumalp = alph1 + alph2
                                    ! Start main sampling-rejection loop
1     continue
        call randomset(rnnow)
                                              ! Start of 1/br
                                              ! subdistribution sampling
        if (alph1 .ge. sumalp*rnnow) then
          call randomset(rnnow)
          br = exp(alph1*rnnow)/br0i
                                              ! Start of br
                                              ! subdistribution sampling
        else
          call randomset(rnnow1)
          call randomset(rnnow)
          if (egp .ge. (egp + 1.)*rnnow) then
            call randomset(rnnow)
            rnnow1 = max(rnnow1,rnnow)
          end if
          br = ((br0i - 1.)*rnnow1 + 1.)/br0i
        end if

        esg = br*eig                           ! Set up secondary photon

        a1mibr = 1. - br
        esedef = eig*a1mibr + RM
        temp = RM*a1mibr/esg
        sinthe = max(0.D0,temp*(2. - temp))         ! Prevent sinthe < 0
        call randomset(rnnow)
        rejf3 = 1. - br*sinthe/(1. + br*br)

        irloc = ir(np)
        if (incohr(irloc) .eq. 1) then
          medium = med(irloc)
          alamb = 0.012398520
          xval = sqrt(temp/2.)*eig/alamb
          if (xval .ge. 5.E-3 .and. xval .le. 80.) then
            xlv = log(xval)
            lxlv = sco1(medium)*xlv + sco0(medium)
            sxz = sxz1(lxlv,medium)*xlv + sxz0(lxlv,medium)
          else if (xval .gt. 80.) then
            sxz = 1.
          else
            sxz = 0.
          end if
          rejf3 = rejf3*sxz
        end if
        if (rnnow .gt. rejf3) go to 1

      sinthe = sqrt(sinthe)
      costhe = 1. - temp

      irloc = ir(np)
      if (iprofr(irloc) .eq. 1) then
        esgmax = eig - cpimev
        valloc = sqrt(eig*eig + esgmax*esgmax - 2.*eig*esgmax*costhe)
        qvalmx = (eig - esgmax - eig*esgmax*(1. - costhe)/RM)*
     *           137./valloc
        if (qvalmx .ge. 100.) then
          esg = eig/(1. + eig/RM*(1. - costhe))
          go to 2
        end if

 3      continue
        medium = med(irloc)
        call randomset(rnnow)

        if (icprof(medium) .eq. 1) then
          lvallc = cco1(medium)*rnnow + cco0(medium)
          cpr = cpr1(lvallc,medium)*rnnow + cpr0(lvallc,medium)
        end if

        if (icprof(medium) .eq. 3) then
          call randomset(rnnow1)
          do ishell=1,mxshel(medium)
            if (rnnow1 .le. elecno(ishell,medium)) go to 4
          end do
 4        continue
          lvallc = ccos1(medium)*rnnow + ccos0(medium)
          cpr = cprs1(lvallc,ishell,medium)*rnnow + 
     *          cprs0(lvallc,ishell,medium)
        end if

        f1 = (cpr/137.)*(cpr/137.)
        f2 = (1. - costhe)/RM
        f3 = (1. + f2*eig)*(1. + f2*eig) - f1
        f4 = (f1*costhe - 1. - f2*eig)
        f5 = f4*f4 - f3 + f1*f3
        eps1 = 0.0
        if (f5 .lt. eps1) go to 3
        esg1 = (-f4 - sqrt(f5))/f3*eig
        esg2 = (-f4 + sqrt(f5))/f3*eig
        call randomset(rnnow2)
        if (rnnow2 .lt. 0.5) then
          esg = esg1
        else
          esg = esg2
        end if
        if (icprof(medium) .eq. 3) esgmax = eig - capio(ishell,medium)
        if (esg .gt. esgmax .or. esg .lt. 0.) go to 3
        call randomset(rnnow3)
        if (esg .lt. esgmax*rnnow3) go to 3
 2      continue
      end if

      ese = eig - esg + RM

      if (iprofr(irloc) .eq. 1) then
        ese = ese - capio(ishell,medium)
        edep = capio(ishell,medium)
        etmp = e(np)
        e(np) = edep
        iqtmp = iq(np)
        iq(np) = 0

        iarg = 4
!                                  =================
        if (iausfl(iarg+1) .ne. 0) call ausgab(iarg)
!                                  =================
        e(np) = etmp
        iq(np) = iqtmp
      end if

      if (lpolar(irloc).eq. 0) then
!       ==============
        call uphi(2,1)
!       ==============
      else
!       =============
        call aphi(br)
!       =============
!       ==============
        call uphi(3,1)
!       ==============
      end if

      np = np + 1                            ! Set up secondary electron
      psq = esedef**2 - RMSQ

      if (psq .le. 0.0) then                 ! To avoid division by zero
        costhe = 0.0
        sinthe = -1.0
      else
        costhe = (ese + esg)*a1mibr/sqrt(psq)
        sinthe = -sqrt(max(0.D0,1.D0 - costhe*costhe))
      end if

      call uphi(3,2)                             ! Set direction cosines

                                            ! Put lowest energy particle
                                            ! on top of stack
      uf(np) = 0.
      vf(np) = 0.
      wf(np) = 0.
      k1step(np) = 0.
      k1init(np) = 0.
      k1rsd(np) = 0.
      k1step(np-1) = 0.
      k1init(np-1) = 0.
      k1rsd(np-1) = 0.
      
      if (ese .le. esg) then
        iq(np) = -1
        e(np) = ese
        e(np-1) = esg
      else
        iq(np) = 0
        iq(np-1) = -1
        e(np) = esg
        e(np-1) = ese
        t = u(np)
        u(np) = u(np-1)
        u(np-1) = t
        t = v(np)
        v(np) = v(np-1)
        v(np-1) = t
        t = w(np)
        w(np) = w(np-1)
        w(np-1) = t
        t=uf(np)
        uf(np) = uf(np-1)
        uf(np-1) = t
        t = vf(np)
        vf(np) = vf(np-1)
        vf(np-1) = t
        t = wf(np)
        wf(np) = wf(np-1)
        wf(np-1) = t
      end if
                                                      ! ----------------
      return                                          ! Return to PHOTON
                                                      ! ----------------
      end

!-----------------------last line of egs5_compt.f-----------------------
!-----------------------------egs5_edgbin.f-----------------------------
! Version: 051219-1435
! Reference: SLAC-R-730/KEK-2005-8
!-----------------------------------------------------------------------
!23456789|123456789|123456789|123456789|123456789|123456789|123456789|12

      subroutine edgbin

      implicit none

      include 'include/egs5_h.f'               ! Main EGS5 "header" file

      include 'include/egs5_edge.f'      ! COMMONs required by EGS5 code
      include 'include/egs5_brempr.f'
      include 'include/egs5_epcont.f'
      include 'include/egs5_media.f'
      include 'include/egs5_misc.f'
      include 'include/egs5_photin.f'
      include 'include/egs5_stack.f'
      include 'include/egs5_thresh.f'
      include 'include/egs5_uphiot.f'
      include 'include/egs5_useful.f'

      include 'include/counters.f'       ! Additional (non-EGS5) COMMONs

      real*8 eig,eee                                   ! Local variables
      integer ii,jj,izn,ner,iz1,ikl

      iedgbin = iedgbin + 1                ! Count entry into subroutine

      do medium=1,nmed
        ner = nepm(medium)

        if (ner .gt. 20) then
          write(6,101)
 101      FORMAT(' Number of elements in medium must be less than 20 !')
          stop
        end if

        nedgb(medium) = 0
        do izn=1,ner
          iz1 = zelem(medium,izn)
          do ikl=1,4
            eee = eedge(ikl,iz1)/1000.0
            if (eee .gt. ap(medium)) then
              nedgb(medium) = nedgb(medium) + 1
              eig = log(eee)
              ledgb(nedgb(medium),medium) = ge1(medium)*eig +
     *                                      ge0(medium)
              edgb(nedgb(medium),medium) = eee
            end if
          end do
        end do

        if (nedgb(medium) .gt. 0) then
          do ii=1,nedgb(medium)
            do jj=1,nedgb(medium)
              if (ii .ne. jj) then
                if (ledgb(ii,medium) .eq. ledgb(jj,medium)) then
                  write(6,102)medium
 102              FORMAT(' K- or L-edge exists in the same fitting bin a
     *t MEDIUM=',I2,'!'/ ' It is better to produce material having a sma
     *ll UE.')
                end if
              end if
            end do
          end do
        end if
      end do
!                                                      ! ---------------
      return                                           ! Return to HATCH
!                                                      ! ---------------
      end

!-----------------------last line of egs5_edgbin.f----------------------
!------------------------------egs5_eii.f-------------------------------
! Version: 070117-1210
!          080425-1100   Add time as the time after start.
! Reference: SLAC-R-730/KEK-2005-8
!-----------------------------------------------------------------------
!23456789|123456789|123456789|123456789|123456789|123456789|123456789|12

      subroutine eii

      implicit none

      include 'include/egs5_h.f'               ! Main EGS5 "header" file

      include 'include/egs5_edge.f'      ! COMMONs required by EGS5 code
      include 'include/egs5_epcont.f'
      include 'include/egs5_eiicom.f'
      include 'include/egs5_misc.f'
      include 'include/egs5_stack.f'
      include 'include/egs5_uphiot.f'
      include 'include/egs5_useful.f'

      include 'include/counters.f'       ! Additional (non-EGS5) COMMONs

      real*8 rnnow                                           ! Arguments
      integer iarg

      real*8 capbind,etmp,ese1,ese2,ratio,esesum       ! Local variables
      integer iqtmp,jnp,ieie

      ieii = ieii + 1                      ! Count entry into subroutine

      nxray = 0
      nauger = 0
      capbind = eedge(1,iz)*0.001
      ese1 = e(np-1)
      ese2 = e(np)
      esesum = e(np) + e(np-1) - 2.*RM
      if (esesum.lt.capbind) return   ! Added to fix bug 10Dec2012/YN,HH
      if (ese1 - RM .gt. capbind .or. ese2 - RM .gt. capbind) then
        call randomset(rnnow)
        if (rnnow .gt. 0.5) then
          e(np-1) = ese1 - capbind
          e(np) = ese2
        else
          e(np-1) = ese1
          e(np) = ese2 - capbind
        end if
        if (e(np-1) .le. RM .or. e(np) .le. RM) then
          if (rnnow .le. 0.5) then
            e(np-1) = ese1 - capbind
            e(np) = ese2
          else
            e(np-1) = ese1
            e(np) = ese2 - capbind
          end if
        end if
      else
        ratio = 1. - capbind/esesum
        e(np-1) = (ese1 - rm)*ratio + RM
        e(np) = (ese2 - rm)*ratio + RM
      end if

      if (ieispl .eq. 1) then
!       -------------------
!       Set feispl for user
!       -------------------
        if(feispl.eq.0.d0) then
          if(neispl.gt.0) then
            feispl = 1.d0/float(neispl)
          else
            write(6,105)
 105        FORMAT(' *** ERROR ***.  EII splitting requested but',
     *      ' number of splits .le. 0.  Stopping')
           stop
          endif
        endif

        if (neispl .gt. 1 .and. (np + neispl) .ge. MXSTACK) then
 1        continue
            write(6,100) MXSTACK,neispl,(2*neispl+1)/3
 100        FORMAT('0*** WARNING ***. STACK SIZE = ',I4,' MIGHT OVERFLOW
     *'/ '                 NEISPL BEING REDUCED, ',I4,'-->',I4/)                
            neispl = (2*neispl + 1)/3
            feispl = 1./float(neispl)
            if (neispl .eq. 1) then
              write(6,200) MXSTACK
 200          FORMAT('0*** WARNING ***. STACK SIZE = ',I4,' IS TOO SMALL
     *'/ '                 EII SPLITTING NOW SHUT OFF'/)                        
              ieispl = 0
            end if
            if (np + neispl .lt. MXSTACK) go to 2
          go to 1
 2        continue
        end if
      else
        neispl = 1
        feispl = 1.
      end if

!     ===========
      call kshell
!     ===========

      if (nxray .ge. 1 .and. exray(1) .gt. eedge(2,iz)*0.001) then
        enew = exray(1)
        ieie = 1
      else
        enew = 0.0
        ieie = 0
      end if
      edep = capbind - enew
      etmp = e(np)
      e(np)= edep
      iqtmp = iq(np)
      iq(np) = -1

      iarg=4

!                                =================
      if (iausfl(iarg+1) .ne. 0) call ausgab(iarg)
!                                =================
      e(np) = etmp
      iq(np) = iqtmp
      if (ieie .eq. 1) then
        np = np + 1
        e(np) = enew
        iq(np) = 0
        wt(np-1) = wt(np-1)*feispl
        do jnp=1,neispl
          iq(np-1+jnp) = iq(np)
          e(np-1+jnp) = e(np)
          call randomset(rnnow)
          costhe = 2.*rnnow - 1.
          sinthe = sqrt(1. - costhe*costhe)
          u(np) = 0.
          v(np) = 0.
          w(np) = 1.

          call uphi(2,1)

          u(np-1+jnp) = u(np)
          v(np-1+jnp) = v(np)
          w(np-1+jnp) = w(np)
          x(np-1+jnp) = x(np-1)
          y(np-1+jnp) = y(np-1)
          z(np-1+jnp) = z(np-1)
          ir(np-1+jnp) = ir(np-1)
          wt(np-1+jnp) = wt(np-1)
          time(np-1+jnp) = time(np-1)
          dnear(np-1+jnp) = dnear(np-1)
          latch(np-1+jnp) = latch(np-1)
          k1step(np-1+jnp) = 0.
          k1init(np-1+jnp) = 0.
          k1rsd(np-1+jnp) = 0.
        end do
        wt(np-1) = wt(np-1)/feispl
        np = np + neispl - 1
      end if
                                                      ! ----------------
      return                                          ! Return to MOLLER
                                                      ! ----------------
      end

!------------------------last line of egs5_eii.f------------------------
!------------------------------egs5_electr.f----------------------------
! Version: 060825-0900
!          080425-1100   Add time as the time after start.
!          090114-0920   Correction for time when starting in vacuum
!          091105-0835   Replaced tvstep with tmstep
! Reference: SLAC-R-730/KEK-2005-8
!-----------------------------------------------------------------------
!23456789|123456789|123456789|123456789|123456789|123456789|123456789|12

      subroutine electr(ircode)

      implicit none

      include 'include/egs5_h.f'               ! Main EGS5 "header" file

      include 'include/egs5_bounds.f'    ! COMMONs required by EGS5 code
      include 'include/egs5_brempr.f'
      include 'include/egs5_elecin.f'
      include 'include/egs5_epcont.f'
      include 'include/egs5_media.f'
      include 'include/egs5_misc.f'
      include 'include/egs5_mults.f'
      include 'include/egs5_stack.f'
      include 'include/egs5_scpw.f'            ! Scattering power COMMON
      include 'include/egs5_ms.f'           ! Multiple scattering COMMON
      include 'include/egs5_thresh.f'
      include 'include/egs5_uphiin.f'
      include 'include/egs5_uphiot.f'
      include 'include/egs5_useful.f'
      include 'include/egs5_userpr.f'
      include 'include/egs5_usersc.f'

      include 'include/counters.f'       ! Additional (non-EGS5) COMMONs

      real*8 rnnow                                           ! Arguments
      integer ircode,iarg
      logical go1

      real*8                                           ! Local variables
     * eie,                        ! Energy (total) of incident electron
     * de,ekef,
     * estepe,range,range0,dedx,dedx0,sig,sig0,scpow0,
     * ams,blcold,tmxs

      integer ierust,idr,lelec,irl,lelke,ib,nk1i,dok1s0

      real*8 
     * ustep0,kinit0,ktotal,detot,scpow,thard,tmscat,tinel,
     * hardstep,ecsda,k1s0
      
      real*8 EPSEMFP,ENEPS                            ! Local parameters

      data
     * EPSEMFP/1.E-12/,                    ! Smallest electron mfp value
     * ENEPS/0.0005/,     ! Difference between ecut and end-point energy
     * ierust/0/

      ielectr = ielectr + 1                ! Count entry into subroutine

      deresid = 0.d0
      deinitial = 0.d0
      denstep = 0.d0
      hardstep = 0.d0

      dok1s0 = 0
      if(ircode.eq.-1) then
        if(ek1s1(1,1).ne.0.d0 .or. ek1s0(1,1).ne.0.d0) then
          dok1s0 = 1
          nk1i = 0
        endif
      endif
                                            ! --------------------------
      ircode = 1                            ! Set up for normal return
      irold = ir(np)                        ! Initialize previous region
      irl = ir(np)                          ! Region number 
                                            ! --------------------------
1     continue                              ! Start of NEW-ELECTRON loop
                                            ! --------------------------
        lelec = iq(np)
        eie = e(np)
        medium = med(irl)
        
        !-->  Vacuum  
        if (medium .eq.0 ) then
          tstep = vacdst
          go to 6
        end if
  
        !++++++++++++++++++++++++++++++++++++++++++++++++++++++++!
        ! Top of tracking loop - compute parameters
        !++++++++++++++++++++++++++++++++++++++++++++++++++++++++!

        !-->  re-enter here after a hard collision or to check cutoffs
2       continue

          !--> Check energy.  If born below cutoff or at end of last 
          !--> portion of final energy hinge, discard.  Both cases are 
          !--> indicated by e .le. ecut and deresid .eq. 0.d0
          if(e(np) .le. ecut(irl) .and. deresid.eq.0.d0) go to 14

          !--> Sample number of mfp's to transport before interacting
          if(hardstep .eq. 0.d0) then
            call randomset(rnnow)
            hardstep = max(-log(rnnow),EPSEMFP)
          end if

          !-->  re-enter loop here after an energy hinge
3         continue

          rhof=rhor(irl)/rhom(medium)

          !-->  Get energy grid parameters
          eke = eie - RM
          elke = log(eke)
          lelke = eke1(medium)*elke + eke0(medium)

          !-->  Get stopping, scattering power, range
          if (lelec .eq. -1) then
            dedx0 = ededx1(lelke,medium)*elke + ededx0(lelke,medium)
            scpow0 = escpw1(lelke,medium)*elke + escpw0(lelke,medium)
            range0 = erang1(lelke,medium)*elke + erang0(lelke,medium)
            range0 = range0 - ectrng(irl)
          else
            dedx0 = pdedx1(lelke,medium)*elke + pdedx0(lelke,medium)
            scpow0 = pscpw1(lelke,medium)*elke + pscpw0(lelke,medium)
            range0 = prang1(lelke,medium)*elke + prang0(lelke,medium)
            range0 = range0 - pctrng(irl)
          end if
          dedx = rhof*dedx0
          scpow = rhof*scpow0
          range = rhof*range0 + ENEPS/dedx

          !-->  use current energy to get Moliere parameters
          !-->  and set up test for step size max check
          if(useGSD(medium).eq.0) then
            ems = e(np)
            bms = (ems - RM)*(ems + RM)/ems**2
            ams = blcc(medium)/bms
            gms = rhof*(xcc(medium)/(ems*bms))**2
            tmxs = 1.d0 / (log(ams/gms) * gms)
          end if

          !-->  get hard cross section 
          call hardx(lelec,eke,lelke,elke,sig0)
          sig = rhof*sig0                       ! Density-ratio scaling

          if (sig .le. 0.) then
            thard = vacdst
          else
            thard = hardstep / sig
          end if

          !-->  Get a new energy loss step, set energy hinge distance
          if(denstep.eq.0.d0) then
            denstep = deresid
            estepe = estep1(lelke,medium)*elke + estep0(lelke,medium)
            detot = eke * estepe
            !-->  allow region dependent scaling
            if(estepr(irl) .ne. 0) detot = detot * estepr(irl)  
            !-->  if this step takes us below the cutoff, adjust
            if( (e(np)-detot) .lt. ecut(irl)) then
              detot = e(np)-ecut(irl)
            end if
            call randomset(rnnow)
            deinitial = rnnow * detot
            deresid = detot - deinitial
            denstep = denstep + deinitial
          end if

          if(dedx.le.0.) then
            tinel = vacdst
            range = vacdst
          else
            tinel = denstep / dedx
          end if

          !-->  re-enter loop here after a multiple scatter
4         continue

          !-->  Get the scattering strength, sent the hinge length
          if(k1step(np) .eq. 0.d0) then
            k1step(np) = k1rsd(np)

            !-->  Get max scattering strength
            if(lelec .eq. -1) then
              kinit0 = ekini1(lelke,medium)*elke + ekini0(lelke,medium)
            else
              kinit0 = pkini1(lelke,medium)*elke + pkini0(lelke,medium)
            end if

            !->  steps can be scaled by region
            if(k1Lscl(irl).ne.0.d0) then
              kinit0 = kinit0 * (k1Lscl(irl) +  k1Hscl(irl) * elke)
            end if

            !-->  Get starting scattering strength
            if(dok1s0.eq.1) then
              if(lelec .eq. -1) then
                k1s0 = ek1s1(lelke,medium)*elke + ek1s0(lelke,medium)
              else
                k1s0 = pk1s1(lelke,medium)*elke + pk1s0(lelke,medium)
              end if
              k1s0 = k1s0*(2**nk1i)
              nk1i = nk1i + 1
              if(kinit0.gt.k1s0) then
                kinit0 = k1s0
              else
                dok1s0=0
              end if
            end if

            ktotal = rhof*kinit0

            if(useGSD(medium).eq.0) then
              !->  make sure total K1 is less than that of tmxs
              if(ktotal/scpow.gt.tmxs) then
                itmxs = itmxs + 1
                if(tmxset) ktotal = tmxs*scpow
              !-> make sure that kinit gives us a valid omega0.
              !-> use 2.80 instead of e because the increase in scpow 
              !-> as particle slows means tmstep will be < tmscat.
              else 
                omega0 = ams * ktotal/scpow
                if(omega0 .lt. 2.80) then
                   ktotal = scpow * 2.80 / ams
                end if
              end if
            end if

            call randomset(rnnow)
            k1init(np) = rnnow * ktotal
            k1rsd(np) = ktotal - k1init(np)
            k1step(np) = k1step(np) + k1init(np)

          end if

          if (scpow .le. 0.) then
            tmscat = vacdst
          else
            tmscat = k1step(np) / scpow
          end if

          !-->  re-enter loop here after a boundary crossing or B 
          !-->  field step was taken
5         continue

          tstep = MIN(tmscat,tinel,thard)

          !-->  enter loop here to take vacuum step
6         continue

          irnew = ir(np)          ! New region is old region (default)
          idisc = 0                         ! Set NO discard (default)
          ustep0 = tstep                            ! Distance to event
          ustep = ustep0
          edep = 0.d0
          medold = medium

          !++++++++++++++++++++++
          ! Check user's geometry
          !++++++++++++++++++++++

          !-->  HOWFAR call necessary only if event farther than dnear
!                                    ===========
          if (ustep .gt. dnear(np))  call howfar
!                                    ===========
          !+++++++++++++++++++
!         ! USER-RANGE-DISCARD
          !+++++++++++++++++++
          if (range .lt. dnear(np) .and. e(np) .le. esave(irl) 
     &                                      .and.  medium .ne. 0) then
            if (lelec .eq. -1) then
              idisc = 1
            else             ! Signal to tell ELECTR that annihilation
              idisc = 99     ! gammas are to be produced, since they
            end if           ! can transport energy beyond range of e+
          end if
                                                   !------------------
          if (idisc .gt. 0) go to 16               ! IMMEDIATE discard
                                                   !------------------
      ! ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      ! Check for negative USTEP.  This signals a truncation problem
      ! at a boundary, which means we probably are not in the region
      ! we think we are in.  The default is to set ustep = 0. and to
      ! continue on under the assumption that the user has set IRNEW
      ! (in HOWFAR) to the region we are most likely to be in.
      ! ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
          if (ustep .lt. 0.d0) then
            if (ustep .lt. -1.D-9) then
              ierust = ierust + 1
              write(6,102) ierust,ustep,ir(np),irnew, irold,x(np),
     *                      y(np),z(np),sqrt(x(np)**2 + y(np)**2)
 102  FORMAT(I6,' NEGATIVE USTEP=',E12.6,' IR,IRNEW,IROLD=',
     *               3I4,'X,Y,Z,R=',4E10.3)
              if (ierust .gt. 1000) then
                write(6,103)
 103  FORMAT(///' STOP, TOO MANY USTEP ERRORS'///)
                stop
              end if
            end if
            ustep = 0.d0
          end if

          !++++++++++++
          ! Take a step
          !++++++++++++

          !-->  Three cases:
          !  ustep == 0  -> set new region, adjust distances
          !  ustep != ustep0 -> boundary, get new region, distances
          !  ustep == ustep0 -> event.

          !-->  positive step 
          if(ustep.ne.0) then

            if(medium.ne.0) edep = ustep * dedx

            !--> Tally - Electron TO BE TRANSPORTED distance ustep
            iarg = 0
            e(np) = e(np) - deinitial + denstep
!                                      =================
            if (iausfl(iarg+1) .ne. 0) call ausgab(iarg)
!                                      =================
            e(np) = e(np) + deinitial - denstep

            x(np) = x(np) + u(np)*ustep
            y(np) = y(np) + v(np)*ustep
            z(np) = z(np) + w(np)*ustep
            time(np) = time(np) +ustep*eie/(2.99792458d10
     1      *sqrt((eie-RM)*(eie+RM)))

!           ----------------------------------------
!           Deduct from distance to nearest boundary
!           ----------------------------------------
            dnear(np) = dnear(np) - ustep
            irold = ir(np)                    ! Save previous region

            !-->  Unless vacuum transport, decrement steps
            if(medium.ne.0) then
              thard = thard - ustep
              tmscat = tmscat - ustep
              tinel = tinel - ustep
              hardstep = thard * sig
              k1step(np) = tmscat * scpow
              denstep = tinel * dedx
            end if

          end if

          !-->  Set new region if this is not an event
          if (ustep.ne.ustep0 .or. ustep.eq.0.d0) then
            ir(np) = irnew
            irl = irnew
            medium = med(irl)
          end if 

          !--> Tally - Electron WAS TRANSPORTED distance ustep
          if (ustep .ne. 0.d0) then
            iarg = 5
            e(np) = e(np) - deinitial + denstep
                                       !================
            if (iausfl(iarg+1) .ne. 0) call ausgab(iarg)
                                       !================
            e(np) = e(np) + deinitial - denstep
          end if
                                     !-----------------
          if (idisc .lt. 0) go to 16 ! DEFERRED discard
                                     !-----------------
          !-->  next step if current medium is vacuum
          if(medium .eq. 0) then
            tstep = vacdst
            go to 6
          end if

          if(irnew.ne.irold) then
            if(ecut(irnew).gt.ecut(irold).or.med(irold).eq.0) then 
              !-->  check energy cut-offs in new regions against current
              !-->  CSDA energy (not e(np))
              ecsda = eie - deinitial + denstep
              !-->  1. discard particle below new cutoff 
              if(ecsda.le.ecut(irnew)) then
                e(np) = ecsda
                deinitial = 0.d0
                denstep = 0.d0
                deresid = 0.d0
                go to 14
              !-->  2. impose hinge immediately if transport through
              !-->  denstep puts energy below new cutoff 
              else if(ecsda - denstep .le. ecut(irnew)) then
                eie = ecsda
                e(np) = eie
                detot = e(np) - ecut(irnew)
                call randomset(rnnow)
                deinitial = rnnow * detot
                deresid = detot - deinitial
                denstep = deinitial 
                go to 3 
              !-->  3. truncate residual part of hinge if the transport
              !-->  through full hinge puts energy below new cutoff
              else if(eie - (deinitial + deresid) .le. ecut(irnew)) then
                deresid = eie - deinitial - ecut(irnew)
                go to 3
              endif
            endif

            !-->  new medium or density
            if(medium.ne.medold .or. rhor(irnew).ne.rhom(medium)) then
              !-->  trap for initial particles incident on vacuum
              if(denstep.eq.0.d0) then
                go to 2
              !-->  update all medium parameters, then take a step
              else
                go to 3
              end if
            !-->  take a step in same medium as previous step
            else
              go to 5
            end if
          end if

          !+++++++++++++++++++++++++++++++++++++++++++++++++++
          ! Event Analyis section - one of the distances is 0.
          !+++++++++++++++++++++++++++++++++++++++++++++++++++

          !-->  Energy loss hinge
          if(tinel .eq. 0.) then

            !-->  Get total energy loss over hinge
            de = deinitial + deresid

            ekef = eke - de
            eold = eie
            enew = eold - de

            edep = de
            iarg = 27        !--> Tally - just before energy hinge
                                       !================
            if (iausfl(iarg+1) .ne. 0) call ausgab(iarg)
                                       !================
            de = edep
            eie = eie - de
            e(np) = eie

            iarg = 28        !--> Tally - just after energy hinge
                                       !================
            if (iausfl(iarg+1) .ne. 0) call ausgab(iarg)
                                       !================

            !-->  if this is last hinge, discard if this is the residual 
            !-->  leg, otherwise prepare for transport through final leg
            if ( abs(eie - ecut(irl)) .lt. 1.d-14) then 
              eie = ecut(irl)
              e(np) = eie
              if(deresid.eq.0.d0) then
                go to 14
              else
                denstep = deresid
                deinitial = 0.d0
                deresid = 0.d0
              end if
            end if

            !-->  get next energy hinge step
            go to 3

          !-->  Multiple Scattering hinge
          else if(tmscat .eq. 0.0) then

            !-->  get the Moliere parameters, based on the distance
            !-->  which would have been traveled in this media having
            !-->  burned the original scattering strength

            tmstep = (k1init(np) + k1rsd(np)) / scpow

            if(useGSD(medium).eq.0) then
              omega0 = ams*tmstep*rhof
              if (omega0 .le. 2.718282) then
                iskpms = 1
              else
                iskpms = 0
                blc = log(omega0)
                blcold = blc
                if (blc .lt. 1.306853) then
                  b = -10.27666 + blc*(17.82596 - 6.468813*blc)
                else
                  ib = b0bgb + blc*b1bgb
                  if (ib .gt. nbgb) then
                    write(6,101) ib
 101                FORMAT('electr warning: IB > NBGB =',I5,' set to 8')
                    ib = nbgb
                  end if
                  b = bgb0(ib) + blc*(bgb1(ib) + blc*bgb2(ib))
                end if
              end if
            end if

            iarg = 29       !--> Tally - just before multiple scattering
                                       !================
            if (iausfl(iarg+1) .ne. 0) call ausgab(iarg)
                                       !================
            !=========
            call mscat
            !=========

            iarg = 30       !--> Tally - just after multiple scattering
                                       !================
            if (iausfl(iarg+1) .ne. 0) call ausgab(iarg)
                                       !================
!           ==============
            call uphi(2,1)                       ! Set direction cosines
!           ==============

            !--> get new mscat step
            go to 4

          !-->  Hard collision
          else if(thard .eq. 0.) then           !  hard collision

            !-->  terminate the hinge at this point.  
            e(np) = e(np) - deinitial + denstep

            deresid = 0.d0
            deinitial = 0.d0
            denstep = 0.d0

!           ===============================
            call collis(lelec,irl,sig0,go1)
!           ===============================

            !-->  shuffled stack?  return to shower
            if (iq(np) .eq. 0) then
              return
            !-->  go to top if tracking secondary first
            else if(go1) then
              go to 1
            else
            !--> get new hard distance, new energy hinge, initial e-
              eie = e(np)
              go to 2
            end if
          end if

          !-->  we reach here if we are transporting in 
          !-->  magnetic field and step was restricted
          go to 5

      !++++++++++++++++++++++++++++++
      ! CUTOFF-ENERGY DISCARD SECTION
      !++++++++++++++++++++++++++++++
 14   continue

      if (e(np) .gt. ae(medium)) then
        idr = 1                ! Electron energy below ECUT (but not AE)
      else
        idr = 2                ! Electron energy below both  AE and ECUT
      end if

      edep = e(np) - RM
      
      if(edep .ne. 0) then
        iarg = idr
                                   !================
        if (iausfl(iarg+1) .ne. 0) call ausgab(iarg)
                                   !================
      end if

      !++++++++++++++++++++++++++++++++++++++++
      ! POSITRON-ANNIHILATION (at rest) SECTION
      !++++++++++++++++++++++++++++++++++++++++
 15   continue

!     -------------------
!     Set up first photon
!     -------------------
      if (lelec .eq. 1) then
        if (edep .lt. e(np)) then
          call randomset(rnnow)
          costhe = rnnow
          call randomset(rnnow)
          if (rnnow .le. 0.5) costhe = -costhe
          sinthe = sqrt(1. - costhe**2)
          e(np) = RM
          iq(np) = 0
          u(np) = 0.                       ! Make photon go along z-axis
          v(np) = 0.
          w(np) = 1.
!         ==============
          call uphi(2,1)                         ! Set direction cosines
!         ==============
          uf(np) = 0.
          uf(np+1) = 0.
          vf(np) = 0.
          vf(np+1) = 0.
          wf(np) = 0.
          wf(np+1) = 0.
          hardstep = 0.
          denstep = 0.
          deinitial = 0.
          deresid = 0.
          k1step(np) = 0.
          k1init(np) = 0.
          k1rsd(np) = 0.

!         ------------------------------------------
!         Set up second photon in opposite direction
!         ------------------------------------------
          np = np + 1
          e(np) = RM
          iq(np) = 0
          x(np) = x(np-1)
          y(np) = y(np-1)
          z(np) = z(np-1)
          ir(np) = ir(np-1)
          wt(np) = wt(np-1)
          dnear(np) = dnear(np-1)
          latch(np) = latch(np-1)
          u(np) = -u(np-1)
          v(np) = -v(np-1)
          w(np) = -w(np-1)
          hardstep = 0.
          denstep = 0.
          deinitial = 0.
          k1step(np) = 0.
          k1init(np) = 0.
          k1rsd(np) = 0.
          time(np) = time(np-1)

          iarg = 14                   ! Positron has annihilated AT REST
!                                    =================
          if (iausfl(iarg+1) .ne. 0) call ausgab(iarg)
!                                    =================
                                                      ! ----------------
          return                                      ! Return to SHOWER
        end if                                        ! ----------------
      end if

      np = np - 1              ! Remove particle (move pointer on stack)
      ircode = 2
      return                                          ! Return to SHOWER

      !++++++++++++++++++++++++++++++++++++++++
      ! USER-REQUESTED ELECTRON DISCARD SECTION
      !++++++++++++++++++++++++++++++++++++++++
 16   continue
      
      ! adjust energy for mid-hinge discard case
      e(np) = e(np) - deinitial + denstep
      deinitial = 0.d0
      denstep = 0.d0
      deresid = 0.d0

      idisc = abs(idisc)

      if (lelec .eq. -1 .or. idisc .eq. 99) then
        edep = e(np) - RM
      else
        edep = e(np) + RM     ! positron escape
      end if

      iarg = 3                   ! User requested discard
!                                =================
      if (iausfl(iarg+1) .ne. 0) call ausgab(iarg)
!                                =================

      if(idisc .eq. 99) go to 15
      np = np - 1              ! Remove particle (move pointer on stack)
      ircode = 2
                                                      ! ----------------
      return                                          ! Return to SHOWER
                                                      ! ----------------
      end

!-----------------------last line of egs5_electr.f----------------------
!-------------------------------egs5_hardx.f----------------------------
! Version: 090303-1415
! Get hard collision cross section for electr
!-----------------------------------------------------------------------
!23456789|123456789|123456789|123456789|123456789|123456789|123456789|12

      subroutine hardx(charge,kEnergy,keIndex,keFraction,sig0)

      implicit none

      integer charge
      integer keIndex
      double precision kEnergy
      double precision keFraction

      include 'include/egs5_h.f'               ! Main EGS5 "header" file

      include 'include/egs5_edge.f'      ! COMMONs required by EGS5 code
      include 'include/egs5_elecin.f'
      include 'include/egs5_thresh.f'
      include 'include/egs5_useful.f'

      include 'include/counters.f'       ! Additional (non-EGS5) COMMONs

      double precision sig0

      integer mollerIndex
      double precision mollerThresh
      double precision logMollerThresh

      ihardx = ihardx + 1                  ! Count entry into subroutine

      if (charge .lt. 0) then                                  ! e-
        iextp = 0
        !  correction for Moller threshold ?
        if (kEnergy .lt. (thmoll(medium)-RM)*1.5) then
          mollerThresh = thmoll(medium) - RM
          logMollerThresh = log(mollerThresh)
          mollerIndex = eke1(medium)*logMollerThresh + eke0(medium)
          if (mollerIndex .eq. keIndex) then
            if (thmoll(medium)-RM .le. kEnergy) then
              iextp = 1
            else
              iextp = -1
            end if
          end if
        end if
        sig0 = esig1(keIndex+iextp,medium)*keFraction + 
     &                              esig0(keIndex+iextp,medium)

       else                                                    ! e+
         sig0 = psig1(keIndex,medium)*keFraction + 
     &                                    psig0(keIndex,medium)
       end if
      if(sig0.le.0.0)sig0=1.e-10
 
       return
       end

!-----------------------last line of egs5_hardx.f-----------------------
!-----------------------------egs5_hatch.f------------------------------
! Version: 060318-1555
! Reference: SLAC-R-730/KEK-2005-8
!-----------------------------------------------------------------------
!23456789|123456789|123456789|123456789|123456789|123456789|123456789|12

      subroutine hatch

      implicit none

      include 'include/egs5_h.f'               ! Main EGS5 "header" file

      include 'include/egs5_bcomp.f'     ! COMMONs required by EGS5 code
      include 'include/egs5_bounds.f'
      include 'include/egs5_brempr.f'
      include 'include/egs5_edge.f'
      include 'include/egs5_eiicom.f'
      include 'include/egs5_elecin.f'
      include 'include/egs5_media.f'
      include 'include/egs5_misc.f'
      include 'include/egs5_photin.f'
      include 'include/egs5_scpw.f'
      include 'include/egs5_stack.f'
      include 'include/egs5_thresh.f'
      include 'include/egs5_uphiin.f' ! Probably don't need this anymore
      include 'include/egs5_uphiot.f'
      include 'include/egs5_useful.f'
      include 'include/egs5_userpr.f'
      include 'include/egs5_usersc.f'
      include 'include/egs5_uservr.f'
      include 'include/egs5_userxt.f'

      include 'include/counters.f'       ! Additional (non-EGS5) COMMONs
      include 'include/randomm.f'

      real*8 rnnow                                           ! Arguments

      real*8                                           ! Local variables
     * zeros(3),
     * acd,asd,cost,s2c2,p,dfact,dfactr,dunitr,dfacti,pznorm,
     * sint,wss,fnsss,wid,dunito,ys,del,xs,adev,s2c2mx,
     * cthet,rdev,s2c2mn,sxx,sxy,sx,sy,xs0,xsi,xs1, tebinda,
     * ecutmn, eke, elke

      integer
     * lok(MXMED),ngs(MXMED),ngc(MXMED),neii(MXMED),
     * msge(MXMED),mge(MXMED),mseke(MXMED),
     * mleke(MXMED),mcmfp(MXMED),mrange(MXMED),
     * ib,im,nm,il,nsge,irayl,ie,i,irn,id,md,jr,neke,nseke,
     * nge,nleke,ngrim,nrange,ncmfp,j,nisub,isub,lmdn,lmdl,
     * i1st,istest,nrna,nsinss,mxsinc_loc,iss,izz,ner,is,
     * mxsim,ii,ifun,ngcim,incoh,impact,ibound,ngsim,lelke

      character mbuf(72),mdlabl(8)

      data 
     * mdlabl/' ','M','E','D','I','U','M','='/,
     * lmdl/8/,
     * lmdn/24/,
     * dunito/1./,
     * i1st/1/,
     * nsinss/37/,
     * mxsinc_loc/MXSINC/,
     * istest/0/,
     * nrna/1000/

! ---------------------
! I/O format statements
! ---------------------
1250  FORMAT(1X,14I5)
1260  FORMAT(1X,1PE14.5,4E14.5)
1270  FORMAT(72A1)
!-----------------------------------------------------------------------
1340  FORMAT(1PE20.7,4E20.7)
1350  FORMAT(' SINE TESTS,MXSINC,NSINSS=',2I5)
1360  FORMAT(' ADEV,RDEV,S2C2(MN,MX) =',1PE16.8,3E16.8)
1380  FORMAT(' TEST AT ',I7,' RANDOM ANGLES IN (0,5*PI/2)')
1390  FORMAT(' ADEV,RDEV,S2C2(MN,MX) =',1PE16.8,3E16.8)
!-----------------------------------------------------------------------
1440  FORMAT(' RAYLEIGH OPTION REQUESTED FOR MEDIUM NUMBER',I3,/)
!-----------------------------------------------------------------------
1510  FORMAT(' DATA FOR MEDIUM #',I3,', WHICH IS:',72A1)
1540  FORMAT (5A1,',RHO=',1PG11.4,',NE=',I2,',COMPOSITION IS :')
1560  FORMAT (6A1,2A1,3X,F3.0,3X,F9.0,4X,F12.0,6X,F12.0)
1570  FORMAT (6A1,2A1,',Z=',F3.0,',A=',F9.3,',PZ=',1PE12.5,',RHOZ=',
     *    1PE12.5)
1580  FORMAT(' ECHO READ:$LGN(RLC,AE,AP,UE,UP(IM))')
!-----------------------------------------------------------------------
1600  FORMAT(' ECHO READ:($LGN(DL(I,IM)/1,2,3,4,5,6/),I=1,6)')
1610  FORMAT(' ECHO READ:DELCM(IM),($LGN(ALPHI,BPAR, DELPOS(I,IM)),I=1
     *,2)')
1620  FORMAT(' ECHO READ:$LGN(XR0,TEFF0,BLCC,XCC(IM))')
1630  FORMAT(' ECHO READ:$LGN(EKE(IM)/0,1/)')
1640  FORMAT(' ECHO READ:($LGN(ESIG,PSIG,EDEDX,PDEDX,EBR1,PBR1,PBR2, T
     *MXS(I,IM)/0,1/),I=1,NEKE)')
1660  FORMAT(' ECHO READ:($LGN(GMFP,GBR1,GBR2(I,IM)/0,1/),I=1,NGE)')
1680  FORMAT(' ECHO READ:NGR(IM)')
1690  FORMAT(' ECHO READ:$LGN(RCO(IM)/0,1/)')
!-----------------------------------------------------------------------
1700  FORMAT(' ECHO READ:($LGN(RSCT(I,IM)/0,1/),I=1,NGRIM)')
1710  FORMAT(' ECHO READ:($LGN(COHE(I,IM)/0,1/),I=1,NGE)')
1720  FORMAT(' RAYLEIGH DATA AVAILABLE FOR MEDIUM',I3, ' BUT OPTION NOT 
     *REQUESTED.',/)
1730  FORMAT(' DUNIT REQUESTED&USED ARE:',1PE14.5,E14.5,'(CM.)')
!-----------------------------------------------------------------------
1830  FORMAT(' EGS SUCCESSFULLY ''HATCHED'' FOR ONE MEDIUM.')
1840  FORMAT(' EGS SUCCESSFULLY ''HATCHED'' FOR ',I5,' MEDIA.')
1850  FORMAT(' END OF FILE ON UNIT ',I2,//, ' PROGRAM STOPPED IN HATCH B
     *ECAUSE THE',/, ' FOLLOWING NAMES WERE NOT RECOGNIZED:',/)
1870  FORMAT(40X,'''',24A1,'''')
!-----------------------------------------------------------------------
2000  FORMAT(5A1,5X,F11.0,4X,I2,9X,I1,9X,I1,9X,I1)
2001  FORMAT(5A1,5X,F11.0,4X,I2,26X,I1,9X,I1,9X,I1)
!-----------------------------------------------------------------------
4090  FORMAT(' INCOHERENT OPTION REQUESTED FOR MEDIUM NUMBER',I3,/)
4110  FORMAT(' COMPTON PROFILE OPTION REQUESTED FOR MEDIUM NUMBER',I3,/)
4130  FORMAT(' E- IMPACT IONIZATION OPTION REQUESTED FOR MEDIUM NUMBER',
     *I3,/)
4280  FORMAT(' ECHO READ:$LGN(MSGE,MGE,MSEKE,MEKE,MLEKE,MCMFP,MRANGE(I
     *M)),IRAYL,IBOUND,INCOH, ICPROF(IM),IMPACT')
4340  FORMAT(' ECHO READ:TEBINDA,$LGN(GE(IM)/0,1/)')
4360  FORMAT(' STOPPED IN HATCH: REQUESTED RAYLEIGH OPTION FOR MEDIUM',
     *I3, /,' BUT RAYLEIGH DATA NOT INCLUDED IN DATA CREATED BY PEGS.')
4370  FORMAT(' STOPPED IN HATCH: REQUESTED INCOHERENT OPTION FOR MEDIUM'
     *,I3, /,' BUT INCOHERENT DATA NOT INCLUDED IN DATA CREATED BY PEGS.
     *')
4375  FORMAT(' STOPPED IN HATCH: REQUESTED INCOHERENT OPTION FOR MEDIUM'
     *,I3, /,' BUT BOUND COMPTON COMPTON CROSS SECTION NOT INCLUDED IN '
     *,'DATA CREATED',/,' BY PEGS -- USE IBOUND=1 IN PEGS.')
4380  FORMAT(' STOPPED IN HATCH: REQUESTED COMPTON PROFILE OPTION FOR ME
     *DIUM',I3, /,' BUT CORRECT COMPTON PROFILE DATA NOT INCLUDED IN ',
     *'DATA CREATED',/,' BY PEGS -- USE ICPROF=-3 IN PEGS.')
4390  FORMAT(' STOPPED IN HATCH: REQUESTED COMPTON PROFILE OPTION FOR ME
     *DIUM',I3, /,' BUT INCOHERENT DATA NOT REQUESTED IN USER CODE.',/,
     *' THIS IS PHYSICALLY INCONSISTENT -- USE INCOHR(I)=1 WHEN IPROFR('
     *,'I)=1.')
4400  FORMAT(' STOPPED IN HATCH: REQUESTED COMPTON PROFILE OPTION FOR ME
     *DIUM',I3, /,' BUT BOUND COMPTON CROSS SECTION NOT USED IN DATA ',
     *'CREATED BY PEGS.',/,' YOU MUST USE IBOUND=1 IN PEGS.')
4410  FORMAT(' STOPPED IN HATCH: REQUESTED e- IMPACT IONIZATION OPTION F
     *OR MEDIUM', I3,/,' BUT e- IMPACT IONIZATION DATA NOT INCLUDED IN D
     *ATA CREATED BY PEGS.')
4450  FORMAT(' ECHO READ:NGS(IM)')
4460  FORMAT(' ECHO READ:$LGN(SCO(IM)/0,1/)')
4470  FORMAT(' ECHO READ:($LGN(SXZ(I,IM)/0,1/),I=1,NGSIM)')
4480  FORMAT(' INCOHERENT DATA AVAILABLE FOR MEDIUM',I3, ' BUT OPTION NO
     *T REQUESTED.',/)
4490  FORMAT(' ECHO READ:NGC(IM)')
4500  FORMAT(' ECHO READ:$LGN(CCO(IM)/0,1/),CPIMEV')
4510  FORMAT(' ECHO READ:($LGN(CPR(I,IM)/0,1/),I=1,NGCIM)')
4520  FORMAT(' TOTAL COMPTON PROFILE DATA AVAILABLE FOR MEDIUM',I3,
     *' BUT OPTION NOT REQUESTED.',/)
4530  FORMAT(' ECHO READ:MXSHEL(IM),NGC(IM)')
4540  FORMAT(' ECHO READ:(ELECNO(I,IM),I=1,MXSIM)')
4550  FORMAT(' ECHO READ:(CAPIO(I,IM),I=1,MXSIM)')
4560  FORMAT(' ECHO READ:$LGN(CCOS(IM)/0,1/)')
4570  FORMAT(' ECHO READ:(($LGN(CPRS(I,IS,IM)/0,1/),IS=1,MXSIM),I=1,NGCI
     *M)')
4580  FORMAT(' SHELL COMPTON PROFILE DATA AVAILABLE FOR MEDIUM',I3,
     *' BUT OPTION NOT REQUESTED.',/)
4590  FORMAT(' ECHO READ:NEPM(IM)')
4610  FORMAT(' ECHO READ:NEII(IM)')
4620  FORMAT(' ECHO READ:$LGN(EICO(IM)/0,1/)')
4630  FORMAT(' ECHO READ:(($LGN(EII(I,IFUN,IM)/0,1/),IFUN=1,NER), I=1,NE
     *II(IM))')
4640  FORMAT(' ELECTRON IMPACT IONIZATION DATA AVAILABLE FOR MEDIUM',I3,
     *' BUT OPTION NOT REQUESTED.',/)
5000  FORMAT(' in HATCH: subroutine block_set has not been called.',/,
     * 'Please check your user code.  Aborting.')
5005  FORMAT(' Warning: Initial Energy less than 10% of UE for medium '
     * ,24a1,/ ' multiple scattering step sizes may be very large') 
5006  FORMAT( ' EMAXE set in HATCH to MIN(UE,UP+RM), = ',1pe13.4)
5007  FORMAT( ' Stopped in HATCH with EMAXE < 100 eV, = ',1pe13.4)
5008  FORMAT( ' Stopped in HATCH with EMAXE = ',1pe13.4,' > UE of ',
     *1pe13.4,' for matierial ',i4)
5009  FORMAT( ' Stopped in HATCH with EKEMAX = ',1pe13.4,' > UP+RM of ',
     *1pe13.4,' for matierial ',i4)


!-----------------------------------------------------------------------

      ihatch = ihatch + 1                  ! Count entry into subroutine

      if(iblock.ne.1) then            ! Check for block_set initizations
        write(6,5000) 
        stop
      endif

      if (i1st .ne. 0) then
        i1st=0
        if (.not.rluxset) then
          write(6,*) 'RNG ranlux not initialized:  doing so in HATCH'
          call rluxgo
        end if
        latchi = 0.0

        nisub = mxsinc_loc - 2
        fnsss = nsinss
        wid = PI5D2/float(nisub)
        wss = wid/(fnsss - 1.0)
        zeros(1) = 0.
        zeros(2) = PI
        zeros(3) = TWOPI
        do isub=1,mxsinc_loc
          sx = 0.
          sy = 0.
          sxx = 0.
          sxy = 0.
          xs0 = wid*float(isub - 2)
          xs1 = xs0 + wid
          iz = 0
          do izz=1,3
            if (xs0 .le. zeros(izz) .and. zeros(izz) .le. xs1) then
              iz = izz
              go to 1
            end if
          end do
 1        continue
          if (iz .eq. 0) then
            xsi = xs0
          else
            xsi = zeros(iz)
          end if
          do iss=1,nsinss
            xs = wid*float(isub - 2) + wss*float(iss - 1) -xsi
            ys = sin(xs + xsi)
            sx = sx + xs
            sy = sy + ys
            sxx = sxx + xs*xs
            sxy = sxy + xs*ys
          end do
          if (iz .ne. 0) then
            sin1(isub) = sxy/sxx
            sin0(isub) = -sin1(isub)*xsi
          else
            del = fnsss*sxx-sx*sx
            sin1(isub) = (fnsss*sxy - sy*sx)/del
            sin0(isub) = (sy*sxx - sx*sxy)/del - sin1(isub)*xsi
          end if
        end do

        sinc0 = 2.
        sinc1 = 1./wid
        if (istest .ne. 0) then
          adev = 0.
          rdev = 0.
          s2c2mn = 10.
          s2c2mx = 0.
          do isub=1,nisub
            do iss=1,nsinss
              theta = wid*float(isub - 1) + wss*float(iss - 1)
              cthet = PI5D2 - theta
              sinthe = sin(theta)
              costhe = sin(cthet)
              sint = sin(theta)
              cost = cos(theta)
              asd = abs(sinthe - sint)
              acd = abs(costhe - cost)
              adev = max(adev,asd,acd)
              if (sint .ne. 0.) rdev = max(rdev,asd/abs(sint))
              if (cost .ne. 0.) rdev = max(rdev,acd/abs(cost))
              s2c2 = sinthe**2 + costhe**2
              s2c2mn = min(s2c2mn,s2c2)
              s2c2mx = max(s2c2mx,s2c2)
              if (isub .lt. 11) write(6,1340) theta,sinthe,sint,
     *                                        costhe,cost
            end do
          end do
          write(6,1350) mxsinc_loc,nsinss
          write(6,1360) adev,rdev,s2c2mn,s2c2mx
          adev = 0.
          rdev = 0.
          s2c2mn = 10.
          s2c2mx = 0.
          do irn=1,nrna
            call randomset(rnnow)
            theta = rnnow*PI5D2
            cthet = PI5D2 - theta
            sinthe = sin(theta)
            costhe = sin(cthet)
            sint = sin(theta)
            cost = cos(theta)
            asd = abs(sinthe - sint)
            acd = abs(costhe - cost)
            adev = max(adev,asd,acd)
            if (sint .ne. 0.) rdev = max(rdev,asd/abs(sint))
            if (cost .ne. 0.) rdev = max(rdev,acd/abs(cost))
            s2c2 = sinthe**2 + costhe**2
            s2c2mn = min(s2c2mn,s2c2)
            s2c2mx = max(s2c2mx,s2c2)
          end do
          write(6,1380) nrna
          write(6,1390) adev,rdev,s2c2mn,s2c2mx
        end if
        p = 1.
        do i=1,50
          pwr2i(i) = p
          p = p/2.
        end do
      end if

      do j=1,nmed
        do i=1,nreg
          if (iraylr(i) .eq. 1 .and. med(i) .eq. j) then
            iraylm(j) = 1
            go to 2
          end if
        end do
 2      continue
      end do

      do j=1,nmed
        do i=1,nreg
          if (incohr(i) .eq. 1 .and. med(i).eq.j) then
            incohm(j) = 1
            go to 20
          end if
        end do
 20     continue
      end do

      do j=1,nmed
        do i=1,nreg
          if (iprofr(i) .eq. 1 .and. med(i) .eq. j) then
            iprofm(j) = 1
            go to 21
          end if
        end do
 21     continue
      end do

      do j=1,nmed
        do i=1,nreg
          if (impacr(i) .eq. 1 .and. med(i) .eq. j) then
            impacm(j) = 1
            go to 22
          end if
        end do
 22     continue
      end do

      rewind kmpi

      nm = 0
      do im=1,nmed
        lok(im) = 0
        if (iraylm(im) .eq. 1) write(6,1440) im
      end do

      do im=1,nmed
        if (incohm(im) .eq. 1) write(6,4090) im
      end do

      do im=1,nmed
        if (iprofm(im) .eq. 1) write(6,4110) im
      end do

      do im=1,nmed
        if (impacm(im) .eq. 1) write(6,4130) im
      end do

 5    continue
      read(kmpi,1270,end=1470) mbuf
      do ib=1,lmdl
        if (mbuf(ib) .ne. mdlabl(ib)) go to 5
      end do
      do 6 im=1,nmed
        do ib=1,lmdn
          il = lmdl + ib
          if (mbuf(il) .ne. media(ib,im)) go to 6
          if (ib .eq. lmdn) go to 7
        end do
 6    continue
      go to 5

 7    continue
      if (lok(im) .ne. 0) go to 5
      lok(im) = 1
      nm = nm + 1

      write(kmpo,1510) im,mbuf
      read(kmpi,2000,err=1000) (mbuf(i),i=1,5),rhom(im),nne(im),
     * iunrst(im),epstfl(im),iaprim(im)
      go to 8

 1000 backspace(kmpi)
      read(kmpi,2001) (mbuf(i),i=1,5),rhom(im),nne(im),iunrst(im),
     * epstfl(im),iaprim(im)

 8    continue
      write(kmpo,1540) (mbuf(i),i=1,5),rhom(im),nne(im)

      do ie=1,nne(im)
        read(kmpi,1560) (mbuf(i),i=1,6),(asym(im,ie,i),i=1,2),
     *   zelem(im,ie),wa(im,ie),pz(im,ie),rhoz(im,ie)
        write(kmpo,1570) (mbuf(i),i=1,6),(asym(im,ie,i),i=1,2),
     *   zelem(im,ie),wa(im,ie),pz(im,ie),rhoz(im,ie)
      end do

      write(kmpo,1580)
      read(kmpi,1260) rlcm(im),ae(im),ap(im),ue(im),up(im)
      write(kmpo,1260)rlcm(im),ae(im),ap(im),ue(im),up(im)

      te(im) = ae(im) - RM
      thmoll(im) = te(im)*2. + RM

      write(kmpo,4280)
      read(kmpi,1250) msge(im),mge(im),mseke(im),meke(im),mleke(im),
     * mcmfp(im),mrange(im),irayl,ibound,incoh,icprof(im),impact
      write(kmpo,1250) msge(im),mge(im),mseke(im),meke(im),mleke(im),
     * mcmfp(im),mrange(im),irayl,ibound,incoh,icprof(im),impact

      nsge = msge(im)
      nge = mge(im)
      nseke = mseke(im)
      neke = meke(im)
      nleke = mleke(im)
      ncmfp = mcmfp(im)
      nrange = mrange(im)

      write(kmpo,1600)
      read(kmpi,1260) (dl1(i,im),dl2(i,im),dl3(i,im),dl4(i,im),
     * dl5(i,im),dl6(i,im),i=1,6)
      write(kmpo,1260) (dl1(i,im),dl2(i,im),dl3(i,im),dl4(i,im),
     * dl5(i,im),dl6(i,im),i=1,6)
      write(kmpo,1610)
      read(kmpi,1260) delcm(im),(alphi(i,im),bpar(i,im),
     * delpos(i,im),i=1,2)
      write(kmpo,1260) delcm(im),(alphi(i,im),bpar(i,im),
     * delpos(i,im),i=1,2)
      write(kmpo,1620)
      read(kmpi,1260) xr0(im),teff0(im),blcc(im),xcc(im)
      write(kmpo,1260) xr0(im),teff0(im),blcc(im),xcc(im)
      write(kmpo,1630)
      read(kmpi,1260) eke0(im),eke1(im)
      write(kmpo,1260) eke0(im),eke1(im)
      write(kmpo,1640)
      read(kmpi,1260) (esig0(i,im),esig1(i,im),psig0(i,im),psig1(i,im),
     * ededx0(i,im),ededx1(i,im),pdedx0(i,im),pdedx1(i,im),ebr10(i,im),
     * ebr11(i,im),pbr10(i,im),pbr11(i,im),pbr20(i,im),pbr21(i,im),
     * tmxs0(i,im),tmxs1(i,im),escpw0(i,im),escpw1(i,im),pscpw0(i,im),
     * pscpw1(i,im),ekini0(i,im),ekini1(i,im),pkini0(i,im),pkini1(i,im),
     * erang0(i,im),erang1(i,im),prang0(i,im),prang1(i,im),estep0(i,im),
     * estep1(i,im),i=1,neke)
      write(kmpo,1260) (esig0(i,im),esig1(i,im),psig0(i,im),psig1(i,im),
     * ededx0(i,im),ededx1(i,im),pdedx0(i,im),pdedx1(i,im),ebr10(i,im),
     * ebr11(i,im),pbr10(i,im),pbr11(i,im),pbr20(i,im),pbr21(i,im),
     * tmxs0(i,im),tmxs1(i,im),escpw0(i,im),escpw1(i,im),pscpw0(i,im),
     * pscpw1(i,im),ekini0(i,im),ekini1(i,im),pkini0(i,im),pkini1(i,im),
     * erang0(i,im),erang1(i,im),prang0(i,im),prang1(i,im),estep0(i,im),
     * estep1(i,im),i=1,neke)

      write(kmpo,4340)
      read(kmpi,1260) tebinda,ge0(im),ge1(im)
      write(kmpo,1260) tebinda,ge0(im),ge1(im)

      write(kmpo,1660)
      read(kmpi,1260) (gmfp0(i,im),gmfp1(i,im),gbr10(i,im),gbr11(i,im),
     * gbr20(i,im),gbr21(i,im),i=1,nge)
      write(kmpo,1260) (gmfp0(i,im),gmfp1(i,im),gbr10(i,im),gbr11(i,im),
     * gbr20(i,im),gbr21(i,im),i=1,nge)

      if (iraylm(im) .eq. 1 .and. (irayl .lt. 1 .or. irayl .gt. 2)) then
        write(6,4360) im
        stop
      end if

      if (incohm(im) .eq. 1 .and. incoh .ne. 1) then
        write(6,4370) im
        stop
      end if

      if (incohm(im) .eq. 1 .and. ibound .ne. 1) then
        write(6,4375) im
        stop
      end if

      if (iprofm(im) .eq. 1) then
        if(icprof(im) .ne. 3) then
          write(6,4380) im
          stop
        else if(incohm(im) .eq. 0) then
          write(6,4390) im
          stop
        else if(ibound .eq. 0) then
          write(6,4400) im
          stop
        endif
      end if

      if (impacm(im) .eq. 1 .and. impact .eq. 0) then
        write(6,4410) im
        stop
      end if

      if (irayl .eq. 1 .or. irayl .eq. 2 .or. irayl .eq. 3) then
        write(kmpo,1680)
        read(kmpi,1250) ngr(im)
        write(kmpo,1250) ngr(im)

        ngrim = ngr(im)

        write(kmpo,1690)
        read(kmpi,1260) rco0(im),rco1(im)
        write(kmpo,1260) rco0(im),rco1(im)
        write(kmpo,1700)
        read(kmpi,1260) (rsct0(i,im),rsct1(i,im),i=1,ngrim)
        write(kmpo,1260) (rsct0(i,im),rsct1(i,im),i=1,ngrim)
        write(kmpo,1710)
        read(kmpi,1260) (cohe0(i,im),cohe1(i,im),i=1,nge)
        write(kmpo,1260) (cohe0(i,im),cohe1(i,im),i=1,nge)

        if (iraylm(im) .ne. 1) write(6,1720) im
      end if

      if (incoh .eq. 1) then
        write(kmpo,4450)
        read(kmpi,1250) ngs(im)
        write(kmpo,1250) ngs(im)
        ngsim = ngs(im)
        write(kmpo,4460)
        read(kmpi,1260) sco0(im),sco1(im)
        write(kmpo,1260) sco0(im),sco1(im)
        write(kmpo,4470)
        read(kmpi,1260) (sxz0(i,im),sxz1(i,im),i=1,ngsim)
        write(kmpo,1260) (sxz0(i,im),sxz1(i,im),i=1,ngsim)
        if (incohm(im) .ne. 1) write(6,4480) im
      end if

        if (icprof(im) .eq. 1 .or. icprof(im) .eq. 2) then
          write(kmpo,4490)
          read(kmpi,1250) ngc(im)
          write(kmpo,1250) ngc(im)
          ngcim = ngc(im)
          write(kmpo,4500)
          read(kmpi,1260) cco0(im),cco1(im),cpimev
          write(kmpo,1260) cco0(im),cco1(im),cpimev
          write(kmpo,4510)
          read(kmpi,1260) (cpr0(i,im),cpr1(i,im),i=1,ngcim)
          write(kmpo,1260) (cpr0(i,im),cpr1(i,im),i=1,ngcim)
          if (iprofm(im) .ne. 1) write(6,4520) im
        end if

        if (icprof(im) .eq. 3 .or. icprof(im) .eq. 4) then
          write(kmpo,4530)
          read(kmpi,1250) mxshel(im),ngc(im)
          write(kmpo,1250) mxshel(im),ngc(im)
          ngcim = ngc(im)
          mxsim = mxshel(im)
          write(kmpo,4540)
          read(kmpi,1260) (elecno(i,im),i=1,mxsim)
          write(kmpo,1260) (elecno(i,im),i=1,mxsim)
          write(kmpo,4550)
          read(kmpi,1260) (capio(i,im),i=1,mxsim)
          write(kmpo,1260) (capio(i,im),i=1,mxsim)
          write(kmpo,4560)
          read(kmpi,1260) ccos0(im),ccos1(im)
          write(kmpo,1260) ccos0(im),ccos1(im)
          write(kmpo,4570)
          read(kmpi,1260) ((cprs0(i,is,im),cprs1(i,is,im),is=1,mxsim),
     *                    i=1,ngcim)
          write(kmpo,1260) ((cprs0(i,is,im),cprs1(i,is,im),is=1,mxsim),
     *                     i=1,ngcim)
          if (iprofm(im) .ne. 1) write(6,4580) im
        end if

        if (impact .ge. 1) then
          write(kmpo,4590)
          read(kmpi,1250) nepm(im)
          write(kmpo,1250) nepm(im)
          ner = nepm(im)
          write(kmpo,4610)
          read(kmpi,1250) neii(im)
          write(kmpo,1250) neii(im)
          write(kmpo,4620)
          read(kmpi,1260) eico0(im),eico1(im)
          write(kmpo,1260) eico0(im),eico1(im)
          write(kmpo,4630)
          read(kmpi,1260) ((eii0(i,ifun,im),eii1(i,ifun,im),
     *                    ifun=1,ner),i=1,neii(im))
          write(kmpo,1260) ((eii0(i,ifun,im),eii1(i,ifun,im),
     *                     ifun=1,ner),i=1,neii(im))
          if (impacm(im) .ne. 1) write(6,4640) im
        end if

      if (nm .ge. nmed) go to 9
      go to 5

 9    continue

!----------------------------------------------
! Finished reading all media data at this point
!----------------------------------------------

!----------------------------------------------------------
! Check to make sure that the incident total energy is
! below the limits in PEGS (i.e., UE and UP) for all media.
!----------------------------------------------------------
      if(emaxe .eq. 0.d0) then
        emaxe=1.d50
        do j=1,nmed
          emaxe=min(ue(j),up(j)+RM,emaxe)
        end do
        write(6,5006) emaxe
      else if(emaxe .lt. 1.d-4) then
        write(6,5007) emaxe
        stop
      else
        do j=1,nmed
          if (emaxe .gt. ue(j)) then
            write(6,5008) emaxe, ue(j), j
            stop
          end if
          if (emaxe-RM .gt. up(j)) then
            write(6,5009) emaxe-RM, up(j), j
            stop
          end if
        end do
      end if

      dunitr = dunit
      if (dunit .lt. 0.) then
        id = max0(1,min0(MXMED,int(-dunit)))
        dunit = rlcm(id)
      end if
      if (dunit .ne. 1.) write(6,1730) dunitr,dunit

      do im=1,nmed
        dfact = rlcm(im)/dunit
        dfacti = 1.0/dfact

        i = 1
        go to 11
 10     i = i + 1
 11     if (i - meke(im) .gt. 0) go to 12
        esig0(i,im) = esig0(i,im)*dfacti
        esig1(i,im) = esig1(i,im)*dfacti
        psig0(i,im) = psig0(i,im)*dfacti
        psig1(i,im) = psig1(i,im)*dfacti
        ededx0(i,im) = ededx0(i,im)*dfacti
        ededx1(i,im) = ededx1(i,im)*dfacti
        pdedx0(i,im) = pdedx0(i,im)*dfacti
        pdedx1(i,im) = pdedx1(i,im)*dfacti
        tmxs0(i,im) = tmxs0(i,im)*dfact
        tmxs1(i,im) = tmxs1(i,im)*dfact
        go to 10
 12     continue

        i = 1
        go to 14
 13     i = i + 1
 14     if (i - mleke(im) .gt. 0) go to 15
        if (dunitr.eq.1) then
          dfactr = 1.d0
        else
          dfactr = 1.d0 / dunit
        end if
        escpw0(i,im) = escpw0(i,im)*dfactr
        escpw1(i,im) = escpw1(i,im)*dfactr
        pscpw0(i,im) = pscpw0(i,im)*dfactr
        pscpw1(i,im) = pscpw1(i,im)*dfactr
        ekini0(i,im) = ekini0(i,im)*dfactr
        ekini1(i,im) = ekini1(i,im)*dfactr
        pkini0(i,im) = pkini0(i,im)*dfactr
        pkini1(i,im) = pkini1(i,im)*dfactr
        erang0(i,im) = erang0(i,im)*dfactr
        erang1(i,im) = erang1(i,im)*dfactr
        prang0(i,im) = prang0(i,im)*dfactr
        prang1(i,im) = prang1(i,im)*dfactr
        go to 13
 15     continue

        teff0(im) = teff0(im)*dfact
        blcc(im) = blcc(im)*dfacti
        xcc(im) = xcc(im)*sqrt(dfacti)
        rldu(im) = rlcm(im)/dunit

        i = 1
        go to 17
 16     i = i + 1
 17     if (i - mge(im) .gt. 0) go to 18
        gmfp0(i,im) = gmfp0(i,im)*dfact
        gmfp1(i,im) = gmfp1(i,im)*dfact
        go to 16
 18     continue
      end do

      ecutmn = 1.d20
      vacdst = vacdst*dunito/dunit
      dunito = dunit
      do jr=1,nreg
        md = med(jr)
        if (md .ge. 1 .and. md .le. nmed) then
          ecut(jr) = max(ecut(jr),ae(md))
          pcut(jr) = max(pcut(jr),ap(md))
          ecutmn = min(ecutmn,ecut(jr))
          !  get the range at cutoff for e- and e+, so we don't
          !  have to comput it continually
          eke = ecut(jr) - RM
          elke = log(eke)
          lelke = eke1(md)*elke + eke0(md)
          ectrng(jr) = erang1(lelke,md)*elke + erang0(lelke,md)
          pctrng(jr) = prang1(lelke,md)*elke + prang0(lelke,md)
          if (rhor(jr) .eq. 0.0) rhor(jr) = rhom(md)
        end if
      end do

      if (ibrdst .eq. 1 .or. iprdst .gt. 0) then
        do im=1,nmed
          zbrang(im) = 0.
          pznorm = 0.
          do ie=1,nne(im)
            zbrang(im) = zbrang(im) + pz(im,ie)*zelem(im,ie)*
     *                   (zelem(im,ie) + 1.0)
            pznorm = pznorm + pz(im,ie)
          end do
          zbrang(im) = (8.116224E-05)*(zbrang(im)/pznorm)**(1./3.)
        end do
      end if

      do ii=1,nreg
        if (iedgfl(ii) .ne. 0 .or. iauger(ii) .ne. 0) then
!         ===========
          call edgbin
!         ===========
          go to 19
        end if
      end do

 19   continue

      if (nmed .eq. 1) then
        write(6,1830)
      else
        write(6,1840) nmed
      end if

!     ===========
      call rk1                         ! get problem scattering strength
!     ===========

!     ===========
      call rmsfit                        ! read multiple scattering data
!     ===========

!  Warning here about EFRAC in case UE << emaxe and using
!  old algorithm without characteristic dimension.  

      do i=1,nmed
        if(emaxe/ue(i).lt.0.1d0 .and. charD(i).eq.0.d0) then
          write(6,5005) (media(j,i),j=1,24)
        endif
      end do

      return

1470  write(6,1850) kmpi
      do im=1,nmed
        if (lok(im) .ne. 1) write(6,1870) (media(i,im),i=1,lmdn)
      end do

      stop
      end

!----------------------last line of egs5_hatch.f------------------------
!-----------------------------egs5_kauger.f-----------------------------
! Version: 051219-1435
! Reference: SLAC-R-730/KEK-2005-8
!-----------------------------------------------------------------------
!23456789|123456789|123456789|123456789|123456789|123456789|123456789|12

      subroutine kauger

      implicit none

      include 'include/egs5_h.f'               ! Main EGS5 "header" file

      include 'include/egs5_bounds.f'    ! COMMONs required by EGS5 code
      include 'include/egs5_edge.f'
      include 'include/egs5_epcont.f'
      include 'include/egs5_media.f'
      include 'include/egs5_misc.f'
      include 'include/egs5_photin.f'
      include 'include/egs5_stack.f'
      include 'include/egs5_uphiot.f'
      include 'include/egs5_useful.f'

      include 'include/counters.f'       ! Additional (non-EGS5) COMMONs

      real*8 rnnow                                           ! Arguments

      integer kaug                                     ! Local variables

      ikauger = ikauger + 1                ! Count entry into subroutine

      if (dfkaug(13,iz) .eq. 0.) return

      nauger = nauger + 1
      call randomset(rnnow)
      do kaug=1,13
        if (rnnow .le. dfkaug(kaug,iz)) then
          eauger(nauger) = ekaug(kaug,iz)*1.E-3
          go to 1
        end if
      end do
      eauger(nauger) = ekaug(14,iz)*1.E-3

 1    continue
      if (kaug .eq. 1) then
!       ==============
        call lshell(1)
!       ==============
!       ==============
        call lshell(1)
!       ==============
      else if (kaug .eq. 2) then
!       ==============
        call lshell(1)
!       ==============
!       ==============
        call lshell(2)
!       ==============
      else if (kaug .eq. 3) then
!       ==============
        call lshell(1)
!       ==============
!       ==============
        call lshell(3)
!       ==============
      else if (kaug .eq. 4) then
!       ==============
        call lshell(2)
!       ==============
!       ==============
        call lshell(2)
!       ==============
      else if (kaug.eq.5) then
!       ==============
        call lshell(2)
!       ==============
!       ==============
        call lshell(3)
!       ==============
      else if (kaug .eq. 6) then
!       ==============
        call lshell(3)
!       ==============
!       ==============
        call lshell(3)
!       ==============
      else if (kaug .eq. 7 .or. kaug .eq. 10) then
!       ==============
        call lshell(1)
!       ==============
      else if (kaug .eq. 8 .or. kaug .eq. 11) then
!       ==============
        call lshell(2)
!       ==============
      else if (kaug .eq. 9 .or. kaug .eq. 12) then
!       ==============
        call lshell(3)
!       ==============
      else
        return
      end if

      return
      end

!-----------------------last line of egs5_kauger.f----------------------
!-----------------------------egs5_kshell.f-----------------------------
! Version: 051219-1435
! Reference: SLAC-R-730/KEK-2005-8
!-----------------------------------------------------------------------
!23456789|123456789|123456789|123456789|123456789|123456789|123456789|12

      subroutine kshell

      implicit none

      include 'include/egs5_h.f'               ! Main EGS5 "header" file

      include 'include/egs5_bounds.f'    ! COMMONs required by EGS5 code
      include 'include/egs5_edge.f'
      include 'include/egs5_epcont.f'
      include 'include/egs5_media.f'
      include 'include/egs5_misc.f'
      include 'include/egs5_photin.f'
      include 'include/egs5_stack.f'
      include 'include/egs5_uphiot.f'
      include 'include/egs5_useful.f'

      include 'include/counters.f'       ! Additional (non-EGS5) COMMONs

      real*8 rnnow                                           ! Arguments

      ikshell = ikshell + 1                ! Count entry into subroutine

      call randomset(rnnow)
      if (rnnow .gt. omegak(iz)) then
!       ===========
        call kauger
!       ===========
      else
!       ==========
        call kxray
!       ==========
      end if

      return

      end

!-----------------------last line of egs5_kshell.f----------------------
!------------------------------egs5_kxray.f-----------------------------
! Version: 051219-1435
! Reference: SLAC-R-730/KEK-2005-8
!-----------------------------------------------------------------------
!23456789|123456789|123456789|123456789|123456789|123456789|123456789|12

      subroutine kxray

      implicit none

      include 'include/egs5_h.f'               ! Main EGS5 "header" file

      include 'include/egs5_bounds.f'    ! COMMONs required by EGS5 code
      include 'include/egs5_edge.f'
      include 'include/egs5_epcont.f'
      include 'include/egs5_media.f'
      include 'include/egs5_misc.f'
      include 'include/egs5_photin.f'
      include 'include/egs5_stack.f'
      include 'include/egs5_uphiot.f'
      include 'include/egs5_useful.f'

      include 'include/counters.f'       ! Additional (non-EGS5) COMMONs

      real*8 rnnow                                           ! Arguments

      integer ik                                       ! Local variables

      ikxray = ikxray + 1                  ! Count entry into subroutine

      if (dfkx(9,iz) .eq. 0.) return

      nxray = nxray + 1
      call randomset(rnnow)
      do ik=1,9
        if (rnnow .le. dfkx(ik,iz)) then
          exray(nxray) = ekx(ik,iz)*1.E-3
          go to 1
        end if
      end do
      exray(nxray) = ekx(10,iz)*1.E-3

 1    continue
!                    ==============
      if (ik .eq. 1) call lshell(3)
!                    ==============
!                    ==============
      if (ik .eq. 2) call lshell(2)
!                    ==============
!                    ==============
      if (ik .eq. 3) call lshell(1)
!                    ==============
      return

      end

!-----------------------last line of egs5_kxray.f-----------------------
!-----------------------------egs5_lauger.f-----------------------------
! Version: 051219-1435
! Reference: SLAC-R-730/KEK-2005-8
!-----------------------------------------------------------------------
!23456789|123456789|123456789|123456789|123456789|123456789|123456789|12

      subroutine lauger(ll)

      implicit none

      include 'include/egs5_h.f'               ! Main EGS5 "header" file

      include 'include/egs5_bounds.f'    ! COMMONs required by EGS5 code
      include 'include/egs5_edge.f'
      include 'include/egs5_epcont.f'
      include 'include/egs5_media.f'
      include 'include/egs5_misc.f'
      include 'include/egs5_photin.f'
      include 'include/egs5_stack.f'
      include 'include/egs5_uphiot.f'
      include 'include/egs5_useful.f'

      include 'include/counters.f'       ! Additional (non-EGS5) COMMONs

      real*8 rnnow                                           ! Arguments
      integer ll

      integer laug                                     ! Local variables

      ilauger = ilauger + 1                ! Count entry into subroutine

      call randomset(rnnow)
      go to (1,2,3) ll

 1    continue
      if (dfl1aug(5,iz) .eq. 0.) return

      nauger = nauger + 1
      do laug=1,5
        if (rnnow .le. dfl1aug(laug,iz)) then
          eauger(nauger) = el1aug(laug,iz)*1.E-3
          return
        end if
      end do
      eauger(nauger) = el1aug(6,iz)*1.E-3
      return

 2    continue
      if (dfl2aug(5,iz) .eq. 0.) return

      nauger = nauger + 1
      do laug=1,5
        if (rnnow .le. dfl2aug(laug,iz)) then
          eauger(nauger) = el2aug(laug,iz)*1.E-3
          return
        end if
      end do
      eauger(nauger) = el2aug(6,iz)*1.E-3
      return

 3    continue
      if (dfl3aug(5,iz) .eq. 0.) return

      nauger = nauger + 1
      do laug=1,5
        if (rnnow .le. dfl3aug(laug,iz)) then
          eauger(nauger) = el3aug(laug,iz)*1.E-3
          return
        end if
      end do
      eauger(nauger) = el3aug(6,iz)*1.E-3
      return

      end

!-----------------------last line of egs5_lauger.f----------------------
!-----------------------------egs5_lshell.f-----------------------------
! Version: 051219-1435
! Reference: SLAC-R-730/KEK-2005-8
!-----------------------------------------------------------------------
!23456789|123456789|123456789|123456789|123456789|123456789|123456789|12

      subroutine lshell(ll)

      implicit none

      include 'include/egs5_h.f'               ! Main EGS5 "header" file

      include 'include/egs5_bounds.f'    ! COMMONs required by EGS5 code
      include 'include/egs5_edge.f'
      include 'include/egs5_epcont.f'
      include 'include/egs5_media.f'
      include 'include/egs5_misc.f'
      include 'include/egs5_photin.f'
      include 'include/egs5_stack.f'
      include 'include/egs5_uphiot.f'
      include 'include/egs5_useful.f'

      include 'include/counters.f'       ! Additional (non-EGS5) COMMONs

      real*8 rnnow                                           ! Arguments
      integer ll

      integer ickflg                                   ! Local variables

      ilshell = ilshell + 1                ! Count entry into subroutine

      ickflg = 0
      if (ll .eq. 2) go to 1
      if (ll .eq. 3) go to 2
      call randomset(rnnow)
      ebind = eedge(2,iz)*1.E-3
      if (rnnow .gt. omegal1(iz)) then
        if (rnnow .le. omegal1(iz) + f12(iz)) then
          ickflg = 1
          go to 1
        else if (rnnow .le. omegal1(iz) + f12(iz) + f13(iz)) then
          ickflg = 1
          go to 2
        else
!         ==============
          call lauger(1)
!         ==============
        end if
      else
!       =============
        call lxray(1)
!       =============
      end if
      return

 1    continue
      call randomset(rnnow)
      if (ickflg .eq. 0) then
        ebind = eedge(3,iz)*1.E-3
      end if
      if (rnnow .gt. omegal2(iz)) then
        if (rnnow .le. omegal2(iz) + f23(iz)) then
          ickflg = 1
          go to 2
        else
!         ==============
          call lauger(2)
!         ==============
        end if
      else
!       =============
        call lxray(2)
!       =============
      end if
      return

 2    continue
      if (ickflg .eq. 0) ebind = eedge(4,iz)*1.E-3
      call randomset(rnnow)
      if (rnnow .gt. omegal3(iz)) then
!       ==============
        call lauger(3)
!       ==============
      else
!       =============
        call lxray(3)
!       =============
      end if

      return

      end

!-----------------------last line of egs5_lshell.f----------------------
!------------------------------egs5_lxray.f-----------------------------
! Version: 051219-1435
! Reference: SLAC-R-730/KEK-2005-8
!-----------------------------------------------------------------------
!23456789|123456789|123456789|123456789|123456789|123456789|123456789|12

      subroutine lxray(ll)

      implicit none

      include 'include/egs5_h.f'               ! Main EGS5 "header" file

      include 'include/egs5_bounds.f'    ! COMMONs required by EGS5 code
      include 'include/egs5_edge.f'
      include 'include/egs5_epcont.f'
      include 'include/egs5_media.f'
      include 'include/egs5_misc.f'
      include 'include/egs5_photin.f'
      include 'include/egs5_stack.f'
      include 'include/egs5_uphiot.f'
      include 'include/egs5_useful.f'

      include 'include/counters.f'       ! Additional (non-EGS5) COMMONs

      real*8 rnnow                                           ! Arguments
      integer ll

      integer lx                                       ! Local variables

      ilxray = ilxray + 1                  ! Count entry into subroutine

      call randomset(rnnow)
      go to (1,2,3) ll

 1    continue
      if (dflx1(7,iz) .eq. 0.) return

      nxray = nxray + 1
      do lx=1,7
        if (rnnow .le. dflx1(lx,iz)) then
          exray(nxray) = elx1(lx,iz)*1.E-3
          return
        end if
      end do
      exray(nxray) = elx1(8,iz)*1.E-3
      return

 2    continue
      if (dflx2(4,iz) .eq. 0.) return

      nxray = nxray + 1
      do lx=1,4
        if (rnnow .le. dflx2(lx,iz)) then
          exray(nxray) = elx2(lx,iz)*1.E-3
          return
        end if
      end do
      exray(nxray) = elx2(5,iz)*1.E-3
      return

 3    continue
      if (dflx3(6,iz) .eq. 0.) return

      nxray = nxray + 1
      do lx=1,6
        if (rnnow .le. dflx3(lx,iz)) then
          exray(nxray) = elx3(lx,iz)*1.E-3
          return
        end if
      end do
      exray(nxray) = elx3(7,iz)*1.E-3
      return

      end

!-----------------------last line of egs5_lxray.f-----------------------
!-----------------------------egs5_moller.f-----------------------------
! Version: 051219-1435
! Reference: SLAC-R-730/KEK-2005-8
!-----------------------------------------------------------------------
!23456789|123456789|123456789|123456789|123456789|123456789|123456789|12

      subroutine moller
      
      implicit none

      include 'include/egs5_h.f'               ! Main EGS5 "header" file

      include 'include/egs5_brempr.f'    ! COMMONs required by EGS5 code
      include 'include/egs5_edge.f'
      include 'include/egs5_eiicom.f'
      include 'include/egs5_elecin.f'
      include 'include/egs5_epcont.f'
      include 'include/egs5_misc.f'
      include 'include/egs5_stack.f'
      include 'include/egs5_thresh.f'
      include 'include/egs5_uphiot.f'
      include 'include/egs5_useful.f'

      include 'include/counters.f'       ! Additional (non-EGS5) COMMONs

      real*8 rnnow                                           ! Arguments
      integer iarg

      real*8                                           ! Local variables
     * eie,                          ! Total energy of incident electron
     * ekin,                       ! Kinetic energy of incident electron
     * ekse2,                  ! Kinetic energy of secondary electron #2
     * ese1,                     ! Total energy of secondary electron #1
     * ese2,                     ! Total energy of secondary electron #2
     * t0,e0,extrae,e02,ep0,g2,g3,gmax,br,r,rejf,h1,dcosth,eiir,elke2

      integer ifun,lelke2

      imoller = imoller + 1                ! Count entry into subroutine
 
      eie   = e(np)
      ekin  = eie - RM
      t0    = ekin/RM
      e0    = t0 + 1.0
      extrae= eie - thmoll(medium)
      e02   = e0*e0 
      ep0   = te(medium)/ekin
      g2    = t0*t0/e02
      g3    = (2.0*t0 + 1.0)/e02
      gmax  = (1.0 + 1.25*g2)
                                            ! Sample/reject to obtain br
1     continue
        call randomset(rnnow)
        br = te(medium)/(ekin - extrae*rnnow)
         r = br/(1.0 - br)

        ! Decide whether or not to accept
        call randomset(rnnow)
        rejf = 1.0 + g2*br*br + r*(r - g3)
        rnnow = gmax*rnnow
        if (rnnow .gt. rejf) go to 1
                                                  ! Divide up the energy
      ekse2 = br*ekin
      ese1 = eie - ekse2
      ese2 = ekse2 + RM
      e(np) = ese1
      e(np+1) = ese2
                                            ! Moller angles are uniquely
                                            ! determined by kinematics
      h1 = (eie + RM)/ekin
      dcosth = h1*(ese1 - RM)/(ese1 + RM)
      sinthe = sqrt(1.0 - dcosth)
      costhe = sqrt(dcosth)
      call uphi(2,1)                             ! Set direction cosines
      np = np + 1
      iq(np)=-1
      dcosth = h1*(ese2 - RM)/(ese2 + RM)
      sinthe = - sqrt(1.0 - dcosth)
      costhe = sqrt(dcosth)
      call uphi(3,2)                             ! Set direction cosines
      k1step(np) = 0.
      k1init(np) = 0.
      k1rsd(np) = 0.

!     --------------------------
!     Electron impact ionization
!     --------------------------
      if (impacr(ir(np)) .eq. 1.and. iedgfl(ir(np)) .ne. 0) then
        call randomset(rnnow)
        eke = eie - RM
        elke2 = log(eke)
        lelke2 = eico1(medium)*elke2 + eico0(medium)
        do ifun=1,nepm(medium)
          eiir = eii1(lelke2,ifun,medium)*elke2 + 
     *           eii0(lelke2,ifun,medium)
          if (rnnow .lt. eiir) then

            iarg = 25                                  ! BEFORE call eii
!                                      =================
            if (iausfl(iarg+1) .ne. 0) call ausgab(iarg)
!                                      =================
            iz = zelem(medium,ifun)
!           ========
            call eii
!           ========
            iarg = 26                                   ! AFTER call eii
!                                      =================
            if (iausfl(iarg+1) .ne. 0) call ausgab(iarg)
!                                      =================
            return
          end if
        end do
      end if
                                                      ! ----------------
      return                                          ! Return to ELECTR
                                                      ! ----------------
      end

!-----------------------last line of egs5_moller.f----------------------
!------------------------------egs5_mscat.f-----------------------------
! Version: 060313-1005
!          091105-0835   Replaced tvstep with tmstep
! Reference: SLAC-R-730/KEK-2005-8
!-----------------------------------------------------------------------
!23456789|123456789|123456789|123456789|123456789|123456789|123456789|12

      subroutine mscat

      implicit none

      include 'include/egs5_h.f'               ! Main EGS5 "header" file

      include 'include/egs5_elecin.f'    ! COMMONs required by EGS5 code
      include 'include/egs5_epcont.f'
      include 'include/egs5_misc.f'
      include 'include/egs5_ms.f'
      include 'include/egs5_mults.f'
      include 'include/egs5_media.f'
      include 'include/egs5_stack.f'
      include 'include/egs5_thresh.f'
      include 'include/egs5_uphiin.f'
      include 'include/egs5_uphiot.f'
      include 'include/egs5_useful.f'
      include 'include/egs5_userpr.f'
      include 'include/egs5_usersc.f'

      include 'include/counters.f'       ! Additional (non-EGS5) COMMONs

      real*8 rms1,rms2,rms3,rms4,rms5,rms6,rms7,rms8         ! Arguments

      real*8                                           ! Local variables
     * g21,g22,g31,g32,g2,g3,
     * bm1,bm2,bi,bmd,xr,eta,thr,cthet
      integer i21,i22,i31,i32

      integer iegrid, iamu, iprt, ik1, im
      real*8 decade,delog,demod, xmu, xi,  b1,c1, x1,x2, fject,fmax
      real*8 ktot

      imscat = imscat + 1                  ! Count entry into subroutine
      im = medium

!     GS multiple scattering distribution.  Optional, 
!     and kinetic energy must be less than 100 MeV.

      if(useGSD(im).ne.0 .and. eke.lt.msgrid(nmsgrd(im),im)) then

        if(iq(np) .eq. -1) then
          iprt = 1
        else
          iprt = 2
        endif

        !-->  find the correct energy interval
        delog = DLOG10(eke*1.d6)
        demod = MOD(delog,1.d0)
        decade = delog - demod
        iegrid = nmsdec(im) * (decade - initde(im) + demod) - jskip(im)

        if(iegrid .ge. nmsgrd(im)) iegrid = nmsgrd(im) - 1

        !-->  randommly select which energy point to use
        fject = (eke - msgrid(iegrid,im)) / 
     &                  (msgrid(iegrid+1,im) - msgrid(iegrid,im))
        call randomset(xi)
        if(xi .lt. fject) iegrid = iegrid +1

        !-->  find the correct K1 interval
        ktot = k1rsd(np) + k1init(np)
        ik1 = DLOG(ktot/k1grd(iprt,1)) / dk1log(iprt) + 1
        if(ik1 .gt. NK1) then
          ik1 = NK1-1
        else if(ik1 .le. 0) then
          ik1 = 1
        endif

        !-->  randommly select which interval to use
        fject = (ktot - k1grd(iprt,ik1)) / 
     &                  (k1grd(iprt,ik1+1) - k1grd(iprt,ik1))
        call randomset(xi)
        if(xi .lt. fject) ik1 = ik1 +1

        !-->  first check for no-scatter probability
        call randomset(xi)
        if(xi.lt.pnoscat(iprt,iegrid,ik1,im)) return

        !-->  get the angular interval 
        call randomset(xi)
        iamu = xi * neqp(im) + 1
        !-->  if we're in the last bin, get the sub-bin number
        if(iamu .eq. neqp(im)) then
          call randomset(xi)
          if(xi .lt. ecdf(2,iprt,iegrid,ik1,im)) then
            iamu = 1
          else
            call findi(ecdf(1,iprt,iegrid,ik1,im),xi,neqa(im)+1,iamu)
          endif
          iamu = iamu + neqp(im) - 1
        endif

        b1 = ebms(iamu,iprt,iegrid,ik1,im)
        eta = eetams(iamu,iprt,iegrid,ik1,im)
        x1 = eamu(iamu,iprt,iegrid,ik1,im)
        x2 = eamu(iamu+1,iprt,iegrid,ik1,im)

        c1 = (x2 + eta) / (x2 - x1)
!        fmax = 1.d0 + b1 * (x2 - x1)**2
        fmax = 1.d0 + .25d0 * b1 * (x2 - x1)**2

        !-->  rejection loop
 6      continue
          !-->  sample Wentzel shape part of fit
          call randomset(xi)
          xmu = ((eta * xi) + (x1 * c1)) / (c1 - xi)
          !-->  rejection test
          fject = 1.d0 + b1 * (xmu - x1) * (x2 - xmu)
          call randomset(xi)
          if(xi * fmax .gt. fject) go to 6 

        costhe = 1.d0 - 2.d0 * xmu 
        sinthe =  DSQRT(1.d0 - costhe * costhe)
        iskpms = 0
        return

      !--> user or lower limit initiated skip of MS
      else if (nomsct(ir(np)).eq.1 .or. iskpms.ne.0) then
        sinthe = 0.
        costhe = 1.
        theta = 0.
        noscat = noscat + 1
        iskpms = 0
        return

      end if

      xr = sqrt(gms*tmstep*b)

!   Set bi (B-inverse) that will be used in sampling
!   (bi must not be larger than 1/lambda=1/2)
      if (b .gt. 2.) then
        bi = 1./b
      else
        bi = 0.5
      end if
      bmd = 1. + 1.75*bi
      bm1 = (1. - 2./b)/bmd
      bm2 = (1. + 0.025*bi)/bmd

                 ! -----------------------------------------------------
 1    continue   ! Loop for Bethe correction factor (or other) rejection
                 ! -----------------------------------------------------
        call randomset(rms1)
        if (rms1 .le. bm1) then                           ! Gaussian, F1
          call randomset(rms2)
          if (rms2 .eq. 0.) then
            rms2 = 1.E-30
          end if
          thr = sqrt(max(0.D0,-log(rms2)))
        else if (rms1 .le. bm2) then                          ! Tail, F3
          call randomset(rms3)
          call randomset(rms4)
          eta = max(rms3,rms4)
          i31 = b0g31 + eta*b1g31
          g31 = g310(i31) + eta*(g311(i31) + eta*g312(i31))
          i32 = b0g32 + eta*b1g32
          g32 = g320(i32) + eta*(g321(i32) + eta*g322(i32))
          g3 = g31 + g32*bi                         ! Rejection function
          call randomset(rms5)
          if (rms5 .gt. g3) go to 1
          thr = 1./eta
        else           ! Central correction, F2
          call randomset(rms6)
          thr = rms6
          i21 = b0g21 + thr*b1g21
          g21 = g210(i21) + thr*(g211(i21) + thr*g212(i21))
          i22 = b0g22 + thr*b1g22
          g22 = g220(i22) + thr*(g221(i22) + thr*g222(i22))
          g2  = g21 + g22*bi                        ! Rejection function
          call randomset(rms7)
          if (rms7 .gt. g2) go to 1
        end if

        theta = thr*xr           ! Real angle (thr is the reduced angle)
        if (theta .ge. PI) go to 1
        sinthe = sin(theta)
        call randomset(rms8)
        if (rms8**2*theta .le. sinthe) go to 2
      go to 1

 2    continue
      cthet = PI5D2 - theta
      costhe = sin(cthet)
                                                      ! ----------------
      return                                          ! Return to ELECTR
                                                      ! ----------------
      end

!-----------------------last line of egs5_mscat.f-----------------------
!------------------------------egs5_pair.f------------------------------
! Version: 051219-1435
! Reference: SLAC-R-730/KEK-2005-8
!-----------------------------------------------------------------------
!23456789|123456789|123456789|123456789|123456789|123456789|123456789|12

      subroutine pair

      implicit none

      include 'include/egs5_h.f'               ! Main EGS5 "header" file

      include 'include/egs5_brempr.f'    ! COMMONs required by EGS5 code
      include 'include/egs5_stack.f'
      include 'include/egs5_thresh.f'
      include 'include/egs5_uphiot.f'
      include 'include/egs5_useful.f'

      include 'include/counters.f'       ! Additional (non-EGS5) COMMONs

      real*8 rnnow,rnnow1,rnnow2,rnnow3                      ! Arguments

      real*8                                           ! Local variables
     * eig,                                  ! Energy of incident photon
     * ese1,                     ! Total energy of secondary electron #1
     * ese2,                     ! Total energy of secondary electron #2
     * br,del,delta,rejf,ese,pse,ztarg,tteig,ttese,ttpse,esedei,eseder,
     * ximin,rejmin,ya,xitry,galpha,gbeta,ximid,rejmid,rejtop,xitst,
     * rejtst,rtest
      integer lvx,lvl0,lvl,ichrg

      ipair = ipair + 1                    ! Count entry into subroutine

      eig = e(np)
      if (eig .le. 2.1) then               ! Below 2.1 MeV (approximate)
        call randomset(rnnow)                 ! KEK method for smoothing
        ese2 = RM + rnnow*(eig/2. - RM)       !   connection at boundary
      else
        if (eig .lt. 50.) then                  ! Above 2.1 MeV - sample
          lvx = 1
          lvl0 = 0
        else
          lvx = 2
          lvl0 = 3
        end if
                                 ! Start of main sampling-rejection loop
1       continue
          call randomset(rnnow1)
          call randomset(rnnow)
                                              ! Start of 12(br-0.5)**2
                                              ! subdistribution sampling
          if (rnnow .ge. bpar(lvx,medium)) then
            lvl = lvl0 + 1
            call randomset(rnnow2)
            call randomset(rnnow3)
            br = 0.5*(1.0 - max(rnnow1,rnnow2,rnnow3))

                             ! Start of uniform subdistribution sampling
          else
            lvl = lvl0 + 3
            br = rnnow1*0.5
          end if
                                                 ! Check that br, Adelta
                                                 ! and Cdelta > 0
          if(eig*br .lt. RM) go to 1
          del = 1.0/(eig*br*(1.0 - br))
          if (del .ge. delpos(lvx,medium)) go to 1
          delta = delcm(medium)*del
          if (delta .lt. 1.0) then
            rejf = dl1(lvl,medium) + delta*(dl2(lvl,medium) +
     *             delta*dl3(lvl,medium))
          else
            rejf = dl4(lvl,medium) + dl5(lvl,medium)*
     *             log(delta + dl6(lvl,medium))
          end if
          call randomset(rnnow)
          if (rnnow .gt. rejf) go to 1

        ese2 = br*eig
      end if
                                        ! Set up secondary electron #1
                                        ! (electron #2 has lower energy)
      ese1 = eig - ese2
      e(np) = ese1
      e(np+1) = ese2
      k1step(np) = 0.
      k1init(np) = 0.
      k1rsd(np) = 0.
      k1step(np+1) = 0.
      k1init(np+1) = 0.
      k1rsd(np+1) = 0.
                                            ! Sample to get polar angles
                                            ! of secondary electrons
                              ! Sample lowest-order angular distribution
      if ((iprdst .eq. 1) .or.
     *     ((iprdst .eq. 2) .and. (eig .lt. 4.14))) then

        do ichrg=1,2
          if (ichrg .eq. 1) then
            ese = ese1
          else
            ese = ese2
          end if
          pse = sqrt(max(0.D0,(ese - RM)*(ese + RM)))
          call randomset(rnnow)
          costhe = 1.0 - 2.0*rnnow
          sinthe = RM*sqrt((1.0 - costhe)*(1.0 + costhe))/
     *             (pse*costhe + ese)
          costhe = (ese*costhe + pse)/(pse*costhe + ese)
          if (ichrg .eq. 1) then
            call uphi(2,1)
          else
            np = np + 1
            sinthe = -sinthe
            call uphi(3,2)
          end if
        end do

        call randomset(rnnow)
        if (rnnow .le. 0.5) then
          iq(np) = 1
          iq(np-1) = -1
        else
          iq(np) = -1
          iq(np-1) = 1
        end if
        return
                                           ! Sample from Motz-Olsen-Koch
                                           ! (1969) distribution
      else if ((iprdst .eq. 2) .and.
     *         (eig .ge. 4.14)) then
        ztarg = zbrang(medium)
        tteig = eig/RM

        do ichrg=1,2
          if (ichrg .eq. 1) then
            ese = ese1
          else
            ese = ese2
          end if
          ttese = ese/RM
          ttpse = sqrt((ttese - 1.0)*(ttese + 1.0))
          esedei = ttese/(tteig - ttese)
          eseder = 1.0/esedei
          ximin = 1.0/(1.0 + (PI*ttese)**2)
          rejmin = 2.0 + 3.0*(esedei + eseder) - 4.00*(esedei +
     *             eseder + 1.0 - 4.0*(ximin - 0.5)**2)*(1.0 +
     *             0.25*log(((1.0 + eseder)*(1.0 + esedei)/
     *             (2.0*tteig))**2 + ztarg*ximin**2))
          ya = (2.0/tteig)**2
          xitry = max(0.01D0,max(ximin,min(0.5D0,sqrt(ya/ztarg))))
          galpha = 1.0 + 0.25*log(ya + ztarg*xitry**2)
          gbeta = 0.5*ztarg*xitry/(ya + ztarg*xitry**2)
          galpha = galpha - gbeta*(xitry - 0.5)
          ximid = galpha/(3.0*gbeta)
          if (galpha .ge. 0.0) then
            ximid = 0.5 - ximid + sqrt(ximid**2 + 0.25)
          else
            ximid = 0.5 - ximid - sqrt(ximid**2+0.25)
          end if
          ximid = max(0.01D0,max(ximin,min(0.5D0,ximid)))
          rejmid = 2.0 + 3.0*(esedei + eseder) - 4.0*(esedei +
     *             eseder + 1.0 - 4.0*(ximid - 0.5)**2)*(1.0 +
     *             0.25*log(((1.0 + eseder)*(1.0 + esedei)/
     *             (2.0*tteig))**2 + ztarg*ximid**2))
          rejtop = 1.02*max(rejmin,rejmid)

2         continue
            call randomset(xitst)
            rejtst = 2.0 + 3.0*(esedei + eseder) - 4.0*(esedei +
     *               eseder + 1.0 - 4.0*(xitst - 0.5)**2)*(1.0 +
     *               0.25*log(((1.0 + eseder)*(1.0 + esedei)/
     *               (2.0*tteig))**2 + ztarg*xitst**2))
            call randomset(rtest)
            theta = sqrt(1.0/xitst - 1.0)/ttese
            if ((rtest .gt. (rejtst/rejtop) .or.
     *          (theta .ge. PI))) go to 2

          sinthe=sin(theta)
          costhe=cos(theta)
          if (ichrg .eq. 1) then
            call uphi(2,1)
          else
            np = np+1
            sinthe = -sinthe
            call uphi(3,2)
          end if
          end do

        call randomset(rnnow)
        if (rnnow .le. 0.5) then
          iq(np) = 1
          iq(np-1) = -1
        else
          iq(np) = -1
          iq(np-1) = 1
        end if
        return

      ! Polar angle is m/k (default)
      else
        theta=RM/eig
      end if

      call uphi(1,1)             ! Set direction cosines for electron #1

      np = np + 1
      sinthe = -sinthe

      call uphi(3,2)             ! Set direction cosines for electron #2

                          ! Randomly decide which particle is "positron"
      call randomset(rnnow)
      if (rnnow .le. 0.5) then
        iq(np) = 1
        iq(np-1) = -1
      else
        iq(np) = -1
        iq(np-1) = 1
      end if
                                                      ! ----------------
      return                                          ! Return to PHOTON
                                                      ! ----------------
      end

!------------------------last line of egs5_pair.f-----------------------
!-----------------------------egs5_photo.f------------------------------
! Version: 051219-1435
!          080425-1100   Add time as the time after start.
! Reference: SLAC-R-730/KEK-2005-8
!-----------------------------------------------------------------------
!23456789|123456789|123456789|123456789|123456789|123456789|123456789|12

      subroutine photo

      implicit none

      include 'include/egs5_h.f'               ! Main EGS5 "header" file

      include 'include/egs5_bounds.f'    ! COMMONs required by EG55 code
      include 'include/egs5_brempr.f'
      include 'include/egs5_edge.f'
      include 'include/egs5_epcont.f'
      include 'include/egs5_media.f'
      include 'include/egs5_misc.f'
      include 'include/egs5_photin.f'
      include 'include/egs5_stack.f'
      include 'include/egs5_uphiot.f'
      include 'include/egs5_useful.f'
      include 'include/egs5_uservr.f'
      include 'include/egs5_userxt.f'

      include 'include/counters.f'       ! Additional (non-EGS5) COMMONs

      real*8 rnnow                                           ! Arguments
      integer iarg

      real*8                                           ! Local variables
     * crosk(MXEL),crosl1(MXEL),crosl2(MXEL),
     * crosl3(MXEL),crosm(MXEL),tcros(MXEL),bshk(MXEL),
     * bshl1(MXEL),bshl2(MXEL),bshl3(MXEL),pbran(MXEL),
     * eig,eigk,phol,pholk,pholk2,pholk3,total,eelec,
     * alpha,beta,gamma,ratio,rnpht,fkappa,xi,sinth2

      integer
     * irl,i,noel,iphot,ielec

      iphoto = iphoto + 1                  ! Count entry into subroutine

      nxray = 0
      nauger = 0
      irl = ir(np)
      eig = e(np)
      phol = log(eig)
      medium = med(irl)

! Calculate energy dependent sub-shell ratio
      eigk = eig*1000.D0
      pholk = log(eigk)
      pholk2 = pholk*pholk
      pholk3 = pholk2*pholk
      total = 0.

      do i=1,nne(medium)          ! Shell-wise photoelectric calculation
        iz = zelem(medium,i)
        if (eigk .le. eedge(1,iz)) then
          crosk(i) = 0.
        else
          crosk(i) = exp(pm0(1,iz) + pm1(1,iz)*pholk +
     *                   pm2(1,iz)*pholk2 + pm3(1,iz)*pholk3)
        end if
        if (pm0(2,iz) .eq. 0. .or. eigk .le. eedge(2,iz)) then
          crosl1(i) = 0.
        else
          crosl1(i) = exp(pm0(2,iz) + pm1(2,iz)*pholk +
     *                    pm2(2,iz)*pholk2 + pm3(2,iz)*pholk3)
        end if
        if (pm0(3,iz) .eq. 0. .or. eigk .le. eedge(3,iz)) then
          crosl2(i) = 0.
        else
          crosl2(i) = exp(pm0(3,iz) + pm1(3,iz)*pholk +
     *                    pm2(3,iz)*pholk2 + pm3(3,iz)*pholk3)
        end if
        if (pm0(4,iz) .eq. 0. .or. eigk .le. eedge(4,iz)) then
          crosl3(i) = 0.
        else
          crosl3(i) = exp(pm0(4,iz) + pm1(4,iz)*pholk +
     *                    pm2(4,iz)*pholk2 + pm3(4,iz)*pholk3)
        end if
        if (eigk .le. embind(iz)) then
          tcros(i) = 0.
        else
          if (pm0(5,iz) .eq. 0.) then
            crosm(i) = 0.
          else
            crosm(i) = exp(pm0(5,iz) + pm1(5,iz)*pholk +
     *                     pm2(5,iz)*pholk2 + pm3(5,iz)*pholk3)
          end if
          tcros(i) = crosk(i) + crosl1(i) + crosl2(i) +
     *               crosl3(i) + crosm(i)
          bshk(i) = crosk(i)/tcros(i)
          bshl1(i) = (crosk(i) + crosl1(i))/tcros(i)
          bshl2(i) = (crosk(i) + crosl1(i) + crosl2(i))/tcros(i)
          bshl3(i) = (tcros(i) - crosm(i))/tcros(i)
        end if
        tcros(i) = tcros(i)*pz(medium,i)
        total = total + tcros(i)
      end do

      if (total .eq. 0.) then            ! Below M-edge for all elements
        edep = eig
        iarg = 4                    ! Deposit all of the photon's energy

!                                  =================
        if (iausfl(iarg+1) .ne. 0) call ausgab(iarg)
!                                  =================
        e(np) = 0.
        return
      end if

      if (nne(medium) .eq. 1) then
        iz = zelem(medium,1)
        noel = 1
        go to 1
      end if

      do i=1,nne(medium)-1
        if (i .eq. 1) then
          pbran(i) = tcros(i)/total
        else
          pbran(i) = pbran(i-1) + tcros(i)/total
        end if
      end do

      call randomset(rnnow)
      do i=1,nne(medium)-1
        if (rnnow .le. pbran(i)) then
          iz = zelem(medium,i)
          noel = i
          go to 1
        end if
      end do

      iz = zelem(medium,nne(medium))
      noel = nne(medium)

 1    continue                  ! Determine K, L x-rays for each element
      if (eigk .le. eedge(4,iz)) then        ! Below L3 edge, treat as M
        ebind = embind(iz)*1.D-3
        edep = ebind
        go to 2                                          ! Below L3 edge
      end if

      call randomset(rnnow)                     ! Sample to decide shell
      if (rnnow .gt. bshl3(noel)) then   ! M,N,...absorption, treat as M
        ebind = embind(iz)*1.D-3
        edep = ebind
        go to 2
      else if(rnnow .le. bshk(noel)) then                 ! K absorption
!       ===========
        call kshell            
!       ===========
        ebind = eedge(1,iz)*1.D-3
      else if(rnnow .le. bshl1(noel)) then               ! L1 absorption
!       ==============
        call lshell(1)
!       ==============
      else if(rnnow .le. bshl2(noel)) then               ! L2 absorption
!       =============
        call lshell(2)
!       ==============
      else                                               ! L3 absorption
!       ==============
        call lshell(3)
!       ==============
      end if

      if (iedgfl(irl) .le. 0) nxray = 0
      if (iauger(irl) .le. 0) nauger = 0

      edep = ebind

      if (nxray .ge. 1) then
        do iphot=1,nxray
          edep = edep - exray(iphot)
        end do
      end if

      if (nauger .ge. 1) then
        do ielec=1,nauger
          edep = edep - eauger(ielec)
        end do
      end if

      if (edep .lt. 0.) edep = 0. ! To avoid numerical precision problem

 2    continue
      e(np) = edep
      iarg = 4                     ! Part of binding energy is deposited

!                                =================
      if (iausfl(iarg+1) .ne. 0) call ausgab(iarg)
!                                =================
! Set up particles
      iq(np) = -1                        ! Photoelectron (always set up)
      e(np) = eig - ebind + RM

      if (iphter(ir(np)) .eq. 1) then   ! Select photoelectron direction
        eelec = e(np)
        if (eelec .gt. ecut(ir(np))) then
          beta = sqrt((eelec - RM)*(eelec + RM))/eelec
          gamma = eelec/RM
          alpha = 0.5D0*gamma - 0.5D0 + 1.D0/gamma
          ratio = beta/alpha

 3        continue
            call randomset(rnnow)
            rnpht = 2.D0*rnnow - 1.D0
            if (ratio .le. 0.2D0) then
              fkappa = rnpht + 0.5D0*ratio*(1.D0 - rnpht)*(1.D0 + rnpht)
              costhe = (beta + fkappa)/(1.D0 + beta*fkappa)
              xi = 1.D0/(1.D0 - beta*costhe)
            else
              xi = gamma*gamma*(1.D0 + alpha*(sqrt(1.D0 +
     *             ratio*(2.D0*rnpht + ratio))- 1.D0))
              costhe = (1.D0 - 1.D0/xi)/beta
            end if
            sinth2 = max(0.D0,(1.D0 - costhe)*(1.D0 + costhe))
            call randomset(rnnow)
            if(rnnow .le. 0.5D0*(1.D0 + gamma)*sinth2*xi/gamma) go to 4
          go to 3

 4        continue
          sinthe = sqrt(sinth2)
          call uphi(2,1)
        end if
      end if
      
      if (nauger .ne. 0) then                   ! Set up Auger electrons
        do ielec=1,nauger
          np = np + 1
          e(np) = eauger(ielec) + RM
          iq(np) = -1
          call randomset(rnnow)
          costhe = 2.D0*rnnow - 1.D0
          sinthe = sqrt(1.D0 -costhe*costhe)
          u(np) = 0.
          v(np) = 0.
          w(np) = 1.D0
          call uphi(2,1)
          x(np) = x(np-1)
          y(np) = y(np-1)
          z(np) = z(np-1)
          ir(np) = ir(np-1)
          wt(np) = wt(np-1)
          time(np) = time(np-1)
          dnear(np) = dnear(np-1)
          latch(np) = latch(np-1)
          k1step(np) = 0.
          k1init(np) = 0.
          k1rsd(np) = 0.
        end do
      end if

      if (nxray .ne. 0) then                ! Set up fluorescent photons
        do iphot=1,nxray
          np = np + 1
          e(np) = exray(iphot)
          iq(np) = 0
          call randomset(rnnow)
          costhe = 2.D0*rnnow - 1.D0
          sinthe = sqrt(1.D0 - costhe*costhe)
          u(np) = 0.
          v(np) = 0.
          w(np) = 1.D0
          call uphi(2,1)
          x(np) = x(np-1)
          y(np) = y(np-1)
          z(np) = z(np-1)
          ir(np) = ir(np-1)
          wt(np) = wt(np-1)
          time(np) = time(np-1)
          dnear(np) = dnear(np-1)
          latch(np) = latch(np-1)
          k1step(np) = 0.
          k1init(np) = 0.
          k1rsd(np) = 0.
        end do
      end if
                                                      ! ----------------
      return                                          ! Return to PHOTON
                                                      ! ----------------
      end

!-----------------------last line of egs5_photo.f-----------------------
!-----------------------------egs5_photon.f-----------------------------
! Version: 051219-1435
!          080425-1100   Add time as the time after start.
!          091105-0835   Remove redundant tvstep/ustep equivalence
! Reference: SLAC-R-730/KEK-2005-8
!-----------------------------------------------------------------------
!23456789|123456789|123456789|123456789|123456789|123456789|123456789|12

      subroutine photon(ircode)

      implicit none

      include 'include/egs5_h.f'               ! Main EGS5 "header" file

      include 'include/egs5_bounds.f'    ! COMMONs required by EGS5 code
      include 'include/egs5_edge.f'
      include 'include/egs5_epcont.f'
      include 'include/egs5_media.f'
      include 'include/egs5_misc.f'
      include 'include/egs5_photin.f'
      include 'include/egs5_stack.f'
      include 'include/egs5_thresh.f'
      include 'include/egs5_uphiot.f'
      include 'include/egs5_useful.f'
      include 'include/egs5_uservr.f'

      include 'include/counters.f'       ! Additional (non-EGS5) COMMONs

      real*8 rnnow                                           ! Arguments
      integer ircode,iarg

      real*8                                           ! Local variables
     * eig,                                  ! Energy of incident photon
     * gbr1,gbr2,t,temp,
     * bexptr,dpmfp,gmfpr0,gmfp,cohfac
      integer idr,lgle,irl,iij

      real*8 EPSGMFP                                  ! Local parameters
      data EPSGMFP/1.E-6/                     ! Smallest gamma mfp value

      iphoton = iphoton + 1                ! Count entry into subroutine

      ircode = 1                              ! Set up for normal return
      eig = e(np)                             ! Energy of current photon
      irl = ir(np)                      ! Region number (local variable)
      medium = med(irl)                       ! Medium of current photon

                                           ! ---------------------------
      if (eig .le. pcut(irl)) go to 1      ! Below cutoff energy/discard
                                           ! ---------------------------

                                              ! ------------------------
 2    continue                                ! Start of NEW-ENERGY loop
                                              ! ------------------------

!     ------------------------------------------------------
!     Sample number of mfp's to transport before interacting
!     ------------------------------------------------------
      gle = log(eig)                            ! Log of incident energy
      call randomset(rnnow)
      if (rnnow .eq. 0.) rnnow = 1.E-30
      dpmfp = -log(rnnow)                              ! Number of mfp's

      if (cexptr .ne. 0.) then        ! Apply exponential transformation
        if (w(np) .gt. 0.) then
          temp = cexptr*w(np)
          bexptr = 1./(1. - temp)
          dpmfp = dpmfp*bexptr               ! Number of mfp's (revised)
          wt(np) = wt(np)*bexptr*exp(-dpmfp*temp)    ! Associated weight
        end if
      end if

      irold = ir(np)                        ! Initialize previous region

                                              ! ------------------------
 3    continue                                ! Start of NEW-MEDIUM loop
                                              ! ------------------------
                                              ! Here each time we change
                                              ! medium during transport
                                              ! ------------------------
      if (medium .ne. 0) then                        ! Set PWLF interval
        lgle = ge1(medium)*gle + ge0(medium)
        iextp=0
        if (eig .lt. 0.15) then
          do iij=1,nedgb(medium)
            if (ledgb(iij,medium) .eq. lgle) then
              if (edgb(iij,medium) .le .eig) then
                iextp = 1
              else
                iextp = -1
              end if
            end if
          end do
        end if
        gmfpr0 = gmfp1(lgle+iextp,medium)*gle +
     *           gmfp0(lgle+iextp,medium)
      end if
!     ------------------------------
!     Start of PHOTON-TRANSPORT loop
!     ------------------------------
 4    continue

      if (medium .eq. 0) then      ! Set for large vacuum-step transport
        tstep = vacdst               ! Distance to next interaction (cm)
      else                                            ! Normal transport
        rhof = rhor(irl)/rhom(medium)                    ! Density ratio
        gmfp = gmfpr0/rhof                                  ! Scaled mfp
        if (iraylr(irl) .eq. 1) then         ! Apply Rayleigh correction
          cohfac = cohe1(lgle+iextp,medium)*gle +
     *             cohe0(lgle+iextp,medium)
          gmfp = gmfp*cohfac                             ! Corrected mfp
        end if
        tstep = gmfp*dpmfp           ! Distance to next interaction (cm)
      end if

!     ------------------------------------------------
!     Set default values for flags sent back from user
!     ------------------------------------------------
      irnew = ir(np)                              ! New (default) region
      idisc = 0                            ! Assume photon not discarded

      ustep = tstep                ! User (straight-line) step requested

!     ---------------------
!     Check user's geometry
!     ---------------------
!                               ===========
      if (ustep .gt. dnear(np)) call howfar
!                               ===========

                                                     ! -----------------
      if (idisc .gt. 0) go to 5                      ! IMMEDIATE discard
                                                     ! -----------------

      edep = 0.           ! Energy deposition (none on photon transport)

      iarg = 0                 ! Photon TO BE TRANSPORTED distance ustep
!                                =================
      if (iausfl(iarg+1) .ne. 0) call ausgab(iarg)
!                                =================
!     --------------------
!     Translate the photon
!     --------------------
      x(np) = x(np) + u(np)*ustep
      y(np) = y(np) + v(np)*ustep
      z(np) = z(np) + w(np)*ustep
      time(np) = time(np) +ustep/2.99792458d10

!     ----------------------------------------
!     Deduct from distance to nearest boundary
!     ----------------------------------------
      dnear(np) = dnear(np) - ustep

!     ----------------------
!     Deduct number of mfp's
!     ----------------------
      if (medium .ne. 0) dpmfp = max(0.D0,dpmfp - ustep/gmfp)

      irold = ir(np)                              ! Save previous region
      medold = medium                             ! Save previous medium
      if (irnew .ne. irold) then                    ! Region has changed
        ir(np) = irnew
        irl = irnew
        medium = med(irl)
      end if

      iarg = 5                  ! Photon WAS TRANSPORTED distance ustep
!                                =================
      if (iausfl(iarg+1) .ne. 0) call ausgab(iarg)
!                                =================
                                           ! ---------------------------
      if (eig .le. pcut(irl)) go to 1      ! Below cutoff energy/discard
                                           ! ---------------------------
                                                      ! ----------------
      if (idisc .lt. 0) go to 5                       ! DEFERRED discard
                                                      ! ----------------
                                       !--------------------------------
      if (medium .ne. medold) go to 3  ! Medium changed during transport
                                       !   (recalculate value for mfp)
                                       !--------------------------------
      if (medium .eq. 0 .or.                           ! ---------------
     *    dpmfp .gt. EPSGMFP) go to 4                  ! Transport again
                                                       !----------------

!     ------------------------------------------------------------------
!     It is finally time to interact  ---  determine type of interaction
!     ------------------------------------------------------------------
!     First check if it is a Rayleigh scatter (provided option is ON)
!     ---------------------------------------------------------------
      if (iraylr(irl) .eq. 1) then
        call randomset(rnnow)
        if (rnnow .le. (1.0 - cohfac)) then

          iarg = 23          ! BEFORE Rayleigh angle has been determined
!                                    =================
          if (iausfl(iarg+1) .ne. 0) call ausgab(iarg)
!                                    =================
!         ===========
          call raylei
!         ===========

          iarg = 24           ! AFTER Rayleigh angle has been determined
!                                    =================
          if (iausfl(iarg+1) .ne. 0) call ausgab(iarg)
!                                    =================
          go to 2  ! Rayleigh finished - go back through NEW-ENERGY loop
        end if
      end if

!     ------------------------------------------------------------------
!     Otherwise determine if PAIR, COMPTON, or PHOTOELECTRIC interaction
!     ------------------------------------------------------------------
!     ---------------------
!     PAIR production check
!     ---------------------
      call randomset(rnnow)
      gbr1 = gbr11(lgle+iextp,medium)*gle + gbr10(lgle+iextp,medium)
      if (rnnow. le .gbr1 .and. e(np) .gt. RMT2) then

        iarg = 15                                     ! BEFORE call pair
!                                  =================
        if (iausfl(iarg+1) .ne. 0) call ausgab(iarg)
!                                  =================
!       =========
        call pair               ! To determine energies and polar angles
!       =========

        iarg = 16                                      ! AFTER call pair
!                                  =================
        if (iausfl(iarg+1) .ne. 0) call ausgab(iarg)
!                                  =================
                                                      ! ----------------
        return                                        ! Return to SHOWER
                                                      ! ----------------
      end if

!     -------------
!     COMPTON check
!     -------------
      gbr2 = gbr21(lgle+iextp,medium)*gle + gbr20(lgle+iextp,medium)
      if (rnnow .lt. gbr2) then

        iarg = 17                                    ! BEFORE call compt
!                                  =================
        if (iausfl(iarg+1) .ne. 0) call ausgab(iarg)
!                                  =================
!       ==========
        call compt
!       ==========
!       ---------------------------------------------------------
!       Interchange stack position of photon and Compton electron
!       (provided electron will be immediately discarded anyway)
!       ---------------------------------------------------------
        if (iq(np) .eq. 0  .and. e(np-1) .lt. ecut(ir(np-1))) then
          iq(np) = iq(np-1)
          iq(np-1) = 0
          t = e(np)
          e(np) = e(np-1)
          e(np-1) = t
          t = u(np)
          u(np) = u(np-1)
          u(np-1) = t
          t = v(np)
          v(np) = v(np-1)
          v(np-1) = t
          t = w(np)
          w(np) = w(np-1)
          w(np-1) = t
          t = uf(np)
          uf(np) = uf(np-1)
          uf(np-1) = t
          t = vf(np)
          vf(np) = vf(np-1)
          vf(np-1) = t
          t = wf(np)
          wf(np) = wf(np-1)
          wf(np-1) = t
        end if

        iarg = 18                                     ! AFTER call compt
!                                  =================
        if (iausfl(iarg+1) .ne. 0) call ausgab(iarg)
!                                  =================
                                                      ! ----------------
        if (iq(np) .ne. 0) return                     ! Return to SHOWER
                                                      ! ----------------
                               ! ---------------------------------------
      else                     ! Must be PHOTOELECTRIC (only thing left)
                               ! ---------------------------------------

        iarg = 19                                    ! BEFORE call photo
!                                  =================
        if (iausfl(iarg+1) .ne. 0) call ausgab(iarg)
!                                  =================
!       ==========
        call photo
!       ==========
        if (np .eq. 0) then
          ircode = 2                 ! ---------------------------------
          return                     ! Stack is EMPTY - return to SHOWER
        end if                       !----------------------------------

        iarg = 20                                     ! AFTER call photo
!                                  =================
        if (iausfl(iarg+1) .ne. 0) call ausgab(iarg)
!                                  =================
                                                      ! ----------------
        if (iq(np) .eq. -1) return                    ! Return to SHOWER
      end if                                          ! ----------------

      eig = e(np)
      if (eig .lt. pcut(irl)) go to 1              ! Below cutoff energy
      go to 2                         ! Go through NEW-ENERGY loop again

                                                 ! ---------------------
 1    continue                                   ! CUTOFF-ENERGY DISCARD
                                                 ! ---------------------
      if (eig .gt. ap(medium)) then
        idr = 1                  ! Photon energy below PCUT (but not AP)
      else
        idr = 2                   ! Photon energy below both AP and PCUT
      end if
      edep = eig
      iarg = idr
!                                =================
      if (iausfl(iarg+1) .ne. 0) call ausgab(iarg)    ! With iarg=1 or 2
!                                =================
      ircode = 2
      np = np - 1              ! Remove particle (move pointer on stack)
                                                      ! ----------------
      return                                          ! Return to SHOWER
                                                      ! ----------------
                                         ! -----------------------------
 5    continue                           ! USER-REQUESTED PHOTON DISCARD
                                         ! -----------------------------
      edep = eig

      iarg = 3                                  ! USER-REQUESTED discard
!                                =================
      if (iausfl(iarg+1) .ne. 0) call ausgab(iarg)
!                                =================
      ircode = 2
      np = np - 1              ! Remove particle (move pointer on stack)
                                                      ! ----------------
      return                                          ! Return to SHOWER
                                                      ! ----------------
      end

!-----------------------last line of egs5_photon.f----------------------
!-----------------------------egs5_raylei.f-----------------------------
! Version: 051219-1435
! Reference: SLAC-R-730/KEK-2005-8
!-----------------------------------------------------------------------
!23456789|123456789|123456789|123456789|123456789|123456789|123456789|12

      subroutine raylei

      implicit none

      include 'include/egs5_h.f'               ! Main EGS5 "header" file

      include 'include/egs5_misc.f'      ! COMMONs required by EGS5 code
      include 'include/egs5_photin.f'
      include 'include/egs5_stack.f'
      include 'include/egs5_thresh.f'
      include 'include/egs5_uphiot.f'
      include 'include/egs5_useful.f'

      include 'include/counters.f'       ! Additional (non-EGS5) COMMONs

      real*8 rnnow                                           ! Arguments

      real*8 x2,q2,csqthe,rejf,br                      ! Local variables
      integer lxxx

      iraylei = iraylei + 1                ! Count entry into subroutine

 1    continue
        call randomset(rnnow)
        lxxx = rco1(medium)*rnnow + rco0(medium)
        x2 = rsct1(lxxx,medium)*rnnow + rsct0(lxxx,medium)
        q2 = x2*RMSQ/(20.60744*20.60744)
        costhe = 1.-q2/(2.*e(np)*e(np))
        if (abs(costhe) .gt. 1.) go to 1
        csqthe = costhe*costhe
        rejf = (1. + csqthe)/2.
        call randomset(rnnow)
        if (rnnow .le. rejf) go to 2
      go to 1
 2    continue
      sinthe = sqrt(1. - csqthe)
      if (lpolar(ir(np)) .eq. 0) then
        call uphi(2,1)
      else
        br = 1.
        call aphi(br)
        call uphi(3,1)
      end if
                                                      ! ----------------
      return                                          ! Return to PHOTON
                                                      ! ----------------
      end

!-----------------------last line of egs5_raylei.f----------------------
!-----------------------------egs5_rk1.f--------------------------------
! Version: 060313-0945
! Read the input data tables of K1 vs E, charD and Z, and construct
! PWL of K1 vs. E for all materials in this problem
!-----------------------------------------------------------------------
!23456789|123456789|123456789|123456789|123456789|123456789|123456789|12

      subroutine rk1

      implicit none

      include 'include/egs5_h.f'         ! COMMONs required by EGSD5 code
      include 'include/egs5_useful.f'
      include 'include/egs5_brempr.f'
      include 'include/egs5_thresh.f'
      include 'include/egs5_elecin.f'
      include 'include/egs5_media.f'
      include 'include/egs5_misc.f'
      include 'include/egs5_scpw.f'
      include 'include/egs5_mscon.f'
      
      real*8                                           ! Local variables
     * ek0k1(50,20),
     * slopek1(50,20), bk1(50,20), 
     * dlowk1(50,20), dhighk1(50,20),
     * k1low(50,20), k1high(50,20),
     * z2k1w(20), rhok1(20)

      integer nmatk1,nek1(20)

      real*8
     * lcharD, eke, elke, delke, z2w, z, watot, 
     * zfrac, frace, k1ez(2),
     * k1z(2), k1new, k1old, elkeold, scpe, scpp, scprat, 
     * c1, c2, extrape, scpeMax(2),
     * k1sold, k1s, k1sz(2), k1sez(2)

      integer nz2, idx, iek1, iedx, niek, neke, iz2
      integer i,im,ie,j,n, m

      open(UNIT=17,FILE='data/K1.dat',STATUS='old')

!  Read the K1 data file, and compute the interpolated values 
!  of K1 corresponding to the characteristic dimension at
!  each energy for each material

      read(17,*) nmatk1
      do i = 1, nmatk1
        read(17,*) z, z2k1w(i), watot, rhok1(i)
        z2k1w(i) = z2k1w(i) / watot
        read(17,*) nek1(i)
        do j = nek1(i), 1, -1
          read(17,*) ek0k1(j,i), slopek1(j,i), bk1(j,i), 
     *              dlowk1(j,i),k1low(j,i),dhighk1(j,i),k1high(j,i)
          ! prep for interp in D * rho
          bk1(j,i) = bk1(j,i) - slopek1(j,i) * DLOG(rhok1(i))
          dhighk1(j,i) = dhighk1(j,i) * rhok1(i)
          dlowk1(j,i) = dlowk1(j,i) * rhok1(i)
        end do
      end do

      close(17)

!  Get Z^2 and then loop over energies and do two-way 
!  interpolation of charD rho in Z^2 and E

      do 10 im = 1, nmed

        if(charD(im).eq.0.d0) go to 10
        lcharD = dlog(charD(im)*rhom(im))

        watot = 0.d0
        z2w = 0.d0
        do ie = 1, nne(im)
          z2w = z2w + pz(im,ie) * zelem(im,ie) * (zelem(im,ie) + 1.d0)
          watot = watot + pz(im,ie) * wa(im,ie)
        end do
        z2w = z2w / watot

        if(z2w.gt.z2k1w(nmatk1)) then
          nz2 = 1
          iz2 = nmatk1
          zfrac = 0.d0
        else if(z2w.lt.z2k1w(1)) then
          nz2 = 1
          iz2 = 1
          zfrac = 0.d0
        else 
          nz2 = 2
          call findi(z2k1w,z2w,nmatk1,iz2)
          zfrac = (z2w - z2k1w(iz2)) / (z2k1w(iz2+1) - z2k1w(iz2))
        end if

        !-->  get scpow at max E of the reference materials,
        !-->  in case we have to extrapolate
        do  n = 1, nz2
          idx = iz2 + n - 1
          eke = ek0k1(nek1(idx),idx)
          if(ue(im)-RM.gt.eke) then
            elke = log(eke)
            j = eke0(im) + elke * eke1(im)
            scpeMax(n) = escpw1(j,im)*elke + escpw0(j,im)
          else
            scpeMax(n) = 0.0
          end if
        end do

        neke = meke(im)
        do j = 1,neke-1
          if(j.eq.1) then
            eke = ae(im) - RM
            elke = log(eke)
          else if(j.eq.neke-1) then
            eke = ue(im) - RM
            elke = log(eke)
          else
            elke = (j + 1 - eke0(im)) / eke1(im)
            eke = DEXP(elke)
          end if

          scpe = escpw1(j,im)*elke + escpw0(j,im)
          scpp = pscpw1(j,im)*elke + pscpw0(j,im)
          scprat = scpp/scpe
          do n = 1, nz2
            extrape = 1.d0
            idx = iz2 + n - 1
            if(eke.gt.ek0k1(nek1(idx),idx)) then
              niek = 1
              iek1 = nek1(idx)
              frace = 0.d0
              extrape = scpe/scpeMax(n)
            else if(eke.lt.ek0k1(1,idx)) then
              niek = 1
              iek1 = 1
              frace = 0.d0
            else 
              niek = 2
              call findi(ek0k1(1,idx),eke,nek1(idx),iek1)
              frace = (eke - ek0k1(iek1,idx)) / 
     *                    (ek0k1(iek1+1,idx) - ek0k1(iek1,idx))
            end if
        
            do m = 1, niek
              iedx = iek1 + m - 1
              if(charD(im)*rhom(im).gt.dhighk1(iedx,idx)) then
                k1ez(m) = k1high(iedx,idx)
              else if(charD(im)*rhom(im).lt.dlowk1(iedx,idx)) then
                k1ez(m) = k1low(iedx,idx)
              else
                k1ez(m) = DEXP(slopek1(iedx,idx)*lcharD + bk1(iedx,idx))
              end if
              k1sez(m) =  k1low(iedx,idx)
            end do
            k1z(n) = k1ez(1) + frace * (k1ez(2) - k1ez(1))
            k1z(n) = extrape * k1z(n)
            k1sz(n) = k1sez(1) + frace * (k1sez(2) - k1sez(1))
            k1sz(n) = extrape * k1sz(n)
          end do
          k1new = k1z(1) + zfrac * (k1z(2) - k1z(1))
          k1s = k1sz(1) + zfrac * (k1sz(2) - k1sz(1))

          if(k1new.gt.k1maxe) then
            k1maxe = k1new
          else if(k1new.lt.k1mine) then
            k1mine = k1new
          end if
          if(k1new*scprat.gt.k1maxp) then
            k1maxp = k1new*scprat
          else if(k1new*scprat.lt.k1minp) then
            k1minp = k1new*scprat
          end if
          if(k1s.lt.k1mine) then
            k1mine = k1s
          end if
          if(k1s*scprat.lt.k1minp) then
            k1minp = k1s*scprat
          end if

          if(j.gt.1) then
            delke = elke - elkeold
            ekini1(j,im) =  (k1new - k1old) / delke
            ekini0(j,im) = (k1old * elke - k1new * elkeold) / delke
            pkini1(j,im) = ekini1(j,im) * scprat
            pkini0(j,im) = ekini0(j,im) * scprat
            ek1s1(j,im) =  (k1s - k1sold) / delke
            ek1s0(j,im) = (k1sold * elke - k1s * elkeold) / delke
            pk1s1(j,im) = ek1s1(j,im) * scprat
            pk1s0(j,im) = ek1s0(j,im) * scprat
          end if
          elkeold = elke
          k1old = k1new
          k1sold = k1s
        end do
            
        ekini1(1,im) = ekini1(2,im)
        ekini0(1,im) = ekini0(2,im)
        pkini1(1,im) = pkini1(2,im)
        pkini0(1,im) = pkini0(2,im)
        ekini1(neke,im) = ekini1(neke-1,im)
        ekini0(neke,im) = ekini0(neke-1,im)
        pkini1(neke,im) = pkini1(neke-1,im)
        pkini0(neke,im) = pkini0(neke-1,im)
        ek1s1(1,im) = ek1s1(2,im)
        ek1s0(1,im) = ek1s0(2,im)
        pk1s1(1,im) = pk1s1(2,im)
        pk1s0(1,im) = pk1s0(2,im)
        ek1s1(neke,im) = ek1s1(neke-1,im)
        ek1s0(neke,im) = ek1s0(neke-1,im)
        pk1s1(neke,im) = pk1s1(neke-1,im)
        pk1s0(neke,im) = pk1s0(neke-1,im)

10    continue

!  compute constants for user requested K1 scaling

      do j = 1, nreg
        if(k1Lscl(j).gt.0.d0 .and. k1Hscl(j).gt.0.d0) then
          im = med(j)
          c2 = (k1Lscl(j) - k1Hscl(j)) /
     *           dlog( (ae(im) - RM)/(ue(im) - RM) )
          c1 = k1Hscl(j) - dlog(ue(im) - RM) * c2
          k1Lscl(j) = c1
          k1Hscl(j) = c2
        else if(k1Lscl(j).ne.0.d0) then
          k1Lscl(j) = 0.d0 
        end if 
      end do


      return
      end

!-----------------------last line of egs5_rk1.f-------------------------
!-----------------------------egs5_rmsfit.f----------------------------
! Version: 060314-0855
! Read Coefficients for fit of GS distribution
!-----------------------------------------------------------------------
!23456789|123456789|123456789|123456789|123456789|123456789|123456789|12

      subroutine rmsfit

      implicit none

      include 'include/egs5_h.f'               ! Main EGS5 "header" file

      include 'include/egs5_media.f'     ! COMMONs required by EGS5 code
      include 'include/egs5_ms.f'
      include 'include/egs5_mscon.f'
      include 'include/egs5_misc.f'
      include 'include/egs5_usersc.f'
      include 'include/egs5_thresh.f'
      
      real*8                                           ! Local variables
     * dummy
      integer i,j,k,ib,il,iprt,ik1,idummy, nfmeds, hasGS
      integer nm, lmdl, lmdn, lok(MXMED)
      character buffer(72),mdlabl(8)
      logical openMS, useGS, doGS, GSFile
      real*8
     * charD0, efrch0, efrcl0, ue0, ae0

      data
     * mdlabl/' ','M','E','D','I','U','M','='/,
     * lmdl/8/,
     * lmdn/24/

! check to see if we have the data we need
      
      openMS = .false.
      useGS = .false.
      do k=1,nmed
        if(charD(k).eq.0.d0) then
          openMS = .true.
        endif
        if(useGSD(k).eq.0) then
          nmsgrd(k) = 1
          msgrid(1,k) = 0.d0
        else
          useGS = .true.
        endif
      end do

      if(.not.(useGS .or. openMS)) then
        return
      endif

      if(useGS) then
        do k=1,nmed
          if(useGSD(k).eq.0) then
            write(6,1001) k
            useGSD(k) = 1
          endif
        end do
      endif

!  Read the header about the materials in gsdist file:
!  If there's no data there, then read what's in the
!  msmatdat file, which was written earlier on this run.

      GSFile = .false.
      doGS=.false.
      open(UNIT=17,FILE='gsdist.dat',STATUS='old',ERR=14)
      GSFile = .true.
      go to 15
      !  all this is for corrupt gsdist.dat files
13    close(17)
14    open(UNIT=17,FILE='pgs5job.msfit',STATUS='old')

!  Make sure we have GS data is we're looking for that.
!  If we have the data, inform the user of what the
!  old constants were -- user inputs will be ignored.
!  If not a GS run, make sure that we have data in 
!  k1init arrays, i.e., the pegs file was compiled
!  without charD also.

15    read(17,*,end=13) nfmeds
      do 18 j = 1, nfmeds
        read(17,'(72a1)',end=1000) buffer
        read(17,*) hasGS, charD0, efrch0, efrcl0, ue0, ae0
        do 16 k=1,nmed
          do ib=1,lmdn
            il = lmdl + ib
            if (buffer(il) .ne. media(ib,k)) go to 16
            if (ib .eq. lmdn) go to 17
          end do
16      continue
        go to 18
        !  here if we are using this material
17      if(useGS .and. hasGS.eq.0) then
          write(6,1002) k
          doGS = .true.
        endif

        if(charD(k).eq.0.d0) then
          if(charD0.ne.0.d0) then
            if(useGS) then
              write(6,1003) k, charD0
            else
              write(6,1004) k
              stop
            endif
          else
            write(6,1005) k, efrch0, efrcl0
          endif
        endif

        !  make sure previous data was for less than or equal ue/ae
        if(useGS .and. (ue0.lt.ue(j) .or. ae0.gt.ae(j))) then
          write(6,1006) k, ae0, ue0, ae(j), ue(j)
          doGS = .true.
        endif

18    continue

1001  format(' WARNING in RMSFIT:  forcing use of GS Dist in media ',i3)
1002  format(' WARNING in RMSFIT: requested GS Multiple Scattering in ',
     * 'media ',i3,/,' but no data in gsdist.dat file.  Calling ',
     * 'elastino')
1003  format(' WARNING in RMSFIT:  using GS Multiple Scattering in ',
     * 'media ',i3,/,' but no characteristic dimension specified. ',
     * /,' Using old data from gsdist.dat with charD = ',1pe13.4)
1004  format(' ERROR in RMSFIT:  no characteristic dimension given for',
     * ' media ',i3,/,' and no data in pgs5job.pegs5dat file.  Please '
     * ,'re-run with call',/,' to PEGS or with charD.  Aborting.')
1005  format(' WARNING in RMSFIT: no characteristic dimension input for'
     * ,' media ',i3,/,' Using old data from gsdist.dat with: ',/,
     * ' efrach, efrachl = ',2(2x,1pe13.4))
1006  format(' WARNING in RMSFIT:  requested GS Multiple Scattering in',
     * ' media ',i3,/,' but data in gsdist.dat file does not ',
     * 'match data in pgs5job.pegs5dat:',/,
     * ' gsdist.dat:     AE, UE = ', 2(2x,1pe13.4),/,
     * ' pegs5dat:  AE, UE = ', 2(2x,1pe13.4),/,' Calling elastino.')
1007  format(' ERROR in RMSFIT:  requested GS Multiple Scattering in ',
     * 'media ',i3,/,' Can not use GS in conjunction with density ',
     * 'scaling requested ',/,' for region ',i8,' Aborting.')
1008  format(' WARNING in RMSFIT:  using GS Multiple Scattering in ',
     * 'media ',i3,/,' User requested K1 scaling will be ignored.')
1009  format(' WARNING in RMSFIT: requested GS Multiple Scattering',/,
     * ' but wrong charD data in gsdist.dat file.  Calling elastino')

!  return if not using GS distribution

      if(.not.useGS) then
        close(17)
        return
      endif

!  do more traps for bad user input data for GS dist case:

      do j = 1, nreg
        k = med(j)
        if(k.ne.0) then
          if( rhom(k).ne.rhor(j)) then
            write(6,1007) j,k
            stop
          end if
        end if
      end do

      do j = 1, nreg
        if( (k1Hscl(j) + k1Lscl(j)) .gt. 0.d0) then
          k1Hscl(j) = 0.d0
          k1Lscl(j) = 0.d0
          write(6,1008) j
        endif
      end do

      ! instead of checking charD, we can check k1max and k1min
      if(GSFile .and. .not.doGS) then
        read(17,'(72a1)',end=1000) buffer
        read(17,'(72a1)',end=1000) buffer
        do i = 1, NK1
          read(17,*) idummy, k1grd(1,i), k1grd(2,i)
        end do
        if(abs(k1mine-k1grd(1,1))/k1mine .gt. 1.d-6 .or. 
     *       abs(k1minp-k1grd(2,1))/k1minp .gt. 1.d-6 .or.
     *         abs(k1maxe-k1grd(1,NK1))/k1maxe .gt. 1.d-6 .or.
     *           abs(k1maxp-k1grd(2,NK1))/k1maxp .gt. 1.d-6) then
          write(6,1009)
          doGS = .true.
        else
          go to 30
        end if
      end if

      ! if we need to run elastino, we'll be writing the gsdist.dat
      ! file, so close whichever file we're reading on 17, open
      ! gsdist.dat, and run elastino. Then close the file and open it
      ! again for reading.  Finally, skip to the right spot
      if(doGS) then

        close(17)
        open(UNIT=17,FILE='gsdist.dat',STATUS='unknown')
        call elastino
        close(17)
        open(UNIT=17,FILE='gsdist.dat',STATUS='old')
        do j = 1, nfmeds*2 + 1
          read(17,'(72a1)',end=1000) buffer
        end do
      end if

!  read the k1 ladder for this problem

      read(17,'(72a1)',end=1000) buffer
      read(17,'(72a1)',end=1000) buffer
      do i = 1, NK1
        read(17,*) idummy, k1grd(1,i), k1grd(2,i)
      end do

 30   dk1log(1) = dlog(k1grd(1,2)/k1grd(1,1))
      dk1log(2) = dlog(k1grd(2,2)/k1grd(2,1))

!  look for the correct material in the file for each media,
!  just as HATCH does.  Read through the entire data file
!  looking for MEDIUM= strings.  When you find one, see if
!  that's a medium that's being used.  If so, match it up
!  with the list of media for the problem, and read in the
!  medium data.  If not, keep searching for the next MEDIUM=.
!
      nm = 0
      do k=1,nmed
        lok(k) = 0
      end do

5     continue
      read(17,'(72a1)',end=1000) buffer
      !--> look for the MEDIUM= string
      do ib=1,lmdl
        if (buffer(ib) .ne. mdlabl(ib)) go to 5
      end do
      !--> Match medium with the problem media
      do 6 k=1,nmed
        do ib=1,lmdn
          il = lmdl + ib
          if (buffer(il) .ne. media(ib,k)) go to 6
          if (ib .eq. lmdn) go to 7
        end do
 6    continue
      go to 5

 7    continue
      if (lok(k) .ne. 0) go to 5
      lok(k) = 1
      nm = nm + 1

      read(17,'(72a1)') buffer
      read(17,'(72a1)') buffer
      read(17,*) nmsgrd(k)
      read(17,'(72a1)') buffer
      read(17,*) initde(k)
      read(17,'(72a1)') buffer
      read(17,*) nmsdec(k)
      read(17,'(72a1)') buffer
      read(17,*) jskip(k)
      read(17,'(72a1)') buffer
      read(17,*) neqp(k)
      read(17,'(72a1)') buffer
      read(17,*) neqa(k)
!  decrement jskip to save in "bin = N * xi - jskip + 1" when we sample
      jskip(k) = jskip(k) - 1
      do iprt = 1,2
        read(17,'(72a1)') buffer
!  read in the electrons...
        do i = 1,nmsgrd(k)
          read(17,*) msgrid(i,k)
          do ik1 = 1,NK1
            read(17,'(72a1)') buffer
            read(17,*) pnoscat(iprt,i,ik1,k)
            read(17,'(72a1)') buffer
            eamu(1,iprt,i,ik1,k) = 0.0
!  note:  the '-1' is because the last angle in the first part
!  of the distribution is the same as the first angle in the second
            do j = 1, neqp(k) + neqa(k) - 1
              read(17,*) dummy, eamu(j+1,iprt,i,ik1,k), 
     &                    ebms(j,iprt,i,ik1,k), 
     &                    eetams(j,iprt,i,ik1,k)
              !--> do this now to save later
!              ebms(j,iprt,i,ik1,k) = ebms(j,iprt,i,ik1,k) * .25d0
            end do
!  read the cum pdf for the equally spaced angles
            read(17,'(72a1)') buffer
            do j = 1, neqa(k) + 1
              read(17,*) ecdf(j,iprt,i,ik1,k)
            end do
!  round-off fix
            ecdf(neqa(k)+1,iprt,i,ik1,k) = 1.d0
          end do
        end do
      end do

!  go back for more media or quit
      if (nm .ge. nmed) go to 9
      go to 5

 9    continue
      if (nmed .eq. 1) then
        write(6,5001)
      else
        write(6,5002) nmed
      end if
      close(17)
      return

!  media not found error:

1000  write(6,5003)
      write(6,5004)
      do k=1,nmed
        if (lok(k) .ne. 1) write(6,'(24a1)') (media(i,k),i=1,lmdn)
      end do
      close(17)
      stop

5001  format('Read GS Mult scat params for 1 medium')
5002  format('Read GS Mult scat params for ',i2,' media')
5003  format('End-of-file on gsdist.dat')
5004  format('The following media were not located:')

      end

!-----------------------last line of egs5_rmsfit.f----------------------
!-----------------------------egs5_shower.f-----------------------------
! Version: 080425-1100
! Reference: SLAC-R-730/KEK-2005-8
!-----------------------------------------------------------------------
!23456789|123456789|123456789|123456789|123456789|123456789|123456789|12

      subroutine shower(iqi,ei,xi,yi,zi,ui,vi,wi,iri,wti)

      implicit none

      include 'include/egs5_h.f'               ! Main EGS5 "header" file

      include 'include/egs5_stack.f'     ! COMMONs required by EGS5 code
      include 'include/egs5_uphiot.f'

      include 'include/counters.f'       ! Additional (non-EGS5) COMMONs

      real*8 ei,xi,yi,zi,ui,vi,wi,wti,rnnow                  ! Arguments
      integer iqi,iri,ircode

      real*8                                           ! Local variables
     * dneari,deg,dpgl,dei,dpi,dcsth,dcosth

      real*8 PI0MSQ
      data PI0MSQ/1.8215416E4/      ! Pi-zero rest mass squared (MeV**2)

      ishower = ishower + 1                ! Count entry into subroutine

      np = 1
      dneari = 0.0
      iq(1) = iqi
      e(1) = ei
      u(1) = ui
      v(1) = vi
      w(1) = wi
      x(1) = xi
      y(1) = yi
      z(1) = zi
      ir(1) = iri
      wt(1) = wti
      dnear(1) = dneari
      latch(1) = latchi
      deresid = 0.d0
      deinitial = 0.d0
      denstep = 0.d0
      k1step(1) = 0.d0
      k1init(1) = 0.d0
      k1rsd(1) = 0.d0
      time(1) = 0.d0

      if (iqi .eq. 2) then
        if (ei**2 .le. PI0MSQ) then
          write(6,10) ei
10        format(//,' Stopped in Subroutine Shower---pi-zero option invo
     *ked', /,' but the total energy was too small (ei=',g15.5,' MeV)')
          stop
        end if
        call randomset(rnnow)
        dcsth = rnnow
        dei = ei
        dpi = dsqrt(dei*dei - PI0MSQ)
        deg = dei + dpi*dcsth
        dpgl = dpi + dei*dcsth
        dcosth = dpgl/deg
        costhe = dcosth
        sinthe = dsqrt(1.d0 - dcosth*dcosth)
        iq(1) = 0
        e(1) = deg/2.
        call uphi(2,1)
        np = 2
        deg = dei - dpi*dcsth
        dpgl = dpi - dei*dcsth
        dcosth = dpgl/deg
        costhe = dcosth
        sinthe = -dsqrt(1.d0 - dcosth*dcosth)
        iq(2) = 0
        e(2) = deg/2.
        call uphi(3,2)
      end if

      !  For first-step scaling for electrons
      ircode = -1

1     continue
        if (iq(np) .eq. 0) go to 3
2       continue
          call electr(ircode)  
          if (ircode .eq. 2) go to 4
3         call photon(ircode)
          if (ircode .eq. 2) go to 4
        go to 2
4       continue
        if (np .gt. 0) go to 1

      return
      end

!-----------------------last line of egs5_shower.f----------------------
!------------------------------egs5_uphi.f------------------------------
! Version: 070720-1515
!          080425-1100
! Reference: SLAC-R-730/KEK-2005-8
!-----------------------------------------------------------------------
!23456789|123456789|123456789|123456789|123456789|123456789|123456789|12

      subroutine uphi(ientry,lvl)                                               

      implicit none

      include 'include/egs5_h.f'               ! Main EGS5 "header" file
      
      include 'include/egs5_epcont.f'    ! COMMONs required by EGS5 code
      include 'include/egs5_stack.f'
      include 'include/egs5_uphiin.f' ! Probably don't need this anymore
      include 'include/egs5_uphiot.f'

      include 'include/counters.f'       ! Additional (non-EGS5) COMMONs

      integer ientry,lvl,iarg                                ! Arguments

      real*8                                           ! Local variables
     * usav,vsav,wsav,sinpsi,sinps2,cosdel,sindel,
     * us,vs,cthet,cphi,phi,rnnow

      iuphi = iuphi + 1                    ! Count entry into subroutine

      iarg=21
      if (iausfl(iarg+1) .ne. 0) then
        call ausgab(iarg)
      end if

      go to (1,2,3),ientry
      go to 10

1     continue
      sinthe = sin(theta)
      cthet = PI5D2 - theta
      costhe = sin(cthet)

2     call randomset(rnnow)
      phi = rnnow*TWOPI
      sinphi = sin(phi)
      cphi = PI5D2 - phi
      cosphi = sin(cphi)

3     go to (4,5,6),lvl
      go to 10

4     usav = u(np)
      vsav = v(np)
      wsav = w(np)
      go to 7

6     usav = u(np-1)
      vsav = v(np-1)
      wsav = w(np-1)

5     x(np) = x(np-1)
      y(np) = y(np-1)
      z(np) = z(np-1)
      ir(np) = ir(np-1)
      wt(np) = wt(np-1)
      dnear(np) = dnear(np-1)
      latch(np) = latch(np-1)
      time(np) = time(np-1)
      k1step(np) = 0.d0
      k1init(np) = 0.d0
      k1rsd(np) = 0.d0

7     sinps2 = usav*usav + vsav*vsav
      if (sinps2 .lt. 1.e-20) then
        u(np) = sinthe*cosphi
        v(np) = sinthe*sinphi
        w(np) = wsav*costhe
      else
        sinpsi = sqrt(sinps2)
        us = sinthe*cosphi
        vs = sinthe*sinphi
        sindel = vsav/sinpsi
        cosdel = usav/sinpsi
        u(np) = wsav*cosdel*us - sindel*vs + usav*costhe
        v(np) = wsav*sindel*us + cosdel*vs + vsav*costhe
        w(np) = -sinpsi*us + wsav*costhe
      end if

      iarg=22
      if (iausfl(iarg+1) .ne. 0) then
        call ausgab(iarg)
      end if

      return

10    write(6,100) ientry,lvl
100   format(' Stopped in uphi with ientry,lvl=',2I6)

      stop
      end

!-----------------------last line of egs5_uphi.f------------------------
!------------------------------randomset.f------------------------------
! Version: 051219-1435
! Reference: RANLUX, after James,
!            Computer Phys. Commun. 79 (1994) 111-114.
!            Subtract-and-borrow random number generator proposed by
!            Marsaglia and Zaman, implemented by F. James with the name
!            RCARRY in 1991, and later improved by Martin Luescher
!            in 1993 to produce "Luxury Pseudorandom Numbers".
!            Fortran 77 coded by F. James, 1993
!
!  This is a double-precision RNG devloped by Namito Yoshihito and
!  Takashi Maruyama. It replaced the single-precision RNG that egs5 used
!  by default. If the lower-precision version is needed again for any
!  reason, please contact Bradley Yale <btu29@wildcats.unh.edu>.
!
!-----------------------------------------------------------------------
!23456789|123456789|123456789|123456789|123456789|123456789|123456789|12

      subroutine randomset(rndum)
      
      implicit none

      include 'include/randomm.f'

!  global variables

      real*8 rndum

!  local variables

      integer isk
      real uni

      uni = seeds(j24) - seeds(i24) - carry 
      if (uni .lt. 0.)  then
        uni = uni + 1.0
        carry = twom24
      else
        carry = 0.
      endif
      seeds(i24) = uni
      i24 = next(i24)
      j24 = next(j24)
      rndum = uni
!  small numbers (with less than 12 "significant" bits) are "padded".
!  All numbers are "padded by comment out next line" 6Aug2015/YN/HH
!      if (uni .lt. twom12)  then
        rndum = rndum + twom24*seeds(j24)
!  and zero is forbidden in case someone takes a logarithm
        if (rndum .eq. 0.)  rndum = twom48
!      endif

!     Skipping to luxury.  As proposed by Martin Luscher.
      in24 = in24 + 1
      if (in24 .eq. 24)  then
        in24 = 0
        kount = kount + nskip
        do isk = 1, nskip
          uni = seeds(j24) - seeds(i24) - carry
          if (uni .lt. 0.)  then
            uni = uni + 1.0
            carry = twom24
          else
            carry = 0.
          endif
          seeds(i24) = uni
          i24 = next(i24)
          j24 = next(j24)
        end do
      endif

      ! store for restart from number of rngs used
      kount = kount + 1
      if(kount .ge. igiga) then
        mkount = mkount + 1
        kount = kount - igiga
      endif

      return
      end

!-------------------------last line of randomset.f----------------------
!-----------------------------rluxinit.f--------------------------------
! Version: 051219-1435
! Reference: RLUXIN, RLUXGO, and RLUXUT of RANLUX, after James,
!            Computer Phys. Commun. 79 (1994) 111-114. 
!-----------------------------------------------------------------------
!23456789|123456789|123456789|123456789|123456789|123456789|123456789|12

      subroutine rluxinit
      
      implicit none

      !  inputs, from common block RLUXCOM in randomm.f

      !  integer luxlev  !  desired luxury level, default is 1,
                         !  levels 2 and higher should never be
                         !  needed

      !  integer inseed  !  seed for generating unique sequences,
                         !  in theory, any number between 1 and 2^31
                         !  "every possible value of INS gives rise 
                         !  to a valid, independent sequence which will 
                         !  not overlap any sequence initialized with 
                         !  any other value of INS.". 
 
      !  integer kount   !  number of randoms already used, for
      !  integer mkount  !  restart with rluxgo

      !  integer isdext  !  array of 25 ints for restart with rluxin

      include 'include/randomm.f'

!   LUXURY LEVELS.
!   ------ ------      The available luxury levels are:
!
!  level 0  (p=24): equivalent to the original RCARRY of Marsaglia
!           and Zaman, very long period, but fails many tests.
!  level 1  (p=48): considerable improvement in quality over level 0,
!           now passes the gap test, but still fails spectral test.
!  level 2  (p=97): passes all known tests, but theoretically still
!           defective.
!  level 3  (p=223): Any theoretically possible correlations have
!           very small chance of being observed.
!  level 4  (p=389): highest possible luxury, all 24 bits chaotic.

!  Luxury Level   0    *1*    2    3     4
!  Time factor    1     2     3    6    10

      integer i, tisdext

      !  see if isdext has been set
     
      tisdext = 0
      do i = 1,25
        tisdext = tisdext + isdext(i)
      end do

      if(tisdext.ne.0) then
        call rluxin                    ! restart with 25 seeds
      else
        call rluxgo
      endif
   
      return
      end

!--------------------last line of rluxinit.f----------------------------
!-------------------------------rluxgo.f--------------------------------
! Version: 051219-1435
!-----------------------------------------------------------------------
!23456789|123456789|123456789|123456789|123456789|123456789|123456789|12

      subroutine rluxgo
      
      implicit none

      include 'include/randomm.f'

      integer jsdflt, itwo24, icons
      parameter (jsdflt=314159265)
      parameter (itwo24=2**24, icons=2147483563)

      real uni
      integer jseed, i,k, iseeds(24), izip, izip2, isk, inner, iouter

!  check luxury level:
      if (luxlev .le. 0 .or. luxlev .gt. maxlev) then
        write (6,'(a,i7)') ' illegal ranlux level in rluxgo: ',luxlev
        stop
      endif

      nskip = ndskip(luxlev)
      write(6,'(a,i2,a,i4)') ' ranlux luxury level set by rluxgo :',
     +        luxlev,'     p=', nskip+24

!  set seeds - any positive seed is valid:
      in24 = 0
      if (inseed .lt. 0) then
        write (6,'(a)') 
     +   ' Illegal initialization in rluxgo, negative input seed'
        stop
      else if (inseed .gt. 0) then
        jseed = inseed
        write(6,'(a,i12)')
     +   ' ranlux initialized by rluxgo from seed', jseed
      else
        jseed = jsdflt
        write(6,'(a)')' ranlux initialized by rluxgo from default seed'
      endif
      inseed = jseed
      twom24 = 1.
      do i= 1, 24
        twom24 = twom24 * 0.5
        k = jseed/53668
        jseed = 40014*(jseed-k*53668) -k*12211
        if (jseed .lt. 0)  jseed = jseed+icons
        iseeds(i) = mod(jseed,itwo24)
      end do

      twom12 = twom24 * 4096.

      do i= 1,24
        seeds(i) = real(iseeds(i))*twom24
        next(i) = i-1
      end do
      next(1) = 24

      i24 = 24
      j24 = 10
      carry = 0.
      if (seeds(24) .eq. 0.) carry = twom24
      
!     If restarting at a break point, skip kount + igiga*mkount
!     Note that this is the number of numbers delivered to
!     the user PLUS the number skipped (if luxury .gt. 0).

      if (kount+mkount .ne. 0)  then
        write(6,'(a,i20,a,i20)')
     +' Restarting ranlux with kount = ',kount,' and mkount = ',mkount
        do iouter= 1, mkount+1
          inner = igiga
          if (iouter .eq. mkount+1)  inner = kount
          do isk= 1, inner
            uni = seeds(j24) - seeds(i24) - carry
            if (uni .lt. 0.)  then
               uni = uni + 1.0
               carry = twom24
            else
               carry = 0.
            endif
            seeds(i24) = uni
            i24 = next(i24)
            j24 = next(j24)
          end do
        end do
!       Get the right value of in24 by direct calculation
        in24 = mod(kount, nskip+24)
        if (mkount .gt. 0)  then
           izip = mod(igiga, nskip+24)
           izip2 = mkount*izip + in24
           in24 = mod(izip2, nskip+24)
        endif
!       Now in24 had better be between zero and 23 inclusive
        if (in24 .gt. 23) then
           write (6,'(a/a,3i11,a,i5)')
     +    '  Error in RESTARTING with RLUXGO:','  The values', inseed,
     +     kount, mkount, ' cannot occur at luxury level', luxlev
           stop
        endif
      endif

      rluxset = .true.

      return
      end

!--------------------last line of rluxgo.f------------------------------
!------------------------------rluxin.f---------------------------------
! Version: 051219-1435
!-----------------------------------------------------------------------
!23456789|123456789|123456789|123456789|123456789|123456789|123456789|12

      subroutine rluxin
      
      implicit none

      include 'include/randomm.f'

      integer i, isd

      write(6,'(a)') ' full initialization of ranlux with 25 integers:'
      write(6,'(5x,5i12)') isdext

      twom24 = 1.

      do i = 1, 24
        next(i) = i-1
        twom24 = twom24 * 0.5
      end do
      next(1) = 24
      twom12 = twom24 * 4096.

      do i = 1, 24
        seeds(i) = real(isdext(i))*twom24
      end do
      carry = 0.
      if (isdext(25) .lt. 0)  carry = twom24
      isd = iabs(isdext(25))
      i24 = mod(isd,100)
      isd = isd/100
      j24 = mod(isd,100)
      isd = isd/100
      in24 = mod(isd,100)
      isd = isd/100
      luxlev = isd
      if (luxlev .le. maxlev) then
        nskip = ndskip(luxlev)
        write (6,'(a,i2)') 
     +       ' ranlux luxury level set by rluxin to: ', luxlev
      else
        write (6,'(a,i5)') ' ranlux illegal luxury rluxin: ',luxlev
        stop
      endif

      inseed = -1

      rluxset = .true.

      return
      end

!------------------last line of rluxin.f--------------------------------
!------------------------------rluxout.f--------------------------------
! Version: 051219-1435
!-----------------------------------------------------------------------
!23456789|123456789|123456789|123456789|123456789|123456789|123456789|12

      subroutine rluxout
      
      implicit none

      include 'include/randomm.f'

      real twop12
      parameter (twop12=4096.)

      integer i

      do i = 1,24
        isdext(i) = int(seeds(i)*twop12*twop12)
      end do

      isdext(25) = i24 + 100*j24 + 10000*in24 + 1000000*luxlev
      if (carry .gt. 0.)  isdext(25) = -isdext(25)

      return
      end

!------------------last line of rluxout.f-------------------------------
!-------------------------------cg_related.f--------------------------------
! Version: 090114-0925
! Provided by T. Torii and T. Sugita
! Reference: T. Torii and T. Sugita, "Development of PRESTA-CG 
! Incorporating Combinatorial Geometry in EGS4/PRESTA", JNC TN1410 2002-201,
! Japan Nuclear Cycle Development Institute (2002).
!
!-----------------------------------------------------------------------
!23456789|123456789|123456789|123456789|123456789|123456789|123456789|12

! ----------------------------------------------------------------------
! Required (geometry) subroutine for use with the EGS5 Code System
! ----------------------------------------------------------------------
! This is a cg-related subroutine. 
! ----------------------------------------------------------------------

!-------------------------------qadrti.f--------------------------------
! Version: 051219-1435
! Reference: JNC TN1410 2002-201
!-----------------------------------------------------------------------
!23456789|123456789|123456789|123456789|123456789|123456789|123456789|12

! ----------------------------------------------------------------------
! Required (geometry) subroutine for use with the EGS5 Code System
! ----------------------------------------------------------------------
! This is a subroutine qadrti. 
! ----------------------------------------------------------------------
      subroutine qadrti(a,x1,x2,i)
      implicit none
      double precision a(3),x1,x2
      integer i
      double precision p,q,d,rd
c
c'    a(1)*x*x+a(2)*x+a(3)=0. ==>> (x-x1)(x-x2)=0.
c
      if(a(1).ne.0.) then
        p=a(2)/a(1)
        q=a(3)/a(1)
        p=0.5d0*p
        d=p*p-q
        if(d.lt.0.0d0) then
          rd=sqrt(-d)
          x1=-p
          x2=rd
          i=0
        elseif(d.eq.0.) then
          x1=-p
          x2=-p
          i=2
        else
          i=2
          rd=sqrt(d)
          if(p.lt.0.) then
            x1=-p+rd
            x2=q/x1
          elseif(p.eq.0.) then
            x1=rd
            x2=-rd
          else
            x1=-p-rd
            x2=q/x1
          endif
        endif
      else
        if(a(2).ne.0.) then
          x1=-a(3)/a(2)
          x2=-a(3)/a(2)
          i=2
        else
          write(6,'(1h ,a)')
     &    '<< qadrti >> parameter error : a(1) = 0. & a(2) =0.)'
          i=0
          x1=0.0d0
          x2=0.0d0
        endif
      endif
      return
      end
!-----------------last line of subroutine qadrti-----------------------
                                                                        
!-------------------------------carda.f--------------------------------
! Version: 051219-1435
! Reference: JNC TN1410 2002-201
!-----------------------------------------------------------------------
!23456789|123456789|123456789|123456789|123456789|123456789|123456789|12

! ----------------------------------------------------------------------
! Required (geometry) subroutine for use with the EGS5 Code System
! ----------------------------------------------------------------------
! This is a subroutine carda. 
! ----------------------------------------------------------------------
      subroutine carda(aa,x1,x2,x3,i)
      implicit none
      double precision aa(4),a(4),b(4),x1,x2,x3
      double precision eps1,eps2,eps3,e,con,d,p,q,r,s,t,r1,r1c,r2c,rd,
     &                 u,v,xt1,xt2,xt3,dd,r2
      double precision theata,sr,ct,st,xmax
      integer i,ind1,ind2
c
c'    aa(1)*x*x*x+aa(2)*x*x+aa(3)*x+aa(4)=0. ==>> (x-x1)(x-x2)(x-x3)=0.
c
      ind1=0
c      eps1=1.0d-75
c      eps2=1.0d-25
      eps1=1.0d-12
      eps2=1.0d-12
      eps3=1.0d-8
      do 100 i=1,4
        a(i)=aa(i)
        b(i)=aa(i)
  100 continue
c                                                                               
      e=1.0d0/3.0d0
      con=sqrt(3.0d0)/2.0d0
      a(2)=a(2)/a(1)
      a(3)=a(3)/a(1)
      a(4)=a(4)/a(1)
c
  200 continue
        r=a(2)/3.0d0
        s=a(3)/3.0d0
        p=-(r*r)+s
        q=2.0d0*r*r*r-a(2)*s+a(4)
        if((abs(q).le.eps1).and.(abs(p).le.eps2)) go to 300
          t=p*p*p
          d=q*q+4.0d0*t
          if(abs(d).lt.eps3*(abs(q*q)+abs(4.0d0*t))) d=0.0d0
          if (d.eq.0.0d0) then
            r1=-q*0.5d0
            r1c=(abs(r1))**e
            if(r1.lt.0.) then
              r1c=-r1c
            endif
            x1=2.0d0*r1c-r
            if(abs(x1).lt.eps3*(abs(2.0d0*r1c)+abs(r))) x1=0.0d0
            x2=-r1c-r
            x3=-r1c-r
            if(abs(x2).lt.eps3*(abs(r1c)+abs(r))) then
              x2=0.0d0
              x3=0.0d0
            endif
            i=3
            return
          elseif (d.gt.0.0d0) then
            rd=sqrt(d)
            if (q.gt.0.0d0) then
              r1=(-q-rd)*0.5d0
            else
              r1=(-q+rd)*0.5d0
            endif
            r2=-t/r1
            r1c=(abs(r1))**e
            if(r1.lt.0.0d0) then
              r1c=-r1c
            endif
            r2c=(abs(r2))**e
            if(r2.lt.0.0d0) then
              r2c=-r2c
            endif
            u=r1c+r2c
            v=r1c-r2c
            xt1=u-r
            xt2=-0.5d0*u-r
            xt3=con*v
            if(ind1.eq.1) then
              if(ind2.eq.1) then
                x1=1.0d0/xt1
              else
                dd=xt2*xt2+xt3*xt3
                x2=xt2/dd
                x3=xt3/dd
              endif
              i=1
              return
            else
              if((xt1*xt1).ge.(xt2*xt2+xt3*xt3)) then
                x1=xt1
                ind2=0
              else
                x2=xt2
                x3=xt3
                ind2=1
              endif
            endif
          else
            rd=sqrt(-d)
            theata=(atan2(rd,(-q)))/3.0d0
            sr=2.0d0*sqrt(-p)
            ct=cos(theata)
            st=sin(theata)
            xt1=sr*ct-r
            r1c=-0.5d0*ct*sr
            r2c=con*st*sr
            xt2=r1c-r2c-r
            xt3=r1c+r2c-r
            if(abs(xt1).ge.max(abs(xt2),abs(xt3))) then
              xmax=xt1
            elseif(abs(xt2).ge.abs(xt3)) then
              xmax=xt2
            else
              xmax=xt3
            endif
            if(ind1.eq.1) then
              x2=1.0d0/xmax
              x3=-(b(4)/b(1))/(x1*x2)
              i=3
              return
            else
              x1=xmax
            endif
          endif
          ind1=1
          a(2)=b(3)/b(4)
          a(3)=b(2)/b(4)
          a(4)=b(1)/b(4)
          goto 200
  300 continue
      x1=-r
      x2=-r
      x3=-r
      i=3
      return
      end
C                                                                       
!-----------------------last line of subroutine carda------------------
                                                                        
!-------------------------------ferra.f--------------------------------
! Version: 051219-1435
! Reference: JNC TN1410 2002-201
!-----------------------------------------------------------------------
!23456789|123456789|123456789|123456789|123456789|123456789|123456789|12

! ----------------------------------------------------------------------
! Required (geometry) subroutine for use with the EGS5 Code System
! ----------------------------------------------------------------------
! This is a subroutine ferra. 
! ----------------------------------------------------------------------
      subroutine ferra(a,x1,x2,x3,x4,i)
      implicit none
      double precision a(5),ca(4),aa(3),x1,x2,x3,x4
      double precision eps,b1,b2,b3,b4,bb,p,q,r,s,t,dd,u,
     &                 x1r,x1i,theata,rou,e,x2r,x2i,as,d
      double precision y1,y2,y3,z1,z2
      integer i,j,k,l
c
c'    a(1)*x*x*x*x+a(2)*x*x*x+a(3)*x*x+a(4)*x+a(5)=0.
c'     ==>> (x-x1)(x-x2)(x-x3)(x-x4)=0.
c
c      eps=1.0d-75
      eps=1.0d-12
      b1=a(2)/a(1)
      b2=a(3)/a(1)
      b3=a(4)/a(1)
      b4=a(5)/a(1)
      bb=b1/4.0d0
      p=-(3.0d0*b1*b1)/8.d0+b2
      q=-b1*p/2.d0-(b1*b1*b1)/16.0d0+b3
      r=-b1*q/8.0d0+(b1*b1*b1*b1)/256.d0-b1*b3/8.0d0+b4
      if(abs(q).le.eps) goto 200
      if(q*q/(abs(4.d0*r*p)+q*q).le.eps) goto 200
      dd=4.d0*r*p-q*q
      if(abs(dd).le.eps) goto 210
      ca(1)=1.0d0
      ca(2)=-p
      ca(3)=-4.0d0*r
      ca(4)=dd
      call carda(ca,y1,y2,y3,l)
      if(l.ne.1) y1=max(y1,y2,y3)
  220 continue
      u=y1/2.0d0
      aa(1)=1.0d0
      if(y1-p.le.0.0) then
c        write(*,*) 'y1-p,y1,p=',y1-p,y1,p
        s=0.
        t=0.
        if(u.le.0) then
          j=2
          x1=sqrt(-u)
          x2=-sqrt(-u)
        else
          j=0
          x1r=0.0d0
          x1i=sqrt(u)
        endif
      else
        s=sqrt(y1-p)
        t=q/(2.0d0*s)
        aa(2)=s
        aa(3)=-t+u
        call qadrti(aa,z1,z2,j)
        if(j.eq.2) then
          x1=z1
          x2=z2
        else
          x1r=z1
          x1i=z2
        endif
      endif
      aa(2)=-s
      aa(3)=t+u
  290 continue
      call qadrti(aa,z1,z2,k)
      if(k.eq.2) then
        x3=z1
        x4=z2
      else
        x2r=z1
        x2i=z2
      endif
      i=j+k
  260 continue
      if(i.eq.0) then
        x1=x1r-bb
        x2=x1i
        x3=x2r-bb
        x4=x2i
      elseif(i.eq.2) then
        if(j.eq.2) then
          x1=x1-bb
          x2=x2-bb
          x3=x2r-bb
          x4=x2i
        else
          x1=x3-bb
          x2=x4-bb
          x3=x1r-bb
          x4=x1i
        endif
      else
        x1=x1-bb
        x2=x2-bb
        x3=x3-bb
        x4=x4-bb
      endif
      return
  210 continue
      y1=0.0d0
      aa(1)=1.d0
      aa(2)=-p
      aa(3)=-4.0d0*r
      call qadrti(aa,y2,y3,l)
      if(l.eq.2) y1=max(y1,y2,y3)
      goto 220
  200 continue
      if(abs(r).le.eps) goto 230
      if(abs(4.d0*r)/(p*p+abs(4.d0*r)).le.eps) goto 230
      d=p*p-4.d0*r
      if(d.lt.0.) then
        e=1.d0/4.d0
        rou=r**e
        as=sqrt(-d)
        theata=(atan2(as,(-p)))*0.5d0
        x1r=rou*cos(theata)
        x1i=rou*sin(theata)
        x2r=-x1r
        x2i=x1i
        i=0 
        goto 260
      else
        aa(1)=1.d0
        aa(2)=0.0d0
        aa(3)=0.5d0*(p+sqrt(d))
        call qadrti(aa,z1,z2,j)
        if(j.eq.2) then
          x1=z1
          x2=z2
        else
          x1r=z1
          x1i=z2
        endif
        aa(3)=0.5d0*(p-sqrt(d))
        goto 290
       endif
 230   continue
        if(abs(p).le.eps) goto 300
          x1=0.0d0
          x2=0.0d0
          j=2
          aa(1)=1.d0
          aa(2)=0.d0
          aa(3)=p
          goto 290
  300 continue
      x1=-bb
      x2=x1
      x3=x1
      x4=x1
      i=4
      return
      end
!-----------------------last line of subroutine ferra.f----------------

!-------------------------------rppset.f--------------------------------
! Version: 051219-1435
! Reference: JNC TN1410 2002-201
!-----------------------------------------------------------------------
!23456789|123456789|123456789|123456789|123456789|123456789|123456789|12

! ----------------------------------------------------------------------
! Required (geometry) subroutine for use with the EGS5 Code System
! ----------------------------------------------------------------------
! This is a subroutine rppset.
      subroutine rppset(izon)
      implicit none
c
      include 'auxcommons/geom_common.f' ! geom-common file
c
      integer izon

      if(rpppnt(1,izon).ge.rpppnt(2,izon).or.
     &   rpppnt(3,izon).ge.rpppnt(4,izon).or.
     &   rpppnt(5,izon).ge.rpppnt(6,izon)) then
        write(*,*) 'Error of RPP ',nbrpp(izon),' : MAX <= MIX'
        stop
      end if
      return
      end
!--------------------last line of subroutine rppset.f------------------

!-------------------------------rppcg1.f--------------------------------
! Version: 051219-1435
! Reference: JNC TN1410 2002-201
!-----------------------------------------------------------------------
!23456789|123456789|123456789|123456789|123456789|123456789|123456789|12

! ----------------------------------------------------------------------
! Required (geometry) subroutine for use with the EGS5 Code System
! ----------------------------------------------------------------------
! This is a subroutine rppcg1. 
! ----------------------------------------------------------------------
      subroutine rppcg1(izon,xl,yl,zl,unp,vnp,wnp)
      implicit none
c
      include 'auxcommons/geom_common.f' ! geom-common file
c
      double precision xl,yl,zl
      double precision unp,vnp,wnp
      integer izon
      double precision udotal
c
      if(unp.ne.0.0d0) then
        udotal=(rpppnt(1,izon)-xl)/unp
        if ((udotal.ge.-rppeps)) then
          if(udotal.ge.0.0d0) then
            itvalm=itvalm+1
            atval(itvalm)=udotal
          else
            itvalm=itvalm+1
            atval(itvalm)=0.0d0
          endif
        end if
        udotal=(rpppnt(2,izon)-xl)/unp
        if ((udotal.ge.-rppeps)) then
          if(udotal.ge.0.0d0) then
            itvalm=itvalm+1
            atval(itvalm)=udotal
          else
            itvalm=itvalm+1
            atval(itvalm)=0.0d0
          endif
        end if
      end if
      if(vnp.ne.0.0d0) then
        udotal=(rpppnt(3,izon)-yl)/vnp
        if ((udotal.ge.-rppeps)) then
          if(udotal.ge.0.0d0) then
            itvalm=itvalm+1
            atval(itvalm)=udotal
          else
            itvalm=itvalm+1
            atval(itvalm)=0.0d0
          endif
        end if
        udotal=(rpppnt(4,izon)-yl)/vnp
        if ((udotal.ge.-rppeps)) then
          if(udotal.ge.0.0d0) then
            itvalm=itvalm+1
            atval(itvalm)=udotal
          else
            itvalm=itvalm+1
            atval(itvalm)=0.0d0
          endif
        end if
      end if
      if(wnp.ne.0.0d0) then
        udotal=(rpppnt(5,izon)-zl)/wnp
        if ((udotal.ge.-rppeps)) then
          if(udotal.ge.0.0d0) then
            itvalm=itvalm+1
            atval(itvalm)=udotal
          else
            itvalm=itvalm+1
            atval(itvalm)=0.0d0
          endif
        end if
        udotal=(rpppnt(6,izon)-zl)/wnp
        if(udotal.ge.-rppeps) then
          if(udotal.ge.0.0d0) then
            itvalm=itvalm+1
            atval(itvalm)=udotal
          else
            itvalm=itvalm+1
            atval(itvalm)=0.0d0
          endif
        end if
      end if
      return
      end 
!-----------------------last line of subroutine rppcg1------------------
                                                                        
!-------------------------------rcccg1.f--------------------------------
! Version: 051219-1435
! Reference: JNC TN1410 2002-201
!-----------------------------------------------------------------------
!23456789|123456789|123456789|123456789|123456789|123456789|123456789|12

! ----------------------------------------------------------------------
! Required (geometry) subroutine for use with the EGS5 Code System
! ----------------------------------------------------------------------
! This is a subroutine rcccg1. 
! ----------------------------------------------------------------------
      subroutine rcccg1(izon,xl,yl,zl,unp,vnp,wnp)
      implicit none
c
      include 'auxcommons/geom_common.f' ! geom-common file
c
      double precision xl,yl,zl
      double precision unp,vnp,wnp
      integer izon
      double precision udotr
      double precision acyl,acylmv,bcyl,bcylmv,bcyln,ccyl,ccyln,dcyl,
     &                 ecylm2,fcyl,argcy,clong2,rootcy
c
      acylmv=1.0d0
      acyl= dsqrt(rccpnt(4,izon)*rccpnt(4,izon)
     &           +rccpnt(5,izon)*rccpnt(5,izon)
     &           +rccpnt(6,izon)*rccpnt(6,izon))
      bcyl=((xl-rccpnt(1,izon))*rccpnt(4,izon)
     &    + (yl-rccpnt(2,izon))*rccpnt(5,izon)
     &    + (zl-rccpnt(3,izon))*rccpnt(6,izon))/acyl
      bcylmv=(rccpnt(4,izon)*unp+rccpnt(5,izon)*vnp
     &      + rccpnt(6,izon)*wnp)/acyl
      ccyl=(xl-rccpnt(1,izon))*(xl-rccpnt(1,izon))
     &    +(yl-rccpnt(2,izon))*(yl-rccpnt(2,izon))
     &    +(zl-rccpnt(3,izon))*(zl-rccpnt(3,izon))
      dcyl=(xl-rccpnt(1,izon))*unp+(yl-rccpnt(2,izon))*vnp
     &    +(zl-rccpnt(3,izon))*wnp
      ecylm2=dabs(acylmv*acylmv-bcylmv*bcylmv)
      if(bcylmv.ne.0.d0) then
        udotr=(acyl-bcyl)/bcylmv*acylmv
        if(udotr.ge.0.d0) then
          bcyln=((xl+udotr*unp-rccpnt(1,izon))*rccpnt(4,izon)
     &          +(yl+udotr*vnp-rccpnt(2,izon))*rccpnt(5,izon)
     &          +(zl+udotr*wnp-rccpnt(3,izon))*rccpnt(6,izon))/acyl
          ccyln=(xl+udotr*unp-rccpnt(1,izon))
     &         *(xl+udotr*unp-rccpnt(1,izon))
     &         +(yl+udotr*vnp-rccpnt(2,izon))
     &         *(yl+udotr*vnp-rccpnt(2,izon))
     &         +(zl+udotr*wnp-rccpnt(3,izon))
     &         *(zl+udotr*wnp-rccpnt(3,izon))
          clong2=dabs(ccyln-bcyln*bcyln)
          if(clong2.le.
     &      (rccpnt(7,izon)+rcceps)*(1.0d0+rcceps)
     &     *(rccpnt(7,izon)+rcceps)*(1.0d0+rcceps) ) then
            itvalm=itvalm+1
            atval(itvalm)=udotr
          end if
        end if
        udotr=-bcyl/bcylmv*acylmv
        if(udotr.ge.0.0d0) then
          bcyln=((xl+udotr*unp-rccpnt(1,izon))*rccpnt(4,izon)
     &          +(yl+udotr*vnp-rccpnt(2,izon))*rccpnt(5,izon)
     &          +(zl+udotr*wnp-rccpnt(3,izon))*rccpnt(6,izon))/acyl
          ccyln=(xl+udotr*unp-rccpnt(1,izon))
     &         *(xl+udotr*unp-rccpnt(1,izon))
     &         +(yl+udotr*vnp-rccpnt(2,izon))
     &         *(yl+udotr*vnp-rccpnt(2,izon))
     &         +(zl+udotr*wnp-rccpnt(3,izon))
     &         *(zl+udotr*wnp-rccpnt(3,izon))
          clong2=dabs(ccyln-bcyln*bcyln)
          if(clong2.le.
     &      (rccpnt(7,izon)+rcceps)*(1.0d0+rcceps)
     &     *(rccpnt(7,izon)+rcceps)*(1.0d0+rcceps)) then
            itvalm=itvalm+1
            atval(itvalm)=udotr
          end if
        end if
      end if
      if(ecylm2.ne.0.0d0) then
        fcyl=(-dcyl+bcylmv*bcyl)/ecylm2
        argcy=fcyl*fcyl-(ccyl-bcyl*bcyl-rccpnt(7,izon)*rccpnt(7,izon))
     &       /ecylm2
        if(argcy.ge.0.0d0) then
          rootcy=dsqrt(argcy)
          udotr=(fcyl-rootcy)
          if(udotr.ge.0.0d0) then
            bcyln=((xl+udotr*unp-rccpnt(1,izon))*rccpnt(4,izon)
     &            +(yl+udotr*vnp-rccpnt(2,izon))*rccpnt(5,izon)
     &            +(zl+udotr*wnp-rccpnt(3,izon))*rccpnt(6,izon))/acyl
            if((bcyln+rcceps)*(1.0d0+rcceps).ge.0.0d0.and.
     &         (bcyln-rcceps)*(1.0d0-rcceps).le.acyl) then
              itvalm=itvalm+1
              atval(itvalm)=udotr
            end if
          end if
          udotr=(fcyl+rootcy)
          if(udotr.ge.0.0d0) then
            bcyln=((xl+udotr*unp-rccpnt(1,izon))*rccpnt(4,izon)
     &            +(yl+udotr*vnp-rccpnt(2,izon))*rccpnt(5,izon)
     &            +(zl+udotr*wnp-rccpnt(3,izon))*rccpnt(6,izon))/acyl
            if (((bcyln+rcceps)*(1.0d0+rcceps).ge.0.0d0.and.
     &           (bcyln-rcceps)*(1.0d0-rcceps).le.acyl)) then
              itvalm=itvalm+1
              atval(itvalm)=udotr
            end if
          end if
        end if
      end if
      return
      end 
!-----------------------last line of subroutine rcccg1------------------
                                                                        
!-------------------------------sphcg1.f--------------------------------
! Version: 051219-1435
! Reference: JNC TN1410 2002-201
!-----------------------------------------------------------------------
!23456789|123456789|123456789|123456789|123456789|123456789|123456789|12

! ----------------------------------------------------------------------
! Required (geometry) subroutine for use with the EGS5 Code System
! ----------------------------------------------------------------------
! This is a subroutine sphcg1. 
! ----------------------------------------------------------------------
      subroutine sphcg1(izon,xl,yl,zl,unp,vnp,wnp)
      implicit none
c
      include 'auxcommons/geom_common.f' ! geom-common file
c
      double precision xl,yl,zl
      double precision unp,vnp,wnp
      integer izon
      double precision udotr
      double precision asph,bsph,csph,argsp,rootsp
      asph=1.0d0
      bsph=((xl-sphpnt(1,izon))*unp+(yl-sphpnt(2,izon))*vnp
     &    + (zl-sphpnt(3,izon))*wnp)/asph
      csph=(xl-sphpnt(1,izon))*(xl-sphpnt(1,izon))
     &    +(yl-sphpnt(2,izon))*(yl-sphpnt(2,izon))
     &    +(zl-sphpnt(3,izon))*(zl-sphpnt(3,izon))
     &    -sphpnt(4,izon)*sphpnt(4,izon)
      argsp=bsph*bsph-csph
      if(argsp.ge.0.0d0) then
        rootsp=dsqrt(argsp)
        if(csph.le.0.0d0) then
          udotr=(-bsph+rootsp)/asph
          if(udotr.ge.-spheps) then
            if(udotr.ge.0.0d0) then
              itvalm=itvalm+1
              atval(itvalm)=udotr
            else
              itvalm=itvalm+1
              atval(itvalm)=0.0d0
            end if
          end if
        else
          udotr=(-bsph-rootsp)/asph
          if(udotr.ge.-spheps) then
            if(udotr.ge.0.0d0) then
              itvalm=itvalm+1
              atval(itvalm)=udotr
            else
              itvalm=itvalm+1
              atval(itvalm)=0.0d0
            end if
          end if
          udotr=(-bsph+rootsp)/asph
          if(udotr.ge.-spheps) then
            if(udotr.ge.0.0d0) then
              itvalm=itvalm+1
              atval(itvalm)=udotr
            else
              itvalm=itvalm+1
              atval(itvalm)=0.0d0
            end if
          end if
        end if
      end if
      return
      end 
!-----------------------last line of subroutine sphcg1------------------
                                                                        
!-------------------------------srzold.f--------------------------------
! Version: 051219-1435
! Reference: JNC TN1410 2002-201
!-----------------------------------------------------------------------
!23456789|123456789|123456789|123456789|123456789|123456789|123456789|12

! ----------------------------------------------------------------------
! Required (geometry) subroutine for use with the EGS5 Code System
! ----------------------------------------------------------------------
! This is a subroutine srzold. 
! ----------------------------------------------------------------------
      subroutine srzold(xl,yl,zl,irlold,irlfg)
      implicit none
c
      include 'auxcommons/dataconst_common.f' ! dataconst-common file
      include 'auxcommons/geom_common.f' ! geom-common file
c
      double precision xl,yl,zl
      integer irlold,irlfg
      double precision acyl,bcyl,ccyl,clong1
      double precision thator,thstor,thetor
      integer nozone,jty,kno
      integer m,n,k,imach,imachf,ithafg
c rec & tec
      double precision hh,rs2,rl2,rs4,rl4,plx,ply,plz
      double precision vph,vrs,vrl,um
c tec
      double precision rrf,rrf2
c ell & gel
      double precision b1,b2
c wed & box & hex & arb
      integer i,ii,ifgin
c wed & box & hex
      double precision ap(4)
c wed
      double precision ap4,top
c arb
      integer nside
      double precision dx
c gel
      double precision b3
c
      integer iorchk,iio
      integer iinout(MAX_GEOM)
c
      do n=1,MAX_GEOM
        iinout(n)=0
      enddo
c
      irlfg=1
      do n=irlold,irlold
        imach=1
        imachf=0
        iorchk=iorcnt(n)
        do k=1,nbbody(n)
          if(zoneor(k,n).eq.'OR'.or.zoneor(k,n).eq.'or') then
            iorchk=iorchk-1
            if(imachf.eq.1.and.imach.eq.1) then
              irlfg=0
              goto 900
            else
              imachf=0
              imach=1
            end if
          end if
          m=nbzone(k,n)
          nozone=abs(nbzone(k,n))
          jty=itblty(nozone)
          kno=itblno(nozone)
          iio=iinout(nozone)
          if(iio.lt.0.and.m.gt.0) then
            imach=0
            imachf=1
          elseif(iio.gt.0.and.m.lt.0) then
            imach=0
            imachf=1
          else
c     rpp check
            if(jty.eq.ityknd(1)) then
              if(kno.ge.1.and.kno.le.irppin) then
                if(xl.lt.rpppnt(1,kno).or.xl.gt.rpppnt(2,kno).or.
     &             yl.lt.rpppnt(3,kno).or.yl.gt.rpppnt(4,kno).or.
     &             zl.lt.rpppnt(5,kno).or.zl.gt.rpppnt(6,kno)) then
                  iinout(nozone)=-1
                  if ((m.gt.0)) then
                    imach=0
                  end if
                else
                  iinout(nozone)=1
                  if ((m.lt.0)) then
                    imach=0
                  end if
                end if
                imachf=1
              endif
c     sph check
            elseif(jty.eq.ityknd(2)) then
              if(kno.ge.1.and.kno.le.isphin) then
                if((xl-sphpnt(1,kno))*(xl-sphpnt(1,kno))
     &            +(yl-sphpnt(2,kno))*(yl-sphpnt(2,kno))
     &            +(zl-sphpnt(3,kno))*(zl-sphpnt(3,kno)) .gt.
     &            sphpnt(4,kno)*sphpnt(4,kno)) then
                  iinout(nozone)=-1
                  if(m.gt.0) then
                    imach=0
                  end if
                else
                  iinout(nozone)=1
                  if(m.lt.0) then
                    imach=0
                  end if
                end if
                imachf=1
              end if
c     rcc check
            elseif(jty.eq.ityknd(3)) then
              if(kno.ge.1.and.kno.le.irccin) then
                acyl=dsqrt(rccpnt(4,kno)*rccpnt(4,kno)
     &                    +rccpnt(5,kno)*rccpnt(5,kno)
     &                    +rccpnt(6,kno)*rccpnt(6,kno))
                bcyl=((xl-rccpnt(1,kno))*rccpnt(4,kno)
     &              + (yl-rccpnt(2,kno))*rccpnt(5,kno)
     &              + (zl-rccpnt(3,kno))*rccpnt(6,kno))/acyl
                ccyl=(xl-rccpnt(1,kno))*(xl-rccpnt(1,kno))
     &              +(yl-rccpnt(2,kno))*(yl-rccpnt(2,kno))
     &              +(zl-rccpnt(3,kno))*(zl-rccpnt(3,kno))
                clong1=dsqrt(dabs(ccyl-bcyl*bcyl))
                if(clong1.gt.rccpnt(7,kno).or.
     &             bcyl.lt.0.0d0.or.bcyl.gt.acyl) then
                  iinout(nozone)=-1
                  if(m.gt.0) then
                    imach=0
                 end if
                else
                  iinout(nozone)=1
                  if(m.lt.0) then
                    imach=0
                  end if
                end if
                imachf=1
              end if
c     trc check
            elseif(jty.eq.ityknd(4)) then
              if(kno.ge.1.and.kno.le.itrcin) then
                acyl=dsqrt(trcpnt(4,kno)*trcpnt(4,kno)
     &                    +trcpnt(5,kno)*trcpnt(5,kno)
     &                    +trcpnt(6,kno)*trcpnt(6,kno))
                bcyl=((xl-trcpnt(1,kno))*trcpnt(4,kno)
     &              + (yl-trcpnt(2,kno))*trcpnt(5,kno)
     &              + (zl-trcpnt(3,kno))*trcpnt(6,kno))/acyl
                ccyl=(xl-trcpnt(1,kno))*(xl-trcpnt(1,kno))
     &              +(yl-trcpnt(2,kno))*(yl-trcpnt(2,kno))
     &              +(zl-trcpnt(3,kno))*(zl-trcpnt(3,kno))
                clong1=dsqrt(dabs(ccyl-bcyl*bcyl))
                if(clong1.gt.(trcpnt(8,kno)-trcpnt(7,kno))/acyl*bcyl
     &            +trcpnt(7,kno) .or.bcyl.lt.0.0.or.bcyl.gt.acyl) then
                  iinout(nozone)=-1
                  if(m.gt.0) then
                    imach=0
                  end if
                else
                  iinout(nozone)=1
                  if(m.lt.0) then
                    imach=0
                  end if
                end if
                imachf=1
              end if
c     tor check
            elseif(jty.eq.ityknd(5)) then
              if(kno.ge.1.and.kno.le.itorin) then
                if(nint(torpnt(8,kno)).eq.1) then
                  acyl=(yl-torpnt(2,kno))*(yl-torpnt(2,kno))
     &                +(zl-torpnt(3,kno))*(zl-torpnt(3,kno))
                  bcyl=xl-torpnt(1,kno)
                  if(yl-torpnt(2,kno).eq.0.0d0.and.
     &               zl-torpnt(3,kno).eq.0.0d0) then
                    thator=0.0d0
                  else
                    thator=atan2(zl-torpnt(3,kno),yl-torpnt(2,kno))
                  end if
                end if
                if(nint(torpnt(8,kno)).eq.2) then
                  acyl=(zl-torpnt(3,kno))*(zl-torpnt(3,kno))
     &                +(xl-torpnt(1,kno))*(xl-torpnt(1,kno))
                  bcyl=yl-torpnt(2,kno)
                  if(zl-torpnt(3,kno).eq.0.0d0.and.
     &               xl-torpnt(1,kno).eq.0.0d0) then
                    thator=0.0d0
                  else
                    thator=atan2(xl-torpnt(1,kno),zl-torpnt(3,kno))
                  end if
                end if
                if(nint(torpnt(8,kno)).eq.3) then
                  acyl=(xl-torpnt(1,kno))*(xl-torpnt(1,kno))
     &                +(yl-torpnt(2,kno))*(yl-torpnt(2,kno))
                  bcyl=zl-torpnt(3,kno)
                  if(yl-torpnt(2,kno).eq.0.0d0.and.
     &               xl-torpnt(1,kno).eq.0.0d0) then
                    thator=0.0d0
                  else
                    thator=atan2(yl-torpnt(2,kno),xl-torpnt(1,kno))
                  end if
                end if
                thator=thator*180.0d0/PI
                thstor=torpnt(6,kno)
                thetor=torpnt(7,kno)
                if(thator.lt.0.0d0) then
                  thator=thator+360.0d0
                end if
                if(thstor.lt.0.0d0) then
                  thstor=thstor+360.0d0
                end if
                if(thetor.lt.0.0d0) then
                  thetor=thetor+360.0d0
                end if
                ithafg=0
                if(thstor.eq.thetor) then
                  ithafg=1
                else
                  if(thetor.gt.thstor) then
                    if(thator.ge.thstor.and.thator.le.thetor) then
                      ithafg=1
                    end if
                  else
                    if(thator.ge.thstor.or.thator.le.thetor) then
                      ithafg=1
                    end if
                  end if
                end if
                ccyl=dsqrt(acyl)-torpnt(4,kno)
                clong1=dsqrt(ccyl*ccyl+bcyl*bcyl)
                if(clong1.gt.torpnt(5,kno).or.ithafg.eq.0) then
                  iinout(nozone)=-1
                  if(m.gt.0) then
                    imach=0
                  end if
                else
                  iinout(nozone)=1
                  if(m.lt.0) then
                    imach=0
                  end if
                end if
                imachf=1
              end if
c     rec check
            elseif(jty.eq.ityknd(6)) then
              if(kno.ge.1.and.kno.le.irecin) then
                hh =recpnt(13,kno)
                rs2=recpnt(14,kno)
                rl2=recpnt(15,kno)
                rs4=rs2*rs2
                rl4=rl2*rl2
c
c5    COMPUTE (V-XB) FOR X,Y,Z COORDINATES
c
                plx=recpnt(1,kno)-xl
                ply=recpnt(2,kno)-yl
                plz=recpnt(3,kno)-zl
c
c6    TRANSFORM XL,YL,ZL TO THE COORDINATES OF THE REC
c
                vph=plx*recpnt( 4,kno)+ply*recpnt( 5,kno)
     &             +plz*recpnt( 6,kno)
                vrs=plx*recpnt( 7,kno)+ply*recpnt( 8,kno)
     &             +plz*recpnt( 9,kno)
                vrl=plx*recpnt(10,kno)+ply*recpnt(11,kno)
     &             +plz*recpnt(12,kno)
                um=rl4*vrs*vrs+rs4*vrl*vrl-rs4*rl4
                if(-vph.lt.0.0d0.OR.-vph.gt.hh.or.um.gt.0.0d0) then
                  iinout(nozone)=-1
                  if(m.gt.0) then
                    imach=0
                  end if
                else
                  iinout(nozone)=1
                  if(m.lt.0) then
                    imach=0
                  end if
                end if
                imachf=1
              end if
c     ell check
            elseif(jty.eq.ityknd(7)) then
              if(kno.ge.1.and.kno.le.iellin) then
                b1=(xl-ellpnt(1,kno))*(xl-ellpnt(1,kno))
     &            +(yl-ellpnt(2,kno))*(yl-ellpnt(2,kno))
     &            +(zl-ellpnt(3,kno))*(zl-ellpnt(3,kno))
                b2=(xl-ellpnt(4,kno))*(xl-ellpnt(4,kno))
     &            +(yl-ellpnt(5,kno))*(yl-ellpnt(5,kno))
     &            +(zl-ellpnt(6,kno))*(zl-ellpnt(6,kno))
                if(dsqrt(b1)+dsqrt(b2).gt.ellpnt(7,kno)) then
                  iinout(nozone)=-1
                  if(m.gt.0) then
                    imach=0
                  end if
                else
                  iinout(nozone)=1
                  if(m.lt.0) then
                    imach=0
                  end if
                end if
                imachf=1
              end if
c     wed check
            elseif(jty.eq.ityknd(8)) then
              if(kno.ge.1.and.kno.le.iwedin) then
                ifgin=1
                do i=1,3
                  ii=3*i+1
                  ap(i)=(xl-wedpnt(1,kno))*wedpnt(ii,kno)
     &                 +(yl-wedpnt(2,kno))*wedpnt(ii+1,kno)
     &                 +(zl-wedpnt(3,kno))*wedpnt(ii+2,kno)
                  if(ap(i).lt.0.0.or.ap(i).gt.wedpnt(i+12,kno)) then
                    ifgin=0
                    go to 810
                  endif
                end do
                ap4=ap(1)*wedpnt(14,kno)+ap(2)*wedpnt(13,kno)
                top=wedpnt(13,kno)*wedpnt(14,kno)-ap4
                if(top.lt.0.0) then
                  ifgin=0
                endif
c
  810           continue
                if(ifgin.eq.0) then
                  iinout(nozone)=-1
                  if(m.gt.0) then
                    imach=0
                  end if
                else
                  iinout(nozone)=1
                  if(m.lt.0) then
                    imach=0
                  end if
                end if
               imachf=1
              end if
c     box check
            elseif(jty.eq.ityknd(9)) then
              if(kno.ge.1.and.kno.le.iboxin) then
                ifgin=1
                do i=1,3
                  ii=3*i+1
                  ap(i)=(xl-boxpnt(1,kno))*boxpnt(ii,kno)
     &                 +(yl-boxpnt(2,kno))*boxpnt(ii+1,kno)
     &                 +(zl-boxpnt(3,kno))*boxpnt(ii+2,kno)
c
                  if(ap(i).lt.0.0d0.or.ap(i).gt.boxpnt(i+12,kno)) then
                    ifgin=0
                    go to 910
                  endif
                end do
c
  910           continue
                if(ifgin.eq.0) then
                  iinout(nozone)=-1
                  if(m.gt.0) then
                    imach=0
                  end if
                else
                  iinout(nozone)=1
                  if(m.lt.0) then
                    imach=0
                  end if
                end if
                imachf=1
              end if
c     arb check
            elseif(jty.eq.ityknd(10)) then
              if(kno.ge.1.and.kno.le.iarbin) then
                ifgin=1
                nside=arbtbl(26,kno)
                do i=1,nside
                  ii=(i-1)*4+1
                  dx=arbtbl(ii  ,kno)*xl+arbtbl(ii+1,kno)*yl
     &              +arbtbl(ii+2,kno)*zl+arbtbl(ii+3,kno)
                  if( dx.lt.0.0d0) then
                    ifgin=0
                    goto 1010
                  endif
                end do
 1010           continue
                if(ifgin.eq.0) then
                  iinout(nozone)=-1
                  if(m.gt.0) then
                    imach=0
                  end if
                else
                  iinout(nozone)=1
                  if(m.lt.0) then
                    imach=0
                  end if
                end if
                imachf=1
              end if
c     hex check
            elseif(jty.eq.ityknd(11)) then
              if(kno.ge.1.and.kno.le.ihexin) then
                ifgin=1
                do i=1,4
                  ii=3*i
                  ap(i)=(xl-hexpnt(ii+12,kno))*hexpnt(ii+24,kno)
     &                 +(yl-hexpnt(ii+13,kno))*hexpnt(ii+25,kno)
     &                 +(zl-hexpnt(ii+14,kno))*hexpnt(ii+26,kno)
c
                  if(ap(i).lt.0.0d0.or.ap(i).gt.hexpnt(i+10,kno)) then
                    ifgin=0
                    go to 1110
                  endif
                end do
 1110           continue
                if(ifgin.eq.0) then
                  iinout(nozone)=-1
                  if(m.gt.0) then
                    imach=0
                  end if
                else
                  iinout(nozone)=1
                  if(m.lt.0) then
                    imach=0
                  end if
                end if
                imachf=1
              end if
c     haf check
            elseif(jty.eq.ityknd(12)) then
              if(kno.ge.1.and.kno.le.ihafin) then
                if((xl*hafpnt(1,kno)
     &              +yl*hafpnt(2,kno)
     &              +zl*hafpnt(3,kno)).lt.
     &              hafpnt(4,kno)*hafpnt(5,kno)) then
                  iinout(nozone)=-1
                  if(m.gt.0) then
                    imach=0
                  end if
                else
                  iinout(nozone)=1
                  if(m.lt.0) then
                    imach=0
                  end if
                end if
                imachf=1
              end if
c     tec check
            elseif(jty.eq.ityknd(13)) then
              if(kno.ge.1.and.kno.le.itecin) then
                hh =tecpnt(14,kno)
                rs2=tecpnt(15,kno)
                rl2=tecpnt(16,kno)
                rs4=rs2*rs2
                rl4=rl2*rl2
                plx=(xl-tecpnt(1,kno))
                ply=(yl-tecpnt(2,kno))
                plz=(zl-tecpnt(3,kno))
                vph=plx*tecpnt( 4,kno)+ply*tecpnt( 5,kno)
     &             +plz*tecpnt( 6,kno)
                vrs=plx*tecpnt( 7,kno)+ply*tecpnt( 8,kno)
     &             +plz*tecpnt( 9,kno)
                vrl=plx*tecpnt(10,kno)+ply*tecpnt(11,kno)
     &             +plz*tecpnt(12,kno)
                rrf =1.0d0-(1.0d0-tecpnt(13,kno))*vph/hh
                rrf2=rrf*rrf
                um=rl4*vrs*vrs+rs4*vrl*vrl-rs4*rl4*rrf2
                IF(vph.lt.0.0d0.or.vph.gt.hh.or.um.gt.0.0d0) then
                  iinout(nozone)=-1
                  if(m.gt.0) then
                    imach=0
                  end if
                else
                  iinout(nozone)=1
                  if(m.lt.0) then
                    imach=0
                  end if
                end if
               imachf=1
              end if
c     gel check
            elseif(jty.eq.ityknd(14)) then
              if(kno.ge.1.and.kno.le.igelin) then
                b1=((xl-gelpnt(1,kno))*gelpnt(4,kno)
     &             +(yl-gelpnt(2,kno))*gelpnt(5,kno)
     &             +(zl-gelpnt(3,kno))*gelpnt(6,kno))
     &             /gelpnt(13,kno)/gelpnt(13,kno)
                b2=((xl-gelpnt(1,kno))*gelpnt(7,kno)
     &             +(yl-gelpnt(2,kno))*gelpnt(8,kno)
     &             +(zl-gelpnt(3,kno))*gelpnt(9,kno))
     &             /gelpnt(14,kno)/gelpnt(14,kno)
                b3=((xl-gelpnt(1,kno))*gelpnt(10,kno)
     &             +(yl-gelpnt(2,kno))*gelpnt(11,kno)
     &             +(zl-gelpnt(3,kno))*gelpnt(12,kno))
     &             /gelpnt(15,kno)/gelpnt(15,kno)
                if((b1*b1+b2*b2+b3*b3).gt.1.0d0) then
                  iinout(nozone)=-1
                  if(m.gt.0) then
                    imach=0
                  end if
                else
                  iinout(nozone)=1
                  if(m.lt.0) then
                    imach=0
                  end if
                end if
                imachf=1
              end if
c
c**** added new geometry
c
            end if
          end if
          if(imachf.eq.1.and.imach.eq.0.and.iorchk.eq.0) then
            goto 800
          endif
        end do

        if(imachf.eq.1.and.imach.eq.1) then
          irlfg=0
          goto 900
        end if
  800   continue
      end do
c
  900 continue
      return
      end 
!--------------------last line of subroutine srzold.f-------------------
                                                                        
!-------------------------------srzone.f--------------------------------
! Version: 051219-1435
! Reference: JNC TN1410 2002-201
! Provided as new one from T. Sugita   07/28/2004
!-----------------------------------------------------------------------
!23456789|123456789|123456789|123456789|123456789|123456789|123456789|12
! ----------------------------------------------------------------------
! Required (geometry) subroutine for use with the EGS5 Code System
! ----------------------------------------------------------------------
! This is a subroutine srzone. 
! ----------------------------------------------------------------------
      subroutine srzone(xl,yl,zl,iray,irnow,irl)
      implicit none
c
      include 'auxcommons/dataconst_common.f' ! dataconst-common file
      include 'auxcommons/geom_common.f' ! geom-common file
c
      double precision xl,yl,zl
      integer iray,irnow,irl
      double precision acyl,bcyl,ccyl,clong1
      double precision thator,thstor,thetor
      integer n,k,imach,imachf,m,ithafg
      integer nn,iorchk,iio,nnn
      integer nozone,jty,kno
c rec & tec
      double precision hh,rs2,rl2,rs4,rl4,plx,ply,plz
      double precision vph,vrs,vrl,um
c tec
      double precision rrf,rrf2
c ell & gel
      double precision b1,b2
c wed & box & hex & arb
      integer i,ii,ifgin
c wed & box & hex
      double precision ap(4)
c wed
      double precision ap4,top
c arb
      integer nside
      double precision dx
c gel
      double precision b3
c
      integer iinout(MAX_GEOM)
c
      do nn=1,MAX_GEOM
        iinout(nn)=0
      enddo
c
      irl=0
      do nn=1,izonin
        nnn = nn
        if((izonin-1)*irnow.ne.0) then
          if((nn-1)/2.eq.0) then
            nnn=3-nn
          endif   
        endif   
        n=iznnxp(iray,nnn,irnow+1)
c       n=iznnxp(iray,nn,irnow)
        imach=1
        imachf=0
        iorchk=iorcnt(n)
        do k=1,nbbody(n)
          if(zoneor(k,n).eq.'OR'.or.zoneor(k,n).eq.'or') then
            iorchk=iorchk-1
            if(imachf.eq.1.and.imach.eq.1) then
              irl=n
              goto 900
            else
              imachf=0
              imach=1
            end if
          end if
c
          m=nbzone(k,n)
          nozone=abs(nbzone(k,n))
          jty=itblty(nozone)
          kno=itblno(nozone)
          iio=iinout(nozone)
          if(iio.lt.0.and.m.gt.0) then
            imach=0
            imachf=1
          elseif(iio.gt.0.and.m.lt.0) then
            imach=0
            imachf=1
          else
c     rpp check
            if(jty.eq.ityknd(1)) then
              if(kno.ge.1.and.kno.le.irppin) then
                if(xl.lt.rpppnt(1,kno).or.xl.gt.rpppnt(2,kno).or.
     &             yl.lt.rpppnt(3,kno).or.yl.gt.rpppnt(4,kno).or.
     &             zl.lt.rpppnt(5,kno).or.zl.gt.rpppnt(6,kno)) then
                  iinout(nozone)=-1
                  if(m.gt.0) then
                    imach=0
                  end if
                else
                  iinout(nozone)=1
                  if(m.lt.0) then
                    imach=0
                  end if
                end if
                imachf=1
              end if
c     sph check
            elseif(jty.eq.ityknd(2)) then
              if(kno.ge.1.and.kno.le.isphin) then
                if((xl-sphpnt(1,kno))*(xl-sphpnt(1,kno))
     &            +(yl-sphpnt(2,kno))*(yl-sphpnt(2,kno))
     &            +(zl-sphpnt(3,kno))*(zl-sphpnt(3,kno)) .gt.
     &              sphpnt(4,kno)*sphpnt(4,kno)) then
                  iinout(nozone)=-1
                  if(m.gt.0) then
                    imach=0
                  end if
                else
                  iinout(nozone)=1
                  if(m.lt.0) then
                    imach=0
                  end if
                end if
                imachf=1
              end if
c     rcc check
            elseif(jty.eq.ityknd(3)) then
              if(kno.ge.1.and.kno.le.irccin) then
                acyl=dsqrt(rccpnt(4,kno)*rccpnt(4,kno)
     &                    +rccpnt(5,kno)*rccpnt(5,kno)
     &                    +rccpnt(6,kno)*rccpnt(6,kno))
                bcyl=((xl-rccpnt(1,kno))*rccpnt(4,kno)
     &              + (yl-rccpnt(2,kno))*rccpnt(5,kno)
     &              + (zl-rccpnt(3,kno))*rccpnt(6,kno))/acyl
                ccyl=(xl-rccpnt(1,kno))*(xl-rccpnt(1,kno))
     &              +(yl-rccpnt(2,kno))*(yl-rccpnt(2,kno))
     &              +(zl-rccpnt(3,kno))*(zl-rccpnt(3,kno))
                clong1=dsqrt(dabs(ccyl-bcyl*bcyl))
                if(clong1.gt.rccpnt(7,kno).or.
     &               bcyl.lt.0.0d0.or.bcyl.gt.acyl) then
                  iinout(nozone)=-1
                  if(m.gt.0) then
                    imach=0
                  end if
                else
                  iinout(nozone)=1
                  if(m.lt.0) then
                    imach=0
                  end if
               end if
               imachf=1
              end if
c     trc check
            elseif(jty.eq.ityknd(4)) then
              if(kno.ge.1.and.kno.le.itrcin) then
                acyl=dsqrt(trcpnt(4,kno)*trcpnt(4,kno)
     &                    +trcpnt(5,kno)*trcpnt(5,kno)
     &                    +trcpnt(6,kno)*trcpnt(6,kno))
                bcyl=((xl-trcpnt(1,kno))*trcpnt(4,kno)
     &              + (yl-trcpnt(2,kno))*trcpnt(5,kno)
     &              + (zl-trcpnt(3,kno))*trcpnt(6,kno))/acyl
                ccyl=(xl-trcpnt(1,kno))*(xl-trcpnt(1,kno))
     &              +(yl-trcpnt(2,kno))*(yl-trcpnt(2,kno))
     &              +(zl-trcpnt(3,kno))*(zl-trcpnt(3,kno))
                clong1=dsqrt(dabs(ccyl-bcyl*bcyl))
                if(clong1.gt.(trcpnt(8,kno)-trcpnt(7,kno))/acyl*bcyl
     &          +trcpnt(7,kno).or.bcyl.lt.0.0d0.or.bcyl.gt.acyl) then
                  iinout(nozone)=-1
                  if(m.gt.0) then
                    imach=0
                  end if
                else
                  iinout(nozone)=1
                  if(m.lt.0) then
                    imach=0
                  end if
                end if
                imachf=1
              end if
c     tor check
            elseif(jty.eq.ityknd(5)) then
              if(kno.ge.1.and.kno.le.itorin) then
                if(nint(torpnt(8,kno)).eq.1) then
                  acyl=(yl-torpnt(2,kno))*(yl-torpnt(2,kno))
     &                +(zl-torpnt(3,kno))*(zl-torpnt(3,kno))
                  bcyl=xl-torpnt(1,kno)
                  if(yl-torpnt(2,kno).eq.0.0d0.and.
     &                 zl-torpnt(3,kno).eq.0.0d0) then
                    thator=0.0d0
                  else
                    thator=atan2(zl-torpnt(3,kno),yl-torpnt(2,kno))
                  end if
                end if
                if(nint(torpnt(8,kno)).eq.2) then
                  acyl=(zl-torpnt(3,kno))*(zl-torpnt(3,kno))
     &                +(xl-torpnt(1,kno))*(xl-torpnt(1,kno))
                  bcyl=yl-torpnt(2,kno)
                  if(zl-torpnt(3,kno).eq.0.0d0.and.
     &                 xl-torpnt(1,kno).eq.0.0d0) then
                    thator=0.0d0
                  else
                    thator=atan2(xl-torpnt(1,kno),zl-torpnt(3,kno))
                  end if
                end if
                if(nint(torpnt(8,kno)).eq.3) then
                  acyl=(xl-torpnt(1,kno))*(xl-torpnt(1,kno))
     &                +(yl-torpnt(2,kno))*(yl-torpnt(2,kno))
                  bcyl=zl-torpnt(3,kno)
                  if(yl-torpnt(2,kno).eq.0.0d0.and.
     &                 xl-torpnt(1,kno).eq.0.0d0) then
                    thator=0.0d0
                  else
                    thator=atan2(yl-torpnt(2,kno),xl-torpnt(1,kno))
                  end if
                end if
                thator=thator*180.0d0/PI
                thstor=torpnt(6,kno)
                thetor=torpnt(7,kno)
                if(thator.lt.0.0d0) then
                  thator=thator+360.0d0
                end if
                if(thstor.lt.0.0d0) then
                  thstor=thstor+360.0d0
                end if
                if(thetor.lt.0.0d0) then
                  thetor=thetor+360.0d0
                end if
                ithafg=0
                if(thstor.eq.thetor) then
                  ithafg=1
                else
                  if(thetor.gt.thstor) then
                    if(thator.ge.thstor.and.thator.le.thetor) then
                      ithafg=1
                    end if
                  else
                    if(thator.ge.thstor.or.thator.le.thetor) then
                      ithafg=1
                    end if
                  end if
                end if
                ccyl=dsqrt(acyl)-torpnt(4,kno)
                clong1=dsqrt(ccyl*ccyl+bcyl*bcyl)
                if(clong1.gt.torpnt(5,kno).or.ithafg.eq.0) then
                  iinout(nozone)=-1
                  if(m.gt.0) then
                    imach=0
                  end if
                else
                  iinout(nozone)=1
                  if(m.lt.0) then
                    imach=0
                  end if
                end if
                imachf=1
              end if
c     rec check
            elseif(jty.eq.ityknd(6)) then
              if(kno.ge.1.and.kno.le.irecin) then
                hh =recpnt(13,kno)
                rs2=recpnt(14,kno)
                rl2=recpnt(15,kno)
                rs4=rs2*rs2
                rl4=rl2*rl2
c
c5    COMPUTE (V-XB) FOR X,Y,Z COORDINATES
c
                plx=recpnt(1,kno)-xl
                ply=recpnt(2,kno)-yl
                plz=recpnt(3,kno)-zl
c
c6    TRANSFORM XL,YL,ZL TO THE COORDINATES OF THE REC
c
                vph=plx*recpnt( 4,kno)+ply*recpnt( 5,kno)
     &             +plz*recpnt( 6,kno)
                vrs=plx*recpnt( 7,kno)+ply*recpnt( 8,kno)
     &             +plz*recpnt( 9,kno)
                vrl=plx*recpnt(10,kno)+ply*recpnt(11,kno)
     &             +plz*recpnt(12,kno)
                um=rl4*vrs*vrs+rs4*vrl*vrl-rs4*rl4
                if(-vph.lt.0.0d0.OR.-vph.gt.hh.or.um.gt.0.0d0) then
                  iinout(nozone)=-1
                  if(m.gt.0) then
                    imach=0
                  end if
                else
                  iinout(nozone)=1
                  if(m.lt.0) then
                    imach=0
                  end if
                end if
                imachf=1
              end if
c     ell check
            elseif(jty.eq.ityknd(7)) then
              if(kno.ge.1.and.kno.le.iellin) then
                b1=(xl-ellpnt(1,kno))*(xl-ellpnt(1,kno))
     &            +(yl-ellpnt(2,kno))*(yl-ellpnt(2,kno))
     &            +(zl-ellpnt(3,kno))*(zl-ellpnt(3,kno))
                b2=(xl-ellpnt(4,kno))*(xl-ellpnt(4,kno))
     &            +(yl-ellpnt(5,kno))*(yl-ellpnt(5,kno))
     &            +(zl-ellpnt(6,kno))*(zl-ellpnt(6,kno))
                if(dsqrt(b1)+dsqrt(b2).gt.ellpnt(7,kno)) then
                  iinout(nozone)=-1
                  if(m.gt.0) then
                    imach=0
                  end if
                else
                  iinout(nozone)=1
                  if(m.lt.0) then
                    imach=0
                  end if
                end if
                imachf=1
              end if
c     wed check
            elseif(jty.eq.ityknd(8)) then
              if(kno.ge.1.and.kno.le.iwedin) then
                ifgin=1
                do i=1,3
                  ii=3*i+1
                  ap(i)=(xl-wedpnt(1,kno))*wedpnt(ii,kno)
     &                 +(yl-wedpnt(2,kno))*wedpnt(ii+1,kno)
     &                 +(zl-wedpnt(3,kno))*wedpnt(ii+2,kno)
                  if(ap(i).lt.0.0.or.ap(i).gt.wedpnt(i+12,kno)) then
                    ifgin=0
                    go to 810
                  endif
                end do
                ap4=ap(1)*wedpnt(14,kno)+ap(2)*wedpnt(13,kno)
                top=wedpnt(13,kno)*wedpnt(14,kno)-ap4
                if(top.lt.0.0) then
                  ifgin=0
                endif
c
  810           continue
                if(ifgin.eq.0) then
                  iinout(nozone)=-1
                  if(m.gt.0) then
                    imach=0
                  end if
                else
                  iinout(nozone)=1
                  if(m.lt.0) then
                    imach=0
                  end if
                end if
                imachf=1
              end if
c     box check
            elseif(jty.eq.ityknd(9)) then
              if(kno.ge.1.and.kno.le.iboxin) then
                ifgin=1
                do i=1,3
                  ii=3*i+1
                  ap(i)=(xl-boxpnt(1,kno))*boxpnt(ii,kno)
     &                 +(yl-boxpnt(2,kno))*boxpnt(ii+1,kno)
     &                 +(zl-boxpnt(3,kno))*boxpnt(ii+2,kno)
c
                  if(ap(i).lt.0.0d0.or.ap(i).gt.boxpnt(i+12,kno)) then
                    ifgin=0
                    go to 910
                  endif
                end do
c
  910           continue
                if(ifgin.eq.0) then
                  iinout(nozone)=-1
                  if(m.gt.0) then
                    imach=0
                  end if
                else
                  iinout(nozone)=1
                  if(m.lt.0) then
                    imach=0
                  end if
                end if
                imachf=1
              end if
c     arb check
            elseif(jty.eq.ityknd(10)) then
              if(kno.ge.1.and.kno.le.iarbin) then
                ifgin=1
                nside=arbtbl(26,kno)
                do i=1,nside
                  ii=(i-1)*4+1
                  dx=arbtbl(ii  ,kno)*xl+arbtbl(ii+1,kno)*yl
     &              +arbtbl(ii+2,kno)*zl+arbtbl(ii+3,kno)
                  if( dx.lt.0.0d0) then
                    ifgin=0
                    goto 1010
                  endif
                end do
 1010           continue
                if(ifgin.eq.0) then
                  iinout(nozone)=-1
                  if(m.gt.0) then
                    imach=0
                  end if
                else
                  iinout(nozone)=1
                  if(m.lt.0) then
                    imach=0
                  end if
                end if
                imachf=1
              end if
c     hex check
            elseif(jty.eq.ityknd(11)) then
              if(kno.ge.1.and.kno.le.ihexin) then
                ifgin=1
                do i=1,4
                  ii=3*i
                  ap(i)=(xl-hexpnt(ii+12,kno))*hexpnt(ii+24,kno)
     &                 +(yl-hexpnt(ii+13,kno))*hexpnt(ii+25,kno)
     &                 +(zl-hexpnt(ii+14,kno))*hexpnt(ii+26,kno)
c
                  if(ap(i).lt.0.0d0.or.ap(i).gt.hexpnt(i+10,kno)) then
                    ifgin=0
                    go to 1110
                  endif
                end do
 1110           continue
                if(ifgin.eq.0) then
                  iinout(nozone)=-1
                  if(m.gt.0) then
                    imach=0
                  end if
                else
                  iinout(nozone)=1
                  if(m.lt.0) then
                    imach=0
                  end if
                end if
                imachf=1
              end if
c     haf check
            elseif(jty.eq.ityknd(12)) then
              if(kno.ge.1.and.kno.le.ihafin) then
                if((xl*hafpnt(1,kno)
     &              +yl*hafpnt(2,kno)
     &              +zl*hafpnt(3,kno)).lt.
     &              hafpnt(4,kno)*hafpnt(5,kno)) then
                  iinout(nozone)=-1
                  if(m.gt.0) then
                    imach=0
                  end if
                else
                  iinout(nozone)=1
                  if(m.lt.0) then
                    imach=0
                  end if
                end if
                imachf=1
              end if
c     tec check
            elseif(jty.eq.ityknd(13)) then
              if(kno.ge.1.and.kno.le.itecin) then
                hh =tecpnt(14,kno)
                rs2=tecpnt(15,kno)
                rl2=tecpnt(16,kno)
                rs4=rs2*rs2
                rl4=rl2*rl2
                plx=(xl-tecpnt(1,kno))
                ply=(yl-tecpnt(2,kno))
                plz=(zl-tecpnt(3,kno))
                vph=plx*tecpnt( 4,kno)+ply*tecpnt( 5,kno)
     &             +plz*tecpnt( 6,kno)
                vrs=plx*tecpnt( 7,kno)+ply*tecpnt( 8,kno)
     &             +plz*tecpnt( 9,kno)
                vrl=plx*tecpnt(10,kno)+ply*tecpnt(11,kno)
     &             +plz*tecpnt(12,kno)
                rrf =1.0d0-(1.0d0-tecpnt(13,kno))*vph/hh
                rrf2=rrf*rrf
                um=rl4*vrs*vrs+rs4*vrl*vrl-rs4*rl4*rrf2
                IF(vph.lt.0.0d0.or.vph.gt.hh.or.um.gt.0.0d0) then
                  iinout(nozone)=-1
                  if(m.gt.0) then
                    imach=0
                  end if
                else
                  iinout(nozone)=1
                  if(m.lt.0) then
                    imach=0
                  end if
                end if
                imachf=1
              end if
c     gel check
            elseif(jty.eq.ityknd(14)) then
              if(kno.ge.1.and.kno.le.igelin) then
                b1=((xl-gelpnt(1,kno))*gelpnt(4,kno)
     &             +(yl-gelpnt(2,kno))*gelpnt(5,kno)
     &             +(zl-gelpnt(3,kno))*gelpnt(6,kno))
     &             /gelpnt(13,kno)/gelpnt(13,kno)
                b2=((xl-gelpnt(1,kno))*gelpnt(7,kno)
     &             +(yl-gelpnt(2,kno))*gelpnt(8,kno)
     &             +(zl-gelpnt(3,kno))*gelpnt(9,kno))
     &             /gelpnt(14,kno)/gelpnt(14,kno)
                b3=((xl-gelpnt(1,kno))*gelpnt(10,kno)
     &             +(yl-gelpnt(2,kno))*gelpnt(11,kno)
     &             +(zl-gelpnt(3,kno))*gelpnt(12,kno))
     &             /gelpnt(15,kno)/gelpnt(15,kno)
                if((b1*b1+b2*b2+b3*b3).gt.1.0d0) then
                  iinout(nozone)=-1
                  if(m.gt.0) then
                    imach=0
                  end if
                else
                  iinout(nozone)=1
                  if(m.lt.0) then
                    imach=0
                  end if
                end if
                imachf=1
              end if
c
c**** added new geometry
c
            end if
          end if
          if(imachf.eq.1.and.imach.eq.0.and.iorchk.eq.0) then
            goto 800
          endif
        end do
        if(imachf.eq.1.and.imach.eq.1) then
          irl=n
          goto 900
        end if
  800   continue
      end do
c
  900 continue
      return
      end
!--------------------last line of subroutine srzone.f-------------------
                                                                        
!-------------------------------trccg1.f--------------------------------
! Version: 051219-1435
! Reference: JNC TN1410 2002-201
!-----------------------------------------------------------------------
!23456789|123456789|123456789|123456789|123456789|123456789|123456789|12

! ----------------------------------------------------------------------
! Required (geometry) subroutine for use with the EGS5 Code System
! ----------------------------------------------------------------------
! This is a subroutine trccg1. 
! ----------------------------------------------------------------------
      subroutine trccg1(izon,xl,yl,zl,unp,vnp,wnp)
      implicit none
c
      include 'auxcommons/geom_common.f' ! geom-common file
c
      double precision xl,yl,zl
      double precision unp,vnp,wnp
      integer izon
      double precision udotr
      double precision hhtrc,xyztrc,uvwtrc,btrcn,ctrcn
      double precision clong2,rootcy
      double precision ddtrc,dwtrc,tbtrc,rrtrc,c2trc,bmtrc,a2trc,batrc
c
      hhtrc=dsqrt(trcpnt(4,izon)*trcpnt(4,izon)
     &           +trcpnt(5,izon)*trcpnt(5,izon)
     &           +trcpnt(6,izon)*trcpnt(6,izon))
      xyztrc=((xl-trcpnt(1,izon))*trcpnt(4,izon)
     &      + (yl-trcpnt(2,izon))*trcpnt(5,izon)
     &      + (zl-trcpnt(3,izon))*trcpnt(6,izon))/hhtrc
      uvwtrc=(trcpnt(4,izon)*unp+trcpnt(5,izon)*vnp
     &      + trcpnt(6,izon)*wnp)/hhtrc
      if(uvwtrc.lt.0.0d0) then
        if(xyztrc.ge.0.0d0) then
          udotr=-xyztrc/uvwtrc
          btrcn=((xl+udotr*unp-trcpnt(1,izon))*trcpnt(4,izon)
     &          +(yl+udotr*vnp-trcpnt(2,izon))*trcpnt(5,izon)
     &          +(zl+udotr*wnp-trcpnt(3,izon))*trcpnt(6,izon))/hhtrc
          ctrcn=(xl+udotr*unp-trcpnt(1,izon))
     &         *(xl+udotr*unp-trcpnt(1,izon))
     &         +(yl+udotr*vnp-trcpnt(2,izon))
     &         *(yl+udotr*vnp-trcpnt(2,izon))
     &         +(zl+udotr*wnp-trcpnt(3,izon))
     &         *(zl+udotr*wnp-trcpnt(3,izon))
          clong2=dabs(ctrcn-btrcn*btrcn)
          if(clong2.le.(trcpnt(7,izon)+trceps)*(1.0d0+trceps)
     &                *(trcpnt(7,izon)+trceps)*(1.0d0+trceps) ) then
            itvalm=itvalm+1
            atval(itvalm)=udotr
          end if
        end if
        if ((xyztrc.ge.hhtrc)) then
          udotr=(hhtrc-xyztrc)/uvwtrc
          btrcn=((xl+udotr*unp-trcpnt(1,izon))*trcpnt(4,izon)
     &          +(yl+udotr*vnp-trcpnt(2,izon))*trcpnt(5,izon)
     &          +(zl+udotr*wnp-trcpnt(3,izon))*trcpnt(6,izon))/hhtrc
          ctrcn=(xl+udotr*unp-trcpnt(1,izon))
     &         *(xl+udotr*unp-trcpnt(1,izon))
     &         +(yl+udotr*vnp-trcpnt(2,izon))
     &         *(yl+udotr*vnp-trcpnt(2,izon))
     &         +(zl+udotr*wnp-trcpnt(3,izon))
     &         *(zl+udotr*wnp-trcpnt(3,izon))
          clong2=dabs(ctrcn-btrcn*btrcn)
          if(clong2.le.(trcpnt(8,izon)+trceps)*(1.0d0+trceps)
     &                *(trcpnt(8,izon)+trceps)*(1.0d0+trceps) ) then
            itvalm=itvalm+1
            atval(itvalm)=udotr
          end if
        end if
      end if
      if(uvwtrc.gt.0.0d0) then
        if(xyztrc.le.0.0d0) then
          udotr=-xyztrc/uvwtrc
          btrcn=((xl+udotr*unp-trcpnt(1,izon))*trcpnt(4,izon)
     &          +(yl+udotr*vnp-trcpnt(2,izon))*trcpnt(5,izon)
     &          +(zl+udotr*wnp-trcpnt(3,izon))*trcpnt(6,izon))/hhtrc
          ctrcn=(xl+udotr*unp-trcpnt(1,izon))
     &         *(xl+udotr*unp-trcpnt(1,izon))
     &         +(yl+udotr*vnp-trcpnt(2,izon))
     &         *(yl+udotr*vnp-trcpnt(2,izon))
     &         +(zl+udotr*wnp-trcpnt(3,izon))
     &         *(zl+udotr*wnp-trcpnt(3,izon))
          clong2=dabs(ctrcn-btrcn*btrcn)
          if(clong2.le.(trcpnt(7,izon)+trceps)*(1.0d0+trceps)
     &                *(trcpnt(7,izon)+trceps)*(1.0d0+trceps) ) then
            itvalm=itvalm+1
            atval(itvalm)=udotr
          end if
        end if
        if(xyztrc.le.hhtrc) then
          udotr=(hhtrc-xyztrc)/uvwtrc
          btrcn=((xl+udotr*unp-trcpnt(1,izon))*trcpnt(4,izon)
     &          +(yl+udotr*vnp-trcpnt(2,izon))*trcpnt(5,izon)
     &          +(zl+udotr*wnp-trcpnt(3,izon))*trcpnt(6,izon))/hhtrc
          ctrcn=(xl+udotr*unp-trcpnt(1,izon))
     &         *(xl+udotr*unp-trcpnt(1,izon))
     &         +(yl+udotr*vnp-trcpnt(2,izon))
     &         *(yl+udotr*vnp-trcpnt(2,izon))
     &         +(zl+udotr*wnp-trcpnt(3,izon))
     &         *(zl+udotr*wnp-trcpnt(3,izon))
          clong2=dabs(ctrcn-btrcn*btrcn)
          if(clong2.le.(trcpnt(8,izon)+trceps)*(1.0d0+trceps)
     &                *(trcpnt(8,izon)+trceps)*(1.0d0+trceps) ) then
            itvalm=itvalm+1
            atval(itvalm)=udotr
          end if
        end if
      end if
      ddtrc=(xl-trcpnt(1,izon))*(xl-trcpnt(1,izon))
     &     +(yl-trcpnt(2,izon))*(yl-trcpnt(2,izon))
     &     +(zl-trcpnt(3,izon))*(zl-trcpnt(3,izon))
      dwtrc=-((xl-trcpnt(1,izon))*unp+(yl-trcpnt(2,izon))*vnp
     &       +(zl-trcpnt(3,izon))*wnp)
      tbtrc=(trcpnt(8,izon)-trcpnt(7,izon))/hhtrc
      rrtrc=trcpnt(7,izon)+xyztrc*tbtrc
      c2trc=ddtrc-rrtrc*rrtrc-xyztrc*xyztrc
      bmtrc=dwtrc+uvwtrc*(xyztrc+tbtrc*rrtrc)
      a2trc=1.0d0-uvwtrc*uvwtrc*(1.0d0+tbtrc*tbtrc)
      if(a2trc.eq.0.0d0) then
        udotr=c2trc/(2.0d0*bmtrc)
        if(udotr.ge.0.0d0) then
          btrcn=((xl+udotr*unp-trcpnt(1,izon))*trcpnt(4,izon)
     &          +(yl+udotr*vnp-trcpnt(2,izon))*trcpnt(5,izon)
     &          +(zl+udotr*wnp-trcpnt(3,izon))*trcpnt(6,izon))/hhtrc
          if(btrcn.ge.-trceps.and.
     &       btrcn.le.(hhtrc+trceps)*(1.0d0+trceps)) then
            itvalm=itvalm+1
            atval(itvalm)=udotr
          end if
        end if
      end if
      if(a2trc.ne.0.0d0) then
        batrc=bmtrc/a2trc
        rootcy=batrc*batrc-c2trc/a2trc
        if(rootcy.ge.0.0d0) then
          udotr=batrc-dsqrt(rootcy)
          if(udotr.ge.0.0d0) then
            btrcn=((xl+udotr*unp-trcpnt(1,izon))*trcpnt(4,izon)
     &            +(yl+udotr*vnp-trcpnt(2,izon))*trcpnt(5,izon)
     &            +(zl+udotr*wnp-trcpnt(3,izon))*trcpnt(6,izon))/hhtrc
            if(btrcn.ge.-trceps.and.
     &         btrcn.le.(hhtrc+trceps)*(1.0d0+trceps)) then
              itvalm=itvalm+1
              atval(itvalm)=udotr
            end if
          end if
          udotr=batrc+dsqrt(rootcy)
          if(udotr.ge.0.0d0) then
            btrcn=((xl+udotr*unp-trcpnt(1,izon))*trcpnt(4,izon)
     &            +(yl+udotr*vnp-trcpnt(2,izon))*trcpnt(5,izon)
     &            +(zl+udotr*wnp-trcpnt(3,izon))*trcpnt(6,izon))/hhtrc
            if(btrcn.ge.-trceps.and.
     &         btrcn.le.(hhtrc+trceps)*(1.0d0+trceps)) then
              itvalm=itvalm+1
              atval(itvalm)=udotr
            end if
          end if
        end if
      end if
      return
      end 
!--------------------last line of subroutine trccg1.f-------------------
                                                                        
!-------------------------------torcg1.f--------------------------------
! Version: 051219-1435
! Reference: JNC TN1410 2002-201
!-----------------------------------------------------------------------
!23456789|123456789|123456789|123456789|123456789|123456789|123456789|12

! ----------------------------------------------------------------------
! Required (geometry) subroutine for use with the EGS5 Code System
! ----------------------------------------------------------------------
! This is a subroutine torcg1. 
! ----------------------------------------------------------------------
      subroutine torcg1(izon,xl,yl,zl,unp,vnp,wnp)
      implicit none
c
      include 'auxcommons/dataconst_common.f' ! dataconst-common file
      include 'auxcommons/geom_common.f' ! geom-common file
c
      double precision xl,yl,zl
      double precision unp,vnp,wnp
      double precision xyztor,xxtor,yytor,zztor,uvwtor,a(5)
      integer izon
      double precision uxvywz,sstor
      double precision x1,x2,x3,x4,clonga(4)
      double precision thator,thstor,thetor
      double precision xlnew,ylnew,zlnew,clong1
      double precision torll1,torll2,torll3,ator,btor,
     &                 utora1,utora2,utora3
      double precision etor1,ftor1,gtor1,etor2,ftor2,gtor2,rtor1
      integer i,ifg
c
      xxtor=xl-torpnt(1,izon)
      yytor=yl-torpnt(2,izon)
      zztor=zl-torpnt(3,izon)
      if(nint(torpnt(8,izon)).eq.1) then
        uvwtor=unp
        xyztor=xxtor
      end if 
      if(nint(torpnt(8,izon)).eq.2) then
        uvwtor=vnp
        xyztor=yytor
      end if
      if(nint(torpnt(8,izon)).eq.3) then
        uvwtor=wnp
        xyztor=zztor
      end if
      uxvywz=unp*xxtor+vnp*yytor+wnp*zztor
      sstor=xxtor*xxtor+yytor*yytor+zztor*zztor
     &     -torpnt(5,izon)*torpnt(5,izon)-torpnt(4,izon)*torpnt(4,izon)
      a(1)=1.0d0
      a(2)=4.0d0*uxvywz
      a(3)=4.0d0*uxvywz*uxvywz+2.0d0*sstor
     &    +4.0d0*torpnt(4,izon)*torpnt(4,izon)*uvwtor*uvwtor
      a(4)=4.0d0*uxvywz*sstor
     &    +8.0d0*torpnt(4,izon)*torpnt(4,izon)*uvwtor*xyztor
      a(5)=sstor*sstor+4.0d0*torpnt(4,izon)*torpnt(4,izon)
     &    *(xyztor*xyztor-torpnt(5,izon)*torpnt(5,izon))
      call ferra(a,x1,x2,x3,x4,ifg)
      clonga(1)=x1
      clonga(2)=x2
      clonga(3)=x3
      clonga(4)=x4
      thstor=torpnt(6,izon)
      thetor=torpnt(7,izon)
      if(thstor.gt.180.d0) then
        thstor=thstor-360.0d0
      end if
      if(thetor.gt.180.0d0) then
        thetor=thetor-360.0d0
      end if
      thstor=thstor/180.0d0*PI
      thetor=thetor/180.0d0*PI
      do i=1,ifg
        if(clonga(i).ge.0.0d0) then
          xlnew=xl+clonga(i)*unp-torpnt(1,izon)
          ylnew=yl+clonga(i)*vnp-torpnt(2,izon)
          zlnew=zl+clonga(i)*wnp-torpnt(3,izon)
          if(nint(torpnt(8,izon)).eq.1) then
            if(zlnew.eq.0.0d0.and.ylnew.eq.0.0d0) then
              thator=0.0d0
            else
              thator=atan2(zlnew,ylnew)
            end if
          end if
          if(nint(torpnt(8,izon)).eq.2) then
            if(xlnew.eq.0.0d0.and.zlnew.eq.0.0d0) then
              thator=0.0d0
            else
              thator=atan2(xlnew,zlnew)
            end if
          end if
          if(nint(torpnt(8,izon)).eq.3) then
            if(ylnew.eq.0.0d0.and.xlnew.eq.0.0d0) then
              thator=0.0d0
            else
              thator=atan2(ylnew,xlnew)
            end if
          end if
          if(thetor.eq.thstor) then
            itvalm=itvalm+1
            atval(itvalm)=clonga(i)
          end if
          if(thetor.gt.thstor) then
            if ((thator.ge.thstor.and.thator.le.thetor)) then
              itvalm=itvalm+1
              atval(itvalm)=clonga(i)
            end if
          end if
          if(thetor.lt.thstor) then
            if ((thator.ge.thstor.or.thator.le.thetor)) then
              itvalm=itvalm+1
              atval(itvalm)=clonga(i)
            end if
          end if
        end if
      end do
      if(thetor.ne.thstor) then
        if(nint(torpnt(8,izon)).eq.1) then
          torll1=xl-torpnt(1,izon)
          torll2=yl-torpnt(2,izon)
          torll3=zl-torpnt(3,izon)
          utora1=unp
          utora2=vnp
          utora3=wnp
        end if
        if(nint(torpnt(8,izon)).eq.2) then
          torll1=yl-torpnt(2,izon)
          torll2=zl-torpnt(3,izon)
          torll3=xl-torpnt(1,izon)
          utora1=vnp
          utora2=wnp
          utora3=unp
        end if
        if(nint(torpnt(8,izon)).eq.3) then
          torll1=zl-torpnt(3,izon)
          torll2=xl-torpnt(1,izon)
          torll3=yl-torpnt(2,izon)
          utora1=wnp
          utora2=unp
          utora3=vnp
        end if
        ator=torll2*torll2+torll3*torll3
        btor=torll1*torll1
        if(torll2.eq.0.0d0.and.torll3.eq.0.0d0) then
          thator=0.0d0
        else
          thator=atan2(torll3,torll2)
        end if
        etor1=(torll2-torpnt(4,izon)*dcos(thstor))**2
     &   +(torll3-torpnt(4,izon)*dsin(thstor))**2 +torll1*torll1
        ftor1=(torll2-torpnt(4,izon)*dcos(thstor))*(-dsin(thstor))
     &       +(torll3-torpnt(4,izon)*dsin(thstor))*dcos(thstor)
        gtor1=utora2*(-dsin(thstor))+utora3*dcos(thstor)
        if((ftor1.gt.0.0d0.and.gtor1.lt.0.0d0).or.
     &     (ftor1.lt.0.0d0.and.gtor1.gt.0.0d0)) then
          clong1=-ftor1/gtor1
          rtor1=(torll2+utora2*clong1-torpnt(4,izon)*dcos(thstor))**2
     &         +(torll3+utora3*clong1-torpnt(4,izon)*dsin(thstor))**2
     &         +(torll1+utora1*clong1)*(torll1+utora1*clong1)
          if(rtor1.le.
     &      (torpnt(5,izon)*torpnt(5,izon)+toreps)*(1.0d0+toreps)) then
            itvalm=itvalm+1
            atval(itvalm)=clong1
          end if
        end if
        etor2=(torll2-torpnt(4,izon)*dcos(thetor))**2
     &       +(torll3-torpnt(4,izon)*dsin(thetor))**2 +torll1*torll1
        ftor2=(torll2-torpnt(4,izon)*dcos(thetor))*(-dsin(thetor))
     &       +(torll3-torpnt(4,izon)*dsin(thetor))*dcos(thetor)
        gtor2=utora2*(-dsin(thetor))+utora3*dcos(thetor)
        if((ftor2.gt.0.0d0.and.gtor2.lt.0.0d0).or.
     &     (ftor2.lt.0.0d0.and.gtor2.gt.0.0d0)) then
          clong1=-ftor2/gtor2
          rtor1=(torll2+utora2*clong1-torpnt(4,izon)*dcos(thetor))**2
     &         +(torll3+utora3*clong1-torpnt(4,izon)*dsin(thetor))**2
     &         +(torll1+utora1*clong1)*(torll1+utora1*clong1)
          if(rtor1.le.
     &      (torpnt(5,izon)*torpnt(5,izon)+toreps)*(1.0d0+toreps)) then
            itvalm=itvalm+1
            atval(itvalm)=clong1
          end if
        end if
      end if
      return
      end
!--------------------last line of subroutine torcg1.f-------------------
                                                                        
!-------------------------------block data cgtype-----------------------
! Version: 060117-0900
! Reference: JNC TN1410 2002-201
!-----------------------------------------------------------------------
!23456789|123456789|123456789|123456789|123456789|123456789|123456789|12

! ----------------------------------------------------------------------
! Required (geometry) subroutine for use with the EGS5 Code System
! ----------------------------------------------------------------------
! This is a block data cgtype.
! ----------------------------------------------------------------------
      block data cgtype
      include 'auxcommons/geom_common.f' ! geom-common file
      data ityknd/ 1, 2, 3, 4, 5, 6, 7, 8, 9,10,
     &            11,12,13,14/
      data cgmnst/1.0d-4/
      data cgeps1/1.0d-4/
      data cgeps2/1.0d-4/
      data rcceps/1.0d-4/
      data trceps/1.0d-4/
      data rppeps/1.0d-4/
      data spheps/1.0d-4/
      data toreps/1.0d-4/
      data elleps/1.0d-4/
      data arbeps/1.0d-4/
      data receps/1.0d-4/
      data wedeps/1.0d-4/
      data boxeps/1.0d-4/
      data hafeps/1.0d-4/
      data hexeps/1.0d-4/
      data teceps/1.0d-4/
      data geleps/1.0d-4/
      data itbody/0/
      data irppin/0/
      data isphin/0/
      data irccin/0/
      data itrcin/0/
      data itorin/0/
      data irecin/0/
      data iellin/0/
      data iwedin/0/
      data iboxin/0/
      data iarbin/0/
      data ihexin/0/
      data ihafin/0/
      data itecin/0/
      data igelin/0/
      data izonin/0/
      data itverr/0/
      data igmmax/0/
      end
!--------------------last line of block data cgtype---------------------
                                                                        
!-------------------------------stgeom.f--------------------------------
! Version: 051219-1435
! Reference: JNC TN1410 2002-201
!-----------------------------------------------------------------------
!23456789|123456789|123456789|123456789|123456789|123456789|123456789|12

! ----------------------------------------------------------------------
! Required (geometry) subroutine for use with the EGS5 Code System
! ----------------------------------------------------------------------
! This is a subroutine stgeom. 
! ----------------------------------------------------------------------
      subroutine stgeom(chkey,igmid,gmdata)
      implicit none
c
      include 'auxcommons/geom_common.f' ! geom-common file
c
      integer igmid
cssl  character chkey*3
      character chkey*(*)
      double precision gmdata(*)
      integer i
      if(chkey.eq.'RPP'.or.chkey.eq.'rpp')then
        itbody=itbody+1
        irppin=irppin+1
        if(irppin.gt.MAX_RPP) then
          write(*,*) 'Dimension over of RPP=',MAX_RPP
          stop
        endif
        nbrpp(irppin)=igmid
        irppuse(irppin)=0
        igmmax = max(igmmax,igmid)
        if(igmmax.gt.MAX_GEOM) then
          write(*,*) 'Dimension over of GEOM=',MAX_GEOM
          stop
        endif
        do i=1,6
          rpppnt(i,irppin)=gmdata(i)
        end do
        itblty(igmid)=ityknd(1)
        itblno(igmid)=irppin
        call rppset(irppin)
      elseif(chkey.eq.'SPH'.or.chkey.eq.'sph') then
        itbody=itbody+1
        isphin=isphin+1
        if(isphin.gt.MAX_SPH) then
          write(*,*) 'Dimension over of SPH=',MAX_SPH
          stop
        endif
        nbsph(isphin)=igmid
        isphuse(isphin)=0
        igmmax = max(igmmax,igmid)
        if(igmmax.gt.MAX_GEOM) then
          write(*,*) 'Dimension over of GEOM=',MAX_GEOM
          stop
        endif
        do i=1,4
          sphpnt(i,isphin)=gmdata(i)
        end do
        itblty(igmid)=ityknd(2)
        itblno(igmid)=isphin
      elseif(chkey.eq.'RCC'.or.chkey.eq.'rcc') then
        itbody=itbody+1
        irccin=irccin+1
        if(irccin.gt.MAX_RCC) then
          write(*,*) 'Dimension over of RCC=',MAX_RCC
          stop
        endif
        nbrcc(irccin)=igmid
        irccuse(irccin)=0
        igmmax = max(igmmax,igmid)
        if(igmmax.gt.MAX_GEOM) then
          write(*,*) 'Dimension over of GEOM=',MAX_GEOM
          stop
        endif
        do i=1,7
          rccpnt(i,irccin)=gmdata(i)
        end do
        itblty(igmid)=ityknd(3)
        itblno(igmid)=irccin
      elseif(chkey.eq.'TRC'.or.chkey.eq.'trc') then
        itbody=itbody+1
        itrcin=itrcin+1
        if(itrcin.gt.MAX_TRC) then
          write(*,*) 'Dimension over of TRC=',MAX_TRC
          stop
        endif
        nbtrc(itrcin)=igmid
        itrcuse(itrcin)=0
        igmmax = max(igmmax,igmid)
        if(igmmax.gt.MAX_GEOM) then
          write(*,*) 'Dimension over of GEOM=',MAX_GEOM
          stop
        endif
        do i=1,8
          trcpnt(i,itrcin)=gmdata(i)
        end do
        itblty(igmid)=ityknd(4)
        itblno(igmid)=itrcin
      elseif(chkey.eq.'TOR'.or.chkey.eq.'tor') then
        itbody=itbody+1
        itorin=itorin+1
        if(itorin.gt.MAX_TOR) then
          write(*,*) 'Dimension over of TOR=',MAX_TOR
          stop
        endif
        nbtor(itorin)=igmid
        itoruse(itorin)=0
        igmmax = max(igmmax,igmid)
        if(igmmax.gt.MAX_GEOM) then
          write(*,*) 'Dimension over of GEOM=',MAX_GEOM
          stop
        endif
        do i=1,8
          torpnt(i,itorin)=gmdata(i)
        end do
        itblty(igmid)=ityknd(5)
        itblno(igmid)=itorin
      elseif(chkey.eq.'REC'.or.chkey.eq.'rec') then
        itbody=itbody+1
        irecin=irecin+1
        if(irecin.gt.MAX_REC) then
          write(*,*) 'Dimension over of REC=',MAX_REC
          stop
        endif
        nbrec(irecin)=igmid
        irecuse(irecin)=0
        igmmax = max(igmmax,igmid)
        if(igmmax.gt.MAX_GEOM) then
          write(*,*) 'Dimension over of GEOM=',MAX_GEOM
          stop
        endif
        do i=1,12
          recpnt(i,irecin)=gmdata(i)
        end do
        itblty(igmid)=ityknd(6)
        itblno(igmid)=irecin
        call recset(irecin)
      elseif(chkey.eq.'ELL'.or.chkey.eq.'ell') then
        itbody=itbody+1
        iellin=iellin+1
        if(iellin.gt.MAX_ELL) then
          write(*,*) 'Dimension over of ELL=',MAX_ELL
          stop
        endif
        nbell(iellin)=igmid
        ielluse(iellin)=0
        igmmax = max(igmmax,igmid)
        if(igmmax.gt.MAX_GEOM) then
          write(*,*) 'Dimension over of GEOM=',MAX_GEOM
          stop
        endif
        do i=1,7
          ellpnt(i,iellin)=gmdata(i)
        end do
        itblty(igmid)=ityknd(7)
        itblno(igmid)=iellin
        call ellset(iellin)
      elseif(chkey.eq.'WED'.or.chkey.eq.'wed') then
        itbody=itbody+1
        iwedin=iwedin+1
        if(iwedin.gt.MAX_WED) then
          write(*,*) 'Dimension over of WED=',MAX_WED
          stop
        endif
        nbwed(iwedin)=igmid
        iweduse(iwedin)=0
        igmmax = max(igmmax,igmid)
        if(igmmax.gt.MAX_GEOM) then
          write(*,*) 'Dimension over of GEOM=',MAX_GEOM
          stop
        endif
        do i=1,12
          wedpnt(i,iwedin)=gmdata(i)
        end do
        itblty(igmid)=ityknd(8)
        itblno(igmid)=iwedin
        call wedset(iwedin)
      elseif(chkey.eq.'BOX'.or.chkey.eq.'box') then
        itbody=itbody+1
        iboxin=iboxin+1
        if(iboxin.gt.MAX_BOX) then
          write(*,*) 'Dimension over of BOX=',MAX_BOX
          stop
        endif
        nbbox(iboxin)=igmid
        iboxuse(iboxin)=0
        igmmax = max(igmmax,igmid)
        if(igmmax.gt.MAX_GEOM) then
          write(*,*) 'Dimension over of GEOM=',MAX_GEOM
          stop
        endif
        do i=1,12
          boxpnt(i,iboxin)=gmdata(i)
        end do
        itblty(igmid)=ityknd(9)
        itblno(igmid)=iboxin
        call boxset(iboxin)
      elseif(chkey.eq.'ARB'.or.chkey.eq.'arb') then
        itbody=itbody+1
        iarbin=iarbin+1
        if(iarbin.gt.MAX_ARB) then
          write(*,*) 'Dimension over of ARB=',MAX_ARB
          stop
        endif
        nbarb(iarbin)=igmid
        iarbuse(iarbin)=0
        igmmax = max(igmmax,igmid)
        if(igmmax.gt.MAX_GEOM) then
          write(*,*) 'Dimension over of GEOM=',MAX_GEOM
          stop
        endif
        do i=1,30
          arbpnt(i,iarbin)=gmdata(i)
        end do
        itblty(igmid)=ityknd(10)
        itblno(igmid)=iarbin
        call arbset(iarbin)
      elseif(chkey.eq.'HEX'.or.chkey.eq.'hex') then
        itbody=itbody+1
        ihexin=ihexin+1
        if(ihexin.gt.MAX_HEX) then
          write(*,*) 'Dimension over of HEX=',MAX_HEX
          stop
        endif
        nbhex(ihexin)=igmid
        ihexuse(ihexin)=0
        igmmax = max(igmmax,igmid)
        if(igmmax.gt.MAX_GEOM) then
          write(*,*) 'Dimension over of GEOM=',MAX_GEOM
          stop
        endif
        do i=1,10
          hexpnt(i,ihexin)=gmdata(i)
        end do
        itblty(igmid)=ityknd(11)
        itblno(igmid)=ihexin
        call hexset(ihexin)
      elseif(chkey.eq.'HAF'.or.chkey.eq.'haf') then
        itbody=itbody+1
        ihafin=ihafin+1
        if(ihafin.gt.MAX_HAF) then
          write(*,*) 'Dimension over of HAF=',MAX_HAF
          stop
        endif
        nbhaf(ihafin)=igmid
        ihafuse(ihafin)=0
        igmmax = max(igmmax,igmid)
        if(igmmax.gt.MAX_GEOM) then
          write(*,*) 'Dimension over of GEOM=',MAX_GEOM
          stop
        endif
        do i=1,4
          hafpnt(i,ihafin)=gmdata(i)
        end do
        itblty(igmid)=ityknd(12)
        itblno(igmid)=ihafin
        call hafset(ihafin)
      elseif(chkey.eq.'TEC'.or.chkey.eq.'tec') then
        itbody=itbody+1
        itecin=itecin+1
        if(itecin.gt.MAX_TEC) then
          write(*,*) 'Dimension over of TEC=',MAX_TEC
          stop
        endif
        nbtec(itecin)=igmid
        itecuse(itecin)=0
        igmmax = max(igmmax,igmid)
        if(igmmax.gt.MAX_GEOM) then
          write(*,*) 'Dimension over of GEOM=',MAX_GEOM
          stop
        endif
        do i=1,13
          tecpnt(i,itecin)=gmdata(i)
        end do
        itblty(igmid)=ityknd(13)
        itblno(igmid)=itecin
        call tecset(itecin)
      elseif(chkey.eq.'GEL'.or.chkey.eq.'gel') then
        itbody=itbody+1
        igelin=igelin+1
        if(igelin.gt.MAX_GEL) then
          write(*,*) 'Dimension over of GEL=',MAX_GEL
          stop
        endif
        nbgel(igelin)=igmid
        igeluse(igelin)=0
        igmmax = max(igmmax,igmid)
        if(igmmax.gt.MAX_GEOM) then
          write(*,*) 'Dimension over of GEOM=',MAX_GEOM
          stop
        endif
        do i=1,12
          gelpnt(i,igelin)=gmdata(i)
        end do
        itblty(igmid)=ityknd(14)
        itblno(igmid)=igelin
        call gelset(igelin)
      endif
      return
      end
!--------------------last line of subroutine stgeom.f------------------
                                                                        
!-------------------------------stzone.f--------------------------------
! Version: 051219-1435
! Reference: JNC TN1410 2002-201
!-----------------------------------------------------------------------
!23456789|123456789|123456789|123456789|123456789|123456789|123456789|12

! ----------------------------------------------------------------------
! Required (geometry) subroutine for use with the EGS5 Code System
! ----------------------------------------------------------------------
! This is a subroutine stzone. 
! ----------------------------------------------------------------------
      subroutine stzone(chkey,iznid,iznor,izndt,iprm)
      implicit none
c
      include 'auxcommons/geom_common.f' ! geom-common file
c
      integer iznid,iprm
cssl  character chkey*3
      character chkey*(*)
      character iznor(MAX_IZN)*2
      integer izndt(MAX_IZN)
      integer i,j
      izonin = izonin + 1
      if(izonin.gt.MAX_ZONE) then
        write(*,*) 'Dimension over of ZONE=',MAX_ZONE
        stop
      endif
      zoneid(izonin) = chkey
      nbbody(izonin) = iprm
      if(iprm.gt.MAX_BODY) then
        write(*,*) 'Dimension over of BODY=',MAX_BODY
        stop
      endif
      do i=1,iprm
        zoneor(i,izonin) = iznor(i)
        if(iznor(i).eq.'or'.or.iznor(i).eq.'OR') then
          j=j+1
        endif
        nbzone(i,izonin) = izndt(i)
        call zonegeom(abs(izndt(i)))
      end do
      iorcnt(izonin)=j
      return
      end
!--------------------last line of subroutine stzone.f-------------------

!-------------------------------zonegeom.f------------------------------
! Version: 051219-1435
! Reference: JNC TN1410 2002-201
!-----------------------------------------------------------------------
!23456789|123456789|123456789|123456789|123456789|123456789|123456789|12

! ----------------------------------------------------------------------
! Required (geometry) subroutine for use with the EGS5 Code System
! ----------------------------------------------------------------------
! This is a subroutine zonegeom
! ----------------------------------------------------------------------
      subroutine zonegeom(izndt)
      implicit none
c
      include 'auxcommons/geom_common.f' ! geom-common file
      integer izndt,i
c
      do i=1,irppin
        if(nbrpp(i).eq.izndt) then
          irppuse(i)=1
          goto 190
        endif
      end do
      do i=1,isphin
        if(nbsph(i).eq.izndt) then
          isphuse(i)=1
          goto 190
        endif
      end do
      do i=1,irccin
        if(nbrcc(i).eq.izndt) then
          irccuse(i)=1
          goto 190
        endif
      end do
      do i=1,itrcin
        if(nbtrc(i).eq.izndt) then
          itrcuse(i)=1
          goto 190
        endif
      end do
      do i=1,itorin
        if(nbtor(i).eq.izndt) then
          itoruse(i)=1
          goto 190
        endif
      end do
      do i=1,irecin
        if(nbrec(i).eq.izndt) then
          irecuse(i)=1
          goto 190
        endif
      end do
      do i=1,iellin
        if(nbell(i).eq.izndt) then
          ielluse(i)=1
          goto 190
        endif
      end do
      do i=1,iwedin
        if(nbwed(i).eq.izndt) then
          iweduse(i)=1
          goto 190
        endif
      end do
      do i=1,iboxin
        if(nbbox(i).eq.izndt) then
          iboxuse(i)=1
          goto 190
        endif
      end do
      do i=1,iarbin
        if(nbarb(i).eq.izndt) then
          iarbuse(i)=1
          goto 190
        endif
      end do
      do i=1,ihexin
        if(nbhex(i).eq.izndt) then
          ihexuse(i)=1
          goto 190
        endif
      end do
      do i=1,ihafin
        if(nbhaf(i).eq.izndt) then
          ihafuse(i)=1
          goto 190
        endif
      end do
      do i=1,itecin
        if(nbtec(i).eq.izndt) then
          itecuse(i)=1
          goto 190
        endif
      end do
      do i=1,igelin
        if(nbgel(i).eq.izndt) then
          igeluse(i)=1
          goto 190
        endif
      end do
  190 continue
      return
      end
!--------------------last line of subroutine zonegeom.f-----------------
                                                                        
!-------------------------------geomgt.f--------------------------------
! Version: 051219-1435
! Reference: JNC TN1410 2002-201
!-----------------------------------------------------------------------
!23456789|123456789|123456789|123456789|123456789|123456789|123456789|12

! ----------------------------------------------------------------------
! Required (geometry) subroutine for use with the EGS5 Code System
! ----------------------------------------------------------------------
! This is a subroutine geomgt. 
! ----------------------------------------------------------------------
      subroutine geomgt(ifti,ifto)
      implicit none
      include 'auxcommons/geom_common.f' ! geom-common file
      include 'include/egs5_h.f'
      integer ifti,ifto
cssl  character chkey*3
      character chkey*10
      double precision gmdata(50)
      character iznor(MAX_IZN)*2
      integer izndt(MAX_IZN)
      integer ioptid
      data ioptid/0/
      integer i,j,jps,jpe,iznid,igmid,iprm
      itbody=0
      irppin=0
      isphin=0
      irccin=0
      itorin=0
      itrcin=0
      irecin=0
      iellin=0
      iwedin=0
      iboxin=0
      iarbin=0
      ihexin=0
      ihafin=0
      itecin=0
      igelin=0
      izonin=0
      itverr=0
      igmmax=0
100   continue
      call freegm(ifti,ifto,chkey,igmid,gmdata,iprm)
      if((chkey.eq.'   '))then
        write(ifto,'(a)') ' geom data error : not found [ end ]'
        goto 900
      elseif(chkey.eq.'END'.or.chkey.eq.'end') then
        write(ifto,6000) chkey
6000  format(1h ,2x,a10,i10,1p8e12.4)
        goto 200
      else
        do i=1,iprm,8
          jps = i
          jpe = min(iprm,i+7)
          if(i.eq.1) then
            write(ifto,6000) chkey,igmid,(gmdata(j),j=jps,jpe)
          else
            write(ifto,6050) (gmdata(j),j=jps,jpe)
6050  format(1h ,22x,1p8e12.4)
          endif
        end do
        call stgeom(chkey,igmid,gmdata)
      endif
      goto 100
200   continue
      call freezn(ifti,ifto,chkey,iznid,iznor,izndt,iprm,ioptid,
     &            MAX_IZN)
      if((chkey.eq.'   '))then
        write(ifto,'(a)') ' zone data error : not found [ end ]'
        goto 900
      elseif(chkey.eq.'END'.or.chkey.eq.'end') then
        write(ifto,6000) chkey
        goto 300
      else
        do i=1,iprm,10
          jps = i
          jpe = min(iprm,i+9)
          if((i.eq.1))then
            if((ioptid.eq.0))then
              write(ifto,6200) chkey,(iznor(j),izndt(j),j=jps,jpe)
6200  format(1h ,2x,a10,5x,1x,10(a2,i8,1x))
            else
              write(ifto,6250) chkey,iznid,(iznor(j),izndt(j),
     &                                                     j=jps,jpe)
6250  format(1h ,2x,a10,i5,1x,10(a2,i8,1x))
            endif
          else
            write(ifto,6300) (iznor(j),izndt(j),j=jps,jpe)
6300  format(1h ,17x ,1x,10(a2,i8,1x))
          endif
        end do
        call stzone(chkey,iznid,iznor,izndt,iprm)
      endif
      goto 200
900   continue
      stop
300   continue
c
      call setnxt                      
c
      !  Check nreg=izonin value
      if (izonin.gt.mxreg) then
        write(1,140) izonin,mxreg 
140     FORMAT(' nreg(=',I12,') must be less than MXREG(=',I12,')' /
     *  ' You must change MXREG in include/egs5_h.f.')
        stop
      end if

      return
      end
!--------------------last line of subroutine geomgt.f------------------
                                                                        
!-------------------------------setnxt.f--------------------------------
! Version: 051219-1435
! Reference: JNC TN1410 2002-201
!-----------------------------------------------------------------------
!23456789|123456789|123456789|123456789|123456789|123456789|123456789|12

! ----------------------------------------------------------------------
! Required (geometry) subroutine for use with the EGS5 Code System
! ----------------------------------------------------------------------
! This is a subroutine setnxt.
! ----------------------------------------------------------------------
      subroutine setnxt
      implicit none
c
      include 'auxcommons/geom_common.f' ! geom-common file
      integer i,j,l,jj,l1,l2,ll
c
      l=0
      do i=1,3
        iznnxs(i,1)=0
        iznnxs(i,2)=0
        do j=1,izonin
          iznnxt(i,j,l)=j
          iznnxc(i,j,l)=0
        enddo
      enddo
      do i=1,3
        do j=1,izonin
          iznnxp(i,iznnxt(i,j,l),l)=j
        enddo
c       write(*,*) 'i,izonnxp=',i,(iznnxp(i,j,l),j=1,izonin)
      enddo
      do l=1,izonin
        l1=l-1
        l2=izonin-l
        ll=min(l1,l2)
        do j=1,izonin
          if(j.eq.l) then
c           jj=izonin
            jj=0
          elseif(iabs(j-l).gt.ll) then
            jj=iabs(j-l)+ll
          elseif(j.lt.l) then
           jj=2*(l-j)
          else
           jj=2*(j-l)-1
          endif
          do i=1,3
c           iznnxt(i,j,l)=jj
            iznnxt(i,j,l)=jj+1
            iznnxc(i,j,l)=0
          enddo
        enddo
      enddo
      do l=1,izonin
        do j=1,izonin
          do i=1,3
            iznnxp(i,iznnxt(i,j,l),l)=j
          enddo
        enddo
        do i=1,3
c         write(*,*) 'i,izonnxp=',i,(iznnxp(i,j,l),j=1,izonin)
        enddo
      enddo
      return
      end
!--------------------last line of subroutine setnxt.f------------------

!-------------------------------rstnxt.f--------------------------------
! Version: 051219-1435
! Reference: JNC TN1410 2002-201
!-----------------------------------------------------------------------
!23456789|123456789|123456789|123456789|123456789|123456789|123456789|12

! ----------------------------------------------------------------------
! Required (geometry) subroutine for use with the EGS5 Code System
! ----------------------------------------------------------------------
! This is a subroutine rstnxt.
! ----------------------------------------------------------------------
      subroutine rstnxt(iray,irold,irnew)                     
      implicit none
c
      include 'auxcommons/geom_common.f' ! geom-common file
      integer iray,irold,irnew
      integer i,j,ll,n,ns,ne,jj
      integer icknxt,ickmin,id,ik,k
      data icknxt/100000000/
      data ickmin/1000/
      integer itbl(max_zone),itbl1
c
   
      if(irold.eq.0) then
        ll=1
        ns=0
        ne=0
      else
        ll=2
        ns=1
        ne=izonin
      endif   
      if(iznnxs(iray,ll).lt.icknxt) then
        iznnxs(iray,ll)=iznnxs(iray,ll)+1
        iznnxc(iray,irnew,irold)=iznnxc(iray,irnew,irold)+1
        if(mod(iznnxs(iray,ll),ickmin).eq.0) then
          ik=0
          k=1
  100     continue
          id=k*ickmin
          if(iznnxs(iray,ll).eq.id) then
            ik=1
          elseif(id.lt.icknxt) then
            k=k*10
            goto 100
          endif
c
          if(ik.eq.1) then
            do n=ns,ne
              if(ll.eq.1) then
                do j=1,izonin
                  itbl(j)=j
                enddo
              else
                itbl(1)=n
                jj=0
                do j=2,izonin
                   jj=jj+1
                  if(jj.eq.n) then
                    jj=jj+1
                   endif
                   itbl(j)=jj
                enddo
              endif
              do i=1,izonin-1
                do j=i+1,izonin
                  if(ll.eq.1.or.i.ne.1) then
                    if(iznnxc(iray,itbl(j),n).gt.
     &                 iznnxc(iray,itbl(i),n)) then
                       itbl1=itbl(j)
                       itbl(j)=itbl(i)
                       itbl(i)=itbl1
                    elseif(iznnxc(iray,itbl(j),n).eq.
     &                     iznnxc(iray,itbl(i),n)) then
                      if(iznnxt(iray,itbl(j),n).lt.
     &                   iznnxt(iray,itbl(i),n)) then
                        itbl1=itbl(j)
                        itbl(j)=itbl(i)
                        itbl(i)=itbl1
                      endif
                    endif
                  endif
                enddo
              enddo
              do j=1,izonin
                iznnxt(iray,itbl(j),n)=j
              enddo
              do j=1,izonin
                iznnxp(iray,iznnxt(iray,j,n),n)=j
              enddo
c             write(*,*) 'iray,ir,iznnxt=',
c    &                    iray,n,(iznnxt(iray,j,n),j=1,izonin)
c             write(*,*) 'iray,ir,iznnxp=',
c    &                    iray,n,(iznnxp(iray,j,n),j=1,izonin)
c             write(*,*) 'iray,ir,iznnxc=',
c    &                    iray,n,(iznnxc(iray,j,n),j=1,izonin)
            enddo
          endif
        endif
      endif
      return
      end
!--------------------last line of subroutine rstnxt.f------------------
                                                                        
!-------------------------------getnxt.f--------------------------------
! Version: 051219-1435
! Reference: JNC TN1410 2002-201
!-----------------------------------------------------------------------
!23456789|123456789|123456789|123456789|123456789|123456789|123456789|12

! ----------------------------------------------------------------------
! Required (geometry) subroutine for use with the EGS5 Code System
! ----------------------------------------------------------------------
! This is a subroutine getnxt.
! ----------------------------------------------------------------------
      subroutine getnxt(iray,jj,irnow,inext)                     
      implicit none
      include 'auxcommons/geom_common.f' ! geom-common file
      integer iray,jj,irnow,inext
      inext=iznnxp(iray,jj,irnow)
      return
      end
!--------------------last line of subroutine gettxt.f------------------
                                                                        
!-------------------------------freegm.f--------------------------------
! Version: 051219-1435
! Reference: JNC TN1410 2002-201
!-----------------------------------------------------------------------
!23456789|123456789|123456789|123456789|123456789|123456789|123456789|12

! ----------------------------------------------------------------------
! Required (geometry) subroutine for use with the EGS5 Code System
! ----------------------------------------------------------------------
! This is a subroutine freegm. 
! ----------------------------------------------------------------------
      subroutine freegm(ifti,ifto,chkey,igmid,gmdata,iprm)
      implicit none
      integer ifti,ifto,igmid,iprm
cssl  character chkey*3
      character chkey*(*)
      double precision gmdata(*)
      character chin*250
      integer istbl(200),ietbl(200)
      character gmtyps(15)*3,gmtypl(15)*3
      integer ityppm(15)
      integer igmtyp
      data igmtyp/15/
      data gmtypl/'END','RPP','SPH','RCC','TRC','TOR',
     &            'REC','ELL','WED','BOX','ARB','HEX',
     &            'HAF','TEC','GEL'                 /
      data gmtyps/'end','rpp','sph','rcc','trc','tor',
     &            'rec','ell','wed','box','arb','hex',
     &            'haf','tec','gel'                  /
      data ityppm/ 0, 6, 4, 7, 8, 8,
     &            12, 7,12,12,30,10,
     &             4,13,12         /
      integer i,j,ino,ich,inonw,ichs,iche,inost,ikey,ichkfg
      inonw = 0
      chkey = '   '
      iprm = 0
      ichs = 1
      iche = 250
100   continue
      read(ifti,'(a)',end=990) chin
      call splitc(chin,ich,ichs,iche,ino,istbl,ietbl)
      if((ino.eq.0))goto 100
      if(((ietbl(1)-istbl(1)).ne.2))goto 990
      chkey = chin(istbl(1):ietbl(1))
      inost = 2
      do i=1,igmtyp
        if(chkey.eq.gmtypl(i).or.chkey.eq.gmtyps(i))then
          ikey = i
          iprm = ityppm(i)
          goto 220
        endif
      end do
      goto 990
220   continue
      if((ikey.eq.1))goto 900
      if((ino.ne.1))goto 400
300   continue
      read(ifti,'(a)',end=990) chin
      call splitc(chin,ich,ichs,iche,ino,istbl,ietbl)
      if((ino.eq.0))goto 300
      inost = 1
400   continue
      if((inonw.eq.0))then
      j = ietbl(inost)-istbl(inost)+1
      call chkint(chin(istbl(inost):ietbl(inost)),j,ichkfg)
      if((ichkfg.ne.0))goto 990
      read(chin(istbl(inost):ietbl(inost)),*) igmid
      inost = inost + 1
      endif
      do i=inost,ino
        inonw = inonw + 1
        if((inonw.gt.ityppm(ikey)))goto 990
        j = ietbl(i)-istbl(i)+1
        call chkflt(chin(istbl(i):ietbl(i)),j,ichkfg)
        if((ichkfg.ne.0))goto 990
        read(chin(istbl(i):ietbl(i)),*) gmdata(inonw)
      end do
      if((inonw.lt.ityppm(ikey)))goto 300
900   continue
      return
990   continue
      write(ifto,'(a,a)') ' geom data error : ',chkey
      write(ifto,'(a)') chin
      stop
      end
!--------------------last line of subroutine freegm.f------------------
                                                                        
!-------------------------------freezn.f--------------------------------
! Version: 051219-1435
! Reference: JNC TN1410 2002-201
!-----------------------------------------------------------------------
!23456789|123456789|123456789|123456789|123456789|123456789|123456789|12

! ----------------------------------------------------------------------
! Required (geometry) subroutine for use with the EGS5 Code System
! ----------------------------------------------------------------------
! This is a subroutine freezn. 
! ----------------------------------------------------------------------
      subroutine freezn(ifti,ifto,chkey,iznid,iznor,izndt,iprm,ioptid,
     &                  MAX_IZN)
      implicit none
      integer ifti,ifto,iznid,iprm,ioptid
      integer  MAX_IZN
cssl  character chkey*3,iznor(*)*2
      character chkey*(*),iznor(*)*2
      integer izndt(*)
      character chin*250
      integer istbl(200),ietbl(200)
      character zntypl(2)*3,zntyps(2)*3
      integer izntyp
      data izntyp/2/
      data zntypl/'END','ZON'/
      data zntyps/'end','zon'/
      integer i,j,inonw,ichs,iche,inost,ikey,
     &        ino,ich,ichkfg
      inonw = 0
      chkey = '   '
      iprm = 0
      ichs = 1
      iche = 250
      iznor(1) = '  '
100   continue
      read(ifti,'(a)',end=990) chin
      call splitc(chin,ich,ichs,iche,ino,istbl,ietbl)
      if((ino.eq.0))goto 100
      j = ietbl(1)-istbl(1)+1
      call chkint(chin(istbl(1):ietbl(1)),j,ichkfg)
      if((ichkfg.eq.0))goto 990
      inost = 2
      ikey = 0
cssl  if(((ietbl(1)-istbl(1)).gt.3))goto 990
      if(((ietbl(1)-istbl(1)).gt.10))goto 990
      chkey = chin(istbl(1):ietbl(1))
      if(((ietbl(1)-istbl(1)).ne.2))goto 220
      do i=1,izntyp
        if(chkey.eq.zntypl(i).or.chkey.eq.zntyps(i))then
          ikey = i
          goto 220
        endif
      end do
220   continue
      if((ikey.eq.1))goto 900
      if((ino.ne.1))goto 400
300   continue
      read(ifti,'(a)',end=990) chin
      call splitc(chin,ich,ichs,iche,ino,istbl,ietbl)
      if((ino.eq.0))goto 300
      j = ietbl(1)-istbl(1)+1
      call chkint(chin(istbl(1):ietbl(1)),j,ichkfg)
      if((ichkfg.ne.0))then
      if((j.eq.2 .and.((chin(istbl(1):ietbl(1)).eq.'OR') .or.
     &   (chin(istbl(1):ietbl(1)).eq.'or'))))then
      else
      backspace (ifti)
      goto 900
      endif
      endif
      inost = 1
400   continue
      if((inonw.eq.0.and.ioptid.eq.1))then
      j = ietbl(inost)-istbl(inost)+1
      call chkint(chin(istbl(inost):ietbl(inost)),j,ichkfg)
      if((ichkfg.ne.0))goto 990
      read(chin(istbl(inost):ietbl(inost)),*) iznid
      inost = inost + 1
      endif
      do i=inost,ino
        j = ietbl(i)-istbl(i)+1
        call chkint(chin(istbl(i):ietbl(i)),j,ichkfg)
        if((ichkfg.eq.0))then
          inonw = inonw + 1
          if(inonw.ge.MAX_IZN) then
            write(*,*) 'Dimension over of izndt=',MAX_IZN
            stop
          endif
          read(chin(istbl(i):ietbl(i)),*) izndt(inonw)
          iznor(inonw+1) = '  '
        elseif(j.eq.2 .and.((chin(istbl(i):ietbl(i)).eq.'OR') .or.
     &       (chin(istbl(i):ietbl(i)).eq.'or'))) then
          inonw = inonw + 1
          if((inonw.eq.1))goto 990
          if((iznor(inonw).eq.'OR'.or.iznor(inonw).eq.'or'))goto 990
          iznor(inonw) = chin(istbl(i):ietbl(i))
          inonw = inonw - 1
        else
          goto 990
        endif
      end do
      goto 300
900   continue
      iprm = inonw
      return
990   continue
      write(ifto,'(a,a)') ' zone data error : ',chkey
      write(ifto,'(a)') chin
      stop
      end
!--------------------last line of subroutine freezn.f------------------
                                                                        
!-------------------------------splitc.f--------------------------------
! Version: 051219-1435
! Reference: JNC TN1410 2002-201
!-----------------------------------------------------------------------
!23456789|123456789|123456789|123456789|123456789|123456789|123456789|12

! ----------------------------------------------------------------------
! Required (geometry) subroutine for use with the EGS5 Code System
! ----------------------------------------------------------------------
! This is a subroutine splitc. 
! ----------------------------------------------------------------------
      subroutine splitc(chin,ich,ichs,iche,ino,istbl,ietbl)
      implicit none
      character chin*(*)
      integer i,ich,ichs,iche,ino
      integer istbl(*),ietbl(*)
      integer ichss,ichee,is,ie
      do i=ichs,iche
        if (chin(i:i).lt.' ') then
          chin(i:i)=' '
        end if
      end do
      ichss = ichs
      if((ichss.eq.0))then
        ichss = 1
      endif
      ichee = iche
      if((ichee.eq.0))then
        ichee = ich
      endif
      ino = 0
      is = 0
      ie = 0
      do i=ichss,ichee
cssl    if((is.eq.0.and.chin(i:i).ne.' '))then
        if((is.eq.0.and.
     &   (chin(i:i).ne.' '.and.chin(i:i).ne.',')))then
          is = i
          ie = i
cssl    elseif(chin(i:i).ne.' ') then
        elseif(chin(i:i).ne.' '.and.chin(i:i).ne.',') then
          ie = i
        elseif(is.ne.0) then
          ino = ino + 1
          istbl(ino) = is
          ietbl(ino) = ie
          is = 0
          ie = 0
        endif
        if((i.eq.ichee.and.is.ne.0))then
         ino = ino + 1
         istbl(ino) = is
         ietbl(ino) = ie
         is = 0
         ie = 0
        endif
      end do
      return
      end
!--------------------last line of subroutine splitc.f------------------
                                                                        
!-------------------------------chkint.f--------------------------------
! Version: 051219-1435
! Reference: JNC TN1410 2002-201
!-----------------------------------------------------------------------
!23456789|123456789|123456789|123456789|123456789|123456789|123456789|12

! ----------------------------------------------------------------------
! Required (geometry) subroutine for use with the EGS5 Code System
! ----------------------------------------------------------------------
! This is a subroutine chkint. 
! ----------------------------------------------------------------------
      subroutine chkint(chin,ich,ichkfg)
      implicit none
      character chin*(*)
      integer ich,ichkfg
      integer i
      ichkfg = 0
      do i=1,ich
        if((i.eq.1.and.(chin(i:i).eq.'+'.or.chin(i:i).eq.'-')))then
        elseif(chin(i:i).ge.'0'.and.chin(i:i).le.'9') then
        else
          ichkfg = 1
        endif
      end do
      return
      end
!--------------------last line of subroutine chkint.f------------------
                                                                        
!-------------------------------chkflt.f--------------------------------
! Version: 051219-1435
! Reference: JNC TN1410 2002-201
!-----------------------------------------------------------------------
!23456789|123456789|123456789|123456789|123456789|123456789|123456789|12

! ----------------------------------------------------------------------
! Required (geometry) subroutine for use with the EGS5 Code System
! ----------------------------------------------------------------------
! This is a subroutine chkflt. 
! ----------------------------------------------------------------------
      subroutine chkflt(chin,ich,ichkfg)
      implicit none
      character chin*(*)
      integer ich,ichkfg
      integer i,ie,idot
      ichkfg = 0
      idot = 0
      ie = 0
      do i=1,ich
        if((i.eq.1.and.(chin(i:i).eq.'+'.or.chin(i:i).eq.'-')))then
        elseif(ie.eq.0.and.(chin(i:i).eq.'E'.or.chin(i:i).eq.'e')) then
          ie = 1
        elseif(ie.eq.0.and.(chin(i:i).eq.'D'.or.chin(i:i).eq.'d')) then
          ie = 1
        elseif(ie.eq.1.and.(chin(i:i).eq.'+'.or.chin(i:i).eq.'-')) then
          ie =2
        elseif(idot.eq.0.and.chin(i:i).eq.'.') then
          idot = 1
        elseif(chin(i:i).ge.'0'.and.chin(i:i).le.'9') then
          if((ie.eq.1))ie = 2
        else
         ichkfg = 1
        endif
      end do
      return
      end
!--------------------last line of subroutine chkflt.f------------------

!-------------------------------ellset.f--------------------------------
! Version: 051219-1435
! Reference: JNC TN1410 2002-201
!-----------------------------------------------------------------------
!23456789|123456789|123456789|123456789|123456789|123456789|123456789|12

! ----------------------------------------------------------------------
! Required (geometry) subroutine for use with the EGS5 Code System
! ----------------------------------------------------------------------
! This is a subroutine ellset.
      subroutine ellset(izon)
      implicit none
c
      include 'auxcommons/geom_common.f' ! geom-common file
c
      integer izon

      double precision chk1

      chk1 = sqrt(
     &       (ellpnt(4,izon)-ellpnt(1,izon))
     &     * (ellpnt(4,izon)-ellpnt(1,izon))
     &     + (ellpnt(5,izon)-ellpnt(2,izon))
     &     * (ellpnt(5,izon)-ellpnt(2,izon))
     &     + (ellpnt(6,izon)-ellpnt(3,izon))
     &     * (ellpnt(6,izon)-ellpnt(3,izon)))
      if(ellpnt(7,izon).le.chk1) then
        write(*,*) 'Error of ELL ',nbell(izon),' : Radius is short'
        stop
      end if
      return
      end
!--------------------last line of subroutine ellset.f------------------

!-------------------------------ellcg1.f--------------------------------
! Version: 051219-1435
! Reference: JNC TN1410 2002-201
!-----------------------------------------------------------------------
!23456789|123456789|123456789|123456789|123456789|123456789|123456789|12

! ----------------------------------------------------------------------
! Required (geometry) subroutine for use with the EGS5 Code System
! ----------------------------------------------------------------------
! This is a subroutine ellcg1.
! ----------------------------------------------------------------------
      subroutine ellcg1(izon,xl,yl,zl,unp,vnp,wnp)
      implicit none
c
      include 'auxcommons/geom_common.f' ! geom-common file
c
      double precision xl,yl,zl
      double precision unp,vnp,wnp
      integer izon
      double precision a,a1,a2,b,b1,b2,c,u
      double precision ald,alu,rin,rout
      rin = dlnmax
      rout=-dlnmax
      a1=(xl-ellpnt(1,izon))*unp
     &  +(yl-ellpnt(2,izon))*vnp
     &  +(zl-ellpnt(3,izon))*wnp
      b1=(xl-ellpnt(1,izon))*(xl-ellpnt(1,izon))
     &  +(yl-ellpnt(2,izon))*(yl-ellpnt(2,izon))
     &  +(zl-ellpnt(3,izon))*(zl-ellpnt(3,izon))
      a2=(xl-ellpnt(4,izon))*unp
     &  +(yl-ellpnt(5,izon))*vnp
     &  +(zl-ellpnt(6,izon))*wnp
      b2=(xl-ellpnt(4,izon))*(xl-ellpnt(4,izon))
     &  +(yl-ellpnt(5,izon))*(yl-ellpnt(5,izon))
     &  +(zl-ellpnt(6,izon))*(zl-ellpnt(6,izon))
c
      c=ellpnt(7,izon)
      a=(a2-a1)/c
      b=(c*c+b2-b1)/c*0.5d0
      ald=a*a-1.0d0
      alu=(a*b-a2)/ald
      u=(b*b-b2)/ald
      c=alu*alu-u
      IF(c.ge.0.0d0) then
        c=dsqrt(c)
        rin= -alu-c
        rout=-alu+c
        IF(rin.ge.0.0d0) then
          itvalm=itvalm+1
          atval(itvalm)=rin
        elseif(rin.ge.-elleps.and.rin.lt.0.0d0) then
          itvalm=itvalm+1
          atval(itvalm)=0.0d0
        endif
        IF(rout.ge.0.0d0) then
          itvalm=itvalm+1
          atval(itvalm)=rout
        elseif(rout.ge.-elleps.and.rout.lt.0.0d0) then
          itvalm=itvalm+1
          atval(itvalm)=0.0d0
        endif
      endif
      RETURN
      end
!--------------------last line of subroutine ellcg1.f------------------

!-------------------------------arbset.f--------------------------------
! Version: 051219-1435
! Reference: JNC TN1410 2002-201
!-----------------------------------------------------------------------
!23456789|123456789|123456789|123456789|123456789|123456789|123456789|12

! ----------------------------------------------------------------------
! Required (geometry) subroutine for use with the EGS5 Code System
! ----------------------------------------------------------------------
! This is a subroutine arbset.
! ----------------------------------------------------------------------
      SUBROUTINE arbset(izon)
      implicit none
c
      include 'auxcommons/geom_common.f' ! geom-common file
c
      integer izon
      double precision X(3,8),V(3,4)
      integer IX(4,6)
      integer i,j,k,m,ipmax,nside,j1,j2,npl,nmi
      double precision a,b,c,d,dmin,ds,eps
      double precision dltarb2
      dltarb2=1.0d-6
c
      DO 100 I=1,8
        DO 100 J=1,3
          k=3*(I-1)+J
          X(J,I)=arbpnt(k,izon)
  100 continue
C
      IPMAX=0
      NSIDE=0
      DO  140 I=1,6
        K=0
        M=arbpnt(I+24,izon)
        DO  120  J=1,4
          IX(J,I)=M-(M/10)*10
          IF( IX(J,I).NE.0) K=K+1
          IF( IX(J,I).GT.IPMAX) IPMAX=IX(J,I)
          M=M/10
  120   continue
        IF(K.EQ.0) then
          GO TO 150
        elseif(K.lt.3) then
          WRITE(*,6000) 'ERROR IN SIDE DESCRIPTION',
     &                     I,J,arbpnt(I+24,izon)
 6000 FORMAT(1x,a,2I10,F10.0)
          stop
        endif
        NSIDE=I
  140 CONTINUE
C
C  FIND  MINIMUM DISTANCE BETWEEN POINTS
C
  150 continue
      DMIN=dlnmax
      DO 160 I=1,IPMAX-1
        DO 160 J=I+1,IPMAX
          D = (X(1,I)-X(1,J))**2 + (X(2,I)-X(2,J))**2
     &      + (X(3,I)-X(3,J))**2
          IF((D.GT.0).AND.(D.LT.DMIN)) then
            DMIN=D
          endif
  160 CONTINUE
C
      DMIN=DSQRT(DMIN)
C
      DO 300 I=1,NSIDE
        J1= IX( 3,I)
        DO 200  J=2,4,2
          J2= IX( J,I)
          DO 200  K=1,3
            V(K,J) = X(K,J1)- X(K,J2)
  200   continue
        A= V(2,2)*V(3,4) -  V(3,2)*V(2,4)
        B= V(3,2)*V(1,4) -  V(1,2)*V(3,4)
        C= V(1,2)*V(2,4) -  V(2,2)*V(1,4)
        D=-(A*X(1,J1)+ B*X(2,J1)+ C*X(3,J1) )
        EPS=DSQRT(A*A + B*B + C*C)
        NPL=0
        NMI=0
        DO 280 J=1,IPMAX
          DS=(A*X(1,J) + B*X(2,J)+ C*X(3,J) + D)/EPS
          IF(DABS(DS).ge.DMIN*dltarb2) then
            IF(ds.lt.0.0d0) then
              NMI=NMI+1
            ELSEIF(ds.gt.0.0d0) then
              NPL=NPL+1
            endif
          endif
  280   CONTINUE
        if((NMI.GT.0).or.(NPL.EQ.0)) then
          EPS=-EPS
          if((NPL.gt.0).or.(NMI.eq.0)) then
            WRITE(*,6200) 'ERROR IN FACE DESCRIPTION',
     &                  I,NMI,NPL,arbpnt(i+24,izon),A,B,C,D
 6200 FORMAT(1x,a,3I10/5D15.7)
            stop
          ENDIF
        endif
        arbtbl(4*i-3,izon) = A/EPS
        arbtbl(4*i-2,izon) = B/EPS
        arbtbl(4*i-1,izon) = C/EPS
        arbtbl(4*i  ,izon) = D/EPS
  300 continue
c
      arbtbl(25,izon)=DMIN
      arbtbl(26,izon)=NSIDE
cc    do j=1,26
cc    WRITE(*,*) 'j,arbtbl=',j,arbtbl(j,izon)
cc    end do
      RETURN
      END
!--------------------last line of subroutine arbset.f------------------

!-------------------------------arbcg1.f--------------------------------
! Version: 060719-1615
! Reference: JNC TN1410 2002-201
!-----------------------------------------------------------------------
!23456789|123456789|123456789|123456789|123456789|123456789|123456789|12

! ----------------------------------------------------------------------
! Required (geometry) subroutine for use with the EGS5 Code System
! ----------------------------------------------------------------------
! This is a subroutine arbcg1.
! ----------------------------------------------------------------------
      subroutine arbcg1(izon,xl,yl,zl,unp,vnp,wnp)
      implicit none
c
      include 'auxcommons/geom_common.f' ! geom-common file
c
      double precision xl,yl,zl
      double precision unp,vnp,wnp
      integer izon
      integer i,ii,j,jj,nside
      double precision dmin
      double precision rin,rout,dx,dy,dz
      double precision pv(3)
      double precision dltarb2
      dltarb2=1.0d-6
      dmin =arbtbl(25,izon)*dltarb2
      nside=arbtbl(26,izon)
      rin = dlnmax
      rout=-dlnmax
      do 400 ii=1,nside
        i=(ii-1)*4+1
        dx=arbtbl(i  ,izon)*xl+arbtbl(i+1,izon)*yl
     &    +arbtbl(i+2,izon)*zl+arbtbl(i+3,izon)
        dy= -arbtbl(i  ,izon)*unp-arbtbl(i+1,izon)*vnp
     &      -arbtbl(i+2,izon)*wnp
        if( dabs(dy).gt.dltarb2) then
           dz=dx/dy
           pv(1)=xl+dz*unp
           pv(2)=yl+dz*vnp
           pv(3)=zl+dz*wnp
           do 200 jj=1,nside
             j=(jj-1)*4+1
             if(j.ne.i) then
               dx=arbtbl(j  ,izon)*pv(1)+arbtbl(j+1,izon)*pv(2)
     &           +arbtbl(j+2,izon)*pv(3)+arbtbl(j+3,izon)
               if( dx.lt.-arbeps .and. -dx.ge.dmin ) then
                 goto 400
               endif
             end if
  200      continue
c           WRITE(*,*) 'ii,dz,rin,rout=',ii,dz,rin,rout
           if(dz.le.rout) then
             rin=dz
             goto 600
           else
             rin=rout
             rout=dz
             IF(rin.gt.-dlnmax) then
               GOTO 600
             endif
           endif
        endif
c        WRITE(*,*) 'ii,rin,rout=',ii,rin,rout
  400 continue
  600 continue
      IF(rin.ge.0.0d0.and.rin.lt.dlnmax) then
          itvalm=itvalm+1
          atval(itvalm)=rin
      elseif(rin.ge.-arbeps.and.rin.lt.0.0d0) then
          itvalm=itvalm+1
          atval(itvalm)=0.0d0
      endif
      IF(rout.ge.0.0d0.and.rout.lt.dlnmax) then
        itvalm=itvalm+1
        atval(itvalm)=rout
      elseif(rout.ge.-arbeps.and.rout.lt.0.0d0) then
          itvalm=itvalm+1
          atval(itvalm)=0.0d0
      endif
      return
      end
!--------------------last line of subroutine arbcg1.f------------------

!-------------------------------recset.f--------------------------------
! Version: 051219-1435
! Reference: JNC TN1410 2002-201
!-----------------------------------------------------------------------
!23456789|123456789|123456789|123456789|123456789|123456789|123456789|12

! ----------------------------------------------------------------------
! Required (geometry) subroutine for use with the EGS5 Code System
! ----------------------------------------------------------------------
! This is a subroutine recset.
      subroutine recset(izon)
      implicit none
c
      include 'auxcommons/geom_common.f' ! geom-common file
c
      integer izon

      double precision chk1,chk2,chk3,chksqr
      data chksqr/1.0d-4/

      recpnt(13,izon)=recpnt(4,izon)*recpnt(4,izon)
     &               +recpnt(5,izon)*recpnt(5,izon)
     &               +recpnt(6,izon)*recpnt(6,izon)
      recpnt(14,izon)=recpnt(7,izon)*recpnt(7,izon)
     &               +recpnt(8,izon)*recpnt(8,izon)
     &               +recpnt(9,izon)*recpnt(9,izon)
      recpnt(15,izon)=recpnt(10,izon)*recpnt(10,izon)
     &               +recpnt(11,izon)*recpnt(11,izon)
     &               +recpnt(12,izon)*recpnt(12,izon)
      if(recpnt(13,izon).eq.0.0d0.or.
     &   recpnt(14,izon).eq.0.0d0.or.
     &   recpnt(15,izon).eq.0.0d0) then
        write(*,*) 'Error of REC ',nbrec(izon),' : Length is 0.0'
        stop
      end if
c
      chk1 = ( recpnt( 7,izon)*recpnt(4,izon)
     &     +   recpnt( 8,izon)*recpnt(5,izon)
     &     +   recpnt( 9,izon)*recpnt(6,izon) )
     &     / sqrt(recpnt(13,izon)*recpnt(14,izon))
      chk2 = ( recpnt(10,izon)*recpnt(4,izon)
     &     +   recpnt(11,izon)*recpnt(5,izon)
     &     +   recpnt(12,izon)*recpnt(6,izon) )
     &     / sqrt(recpnt(13,izon)*recpnt(15,izon))
      chk3 = ( recpnt(10,izon)*recpnt(7,izon)
     &     +   recpnt(11,izon)*recpnt(8,izon)
     &     +   recpnt(12,izon)*recpnt(9,izon) )
     &     / sqrt(recpnt(14,izon)*recpnt(15,izon))
      if(abs(chk1).gt.chksqr.or.
     &   abs(chk2).gt.chksqr.or.
     &   abs(chk3).gt.chksqr) then
        write(*,*) 'Error of REC ',nbrec(izon),' : Vector is not square'
     &             ,chk1,chk2,chk3,chksqr
        stop
      end if
      return
      end
!--------------------last line of subroutine recset.f------------------

!-------------------------------reccg1.f--------------------------------
! Version: 051219-1435
! Reference: JNC TN1410 2002-201
!-----------------------------------------------------------------------
!23456789|123456789|123456789|123456789|123456789|123456789|123456789|12

! ----------------------------------------------------------------------
! Required (geometry) subroutine for use with the EGS5 Code System
! ----------------------------------------------------------------------
! This is a subroutine reccg1.
      subroutine reccg1(izon,xl,yl,zl,unp,vnp,wnp)
      implicit none
c
      include 'auxcommons/geom_common.f' ! geom-common file
c
      double precision xl,yl,zl
      double precision unp,vnp,wnp
      integer izon
      integer lro,lri,lcp,lcm
      double precision rin,rout
      double precision vph,vphhh
      double precision ambd,ambda,um,umu,den,disc
      double precision sd,f1,r1,r2,hh,wh,cp,cm
c
      double precision rs2,rl2,plx,ply,plz,vrs,vrl,wrs,wrl,wrs2,wrl2
      double precision rs4,rl4,dltrec
c
      double precision plxn,plyn,plzn,vphn
      double precision wph,wph2
      double precision vrsn,vrln

      DATA dltrec/1.0d-6/
c
      rin =-dlnmax
      rout= dlnmax
      lro=0
      lri=0
c
c4    COMPUTE DOT PRODUCTS OF P.P AND Q.Q
c
      hh =recpnt(13,izon)
      rs2=recpnt(14,izon)
      rl2=recpnt(15,izon)
      rs4=rs2*rs2
      rl4=rl2*rl2
c
c5    COMPUTE (V-XB) FOR X,Y,Z COORDINATES
c
      plx=(xl-recpnt(1,izon))
      ply=(yl-recpnt(2,izon))
      plz=(zl-recpnt(3,izon))
c
c6    TRANSFORM XL,YL,ZL TO THE COORDINATES OF THE REC
c
      vph=plx*recpnt( 4,izon)+ply*recpnt( 5,izon)+plz*recpnt( 6,izon)
      vrs=plx*recpnt( 7,izon)+ply*recpnt( 8,izon)+plz*recpnt( 9,izon)
      vrl=plx*recpnt(10,izon)+ply*recpnt(11,izon)+plz*recpnt(12,izon)
c
      um=rl4*vrs*vrs+rs4*vrl*vrl-rs4*rl4
      wh =unp*recpnt(4,izon)+vnp*recpnt(5,izon)+wnp*recpnt(6,izon)
c
      wph=unp*recpnt( 4,izon)+vnp*recpnt( 5,izon)+wnp*recpnt( 6,izon)
      wrs=unp*recpnt( 7,izon)+vnp*recpnt( 8,izon)+wnp*recpnt( 9,izon)
      wrl=unp*recpnt(10,izon)+vnp*recpnt(11,izon)+wnp*recpnt(12,izon)
      wph2=wph*wph
      wrs2=wrs*wrs
      wrl2=wrl*wrl
      ambd=wrs*vrs*rl4+wrl*vrl*rs4
      den=wrs2*rl4+wrl2*rs4
      if(dabs(den).le.dltrec) then
        IF(dabs(ambd).le.dltrec) then
          r1=-dlnmax
          r2= dlnmax
        else
          r1 = -um/ambd/2.0d0
          r2 = -um/ambd/2.0d0
        endif
      else
        ambda=ambd/den
        umu=um/den
        disc=ambda**2-umu
        if(disc.lt.0.0d0) goto 250
c
c8    COMPUTE THE INTERSECT POINTS ON THE QUADRATIC SURFACE
c
        if(disc.eq.0.0d0) then
          r1=-ambda
          r2=-ambda
        else
          sd=dsqrt(disc)
          r1=-ambda-sd
          r2=-ambda+sd
        endif
      endif
      IF(r1.ge.-receps.and.r1.lt.dlnmax) then
        plxn=(r1*unp+xl-recpnt(1,izon))
        plyn=(r1*vnp+yl-recpnt(2,izon))
        plzn=(r1*wnp+zl-recpnt(3,izon))
        vphn=plxn*recpnt( 4,izon)+plyn*recpnt( 5,izon)
     &      +plzn*recpnt( 6,izon)
        IF(vphn.ge.-receps.and.vphn.le.(hh+receps)*(1.0d0+receps)) then
          if(r1.ge.0.0d0) then
            itvalm=itvalm+1
            atval(itvalm)=r1
          else
            itvalm=itvalm+1
            atval(itvalm)=0.0d0
          endif
        endif
      endif
      IF(r2.ge.-receps.and.r2.lt.dlnmax.and.r2.ne.r1) then
        plxn=(r2*unp+xl-recpnt(1,izon))
        plyn=(r2*vnp+yl-recpnt(2,izon))
        plzn=(r2*wnp+zl-recpnt(3,izon))
        vphn=plxn*recpnt( 4,izon)+plyn*recpnt( 5,izon)
     &      +plzn*recpnt( 6,izon)
        IF(vphn.ge.-receps.and.vphn.le.(hh+receps)*(1.0d0+receps)) then
          itvalm=itvalm+1
          if(r2.ge.0.0d0) then
            itvalm=itvalm+1
            atval(itvalm)=r2
          else
            itvalm=itvalm+1
            atval(itvalm)=0.0d0
          endif
        endif
      endif
  250 continue
c
c9    DETERMINE IF RAY PARALLEL TO PLANAR SURFACES
c
      if(wh.lt.0.0d0) then
        if(vph.lt.-receps) goto 300
c
c10   COMPUTE THE INTERSECT POINTS ON THE PLANAR SURFACES
c
        cp=-vph/wh
        cm=(-vph+hh)/wh
        lcp=1
        lcm=2
      ELSEIF(wh.gt.0.0d0) then
        vphhh=-vph+hh
        if(vphhh.lt.-receps) goto 300
        cp=vphhh/wh
        cm=-vph/wh
        lcm=1
        lcp=2
      else
        cp=dlnmax
        cm=-cp
      endif
c
      IF(lcm.le.2) then
        IF(cm.ge.-receps.and.cm.lt.dlnmax) then
          plxn=(cm*unp+xl-recpnt(1,izon))
          plyn=(cm*vnp+yl-recpnt(2,izon))
          plzn=(cm*wnp+zl-recpnt(3,izon))
          vrsn=plxn*recpnt( 7,izon)+plyn*recpnt( 8,izon)
     &        +plzn*recpnt( 9,izon)
          vrln=plxn*recpnt(10,izon)+plyn*recpnt(11,izon)
     &        +plzn*recpnt(12,izon)
          f1=rl4*vrsn*vrsn+rs4*vrln*vrln-rs4*rl4
          IF(f1.le.receps) then
            if(cm.ge.0.0d0) then
              itvalm=itvalm+1
              atval(itvalm)=cm
            else
              itvalm=itvalm+1
              atval(itvalm)=0.0d0
            endif
          endif
        endif
      endif
c
      IF(lcp.le.2) then
        IF(cp.ge.-receps.and.cp.lt.dlnmax) then
          plxn=(cp*unp+xl-recpnt(1,izon))
          plyn=(cp*vnp+yl-recpnt(2,izon))
          plzn=(cp*wnp+zl-recpnt(3,izon))
          vrsn=plxn*recpnt( 7,izon)+plyn*recpnt( 8,izon)
     &        +plzn*recpnt( 9,izon)
          vrln=plxn*recpnt(10,izon)+plyn*recpnt(11,izon)
     &        +plzn*recpnt(12,izon)
          f1=rl4*vrsn*vrsn+rs4*vrln*vrln-rs4*rl4
          IF(f1.le.receps) then
            if(cp.ge.0.0d0) then
              itvalm=itvalm+1
              atval(itvalm)=cp
            else
              itvalm=itvalm+1
              atval(itvalm)=0.0d0
            endif
          endif
        endif
      endif
c
  300 continue
      return
      end
!--------------------last line of subroutine reccg1.f------------------

!-------------------------------wedset.f--------------------------------
! Version: 051219-1435
! Reference: JNC TN1410 2002-201
!-----------------------------------------------------------------------
!23456789|123456789|123456789|123456789|123456789|123456789|123456789|12

! ----------------------------------------------------------------------
! Required (geometry) subroutine for use with the EGS5 Code System
! ----------------------------------------------------------------------
! This is a subroutine wedset.
      subroutine wedset(izon)
      implicit none
c
      include 'auxcommons/geom_common.f' ! geom-common file
c
      integer izon
      integer i,ja

      double precision chk1,chk2,chk3,chksqr
      data chksqr/1.0d-4/
c
      DO 100 i=1,3
        ja=3*i
        wedpnt(i+12,izon)=wedpnt(ja+1,izon)*wedpnt(ja+1,izon)
     &                   +wedpnt(ja+2,izon)*wedpnt(ja+2,izon)
     &                   +wedpnt(ja+3,izon)*wedpnt(ja+3,izon)
  100 continue
c
      if(wedpnt(13,izon).eq.0.0d0.or.
     &   wedpnt(14,izon).eq.0.0d0.or.
     &   wedpnt(15,izon).eq.0.0d0) then
        write(*,*) 'Error of WED ',nbwed(izon),' : Length is 0.0'
        stop
      end if
c
      chk1 = ( wedpnt( 7,izon)*wedpnt(4,izon)
     &     +   wedpnt( 8,izon)*wedpnt(5,izon)
     &     +   wedpnt( 9,izon)*wedpnt(6,izon) )
     &     / sqrt(wedpnt(13,izon)*wedpnt(14,izon))
      chk2 = ( wedpnt(10,izon)*wedpnt(4,izon)
     &     +   wedpnt(11,izon)*wedpnt(5,izon)
     &     +   wedpnt(12,izon)*wedpnt(6,izon) )
     &     / sqrt(wedpnt(13,izon)*wedpnt(15,izon))
      chk3 = ( wedpnt(10,izon)*wedpnt(7,izon)
     &     +   wedpnt(11,izon)*wedpnt(8,izon)
     &     +   wedpnt(12,izon)*wedpnt(9,izon) )
     &     / sqrt(wedpnt(14,izon)*wedpnt(15,izon))
      if(abs(chk1).gt.chksqr.or.
     &   abs(chk2).gt.chksqr.or.
     &   abs(chk3).gt.chksqr) then
        write(*,*) 'Error of WED ',nbwed(izon),' : Vector is not square'
     &             ,chk1,chk2,chk3,chksqr
        stop
      end if
      return
      end      
!--------------------last line of subroutine wedset.f------------------

!-------------------------------wedcg1.f--------------------------------
! Version: 051219-1435
! Reference: JNC TN1410 2002-201
!-----------------------------------------------------------------------
!23456789|123456789|123456789|123456789|123456789|123456789|123456789|12

! ----------------------------------------------------------------------
! Required (geometry) subroutine for use with the EGS5 Code System
! ----------------------------------------------------------------------
! This is a subroutine wedcg1.
      subroutine wedcg1(izon,xl,yl,zl,unp,vnp,wnp)
      implicit none
c
      include 'auxcommons/geom_common.f' ! geom-common file
c
      double precision xl,yl,zl
      double precision unp,vnp,wnp
      integer izon
      integer i,ii,ll,kk
      double precision rin,rout,cm,cp
      double precision al(3),ap(3),aw(3)
      double precision dx,dy,dz
      double precision ag,temp,top,ap4
c
      rin =-dlnmax
      rout= dlnmax
      cm  =-dlnmax
      cp  = dlnmax
      ll=0
      kk=0
      dx=xl-wedpnt(1,izon)
      dy=yl-wedpnt(2,izon)
      dz=zl-wedpnt(3,izon)
      do 100 i=1,3
        ii=3*i+1
        al(i)=wedpnt(i+12,izon)
        ap(i)=dx*wedpnt(ii,izon)+dy*wedpnt(ii+1,izon)
     &       +dz*wedpnt(ii+2,izon)
        aw(i)=unp*wedpnt(ii,izon)+vnp*wedpnt(ii+1,izon)
     &       +wnp*wedpnt(ii+2,izon)
  100 continue
      ap4=ap(1)*al(2)+ap(2)*al(1)
      top=al(1)*al(2)-ap4
c
      do 200 i=1,2
        IF(aw(i).lt.0.0d0) then
          if(ap(i).lt.-wedeps) goto 300
          temp=-ap(i)/aw(i)
          if(temp.lt.cp) then
            cp=temp
            ll=i
          endif
        ELSEIF(aw(i).gt.0.0d0) then
          if((ap(i)+wedeps)*(1.0d0+wedeps).gt.al(i)) goto 300
          if(ap(i).le.0.0d0) then
            temp=-ap(i)/aw(i)
            if(temp.gt.cm) then
              cm=temp
              kk=i
            endif
          endif
        else
          if( ap(i).lt.-wedeps) goto 900
          if( (ap(i)+wedeps)*(1.0d0+wedeps).gt.al(i)) goto 900
        endif
  200 continue
c     WRITE(*,*) 'aw(3),ap(3),al(3)=',aw(3),ap(3),al(3)
      IF(aw(3).lt.0.0d0) then
        temp=-ap(3)+al(3)
        if(temp.le.wedeps) then
          temp=temp/aw(3)
          if(temp.gt.cm) then
            cm=temp
            kk=3
          endif
        endif
        if(ap(3).lt.-wedeps) goto 300
        temp=-ap(3)/aw(3)
        if(temp.lt.cp) then
          cp=temp
          ll=3
        endif
      elseif(aw(3).eq.0.0d0) then
        if(ap(3).lt.-wedeps) goto 300
        if((ap(3)+wedeps)*(1.0d0+wedeps).gt.al(3)) goto 300
      else
        if(ap(3).le.wedeps) then
          temp=-ap(3)/aw(3)
          if(temp.gt.cm) then
            cm=temp
            kk=3
          endif
        endif
        temp=-ap(3)+al(3)
        if(temp.lt.-wedeps) goto 300
        temp=temp/aw(3)
        if(temp.lt.cp) then
          cp=temp
          ll=3
        endif
      endif
      ag=al(2)*aw(1)+al(1)*aw(2)
c
      IF(ag.lt.0.0d0) then
        temp=top/ag
        if(temp.gt.cm) then
          cm=temp
          kk=4
        endif
      elseif(ag.gt.0.0d0) then
        if(top.lt.-wedeps) goto 300
        temp=top/ag
        if(temp.lt.cp) then
          cp=temp
          ll=4
         endif
      else
        if(ap4.lt.-wedeps) goto 300
        if(top.lt.-wedeps) goto 300
      endif
c     WRITE(*,*) '   ll,kk,cp,cm=',ll,kk,cp,cm
      if(ll+kk.ge.1) then
        rout=cp
        rin=cm
      endif
c
  300 continue
c     WRITE(*,*) '300 ll,kk,cp,cm=',ll,kk,cp,cm
      
      IF(rin.ge.0.0d0.and.rin.lt.dlnmax) then
        itvalm=itvalm+1
        atval(itvalm)=rin
      elseif(rin.ge.-wedeps.and.rin.lt.0.0d0) then
        itvalm=itvalm+1
        atval(itvalm)=0.0d0
      endif
      IF(rout.ge.0.0d0.and.rout.lt.dlnmax) then
        itvalm=itvalm+1
        atval(itvalm)=rout
      elseif(rout.ge.-wedeps.and.rout.lt.0.0d0) then
        itvalm=itvalm+1
        atval(itvalm)=0.0d0
      endif
  900 continue
c     WRITE(*,*) '900 ll,kk,cp,cm=',ll,kk,cp,cm
      return
      end
!--------------------last line of subroutine wedcg1.f------------------

!-------------------------------boxset.f--------------------------------
! Version: 051219-1435
! Reference: JNC TN1410 2002-201
!-----------------------------------------------------------------------
!23456789|123456789|123456789|123456789|123456789|123456789|123456789|12

! ----------------------------------------------------------------------
! Required (geometry) subroutine for use with the EGS5 Code System
! ----------------------------------------------------------------------
! This is a subroutine boxset.
      subroutine boxset(izon)
      implicit none
c
      include 'auxcommons/geom_common.f' ! geom-common file
c
      integer izon
      integer i,ja
c
      double precision chk1,chk2,chk3,chksqr
      data chksqr/1.0d-4/
c
      DO 100 i=1,3
        ja=3*i
        boxpnt(i+12,izon)=boxpnt(ja+1,izon)*boxpnt(ja+1,izon)
     &                   +boxpnt(ja+2,izon)*boxpnt(ja+2,izon)
     &                   +boxpnt(ja+3,izon)*boxpnt(ja+3,izon)
  100 continue
c
      if(boxpnt(13,izon).eq.0.0d0.or.
     &   boxpnt(14,izon).eq.0.0d0.or.
     &   boxpnt(15,izon).eq.0.0d0) then
        write(*,*) 'Error of BOX ',nbbox(izon),' : Length is 0.0'
        stop
      end if
c
      chk1 = ( boxpnt( 7,izon)*boxpnt(4,izon)
     &     +   boxpnt( 8,izon)*boxpnt(5,izon)
     &     +   boxpnt( 9,izon)*boxpnt(6,izon) )
     &     / sqrt(boxpnt(13,izon)*boxpnt(14,izon))
      chk2 = ( boxpnt(10,izon)*boxpnt(4,izon)
     &     +   boxpnt(11,izon)*boxpnt(5,izon)
     &     +   boxpnt(12,izon)*boxpnt(6,izon) )
     &     / sqrt(boxpnt(13,izon)*boxpnt(15,izon))
      chk3 = ( boxpnt(10,izon)*boxpnt(7,izon)
     &     +   boxpnt(11,izon)*boxpnt(8,izon)
     &     +   boxpnt(12,izon)*boxpnt(9,izon) )
     &     / sqrt(boxpnt(14,izon)*boxpnt(15,izon))
      if(abs(chk1).gt.chksqr.or.
     &   abs(chk2).gt.chksqr.or.
     &   abs(chk3).gt.chksqr) then
        write(*,*) 'Error of BOX ',nbbox(izon),' : Vector is not square'
     &             ,chk1,chk2,chk3,chksqr
        stop
      end if
      return
      end      
!--------------------last line of subroutine boxset.f------------------

!-------------------------------boxcg1.f--------------------------------
! Version: 051219-1435
! Reference: JNC TN1410 2002-201
!-----------------------------------------------------------------------
!23456789|123456789|123456789|123456789|123456789|123456789|123456789|12

! ----------------------------------------------------------------------
! Required (geometry) subroutine for use with the EGS5 Code System
! ----------------------------------------------------------------------
! This is a subroutine boxcg1.
      subroutine boxcg1(izon,xl,yl,zl,unp,vnp,wnp)
      implicit none
c
      include 'auxcommons/geom_common.f' ! geom-common file
c
      double precision xl,yl,zl
      double precision unp,vnp,wnp
      integer izon
      integer i,ja
      double precision rin,rout
      double precision al,ap,av,dl,ds
c
      rin =-dlnmax
      rout= dlnmax
      DO 300 i=1,3
        ja=3*i
        ap=(boxpnt(1,izon)-xl)*boxpnt(ja+1,izon)
     &    +(boxpnt(2,izon)-yl)*boxpnt(ja+2,izon)
     &    +(boxpnt(3,izon)-zl)*boxpnt(ja+3,izon)
        av=unp*boxpnt(ja+1,izon)
     &    +vnp*boxpnt(ja+2,izon)
     &    +wnp*boxpnt(ja+3,izon)
        al=boxpnt(i+12,izon)
c
        IF(av.eq.0.0d0) then
          if(-ap.lt.-boxeps) goto 900
          if(ap+al.lt.-boxeps) goto 900
        ELSE
          IF(av.lt.0.0d0) then
            dl=ap/av
            if(dl.lt.-boxeps) goto 900
            ds=(ap+al)/av
          else
            dl=(ap+al)/av
            if(dl.lt.-boxeps) goto 900
            ds=ap/av
          endif
          if(rout.ge.dl) then
            rout=dl
          endif
          if(rin.le.ds) then
            rin=ds
          endif
        endif
  300 continue
      IF(rin.ge.0.0d0.and.rin.lt.dlnmax) then
        itvalm=itvalm+1
        atval(itvalm)=rin
      elseif(rin.ge.-boxeps.and.rin.lt.0.0d0) then
        itvalm=itvalm+1
        atval(itvalm)=0.0d0
      endif
      IF(rout.ge.0.0d0.and.rout.lt.dlnmax) then
        itvalm=itvalm+1
        atval(itvalm)=rout
      elseif(rout.ge.-boxeps.and.rout.lt.0.0d0) then
        itvalm=itvalm+1
        atval(itvalm)=0.0d0
      endif
      return
  900 continue
      return
      end
!--------------------last line of subroutine boxcg1.f------------------

!-------------------------------hafset.f--------------------------------
! Version: 051219-1435
! Reference: JNC TN1410 2002-201
!-----------------------------------------------------------------------
!23456789|123456789|123456789|123456789|123456789|123456789|123456789|12

! ----------------------------------------------------------------------
! Required (geometry) subroutine for use with the EGS5 Code System
! ----------------------------------------------------------------------
! This is a subroutine hafset.
      subroutine hafset(izon)
      implicit none
c
      include 'auxcommons/geom_common.f' ! geom-common file
c
      integer izon
c
      hafpnt(5,izon)=dsqrt(hafpnt(1,izon)*hafpnt(1,izon)
     &                    +hafpnt(2,izon)*hafpnt(2,izon)
     &                    +hafpnt(3,izon)*hafpnt(3,izon))
c
      if(hafpnt(5,izon).eq.0.0d0) then
        write(*,*) 'Error of HAF ',nbhaf(izon),' : Vector is 0.0'
        stop
      end if
c
      return
      end
!--------------------last line of subroutine hafset.f------------------

!-------------------------------hafcg1.f--------------------------------
! Version: 051219-1435
! Reference: JNC TN1410 2002-201
!-----------------------------------------------------------------------
!23456789|123456789|123456789|123456789|123456789|123456789|123456789|12

! ----------------------------------------------------------------------
! Required (geometry) subroutine for use with the EGS5 Code System
! ----------------------------------------------------------------------
! This is a subroutine hafcg1.
      subroutine hafcg1(izon,xl,yl,zl,unp,vnp,wnp)
      implicit none
c
      include 'auxcommons/geom_common.f' ! geom-common file
c
      double precision xl,yl,zl
      double precision unp,vnp,wnp
      integer izon
      double precision rin
      double precision als,ap,av,ar
c
      rin =-dlnmax
c
      ap=xl*hafpnt(1,izon)
     &  +yl*hafpnt(2,izon)
     &  +zl*hafpnt(3,izon)
      ar =hafpnt(4,izon)
      als=hafpnt(5,izon)
c
      av=unp*hafpnt(1,izon)
     &  +vnp*hafpnt(2,izon)
     &  +wnp*hafpnt(3,izon)
c
c     WRITE(*,*) 'ap,av,als,ar=',ap,av,als,ar
c
      IF(av.ne.0.0d0) then
        rin=(ar*als-ap)/av
ccc   ELSE
c       IF(ar*als.eq.ap) then
c         rin=0.0d0
ccc     endif
      endif
c
      IF(rin.ge.0.0d0) then
        itvalm=itvalm+1
        atval(itvalm)=rin
      elseif(rin.ge.-hafeps.and.rin.lt.0.0d0) then
        itvalm=itvalm+1
        atval(itvalm)=0.0d0
      endif
      return
      end
!--------------------last line of subroutine hafcg1.f------------------

!-------------------------------hexset.f--------------------------------
! Version: 051219-1435
! Reference: JNC TN1410 2002-201
!-----------------------------------------------------------------------
!23456789|123456789|123456789|123456789|123456789|123456789|123456789|12

! ----------------------------------------------------------------------
! Required (geometry) subroutine for use with the EGS5 Code System
! ----------------------------------------------------------------------
! This is a subroutine hafset.
      subroutine hexset(izon)
      implicit none
c
      include 'auxcommons/geom_common.f' ! geom-common file
c
      integer izon
      integer i
      double precision axsq,angdeg
c
      double precision chk1,chksqr
      data chksqr/1.0d-4/
c
      hexpnt(11,izon)=hexpnt( 4,izon)*hexpnt( 4,izon)
     &               +hexpnt( 5,izon)*hexpnt( 5,izon)
     &               +hexpnt( 6,izon)*hexpnt( 6,izon)
c
      hexpnt(12,izon)=hexpnt( 7,izon)*hexpnt( 7,izon)
      hexpnt(13,izon)=hexpnt(12,izon)
      hexpnt(14,izon)=hexpnt(12,izon)
      axsq=dsqrt(hexpnt( 8,izon)*hexpnt( 8,izon)
     &          +hexpnt( 9,izon)*hexpnt( 9,izon)
     &          +hexpnt(10,izon)*hexpnt(10,izon))

      do i=1,3
        hexpnt(i+26,izon)=hexpnt(i+3,izon)
        hexpnt(i+29,izon)=hexpnt(7,izon)*hexpnt(i+7,izon)
     &                   /axsq
        hexpnt(i+14,izon)=hexpnt(i,izon)
        hexpnt(i+17,izon)=hexpnt(i,izon)-hexpnt(i+29,izon)/2.0d0
      enddo
      angdeg= 60.0d0
      call hexrot(hexpnt(4,izon),hexpnt(30,izon),hexpnt(33,izon),angdeg)
      angdeg=-60.0d0
      call hexrot(hexpnt(4,izon),hexpnt(30,izon),hexpnt(36,izon),angdeg)
      do i=1,3
        hexpnt(i+20,izon)=hexpnt(i,izon)-hexpnt(i+32,izon)/2.0d0
        hexpnt(i+23,izon)=hexpnt(i,izon)-hexpnt(i+35,izon)/2.0d0
      enddo
c     WRITE(*,*) 'hexpnt(11,izon)=',(hexpnt(i,izon),i=11,14)
c     WRITE(*,*) 'hexpnt(15,izon)=',(hexpnt(i,izon),i=15,17)
c     WRITE(*,*) 'hexpnt(18,izon)=',(hexpnt(i,izon),i=18,20)
c     WRITE(*,*) 'hexpnt(21,izon)=',(hexpnt(i,izon),i=21,23)
c     WRITE(*,*) 'hexpnt(24,izon)=',(hexpnt(i,izon),i=24,26)
c     WRITE(*,*) 'hexpnt(27,izon)=',(hexpnt(i,izon),i=27,29)
c     WRITE(*,*) 'hexpnt(30,izon)=',(hexpnt(i,izon),i=30,32)
c     WRITE(*,*) 'hexpnt(33,izon)=',(hexpnt(i,izon),i=33,35)
c     WRITE(*,*) 'hexpnt(36,izon)=',(hexpnt(i,izon),i=36,38)
c
      if(hexpnt(11,izon).eq.0.0d0.or.
     &   axsq.eq.0.0d0.or.
     &   hexpnt(12,izon).eq.0.0d0) then
        write(*,*) 'Error of HEX ',nbhex(izon),' : Length is 0.0'
        stop
      end if
c
      chk1 = ( hexpnt( 8,izon)*hexpnt(4,izon)
     &     +   hexpnt( 9,izon)*hexpnt(5,izon)
     &     +   hexpnt(10,izon)*hexpnt(6,izon) )
     &     / (sqrt(hexpnt(11,izon))*axsq)
      if(abs(chk1).gt.chksqr) then
        write(*,*) 'Error of HEX ',nbhex(izon),' : Vector is not square'
     &             ,chk1,chksqr
        stop
      end if
      return
      end
!--------------------last line of subroutine hexset.f------------------

!-------------------------------hexcg1.f--------------------------------
! Version: 051219-1435
! Reference: JNC TN1410 2002-201
!-----------------------------------------------------------------------
!23456789|123456789|123456789|123456789|123456789|123456789|123456789|12

! ----------------------------------------------------------------------
! Required (geometry) subroutine for use with the EGS5 Code System
! ----------------------------------------------------------------------
! This is a subroutine hexcg1.
      subroutine hexcg1(izon,xl,yl,zl,unp,vnp,wnp)
      implicit none
c
      include 'auxcommons/geom_common.f' ! geom-common file
c
      double precision xl,yl,zl
      double precision unp,vnp,wnp
      integer izon
      integer i,jp,ja
      double precision rin,rout
      double precision al,ap,av,dl,ds
c
      rin =-dlnmax
      rout= dlnmax
      DO 300 i=1,4
        jp=3*i+11
        ja=3*i+23
        ap=(hexpnt(jp+1,izon)-xl)*hexpnt(ja+1,izon)
     &    +(hexpnt(jp+2,izon)-yl)*hexpnt(ja+2,izon)
     &    +(hexpnt(jp+3,izon)-zl)*hexpnt(ja+3,izon)
        av=unp*hexpnt(ja+1,izon)
     &    +vnp*hexpnt(ja+2,izon)
     &    +wnp*hexpnt(ja+3,izon)
        al=hexpnt(i+10,izon)
c
        IF(av.eq.0.0d0) then
          if(-ap.lt.-hexeps) goto 900
          if(ap+al.lt.-hexeps) goto 900
        ELSE
          IF(av.lt.0.0d0) then
            dl=ap/av
            if(dl.lt.-hexeps) goto 900
            ds=(ap+al)/av
          else
            dl=(ap+al)/av
            if(dl.lt.-hexeps) goto 900
            ds=ap/av
          endif
          if(rout.ge.dl) then
            rout=dl
          endif
          if(rin.le.ds) then
            rin=ds
          endif
        endif
  300 continue
      IF(rin.ge.0.0d0.and.rin.lt.dlnmax) then
        itvalm=itvalm+1
        atval(itvalm)=rin
      elseif(rin.ge.-hexeps.and.rin.lt.0.0d0) then
        itvalm=itvalm+1
        atval(itvalm)=0.0
      endif
      IF(rout.ge.0.0d0.and.rout.lt.dlnmax) then
        itvalm=itvalm+1
        atval(itvalm)=rout
      elseif(rout.ge.-hexeps.and.rout.lt.0.0d0) then
        itvalm=itvalm+1
        atval(itvalm)=0.0
      endif
      return
  900 continue
      return
      end
!--------------------last line of subroutine hexcg1.f------------------

!-------------------------------hexrot.f--------------------------------
! Version: 051219-1435
! Reference: JNC TN1410 2002-201
!-----------------------------------------------------------------------
!23456789|123456789|123456789|123456789|123456789|123456789|123456789|12

! ----------------------------------------------------------------------
! Required (geometry) subroutine for use with the EGS5 Code System
! ----------------------------------------------------------------------
! This is a subroutine hexrot.
      subroutine hexrot(axv,vin,vout,ang)
      implicit none
c
      double precision axv(3),vin(3),vout(3),ang
      double precision pi,axyz
      double precision c,s,ax,ay,az,xo,yo,zo,x,y,z
      DATA pi/3.14159265d0/
      c=dcos(ang*pi/180.d0)
      s=dsin(ang*pi/180.d0)
      axyz=dsqrt(axv(1)*axv(1)+axv(2)*axv(2)+axv(3)*axv(3))
      ax=axv(1)/axyz
      ay=axv(2)/axyz
      az=axv(3)/axyz
      xo=vin(1)
      yo=vin(2)
      zo=vin(3)
      call rotrot(c,s,ax,ay,az,xo,yo,zo,x,y,z)
      vout(1)=x
      vout(2)=y
      vout(3)=z
      return
      end
!--------------------last line of subroutine hexrot.f------------------

!-------------------------------rotrot.f--------------------------------
! Version: 051219-1435
! Reference: JNC TN1410 2002-201
!-----------------------------------------------------------------------
!23456789|123456789|123456789|123456789|123456789|123456789|123456789|12

! ----------------------------------------------------------------------
! Required (geometry) subroutine for use with the EGS5 Code System
! ----------------------------------------------------------------------
! This is a subroutine rotrot.

      subroutine rotrot(c,s,ax,ay,az,xo,yo,zo,x,y,z)
      implicit none
      double precision c,s,ax,ay,az,xo,yo,zo,x,y,z
      double precision cvf11,cvf21,cvf31,cvf12,cvf22,
     &                 cvf32,cvf13,cvf23,cvf33
c                                                                               
c     c=cos(ang)                                                                
c     s=sin(ang)                                                                
      cvf11=ax*ax*(1.0d0-c)+c
      cvf21=ay*ax*(1.0d0-c)-az*s
      cvf31=az*ax*(1.0d0-c)+ay*s
      cvf12=ax*ay*(1.0d0-c)+az*s
      cvf22=ay*ay*(1.0d0-c)+c
      cvf32=az*ay*(1.0d0-c)-ax*s
      cvf13=ax*az*(1.0d0-c)-ay*s
      cvf23=ay*az*(1.0d0-c)+ax*s
      cvf33=az*az*(1.0d0-c)+c
c                                                                               
      x=xo*cvf11+yo*cvf21+zo*cvf31
      y=xo*cvf12+yo*cvf22+zo*cvf32
      z=xo*cvf13+yo*cvf23+zo*cvf33
c
      return
      end
!--------------------last line of subroutine rotrot.f------------------

!-------------------------------tecset.f--------------------------------
! Version: 051219-1435
! Reference: JNC TN1410 2002-201
!-----------------------------------------------------------------------
!23456789|123456789|123456789|123456789|123456789|123456789|123456789|12

! ----------------------------------------------------------------------
! Required (geometry) subroutine for use with the EGS5 Code System
! ----------------------------------------------------------------------
! This is a subroutine tecset.
      subroutine tecset(izon)
      implicit none
c
      include 'auxcommons/geom_common.f' ! geom-common file
c
      integer izon
      double precision chk1,chk2,chk3,chksqr
      data chksqr/1.0d-4/

      tecpnt(14,izon)=tecpnt(4,izon)*tecpnt(4,izon)
     &               +tecpnt(5,izon)*tecpnt(5,izon)
     &               +tecpnt(6,izon)*tecpnt(6,izon)
      tecpnt(15,izon)=tecpnt(7,izon)*tecpnt(7,izon)
     &               +tecpnt(8,izon)*tecpnt(8,izon)
     &               +tecpnt(9,izon)*tecpnt(9,izon)
      tecpnt(16,izon)=tecpnt(10,izon)*tecpnt(10,izon)
     &               +tecpnt(11,izon)*tecpnt(11,izon)
     &               +tecpnt(12,izon)*tecpnt(12,izon)
      if(tecpnt(14,izon).eq.0.0d0.or.
     &   tecpnt(15,izon).eq.0.0d0.or.
     &   tecpnt(16,izon).eq.0.0d0) then
        write(*,*) 'Error of TEC ',nbtec(izon),': Length is 0.0'
        stop
      end if
c
      chk1 = ( tecpnt( 7,izon)*tecpnt(4,izon)
     &     +   tecpnt( 8,izon)*tecpnt(5,izon)
     &     +   tecpnt( 9,izon)*tecpnt(6,izon) )
     &     / sqrt(tecpnt(14,izon)*tecpnt(15,izon))
      chk2 = ( tecpnt(10,izon)*tecpnt(4,izon)
     &     +   tecpnt(11,izon)*tecpnt(5,izon)
     &     +   tecpnt(12,izon)*tecpnt(6,izon) )
     &     / sqrt(tecpnt(14,izon)*tecpnt(16,izon))
      chk3 = ( tecpnt(10,izon)*tecpnt(7,izon)
     &     +   tecpnt(11,izon)*tecpnt(8,izon)
     &     +   tecpnt(12,izon)*tecpnt(9,izon) )
     &     / sqrt(tecpnt(15,izon)*tecpnt(16,izon))
      if(abs(chk1).gt.chksqr.or.
     &   abs(chk2).gt.chksqr.or.
     &   abs(chk3).gt.chksqr) then
        write(*,*) 'Error of TEC ',nbtec(izon),' : Vector is not square'
     &            ,chk1,chk2,chk3,chksqr
        stop
      end if
      return
      end
!--------------------last line of subroutine tecset.f------------------

!-------------------------------teccg1.f--------------------------------
! Version: 051219-1435
! Reference: JNC TN1410 2002-201
!-----------------------------------------------------------------------
!23456789|123456789|123456789|123456789|123456789|123456789|123456789|12

! ----------------------------------------------------------------------
! Required (geometry) subroutine for use with the EGS5 Code System
! ----------------------------------------------------------------------
! This is a subroutine teccg1.
      subroutine teccg1(izon,xl,yl,zl,unp,vnp,wnp)
      implicit none
c
      include 'auxcommons/geom_common.f' ! geom-common file
c
      double precision xl,yl,zl
      double precision unp,vnp,wnp
      integer izon
      integer lro,lri,lcp,lcm
      double precision rin,rout
      double precision vph,vphhh
      double precision ambd,ambda,um,umu,den,disc
      double precision sd,f1,r1,r2,hh,wh,cp,cm
c
      double precision rs2,rl2,plx,ply,plz,vrs,vrl,wrs,wrl,wrs2,wrl2
      double precision rs4,rl4,dlttec
c
      double precision plxn,plyn,plzn,vphn
      double precision rrw,rrw2
      double precision rrwr,rrwr2
      double precision rrf,rrf2
      double precision wph,wph2,h4
      double precision vrsn,vrln

      DATA dlttec/1.0d-6/
c
      rin =-dlnmax
      rout= dlnmax
      lro=0
      lri=0
c
c4    COMPUTE DOT PRODUCTS OF P.P AND Q.Q
c
      rrw=tecpnt(13,izon)
      hh =tecpnt(14,izon)
      rs2=tecpnt(15,izon)
      rl2=tecpnt(16,izon)
      h4 =hh*hh
      rs4=rs2*rs2
      rl4=rl2*rl2
      rrwr=1.0d0-rrw
      rrwr2=rrwr*rrwr
c
c5    COMPUTE (V-XB) FOR X,Y,Z COORDINATES
c
      plx=(xl-tecpnt(1,izon))
      ply=(yl-tecpnt(2,izon))
      plz=(zl-tecpnt(3,izon))
c
c6    TRANSFORM XL,YL,ZL TO THE COORDINATES OF THE TEC
c
      vph=plx*tecpnt( 4,izon)+ply*tecpnt( 5,izon)+plz*tecpnt( 6,izon)
      vrs=plx*tecpnt( 7,izon)+ply*tecpnt( 8,izon)+plz*tecpnt( 9,izon)
      vrl=plx*tecpnt(10,izon)+ply*tecpnt(11,izon)+plz*tecpnt(12,izon)
      rrf =1.0d0-rrwr*vph/hh
      rrf2=rrf*rrf
      rrw2=rrw*rrw
c
      um =rl4*vrs*vrs*h4+rs4*vrl*vrl*h4-rs4*rl4*vph*vph*rrwr2
     &   +2.0d0*rs4*rl4*hh*rrwr*vph - rs4*rl4*h4
      wh =unp*tecpnt(4,izon)+vnp*tecpnt(5,izon)+wnp*tecpnt(6,izon)
c
      wph=unp*tecpnt( 4,izon)+vnp*tecpnt( 5,izon)+wnp*tecpnt( 6,izon)
      wrs=unp*tecpnt( 7,izon)+vnp*tecpnt( 8,izon)+wnp*tecpnt( 9,izon)
      wrl=unp*tecpnt(10,izon)+vnp*tecpnt(11,izon)+wnp*tecpnt(12,izon)
      wph2=wph*wph
      wrs2=wrs*wrs
      wrl2=wrl*wrl
      ambd=wrs*vrs*rl4*h4+wrl*vrl*rs4*h4-wph*vph*rs4*rl4*rrwr2
     &    +wph*rs4*rl4*hh*rrwr
      den=wrs2*rl4*h4+wrl2*rs4*h4-rs4*rl4*rrwr2*wph2
      if(dabs(den).le.dlttec) then
        IF(dabs(ambd).le.dlttec) then
          r1=-dlnmax
          r2= dlnmax
        else
          r1 = -um/ambd/2.0d0
          r2 = -um/ambd/2.0d0
        endif
      else
        ambda=ambd/den
        umu=um/den
        disc=ambda**2-umu
        if(disc.lt.0.0d0) goto 250
c
c8    COMPUTE THE INTERSECT POINTS ON THE QUADRATIC SURFACE
c
        if(disc.eq.0.0d0) then
          r1=-ambda
          r2=-ambda
        else
          sd=dsqrt(disc)
          r1=-ambda-sd
          r2=-ambda+sd
        endif
      endif
      IF(r1.ge.-teceps.and.r1.lt.dlnmax) then
        plxn=(r1*unp+xl-tecpnt(1,izon))
        plyn=(r1*vnp+yl-tecpnt(2,izon))
        plzn=(r1*wnp+zl-tecpnt(3,izon))
        vphn=plxn*tecpnt( 4,izon)+plyn*tecpnt( 5,izon)
     &      +plzn*tecpnt( 6,izon)
        IF(vphn.ge.-teceps.and.vphn.le.(hh+teceps)*(1.0d0+teceps)) then
          if(r1.gt.0.0) then
            itvalm=itvalm+1
            atval(itvalm)=r1
          else
            itvalm=itvalm+1
            atval(itvalm)=0.0d0
          endif
        endif
      endif
      IF(r2.ge.-teceps.and.r2.lt.dlnmax.and.r2.ne.r1) then
        plxn=(r2*unp+xl-tecpnt(1,izon))
        plyn=(r2*vnp+yl-tecpnt(2,izon))
        plzn=(r2*wnp+zl-tecpnt(3,izon))
        vphn=plxn*tecpnt( 4,izon)+plyn*tecpnt( 5,izon)
     &      +plzn*tecpnt( 6,izon)
        IF(vphn.ge.-teceps.and.vphn.le.(hh+teceps)*(1.0d0+teceps)) then
          if(r2.gt.0.0) then
            itvalm=itvalm+1
            atval(itvalm)=r2
          else
            itvalm=itvalm+1
            atval(itvalm)=0.0d0
          endif
        endif
      endif
  250 continue
c
c9    DETERMINE IF RAY PARALLEL TO PLANAR SURFACES
c
      if(wh.lt.0.0d0) then
        if(vph.lt.-teceps) goto 300
c
c10   COMPUTE THE INTERSECT POINTS ON THE PLANAR SURFACES
c
        cp=-vph/wh
        cm=(-vph+hh)/wh
        lcp=1
        lcm=2
      ELSEIF(wh.gt.0.0d0) then
        vphhh=-vph+hh
        if(vphhh.lt.-teceps) goto 300
        cp=vphhh/wh
        cm=-vph/wh
        lcm=1
        lcp=2
      else
        cp=dlnmax
        cm=-cp
      endif
c
      IF(lcm.le.2) then
        IF(cm.ge.-teceps.and.cm.lt.dlnmax) then
          plxn=(cm*unp+xl-tecpnt(1,izon))
          plyn=(cm*vnp+yl-tecpnt(2,izon))
          plzn=(cm*wnp+zl-tecpnt(3,izon))
          vrsn=plxn*tecpnt( 7,izon)+plyn*tecpnt( 8,izon)
     &        +plzn*tecpnt( 9,izon)
          vrln=plxn*tecpnt(10,izon)+plyn*tecpnt(11,izon)
     &        +plzn*tecpnt(12,izon)
          if(lcm.eq.1) then
            f1=rl4*vrsn*vrsn+rs4*vrln*vrln-rs4*rl4
          else
            f1=rl4*vrsn*vrsn+rs4*vrln*vrln-rs4*rl4*rrw2
          endif
          IF(f1.le.teceps) then
            if(cm.gt.0.0d0) then
              itvalm=itvalm+1
              atval(itvalm)=cm
            else
              itvalm=itvalm+1
              atval(itvalm)=0.0d0
            endif
          endif
        endif
      endif
c
      IF(lcp.le.2) then
        IF(cp.ge.-teceps.and.cp.lt.dlnmax) then
          plxn=(cp*unp+xl-tecpnt(1,izon))
          plyn=(cp*vnp+yl-tecpnt(2,izon))
          plzn=(cp*wnp+zl-tecpnt(3,izon))
          vrsn=plxn*tecpnt( 7,izon)+plyn*tecpnt( 8,izon)
     &        +plzn*tecpnt( 9,izon)
          vrln=plxn*tecpnt(10,izon)+plyn*tecpnt(11,izon)
     &        +plzn*tecpnt(12,izon)
          if(lcp.eq.1) then
            f1=rl4*vrsn*vrsn+rs4*vrln*vrln-rs4*rl4
          else
            f1=rl4*vrsn*vrsn+rs4*vrln*vrln-rs4*rl4*rrw2
          endif
          IF(f1.le.teceps) then
            if(cp.gt.0.0d0) then
              itvalm=itvalm+1
              atval(itvalm)=cp
            else
              itvalm=itvalm+1
              atval(itvalm)=0.0d0
            endif
          endif
        endif
      endif
c
  300 continue
      return
      end
!--------------------last line of subroutine teccg1.f------------------

!-------------------------------gelset.f--------------------------------
! Version: 060919-1435
! Reference: JNC TN1410 2002-201
!-----------------------------------------------------------------------
!23456789|123456789|123456789|123456789|123456789|123456789|123456789|12

! ----------------------------------------------------------------------
! Required (geometry) subroutine for use with the EGS5 Code System
! ----------------------------------------------------------------------
! This is a subroutine gelset.
      subroutine gelset(izon)
      implicit none
c
      include 'auxcommons/geom_common.f' ! geom-common file
c
      integer izon
      double precision chk1,chk2,chk3,chksqr
      data chksqr/1.0d-4/
c
      gelpnt(13,izon) = sqrt(gelpnt(4,izon)*gelpnt(4,izon)
     &                     + gelpnt(5,izon)*gelpnt(5,izon)
     &                     + gelpnt(6,izon)*gelpnt(6,izon))
      gelpnt(14,izon) = sqrt(gelpnt(7,izon)*gelpnt(7,izon)
     &                     + gelpnt(8,izon)*gelpnt(8,izon)
     &                     + gelpnt(9,izon)*gelpnt(9,izon))
      gelpnt(15,izon) = sqrt(gelpnt(10,izon)*gelpnt(10,izon)
     &                     + gelpnt(11,izon)*gelpnt(11,izon)
     &                     + gelpnt(12,izon)*gelpnt(12,izon))
      if(gelpnt(13,izon).eq.0.0d0) then
        write(*,*) 'Error of GEL ',nbgel(izon),' : Vector R1 is 0.0'
        stop
      end if
      if(gelpnt(14,izon).eq.0.0d0) then
        write(*,*) 'Error of GEL ',nbgel(izon),' : Vector R2 is 0.0'
        stop
      end if
      if(gelpnt(15,izon).eq.0.0d0) then
        write(*,*) 'Error of GEL ',nbgel(izon),' : Vector R3 is 0.0'
        stop
      end if
c
      chk1 = ( gelpnt( 7,izon)*gelpnt(4,izon)
     &     +   gelpnt( 8,izon)*gelpnt(5,izon)
     &     +   gelpnt( 9,izon)*gelpnt(6,izon) )
     &     / sqrt(gelpnt(13,izon)*gelpnt(14,izon))
      chk2 = ( gelpnt(10,izon)*gelpnt(4,izon)
     &     +   gelpnt(11,izon)*gelpnt(5,izon)
     &     +   gelpnt(12,izon)*gelpnt(6,izon) )
     &     / sqrt(gelpnt(13,izon)*gelpnt(15,izon))
      chk3 = ( gelpnt(10,izon)*gelpnt(7,izon)
     &     +   gelpnt(11,izon)*gelpnt(8,izon)
     &     +   gelpnt(12,izon)*gelpnt(9,izon) )
     &     / sqrt(gelpnt(14,izon)*gelpnt(15,izon))
      if(abs(chk1).gt.chksqr.or.
     &   abs(chk2).gt.chksqr.or.
     &   abs(chk3).gt.chksqr) then
        write(*,*) 'Error of GEL ',nbgel(izon),' : Vector is not square'
     &             ,chk1,chk2,chk3,chksqr
        stop
      end if
      return
      end
!--------------------last line of subroutine gelset.f------------------

!-------------------------------gelcg1.f--------------------------------
! Version: 080912-0841
! Reference: JNC TN1410 2002-201
!-----------------------------------------------------------------------
!23456789|123456789|123456789|123456789|123456789|123456789|123456789|12

! ----------------------------------------------------------------------
! Required (geometry) subroutine for use with the EGS5 Code System
! ----------------------------------------------------------------------
! This is a subroutine gelcg1.
! ----------------------------------------------------------------------
      subroutine gelcg1(izon,xl,yl,zl,unp,vnp,wnp)
      implicit none
c
      include 'auxcommons/geom_common.f' ! geom-common file
c
      double precision xl,yl,zl
      double precision unp,vnp,wnp
      integer izon
      double precision a,a1,a2,a3,b,b1,b2,b3,c
      double precision s,rin,rout
      rin = dlnmax
      rout=-dlnmax
      a1=(unp*gelpnt( 4,izon)+vnp*gelpnt( 5,izon)+wnp*gelpnt( 6,izon))
     &  /gelpnt(13,izon)/gelpnt(13,izon)
      a2=(unp*gelpnt( 7,izon)+vnp*gelpnt( 8,izon)+wnp*gelpnt( 9,izon))
     &  /gelpnt(14,izon)/gelpnt(14,izon)
      a3=(unp*gelpnt(10,izon)+vnp*gelpnt(11,izon)+wnp*gelpnt(12,izon))
     &  /gelpnt(15,izon)/gelpnt(15,izon)
      b1=((xl-gelpnt(1,izon))*gelpnt( 4,izon)
     &   +(yl-gelpnt(2,izon))*gelpnt( 5,izon)
     &   +(zl-gelpnt(3,izon))*gelpnt( 6,izon))
     &  /gelpnt(13,izon)/gelpnt(13,izon)
      b2=((xl-gelpnt(1,izon))*gelpnt( 7,izon)
     &   +(yl-gelpnt(2,izon))*gelpnt( 8,izon)
     &   +(zl-gelpnt(3,izon))*gelpnt( 9,izon))
     &  /gelpnt(14,izon)/gelpnt(14,izon)
      b3=((xl-gelpnt(1,izon))*gelpnt(10,izon)
     &   +(yl-gelpnt(2,izon))*gelpnt(11,izon)
     &   +(zl-gelpnt(3,izon))*gelpnt(12,izon))
     &  /gelpnt(15,izon)/gelpnt(15,izon)
      a=a1*a1+a2*a2+a3*a3
      b=a1*b1+a2*b2+a3*b3
      c=b1*b1+b2*b2+b3*b3-1.0d0
      if(a.ne.0.0d0) then
        s=(b*b-a*c)
        if(s.ge.0.0d0) then
          s=dsqrt(s)
          rin =(-b-s)/a
          rout=(-b+s)/a
          IF(rin.ge.0.0d0) then
            itvalm=itvalm+1
            atval(itvalm)=rin
          elseif(rin.ge.-geleps.and.rin.lt.0.0d0) then
            itvalm=itvalm+1
            atval(itvalm)=0.0d0
          endif
          IF(rout.ge.0.0d0) then
            itvalm=itvalm+1
            atval(itvalm)=rout
          elseif(rout.ge.-geleps.and.rout.lt.0.0d0) then
            itvalm=itvalm+1
            atval(itvalm)=0.0d0
          endif
        endif
      elseif(b.ne.0.0d0) then
        rin=-c/(b*2.0)
        IF(rin.ge.0.0d0) then
          itvalm=itvalm+1
          atval(itvalm)=rin
        elseif(rin.ge.-geleps.and.rin.lt.0.0d0) then
          itvalm=itvalm+1
          atval(itvalm)=0.0d0
        endif
      endif
      RETURN
      end
!--------------------last line of subroutine gelcg1.f------------------
!--------------------------------chgtr.f--------------------------------
! Version: 051219-1435
!-----------------------------------------------------------------------
!23456789|123456789|123456789|123456789|123456789|123456789|123456789|12

! ----------------------------------------------------------------------
! Auxiliary (geometry) subroutine for use with the EGS5 Code System
! ----------------------------------------------------------------------
! This subroutine determines ustep is larger than tvalp and, if so,
! changes ustep to tvalp and irnew to irnewp.
! ----------------------------------------------------------------------
! Input arguments:
! ----------------
!   tvalp  = Straight trajectory distance to a boundary surface
!   irnewp = New region that particle may possibly go into next
! ----------------------------------------------------------------------

      subroutine chgtr(tvalp,irnewp)

      implicit none

      include 'include/egs5_h.f'               ! Main EGS5 "header" file

      include 'include/egs5_epcont.f'    ! COMMONs required by EGS5 code

      real*8 tvalp                                           ! Arguments
      integer irnewp

      if (tvalp .le. ustep) then
        ustep = tvalp
        irnew = irnewp
      end if

      return

      end

!---------------------------last line of chgtr.f------------------------
!---------------------------------cone.f--------------------------------
! Version: 051219-1435
!-----------------------------------------------------------------------
!23456789|123456789|123456789|123456789|123456789|123456789|123456789|12

! ----------------------------------------------------------------------
! Auxiliary (geometry) subroutine for use with the EGS5 Code System
! ----------------------------------------------------------------------
! This subroutine determines whether or not a right circular cone is 
! intersected by a particle trajectory and, if so, obtains the distance
! to the surface.  This is the KEK version that was modified by
! Hirayama (16 April 1996) to treat a cone having theta larger than 
! 90 degree.
! ----------------------------------------------------------------------
! Input arguments:
! ----------------
!   icon = cone ID number
!   infl = 1 means particle is inside cone
!        = 0 means particle is outside cone
! Output arguments:
! -----------------
!   ihit = 1 means particle intersects surface
!        = 0 means particle misses surface
!   tcon = distance to surface if intersected
!
! ----------------------------------------------------------------------
! Note: Data in the form of the cotangent of the half angle (cotal),
!       its square (cotal2), and the z offset of the cone are required
!       and are passed by common/codata/ (e.g., defined in MAIN).
! ----------------------------------------------------------------------

      subroutine cone(icon,infl,ihit,tcon)

      implicit none

      include 'include/egs5_h.f'               ! Main EGS5 "header" file
      include 'include/egs5_stack.f'     ! COMMONs required by EGS5 code

      include 'auxcommons/aux_h.f'   ! Auxiliary-code "header" file
      include 'auxcommons/condta.f'        ! Auxiliary-code COMMONs

      real*8 tcon                                            ! Arguments
      integer icon,infl,ihit

      real*8                                           ! Local variables
     * delcon,cpcon,sgncon,znp,wnp,cpcon1,cpcon2,
     * dcon1,dcon2,acon,bcon,bcon1,ccon,tcon1,
     * bprim,ccon1,root,tcon11,tcon22,tcon2
      integer
     * nofcn,itwopr

      data delcon/1.D-8/

      nofcn = infl
      ihit = 0
      itwopr = 0
      cpcon = cotal(icon)
      sgncon = sign(1.D0,cpcon)
      cpcon = cotal2(icon)
      znp = sgncon*(z(np) - smalll(icon))
      wnp = sgncon*w(np)
      cpcon1 = 1.D0 + cpcon
      cpcon2 = sqrt(cpcon1)
      dcon1 = x(np)*x(np) + y(np)*y(np)
      dcon2 = sqrt(dcon1)

      if (znp. ge. 0. .or. wnp .ge. 0.) then
        acon = (u(np)*u(np) + v(np)*v(np))*cpcon - wnp*wnp
        bcon1 = (x(np)*u(np) + y(np)*v(np))*cpcon
        bcon = bcon1 - znp*wnp
        ccon = dcon1*cpcon - znp*znp
        if (acon .eq. 0.) then
          if (bcon .ne. 0.0) then
            if (abs(ccon) .lt. delcon*znp*znp .and. znp .ge. 0.) then
              if (nofcn .eq. 1 .and. bcon .ge. 0. .or. 
     *            nofcn .eq. 0 .and. bcon .le. 0.) then
                tcon1 = -ccon/(2.D0*bcon)
                if (tcon1 .ge. 0.) then
                  if (znp+tcon1*wnp .ge. 0.) then
                    tcon = tcon1
                    ihit = 1
                  end if
                end if
              end if
            end if
          else
            tcon1 = cpcon2*znp*sign(1.D0,-wnp)
            if (tcon1 .ge. 0.) then
              tcon = tcon1
              ihit = 1
            end if
          end if
        else if (abs(ccon) .lt. delcon*znp*znp .and. znp .ge. 0.) then
          bprim = bcon1 - wnp*dcon2
          if (nofcn .eq. 1 .and. bprim .lt. 0.) then
            tcon1 = -2.D0*bcon/acon
            if (tcon1 .ge. 0.) then
              tcon = tcon1
              ihit = 1
            end if
          end if
        else if (nofcn .eq. 1 .and. ccon .gt. 0.) then
          tcon = delcon
          ihit = 1
        else if (nofcn .eq. 0 .and. ccon .lt. 0. .and. znp .ge. 0.) then
          tcon = delcon
          ihit = 1
        else
          ccon1 = bcon*bcon - acon*ccon
          if (ccon1 .ge. 0.) then
            root = sqrt(ccon1)
            if (bcon .gt. 0.) then
              tcon11 = -(bcon + root)/acon
            else
              tcon11 = -ccon/(bcon - root)
            end if
            if (bcon .lt. 0.) then
              tcon22 = -(bcon - root)/acon
            else
              tcon22 = -ccon/(bcon + root)
            end if
            if (tcon11 .ge. 0. .or. tcon22 .ge. 0.) then
              if (tcon11 .lt. 0.) then
                tcon1 = tcon22
              else
                if (tcon22 .lt. 0.) then
                  tcon1 = tcon11
                else
                  itwopr = 1
                  tcon1 = min(tcon11,tcon22)
                  tcon2 = max(tcon11,tcon22)
                end if
              end if
              if (znp+tcon1*wnp .ge. 0.) then
                tcon = tcon1
                ihit = 1
              else if (itwopr .eq. 1 .and. znp+tcon2*wnp .ge. 0.) then
                tcon = tcon2
                ihit = 1
              end if
            end if
          end if
        end if
      end if

      return

      end

!---------------------------last line of cone.f-------------------------
!-------------------------------cone2.f--------------------------------
! Version: 051219-1435
! Reference:
!-----------------------------------------------------------------------
!23456789|123456789|123456789|123456789|123456789|123456789|123456789|12

! ----------------------------------------------------------------------
! Auxiliary (geometry) subroutine for use with the EGS5 Code System
! ----------------------------------------------------------------------
! Subroutine cone2 is generally called from subroutine howfar whenever
! a particle is in a region bounded between two cones.
! Both subroutines cone and chgtr are called by subroutine cone2.
! ----------------------------------------------------------------------
! Input arguments:
! ----------------
!   ncone1 = ID number assigned to cone including current position
!   nrg1   = ID number assigned to region particle trajectory
!            will lead into next
!   ncone2 = ID number assigned to cone inside current position
!   nrg2   = Same as above, but for second cone
! ----------------------------------------------------------------------

      subroutine cone2(ncone1,nrg1,ncone2,nrg2)

      implicit none

      real*8 tcone                                           ! Arguments
      integer ncone1,nrg1,ncone2,nrg2,ihit

      call cone(ncone1,0,ihit,tcone)
      if (ihit .eq. 1) then                             ! Hits 1st cone
        call chgtr(tcone,nrg1)
      end if
      call cone(ncone2,1,ihit,tcone)
      if (ihit .eq. 1) then                             ! Hits 2nd cone
        call chgtr(tcone,nrg2)
      end if

      return

      end

!-------------------------------cyl2.f--------------------------------
!-------------------------------cone21.f--------------------------------
! Version: 051219-1435
! Reference:
!-----------------------------------------------------------------------
!23456789|123456789|123456789|123456789|123456789|123456789|123456789|12

! ----------------------------------------------------------------------
! Auxiliary (geometry) subroutine for use with the EGS5 Code System
! ----------------------------------------------------------------------
! Subroutine cone21 is generally called from subroutine howfar whenever
! a particle is in a region bounded outside two cones.
! Both subroutines cone and chgtr are called by subroutine cone21.
! ----------------------------------------------------------------------
! Input arguments:
! ----------------
!   ncone1 = ID number assigned to cone including current position
!   nrg1   = ID number assigned to region particle trajectory
!            will lead into next
!   ncone2 = ID number assigned to cone inside current position
!   nrg2   = Same as above, but for second cone
! ----------------------------------------------------------------------

      subroutine cone21(ncone1,nrg1,ncone2,nrg2)

      implicit none

      real*8 tcone                                           ! Arguments
      integer ncone1,nrg1,ncone2,nrg2,ihit

      call cone(ncone1,0,ihit,tcone)
      if (ihit .eq. 1) then                             ! Hits 1st cone
        call chgtr(tcone,nrg1)
      end if
      call cone(ncone2,0,ihit,tcone)
      if (ihit .eq. 1) then                             ! Hits 2nd cone
        call chgtr(tcone,nrg2)
      end if

      return

      end

!-------------------------------cyl2.f--------------------------------
!-------------------------------cyl2.f--------------------------------
! Version: 051219-1435
! Reference:
!-----------------------------------------------------------------------
!23456789|123456789|123456789|123456789|123456789|123456789|123456789|12

! ----------------------------------------------------------------------
! Auxiliary (geometry) subroutine for use with the EGS5 Code System
! ----------------------------------------------------------------------
! Subroutine cyl2 is generally called from subroutine howfar whenever
! a particle is in a region bounded by two cylinder.
! Both subroutines cylinder and chgtr are called by subroutine cyl2.
! ----------------------------------------------------------------------
! Input arguments:
! ----------------
!   ncl1 = ID number assigned to cylinder including current position
!   nrg1 = ID number assigned to region particle trajectory
!          will lead into next
!   ncl2 = D number assigned to cylinder inside current position
!   nrg2 = Same as above, but for second cylinder)
! ----------------------------------------------------------------------

      subroutine cyl2(ncl1,nrg1,ncl2,nrg2)

      implicit none

      real*8 tcyl                                            ! Arguments
      integer ncl1,nrg1,ncl2,nrg2,ihit

      call cylndr(ncl1,0,ihit,tcyl)
      if (ihit .eq. 1) then                             ! Hits 1st cylinder
        call chgtr(tcyl,nrg1)
      end if
      call cylndr(ncl2,1,ihit,tcyl)
      if (ihit .eq. 1) then                             ! Hits 2nd cylinder
        call chgtr(tcyl,nrg2)
      end if

      return

      end

!-------------------------------cyl2.f--------------------------------
!-------------------------------cylndr.f--------------------------------
! Version: 051219-1435
!-----------------------------------------------------------------------
!23456789|123456789|123456789|123456789|123456789|123456789|123456789|12

! ----------------------------------------------------------------------
! Auxiliary (geometry) subroutine for use with the EGS5 Code System
! ----------------------------------------------------------------------
! This subroutine determines whether or not a right circular cylinder is 
! intersected by a particle trajectory and, if so, obtains the distance
! to the surface.
! ----------------------------------------------------------------------
! Input arguments:
! ----------------
!   icyl = cylinder ID number
!   infl = 1 means particle is inside cylinder
!        = 0 means particle is outside cylinder
! Output arguments:
! -----------------
!   ihit = 1 means particle intersects surface
!        = 0 means particle misses surface
!   tcyl = distance to surface if intersected
!
! Note: Data in the form of the cylinder-radius squared is required
!       and is transmitted via common/CYLDTA/.
! ----------------------------------------------------------------------

      subroutine cylndr(icyl,infl,ihit,tcyl)

      implicit none

      include 'include/egs5_h.f'               ! Main EGS5 "header" file
      include 'include/egs5_stack.f'     ! COMMONs required by EGS5 code

      include 'auxcommons/aux_h.f'   ! Auxiliary-code "header" file
      include 'auxcommons/cyldta.f'         ! Auxiliary-code COMMON

      real*8 tcyl                                            ! Arguments
      integer icyl,infl,ihit

      real*8 a,b,c,b2,rtarg,delcyl,rootcy              ! Local variables

      data delcyl/1.D-8/

      ihit = 1                                          ! Assume a "hit"
      tcyl = 0.

!     ----------------------------------------
!     Calculate the quadratic parameters a,b,c
!     ----------------------------------------
      a = sqrt(u(np)*u(np) + v(np)*v(np))
      if (a .eq. 0.) then                   ! Quadratic is indeterminate
        ihit = 0
        return
      end if

      b = (x(np)*u(np) + y(np)*v(np))/a
      c = x(np)*x(np) + y(np)*y(np) - cyrad2(icyl)
      b2 = b*b
      rtarg = b2 - c
      if (rtarg .lt. 0.) then                       ! Imaginary solution
        ihit = 0
        return
      end if

      if (abs(c) .lt. delcyl*cyrad2(icyl)) then
        if (infl .eq. 0 .and. b .ge. 0.) then
          ihit = 0
          return
        end if
        if (infl .eq. 1 .and. b .lt. 0.) then
          tcyl = -2.0*b/a
          return
        end if
      end if

      if ((infl .eq. 1 .and. c .ge. 0.) .or.
     *    (infl .eq. 0 .and. c .le. 0.)) then
        tcyl = delcyl*cyrad(icyl)
        return
      end if

!     ----------------------------------------------------------
!     Calculate the root(s) and choose the smallest positive one
!     ----------------------------------------------------------
      rootcy = sqrt(rtarg)
      if (c .lt. 0.) then
        tcyl = (-b + rootcy)/a
        return
      else if (b .lt. 0.) then
        tcyl = (-b - rootcy)/a
        return
      end if
      ihit = 0
      return

      end

!--------------------------last line of cylndr.f------------------------
!------------------------------decod_xyz.f-----------------------------
! Version: 051219-1435
! Reference: 030823-1730 by H. Hirayama and Y. Namito
!-----------------------------------------------------------------------
!23456789|123456789|123456789|123456789|123456789|123456789|123456789|12

! Subroutine to determine the i,j,k indices, for standard x-y-z 
! geometry EGS User Codes, given the region.  The first parameter is 
! the region number and next three are the decoded i,j,k indices.
! ----------------------------------------------------------------------
!
! -------------------
! SUBROUTINE ARGUMENT
! -------------------
!  irl   : region number
!  i     : x-position number
!  j     : y-position number
!  k     : z-position number
!  imax  : x-bin number
!  ijmax : imax*jmax jmax is y-bin number
!
! ----------------------------------------------------------------------
      subroutine decod_xyz(irl,i,j,k,imax,ijmax)

      implicit none

      integer i,ijmax,imax,irl,j,k

      i = mod (irl-1,imax)
      if (i .eq. 0) i = imax

      k = 1 + (irl -1 - i)/ijmax
      j = 1 + (irl -1 - i - (k -1 )*ijmax)/imax

      return
      end

!---------------------------- end of decodir.f ------------------------
!------------------------------decodeir.f-------------------------------
! Version: 051219-1435
! Reference:
!-----------------------------------------------------------------------
!23456789|123456789|123456789|123456789|123456789|123456789|123456789|12

! Subroutine to determine the i,j,k indices, for standard cylinder-slab
! geometry EGS User Codes (e.g., ucrtz.f), given the region.  The first 
! parameter is the region number and next three are the decoded i,j,k
! indices (requires /GEORTZ/ and region number must be greater than 1).
! ----------------------------------------------------------------------

      subroutine decodeir(irdum,idum,jdum,kdum)

      implicit none

      include 'auxcommons/geortz.f'             ! Auxiliary-code COMMONs

      integer                                                ! Arguments
     * irdum,idum,jdum,kdum

      if (irdum .le. 1) then
        write(6,*) '***** Program stopped because region is <= 1'
        stop
      end if

      idum = mod(irdum-1,imax)
      if (idum .eq. 0) idum = imax
      kdum = 1 + (irdum - 1 - idum)/ijmax
      jdum = 1 + (irdum - 1 - idum - (kdum - 1)*ijmax)/imax

      return

      end

!-------------------------last line of decodeir.f-----------------------
!-------------------------------ecnsv1.f--------------------------------
! Version: 060130-1415
!          091105-0930  initialize gsum
!-----------------------------------------------------------------------
!23456789|123456789|123456789|123456789|123456789|123456789|123456789|12

! ----------------------------------------------------------------------
! Auxiliary subroutine for use with the EGS5 Code System
! ----------------------------------------------------------------------
! Subroutine for keeping track of energy conservation.  When ntree=0,
! the program is entered in order to initialize the esum array to zero.
! Otherwise, it is entered for totaling and outputing the results.  The
! esum array is needed in subroutine ausgab, where the energy  that is
! being deposited is added to the element of the array corresponding to
! the current value of iq, ir, and iarg.
! ----------------------------------------------------------------------
! Subroutine ecnsv1 should be called with:
!
!   ntree = 0 before the CALL SHOWER loop (to initialize)
!         = 1 after the CALL SHOWER loop (to normalize to totke)
!
! ----------------------------------------------------------------------

      subroutine ecnsv1(ntree,nreg,totke)

      implicit none

      include 'include/egs5_h.f'               ! Main EGS5 "header" file

      include 'auxcommons/etaly1.f'              ! Auxiliary-code COMMON

      real*8 totke                                           ! Arguments
      integer ntree,nreg

      real*8 rowsum(4,MXREG),                          ! Local variables
     * colsum(4,5),
     * sumsum(4),
     * gsum
      integer i,j,k

      if (nreg .gt. MXREG) then
        write(6,101) nreg,MXREG
 101    FORMAT(///,' ***** NOTE: STOPPED IN SUBROUTINE ECNSV1 ',/,
     *             'BECAUSE NREG= ', I5,' IS LARGER THAN MXREG= ',
     *         I5,' *****')
        stop
      end if

!     --------------------------------------------
!     Initialize various sums to zero (and return)
!     --------------------------------------------
      if (ntree .eq. 0) then
        gsum=0.D0
        do i=1,4              ! Step over particle types (including ALL)
        sumsum(i) = 0.D0
          do j=1,nreg                                ! Step over regions
            rowsum(i,j) = 0.D0
            do k=1,5                             ! Step over iarg values
              esum(i,j,k) = 0.D0
              colsum(i,k) = 0.D0
            end do
          end do
        end do
                                                        ! --------------
        return                                          ! Return to MAIN
                                                        ! --------------
      end if

!     -----------------------------------------------
!     Reach this point when final tally is to be made
!     -----------------------------------------------
!     --------------------------------------------------------------
!     Sum i=1,2,3 into i=4 of esum for (all regions and iarg-values)
!     --------------------------------------------------------------
      do j=1,nreg                                    ! Step over regions
        do k=1,5                                 ! Step over iarg values
          do i=1,3                            ! Step over particle types
            esum(4,j,k) = esum(4,j,k) + esum(i,j,k)
          end do
        end do
      end do

!     -----------------------
!     Normalize data to totke
!     -----------------------
      do i=1,4                ! Step over particle types (including ALL)
        do j=1,nreg                                  ! Step over regions
          do k=1,5                               ! Step over iarg values
            esum(i,j,k) = esum(i,j,k)/totke
          end do
        end do
      end do

!     ------------------------------------------------------
!     Sum-up and rows, columns, row-column sum and grand sum
!     ------------------------------------------------------
      do i=1,4                ! Step over particle types (including ALL)
        do j=1,nreg                                  ! Step over regions
          do k=1,5                               ! Step over iarg values
            rowsum(i,j) = rowsum(i,j) + esum(i,j,k)
            colsum(i,k) = colsum(i,k) + esum(i,j,k)
            sumsum(i) = sumsum(i) + esum(i,j,k)
            if (i .le. 3) gsum = gsum + esum(i,j,k)
          end do
        end do
      end do

!     ----------------------------------------------------------
!     Now write-out the results of the energy deposition summary
!     ----------------------------------------------------------
!     -------------------------------------------
!     Check if suppressed write-out was requested
!     -------------------------------------------
      if (ntree .eq. 2) then
        write(6,102) gsum
 102    FORMAT(//,' ECNSV1 output has been suppressed',//,
     *            ' TOTAL FRACTION=',G15.7,
     *            '     NOTE: THIS NUMBER SHOULD BE VERY CLOSE TO UNITY'
     *  )
                                                        ! --------------
        return                                          ! Return to MAIN
                                                        ! --------------
      end if

!     ---------------------------------------------
!     Otherwise perform normal (complete) write-out
!     ---------------------------------------------
      do i=1,4
        if (i .le. 3) then
          write(6,103) i-2
 103      FORMAT(//,' ENERGY DEPOSITION SUMMARY FOR PARTICLES WITH IQ=',
     *           I2,
     *           /, 55X,'IARG',/,19X,'0',15X,'1',13X,'2',14X,'3',14X,
     *           '4',16X,'ROW SUM', /,3X,'REGION',/)
        else
          write(6,104)
 104      FORMAT(//,' ENERGY DEPOSITION SUMMARY FOR ALL PARTICLES:',
     *           /, 55X,'IARG',/,19X,'0',15X,'1',13X,'2',14X,'3',14X,
     *           '4',16X,'ROW SUM', /,3X,'REGION',/)
        end if

        do j=1,nreg
          write(6,105) j,(esum(i,j,k),k=1,5),rowsum(i,j)
 105      FORMAT(I7,5X,5G15.7,5X,G15.7)
        end do

        write(6,106) (colsum(i,k),k=1,5),sumsum(i)
 106    FORMAT(/,3X,'COL SUM',2X,5G15.7,5X,G15.7)

      end do

      write(6,107) gsum
 107  FORMAT(//,' TOTAL FRACTION=',G15.7,
     *        '     NOTE: THIS NUMBER SHOULD BE VERY CLOSE TO UNITY',//)

                                                        ! --------------
      return                                            ! Return to MAIN
                                                        ! --------------
      end

!--------------------------last line of ecnsv1.f------------------------
!-------------------------------edistr.f--------------------------------
! Version: 051219-1435
! Reference:  Adapted from a program written by D.W.O.R. (Aug 1985)
!-----------------------------------------------------------------------
!23456789|123456789|123456789|123456789|123456789|123456789|123456789|12

! ----------------------------------------------------------------------
! Auxiliary subroutine for use with the EGS5 Code System
! ----------------------------------------------------------------------
! This routine returns an initial kinetic energy given the cumulative
! distribution function (CDF) for the source spectrum stored in ECDF,
! the energy bin tops in Ebin and the minimum energy in EbinMin, all
! of which are passed in COMMON/EDATA/.
! ----------------------------------------------------------------------

      subroutine edistr(ekedum)

      implicit none

      include 'include/egs5_h.f'               ! Main EGS5 "header" file
      include 'auxcommons/aux_h.f'        ! Auxiliary-code "header" file

      include 'auxcommons/edata.f'               ! Auxiliary-code COMMON

      real*8 ekedum,rnnow                                    ! Arguments
      real*8 elow                                      ! Local variables
      integer i

!    ----------------------------------
!    Sample to determine the energy bin
!    ----------------------------------
      call randomset(rnnow)
      i=0
 1    continue
      i = i + 1
      if(encdf(i) .le. rnnow) go to 1

      if (i .gt. 1) then
        elow = ebin(i-1)
      else
        elow = ebinmin
      end if

!     -------------------------------------
!     Sample to determine energy within bin
!     -------------------------------------
      call randomset(rnnow)
      ekedum = elow + rnnow*(ebin(i) - elow)

      return

      end

!--------------------------last line of edistr.f------------------------
!-------------------------------fintrn.f--------------------------------
! Version: 051219-1435
!-----------------------------------------------------------------------
!23456789|123456789|123456789|123456789|123456789|123456789|123456789|12

! ----------------------------------------------------------------------
! Auxiliary (geometry) subroutine for use with the EGS5 Code System
! ----------------------------------------------------------------------
! Subroutine fintrn determines the final coordinates of a particle
! (xfin,yfin,zfin) given a distance (dist) together with the initial
! coordinates of the particle (xtran,ytran,ztran) and its direction 
! cosines (u,v,w).
! ----------------------------------------------------------------------
! SPECIAL NOTE: This routine is for a sphere whose origin is translated
!               along z with respect to the usual EGS coordinate.
!               The variables xtrans, ytrans and ztrans must be defined
!               in AUSGAB and passed in common/TRNDTA/.
! ----------------------------------------------------------------------

      subroutine fintrn(dist,xfin,yfin,zfin)

      implicit none

      include 'include/egs5_h.f'               ! Main EGS5 "header" file

      include 'include/egs5_stack.f'     ! COMMONs required by EGS5 code

      include 'auxcommons/trndta.f'             ! Auxiliary-code COMMONs

      real*8                                                 ! Arguments
     * dist,
     * xfin,yfin,zfin

      xfin = xtran + dist*u(np)
      yfin = ytran + dist*v(np)
      zfin = ztran + dist*w(np)

      return

      end

!-------------------------------fintrn.f--------------------------------
!-------------------------------finval.f--------------------------------
! Version: 051219-1435
! Reference:
!-----------------------------------------------------------------------
!23456789|123456789|123456789|123456789|123456789|123456789|123456789|12

! ----------------------------------------------------------------------
! Auxiliary (geometry) subroutine for use with the EGS5 Code System
! ----------------------------------------------------------------------
! Subroutine finval determines the final coordinates of a particle
! (xfin,yfin,zfin) given a distance (dist) together with the initial
! coordinates of the particle (x,y,z) and its direction cosines (u,v,w).
! ----------------------------------------------------------------------

      subroutine finval(dist,xfin,yfin,zfin)

      implicit none

      include 'include/egs5_h.f'               ! Main EGS5 "header" file

      include 'include/egs5_stack.f'     ! COMMONs required by EGS5 code

      real*8                                                 ! Arguments
     * dist,
     * xfin,yfin,zfin

      xfin = x(np) + dist*u(np)
      yfin = y(np) + dist*v(np)
      zfin = z(np) + dist*w(np)

      return

      end

!-------------------------------finval.f--------------------------------
!----------------------------- geomout.f--------------------------------
! Version: 060118-1515
! Reference: 040628-1700 by H. Hirayama and Y. Namito
!-----------------------------------------------------------------------
!23456789|123456789|123456789|123456789|123456789|123456789|123456789|12

! ----------------------------------------------------------------------
! Auxiliary (for PICT) subroutine for use with the EGS5 Code System
! ----------------------------------------------------------------------
! Subroutine geomout is called from main program and ausgab to output 
! geometry related information for PICT. 
! ----------------------------------------------------------------------
! Input arguments:
! ----------------
!      ncylg  : number of cylinders to draw
!      nplang : number of planes to draw
! ----------------------------------------------------------------------

      subroutine geomout(ncylg,nplang)
      
      include 'auxcommons/aux_h.f'        ! Auxiliary-code "header" file
      include 'auxcommons/cyldta.f'              ! Auxiliary-code COMMON
      include 'auxcommons/pladta.f'              ! Auxiliary-code COMMON
      include 'auxcommons/nfac.f'                ! Auxiliary-code COMMON

      integer ncylg,nplang                       ! Arguments

      real*8 cyl(MXCYLS),zbin(MXPLNS),ybin(MXPLNS),xbin(MXPLNS)
      real*8 pcv,pcv1,pcv2,pcv3,pnv,pnv1,pnv2,pnv3,zwid
      integer i,nxp,nyp,nzp
      
! ---------------------
! I/O format statements
! ---------------------
10     FORMAT('GSTA')
20     FORMAT('CYLS')
30     FORMAT(3I6)
40     FORMAT(4E15.7)
50     FORMAT('GEND')
60     FORMAT('SLAB')

      if (ncylg.ne.0) then                     ! Cylinder slab geometry
        write(39,10)
        write(39,20)
        
        if (nplang.eq.0) then
          nzp=2
          zbin(1)=zmin
          zbin(2)=zmax

        else 
          nzp=0
          do i=1,nplang
            pnv=pnorm(3,i)
            pcv=pcoord(3,i)
            if (pnv.eq.1.and.(pcv.ge.zmin.and.pcv.le.zmax)) then
              nzp=nzp+1
              zbin(nzp)=pcv
            end if
          end do
        end if
        
        write(39,30) ncylg,nzp
        
        do i=1,ncylg
          cyl(i)=cyrad(i)
        end do
        
        write(39,40) (cyl(i), i=1,ncylg)
        write(39,40) (zbin(i),i=1,nzp)
        write(39,50)
        
      else if (nplang.ne.0) then               ! Plane geometry
        write(39,10)
        write(39,60)
        
        nzp=0
        nyp=0
        nxp=0
        
        do i=1,nplang
          pnv1=pnorm(1,i)
          pcv1=pcoord(1,i)
          pnv2=pnorm(2,i)
          pcv2=pcoord(2,i)
          pnv3=pnorm(3,i)
          pcv3=pcoord(3,i)
          if(pnv1.eq.1) then
            if(pcv1.ge.xmin.and.pcv1.le.xmax) then
              nxp=nxp+1
              xbin(nxp)=pcv1
            end if
          else if (pnv2.eq.1) then
            if (pcv2.ge.ymin.and.pcv2.le.ymax) then
              nyp=nyp+1
              ybin(nyp)=pcv2
            end if
          else 
            if(pcv3.ge.zmin.and.pcv3.le.zmax) then
              nzp=nzp+1
              zbin(nzp)=pcv3
            end if
          end if
        end do
        
        zwid=abs(zmax-zmin)
        if (nxp.eq.0) then
          xbin(1)=-zwid/2.0
          xbin(2)= zwid/2.0
        end if
        if (nyp.eq.0) then
          ybin(1)=-zwid/2.0
          ybin(2)= zwid/2.0
        end if
        write(39,30) nxp,nyp,nzp
        write(39,40) (xbin(i), i=1,nxp)
        write(39,40) (ybin(i), i=1,nyp)
        write(39,40) (zbin(i), i=1,nzp)
        write(39,50)
      else                                    ! Not produce geometry
        write(39,10)
        write(39,50)
      end if
      
      return
      end
      
!----------------------last line of geomout.f------------------------
!-------------------------------ntally.f--------------------------------
! Version: 060130-1415
!          091105-0930  initialize gsum
!-----------------------------------------------------------------------
!23456789|123456789|123456789|123456789|123456789|123456789|123456789|12

! ----------------------------------------------------------------------
! Auxiliary subroutine for use with the EGS5 Code System
! ----------------------------------------------------------------------
! Subroutine for tallying up the number of times edep-data has been
! entered into the ecnsv1 array.  The results can also be used to get
! a rough idea of whether certain (iq,ir,iarg) events are rare or not.
! Caution: Do not rely on these numbers for variance estimates.
! ----------------------------------------------------------------------

      subroutine ntally(ntree,nreg)

      implicit none

      include 'include/egs5_h.f'               ! Main EGS5 "header" file

      include 'auxcommons/ntaly1.f'              ! Auxiliary-code COMMON

      integer ntree,nreg                                     ! Arguments

      integer*8 rowsum(4,MXREG),                       ! Local variables
     * colsum(4,5),
     * sumsum(4),
     * gsum
      integer i,j,k

      if (nreg .gt. MXREG) then
        write(6,101) nreg,MXREG
 101    FORMAT(///,' ***** NOTE: STOPPED IN SUBROUTINE NTALLY ',/,
     *             'BECAUSE NREG= ', I5,' IS LARGER THAN MXREG= ',
     *         I5,' *****')
        stop
      end if

!     --------------------------------------------
!     Initialize various sums to zero (and return)
!     --------------------------------------------
      if (ntree .eq. 0) then
        gsum=0.D0
        do i=1,4              ! Step over particle types (including ALL)
        sumsum(i) = 0
          do j=1,nreg                                ! Step over regions
            rowsum(i,j) = 0
            do k=1,5                             ! Step over iarg values
              nsum(i,j,k) = 0
              colsum(i,k) = 0
            end do
          end do
        end do
                                                        ! --------------
        return                                          ! Return to MAIN
                                                        ! --------------
      end if

!     -----------------------------------------------
!     Reach this point when final tally is to be made
!     -----------------------------------------------
!     --------------------------------------------------------------
!     Sum i=1,2,3 into i=4 of nsum for (all regions and iarg-values)
!     --------------------------------------------------------------
      do j=1,nreg                                    ! Step over regions
        do k=1,5                                 ! Step over iarg values
          do i=1,3                            ! Step over particle types
            nsum(4,j,k) = nsum(4,j,k) + nsum(i,j,k)
          end do
        end do
      end do

!     ------------------------------------------------------
!     Sum-up and rows, columns, row-column sum and grand sum
!     ------------------------------------------------------
      do i=1,4                ! Step over particle types (including ALL)
        do j=1,nreg                                  ! Step over regions
          do k=1,5                               ! Step over iarg values
            rowsum(i,j) = rowsum(i,j) + nsum(i,j,k)
            colsum(i,k) = colsum(i,k) + nsum(i,j,k)
            sumsum(i) = sumsum(i) + nsum(i,j,k)
            if (i .le. 3) gsum = gsum + nsum(i,j,k)
          end do
        end do
      end do

!     ----------------------------------------------------
!     Now write-out the results of the event count summary
!     ----------------------------------------------------
!     -------------------------------------------
!     Check if suppressed write-out was requested
!     -------------------------------------------
                                                        ! --------------
      if (ntree .eq. 2) return                          ! Return to MAIN
                                                        ! --------------

!     ---------------------------------------------
!     Otherwise perform normal (complete) write-out
!     ---------------------------------------------
      do i=1,4
        if (i .le. 3) then
          write(6,103) i-2
 103      FORMAT(//,' SUMMARY OF EVENT COUNT FOR PARTICLES WITH IQ=',I2,
     *           /, 55X,'IARG',/,19X,'0',15X,'1',13X,'2',14X,'3',14X,
     *           '4',16X,'ROW SUM', /,3X,'REGION',/)
        else
          write(6,104)
 104      FORMAT(//,' SUMMARY OF EVENT COUNT FOR ALL PARTICLES:',
     *           /, 55X,'IARG',/,19X,'0',15X,'1',13X,'2',14X,'3',14X,
     *           '4',16X,'ROW SUM', /,3X,'REGION',/)
        end if

        do j=1,nreg
          write(6,105) j,(nsum(i,j,k),k=1,5),rowsum(i,j)
 105      FORMAT(I7,5X,5I15,5X,I15)
        end do

        write(6,106) (colsum(i,k),k=1,5),sumsum(i)
 106    FORMAT(/,3X,'COL SUM',2X,5I15,5X,I15)

      end do

      write(6,107) gsum
 107  FORMAT(//,' TOTAL NUMBER OF EVENTS=',I15)
                                                        ! --------------
      return                                            ! Return to MAIN
                                                        ! --------------
      end

!--------------------------last line of ntally.f------------------------
!-------------------------------plan2p.f--------------------------------
! Version: 051219-1435
! Reference:
!-----------------------------------------------------------------------
!23456789|123456789|123456789|123456789|123456789|123456789|123456789|12

! ----------------------------------------------------------------------
! Auxiliary (geometry) subroutine for use with the EGS5 Code System
! ----------------------------------------------------------------------
! Subroutine plan2p is generally called from subroutine howfar whenever
! a particle is in a region bounded by two planes that are PARALLEL.
! Both subroutines plane1 and chgtr are called by subroutine plan2p, but
! for efficiency reasons the second plane1 call is not made if the first
! plane is not hit, or if the trajectory is parallel.
! ----------------------------------------------------------------------
! Input arguments:
! ----------------
!   npl1 = ID number assigned to plane called first
!   nrg1 = ID number assigned to region particle trajectory
!          will lead into next
!   isd1 =  1 normal vector points towards current region
!        = -1 normal vector points away from current region
!   npl2 = Same as above, but for plane called second
!   nrg2 = Same as above, but for plane called second)
!   isd2 = Same as above, but for plane called second
! ----------------------------------------------------------------------

      subroutine plan2p(npl1,nrg1,isd1,npl2,nrg2,isd2)

      implicit none

      real*8 tval                                            ! Arguments
      integer npl1,nrg1,isd1,npl2,nrg2,isd2,ihit

      call plane1(npl1,isd1,ihit,tval)
      if (ihit .eq. 1) then                             ! Hits 1st plane
        call chgtr(tval,nrg1)
      else if(ihit .eq. 0) then           ! Heading away from 1st plane,
        call plane1(npl2,isd2,ihit,tval)  !     but it may hit 2nd plane
        if (ihit .eq. 1) then                           ! Hits 2nd plane
          call chgtr(tval,nrg2)
        end if
      else if(ihit .ne. 2) then
        write(6,101) npl1,nrg1,npl2,nrg2,ihit
 101    FORMAT(' STOPPED IN SUBROUTINE PLAN2P WITH NPL1,NRG1,NPL2,NRG2='
     *         ,4I6,/,' AND WITH IHIT=',I6)
        stop
      end if

      return

      end

!-------------------------------plan2p.f--------------------------------
!-------------------------------plan2x.f--------------------------------
! Version: 051219-1435
! Reference:
!-----------------------------------------------------------------------
!23456789|123456789|123456789|123456789|123456789|123456789|123456789|12

! ----------------------------------------------------------------------
! Auxiliary (geometry) subroutine for use with the EGS5 Code System
! ----------------------------------------------------------------------
! Subroutine plan2x is generally called from subroutine howfar whenever
! a particle is in a region bounded by two planes that are NOT parallel.
! Both subroutines plane1 and chgtr are called by subroutine plan2x.
! ----------------------------------------------------------------------
! Input arguments:
! ----------------
!   npl1 = ID number assigned to plane called first
!   nrg1 = ID number assigned to region particle trajectory
!          will lead into next
!   isd1 =  1 normal vector points towards current region
!        = -1 normal vector points away from current region
!   npl2 = Same as above, but for plane called second
!   nrg2 = Same as above, but for plane called second)
!   isd2 = Same as above, but for plane called second
! ----------------------------------------------------------------------

      subroutine plan2x(npl1,nrg1,isd1,npl2,nrg2,isd2)

      implicit none

      real*8 tval                                            ! Arguments
      integer npl1,nrg1,isd1,npl2,nrg2,isd2,ihit

      call plane1(npl1,isd1,ihit,tval)
      if (ihit .eq. 1) call chgtr(tval,nrg1)

      call plane1(npl2,isd2,ihit,tval)
      if (ihit .eq. 1) call chgtr(tval,nrg2)

      return

      end

!-------------------------------plan2x.f--------------------------------
!-------------------------------plane1.f--------------------------------
! Version: 060116-1100
! Reference:
!-----------------------------------------------------------------------
!23456789|123456789|123456789|123456789|123456789|123456789|123456789|12

! ----------------------------------------------------------------------
! Auxiliary (geometry) subroutine for use with the EGS5 Code System
! ----------------------------------------------------------------------
! This subroutine determines whether or not a plane is intersected by a
! particle trajectory and, if so, obtains the distance to the surface.
! A plane is defined relative to a coordinate system by means of a point
! on its surface (pcoord-array) and a unit vector normal to the surface
! (pnorm-array).  Both arrays are passed in common/PLADTA/.  The user 
! must assign appropriate values to pcoord and pnorm in the User Code.
!
! Subroutine plane1 is called whenever the user wants to determine if
! the straight-line trajectory of a particle with coordinates is (X,Y,Z)
! and direction cosines (U,V,W) intersects a plane.  If it does,
! plane1 returns the distance to the plane.
! ----------------------------------------------------------------------
! Input arguments:
! ----------------
!   nplan = ID number assigned to plane
!   iside =  1 normal points away from current region
!         = -1 normal points towards current region
! Output arguments:
! -----------------
!   ihit  =  1 trajectory will strike plane
!         =  2 trajectory parallel to plane
!         =  0 trajectory moving away from plane
!   tval  = distance to plane (when ihit=1)
! ----------------------------------------------------------------------

      subroutine plane1(nplan,iside,ihit,tval)

      implicit none

      include 'include/egs5_h.f'               ! Main EGS5 "header" file

      include 'include/egs5_stack.f'     ! COMMONs required by EGS5 code

      include 'auxcommons/aux_h.f'        ! Auxiliary-code "header" file
      include 'auxcommons/pladta.f'             ! Auxiliary-code COMMONs

      real*8 tval,tnum                                       ! Arguments
      integer nplan,iside,ihit

      real*8 udota,udotap                              ! Local variables

      udota = pnorm(1,nplan)*u(np) + 
     *        pnorm(2,nplan)*v(np) +
     *        pnorm(3,nplan)*w(np)

      udotap = udota*iside

      if (udota .eq. 0.) then              ! Traveling parallel to plane
        ihit = 2
      else if (udotap .lt. 0.) then          ! Traveling away from plane
        ihit = 0
      else                ! Traveling towards plane---determine distance
        ihit = 1
        tnum = pnorm(1,nplan)*(pcoord(1,nplan) - x(np)) +
     *         pnorm(2,nplan)*(pcoord(2,nplan) - y(np)) +
     *         pnorm(3,nplan)*(pcoord(3,nplan) - z(np))
        tval = tnum/udota
      end if

      return

      end

!--------------------------last line of plane1.f------------------------
!----------------------------- plotxyz.f--------------------------------
! Version: 060620-1400
!          070817-1500    Include time information
!          080425-1600
! Reference:  by H. Hirayama and Y. Namito
!-----------------------------------------------------------------------
!23456789|123456789|123456789|123456789|123456789|123456789|123456789|12

! ----------------------------------------------------------------------
! Auxiliary (plot) subroutine for use with the EGS5 Code System
! ----------------------------------------------------------------------
! Subroutine plotxyz is called from main program and ausgab to output 
! X,Y,Z,IQ,E,IR,WT,TIME for 3 dimensional graphic display on PC. 
! This subroutine is based on PLOTXZ developed at SLAC for 2 dimensional
! display with UG.
! ----------------------------------------------------------------------
! Input arguments:
! ----------------
!   iarg = Indicates the situation under which ausgab is being called
!          99 means output buffered data
!   np   = Stack pointer
!   iq   = Integer charge of particle
!   x,y,z= Position of particle
!   enp  = Kinetic energy of particle
!   ir   = Index of particle's current region
!   wt   = Statistical weight of current particle used for CGView only
!   time = Time after start in seconds
! ----------------------------------------------------------------------

      subroutine plotxyz(iarg,np,iq,x,y,z,enp,ir,wt,time)

      include 'include/egs5_h.f'               ! Main EGS5 "header" file

      include 'auxcommons/nfac.f'              ! Auxiliary-code COMMON

                                               ! Arguments
      real*8 enp,x,y,z,wt,time
      integer iarg,iq,ir,np

                                               ! Local variables
      integer iept(100,MXSTACK),iqtold(MXSTACK),irpt(100,MXSTACK),
     &        ixpt(100,MXSTACK),
     *        iypt(100,MXSTACK),izpt(100,MXSTACK),npt(MXSTACK)
     
      real*8 ept(100,MXSTACK),wtpt(100,MXSTACK),xpt(100,MXSTACK),
     &       ypt(100,MXSTACK),
     *       zpt(100,MXSTACK),tpt(100,MXSTACK)
 
      real*8 eee
      integer i,iiq,inp,jarg,iff,ief
      
      data npt/MXSTACK*0/
      
! ---------------------
! I/O format statements
! ---------------------
100         FORMAT(I1,4I5)
110         FORMAT(I1,3I8,I5)
120         FORMAT(I1,3(1PE13.6),1PE10.3,I4,1PE10.3,1PE12.5)
130         FORMAT('-1')
140         FORMAT(I1,3(1PE14.6),1PE11.3,I10,1PE11.3,1PE12.5)

      if (npreci.lt.0) then
        return                      ! check display type
      end if
      if (iarg.eq.99) then                        ! Output buffered data
        do i=1, MXSTACK
          if(npt(i).le.0) go to 1
          if (iqtold(i).eq.0) then
             iiq=1
          else if (iqtold(i).eq.-1) then
             iiq=2
          else
             iiq=3
          end if
          
          if (npreci.ne.0) then
             iiq=iiq+3                              ! use different iiq 
          end if
          
          do inp=1,npt(i)
            if (npreci.eq.0)  then
              write(39,100) iiq,ixpt(inp,i),iypt(inp,i),izpt(inp,i),
     *        iept(inp,i)
            else if (npreci.eq.1) then
              write(39,110) iiq,ixpt(inp,i),iypt(inp,i),izpt(inp,i),
     *        iept(inp,i)
            else if (npreci.eq.2) then
              write(39,120) iiq,xpt(inp,i),ypt(inp,i),zpt(inp,i),
     *        ept(np,i),irpt(inp,i),wtpt(inp,i),tpt(inp,i)
            else 
              write(39,140) iiq,xpt(inp,i),ypt(inp,i),zpt(inp,i),
     *        ept(np,i),irpt(inp,i),wtpt(inp,i),tpt(inp,i)
            end if
            
            if (inp.eq.npt(i)) then
              write(39,130)
            end if
            npt(i)=0
          end do
          
1         continue
        end do
      
      else                                      ! iarg ne 99
        jarg=iarg
        npt(np)=npt(np) + 1
        if (npt(np).eq.1) iqtold(np)=iq
        if (npreci.eq.0) then                   ! 16 bitsPICT
          ixpt(npt(np),np)=x/fnorm*10000+50000
          iypt(npt(np),np)=y/fnorm*10000+50000
          izpt(npt(np),np)=z/fnorm*10000+50000
        else if (npreci.eq.1)  then             ! 32 bits PICT 
          ixpt(npt(np),np)=x/fnorm*8388608+33554432
          iypt(npt(np),np)=y/fnorm*8388608+33554432
          izpt(npt(np),np)=z/fnorm*8388608+33554432
        else
          xpt(npt(np),np)=x
          ypt(npt(np),np)=y
          zpt(npt(np),np)=z
        end if
        
        if (npreci.le.1)  then                 ! PICT 
          if (iq.eq.0)  then                   ! photon
            eee=enp*1000 
          else                                 ! charged particle
            eee=(enp-0.511)*1000.
          end if

          if  (eee.lt.10000.0) then 
            iept(npt(np),np)=int(eee)*10
          else 
            iff=log10(eee)-3
            ief=eee/10**iff
            iept(npt(np),np)=ief*10+iff
          end if 
        
        else                                    ! CGVIEW
          if (iq.eq.0) then 
            ept(npt(np),np)=enp 
          else
            ept(npt(np),np)=enp-0.511
          end if

          wtpt(npt(np),np)=wt
          tpt(npt(np),np)=time
          irpt(npt(np),np)=ir
        end if
        if (iq.ne.iqtold(np)) jarg=-1          ! particle type changes
        if (npt(np).ge.100.or.jarg.ne.0) then
          if (iqtold(np).eq.0) then
            iiq=1
          else if (iqtold(np).eq.-1) then
            iiq=2
          else
            iiq=3
          end if
          if (npreci.ne.0) then                ! not 16 bits PICT
            iiq=iiq+3
          end if
          if (npt(np).ge.1) then 
            do inp=1,npt(np)
              if (npreci.eq.0) then
      ! A particle energy is set at that of starting point of each line.
                if (jarg.ne.0.and.(inp.gt.1.and.inp.eq.npt(np))) then
                  write(39,100) iiq,ixpt(inp,np),iypt(inp,np),
     *            izpt(inp,np),iept(inp-1,np)
                else
                  write(39,100) iiq,ixpt(inp,np),iypt(inp,np),
     *            izpt(inp,np),iept(inp,np)
                end if
              else if (npreci.eq.1) then
      ! A particle energy is set at that of starting point of each line.
                if (jarg.ne.0.and.(inp.gt.1.and.inp.eq.npt(np))) then
                  write(39,110) iiq,ixpt(inp,np),iypt(inp,np),
     *            izpt(inp,np),iept(inp-1,np)
                else
                  write(39,110) iiq,ixpt(inp,np),iypt(inp,np),
     *            izpt(inp,np),iept(inp,np)
                end if
              else if (npreci.eq.2) then
      ! A particle energy is set at that of starting point of each line.
                if (jarg.ne.0.and.(inp.gt.1.and.inp.eq.npt(np))) then
                  write(39,120) iiq,xpt(inp,np),ypt(inp,np),zpt(inp,np),
     *             ept(inp-1,np),irpt(inp,np),wtpt(inp-1,np),
!     *             tpt(inp-1,np)
     *             tpt(inp,np)
                else
                  write(39,120) iiq,xpt(inp,np),ypt(inp,np),zpt(inp,np),
     *             ept(inp,np),irpt(inp,np),wtpt(inp,np),tpt(inp,np)
                end if
              else
      ! A particle energy is set at that of starting point of each line.
                if (jarg.ne.0.and.(inp.gt.1.and.inp.eq.npt(np))) then
                  write(39,140) iiq,xpt(inp,np),ypt(inp,np),zpt(inp,np),
     *             ept(inp-1,np),irpt(inp,np),wtpt(inp-1,np),
!     *             tpt(inp-1,np)
     *             tpt(inp,np)
                else
                  write(39,140) iiq,xpt(inp,np),ypt(inp,np),zpt(inp,np),
     *             ept(inp,np),irpt(inp,np),wtpt(inp,np),tpt(inp,np)
                end if
              end if
              if (inp.eq.npt(np)) then
                write(39,130)
              end if
            end do
          end if

          if (jarg.gt.0.or.iarg.gt.0) then  ! 070817-comment
! jarg.gt.0 can be deleted.
            npt(np)=0
          else if (jarg.eq.-1) then
            if (npreci.le.1) then
              ixpt(1,np)=ixpt(npt(np),np)
              iypt(1,np)=iypt(npt(np),np)
              izpt(1,np)=izpt(npt(np),np)
              iept(1,np)=iept(npt(np),np)
            else
              xpt(1,np)=xpt(npt(np),np)
              ypt(1,np)=ypt(npt(np),np)
              zpt(1,np)=zpt(npt(np),np)
              ept(1,np)=ept(npt(np),np)
              wtpt(1,np)=wtpt(npt(np),np)
              tpt(1,np)=tpt(npt(np),np)
              irpt(1,np)=irpt(npt(np),np)
            end if
            npt(np)=1
            iqtold(np)=iq
          else
            npt(np)=1
            if (npreci.le.1) then
              ixpt(1,np)=ixpt(100,np)
              iypt(1,np)=iypt(100,np)
              izpt(1,np)=izpt(100,np)
              iept(1,np)=iept(100,np)
            else
              xpt(1,np)=xpt(100,np)
              ypt(1,np)=ypt(100,np)
              zpt(1,np)=zpt(100,np)
              ept(1,np)=ept(100,np)
              wtpt(1,np)=wtpt(100,np)
              tpt(1,np)=tpt(100,np)
              irpt(1,np)=irpt(100,np)
            end if
          end if
        else
          iqtold(np)=iq
        end if
      end if
      return
      end

!----------------------last line of plotxyz.f------------------------
!-------------------------------rdistr.f--------------------------------
! Version: 051219-1435
! Reference:  Adapted from a program written by C. J. Huntzinger 
!             (Nov 1987), which is an adaptation from a program 
!             written by  D.W.O.R. (Aug 1985)
!-----------------------------------------------------------------------
!23456789|123456789|123456789|123456789|123456789|123456789|123456789|12

! ----------------------------------------------------------------------
! Auxiliary subroutine for use with the EGS5 Code System
! ----------------------------------------------------------------------
! This routine returns radial coordinates (ridum,xidum,yidum) given the
! cumulative distribution function (CDF) for the source radial distribu-
! tion stored in RCDF, the radial bin tops in Rbin and the minimum
! radial distance in RbinMin, all passed in COMMON/RDATA/.
! ----------------------------------------------------------------------

      subroutine rdistr(ridum,xidum,yidum)

      implicit none

!     ------------
!     EGS5 COMMONs
!     ------------
      include 'include/egs5_h.f'               ! Main EGS5 "header" file
      include 'include/egs5_uphiot.f'    ! COMMONs required by EGS5 code

!     ----------------------
!     Auxiliary-code COMMONs
!     ----------------------
      include 'auxcommons/aux_h.f'   ! Auxiliary-code "header" file
      include 'auxcommons/rdata.f'          ! Auxiliary-code COMMON

      real*8 ridum,xidum,yidum,rnnow                         ! Arguments
      real*8 rlow,angle                                ! Local variables
      integer i

!    ----------------------------------
!    Sample to determine the radius bin
!    ----------------------------------
      call randomset(rnnow)
      i=0
 1    continue
      i = i + 1
      if(rcdf(i) .le. rnnow) go to 1

      if (i .gt. 1) then
        rlow = rbin(i-1)
      else
        rlow = rbinmin
      end if

!     -------------------------------------
!     Sample to determine radius within bin
!     -------------------------------------
      call randomset(rnnow)
      ridum = rlow + rnnow*(rbin(i) - rlow)

!     -----------------------------------
!     Select the azimuthal angle randomly
!     -----------------------------------
      call randomset(rnnow)
      angle = TWOPI*rnnow
      xidum = ridum*cos(angle)
      yidum = ridum*sin(angle)

      return

      end

!--------------------------last line of rdistr.f------------------------
!-------------------------------sph2.f--------------------------------
! Version: 051219-1435
! Reference:
!-----------------------------------------------------------------------
!23456789|123456789|123456789|123456789|123456789|123456789|123456789|12

! ----------------------------------------------------------------------
! Auxiliary (geometry) subroutine for use with the EGS5 Code System
! ----------------------------------------------------------------------
! Subroutine sph2 is generally called from subroutine howfar whenever
! a particle is in a region bounded by two spheres.
! Both subroutines sphere and chgtr are called by subroutine sph2.
! ----------------------------------------------------------------------
! Input arguments:
! ----------------
!   nsp1 = ID number assigned to sphere including current position
!   nrg1 = ID number assigned to region particle trajectory
!          will lead into next
!   nsp2 = ID number assigned to sphere inside current position
!   nrg2 = Same as above, but for second sphere
! ----------------------------------------------------------------------

      subroutine sph2(nsp1,nrg1,nsp2,nrg2)

      implicit none

      real*8 tsph                                            ! Arguments
      integer nsp1,nrg1,nsp2,nrg2,ihit

      call sphere(nsp1,0,ihit,tsph)
      if (ihit .eq. 1) then                             ! Hits 1st sphere
        call chgtr(tsph,nrg1)
      end if
      call sphere(nsp2,1,ihit,tsph)
      if (ihit .eq. 1) then                             ! Hits 2nd sphere
        call chgtr(tsph,nrg2)
      end if

      return

      end

!-------------------------------sph2.f--------------------------------
!-------------------------------sphere.f--------------------------------
! Version: 051219-1435
! Reference:
!-----------------------------------------------------------------------
!23456789|123456789|123456789|123456789|123456789|123456789|123456789|12

! ----------------------------------------------------------------------
! Auxiliary (geometry) subroutine for use with the EGS5 Code System
! ----------------------------------------------------------------------
! This subroutine determines whether or not a sphere is intersected by 
! a particle trajectory and, if so, obtains the distance to the surface.
! The sphere is only defined about the origin of the coordinate system.
! ----------------------------------------------------------------------
! Input arguments:
! ----------------
!   isph = sphere ID number
!   infl = 1 means particle is inside sphere
!        = 0 means particle is outside sphere
! Output arguments:
! -----------------
!   ihit = 1 means particle intersects surface
!        = 0 means particle misses surface
!   tsph = distance to surface if intersected
!
! Note: Data in the form of the sphere-radius squared is required
!       and is transmitted via common/SPHDTA/.
! ----------------------------------------------------------------------

      subroutine sphere(isph,infl,ihit,tsph)

      implicit none

      include 'include/egs5_h.f'               ! Main EGS5 "header" file
      include 'include/egs5_stack.f'     ! COMMONs required by EGS5 code

      include 'auxcommons/aux_h.f'        ! Auxiliary-code "header" file
      include 'auxcommons/sphdta.f'              ! Auxiliary-code COMMON

      real*8 tsph                                            ! Arguments
      integer isph,infl,ihit

      real*8 a,b,c,b2,rtarg,delsph,rootsp              ! Local variables

      data delsph/1.D-8/

      ihit = 1                                          ! Assume a "hit"
      tsph = 0.

!     ----------------------------------------
!     Calculate the quadratic parameters a,b,c
!     ----------------------------------------
      a = 1.D0
      b = (x(np)*u(np) + y(np)*v(np) + z(np)*w(np))/a
      c = x(np)*x(np) + y(np)*y(np) + z(np)*z(np) - sprad2(isph)
 
      b2 = b*b
      rtarg = b2 - c
 
      if (rtarg .eq. 0.) then
        ihit = 0
        return                                      ! Imaginary solution
      end if

      if (abs(c) .lt. delsph*sprad2(isph)) then
        if (infl .eq. 0 .and. b .ge. 0.) then
          ihit = 0 
          return
        end if
        if (infl .eq. 1 .and. b .lt. 0.) then
          tsph = -2.D0*b/a
          return
        end if
      end if

      if ((infl .eq. 1 .and. c .ge. 0.) .or. 
     *    (infl .eq. 0 .and. c .le. 0.)) then
        tsph = delsph * sprad(isph)
        return
      end if
  
! ---------------------------------------------------------- 
! Calculate the root(s) and choose the smallest positive one
! ---------------------------------------------------------- 
      rootsp = sqrt(rtarg)
      if (c .lt. 0.) then
        tsph = (-b + rootsp)/a
        return
      else if (b .lt. 0.) then
        tsph = (-b - rootsp)/a
        return
      end if

      ihit = 0
      return

      end

!--------------------------last line of sphere.f------------------------
!-------------------------------sphtrn.f--------------------------------
! Version: 051219-1435
! Reference:
!-----------------------------------------------------------------------
!23456789|123456789|123456789|123456789|123456789|123456789|123456789|12

! ----------------------------------------------------------------------
! Auxiliary (geometry) subroutine for use with the EGS5 Code System
! ----------------------------------------------------------------------
! This subroutine determines whether or not a sphere is intersected by 
! a particle trajectory and, if so, obtains the distance to the surface.
! ----------------------------------------------------------------------
! Input arguments:
! ----------------
!   isph = sphere ID number
!   infl = 1 means particle is inside sphere
!        = 0 means particle is outside sphere
! Output arguments:
! -----------------
!   ihit = 1 means particle intersects surface
!        = 0 means particle misses surface
!   tsph = distance to surface if intersected
!
! Note: Data in the form of the sphere-radius squared is required
!       and is transmitted via common/SPHDTA/.
! ----------------------------------------------------------------------
! SPECIAL NOTE: This routine is for a sphere whose origin is translated
!               along z with respect to the usual EGS coordinate.
!               The variables xtrans, ytrans and ztrans must be defined
!               in AUSGAB and passed in common/TRNDTA/.
! ----------------------------------------------------------------------

      subroutine sphtrn(isph,infl,ihit,tsph)

      implicit none

      include 'include/egs5_h.f'               ! Main EGS5 "header" file

      include 'include/egs5_stack.f'     ! COMMONs required by EGS5 code

      include 'auxcommons/aux_h.f'   !      Auxiliary-code "header" file

      include 'auxcommons/sphdta.f'             ! Auxiliary-code COMMONs
      include 'auxcommons/trndta.f'

      real*8 tsph                                            ! Arguments
      integer isph,infl,ihit

      real*8 a,b,c,b2,rtarg,delsph,rootsp              ! Local variables

      data delsph/1.D-8/

      ihit = 1                                          ! Assume a "hit"
      tsph = 0.

!     ----------------------------------------
!     Calculate the quadratic parameters a,b,c
!     ----------------------------------------
      a = 1.D0
      b = (xtran*u(np) + ytran*v(np) + ztran*w(np))/a
      c = xtran*xtran + ytran*ytran + ztran*ztran - sprad2(isph)
 
      b2 = b*b
      rtarg = b2 - c
 
      if (rtarg .eq. 0.) then
        ihit = 0
        return                                      ! Imaginary solution
      end if

      if (abs(c) .lt. delsph*sprad2(isph)) then
        if(infl .eq. 0 .and. b .ge. 0.) then
          ihit = 0 
          return
        end if
        if (infl .eq. 1 .and. b .lt. 0.) then
          tsph = -2.D0*b/a
          return
        end if
      end if

      if ((infl .eq. 1 .and. c .ge. 0.) .or. 
     *    (infl .eq. 0 .and. c .le. 0.)) then
        tsph = delsph * sprad(isph)
        return
      end if
  
! ---------------------------------------------------------- 
! Calculate the root(s) and choose the smallest positive one
! ---------------------------------------------------------- 
      rootsp = sqrt(rtarg)
      if (c .lt. 0.) then
        tsph = (-b + rootsp)/a
        return
      else if (b .lt. 0.) then
        tsph = (-b - rootsp)/a
        return
      end if

      ihit = 0
      return

      end

!--------------------------last line of sphtrn.f------------------------
!-------------------------------swatch.f--------------------------------
! Version: 051219-1435
! Reference: SLAC version of SUBROUTINE WATCH (by W. R. Nelson)
!            (WATCH was written by D. W. O. Rogers (NRCC, Jan 1984))
!-----------------------------------------------------------------------
!23456789|123456789|123456789|123456789|123456789|123456789|123456789|12

! ----------------------------------------------------------------------
! Auxiliary subroutine for use with the EGS5 Code System
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
! SWATCH is the SLAC version of a code to print out particle-transport
! information.
!
!   IWATCH = 1   Information printed for EACH DISCRETE INTERACTION.
!   IWATCH = 2   Information printed for EACH STEP as well.
!
! The following calls should be made:
!
!   Before SHOWER loop:     IF(IWATCH>0) CALL SWATCH(-99,IWATCH)
!   Beginning of AUSGAB:    IF(IWATCH>0) CALL SWATCH(IARG,IWATCH)
!   After each CALL SHOWER: IF(IWATCH>0) CALL SWATCH(-1,IWATCH)
!   After SHOWER loop:      IF(IWATCH>0) CALL SWATCH(-88,IWATCH)
!
! with IWATCH passed to AUSGAB in COMMON/WATCH/IWATCH.  Note that
! OUTPUT is on UNIT=77 and the file that is produced is egs5job.out77 .
! ----------------------------------------------------------------------

      subroutine swatch(iarg,iwatch)

      implicit none

      include 'include/egs5_h.f'               ! Main EGS5 "header" file

      include 'include/egs5_epcont.f'    ! COMMONs required by EGS5 code
      include 'include/egs5_media.f'
      include 'include/egs5_misc.f'
      include 'include/egs5_stack.f'
      include 'include/egs5_useful.f'

      integer iarg,iwatch                                    ! Arguments

      integer ihstry,jr,n                              ! Local variables

      data ihstry/1/

!     ------------------------------------------
!     Initialize flags for extra calls to AUSGAB
!     ------------------------------------------
      if (iarg .eq. -99) then                       ! Before SHOWER loop
        open(UNIT=77,FILE='egs5job.out77',STATUS='unknown')
        do jr=1,25
          iausfl(jr) = 1
        end do

        iausfl(6) = 0
        iausfl(22) = 0
        iausfl(23) = 0
        iausfl(24) = 0

        write(77,101) iwatch
 101    FORMAT(' +---------------+',/, ' | SWATCH Output |',/,
     *         ' |  (IWATCH=',I1,')   |',/, ' +---------------+',/)

                                                        ! --------------
        return                                          ! Return to MAIN
                                                        ! --------------

      else if(iarg .eq. -88) then                    ! After SHOWER loop
        close(unit=77)
                                                        ! --------------
        return                                          ! Return to MAIN
                                                        ! --------------

      else if(iarg .eq. -1) then                ! After each CALL SHOWER
        write(77,102) ihstry
 102    FORMAT('*************** End-of-history',I3,' ***************',/)
        ihstry = ihstry + 1
                                                        ! --------------
        return                                          ! Return to MAIN
                                                        ! --------------
      end if

!     ---------------------------------
!     Calls made at beginning of AUSGAB
!     ---------------------------------
                                                      ! ----------------
      if (iarg .eq. 5 .or. iarg .lt. 0) return        ! Return to AUSGAB
                                                      ! ----------------
      if (iarg .eq. 0 .and. iwatch .eq. 2) then
        write(77,103)
 103    FORMAT(/,' Step about to occur (IARG=0):')
      else if (iarg .eq. 0) then
                                                      ! ----------------
        return                                        ! Return to AUSGAB
                                                      ! ----------------
      else if (iarg .eq. 1) then
        write(77,104)
 104    FORMAT(/,' Discard (IARG=1) -- AE<E<ECUT or AP<E<PCUT:')

      else if (iarg .eq. 2) then
        write(77,105)
 105    FORMAT(/,' Discard (IARG=2) -- E<AE or E<AP:')

      else if (iarg .eq. 3) then
        write(77,106)
 106    FORMAT(/,' Discard (IARG=3) -- user requested:')

      else if (iarg .eq. 4) then
        write(77,107)
 107    FORMAT(/,' Discard (IARG=4) -- EBINDA:')

      else if (iarg .eq. 6) then
        write(77,108)
 108    FORMAT(/,' Bremsstrahlung about to occur (IARG=6):')

      else if (iarg .eq. 7) then
        write(77,109)
 109    FORMAT(2X,' with resulting electron/photon (IARG=7):')

      else if (iarg .eq. 8) then
        write(77,110)
 110    FORMAT(/,' Moller about to occur (IARG=8):')

      else if (iarg .eq. 9) then
        write(77,111)
 111    FORMAT(2X,' with resulting electrons (IARG=9):')

      else if (iarg .eq. 10) then
        write(77,112)
 112    FORMAT(/,' Bhabha about to occur (IARG=10):')

      else if (iarg .eq. 11) then
        write(77,113)
 113    FORMAT(2X,' with resulting e- or e+ (IARG=11):')

      else if (iarg .eq. 12) then
        write(77,114)
 114    FORMAT(/,' Positron about to decay in flight (IARG=12):')

      else if (iarg .eq. 13) then
        write(77,115)
 115    FORMAT(2X,' with resulting photons (IARG=13):')

      else if (iarg .eq. 14) then
        write(77,116)
 116    FORMAT(/,' Positron annihilates at rest (IARG=14):')

      else if (iarg .eq. 15) then
        write(77,117)
 117    FORMAT(/,' Pair production about to occur (IARG=15):')

      else if (iarg .eq. 16) then
        write(77,118)
 118    FORMAT(2X,' with resulting pair (IARG=16):')

      else if (iarg .eq. 17) then
        write(77,119)
 119    FORMAT(/,' Compton about to occur (IARG=17):')

      else if (iarg .eq. 18) then
        write(77,120)
 120    FORMAT(2X,' with resulting photon/electron (IARG=18):')

      else if (iarg .eq. 19) then
        write(77,121)
 121    FORMAT(/,' Photoelectric about to occur (IARG=19):')

      else if (iarg .eq. 20) then
        write(77,122)
 122    FORMAT(2X,' with resulting electron (IARG=20):')

      else if (iarg .eq. 24) then
        write(77,123)
 123    FORMAT(/,' Rayleigh scattering occured (IARG=24):')
        write(77,124) iraylm(med(ir(np))),iraylr(ir(np))
 124    FORMAT(2X,' with IRAYLM=',I5,' and IRAYLR=',I5)
      end if

      eke = e(np)
      if (iq(np) .ne. 0) then
        eke = e(np) - RM
      end if

      write(77,125) np,iq(np),ir(np)
 125  FORMAT(' NP=',I3,7X,'IQ=',I3,3X,'IR=',I5)
      write(77,126) eke,wt(np)
 126  FORMAT(10X,'EKE/WT=',2G15.7)
      write(77,127) x(np),y(np),z(np)
 127  FORMAT(10X,' X/Y/Z=',3G15.7)
      write(77,128) u(np),v(np),w(np)
 128  FORMAT(10X,' U/V/W=',3G15.7)

      if (iarg .eq. 0 .and. iwatch .eq. 2) then
        write(77,129) ustep,tvstep,edep
 129    FORMAT(11X,'USTEP         TVSTEP         EDEP',
     *  /,9X,3G15.7)
      end if
                                                      ! ----------------
      if (np.eq.1 .or. iarg.eq.0) return              ! Return to AUSGAB
                                                      ! ----------------
      if (iarg .eq. 7 .or. iarg .eq. 9 .or.
     *    iarg .eq. 11 .or. iarg .eq. 13 .or.
     *    iarg .eq. 14 .or. iarg .eq. 16 .or.
     *    iarg .eq. 18 .or. iarg .le. 3) then

        n = np - 1
        eke = e(n)
        if (iq(n) .ne. 0) then
          eke = e(n) - RM
        end if
        if (iarg .le. 3) then
          write(77,130)
 130      FORMAT(' Now on top of stack:')
        else
          write(77,131)
 131      FORMAT('                    :')
        end if

        write(77,132) n,iq(n),ir(n)
 132    FORMAT(' NP=',I3,3X,'IQ=',I3,3X,'IR=',I5)
        write(77,133)eke,wt(n)
 133    FORMAT(10X,'EKE/WT=',2G15.7)
        write(77,134) x(n),y(n),z(n)
 134    FORMAT(10X,' X/Y/Z=',3G15.7)
        write(77,135) u(n),v(n),w(n)
 135    FORMAT(10X,' U/V/W=',3G15.7)

      end if
                                                      ! ----------------
      return                                          ! Return to AUSGAB
                                                      ! ----------------
      end

!--------------------------last line of swatch.f------------------------
!-------------------------------xyzbound.f------------------------------
! Version: 070116-1700
!-----------------------------------------------------------------------
!23456789|123456789|123456789|123456789|123456789|123456789|123456789|12

! ----------------------------------------------------------------------
! Auxiliary (geometry) subroutine for use with the EGS5 Code System
! ----------------------------------------------------------------------
! Subroutine xyzbound reads in an checks the boundary coordinates.
! ----------------------------------------------------------------------

      subroutine xyzbound(maxx,maxy,maxz)

      implicit none

      include 'auxcommons/aux_h.f'        ! Auxiliary-code "header" file
      include 'auxcommons/geoxyz.f'              ! Auxiliary-code COMMON

      real*8                                           ! Local variables
     * width
      integer
     * maxbd,maxx,maxy,maxz,i,ngroup,igroup,nn,nnn,in

 100  FORMAT(/,T20,'INPUT BOUNDARIES IN THE X DIRECTION')
 200  FORMAT(/,T20,'INPUT BOUNDARIES IN THE Y DIRECTION')
 300  FORMAT(/,T20,'INPUT BOUNDARIES IN THE Z DIRECTION')

1350  FORMAT(' INNER boundary for INDEX=',I3)
1360  FORMAT(F10.0)
1370  FORMAT(//,' BOUNDARY OUT OF ORDER**************')
1380  FORMAT(1X,T10,G15.7)
1390  FORMAT(' OUTER boundary for INDEX=',I3)
1420  FORMAT(' INITIAL boundary: ')
1440  FORMAT(1X,G15.7)
1460  FORMAT(' WIDTH in this group, NO. OF REGIONS in group: ')
1470  FORMAT(F10.0,I5)
1480  FORMAT(1X,G15.7,I5)
1500  FORMAT(/,' Program stopped in SUBROUTINE GetXYZ',/,
     *               ' because MXXPLNS, MXYPLNS, MXZPLNS too small')
1510  FORMAT(' Boundaries:',/,(6G15.7))

!     --------------------
!     Get the x-boundaries
!     --------------------
      maxbd = MXXPLNS
      write(6,100)
      if (maxx .gt. 0) then     ! Just pick up boundaries, one-at-a-time
        do i=1,maxx
          write(6,1350) i
!1350  FORMAT(' INNER boundary for INDEX=',I3)
          read(5,1360) xbound(i)
!1360  FORMAT(F10.0)
          if (i .ne. 1 .and. xbound(i) .le. xbound(i-1)) write(6,1370)
          write(6,1380) xbound(i)
!1380  FORMAT(1X,T10,G15.7)
        end do
        write(6,1390) maxx
!1390  FORMAT(' OUTER boundary for INDEX=',I3)
        read(5,1360) xbound(maxx+1)
        write(6,1380) xbound(maxx+1)
!1380  FORMAT(1X,T10,G15.7)
      else  ! maxx < 0,  Input GROUPS of regions 
            ! Assume maxbd set to MXXPLNS
        write(6,1420)
        read(5,1360) xbound(1)
        write(6,1440) xbound(1)
        ngroup = -maxx              ! Number of groups in this direction
        maxx = 0
        do igroup=1,ngroup
          write(6,1460)
          read(5,1470) width,nn
          if (nn .le. 0) nn = 1
          if (width .le. 0.) width = 1.D0
          write(6,1480) width,nn
          nnn = min(nn,maxbd-maxx) ! Ensures not adding too many regions
          if (nnn .ne. 0) then
            do in=maxx+1,maxx+nnn
              xbound(in+1) = xbound(in) + width
            end do
          end if
          if (nn .ne. nnn) then
            write(6,1500)
            stop
          end if
          maxx = maxx + nnn
        end do
      end if
      write(6,1510) (xbound(i),i=1,maxx+1)
      imax = maxx

!     --------------------
!     Get the y-boundaries
!     --------------------
      maxbd = MXYPLNS
      write(6,200)
      if (maxy .gt. 0) then     ! Just pick up boundaries, one-at-a-time
        do i=1,maxy
          write(6,1350) i
          read(5,1360) ybound(i)
          if (i .ne. 1 .and. ybound(i) .le. ybound(i-1)) write(6,1370)
          write(6,1380) ybound(i)
        end do
        write(6,1390) maxy
        read(5,1360) ybound(maxy+1)
        write(6,1380) ybound(maxy+1)
      else  ! maxy < 0,  Input GROUPS of regions 
            ! Assume maxbd set to MXYPLNS
        write(6,1420)
        read(5,1360) ybound(1)
        write(6,1440) ybound(1)
        ngroup = -maxy              ! Number of groups in this direction
        maxy = 0
        do igroup=1,ngroup
          write(6,1460)
          read(5,1470) width,nn
          if (nn .le. 0) nn = 1
          if (width .le. 0.) width = 1.D0
          write(6,1480) width,nn
          nnn = min(nn,maxbd-maxy) ! Ensures not adding too many regions
          if (nnn .ne. 0) then
            do in=maxy+1,maxy+nnn
              ybound(in+1) = ybound(in) + width
            end do
          end if
          if (nn .ne. nnn) then
            write(6,1500)
            stop
          end if
          maxy = maxy + nnn
        end do
      end if
      write(6,1510) (ybound(i),i=1,maxy+1)
      jmax = maxy

!     --------------------
!     Get the z-boundaries
!     --------------------
      maxbd = MXZPLNS
      write(6,300)
      if (maxz .gt. 0) then     ! Just pick up boundaries, one-at-a-time
        do i=1,maxz
          write(6,1350) i
          read(5,1360) zbound(i)
          if (i .ne. 1 .and. zbound(i) .le. zbound(i-1)) write(6,1370)
          write(6,1380) zbound(i)
        end do
        write(6,1390) maxz
        read(5,1360) zbound(maxz+1)
        write(6,1380) zbound(maxz+1)
      else  ! maxz < 0,  Input GROUPS of regions 
            ! Assume maxbd set to MXZPLNS
        write(6,1420)
        read(5,1360) zbound(1)
        write(6,1440) zbound(1)
        ngroup = -maxz              ! Number of groups in this direction
        maxz = 0
        do igroup=1,ngroup
          write(6,1460)
          read(5,1470) width,nn
          if (nn .le. 0) nn = 1
          if (width .le. 0.) width = 1.D0
          write(6,1480) width,nn
          nnn = min(nn,maxbd-maxz) ! Ensures not adding too many regions
          if (nnn .ne. 0) then
            do in=maxz+1,maxz+nnn
              zbound(in+1) = zbound(in) + width
            end do
          end if
          if (nn .ne. nnn) then
            write(6,1500)
            stop
          end if
          maxz = maxz + nnn
        end do
      end if
      write(6,1510) (zbound(i),i=1,maxz+1)
      kmax = maxz

      return

      end

!-------------------------------xyzbound.f------------------------------
!-------------------------------csdar.f---------------------------------
! Version: 060316-1345
!-----------------------------------------------------------------------
!23456789|123456789|123456789|123456789|123456789|123456789|123456789|12

!  returns the csda range for an electron or positron

      double precision function csdar(iq,e)

      implicit none

      include 'include/egs5_h.f'

      include 'pegscommons/rngspl.f'
      include 'pegscommons/dercon.f'

      integer iq
      double precision e

!  locals

      integer i
      double precision ke

      ke = e - RM
      call findi(erng,ke,nrng,i)
      if(iq .eq. -1) then
       csdar = arnge(i)+ke*(brnge(i)+ke*(crnge(i)+ke*drnge(i)))
      else
       csdar = arngp(i)+ke*(brngp(i)+ke*(crngp(i)+ke*drngp(i)))
      endif

      return
      end
!------------------------last line of csdar.f---------------------------
!-----------------------------------------------------------------------
!                       FUNCTION DCSEL
!  Version: 051219-1435
!  Reference:  Based on code developed and provided by F. Salvat
!              for computing GS Mult Scat with PW cross sections
!-----------------------------------------------------------------------
!23456789|123456789|123456789|123456789|123456789|123456789|123456789|12

      double precision function DCSEL(RMU)
!
!  This function computes the DCS in (cm**2/sr) by cubic spline inter-
!  polation in RMU=(1-cos(theta))/2.
!
!  includes required:
!    egs5_h
!    cdcsep
!    cdcspl

!  calls:
!    findi - get index

!  returns:
!    dcsel  - differential cross secion at given angle

      implicit none

      include 'include/egs5_h.f'
      include 'include/egs5_cdcsep.f'
      include 'include/egs5_cdcspl.f'

      double precision rmu
      integer i

      CALL FINDI(XMU,RMU,NREDA,I)
      DCSEL=EXP(RA(I)+RMU*(RB(I)+RMU*(RC(I)+RMU*RD(I))))

      RETURN
      END
!-----------------------------------------------------------------------
!                       FUNCTION DCSN
!  Version: 051219-1435
!  Reference:  Based on code developed and provided by F. Salvat
!              for computing GS Mult Scat with PW cross sections
!-----------------------------------------------------------------------
!23456789|123456789|123456789|123456789|123456789|123456789|123456789|12

      double precision function DCSN(RMU)
C
C     Integrand of the IL-th transport coefficient.

      implicit none

      include 'include/egs5_csplcf.f'

      double precision  rmu, x, pl(1000)

      X=1.0D0-2.0D0*RMU
      CALL LEGENP(X,PL,IL)
      DCSN=EXP(ASPL+RMU*(BSPL+RMU*(CSPL+RMU*DSPL)))*PL(IL)

      RETURN
      END
!----------------------------dcsstor.f----------------------------------
! Version: 060314-0825
!-----------------------------------------------------------------------
!23456789|123456789|123456789|123456789|123456789|123456789|123456789|12
!
!  To use the partial wave multiple scattering distribution from
!  Salvat in an efficient way, we need to call the precomputation
!  routine "elastino" after all the materials have been LAY'ed by
!  PEGS.  "elastino" typically treats one material at a time, and
!  uses data that is read in previously and loaded into commons
!  by "elinit", called once for each material by PEGS.  Thus, these 
!  two routines, dcsstor, and dcsload, are needed to first store
!  the material dependent differential cross sections, and then
!  load them back into the elastino commons as needed.

      subroutine dcsstor(ecut,emax)

      implicit none

      real*8 ecut, emax

      include 'include/egs5_h.f'
      include 'include/egs5_cdcsep.f'
      include 'include/egs5_mscon.f'

      include 'pegscommons/dcsstr.f'
      include 'pegscommons/molvar.f'
      include 'pegscommons/pmcons.f'
      include 'pegscommons/mxdatc.f'
      include 'pegscommons/mscom.f'

      integer i,j
C
      nsdcs = nsdcs + 1
      atomd(nsdcs) = an * rho / wm
      efrch(nsdcs) = efrach
      efrcl(nsdcs) = efracl
      do i = 1, 24
        mednam(nsdcs,i) = medium(i)
      end do
      do i = 1, negrid
        secs(nsdcs,i) = ecs(i)
        setcs1(nsdcs,i) = etcs1(i)
        setcs2(nsdcs,i) = etcs2(i)
        spcs(nsdcs,i) = pcs(i)
        sptcs1(nsdcs,i) = ptcs1(i)
        sptcs2(nsdcs,i) = ptcs2(i)
        do j= 1, nreda
          sedcs(nsdcs,i,j) = edcs(i,j)
          spdcs(nsdcs,i,j) = pdcs(i,j)
        end do
      end do

      egrdlo(nsdcs) = ecut
      egrdhi(nsdcs) = emax
      nlegmd(nsdcs) = nleg0

      return
      end
!-------------------------last line of dcsstor.f------------------------

!----------------------------dcsload.f----------------------------------
! Version: 051219-1435
!-----------------------------------------------------------------------
!23456789|123456789|123456789|123456789|123456789|123456789|123456789|12
      subroutine dcsload(n,ecut,emax,nleg0)
C
      include 'include/egs5_h.f'
      include 'include/egs5_cdcsep.f'

      include 'pegscommons/dcsstr.f'
      include 'pegscommons/mxdatc.f'

      integer n, nleg0
      double precision ecut, emax

      integer i,j
C
      do i = 1, 24
        medium(i) = mednam(n,i)
      end do
      do i = 1, negrid
        ecs(i) = secs(n,i)
        etcs1(i) = setcs1(n,i)
        etcs2(i) = setcs2(n,i)
        pcs(i) = spcs(n,i)
        ptcs1(i) = sptcs1(n,i)
        ptcs2(i) = sptcs2(n,i)
        do j= 1, nreda
          edcs(i,j) = sedcs(n,i,j)
          pdcs(i,j) = spdcs(n,i,j)
        end do
      end do

      ecut = egrdlo(n)
      emax = egrdhi(n)
      nleg0 = nlegmd(n)

      return
      end
!-------------------------last line of dcsload.f------------------------
!-----------------------------------------------------------------------
!                       SUBROUTINE DCSTAB
!  Version: 051219-1435
!  Reference:  Based on code developed and provided by F. Salvat
!              for computing GS Mult Scat with PW cross sections
!-----------------------------------------------------------------------
!23456789|123456789|123456789|123456789|123456789|123456789|123456789|12

      SUBROUTINE DCSTAB(E,IELEC)
C
C  This subroutine computes a table of the molecular elastic DCS for
C  electrons (IELEC=-1) or positrons (IELEC=+1) with kinetic energy
C  E (eV) by log-log cubic spline interpolation in E.
C
      include 'include/egs5_h.f'
      include 'include/egs5_cdcsep.f'
      include 'include/egs5_cdcspl.f'
      include 'include/egs5_coefgs.f'

      integer ielec
      double precision e

      integer ia, ie
      double precision el
      double precision X(NEGRID),Y(NEGRID),
     1          A(NEGRID),B(NEGRID),C(NEGRID),D(NEGRID)
C
      DO IE=1,NEGRID
        X(IE)=LOG(ET(IE))
      ENDDO
      EL=LOG(E)
C
      DO IA=1,NREDA
        DO IE=1,NEGRID
          IF(IELEC.EQ.-1) THEN
            Y(IE)=LOG(EDCS(IE,IA))
          ELSE
            Y(IE)=LOG(PDCS(IE,IA))
          ENDIF
        ENDDO
        CALL SPLINE(X,Y,A,B,C,D,0.0D0,0.0D0,NEGRID)
        CALL FINDI(X,EL,NEGRID,IE)
        DCSIL(IA)=A(IE)+EL*(B(IE)+EL*(C(IE)+EL*D(IE)))
        DCSI(IA)=EXP(DCSIL(IA))
      ENDDO
C
      CALL SPLINE(XMU,DCSIL,RA,RB,RC,RD,0.0D0,0.0D0,NREDA)
      RETURN
      END
!-----------------------------------------------------------------------
!                             SUBROUTINE ELASTINO  
!  Version: 060313-1235
!  Reference:  Based on code developed and provided by F. Salvat
!              for computing GS Mult Scat with PW cross sections
!-----------------------------------------------------------------------
!23456789|123456789|123456789|123456789|123456789|123456789|123456789|12

      subroutine elastino
C
C  This program computes multiple elastic scattering distributions of
C  electrons and positrons in matter. The DCS is obtained by log-log
C  cubic spline interpolation of atomic DCSs from an elastic scatter-
C  ing database, which was generated by running the code ELSEPA with
C  Desclaux's multiconfiguration Dirac-Fock atomic electron density and
C  a Fermi nuclear charge distribution. For electrons, the exchange
C  potential of Furness and McCarthy was used.
C
C                    Francesc Salvat. Barcelona/Ann Arbor. June, 2001.
C
      implicit none

      include 'include/egs5_h.f'
      include 'include/egs5_cdcsep.f'
      include 'include/egs5_coefgs.f'
      include 'include/egs5_mscon.f'
      include 'include/egs5_useful.f'
      include 'include/egs5_media.f'

      include 'pegscommons/dcsstr.f'

      double precision Y(NREDA),RA(NREDA),RB(NREDA),RC(NREDA),RD(NREDA)
      double precision Y1(NREDA),Y2(NREDA)

      integer ntb
      PARAMETER (NTB=NFIT1)
      double precision  XGR(NTB),YC(NTB),Y0(NTB),
     1          YY(NREDA),TA(NREDA),TB(NREDA),TC(NREDA),TD(NREDA)

      logical printDiag, longDiag, otherM, uniInt
      integer i, j, n, ik1, iener, ipart, ielec, nleg, ntermm, nterm
      integer ia, didGS
      integer nleg0
      double precision e, s, thdeg,raddeg
      double precision sumt, sum, sump, dsum, xl, xu, xx, st, dmu
      double precision k1start(2), dk1(2), k1, ecut, emax

      double precision DCSEL

      raddeg = 180/3.14159
      longDiag = .false.
      printDiag = .true.
      otherM = .false.
      uniInt = .false.

!  rewrite the pre-amble to the msfit file to note the new values
!  of the characteristic dimension, etc.

      didGs = 1
      write(17,*) nsdcs
      do n = 1, nsdcs
        write(17,5001) (mednam(n,i),i=1,24)
        write(17,*) didGS,charD(n),efrch(n),efrcl(n),
     *              egrdhi(n),egrdlo(n)
      end do
5001  format(' MEDIUM=',24A1)

!  open a new pegs5 listing file for diagnostics
!  ***  Get the K1 range constants and write the ladder

      open(UNIT=26,FILE='pgs5job.GSlst',STATUS='unknown')

      dk1(1) = dexp( (1.d0/(NK1-1.d0)) * dlog(k1maxe/k1mine) )
      dk1(2) = dexp( (1.d0/(NK1-1.d0)) * dlog(k1maxp/k1minp) )
      k1start(1) = k1mine
      k1start(2) = k1minp

      write(17,*) 'Scattering strength ladder for this run'
      write(17,*) 'IK1       K1(e-)         K1(e+)'
      do ik1 = 1, NK1
        write(17,*) ik1, 
     &              k1start(1) * dk1(1) ** (ik1-1),
     &              k1start(2) * dk1(2) ** (ik1-1)
      end do

      if(printDiag) write(26,1001)
1001  format('In Elastino to get Multiple Scattering distribution')

!  ***  Loop over all the materials in the problem
!
      do n = 1, nsdcs

!  ***  load the cross section data for this material

      call dcsload(n,ecut,emax,nleg0)
      call inigrd(ecut,emax,0)

!  ***  Loop over the two particle types
!
      do ipart = 1,2
!
        NLEG = MIN(nleg0,NGT)
!
        IF(ipart.EQ.1) THEN
          ielec = -1
          if(printDiag) WRITE(26,*) 'Projectile: electron.'
        ELSE
          ielec = +1
          if(printDiag) WRITE(26,*) 'Projectile: positron.'
        ENDIF
!
!  Loop over the energy steps 
!
        do iener = 1,nmscate
!
          e = mscate(iener)
          if(e .gt. 1.d8) go to 1005
C
C  ****  Now, we generate a table of the DCS for the selected projectile
C        and energy, by log-log cubic spline interpolation in E.
C
          CALL DCSTAB(E,IELEC)
C
C  ****  Here we compute the Goudsmit-Saunderson transport coefficients,
C        from which we can obtain the angular distribution after a given
C        path length (in units of the mean free path).
C
          CALL GSCOEF(NLEG)
C
C  store the scattering power (units on tcs1 are cm^2 from GSCOEF)
C
          if(printDiag) then
          WRITE(26,*) 'Kinetic energy (MeV) =',E*1.d-6
          WRITE(26,*) ' Total cross section (cm^-1) =',CS*atomd(n)
          WRITE(26,*) ' 1st transport cross section =',TCS1*atomd(n)
          if(longDiag) WRITE(26,*) ' 2nd transport cross section =',TCS2
          endif

!  loop over all the values of mubar that we're likely to
!  see.  This is actually a loop over a range of distances...

          do ik1 = 1, NK1

!  ****  get s from k1 

            k1 = k1start(ipart) * dk1(ipart) ** (ik1-1)

!            CALL GSTEST  ! Numerical check not available this version
!  ****  We are now ready to evaluate GS distributions for any path
!        length.  Get the pathlength (in mfp's) at this energy

            s = k1 * cs / tcs1
!
            if(printDiag) then
            WRITE(26,*) ' For this step, K1 =',k1
            WRITE(26,*) '  At this K1, path length (mfps)=',S
            WRITE(26,*) '  At this K1, path length (cm) =',
     &                                                     S/CS/atomd(n)
            endif
            if(longDiag) then
              WRITE(26,*) '# '
              WRITE(26,*) '# Differential cross section'
              WRITE(26,*) '# theta(deg)       mu        dcs(cm^2/sr)'
              DO IA=1,NREDA
                WRITE(26,'(1P,4E14.6)') TH(IA),XMU(IA),DCSI(IA)
              ENDDO
              WRITE(26,*) '# Goudsmit-Saunderson distribution, PDF(mu)'
              IF(IELEC.EQ.1) THEN
                WRITE(26,*) '# Projectile: positron'
              ELSE
                WRITE(26,*) '# Projectile: electron'
              ENDIF
              WRITE(26,*) '# Number of terms in Legendre series =',NLEG
              WRITE(26,*) '# '
              WRITE(26,*) '# theta(deg)       mu           PDF(mu)',
     1       '       S*DCS       NTERM '
            endif

            nterm = nleg
            CALL GSDIST(S,0.0D0,Y(1),NTERM)
            NTERMM=NTERM
            DO I=1,NREDA
              CALL GSDIST(S,XMU(I),Y(I),NTERM)
              if(longDiag) then
                THDEG=ACOS(1.0D0-2.0D0*XMU(I))*RADDEG
                WRITE(26,'(1P,4E14.6,I7)') THDEG,XMU(I),Y(I),
     1         2.0D0*S*DCSEL(XMU(I))/CS0,NTERM
              endif
              NTERMM=MAX(NTERMM,NTERM)
            ENDDO
    2       CONTINUE
            if(printDiag) WRITE(26,*) ' Number of terms used: ',NTERMM
            !  this is a temp fix until single scattering
            if(nterm.lt.0) nterm = ntermm
C
C  ****  Finally, we check that the normalization is correct.
C
C  ... we first compute the integral of the continuous distribution,
            CALL SPLINE(XMU,Y,RA,RB,RC,RD,0.0D0,0.0D0,NREDA)
            CALL INTEG(XMU,RA,RB,RC,RD,0.0D0,XMU(NREDA),SUMP,NREDA)
C  ... and we add the probability of no scattering, EXP(-S).
            if(s.lt.100)  then
              SUM=SUMP+EXP(-S)
            else
              sum = sump
            endif
C
            if(printDiag) then
            WRITE(26,*) ' Normalization =',SUM
            WRITE(26,*) ' No-scatter probability = ',sum-sump
            endif
            probns(ipart,iener,ik1) = sum-sump
C
C  ****  Cumulative probability distribution and moments.
C        Equiprobable intervals.
C
            DSUM=SUMP/DBLE(NBFIT)
            XGR(1)=0.0D0
            DO I=2,NBFIT
              SUMT=(I-1)*DSUM
              XL=XGR(I-1)
              XU=1.0D0
1234          XX=0.5D0*(XL+XU)
              CALL INTEG(XMU,RA,RB,RC,RD,0.0D0,XX,ST,NREDA)
              IF(ST.GT.SUMT) THEN
                XU=XX
              ELSE
                XL=XX
              ENDIF
              IF(ABS(XU-XL).GT.1.0D-6*XX) GO TO 1234
              XGR(I)=XX
            ENDDO
C
C  Now get extra bins in the last angle, set uniformly
C
            dmu = (1.0d0 - xgr(NBFIT)) / NEXFIT
            do i=1,NEXFIT
              j = NBFIT + i
              xgr(j) = xgr(NBFIT) + i * dmu
            end do
C
C now get the cum dist for all angles
C
            DO I=1,NREDA
              YY(I)=Y(I)/SUM
            ENDDO
            CALL SPLINE(XMU,YY,TA,TB,TC,TD,0.0D0,0.0D0,NREDA)
            nterm = nleg
            DO I=1,NTB
              CALL GSDIST(S,XGR(I),YC(I),NTERM)
              if(i.le.NBFIT) then
                y0(i) = (i-1)*dsum/sum
              else 
                CALL INTEG(XMU,TA,TB,TC,TD,0.0D0,XGR(I),Y0(I),NREDA)
              endif
            ENDDO
            
            call fitms(xgr,yc,y0,ntb,ipart,iener,ik1) 

           ! other moments...
            if(otherM) then
              DO I=1,NREDA
                YY(I)=YY(I)*XMU(I)
              ENDDO
              CALL SPLINE(XMU,YY,TA,TB,TC,TD,0.0D0,0.0D0,NREDA)
              DO I=1,NTB
                CALL INTEG(XMU,TA,TB,TC,TD,0.0D0,XGR(I),Y1(I),NREDA)
              ENDDO

              DO I=1,NREDA
                YY(I)=YY(I)*XMU(I)
              ENDDO
              CALL SPLINE(XMU,YY,TA,TB,TC,TD,0.0D0,0.0D0,NREDA)
              DO I=1,NTB
                CALL INTEG(XMU,TA,TB,TC,TD,0.0D0,XGR(I),Y2(I),NREDA)
              ENDDO

              if(printDiag) then
              WRITE(26,*) 
     1 '# Cumulative Goudsmit-Saunderson distributions'
              IF(IELEC.EQ.1) THEN
                WRITE(26,*) '# Projectile: positron'
              ELSE
                WRITE(26,*) '# Projectile: electron'
              ENDIF
              WRITE(26,*) '# Kinetic energy (eV) =',E
              WRITE(26,*) '# Path length/mfp =',S
              WRITE(26,*) '# Number of terms in Legendre series =',NLEG
              if(longDiag) then
              WRITE(26,*) '# '
              WRITE(26,3001)
 3001 FORMAT(' # theta(deg)       mu           PDF(mu)',
     1  '        CDF         <MU*PDF>     <MU*MU*PDF>')
              DO I=1,NTB
                THDEG=ACOS(1.0D0-2.0D0*XGR(I))*RADDEG
                WRITE(26,'(1P,6E24.16)') 
     1                 THDEG,XGR(I),YC(I),Y0(I),Y1(I),Y2(I)
              ENDDO
              endif
              endif
            endif
C
C  ****  Cumulative probability distribution and moments
C        for Uniform intervals.
            if(uniInt) then

              DMU=1.0D0/DBLE(NTB-1)
              DO I=1,NTB
                XGR(I)=(I-1)*DMU
              ENDDO

              DO I=1,NREDA
                YY(I)=Y(I)/SUM
              ENDDO
              CALL SPLINE(XMU,YY,TA,TB,TC,TD,0.0D0,0.0D0,NREDA)
              DO I=1,NTB
                CALL GSDIST(S,XGR(I),YC(I),NTERM)
                CALL INTEG(XMU,TA,TB,TC,TD,0.0D0,XGR(I),Y0(I),NREDA)
              ENDDO

              call fitms(xgr,yc,y0,ntb,ipart,iener,ik1) 

C      other moments...
              if(otherM) then
                DO I=1,NREDA
                  YY(I)=YY(I)*XMU(I)
                ENDDO
                CALL SPLINE(XMU,YY,TA,TB,TC,TD,0.0D0,0.0D0,NREDA)
                DO I=1,NTB
                  CALL INTEG(XMU,TA,TB,TC,TD,0.0D0,XGR(I),Y1(I),NREDA)
                ENDDO

                DO I=1,NREDA
                  YY(I)=YY(I)*XMU(I)
                ENDDO
                CALL SPLINE(XMU,YY,TA,TB,TC,TD,0.0D0,0.0D0,NREDA)
                DO I=1,NTB
                  CALL INTEG(XMU,TA,TB,TC,TD,0.0D0,XGR(I),Y2(I),NREDA)
                ENDDO
C
                if(printDiag) then
                WRITE(26,*)
     1 '# Cumulative Goudsmit-Saunderson distributions'
                IF(IELEC.EQ.1) THEN
                  WRITE(26,*) '# Projectile: positron'
                ELSE
                  WRITE(26,*) '# Projectile: electron'
                ENDIF
                WRITE(26,*) '# Kinetic energy (eV) =',E
                WRITE(26,*) '# Path length/mfp =',S
                WRITE(26,*) '# Number of terms in Legendre series=',NLEG
                if(longDiag) then
                WRITE(26,*) '# '
                WRITE(26,3002)
 3002 FORMAT(' # theta(deg)       mu           PDF(mu)',
     1   '        CDF         <MU*PDF>     <MU*MU*PDF>')
                DO I=1,NTB
                  THDEG=ACOS(1.0D0-2.0D0*XGR(I))*RADDEG
                  WRITE(26,'(1P,6E24.16)') 
     1                 THDEG,XGR(I),YC(I),Y0(I),Y1(I),Y2(I)
                ENDDO
                endif
                endif
              endif        !  get other moments
            endif          !  use uniform ints

          end do	   !  end path length loop
        end do	           !  end energy loop
      end do               !  end particle types loop

1005  call wmsfit(k1start,dk1)

1010  end do               !  end material loop

      close(26)

      return
      END
!-----------------------------------------------------------------------
!                       SUBROUTINE ELINIT
!  Version: 060317-1425
!  Reference:  Based on code developed and provided by F. Salvat
!              for computing GS Mult Scat with PW cross sections
!-----------------------------------------------------------------------
!23456789|123456789|123456789|123456789|123456789|123456789|123456789|12

      SUBROUTINE ELINIT(Z,STF,NELEM)
C
C  This subroutine reads atomic elastic cross sections for electrons and
C  positrons from the database files and determines the molecular cross
C  section as the incoherent sum of atomic cross sections.

C  Input arguments:
C    Z (1:NELEM) ..... atomic numbers of the elements in the compound.
C    STF (1:NELEM) ... stoichiometric indices.
C    NELEM ........... number of different elements.
C
      implicit none

      include 'include/egs5_h.f'
      include 'include/egs5_cdcsep.f'
      include 'include/egs5_ms.f'
      include 'include/egs5_uphiot.f'

      include 'pegscommons/scpspl.f'
      include 'pegscommons/mscom.f'
      include 'pegscommons/dercon.f'

      integer nelem, IZ(MXEPERMED)
      double precision STF(NELEM), Z(NELEM), zcor(20)
C
      integer i,ie,ia, iel, ielec, izz, izr, ns, ns1,ns2,ns3
      double precision zplus1, stff, enr, csin, csin1, csin2

      CHARACTER*1 LIT10(10),LIT1,LIT2,LIT3
      DATA LIT10/'0','1','2','3','4','5','6','7','8','9'/
      CHARACTER*24 FILE1,FILE2
C
      double precision EGRID(16)
      DATA EGRID/1.0D0,1.25D0,1.50D0,1.75D0,2.00D0,2.50D0,3.00D0,
     1 3.50D0,4.00D0,4.50D0,5.00D0,6.00D0,7.00D0,8.00D0,9.00D0,
     2 1.00D1/

      integer igrid
      double precision fgrid, e

      double precision g1e
      logical printDiag

      printDiag = .false.

C
      if(et(1).ne.1.d2) then
      IE=0
      IGRID=0
      FGRID=100.0D0
   10 IGRID=IGRID+1
      E=EGRID(IGRID)*FGRID
      IF(IGRID.EQ.16) THEN
        IGRID=1
        FGRID=10.0D0*FGRID
      ENDIF
      IE=IE+1
      ET(IE)=E
C     WRITE(6,'(I5,1P,3E15.7)') IE,ET(IE)
      IF(IE.LT.NEGRID) GO TO 10
      endif

!  first move the atomic numbers into an integer array
!  also compute (Z+1)/Z, so we can impose Z(Z+1) on
!  the cross sections.

      do i=1,nelem
        iz(i) = z(i)
        zcor(i) =  (z(i) + fudgeMS) / z(i)
      end do
C
C  initialize...
C
      DO IE=1,NEGRID
        ECS(IE)=0.0D0
        ETCS1(IE)=0.0D0
        ETCS2(IE)=0.0D0
        PCS(IE)=0.0D0
        PTCS1(IE)=0.0D0
        PTCS2(IE)=0.0D0
        DO IA=1,NREDA
          EDCS(IE,IA)=0.0D0
          PDCS(IE,IA)=0.0D0
        ENDDO
      ENDDO
C
C  ****  Read atomic DCS tables and compute the molecular DCS as the
C        incoherent sum of atomic DCSs.
C
      DO IEL=1,NELEM
        IZZ=IZ(IEL)
        STFF=STF(IEL)
        zplus1 = zcor(iel)
        NS=IZ(IEL)
        IF(NS.GT.999) NS=999
        NS1=NS-10*(NS/10)
        NS=(NS-NS1)/10
        NS2=NS-10*(NS/10)
        NS=(NS-NS2)/10
        NS3=NS-10*(NS/10)
        LIT1=LIT10(NS1+1)
        LIT2=LIT10(NS2+1)
        LIT3=LIT10(NS3+1)
C
        FILE1='data/dcslib/eeldx'//LIT3//LIT2//LIT1//'.tab'
        OPEN(UNIT=31,FILE=FILE1,STATUS='old')
        FILE2='data/dcslib/peldx'//LIT3//LIT2//LIT1//'.tab'
        OPEN(UNIT=32,FILE=FILE2,STATUS='old')
C
        DO IE=1,NEGRID
          READ(31,'(I3,I4,1P,E10.3,5E12.5)')
     1      IELEC,IZR,ENR,csin,csin1,csin2
          ECS(IE) = ECS(IE) + zplus1 * stff * csin
          ETCS1(IE) = ETCS1(IE) + zplus1 * stff * csin1
          ETCS2(IE) = ETCS2(IE) + zplus1 * stff * csin2
          if(printDiag) WRITE(6,'(I3,I4,1P,E10.3,5E12.5)')
     1      IELEC,IZR,ENR,ECS(IE),ETCS1(IE),ETCS2(IE)
          IF(IELEC.NE.-1.OR.IZR.NE.IZZ.OR.ABS(ENR-ET(IE)).GT.1.0D-3)
     1      STOP 'Corrupted data file.'
          READ(31,'(1P,10E12.5)') (DCSI(IA),IA=1,NREDA)
          DO IA=1,NREDA
            EDCS(IE,IA)=EDCS(IE,IA)+zplus1*STFF*DCSI(IA)
          ENDDO
C
          READ(32,'(I3,I4,1P,E10.3,5E12.5)')
     1      IELEC,IZR,ENR,csin,csin1,csin2
          PCS(IE) = PCS(IE) + zplus1 * stff * csin
          PTCS1(IE) = PTCS1(IE) + zplus1 * stff * csin1
          PTCS2(IE) = PTCS2(IE) + zplus1 * stff * csin2
          if(printDiag) WRITE(6,'(I3,I4,1P,E10.3,5E12.5)')
     1      IELEC,IZR,ENR,PCS(IE),PTCS1(IE),PTCS2(IE)
          IF(IELEC.NE.+1.OR.IZR.NE.IZZ.OR.ABS(ENR-ET(IE)).GT.1.0D-3)
     1      STOP 'Corrupted data file.'
          READ(32,'(1P,10E12.5)') (DCSI(IA),IA=1,NREDA)
          DO IA=1,NREDA
            PDCS(IE,IA)=PDCS(IE,IA)+zplus1*STFF*DCSI(IA)
          ENDDO
        ENDDO
C
        CLOSE(31)
        CLOSE(32)
      ENDDO

!  get spline coefficients for the scattering power.  because of the
!  switch to a different cross section above 100 MeV, we fill in
!  Wentzel points and then spline

      do ie = 1,negrid-1
        etl(ie) = dlog(et(ie))
        etcs1(ie) = dlog(etcs1(ie))
        ptcs1(ie) = dlog(ptcs1(ie))
      end do
      etcs1(negrid) = dlog(g1e(-1,110.d0+RM,-1))
      ptcs1(negrid) = dlog(g1e(+1,110.d0+RM,-1))
      etcs1(negrid+1) = dlog(g1e(-1,120.d0+RM,-1))
      ptcs1(negrid+1) = dlog(g1e(+1,120.d0+RM,-1))
      etcs1(negrid+2) = dlog(g1e(-1,130.d0+RM,-1))
      ptcs1(negrid+2) = dlog(g1e(+1,130.d0+RM,-1))
      etl(negrid) = dlog(110.d6)
      etl(negrid+1) = dlog(120.d6)
      etl(negrid+2) = dlog(130.d6)
      
      call spline(etl,etcs1,ag1e,bg1e,cg1e,dg1e,0.d0,0.d0,negrds)
      call spline(etl,ptcs1,ag1p,bg1p,cg1p,dg1p,0.d0,0.d0,negrds)

      RETURN
      END
!----------------------------esteplim.f---------------------------------
! Version: 060318-1800
! Reference: SLAC-R-730/KEK-2005-8
!-----------------------------------------------------------------------
!23456789|123456789|123456789|123456789|123456789|123456789|123456789|12
!
!  get energy dependent values of estepe, the fractional energy loss
!  which assures a tolerance of "etol" in the total energy loss, the
!  scattering power, the total hard scattering probability, and the 
!  hard collision mean free path
!
!  (loops over hard collision variables not yet implemented)
!
      subroutine esteplim

!  globals

      implicit none

      real*8 g1e, sptote, sptotp, csdar

      include 'include/egs5_h.f'
      include 'include/egs5_mscon.f'

      include 'pegscommons/dercon.f'
      include 'pegscommons/molvar.f'
      include 'pegscommons/rngspl.f'
      include 'pegscommons/thres2.f'
      
!  locals

      integer i, ivar
      real*8 e1, e2, e1t, e2t, etol, fspe(NESCPW), rnge(NESCPW),
     *       aspe(NESCPW), bspe(NESCPW), cspe(NESCPW), dspe(NESCPW), 
     *       deltaE, trap, anal, dloge
      data etol/1.d-3/

      !  load the energy array 
      nrng = NESCPW
      erng(1) = log(mscate(1)*1.d-6)
      erng(nrng) = log(mscate(nmscpw)*1.d-6)
      dloge = (erng(nrng) - erng(1)) / (nrng - 1)
      do i = 2, nrng-1
        erng(i) = erng(1) + (i-1) * dloge
      end do

!  do 5 loops:
!  1.)  load electron dedx total and use that to get a range table
!  2.)  load positron dedx total and use that to get a range table
!  3.)  load dedx restricted and get the first limit
!  4.)  load the scattering power, and use that to get limit 2
!  5.)  do the mfp and total scattering limit together (not implemented)

      do ivar = 1,4
        do i=1,nrng
          if (ivar.eq.1) then
            erng(i) = exp(erng(i))
            estepl(i) = .50
          end if 
          e1t = erng(i) + RM

          if (ivar.eq.1) then
            fspe(i) = rlc / sptote(e1t,ae,ap)
          else if (ivar.eq.2) then
            fspe(i) = rlc / sptotp(e1t,ae,ap)
          else if (ivar.eq.4) then
            fspe(i) = g1e(-1,e1t,0) * (rlc / sptote(e1t,ae,ap))
          else if (ivar.eq.5) then
            fspe(i) = 1.d0
          end if
        end do

        !  spline the function for local use

        if (ivar.ne.3) then
          call spline(erng,fspe,aspe,bspe,cspe,dspe,0.d0,0.d0,nrng)
        end if

        !  get the electron or positron CSDA range
        if (ivar.le.2) then
          e1 = erng(1)
          if (ivar.eq.1) then
            rnge(1) = rlc / sptote(e1t,ae,ap) * e1
          else
            rnge(1) = rlc / sptotp(e1t,ae,ap) * e1
          end if
          do i = 2, nrng
            e2 = erng(i)
            call integ(erng,aspe,bspe,cspe,dspe,e1,e2,anal,nrng)
            rnge(i) = rnge(i-1) + dabs(anal)
            e1 = e2
          end do
          !  get global splines for use later is csdar
          if (ivar.eq.1) then
            call spline(erng,rnge,arnge,brnge,crnge,drnge,
     *                                         0.d0,0.d0,nrng)
          else
            call spline(erng,rnge,arngp,brngp,crngp,drngp,
     *                                         0.d0,0.d0,nrng)
          end if

        !  do the energy limit 
        else 
          !  start at 4 because of spline issues at ends
          do i = 4, nrng-3
            e1 = erng(i)
            !  trap possible glitches around 100 MeV
            if (ivar.eq.4 .and. e1.gt.60.d0 .and. e1.lt.140.d0) then
              if (i.gt.4) then 
                estepl(i) = estepl(i-1)
                go to 200
              end if
            end if
            e2 = e1 * (1.d0 - estepl(i))
            if (e2.lt.erng(1)) then
              e2 = erng(1)
              estepl(i) = 1.d0 - e2/e1
            endif
               
            e1t = e1 + RM
 100        e2t = e2 + RM
            if (ivar.le.4) then
              deltaE = (e1 - e2)
              if (ivar.eq.3) then
                trap = rlc / sptote(e1t,ae,ap) + rlc / sptote(e2t,ae,ap)
                anal = csdar(-1,e1t) - csdar(-1,e2t)
              else 
                trap = rlc * g1e(-1,e1t,0) / sptote(e1t,ae,ap)
                trap = trap + rlc * g1e(-1,e2t,0) / sptote(e2t,ae,ap)
                call integ(erng,aspe,bspe,cspe,dspe,e2,e1,anal,nrng)
              end if
              trap = deltaE * trap / 2.d0
              !  this should never happen now except in development
              if (anal.eq.0.d0) then
                write(6,*) 'ERROR in esteplim -- integral = 0.d0'
                stop
              ! if we're within the tolerance, get next energy
              else if (dabs((trap - anal) / anal) .le. etol) then
                ! convergence -- ignore the discreteness from using 5% 
                go to 200
              else
              ! else, store previous, and try something shorter
                e2 = e1 * (1.d0 - estepl(i)*.95)
                estepl(i) = 1.d0 - e2/e1
                !  trap numerical problems -- estepe is smooth
                if(i.gt.4 .and. estepl(i).lt.estepl(i-1)/2.d0) go to 200
                go to 100
              endif
            else
          !  do the mfp limit 
            end if 
 200        continue
          end do
          !  skip regions where splines may be bad
          do i = 1, 3
            estepl(i) = estepl(4)
            estepl(nrng-3+i) = estepl(nrng-3)
          end do
        end if 

      end do
           
      ! get splines for use in pegs
      call spline(erng,estepl,aeste,beste,ceste,deste,0.d0,0.d0,nrng)
      
      return
      end
!-----------------------last line of esteplim.f-------------------------
!----------------------------estepmax.f---------------------------------
! Version: 060317-1045
!-----------------------------------------------------------------------
!23456789|123456789|123456789|123456789|123456789|123456789|123456789|12

!  returns the computed value for estepe to assure a 0.1% tolerance in
!  integrating G1 over an energy hinge

      double precision function estepmax(e)

      implicit none

      include 'include/egs5_h.f'

      include 'pegscommons/rngspl.f'
      include 'pegscommons/dercon.f'

      double precision e

!  locals

      integer i
      double precision ke

      ke = e - RM
      call findi(erng,ke,nrng,i)
      estepmax = aeste(i)+ke*(beste(i)+ke*(ceste(i)+ke*deste(i)))

      return
      end
!----------------------last line of estepmax.f--------------------------
!-----------------------------------------------------------------------
!                       SUBROUTINE FINDI
!  Version: 051219-1435
!  Reference:  Based on code developed and provided by F. Salvat
!              for computing GS Mult Scat with PW cross sections
!-----------------------------------------------------------------------
!23456789|123456789|123456789|123456789|123456789|123456789|123456789|12

      SUBROUTINE FINDI(X,XC,N,I)
C
C  Finds the interval (X(I),X(I+1)) that contains the value XC.
C
C  Input:
C     X(I) (I=1:N) ... grid points (the X values must be in increasing
C                      order).
C     XC ............. point to be located.
C     N  ............. number of grid points.
C  Output:
C     I .............. interval index.
C
      implicit none

      integer n,i, i1, it
      double precision xc, X(N)
C
      IF(XC.GT.X(N)) THEN
        I=N-1
        RETURN
      ENDIF
      IF(XC.LT.X(1)) THEN
        I=1
        RETURN
      ENDIF
      I=1
      I1=N
    1 IT=(I+I1)/2
      IF(XC.GT.X(IT)) I=IT
      IF(XC.LE.X(IT)) I1=IT
      IF(I1-I.GT.1) GO TO 1
      RETURN
      END
!----------------------------------fitms.f------------------------------
! Version: 060313-1235
! Reference: Based on code developed by BLIF and F. Salvat to compute
!            fit to GS MS distribution
!-----------------------------------------------------------------------
!23456789|123456789|123456789|123456789|123456789|123456789|123456789|12

      subroutine fitms(x,p,cdf,na, ipart,eindex,ik1)

!  input required:
!    x(NA)...     reduced scattering angle (1-cos(thet))/2.
!    p(NA)...     distribution function
!    cdf(NA)...   cumulative distribution function
!    ipart...     positron or electron
!    eindex...    current energy index
!    ik1...       current scattering strength index
!
!  output (through common MSCON):
!
!    cumdist(nextra)... cumulative dist over the final eq angle intervals
!    amums(nfit1)... end points (in units of reduced cosine) of intervals
!    ams(nfit)...   first coefficient in expansions
!    bms(nfit)...   second coefficient in expansions
!    cms(nfit)...   third coefficient in expansions
!    etams(nfit)... 'screening parameter' in expansions

      implicit none
      
      include 'include/egs5_h.f'
      include 'include/egs5_mscon.f'

      integer ipart, eindex, na, ik1, jint
      double precision x(na), p(na), cdf(na)

      integer NB
      parameter (NB=NFIT+1)

      integer mesh, istart, iend, interval, i
      double precision fit(NB), r, a, b, c, n
      double precision sum, diff, biggest
      
      double precision p1, p2, x1, x2, n1, n2, ln21, alpha
      double precision F, G

      logical printDiag

      printDiag = .false.
      
      if(na.ne.NB) then
        write (6,*) 'Error:  number of MS dist points != NB.  Stop.'
        stop
      endif

      mesh = NB-1
         
      sum = 0
      biggest = 0

      do interval = 1, mesh

        istart = interval
        iend = interval + 1
        p1 = p(istart)
        p2 = p(iend)
        x1 = x(istart)
        x2 = x(iend)

        if(p2 .ne. p1) then
          r = sqrt(p2/p1)
          n = (r*x2 - x1)/(1 - r)
          a = p1*(x1 + n)**2
        else
          r = 1
          n = 1e10
          a = p1*n**2
        endif

        n1 = x1 + n
        n2 = x2 + n
        ln21 = log(n2/n1)
                      
        F = 1/n1 - 1/n2
        G = (n1 + n2)*ln21 - 2*(n2 - n1)

C....Determine the fitting coefficients
        alpha = cdf(iend) - cdf(istart) - a*F
            
C....Ignoring the c term for now
        b = alpha/G
        c = 0
            
        if (p2 .ne. p1) then
          b = b/a
          c = c/a
        else
          b = 0
          c = 0
        endif
            
!  test accuracy

        if(printDiag) then
        do i = istart, iend
          fit(i) = (a/(x(i) + n)**2)*
     &             (  1 + 
     &                b* (x(i) - x1)*(x2 - x(i)) + 
     &                c*((x(i) - x1)*(x2 - x(i)))**2
     &             )

          diff = abs((fit(i) - p(i))/p(i))
          biggest = max(biggest,diff)
          sum = sum + diff
          write(26,*) x(i), (fit(i) - p(i))/p(i)
        enddo
        write(26,'(''  Biggest relative difference = '',g14.7)') biggest
        write(26,'(''  Goodness of fit = '',g14.7)') sum/NA
        endif
            
        ams(ipart,eindex,ik1,interval) = a
        bms(ipart,eindex,ik1,interval) = b
        cms(ipart,eindex,ik1,interval) = c
        etams(ipart,eindex,ik1,interval) = n
        amums(ipart,eindex,ik1,interval) = x1

!  for the equally spaced intervals, we need a re-normalized
!  cdf over just the last bin.  Total CDF should be 1/NBFIT,
!  and the cdf at x(NBFIT-1) should be (NBFIT-1)/NBFIT, but...
!  because of the no-scattering probability, there is a 
!  discrepency, so we need to use the full expression, and we
!  assume the cdf over the first bin is the correct constant

        if(interval.ge.NBFIT) then
          jint = interval - NBFIT + 1
          cumdist(ipart,eindex,ik1,jint) =
     +                   (cdf(iend) - cdf(NBFIT)) / cdf(2)
        endif
      enddo

      cumdist(ipart,eindex,ik1,NEXFIT) = 1.d0
      amums(ipart,eindex,ik1,mesh+1) = x2

      return
      end
!-------------------------last line of fitms.f--------------------------
!-----------------------------g1ededx.f---------------------------------
! Version: 060306-1000
! Reference: SLAC-R-730/KEK-2005-8
!-----------------------------------------------------------------------
!23456789|123456789|123456789|123456789|123456789|123456789|123456789|12
!
!   functions which returns the product of 1/dedx and g1, 1st transport
!   cross section for electrons
!
      double precision function g1ededx(e)
!
      implicit none

      include 'pegscommons/thres2.f'
      include 'pegscommons/molvar.f'

!  globals
      real*8 e, sptote, g1e

      g1ededx = g1e(-1,e,0) * (rlc / sptote(e,e,e))
      
      return
      end
!-------------------------last line of g1ededx.f------------------------

!-----------------------------g1pdedx.f---------------------------------
! Version: 051219-1435
! Reference: SLAC-R-730/KEK-2005-8
!-----------------------------------------------------------------------
!23456789|123456789|123456789|123456789|123456789|123456789|123456789|12
!
!   functions which returns the product of 1/dedx and g1, 1st transport
!   cross section for positrons
!
      double precision function g1pdedx(e)
!
      implicit none

      include 'pegscommons/thres2.f'
      include 'pegscommons/molvar.f'

!  globals
      real*8 e, sptotp, g1e

      g1pdedx = g1e(+1,e,0) * (rlc / sptotp(e,e,e))
      
      return
      end
!-------------------------last line of g1pdedx.f------------------------
!-----------------------------g1e.f-------------------------------------
! Version: 060317-1630
!          060721-1500  Modify **-2 --> **(-2)
! Reference: SLAC-R-730/KEK-2005-8
!-----------------------------------------------------------------------
!23456789|123456789|123456789|123456789|123456789|123456789|123456789|12
!
!  This function gets the scattering power for electrons or positrons
!  in a media as a function of energy.  For kinetic energies less than
!  100 MeV, tables provided by Salvat and a log-log cubic spline 
!  are used.  At kinetic energies greater than 100 MeV, an analytic
!  intergal over the Moliere cross section is employed.

      double precision function g1e(iq,e,iflag)

      implicit none

      include 'include/egs5_h.f'

      include 'pegscommons/scpspl.f'
      include 'pegscommons/molvar.f'
      include 'pegscommons/pmcons.f'
      include 'pegscommons/dercon.f'

      integer iq,iflag
      double precision e

!  locals

      integer i
      double precision ke, xa2, onemb2, beta2, g1mol, logke

      ke = e - RM
      logke = dlog(ke*1.d6)

      if(logke.le.etl(negrds)) then
!  spline interpolation from Salvat PW data
        call findi(etl,logke,negrds,i)
        if(iq .eq. -1) then
          g1e = 
     +      dexp(ag1e(i)+logke*(bg1e(i)+logke*(cg1e(i)+logke*dg1e(i))))
        else
          g1e = 
     +      dexp(ag1p(i)+logke*(bg1p(i)+logke*(cg1p(i)+logke*dg1p(i))))
        endif
      else
!  from Moliere cross section
        onemb2 = (1.d0 + (ke/RM) )**(-2)
        beta2 = 1.d0 - onemb2
        xa2 = fsc*fsc * 1.13 /(.885*.885) * dexp(zx/zs) / dexp(ze/zs)
        xa2 = xa2 * onemb2 / beta2
        g1mol = dlog((pi*pi + xa2)/xa2) - pi*pi/(pi*pi + xa2)
        g1e = r0*r0 * zs * 2*pi * g1mol * onemb2 / (beta2*beta2)
      endif

!  multiply by atom density

      if(iflag.ne.-1) g1e = g1e * an * rho / wm

      return
      end
!-------------------------last line of g1e.f----------------------------
!-----------------------------------------------------------------------
!                       SUBROUTINE GAULEG
!  Version: 051219-1435
!  Reference:  Based on code developed and provided by F. Salvat
!              for computing GS Mult Scat with PW cross sections
!-----------------------------------------------------------------------
!23456789|123456789|123456789|123456789|123456789|123456789|123456789|12

      SUBROUTINE GAULEG(X,W,N)
C
C  This subroutine returns the abscissas X(1:N) and weights W(1:N) of
C  the Gauss-Legendre N-point quadrature formula.
C
      implicit none

      integer N
      double precision  X(N),W(N)

      integer i,j,m
      double precision xm,xl, z,z1, p1,p2,p3,pp

      double precision eps
      PARAMETER (EPS=1.0D-15)

      M=(N+1)/2
      XM=0.0d0
      XL=1.0d0
      DO I=1,M
        Z=COS(3.141592654D0*(I-0.25D0)/(N+0.5D0))
    1   CONTINUE
          P1=1.0D0
          P2=0.0D0
          DO J=1,N
            P3=P2
            P2=P1
            P1=((2.0D0*J-1.0D0)*Z*P2-(J-1.0D0)*P3)/J
          ENDDO
          PP=N*(Z*P1-P2)/(Z*Z-1.0D0)
          Z1=Z
          Z=Z1-P1/PP
        IF(ABS(Z-Z1).GT.EPS*ABS(Z)+EPS) GO TO 1
        X(I)=XM-XL*Z
        X(N+1-I)=XM+XL*Z
        W(I)=2.0D0*XL/((1.0D0-Z*Z)*PP*PP)
        W(N+1-I)=W(I)
      ENDDO
      RETURN
      END
!-----------------------------------------------------------------------
!                       SUBROUTINE GSCOEF
!  Version: 051219-1435
!  Reference:  Based on code developed and provided by F. Salvat
!              for computing GS Mult Scat with PW cross sections
!-----------------------------------------------------------------------
!23456789|123456789|123456789|123456789|123456789|123456789|123456789|12

      SUBROUTINE GSCOEF(NLEG)
C
C     This subroutine computes the Goudsmit-Saunderson transport coef-
C  ficients from the elastic DCS. It must be linked to an external
C  function named DCSEL(RMU), RMU=(1-C0S(THETA))/2, which gives the DCS
C  per unit solid angle as a function of RMU.
C
C  NLEG is the number of terms included in the GS series
C
      implicit none

      include 'include/egs5_h.f'
      include 'include/egs5_uphiot.f'
      include 'include/egs5_cdcsep.f'
      include 'include/egs5_coefgs.f'

      integer nleg
      double precision DCSEL

      integer nxc, nlm, j,i,l
      double precision dcsl, dcsu, xi, funxi

      double precision X(NGT),W(NGT)
      double precision PL(NGT),XC(100),F0(100),F1(100)
C
      NLM=MAX(500,MIN(NLEG/2,NGT/2))
      CALL GAULEG(X,W,NLM)
C
      J=1
      XC(J)=0.0D0
      DCSL=1.0D-1*DCSI(1)
      DCSU=1.0D+1*DCSI(1)
      DO I=2,NREDA
        IF((DCSL.GT.DCSI(I)).OR.(DCSU.LT.DCSI(I))
     1    .OR.(XMU(I)-XC(J).GT.0.1D0)) THEN
          J=J+1
          XC(J)=XMU(I)
          DCSL=1.0D-1*DCSI(I)
          DCSU=1.0D+1*DCSI(I)
        ENDIF
      ENDDO
C
      IF (XC(J).NE.1.0D0) THEN
        J=J+1
        XC(J)=1.0D0
      ENDIF
      NXC=J
C
      DO J=2,NXC
        F0(J-1)=(XC(J)+XC(J-1))/2.0D0
        F1(J-1)=(XC(J)-XC(J-1))/2.0D0
      ENDDO
C
      NLEGEN=NLEG
      IF(NLEG.GT.NGT) NLEGEN=NGT
      DO L=1,NLEGEN
        GL(L)=0.0D0
      ENDDO
C
C  ****  Gauss-Legendre integration of the GS transport integrals.
C
      DO I=1,NLM
        DO J=1,NXC-1
          XI=F0(J)+X(I)*F1(J)
          FUNXI=F1(J)*W(I)*DCSEL(XI)
          CALL LEGENP(1.0D0-2.0D0*XI,PL,NLEGEN)
          DO L=1,NLEGEN
            IF(L.EQ.1) THEN
              GL(L)=GL(L)+FUNXI
            ELSE
              GL(L)=GL(L)+(1.0D0-PL(L))*FUNXI
            ENDIF
          ENDDO
        ENDDO
      ENDDO
C
      CS0=GL(1)
      GL(1)=0.0D0
      DO L=2,NLEGEN
        GL(L)=GL(L)/CS0
      ENDDO
      CS0=2.0D0*CS0
      CS=CS0*TWOPI
      TCS1=CS*GL(2)
      TCS2=CS*GL(3)

      RETURN
      END
!-----------------------------------------------------------------------
!                       SUBROUTINE GSDIST
!  Version: 060314-0815
!  Reference:  Based on code developed and provided by F. Salvat
!              for computing GS Mult Scat with PW cross sections
!-----------------------------------------------------------------------
!23456789|123456789|123456789|123456789|123456789|123456789|123456789|12

      SUBROUTINE GSDIST(S,RMU,PDF,NTERM)
C
C  Goudsmit-Saunderson multiple scattering distribution.
C
C  Input arguments:
C    S = path length in units of the elastic mean free path.
!    nterm == if nterm is negative, then the last angle didn't
!    converge so we'll use the dcs for this larger angle.  temp
!    fix until single scattering mode is in place
C    RMU =.5 * (1.0D0-COS(THETA)).
C  Output arguments:
C    NTERMS = number of terms needed to get convergence of the series.
C    PDF = probability distribution function of the final 'direction',
C          RMU, of electrons that have been scattered at least once.
C
      implicit none

      integer nterm
      double precision s, rmu, pdf, DCSEL
C
      include 'include/egs5_h.f'
      include 'include/egs5_coefgs.f'

      logical printDiag
      integer l
      double precision x, dxs, sum, sumf, f
      double precision  PL(NGT)

      printDiag = .false.

      IF(RMU.LT.0.0D0.OR.RMU.GT.1.000001D0) STOP 'GS error.'
      X=1.0D0-2.0D0*RMU
      if(nterm.lt.0) then
        SUM=S*DCSEL(RMU)/CS0
        go to 200
      end if
      CALL LEGENP(X,PL,NLEGEN)
C
C  ****  Legendre series.
C
C  -- DXS is the probability of no scattering, which corresponds to a
C     delta distribution at THETA=0. This unscattered component is
C     subtracted from the GS distribution to speed up convergence.
C
      ! trap to prevent underflow exceptions for thick targets
      if(s.lt.100.d0) then
        DXS=EXP(-S)
      else
        dxs = 0.d0
      endif

      SUM=0.0D0
      SUMF=0.0D0
      NTERM=NLEGEN
      DO L=1,NLEGEN
        ! trap to prevent underflow exceptions for thick targets
        if(s*GL(L).lt.100.d0) then
          F=(L-0.5D0)*(EXP(-S*GL(L))-DXS)
        else
          F=-(L-0.5D0)*DXS
        endif
        SUM=SUM+F*PL(L)
        SUMF=SUMF+F
        IF(ABS(F).LT.1.0D-6*ABS(SUM)) then
          NTERM=MIN(NTERM,L)
          if(l.gt.1) go to 100
        endif
      ENDDO
  100 continue
      IF(ABS(F).GT.1.0D-2*ABS(SUM)) THEN
        if(printDiag) then
        WRITE(26,1000) RMU,SUM,F
 1000   FORMAT(1X,'**  Warning. Low accuracy in GSSUM.',
     1        /' at RMU = ',1P,E12.5,', SUM =',E12.5,
     2        ',  last term =',E12.5)
C  ****  ...a negative value of SUM indicates lack of convergence.
        endif
        if(sum.gt.0d0) SUM=-SUM
      ENDIF
C  >>>>  At large angles, the GS distribution may be in serious error
C  due to truncation errors. When this happens, the absolute value of
C  the PDF is much smaller than at MU=0 and we can set PDF=S*DCS.
C
      IF(sum.lt.0d0 .or. ABS(SUM).LT.1.0D-4*SUMF) then
        SUM=S*DCSEL(RMU)/CS0
        !  temp fix until single scattering
        nterm = -1
      end if
C
 200  PDF=2.0D0*SUM
      RETURN
      END
!-----------------------------inigrd.f----------------------------------
! Version: 051219-1435
!-----------------------------------------------------------------------
!23456789|123456789|123456789|123456789|123456789|123456789|123456789|12

!  This subroutine initialized the cut-off dependent grids for the 
!  multiple scattering routines.  

      subroutine inigrd(ecut,emax,sflag)

      implicit none

!    Arguments:
!    ecut...     electron cut-off total energy
!    emax...     maximum electron total energy
!    sflag...    call dcsstor or not.

      integer sflag
      double precision ecut, emax

      include 'include/egs5_h.f'
      include 'include/egs5_cdcsep.f'
      include 'include/egs5_mscon.f'
      include 'include/egs5_uphiot.f'
      include 'include/egs5_useful.f'

      include 'pegscommons/mscom.f'
C
      integer i, j, k
      double precision e, edec, ecutev, emaxev

!  get the multiple scattering grid for this particular
!  problem.  using linear-log combination grid.
!  note that their are actually two grids here, because
!  the Barcelona MS distribution runs only to 100 MeV,
!  but the problem methodology extends to 10 TeV.  Therefore,
!  we track scattering power for the full energy range,
!  with the number of energy points given in the variable
!  nmscpw.  The Barcelona MS dist is based on the same
!  energy grid spacing, but is terminated above 100 MeV.  
!  The number of points is given in nmscate.  There is
!  a possibility of confusion in that the same array 
!  holds the energy grid values for both - mscate()

      ecutev = (ecut - RM) * 1.0d6
      emaxev = (emax - RM) * 1.0d6
      decade1 = dint(dlog10(ecutev))
      decade2 = dint(dlog10(emaxev))
!
!   Loop over the energy steps - start at the decade of ecut
!
      nmscate = 0
      nmscpw = 0
      do i = decade1,decade2
        edec = 10.d0**i
        k = 0
        do j = 1,NDEC
          k = k + 1
          e = edec * 10.d0**((j-1.d0)/NDEC)
!
!  find the first energy...
          if(e.ge.ecutev) then
            if(nmscpw.eq.0) then
              e = ecutev
              if(k.eq.1) then
                joffset = NDEC-1
              else
                joffset = k - 2 
              endif
            else
              e = edec * 10.d0**((j-2.d0)/NDEC)
            endif
!
!  find the last energy...
            if(e.gt.emaxev) then
              if(mscate(nmscpw).ge.emaxev) go to 200
              e = emaxev
            endif
!
            nmscpw = nmscpw + 1
            mscate(nmscpw) = e
            if(e .le. 1.d8) nmscate = nmscate + 1
          endif
        end do
      end do
200   continue
C
      if(sflag.eq.1) call dcsstor(ecut,emax)

C  ****  Angular grid (TH in deg, XMU=(1.0D0-COS(TH))/2).
C
C  do this just once.

      if(th(2).eq.1.d-4) return
      I=1
      TH(I)=0.0D0
      THR(I)=TH(I)*PI/180.0D0
      XMU(I)=(1.0D0-COS(THR(I)))/2.0D0
      I=2
      TH(I)=1.0D-4
      THR(I)=TH(I)*PI/180.0D0
      XMU(I)=(1.0D0-COS(THR(I)))/2.0D0
   20 CONTINUE
      I=I+1
      IF(TH(I-1).LT.0.9999D-3) THEN
        TH(I)=TH(I-1)+2.5D-5
      ELSE IF(TH(I-1).LT.0.9999D-2) THEN
        TH(I)=TH(I-1)+2.5D-4
      ELSE IF(TH(I-1).LT.0.9999D-1) THEN
        TH(I)=TH(I-1)+2.5D-3
      ELSE IF(TH(I-1).LT.0.9999D+0) THEN
        TH(I)=TH(I-1)+2.5D-2
      ELSE IF(TH(I-1).LT.0.9999D+1) THEN
        TH(I)=TH(I-1)+1.0D-1
      ELSE IF(TH(I-1).LT.2.4999D+1) THEN
        TH(I)=TH(I-1)+2.5D-1
      ELSE
        TH(I)=TH(I-1)+5.0D-1
      ENDIF
      THR(I)=TH(I)*PI/180.0D0
      XMU(I)=(1.0D0-COS(THR(I)))/2.0D0
      IF(I.LT.NREDA) GO TO 20

      RETURN
      END
!-------------------------last line of inigrd.f-------------------------
!-----------------------------------------------------------------------
!                       SUBROUTINE INTEG
!  Version: 051219-1435
!  Reference:  Based on code developed and provided by F. Salvat
!              for computing GS Mult Scat with PW cross sections
!-----------------------------------------------------------------------
!23456789|123456789|123456789|123456789|123456789|123456789|123456789|12

      SUBROUTINE INTEG(X,A,B,C,D,XL,XU,SUM,N)
C
C  Computes the integral of a cubic spline function.
C
C  Input:
C     X(I) (I=1:N) ... grid points (the X values must be in increasing
C                      order).
C     A(1:N),B(1:N),C(1:N),D(1:N) ... spline coefficients.
C     N  ............. number of grid points.
C     XL ............. lower limit in the integral.
C     XU ............. upper limit in the integral.
C  Output:
C     SUM ............ value of the integral.
C
      implicit none

      integer n
      double precision  X(N),A(N),B(N),C(N),D(N), xl, xu, sum

      integer i, il, iu
      double precision x1,x2, xll, xuu, sign, sump

C  ****  Set integration limits in increasing order.
      IF(XU.GT.XL) THEN
        XLL=XL
        XUU=XU
        SIGN=1.0D0
      ELSE
        XLL=XU
        XUU=XL
        SIGN=-1.0D0
      ENDIF
C  ****  Check integral limits.
      IF(XLL.LT.X(1).OR.XUU.GT.X(N)) THEN
        WRITE(6,10)
   10   FORMAT(5X,'Integral limits out of range. Stop.')
      ENDIF
C  ****  Find involved intervals.
      SUM=0.0D0
      CALL FINDI(X,XLL,N,IL)
      CALL FINDI(X,XUU,N,IU)
C
      IF(IL.EQ.IU) THEN
C  ****  Only a single interval involved.
        X1=XLL
        X2=XUU
        SUM=X2*(A(IL)+X2*((B(IL)/2)+X2*((C(IL)/3)+X2*D(IL)/4)))
     1     -X1*(A(IL)+X1*((B(IL)/2)+X1*((C(IL)/3)+X1*D(IL)/4)))
      ELSE
C  ****  Contributions from several intervals.
        X1=XLL
        X2=X(IL+1)
        SUM=X2*(A(IL)+X2*((B(IL)/2)+X2*((C(IL)/3)+X2*D(IL)/4)))
     1     -X1*(A(IL)+X1*((B(IL)/2)+X1*((C(IL)/3)+X1*D(IL)/4)))
        IL=IL+1
        DO I=IL,IU
          X1=X(I)
          X2=X(I+1)
          IF(I.EQ.IU) X2=XUU
          SUMP=X2*(A(I)+X2*((B(I)/2)+X2*((C(I)/3)+X2*D(I)/4)))
     1        -X1*(A(I)+X1*((B(I)/2)+X1*((C(I)/3)+X1*D(I)/4)))
          SUM=SUM+SUMP
        ENDDO
      ENDIF
      SUM=SIGN*SUM
      RETURN
      END
!---------------------------------k1e.f---------------------------------
! Version: 060316-1345
!-----------------------------------------------------------------------
!23456789|123456789|123456789|123456789|123456789|123456789|123456789|12

!  This function gets the initial scattering strength for electrons or 
!  positrons in a media as a function of energy.  

      double precision function k1e(iq,e)

      implicit none

      include 'include/egs5_h.f'

      include 'pegscommons/k1spl.f'
      include 'pegscommons/dercon.f'

      integer iq
      double precision e

!  locals

      integer i
      double precision ke

      ke = e - RM
      call findi(ehinge,ke,nhinge,i)
      if(iq .eq. -1) then
        k1e = ak1e(i)+ke*(bk1e(i)+ke*(ck1e(i)+ke*dk1e(i)))
      else
        k1e = ak1p(i)+ke*(bk1p(i)+ke*(ck1p(i)+ke*dk1p(i)))
      endif

      return
      end
!-------------------------last line of k1e.f----------------------------
!-----------------------------------------------------------------------
!                       SUBROUTINE LEGENP
!  Version: 051219-1435
!  Reference:  Based on code developed and provided by F. Salvat
!              for computing GS Mult Scat with PW cross sections
!-----------------------------------------------------------------------
!23456789|123456789|123456789|123456789|123456789|123456789|123456789|12

      SUBROUTINE LEGENP(X,PL,NL)
C
C  This subroutine computes the first NL Legendre polynomials for the
C  argument X, using their recurrence relation. PL is an array of phys-
C  ical dimension equal to NL or larger. On output PL(J), J=1:NL, con-
C  tains the value of the Legendre polynomial of degree (order) J-1.
C
      implicit none

      integer nl
      double precision x,  PL(NL)

      integer j
      double precision twox, f1, f2, d

      PL(1)=1.0D0
      PL(2)=X
      IF(NL.GT.2) THEN
        TWOX=2.0D0*X
        F1=X
        D=1.0D0
        DO J=3,NL
          F1=F1+TWOX
          F2=D
          D=D+1.0D0
          PL(J)=(F1*PL(J-1)-F2*PL(J-2))/D
        ENDDO
      ENDIF
      RETURN
      END
!-----------------------------makek1.f----------------------------------
! Version: 051219-1435
! Reference: SLAC-R-730/KEK-2005-8
!-----------------------------------------------------------------------
!23456789|123456789|123456789|123456789|123456789|123456789|123456789|12
!
!  get energy dependent values of k1total, the initial scattering
!  strength to take at the given media
!
      subroutine makek1(emin,emax)

!  globals

      implicit none

      real*8 emax,emin
      real*8 g1pdedx, g1ededx, sumga
      external g1pdedx, g1ededx

      include 'include/egs5_h.f'
      include 'include/egs5_mscon.f'

      include 'pegscommons/dcsstr.f'
      include 'pegscommons/dercon.f'
      include 'pegscommons/mscom.f'
      include 'pegscommons/k1spl.f'
      
!  locals

      integer i
      real*8 ekmax, ekmin, efrac, destp1, destp2, ctol
      real*8 e1, e2, ek1init(NESCPW), pk1init(NESCPW)

!  Energy spacing scheme:  ESTEPE slides from EFRAC_H to EFRAC_L

      ekmax = emax - RM
      ekmin = emin - RM
      nhinge = nmscpw

!  get the constants to compute ESTEPE as a function of E

      destp2 = (efracl-efrach) / dlog(ekmin/ekmax)
      destp1 = efrach - dlog(ekmax) * destp2

!   loop over the log-linear spaced hinges, set the energy point, then 
!   integrate over the scattering power to get the total
!   scattering strength over that interval

      e2 = ekmax
      do i=1,nhinge-1
        efrac = destp1 + destp2 * dlog(e2)
        e1 = e2 * (1.d0 - efrac)
        if(efrac.ge.0.20) then
          ctol = 1.d-3
        else if(efrac.ge.0.05) then
          ctol = 1.d-4
        else
          ctol = 1.d-5
        endif
      
        pk1init(nhinge-i+1) = sumga(g1pdedx,e1+RM,e2+RM,ctol)
        ek1init(nhinge-i+1) = sumga(g1ededx,e1+RM,e2+RM,ctol)

        ehinge(nhinge-i+1) = e2
        e2 = mscate(nhinge-i)*1.d-6
      end do

      !-->  Extrapolate to get first hinge.
      ehinge(1) = ekmin
      efrac = (ehinge(2)-ehinge(1)) / (ehinge(3)-ehinge(2))
      pk1init(1) = pk1init(2) + (pk1init(2) - pk1init(3)) * efrac
      ek1init(1) = ek1init(2) + (ek1init(2) - ek1init(3)) * efrac

      do i = 1, nhinge
        if(ehinge(i) .le. 1.d8) then
          if(ek1init(i) .lt. k1mine) k1mine = ek1init(i)
          if(ek1init(i) .gt. k1maxe) k1maxe = ek1init(i)
          if(pk1init(i) .lt. k1minp) k1minp = pk1init(i)
          if(pk1init(i) .gt. k1maxp) k1maxp = pk1init(i)
        endif
      end do

      !-->  get splines for use later in PWLF1
100   call spline(ehinge,ek1init,ak1e,bk1e,ck1e,dk1e,0.d0,0.d0,nhinge)
      call spline(ehinge,pk1init,ak1p,bk1p,ck1p,dk1p,0.d0,0.d0,nhinge)

      return
      end
!-------------------------last line of makek1.f-------------------------
!--------------------------------pegs5.f--------------------------------
! Version: 090116-0700
! Reference: SLAC-R-730/KEK-2005-8
!-----------------------------------------------------------------------
!23456789|123456789|123456789|123456789|123456789|123456789|123456789|12

      double precision function addmol(x)
      implicit none
      double precision x
      include 'pegscommons/dercon.f'
      addmol=1.0/(x-rm)**2
      return
      end

      double precision function adfmol(x)
      implicit none
      double precision x
      include 'pegscommons/dercon.f'
      adfmol=-1.0/(x-RM)
      return
      end

      double precision function adimol(x)
      implicit none
      double precision x
      include 'pegscommons/dercon.f'
      adimol=-1.0/x+RM
      return
      end

      subroutine adscpr(mxrawt,mxshet,elecnt,nshelt,capint,scprot,qcapt,
     *pz)
      implicit none
      integer iiend, mxrawt, i, j, ii, nshelt, mxshet
      double precision qcapt, elecnt, pz, capint, scprot
      include 'pegscommons/cpcom.f'
      dimension elecnt(200),nshelt(200),capint(200),scprot(31,200), 
     *          qcapt(31)
      iiend=MXRAW+mxrawt+1
      if (iiend.gt.200) then
        write(  26,100)
100     format(' Error Compton profile data  MXRAW .GT. 200')
        close(26)
        stop
      end if
      do i=1,31
        if (MXRAW.eq.0) then
          qcap(i)=qcapt(i)
        else
          if (qcap(i).ne.qcapt(i)) then
            write(  26,110)
110         format(' Error Compton profile data  qcap are not agreed')
            close(26)
            stop
          end if
        end if
      end do
      do j=1,31
        if (MXRAW.eq.0) then
          scprof(j,iiend)=0.0
        else
          scprof(j,iiend)=scprof(j,MXRAW+1)
        end if
      end do
      do i=1,mxrawt
        ii=MXRAW+I
        nshell(ii)=nshelt(i)+MXSHEL
        elecni(ii)=elecnt(i)*pz
        capin(ii)=capint(i)
        do j=1,31
          scprof(j,ii)=scprot(j,i)
          scprof(j,iiend)=scprof(j,iiend)+scprot(j,i)*elecnt(i)*pz
        end do
      end do
      MXRAW=MXRAW+mxrawt
      mxshel=mxshel+mxshet
      return
      end

      double precision function affact(x)
      implicit none
      double precision aintp, x
      include 'pegscommons/cohcom.f'
      affact=aintp(x,XVAL(1),100,AFAC2(1),1,.true.,.true.)
      return
      end

      double precision function aintp(x,xa,nx,ya,isk,xlog,ylog)
      implicit none
      integer nx, isk, j, i
      double precision xi, xj, xv, yi, yj
      double precision xa(nx),x
      double precision ya(isk,nx)
      logical xlog,ylog,xlogl
      xlogl=xlog
      do j=2,nx
        if (x.lt.xa(j)) go to 100
      end do
      j=nx
100   i=j-1
      if (xa(i).le.0.0) then
        xlogl=.false.
      end if
      if (.not.xlogl) then
        xi=xa(i)
        xj=xa(j)
        xv=x
      else
        xi=dlog(xa(i))
        xj=dlog(xa(j))
        xv=dlog(x)
      end if
      if (ylog.and.(ya(1,i).eq.0.0.or.ya(1,j).eq.0.0)) then
        aintp=0.0
      else
        if (ylog) then
          yi=dlog(ya(1,i))
          yj=dlog(ya(1,j))
          if (xj.eq.xi) then
            aintp=yi
          else
            aintp=(yi*(xj-xv)+yj*(xv-xi))/(xj-xi)
          end if
          aintp=dexp(aintp)
        else
          yi=ya(1,i)
          yj=ya(1,j)
          if (xj.eq.xi) then
            aintp=yi
          else
            aintp=(yi*(xj-xv)+yj*(xv-xi))/(xj-xi)
          end if
        end if
      end if
      return
      end

      double precision function alin(x)
      implicit none
      double precision x
      alin=x
      return
      end

      double precision function alini(x)
      implicit none
      double precision x
      alini=x
      return
      end

      double precision function alke(e)
      implicit none
      double precision e
      include 'pegscommons/dercon.f'
      alke=dlog(e-RM)
      return
      end

      double precision function alkei(x)
      implicit none
      double precision x, dexp
      include 'pegscommons/dercon.f'
      alkei=dexp(x) + RM
      return
      end

      double precision function amoldm(en0,en)
      implicit none
      double precision en0, tm, em, betasq, amolfm, en
      include 'pegscommons/pmcons.f'
      include 'pegscommons/dercon.f'
      include 'pegscommons/molvar.f'
      include 'pegscommons/lamolm.f'
      T0=en0-RM
      tm=T0/RM
      em=tm+1.
      c1=(tm/em)**2
      c2=(2.*tm+1.)/em**2
      betasq=1.-1./em**2
      cmoll=RLC*EDEN*2.*PI*R0**2/(betasq*T0*tm)
      amoldm=amolfm(en)
      return
      end

      double precision function amolfm(en)
      implicit none
      double precision t, en, eps, epsp, epsi, epspi
      include 'pegscommons/dercon.f'
      include 'pegscommons/lamolm.f'
      t=en-RM
      eps=t/T0
      epsp=1.-eps
      epsi=1./eps
      epspi=1./epsp
      amolfm=CMOLL*(C1+epsi*(epsi-C2)+epspi*(epspi-C2))
      return
      end

      double precision function amolrm(en0,en1,en2)
      implicit none
      double precision t0, en0, t1, en1, t2, en2, tm, em, c1, c2,
     & betasq, cmoll2, eps1, epsp1, eps2, epsp2, dlog
      include 'pegscommons/pmcons.f'
      include 'pegscommons/dercon.f'
      include 'pegscommons/molvar.f'
      t0=en0-RM
      t1=en1-RM
      t2=en2-RM
      tm=t0/RM
      em=tm+1.
      c1=(tm/em)**2
      c2=(2.*tm+1.)/em**2
      betasq=1.-1./em**2
      cmoll2=RLC*EDEN*2.*PI*R0**2/(betasq*tm)
      eps1=t1/t0
      epsp1=1.-eps1
      eps2=t2/t0
      epsp2=1.-eps2
      amolrm=cmoll2*(c1*(eps2-eps1)+1./eps1-1./eps2+1./epsp2-1./epsp1 -
     *       c2*dlog(eps2*epsp1/(eps1*epsp2)))
      return
      end

      double precision function amoltm(e0)
      implicit none
      double precision e0, t0, amolrm
      include 'pegscommons/thres2.f'
      include 'pegscommons/dercon.f'
      if (e0.le.THMOLL) then
        amoltm=0.
      else
        t0=e0-RM
        amoltm=amolrm(e0,ae,t0*0.5+RM)
      end if
      return
      end

      double precision function anihdm(e0,k)
      implicit none
      double precision gam, e0, t0p, anihfm
      include 'pegscommons/pmcons.f'
      include 'pegscommons/dercon.f'
      include 'pegscommons/molvar.f'
      include 'pegscommons/lanihm.f'
      double precision k
      gam=e0/RM
      a=gam+1.
      t0p=gam-1.
      C1=RLC*EDEN*PI*R0**2/(a*t0p*RM)
      C2=A+2.0*gam/a
      anihdm=anihfm(k)
      return
      end

      double precision function anihfm(k)
      implicit none
      double precision s1
      include 'pegscommons/dercon.f'
      include 'pegscommons/lanihm.f'
      double precision k,kp,x
      s1(x)=C1*(-1.+(C2-1.0/x)/x)
      kp=k/RM
      anihfm=s1(kp)+s1(A-kp)
      return
      end

      double precision function anihrm(e0,k1,k2)
      implicit none
      double precision s2, c1, c2, dlog, gam, e0, a, t0p
      include 'pegscommons/pmcons.f'
      include 'pegscommons/dercon.f'
      include 'pegscommons/molvar.f'
      double precision k1,k2,kp1,kp2
      double precision x
      s2(x)=RM*c1*(-x+c2*dlog(x)+1.0/x)
      gam=e0/RM
      kp1=k1/RM
      kp2=k2/RM
      a=gam+1.
      T0P=gam-1.
      c1=RLC*EDEN*PI*R0**2/(A*T0P*RM)
      c2=A+2.*gam/A
      anihrm=s2(kp2)-s2(kp1)+s2(A-kp1)-s2(A-KP2)
      return
      end

      double precision function anihtm(e0)
      implicit none
      double precision gam, e0, p0p2, p0p, dsqrt, canih, dlog
      include 'pegscommons/pmcons.f'
      include 'pegscommons/dercon.f'
      include 'pegscommons/molvar.f'
      gam=e0/RM
      p0p2=gam*gam-1.0
      p0p=dsqrt(p0p2)
      canih=RLC*EDEN*PI*R0**2/(gam+1.)
      anihtm=canih*((gam*gam+4.*gam+1.)/p0p2*dlog(gam+p0p)-(gam+3.)/p0p)
      return
      end

      double precision function aprim(z,e)
      implicit none
      integer ie, naprz, napre, iz
      double precision e, em, aintp, z
      include 'pegscommons/dercon.f'
      include 'pegscommons/epstar.f'
      double precision aprimd(115,14),eprim(115),zprim(14),aprimz(115)
      data aprimd/1.32,1.26,1.18,1.13,1.09,1.07,1.05,1.04,1.03, 1.02,8*1
     *.0, 97*0.0, 1.34,1.27,1.19,1.13,1.09,1.07,1.05,1.04,1.03,1.02, 8*1
     *.0, 97*0.0, 1.39,1.30,1.21,1.14,1.10,1.07,1.05,1.04,1.03,1.02,0.99
     *4, 2*0.991,0.990,2*0.989,2*0.988, 97*0.0, 1.46,1.34,1.23,1.15,1.11
     *,1.08, 1.06,1.05,1.03,1.02,0.989, 0.973,0.971,0.969,0.967,0.965,2*
     *0.963, 97*0.0, 1.55,1.40,1.26,1.17,1.12,1.09,1.07,1.05,1.03,1.02,0
     *.955,0.935, 0.930,0.925,0.920,0.915,2*0.911, 97*0.0,  1035*0.0/, E
     *PRIM/2.,3.,4.,5.,6.,7.,8.,9.,10.,11.,21.,31.,41.,51.,61.,71.,81.,9
     *1.,  97*0.0/, ZPRIM/6.,13.,29.,50.d0,79., 9*0.0/
      if (IAPRIM.eq.0) then
        if (IAPRFL .eq. 0) then
          IAPRFL=1
          write(  26,100)
100       format(' IAPRIM=0, i.e. uses KOCH AND MOTZ empirical correctio
     *ns to', ' brem cross section'/)
        end if
        if (e.ge.50) then
          APRIM=1.
        else
          em=e/RM
          do ie=1,18
            aprimz(ie)= aintp(z,zprim,5,aprimd(ie,1),115,.false.,.false.
     *      )
          end do
          aprim=aintp(em,eprim,18,aprimz,1,.false.,.false.)
        end if
      else if(IAPRIM.eq.1) then
        if (IAPRFL.eq.0) then
          write(  26,110)
110       format(' IAPRIM=1, i.e. uses NRC(based on NIST/ICRU)', ' corre
     *ctions to brem cross section'/)
          read(22,*) naprz, napre
          read(22,*) (eprim(ie),ie=1,napre)
          do ie=1,napre
            eprim(ie)=1.+eprim(ie)/RM
          end do
          do iz=1,naprz
            read(22,*) zprim(iz),(aprimd(ie,iz),ie=1,napre)
          end do
          iaprfl=1
          rewind(22)
        end if
        em=e/RM
        do ie=1,napre
          aprimz(ie)= aintp(z,zprim,naprz,aprimd(ie,1),115,.true.,.false
     *    .)
        end do
        aprim=aintp(em,eprim,napre,aprimz,1,.false.,.false.)
      else if (iaprim.eq.2) then
        if (iaprfl .eq. 0) then
          iaprfl=1
          write(  26,140)
140       format(' IAPRIM = 2, i.e. uses NO corrections to brem', ' cros
     *s section'/)
        end if
        aprim=1.0
      else
        write(  26,150) iaprim
150     format(//,' Illegal value for iaprim: ',I4)
        close(26)
        stop
      end if
      return
      end

      double precision function arec(x)
      implicit none
      double precision x
      arec=1.0/x
      return
      end

      double precision function bhabdm(en0,en)
      implicit none
      double precision en0, tm, em, y, bhabfm, en
      include 'pegscommons/pmcons.f'
      include 'pegscommons/dercon.f'
      include 'pegscommons/molvar.f'
      include 'pegscommons/lbhabm.f'
      T0=en0-RM
      tm=T0/RM
      em=tm+1.
      y=1./(tm+2.)
      betasi=1./(1.-1./em**2)
      CBHAB=RLC*EDEN*2.*PI*R0**2/(T0*tm)
      B1=2.-y**2
      B2=3.-y*(6.-y*(1.-y*2.))
      B3=2.-y*(10.-y*(16.-y*8.))
      B4=1.-y*(6.-y*(12.-y*8.))
      bhabdm=bhabfm(en)
      return
      end

      double precision function bhabfm(en)
      implicit none
      double precision t, en, eps, epsi
      include 'pegscommons/dercon.f'
      include 'pegscommons/lbhabm.f'
      t=en-RM
      eps=t/T0
      epsi=1./eps
      bhabfm=CBHAB*(epsi*(epsi*BETASI-B1)+B2+EPS*(eps*B4-B3))
      return
      end

      double precision function bhabrm(en0,en1,en2)
      implicit none
      double precision t0, en0, t1, en1, t2, en2, tm, em, y, betasi,
     & cbhab2, b1, b2, b3, b4, eps1, eps2, dlog
      include 'pegscommons/pmcons.f'
      include 'pegscommons/dercon.f'
      include 'pegscommons/molvar.f'
      t0=en0-RM
      t1=en1-RM
      t2=en2-RM
      tm=t0/RM
      em=tm+1.
      y=1./(tm+2.)
      betasi=1./(1.-1./em**2)
      cbhab2=RLC*EDEN*2.*PI*R0**2/tm
      b1=2.-y**2
      b2=3.-y*(6.-y*(1.-y*2.))
      b3=2.-y*(10.-y*(16.-y*8.))
      b4=1.-y*(6.-y*(12.-y*8.))
      eps1=t1/t0
      eps2=t2/t0
      bhabrm=cbhab2*(betasi*(1./eps1-1./eps2)-b1*dlog(eps2/eps1) +b2*(ep
     *s2-eps1)+eps2*eps2*(eps2*b4/3.-0.5*b3) - eps1*eps1*(eps1*b4/3.-0.5
     **b3))
      return
      end

      double precision function bhabtm(e0)
      implicit none
      double precision e0, bhabrm
      include 'pegscommons/thres2.f'
      include 'pegscommons/dercon.f'
      if (e0.le.ae) then
        bhabtm=0.
      else
        bhabtm=bhabrm(e0,ae,e0)
      end if
      return
      end

      double precision function bremdr(ea,k)
      implicit none
      integer ls
      double precision ea, bremfr
      double precision k
      include 'pegscommons/lbremr.f'
      e=ea
      if (e.ge.50.) then
        LD=2
        ls=3
      else
        LD=1
        ls=0
      end if
      LA=ls+1
      LB=ls+2
      bremdr=bremfr(k)
      return
      end

      double precision function bremdz(z,e,k)
      implicit none
      double precision brmsdz, z, e
      double precision k
      bremdz=brmsdz(z,e,k)/k
      return
      end

      double precision function bremfr(k)
      implicit none
      double precision eps, del, delta, a, b
      double precision k
      include 'pegscommons/bremp2.f'
      include 'pegscommons/dbrpr.f'
      include 'pegscommons/lbremr.f'
      eps=k/e
      del=eps/(e*(1-eps))
      if (del.gt.delpos(ld)) then
        bremfr=0.0
        return
      end if
      delta=DELCM*del
      if (delta.le.1.) then
        a=DL1(LA)+delta*(DL2(LA)+delta*DL3(LA))
        b =DL1(LB)+delta*(DL2(LB)+delta*DL3(LB))
      else
        a=DL4(LA)+DL5(LA)*dlog(delta+DL6(LA))
        b =DL4(LB)+DL5(LB)*dlog(delta+DL6(LB))
      end if
      bremfr=(ALPHI(LD)*(1.-eps)/eps/AL2*A+0.5*(2.*eps)*b)/e
      return
      end

      double precision function bremfz(k)
      implicit none
      double precision brmsfz
      double precision k
      bremfz=brmsfz(k)/k
      return
      end

      double precision function bremrm(e,k1,k2)
      implicit none
      integer i
      double precision bremrz, e
      double precision k1,k2
      include 'pegscommons/mixdat.f'
      include 'pegscommons/mxdatc.f'
      bremrm=0.
      do i=1,ne
        bremrm=bremrm+pz(i)*bremrz(z(i),e,k1,k2)
      end do
      return
      end

      double precision function bremrr(e,k1,k2)
      implicit none
      double precision dummy, bremdr, e, qd
      double precision k1,k2
      external bremfr
      dummy=bremdr(e,k1)
      bremrr=qd(bremfr,k1,k2,'bremfr')
      return
      end

      double precision function bremrz(z,e,k1,k2)
      implicit none
      double precision dummy, bremdz, z, e, qd
      double precision k1,k2
      external bremfz
      dummy=bremdz(z,e,k1)
      bremrz=qd(bremfz,k1,k2,'bremfz')
      return
      end

      double precision function bremtm(e0)
      implicit none
      double precision e0, bremrm
      include 'pegscommons/thres2.f'
      include 'pegscommons/dercon.f'
      if (e0.le.AP+RM) then
        bremtm=0.
      else
        bremtm=bremrm(e0,AP,e0-RM)
      end if
      return
      end

      double precision function bremtr(e0)
      implicit none
      double precision e0, bremrr
      include 'pegscommons/thres2.f'
      include 'pegscommons/dercon.f'
      if (e0.le.AP+RM) then
        bremtr=0.
      else
        bremtr=bremrr(e0,AP,e0-RM)
      end if
      return
      end

      double precision function brmsdz(z,ea,k)
      implicit none
      double precision ea, z, aprim, xsif, dlog, fcoulc, brmsfz
      double precision k
      include 'pegscommons/pmcons.f'
      include 'pegscommons/dercon.f'
      include 'pegscommons/molvar.f'
      include 'pegscommons/lbremz.f'
      e=ea
      delc=136.*z**(-1./3.)*RM/e
      const=aprim(z,e)*(AN*RHO/WM)*R0**2*FSC*z*(Z+XSIF(Z))*RLC
      XLNZ=4./3.*dlog(z)
      if (e.ge.50) XLNZ=XLNZ+4.*fcoulc(z)
      DELTAM=dexP((21.12-XLNZ)/4.184)-0.952
      brmsdz=brmsfz(k)
      return
      end

      double precision function brmsfz(k)
      implicit none
      double precision emkloc, delta, sb1, sb2, dlog, ee
      double precision k
      include 'pegscommons/lbremz.f'
      emkloc=E-k
      if (emkloc.eq.0.0) then
        emkloc=1.D-25
      end if
      delta=DELC*k/emkloc
      if (delta.ge.DELTAM) then
        brmsfz=0.0
      else
        if (delta.le.1.) then
          sb1=20.867+delta*(-3.242+delta*0.625)-XLNZ
          sb2=20.209+delta*(-1.930+delta*(-0.086))-XLNZ
        else
          sb1=21.12-4.184*dlog(delta+0.952)-XLNZ
          sb2=sb1
        end if
        ee=emkloc/E
        brmsfz=CONST*((1.+ee*ee)*sb1-0.666667*ee*sb2)
      end if
      return
      end

      double precision function brmsrm(e,k1,k2)
      implicit none
      integer i
      double precision brmsrz, e
      double precision k1,k2
      include 'pegscommons/mixdat.f'
      include 'pegscommons/mxdatc.f'
      brmsrm=0.
      do i=1,ne
        brmsrm=brmsrm+PZ(I)*brmsrz(z(i),e,k1,k2)
      end do
      return
      end

      double precision function brmsrz(z,e,k1,k2)
      implicit none
      double precision dummy, brmsdz, z, e, qd
      double precision k1,k2
      external brmsfz
      dummy=brmsdz(z,e,k1)
      brmsrz=qd(brmsfz,k1,k2,'brmsfz')
      return
      end

      double precision function brmstm(e0,eg)
      implicit none
      double precision e0, au, eg, brmsrm
      include 'pegscommons/dercon.f'
      if (e0.le.RM) then
        brmstm=0.
      else
        au=dmin1(eg,e0-RM)
        brmstm=brmsrm(e0,0.D0,AU)
      end if
      return
      end

      subroutine cfuns(e,v)
      implicit none
      double precision aintp, e
      include 'pegscommons/bcom.f'
      include 'pegscommons/cpcom.f'
      double precision v(1)
      v(1)=aintp(e,QCAP(1),31,AVCPRF(1),1,.true.,.true.)
      return
      end

      subroutine cfuns2(e,v)
      implicit none
      double precision aintp, e
      include 'pegscommons/bcom.f'
      include 'pegscommons/cpcom.f'
      double precision v(1)
      v(1)=aintp(e,CPROFI(1),301,QCAP10(1),1,.true.,.false.)
      return
      end

      subroutine cfuns3(e,v)
      implicit none
      integer ishell
      double precision aintp, e
      include 'pegscommons/bcom.f'
      include 'pegscommons/cpcom.f'
      double precision v(200)
      do ishell=1,MXSHEL
        v(ishell)=aintp(e,SCPROI(1,ISHELL),301,QCAP10(1),1,.true.,.false
     *  .)
      end do
      return
      end

      subroutine cfuns4(e,v)
      implicit none
      integer ishell
      double precision aintp, e
      include 'pegscommons/bcom.f'
      include 'pegscommons/cpcom.f'
      double precision v(200)
      do ishell=1,mxshel
        v(ishell)=aintp(e,QCAP(1),31,SCPSUM(1,ISHELL),1,.true.,.true.)
      end do
      return
      end

      double precision function cohetm(k)
      implicit none
      integer i
      double precision cohetz, cohetzint
      double precision k
      include 'pegscommons/mixdat.f'
      include 'pegscommons/mxdatc.f'
      include 'pegscommons/cohcom.f'
      cohetm=0.d0
      if (irayl.eq.1) then
        do i=1,NE
          cohetm=cohetm+PZ(I)*cohetz(z(i),k)
        end do
        return
      else if(irayl.eq.2) then
        cohetm=cohetzint(1.d0,k)
        return
      end if
      end

      double precision function cohetz(z,k)
      implicit none
      integer iz
      double precision pcon, z, aintp
      double precision k
      include 'pegscommons/molvar.f'
      include 'pegscommons/phpair.f'
      include 'pegscommons/pmcons.f'
      include 'pegscommons/cohcom.f'
      pcon= 1.D-24*(AN*RHO/WM)*RLC
      iz=z
      cohetz=pcon*aintp(k,PHE(1,iz),NPHE(iz),COHE(1,iz),1,.true.,.true.)
      return
      end

      double precision function cohetzint(z,k)
      implicit none
      integer iz
      double precision pcon, z, aintp
      double precision k
      include 'pegscommons/molvar.f'
      include 'pegscommons/phpair.f'
      include 'pegscommons/pmcons.f'
      include 'pegscommons/cohcom.f'
      pcon= 1.D-24*(AN*RHO/WM)*RLC
      iz=z
      cohetzint= pcon*aintp(k,PHE(1,iz),NPHE(iz),COHEINT(1,iz),1,.true.,
     *.true.)
      return
      end

      double precision function compdm(k0a,k)
      implicit none
      include 'pegscommons/pmcons.f'
      include 'pegscommons/dercon.f'
      include 'pegscommons/molvar.f'
      include 'pegscommons/lcompm.f'
      double precision k0a,k0p,compfm, k
      k0=k0a
      k0p=k0/RM
      CCOMP=RLC*EDEN*PI*R0**2/(k0*k0p)
      C1=1./k0p**2
      C2=1.-(2.+2.*k0p)/k0p**2
      C3=(1.+2.*k0p)/k0p**2
      compdm=compfm(k)
      return
      end

      double precision function compfm(k)
      implicit none
      double precision eps, epsi
      double precision k
      include 'pegscommons/lcompm.f'
      eps=k/K0
      epsi=1./eps
      compfm=CCOMP*( (C1*epsi+C2)*epsi+C3+eps )
      return
      end

      double precision function comprm(k0,k1,k2)
      implicit none
      double precision ccomp2
      include 'pegscommons/pmcons.f'
      include 'pegscommons/dercon.f'
      include 'pegscommons/molvar.f'
      double precision k0,k1,k2
      real*8 c1,c2,c3,eps1,eps2,k0p
      k0p=k0/RM
      ccomp2=RLC*EDEN*PI*R0**2/k0p
      c1=1./k0p**2
      c2=1.-(2.+2.*k0p)/k0p**2
      c3=(1.+2.*k0p)/k0p**2
      eps1=k1/k0
      eps2=k2/k0
      comprm=ccomp2*(c1*(1./eps1-1./eps2)+c2*dlog(eps2/eps1)+eps2* (c3+0
     *.5*eps2) - eps1*(c3+0.5*eps1) )
      return
      end

      double precision function comptm(k0)
      implicit none
      integer i, iz
      double precision pcon, comsum, aintp, comprm
      include 'pegscommons/bcom.f'
      include 'pegscommons/dercon.f'
      include 'pegscommons/mixdat.f'
      include 'pegscommons/mxdatc.f'
      include 'pegscommons/phpair.f'
      include 'pegscommons/pmcons.f'
      include 'pegscommons/molvar.f'
      double precision k0,k1
      if (IBOUND.eq.1) then
        pcon=1.D-24*(AN*RHO/WM)*RLC
        comsum=0.0
        do i=1,NE
          iz=Z(i)
          comsum=comsum+PZ(i)*aintp(K0,PBC(1),NPBC,BCOMP(1,iz),1,.true.,
     *    .true.)
        end do
        comptm=pcon*comsum
      else
        K1=K0*RM/(RM+2.*K0)
        comptm=comprm(K0,K1,K0)
      end if
      return
      end

      double precision function cprfil(x)
      implicit none
      double precision aintp, x
      include 'pegscommons/cpcom.f'
      cprfil=aintp(x,QCAP(1),31,AVCPRF(1),1,.true.,.true.)
      return
      end

      double precision function cratio(e)
      implicit none
      double precision tot, pairtu, e, comptm, photte, cohetm
      tot=pairtu(e)+comptm(e)+photte(e)
      cratio=tot/(tot+cohetm(e))
      return
      end

      double precision function dcadre(f,a,b,aerr,rerr,error,ier)
      implicit none
      integer maxts, maxtbl, mxstge, ier, istage, ibeg, iend, l, n,
     & lm1, n2, istep, ii, iii, i, istep2, it, ibegs, nnleft
      external f
      dimension t(10,10),r(10),ait(10),dif(10),rn(4),ts(2049)
      dimension ibegs(30),begin(30),finis(30),est(30)
      dimension reglsv(30)
      logical h2conv,aitken,right,reglar,reglsv
      double precision t,r,ait,dif,rn,ts,begin,finis,est,aitlow
      double precision h2tol,aittol,length,jumptl,zero,p1,half,one
      double precision two,four,fourp5,ten,hun,cadre,error,a,b
      double precision aerr,rerr,stepmn,stepnm,stage,curest,fnsize
      double precision prever,beg,fbeg,end,fend,step,astep,tabs,hovn
      double precision fn,sum,sumabs,absi,vint,tabtlm,ergl,ergoal
      double precision erra,errr,fextrp,errer,diff,sing,fextm1,alg4o2
      double precision h2nxt,singnx,slope,fbeg2,alpha
      double precision erret,h2tfex,fi
      double precision rval,f
      data aitlow,h2tol,aittol,jumptl,maxts,maxtbl,mxstge/1.1D0,.15D0, .
     *1D0,.01D0,2049,10,30/
      data rn(1),rn(2),rn(3),rn(4)/.7142005D0,.3466282D0,.843751D0, .126
     *3305D0/
      data zero,p1,half,one,two,four,fourp5,ten,hun/0.0D0,0.1D0,0.5D0, 1
     *.0D0,2.0D0,4.0D0,4.5D0,10.0D0,100.0D0/
      alg4o2=dlog10(TWO)
      cadre=zero
      error=zero
      curest=zero
      vint=zero
      ier=0
      length=dabs(B-A)
      if (length.eq.zero) go to 215
      if (rerr.gt.p1.or.rerr.lt.zero) go to 210
      if (aerr.eq.zero.and.(rerr+hun).le.hun) go to 210
      errr=rerr
      erra=dabs(AERR)
      stepmn=(length/float(2**mxstge))
      stepnm=dmax1(length,dabs(A),abs(B))*TEN
      stage=half
      istage=1
      fnsize=zero
      prever=zero
      reglar=.false.
      beg=A
      rval=beg
      fbeg=f(rval)*half
      ts(1)=fbeg
      ibeg=1
      end=B
      rval=end
      fend=f(rval)*half
      ts(2)=fend
      iend=2
5     right=.false.
10    step=end - beg
      astep=dabs(step)
      if (astep.lt.stepmn) go to 205
      if (stepnm+astep.eq.stepnm) go to 205
      t(1,1)=fbeg + fend
      tabs=dabs(fbeg) + dabs(fend)
      l=1
      n=1
      h2conv=.false.
      aitken=.false.
15    lm1=l
      l=l + 1
      n2=n + n
      fn=n2
      istep=(iend - ibeg)/n
      if (istep.gt.1) go to 25
      ii=iend
      iend=iend + n
      if (iend.gt.maxts) go to 200
      hovn=step/fn
      iii=iend
      fi=one
      do i=1,n2,2
        ts(iii)=ts(ii)
        rval=end-fi*hovn
        ts(iii-1)=f(rval)
        fi=fi+two
        iii=iii-2
        ii=ii-1
      end do
      istep=2
25    istep2=ibeg + istep/2
      sum=zero
      sumabs=zero
      do i=istep2,iend,istep
        sum=sum + ts(i)
        sumabs=sumabs + dabs(ts(i))
      end do
      t(l,1)=t(l-1,1)*half+sum/fn
      tabs=tabs*half+sumabs/fn
      absi=astep*tabs
      n=n2
      it=1
      vint=step*t(l,1)
      tabtlm=tabs*ten
      fnsize=dmax1(fnsize,dabs(t(l,1)))
      ergl=astep*fnsize*ten
      ergoal=stage*dmax1(erra,errr*dabs(curest+vint))
      fextrp=one
      do i=1,lm1
        fextrp=fextrp*four
        t(i,l)=t(l,i) - t(l-1,i)
        t(l,i+1)=t(l,i) + t(i,l)/(fextrp-one)
      end do
      errer=astep*dabs(t(1,l))
      if (l.gt.2) go to 40
      if (tabs+p1*dabs(t(1,2)).eq.tabs) go to 135
      go to 15
40    do i=2,lm1
      diff=zero
      if (tabtlm+dabs(t(i-1,l)).ne.tabtlm) diff=t(i-1,lm1)/t(i-1,l)
      t(i-1,lm1)=diff
      end do
      if (dabs(four-t(1,lm1)).le.h2tol) go to 60
      if (t(1,lm1).eq.zero) go to 55
      if (dabs(two-abs(t(1,lm1))).lt.jumptl) go to 130
      if (l.eq.3) go to 15
      h2conv=.false.
      if (dabs((t(1,lm1)-t(1,l-2))/t(1,lm1)).le.aittol) go to 75
50    if (reglar) go to 55
      if (l.eq.4) go to 15
55    if(errer.gt.ergoal.and.(ergl+errer).ne.ergl) go to 175
      go to 145
60    if(h2conv) go to 65
      aitken=.false.
      h2conv=.true.
65    fextrp=four
70    it=it + 1
      vint=step*t(l,it)
      errer=dabs(step/(fextrp-one)*t(it-1,l))
      if (errer.le.ergoal) go to 160
      if (ergl+errer.eq.ergl) go to 160
      if (it.eq.lm1) go to 125
      if (t(it,lm1).eq.zero) go to 70
      if (t(it,lm1).le.fextrp) go to 125
      if (dabs(t(it,lm1)/four-fextrp)/fextrp.lt.aittol) 
     *                                     fextrp=fextrp*four
      go to 70
75    if(t(1,lm1).lt.aitlow) go to 175
      if (aitken) go to 80
      h2conv=.false.
      aitken=.true.
80    fextrp=t(l-2,lm1)
      if (fextrp.gt.fourp5) go to 65
      if (fextrp.lt.aitlow) go to 175
      if (dabs(fextrp-t(l-3,lm1))/t(1,lm1).gt.h2tol) go to 175
      sing=fextrp
      fextm1=one/(fextrp - one)
      ait(1)=zero
      do i=2,l
      ait(i)=t(i,1) + (t(i,1)-t(i-1,1))*fextm1
      r(i)=t(1,i-1)
      dif(i)=ait(i) - ait(i-1)
      end do
      it=2
90    vint=step*ait(l)
      errer=errer*fextm1
      if (errer.gt.ergoal.and.(ergl+errer).ne.ergl) go to 95
      alpha=dlog10(sing)/alg4o2 - one
      ier=max0(ier,65)
      go to 160
95    it=it + 1
      if (it.eq.lm1) go to 125
      if (it.gt.3) go to 100
      h2nxt=four
      singnx=sing+sing
100   if(h2nxt.lt.singnx) go to 105
      fextrp=singnx
      singnx=singnx+singnx
      go to 110
105   fextrp=h2nxt
      h2nxt=four*h2nxt
110   do i=it,lm1
      r(i+1)=zero
      if (tabtlm+dabs(dif(i+1)).ne.tabtlm) r(i+1)=dif(i)/dif(i+1)
      end do
      h2tfex=-h2tol*fextrp
      if (r(l)-fextrp.lt.h2tfex) go to 125
      if (r(l-1)-fextrp.lt.h2tfex) go to 125
      errer=astep*dabs(dif(l))
      fextm1=one/(fextrp - one)
      do i=it,l
      ait(i)=ait(i) + dif(i)*fextm1
      dif(i)=ait(i) - ait(i-1)
      end do
      go to 90
125   fextrp=dmax1(prever/errer,aitlow)
      prever=errer
      if (l.lt.5) go to 15
      if (l-it.gt.2.and.istage.lt.mxstge) go to 170
      erret=errer/(fextrp**(maxtbl-l))
      if (erret.gt.ergoal.and.(ergl+erret).ne.ergl) go to 170
      go to 15
130   if(errer.gt.ergoal.and.(ergl+errer).ne.ergl) go to 170
      diff=dabs(t(1,l))*(fn+fn)
      go to 160
135   slope=(fend-fbeg)*two
      fbeg2=fbeg+fbeg
      do i=1,4
      rval=beg+rn(i)*step
      diff=dabs(f(rval) - fbeg2-rn(i)*slope)
      if (tabtlm+diff.ne.tabtlm) go to 155
      end do
      go to 160
145   slope=(fend-fbeg)*two
      fbeg2=fbeg+fbeg
      i=1
150   rval=beg+rn(i)*step
      diff=dabs(f(rval) - fbeg2-rn(i)*slope)
155   errer=dmax1(errer,astep*diff)
      if (errer.gt.ergoal.and.(ergl+errer).ne.ergl) go to 175
      i=i+1
      if (i.le.4) go to 150
      ier=66
160   cadre=cadre + vint
      error=error + errer
      if (right) go to 165
      istage=istage - 1
      if (istage.eq.0) go to 220
      reglar=reglsv(istage)
      beg=begin(istage)
      end=finis(istage)
      curest=curest - est(istage+1) + vint
      iend=ibeg - 1
      fend=ts(iend)
      ibeg=ibegs(istage)
      go to 180
165   curest=curest + vint
      stage=stage+stage
      iend=ibeg
      ibeg=ibegs(istage)
      end=beg
      beg=begin(istage)
      fend=fbeg
      fbeg=ts(ibeg)
      go to 5
170   reglar=.true.
175   if(istage.eq.mxstge) go to 205
      if (right) go to 185
      reglsv(istage+1)=reglar
      begin(istage)=beg
      ibegs(istage)=ibeg
      stage=stage*half
180   right=.true.
      beg=(beg+end)*half
      ibeg=(ibeg+iend)/2
      ts(ibeg)=ts(ibeg)*half
      fbeg=ts(ibeg)
      go to 10
185   nnleft=ibeg - ibegs(istage)
      if (iend+nnleft.ge.maxts) go to 200
      iii=ibegs(istage)
      ii=iend
      do i=iii,ibeg
      ii=ii + 1
      ts(ii)=ts(i)
      end do
      do i=ibeg,ii
      ts(iii)=ts(i)
      iii=iii + 1
      end do
      iend=iend + 1
      ibeg=iend - nnleft
      fend=fbeg
      fbeg=ts(ibeg)
      finis(istage)=end
      end=beg
      beg=begin(istage)
      begin(istage)=end
      reglsv(istage)=reglar
      istage=istage + 1
      reglar=reglsv(istage)
      est(istage)=vint
      curest=curest + est(istage)
      go to 5
200   ier=131
      go to 215
205   ier=132
      go to 215
210   ier=133
215   cadre=curest + vint
220   dcadre=cadre
      return
      end

      subroutine differ
      implicit none
      double precision al183, f10, f20, a1den, a2den, b1den, b2den,
     & c1den, c2den
      include 'pegscommons/molvar.f'
      include 'pegscommons/bremp2.f'
      include 'pegscommons/dbrpr.f'
      include 'pegscommons/dercon.f'
      include 'pegscommons/radlen.f'
      al2 = dlog(2.D0)
      al183= dlog(a183)
      alphi(1)= al2*(4./3. + 1./(9.*al183*(1.+ZP)))
      alphi(2)= al2*(4./3. + 1./(9.*al183*(1.+ZU)))
      alfp1(1)= 2./3. - 1./(36.*al183*(1.+ZP))
      alfp1(2)= 2./3. - 1./(36.*al183*(1.+ZU))
      alfp2(1)= (1./12.)*(4./3. + 1./(9.*al183*(1+ZP)))
      alfp2(2)= (1./12.)*(4./3. + 1./(9.*al183*(1+ZU)))
      bpar(1)= alfp1(1)/(alfp1(1)+alfp2(1))
      bpar(2)= alfp1(2)/(alfp1(2)+alfp2(2))
      delcm= 136.0*dexp(ZG)*RM
      delpos(1)= (dexp((21.12+4.*ZG)/4.184)-0.952)/DELCM
      delpos(2)= (dexp((21.12+4.*ZV)/4.184)-0.952)/DELCM
      f10=4.*al183
      f20=f10 - 2./3.
      a1den =3.0*f10- f20 + 8.0*ZG
      a2den =3.0*f10- f20 + 8.0*ZV
      b1den = f10 + 4.0*ZG
      b2den = f10 + 4.0*ZV
      c1den = 3.0*f10+ f20 + 16.0*ZG
      c2den = 3.0*f10+ f20 + 16.0*ZV
      dl1(1)= (3.0*20.867-20.209+8.0*ZG)/a1den
      dl2(1)= (3.0*(-3.242)-(-1.930))/a1den
      dl3(1)= (3.0*(0.625)-(0.086))/a1den
      dl4(1)= (2.0*21.12+8.0*ZG)/a1den
      dl5(1)= 2.0*(-4.184)/a1den
      dl6(1)= 0.952
      dl1(4)= (3.0*20.867-20.209+8.0*ZV)/a2den
      dl2(4)= (3.0*(-3.242)-(-1.930))/a2den
      dl3(4)= (3.0*(0.625)-(0.086))/a2den
      dl4(4)= (2.0*21.12+8.0*ZV)/a2den
      dl5(4)= 2.0*(-4.184)/a2den
      dl6(4)= 0.952
      dl1(2)= (20.867+4.0*ZG)/b1den
      dl2(2)= -3.242/b1den
      dl3(2)= 0.625/b1den
      dl4(2)= (21.12+4.0*ZG)/b1den
      dl5(2)= -4.184/b1den
      dl6(2)= 0.952
      dl1(5)= (20.867+4.0*ZV)/b2den
      dl2(5)= -3.242/b2den
      dl3(5)= 0.625/b2den
      dl4(5)= (21.12+4.0*ZV)/b2den
      dl5(5)= -4.184/b2den
      dl6(5)= 0.952
      dl1(3)= (3.0*20.867+20.209+16.0*ZG)/c1den
      dl2(3)= (3.0*(-3.242)+(-1.930))/c1den
      dl3(3)= (3.0*0.625+(-0.086))/c1den
      dl4(3)= (4.0*21.12+16.0*ZG)/c1den
      dl5(3)= 4.0*(-4.184)/c1den
      dl6(3)= 0.952
      dl1(6)= (3.0*20.867+20.209+16.0*ZV)/c2den
      dl2(6)= (3.0*(-3.242)+(-1.930))/c2den
      dl3(6)= (3.0*0.625+(-0.086))/c2den
      dl4(6)= (4.0*21.12+16.0*ZV)/c2den
      dl5(6)= 4.0*(-4.184)/c2den
      dl6(6)= 0.952
      write(  26,100)
100   format(/,' In subroutine differ:'// ' Differential cross-section d
     *ata,common brempr'/ ' dl1(6),dl2(6),dl3(6),dl4(6),dl5(6),dl6(6),al
     *phi(2),bpar(2),', 'delcm,delpos(2)')
      write(  26,110) dl1,dl2,dl3,dl4,dl5,dl6,alphi,bpar,delcm,delpos
110   format(1X,6E14.5)
      return
      end

      double precision function ebind(e)
      implicit none
      integer i, j
      double precision phottz, e, stot, photte
      include 'pegscommons/mixdat.f'
      include 'pegscommons/mxdatc.f'
      include 'pegscommons/phpair.f'
      ebind=0.0
      do i=1,NE
        j=z(i)
        ebind=ebind+PZ(i)*phottz(z(i),e)*EKEDGE(j)*0.001
      end do
      stot=photte(e)
      if (stot.ne.0.0) ebind=ebind/stot
      return
      end

      double precision function ebr1(e)
      implicit none
      double precision brem, bremtm, e, tebr, amoltm
      brem=bremtm(e)
      tebr=brem+amoltm(e)
      if (tebr.gt.0.0) then
        ebr1=brem/tebr
      else
        ebr1=0.0
      end if
      return
      end

      double precision function ededx(e)
      implicit none
      double precision sptote, e
      include 'pegscommons/thres2.f'
      ededx=sptote(e,ae,ap)
      return
      end

      subroutine efuns(e,v)
      implicit none
      double precision brem, bremtm, e, amoll, amoltm, bhab, bhabtm,
     & annih, anihtm, esig, psig, sptote, sptotp, tmxs, g1e, k1e, 
     & csdar, estepmax
      double precision v(15)
      include 'pegscommons/thres2.f'
      include 'pegscommons/legacy.f'
      if (iunrst.eq.0 .or. iunrst.eq.1 .or. iunrst.eq.5) then
        brem=bremtm(e)
        amoll=amoltm(e)
        bhab=bhabtm(e)
        annih=anihtm(e)
        esig=brem+amoll
        v(1)=esig
        psig=brem+bhab+annih
        v(2)=psig
        v(3)=sptote(e,AE,AP)
        v(4)=sptotp(e,AE,AP)
        if (esig.gt.0.0) then
          v(5)=brem/esig
        else
          if (thbrem.le.thmoll) then
            v(5)=1.0
          else
            v(5)=0.0
          end if
        end if
        v(6)=brem/psig
        v(7)=(brem+bhab)/psig
        v(8)=tmxs(e)
        v(9) = g1e(-1,e,0)
        v(10) = g1e(+1,e,0)
        if(oldK1run) then
          v(11) = k1e(-1,e)
          v(12) = k1e(+1,e)
        else
          v(11) = 0.d0
          v(12) = 0.d0
        endif
        v(13) = csdar(-1,e)
        v(14) = csdar(+1,e)
        v(15) = estepmax(e)
      else if (iunrst.eq.2) then
        v(1)=0.0
        v(2)=0.0
        v(5)=0.0
        v(6)=0.0
        v(7)=0.0
        v(3) = sptote(e,e,e)
        v(4) = sptotp(e,e,e)
        v(8) = tmxs(e)
        v(9) = g1e(-1,e,0)
        v(10) = g1e(+1,e,0)
        if(oldK1run) then
          v(11) = k1e(-1,e)
          v(12) = k1e(+1,e)
        else
          v(11) = 0.d0
          v(12) = 0.d0
        endif
        v(13) = csdar(-1,e)
        v(14) = csdar(+1,e)
        v(15) = estepmax(e)
      else if (iunrst.eq.3) then
        brem=bremtm(e)
        annih=anihtm(e)
        v(1)=brem
        v(2)=brem + annih
        v(3)=sptote(e,e,AP)
        v(4)=sptotp(e,e,AP)
        v(5)=1.0
        v(6)=brem/v(2)
        v(7)=v(6)
        v(8)=tmxs(e)
        v(9) = g1e(-1,e,0)
        v(10) = g1e(+1,e,0)
        if(oldK1run) then
          v(11) = k1e(-1,e)
          v(12) = k1e(+1,e)
        else
          v(11) = 0.d0
          v(12) = 0.d0
        endif
        v(13) = csdar(-1,e)
        v(14) = csdar(+1,e)
        v(15) = estepmax(e)
      else if (iunrst.eq.4) then
        v(1)=amoltm(e)
        v(2)=bhabtm(e)
        v(3)=sptote(e,AE,e)
        v(4)=sptotp(e,AE,e)
        v(5)=0.0
        v(6)=0.0
        v(7)=1.0
        v(8)=tmxs(e)
        v(9) = g1e(-1,e,0)
        v(10) = g1e(+1,e,0)
        if(oldK1run) then
          v(11) = k1e(-1,e)
          v(12) = k1e(+1,e)
        else
          v(11) = 0.d0
          v(12) = 0.d0
        endif
        v(13) = csdar(-1,e)
        v(14) = csdar(+1,e)
        v(15) = estepmax(e)
      else
        write(  26,100) iunrst
100     format(//'*********Iunrst=',I4,' not allowed by efuns*****'/ ' I
     *unrst=6 or 7 only allowed with call or pltn options'//)
        close(26)
        stop
      end if
      return
      end

      subroutine eiifuns(e,v)
      implicit none
      integer i
      double precision amoll, amoltm, e, eiisum, zval, eiitm
      double precision v(20)
      include 'pegscommons/mixdat.f'
      include 'pegscommons/mxdatc.f'
      do i=1,20
        v(i)=0.0
      end do
      amoll=amoltm(e)
      if (amoll.lt.1.0d-30) return
      eiisum=0.0
      do i=1,NE
        zval=Z(I)
        eiisum=eiisum+eiitm(e,zval)*PZ(i)
        v(i)=eiisum/amoll
      end do
      return
      end

      double precision function eiitm(e,zval)
      implicit none
      integer j, nismall
      double precision zval, ekbmev, x, e, capi, cape, fr1, fr2, fr3,
     & fr4, rfact, smalla0, capi0, capu, smalld0, smalld1, smalld2,
     & smallb0, smallb1, smallb2, sphi, spsi, dexp, qcap, dlog, cape1,
     & cape2, smalph, eke0, qcapa, qdist2, qclose, qdist, beta2a,
     & beta2, beta02, fcap1, fcap2, fcap3, fcap4, fcap5, sma, smb,
     & smc, qconst, g1, g2
      include 'pegscommons/dercon.f'
      include 'pegscommons/eimpact.f'
      include 'pegscommons/phpair.f'
      include 'pegscommons/pmcons.f'
      include 'pegscommons/mixdat.f'
      include 'pegscommons/mxdatc.f'
      include 'pegscommons/molvar.f'
      J=ZVAL
      ekbmev=ekedge(j)*0.001
      if (ekbmev.eq.0.0) then
        eiitm=0.0
        return
      end if
      x=(e-RM)/ekbmev
      nismall=2
      if (x.gt.1.001) then
        capi=ekedge(j)/RM/1000.0
        cape=(e-RM)/RM
        fr1=(2.0+capi)/(2.0+cape)
        fr2=(1.0+cape)/(1.0+capi)
        fr3=(capi+cape)*(2.0+cape)*(1.0+capi)**2
        fr4=cape*(2.0+cape)*(1.0+capi)**2+capi*(2.0+capi)
        rfact=fr1*fr2**2*(fr3/fr4)**1.5
        if (impact.eq.1) then
          smalla0=5.292E3
          capi0=13.606D-3
          capu=(e-RM)/(EKEDGE(j)*0.001)
          smalld0=-0.0318
          smalld1=0.3160
          smalld2=-0.1135
          smallb0=10.57
          smallb1=-1.736
          smallb2=0.317
          sphi=(EKEDGE(j)/capi0)**(smalld0+smalld1/capu+smalld2/capu**2)
          spsi=smallb0*dexp(smallb1/capu+smallb2/capu**2)
          qcap=nismall*smalla0**2*rfact*(capi0/EKEDGE(j))**2*sphi*spsi *
     *    dlog(capu)/capu
        end if
        if (impact.eq.2) then
          cape=(e-RM)/RM
          cape1=cape+1.0
          cape2=cape+2.0
          capi=ekbmev/RM
          smalph=1.0/137.036
          eke0=0.5*(smalph*ZVAL)**2*RM*1000.0
          qcapa=cape1*cape1/capi/cape/cape2
          qdist2=0.275*(eke0/EKEDGE(j))**3*((1.-16./13.*(1.-EKEDGE(j)/ek
     *    e0))* (dlog(2.*cape*cape2/capi)-cape*cape2/(cape1*cape1))-55./
     *    78.- 32./39.*(1.-EKEDGE(j)/eke0))
          qclose=0.99*(1.0-capi/cape*(1.0-cape*cape/2.0/cape1/cape1+ (2.
     *    0*cape+1.0)/cape1/cape1*dlog(cape/capi)))
          qcap=qcapa*(qdist2+qclose)
        end if
        if (impact.eq.3) then
          cape=(e-RM)/RM
          cape1=cape+1.0
          cape2=cape+2.0
          capi=ekbmev/RM
          qcapa=cape1*cape1/capi/cape/cape2
          qdist=0.275*(dlog(1.19*cape*cape2/capi)-cape*cape2/(cape1*cape
     *    1))
          qclose=0.99*(1.0-capi/cape*(1.0-cape*cape/2.0/cape1/cape1+ (2.
     *    0*cape+1.0)/cape1/cape1*dlog(cape/capi)))
          qcap=qcapa*(qdist+qclose)
        end if
        if (impact.eq.4) then
          beta2a=(1.0+(e-RM)/RM)**(-2)
          beta2=1.0-beta2a
          beta02=1.0-(1.0+EKEDGE(j)/(RM*1000))**(-2)
          fcap1=254.9/(EKEDGE(j)*beta2)
          fcap2=dlog(beta2/beta2a)-beta2
          fcap3=1.0-beta02/beta2
          fcap4=dlog(1.0/beta02)
          fcap5=beta02/beta2
          sma=5.14*ZVAL**(-0.48)
          smb=5.76-0.04*ZVAL
          smc=0.72+0.039*ZVAL-0.0006*ZVAL**2
          qcap=sma*fcap1*(fcap2+smb*fcap3+fcap4*fcap5**smc)
        end if
        if (impact.eq.5.or.impact.eq.6) then
          qconst=0.0656
          g1=1.0/x*((x-1.0)/(x+1.0))**1.5
          g2=1.0+0.6667*(1.0-0.5/x)*dlog(2.7+dsqrt(x-1.d0))
          qcap=qconst*nismall/ekbmev**2*g1*g2
          if (impact.eq.6) then
            qcap=qcap*rfact
          end if
        end if
        if (qcap.lt.0.0) then
          qcap=0.0
        end if
        eiitm=qcap*AN*1.0D-24/WM*RHO*RLC
      else
        eiitm=0.0
      end if
      return
      end

      double precision function esig(e)
      implicit none
      double precision bremtm, e, amoltm
      esig=bremtm(e)+amoltm(e)
      return
      end

      double precision function fcoulc(z)
      implicit none
      double precision asq, z
      include 'pegscommons/dercon.f'
      asq=(FSC*z)**2
      fcoulc = asq*(1.0/(1.0+asq)+0.20206+asq*(-0.0369+ asq*(0.0083+asq*
     *(-0.002))))
      return
      end

      double precision function fi(i,x1,x2,x3,x4)
      implicit none
      integer i
      double precision alin, x1, alini, adfmol, adimol, addmol, dlog,
     & dexp, arec, alke, alkei, amoldm, x2, amolfm, amolrm, x3,
     & amoltm, anihdm, anihfm, anihrm, anihtm, aprim, bhabdm, bhabfm,
     & bhabrm, bhabtm, bremdr, bremfr, bremdz, brmsdz, bremfz, brmsfz,
     & bremrr, bremrm, bremrz, x4, bremtm, bremtr, brmsrm, brmsrz,
     & brmstm, cohetm, cohetz, compdm, compfm, comprm, comptm, cratio,
     & ebind, ebr1, ededx, eiitm, esig, fcoulc, gbr1, gbr2, gmfp,
     & pairdr, pairfr, pairdz, pairfz, pairrm, pairrr, pairrz, pairte,
     & pairtm, pairtr, pairtu, pairtz, pbr1, pbr2, pdedx, phottz,
     & photte, psig, spione, spionp, sptote, sptotp, tmxb, tmxs,
     & tmxde2, xsif
      go to(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,
     *24,25,26,27,28,29,30,31,32,33,34,35,36,37,38,39,40,41,42,43,44,45,
     *46,47,48,49,50,51,52,53,54,55,56,57,58,59,60,61,62,63,64,65,66,67,
     *68,69,70,71,72,73,74,75,76,77,78,79),i
1     fi=alin(x1)
      return
2     fi=alini(x1)
      return
3     fi=adfmol(x1)
      return
4     fi=adimol(x1)
      return
5     fi=addmol(x1)
      return
6     fi=dlog(x1)
      return
7     fi=dexp(x1)
      return
8     fi=arec(x1)
      return
9     fi=alke(x1)
      return
10    fi=alkei(x1)
      return
11    fi=amoldm(x1,x2)
      return
12    fi=amolfm(x1)
      return
13    fi=amolrm(x1,x2,x3)
      return
14    fi=amoltm(x1)
      return
15    fi=anihdm(x1,x2)
      return
16    fi=anihfm(x1)
      return
17    fi=anihrm(x1,x2,x3)
      return
18    fi=anihtm(x1)
      return
19    fi=aprim(x1,x2)
      return
20    fi=bhabdm(x1,x2)
      return
21    fi=bhabfm(x1)
      return
22    fi=bhabrm(x1,x2,x3)
      return
23    fi=bhabtm(x1)
      return
24    fi=bremdr(x1,x2)
      return
25    fi=bremfr(x1)
      return
26    fi=bremdz(x1,x2,x3)
      return
27    fi=brmsdz(x1,x2,x3)
      return
28    fi=bremfz(x1)
      return
29    fi=brmsfz(x1)
      return
30    fi=bremrr(x1,x2,x3)
      return
31    fi=bremrm(x1,x2,x3)
      return
32    fi=bremrz(x1,x2,x3,x4)
      return
33    fi=bremtm(x1)
      return
34    fi=bremtr(x1)
      return
35    fi=brmsrm(x1,x2,x3)
      return
36    fi=brmsrz(x1,x2,x3,x4)
      return
37    fi=brmstm(x1,x2)
      return
38    fi=cohetm(x1)
      return
39    fi=cohetz(x1,x2)
      return
40    fi=compdm(x1,x2)
      return
41    fi=compfm(x1)
      return
42    fi=comprm(x1,x2,x3)
      return
43    fi=comptm(x1)
      return
44    fi=cratio(x1)
      return
45    fi=ebind(x1)
      return
46    fi=ebr1(x1)
      return
47    fi=ededx(x1)
      return
48    fi=eiitm(x1,x2)
      return
49    fi=esig(x1)
      return
50    fi=fcoulc(x1)
      return
51    fi=gbr1(x1)
      return
52    fi=gbr2(x1)
      return
53    fi=gmfp(x1)
      return
54    fi=pairdr(x1,x2)
      return
55    fi=pairfr(x1)
      return
56    fi=pairdz(x1,x2,x3)
      return
57    fi=pairfz(x1)
      return
58    fi=pairrm(x1,x2,x3)
      return
59    fi=pairrr(x1,x2,x3)
      return
60    fi=pairrz(x1,x2,x3,x4)
      return
61    fi=pairte(x1)
      return
62    fi=pairtm(x1)
      return
63    fi=pairtr(x1)
      return
64    fi=pairtu(x1)
      return
65    fi=pairtz(x1,x2)
      return
66    fi=pbr1(x1)
      return
67    fi=pbr2(x1)
      return
68    fi=pdedx(x1)
      return
69    fi=phottz(x1,x2)
      return
70    fi=photte(x1)
      return
71    fi=psig(x1)
      return
72    fi=spione(x1,x2)
      return
73    fi=spionp(x1,x2)
      return
74    fi=sptote(x1,x2,x3)
      return
75    fi=sptotp(x1,x2,x3)
      return
76    fi=tmxb(x1)
      return
77    fi=tmxs(x1)
      return
78    fi=tmxde2(x1)
      return
79    fi=xsif(x1)
      return
      end

      double precision function gbr1(e)
      implicit none
      double precision pair, pairtu, e, comptm, photte
      pair=pairtu(e)
      gbr1=pair/(pair+comptm(e)+photte(e))
      return
      end

      double precision function gbr2(e)
      implicit none
      double precision prco, pairtu, e, comptm, photte
      prco=pairtu(e)+comptm(e)
      gbr2=prco/(prco+photte(e))
      return
      end

      subroutine gfuns(e,v)
      implicit none
      double precision pair, pairtu, e, comp, comptm, phot, photte,
     & cohr, cohetm, tsansc, gmfp
      double precision v(4)
      pair=pairtu(e)
      comp=comptm(e)
      phot=photte(e)
      cohr=cohetm(e)
      tsansc=pair+comp+phot
      gmfp=1.0/tsansc
      v(1)=gmfp
      v(2)=pair*gmfp
      v(3)=(pair+comp)*gmfp
      v(4)=tsansc/(tsansc+cohr)
      return
      end

      double precision function gmfp(e)
      implicit none
      double precision pairtu, e, comptm, photte
      gmfp=1.0/(pairtu(e)+comptm(e)+photte(e))
      return
      end

      subroutine hplt1(ei,el,eh,icap,ntimes,nbins,nh,idf,idsig, irsig,it
     *sig)
      implicit none
      integer ibin, irsig, itsig, idf, nbins, ntimes, i, j, idsig, ic
      double precision amax, rtot, fi, ei, el, eh, ttot, dfh, dfl,
     & deldf, dnorm, eli, ehi, eint, v, y
      integer nh(200)
      character*4 icap(12)
      include 'pegscommons/funcs.f'
      include 'pegscommons/funcsc.f'
      character*4 l(100),cm,cr,cd,cbl
      integer ipnts
      data l/100*' '/,cm/'m'/,cr/'r'/,cd/'d'/,cbl/' '/,ipnts/10/
      ibin(y)=max0(1,min0(100,idint(y/amax*100.)+1))
      rtot=fi(irsig,ei,el,eh,0.d0)
      ttot=fi(itsig,ei,0.d0,0.d0,0.d0)
      dfh=fi(idf,eh,0.d0,0.d0,0.d0)
      dfl=fi(idf,el,0.d0,0.d0,0.d0)
      deldf=(dfh-dfl)/nbins
      dnorm=rtot/(deldf*ntimes)
      amax=0.0
      eli=el
      do i=1,nbins
        ehi=fi(idf+1,dfl+deldf*i,0.d0,0.d0,0.d0)
        amax=dmax1(amax,nh(i)*dnorm,fi(irsig,ei,eli,ehi,0.d0)/deldf)
        do j=1,ipnts
          eint=fi(idf+1,dfl+deldf*(i-1+float(j-1)/(ipnts-1)),
     *                       0.d0,0.d0,0.d0)
          amax=dmax1(amax,fi(idsig,ei,eint,0.d0,0.d0)/
     *                  fi(idf+2,eint,0.d0,0.d0,0.d0))
        end do
        eli=ehi
      end do
      write(  26,100) icap,(fname(i,idsig),i=1,6),(fname(i,irsig),
     *  i=1,6), (fname(i,itsig),i=1,6),((fname(i,idf+j-1),i=1,6),j=1,3),
     *  rtot,ttot
100   format(' HPLT functions:monte,dsig,rsig,tsig,cdf,cdfinverse,pdf=',
     * 12a1,6(',',6a1)/' rtot,ttot=',1p,2e15.5)
      write(  26,110) icap,ei,el,eh,nbins,ntimes,(nh(i),i=1,nbins)
110   formaT(' HPLT:raw egs data for routine ',12a1,',ei,elo,ehi=', 3F12
     *.3,',nbins,ntimes=',2I10,',data='/(1X,10I10))
      write(  26,120)
120   format(' Key to plot,m=montecarlo data,r=theoretical integrals', '
     * over bins,d=differential cross-section'/ '    energy          val
     *ue')
      eli=el
      do i=1,nbins
        ehi=fi(idf+1,dfl+deldf*i,0.d0,0.d0,0.d0)
        v=nh(i)*dnorm
        ic=ibin(v)
        l(ic)=cm
        write(  26,130) eli,v,l
130     format(1X,1P,2E15.5,' I',100A1)
        l(ic)=cbl
        v=fi(irsig,ei,eli,ehi,0.d0)/deldf
        ic=ibin(v)
        l(ic)=cr
        write(  26,140) eli,v,l
140     format(1X,1P,2E15.5,' i',100A1)
        l(ic)=cbl
        do j=1,ipnts
          eint=fi(idf+1,dfl+deldf*(i-1+float(j-1)/(ipnts-1)),
     *               0.d0,0.d0,0.d0)
          v=fi(idsig,ei,eint,0.d0,0.d0)/fi(idf+2,eint,0.d0,0.d0,0.d0)
          ic=ibin(v)
          l(ic)=cd
          write(  26,150) eint,v,l
150       format(1X,1P,2E15.5,' i',100A1)
          l(ic)=cbl
        end do
        eli=ehi
      end do
      return
      end

      integer function ifunt(name)
      implicit none
      integer if, j
      character*4 name(6)
      include 'pegscommons/funcs.f'
      include 'pegscommons/funcsc.f'
      do 100 if=1,nfuns
        do j=1,6
          if (name(j).ne.fname(j,if)) go to 100
        end do
        ifunt=if
        return
100   continue
      ifunt=-1
      write(  26,110) name
110   format(' FUNC=',6A1,' not matched')
      return
      end

      subroutine lay
      implicit none
      integer ip, iuecho, ie, nsge, nseke, nleke, ncmfp, nrange, nge,
     & neke, i, ifun, ishell, is
      include 'pegscommons/bremp2.f'
      include 'pegscommons/mixdat.f'
      include 'pegscommons/mxdatc.f'
      include 'pegscommons/cohcom.f'
      include 'pegscommons/rslts.f'
      include 'pegscommons/thres2.f'
      include 'pegscommons/molvar.f'
      include 'pegscommons/eimpact.f'
      include 'pegscommons/epstar.f'
      include 'pegscommons/phpair.f'
      include 'pegscommons/bcom.f'
      include 'pegscommons/cpcom.f'
      include 'pegscommons/sfcom.f'
100   FORMAT(1X,14I5)
110   FORMAT(1X,1P,5E14.5)
      ip=7
      iuecho=  26
      write(iuecho,120)
120   FORMAT(' $ECHO WRITE:MEDIUM,IDSTRN')
      write(ip,130) medium,idstrn
      write(iuecho,130) medium,idstrn
130   FORMAT(' MEDIUM=',24A1,',STERNCID=',24A1)
      if (gasp.ne.0.0) then
        write(iuecho,140)
140     FORMAT(' $ECHO WRITE:MTYP,RHO,NE,GASP, IUNRST,EPSTFL,IAPRIM')
        write(ip,150) mtyp,rho,ne,gasp, iunrst,epstfl,iaprim
        write(iuecho,150) mtyp,rho,ne,gasp, iunrst,epstfl,iaprim
150     FORMAT(1X,4A1,',RHO=',1P,E11.4,',NE=',I2,',GASP=', 1P,E11.4,', I
     *UNRST=',I1,', EPSTFL=',I1,', IAPRIM=',I1)
      else
        write(iuecho,160)
160     FORMAT(' $ECHO WRITE:MTYP,RHO,NE,IUNRST,EPSTFL,IAPRIM')
        write(ip,170) mtyp,rho,ne,iunrst,epstfl,iaprim
        write(iuecho,170) mtyp,rho,ne,iunrst,epstfl,iaprim
170     FORMAT(1X,4A1,',RHO=',1P,E11.4,',NE=',I2,', IUNRST=',I1, ', EPST
     *FL=',I1,', IAPRIM=',I1)
      end if
      do ie=1,ne
        write(iuecho,180)
180     FORMAT(' $ECHO WRITE:ASYM(IE),Z(IE),WA(IE),PZ(IE),RHOZ(IE)')
        write(ip,190) asym(ie),z(ie),wa(ie),pz(ie),rhoz(ie)
        write(iuecho,190) asym(ie),z(ie),wa(ie),pz(ie),rhoz(ie)
190     FORMAT(' ASYM=',A2,',Z=',F3.0,',A=',F9.3, ',PZ=',1P,E12.5,',RHOZ
     *=',E12.5)
      end do
      write(iuecho,200)
200   FORMAT(' $ECHO WRITE:RLC,AE,AP,UE,UP')
      write(ip,110) rlc,ae,ap,ue,up
      write(iuecho,110) rlc,ae,ap,ue,up
      nsge=0
      nseke=0
      nleke=0
      ncmfp=0
      nrange=0
      nge=ngl
      neke=nel
      write(iuecho,210)
210   FORMAT(' $ECHO WRITE:NSGE,NGE,NSEKE,NEKE,NLEKE,NCMFP,NRANGE,IRAYL,
     * IBOUND,INCOH,ICPROF,IMPACT')
      write(ip,100) nsge,nge,nseke,neke,nleke,ncmfp,nrange,irayl,ibound
     *,incoh,icprof,impact
      write(iuecho,100) nsge,nge,nseke,neke,nleke,ncmfp,nrange,irayl,
     *                  ibound,incoh,icprof,impact
      write(iuecho,220)
220   FORMAT(' $ECHO WRITE:(DL1(I),DL2(I),DL3(I),DL4(I),DL5(I),DL6(I),I=
     *1,6)')
      write(ip,110) (dl1(i),dl2(i),dl3(i),dl4(i),dl5(i),dl6(i),i=1,6)
      write(iuecho,110) (dl1(i),dl2(i),dl3(i),dl4(i),dl5(i),dl6(i),i=1,6
     *)
      write(iuecho,230)
230   FORMAT(' $ECHO WRITE:DELCM,(ALPHI(I),BPAR(I),DELPOS(I),I=1,2)')
      write(ip,110) delcm,(alphi(i),bpar(i),delpos(i),i=1,2)
      write(iuecho,110) delcm,(alphi(i),bpar(i),delpos(i),i=1,2)
      write(iuecho,240)
240   FORMAT(' $ECHO WRITE:XR0,TEFF0,BLCC,XCC')
      write(ip,110) xr0,teff0,blcc,xcc
      write(iuecho,110) xr0,teff0,blcc,xcc
      write(iuecho,250)
250   FORMAT(' $Echo write:BXE,AXE')
      write(ip,110) bxe,axe
      write(iuecho,110) bxe,axe
      write(iuecho,260)
260   FORMAT(' $Echo write:((BFE(i,ifun),AFE(i,ifun),ifun=1,15),
     *i=1,neke)')
      write(ip,110) ((bfe(i,ifun),afe(i,ifun),ifun=1,15),i=1,neke)
      write(iuecho,110) ((bfe(i,ifun),afe(i,ifun),ifun=1,15),i=1,neke)
      write(iuecho,270)
270   FORMAT(' $Echo write:EBINDA,BXG,AXG')
      write(ip,110) ebinda,bxg,axg
      write(iuecho,110) ebinda,bxg,axg
      write(iuecho,280)
280   FORMAT(' $Echo write:((bfg(i,ifun),afg(i,ifun),ifun=1,3),i=1,nge)'
     *)
      write(ip,110) ((bfg(i,ifun),afg(i,ifun),ifun=1,3),i=1,nge)
      write(iuecho,110) ((bfg(i,ifun),afg(i,ifun),ifun=1,3),i=1,nge)
      if (irayl.ne.0) then
        write(iuecho,290)
290     FORMAT(' $Echo write:ngr')
        write(ip,100) ngr
        write(iuecho,100) ngr
        write(iuecho,300)
300     FORMAT(' $Echo write:bxr,axr')
        write(ip,110) bxr,axr
        write(iuecho,110) bxr,axr
        write(iuecho,310)
310     FORMAT(' $Echo write:(bfr(i),afr(i),i=1,ngr)')
        write(ip,110) (bfr(i),afr(i),i=1,ngr)
        write(iuecho,110) (bfr(i),afr(i),i=1,ngr)
        write(iuecho,320)
320     FORMAT(' $Echo write:(bfg(i,4),afg(i,4),i=1,nge)')
        write(ip,110) (bfg(i,4),afg(i,4),i=1,nge)
        write(iuecho,110) (bfg(i,4),afg(i,4),i=1,nge)
      end if
      if (incoh.eq.1) then
        write(iuecho,330)
330     FORMAT(' $Echo write:ngs')
        write(ip,100) ngs
        write(iuecho,100) ngs
        write(iuecho,340)
340     FORMAT(' $Echo write:bxs,axs')
        write(ip,110) bxs,axs
        write(iuecho,110) bxs,axs
        write(iuecho,350)
350     FORMAT(' $Echo write:(bfs(i),afs(i),i=1,ngs)')
        write(ip,110) (bfs(i),afs(i),i=1,ngs)
        write(iuecho,110) (bfs(i),afs(i),i=1,ngs)
      end if
      if (icprof.eq.1.or.icprof.eq.2) then
        write(iuecho,360)
360     FORMAT(' $Echo write:ngc')
        write(ip,100) ngc
        write(iuecho,100) ngc
        write(iuecho,370)
370     FORMAT(' $Echo write:bxc,axc,cpimev')
        write(ip,110) bxc,axc,cpimev
        write(iuecho,110) bxc,axc,cpimev
        write(iuecho,390)
390     FORMAT(' $Echo write:(bfc(i),afc(i),i=1,ngc)')
        write(ip,110) (bfc(i),afc(i),i=1,ngc)
        write(iuecho,110) (bfc(i),afc(i),i=1,ngc)
      end if
      if (icprof.eq.3.or.icprof.eq.4) then
        write(iuecho,400)
400     FORMAT(' $Echo write:mxshel,ngcs')
        write(ip,100) mxshel,ngcs
        write(iuecho,100) mxshel,ngcs
        write(iuecho,410)
410     FORMAT(' $Echo write:(elecno(ishell),ishell=1,mxshel)')
        write(ip,110) (elecno(ishell),ishell=1,mxshel)
        write(iuecho,110) (elecno(ishell),ishell=1,mxshel)
        write(iuecho,420)
420     FORMAT(' $Echo write:(capio(ishell),ishell=1,mxshel)')
        write(ip,110) (capio(ishell),ishell=1,mxshel)
        write(iuecho,110) (capio(ishell),ishell=1,mxshel)
        write(iuecho,430)
430     FORMAT(' $Echo write:bxcs,axcs')
        write(ip,110) bxcs,axcs
        write(iuecho,110) bxcs,axcs
        write(iuecho,440)
440     FORMAT(' $Echo write:((bfcs(i,is),afcs(i,is),is=1,mxshel),i=1,ng
     *cs)')
        write(ip,110) ((bfcs(i,is),afcs(i,is),is=1,mxshel),i=1,ngcs)
        write(iuecho,110) ((bfcs(i,is),afcs(i,is),is=1,mxshel),i=1,ngcs)
      end if
      if (impact.ge.1) then
        write(iuecho,450)
450     FORMAT(' $Echo write:ne')
        write(ip,100) ne
        write(iuecho,100) ne
        write(iuecho,460)
460     FORMAT(' $Echo write:neii')
        write(ip,100) neii
        write(iuecho,100) neii
        write(iuecho,470)
470     FORMAT(' $Echo write:bxeii,axeii')
        write(ip,110) bxeii,axeii
        write(iuecho,110) bxeii,axeii
        write(iuecho,480)
480     FORMAT(' $Echo write:((bfeii(i,ifun),afeii(i,ifun),ifun=1,ne),i=
     *1,neii)')
        write(ip,110) ((bfeii(i,ifun),afeii(i,ifun),ifun=1,ne),i=1,neii)
        write(iuecho,110) ((bfeii(i,ifun),afeii(i,ifun),ifun=1,ne),i=1,
     *  neii)
      end if
      return
      end

      subroutine MIX
      implicit none
      integer i, IZZ
      double precision AL183, ZAB, FZC, FCOUL, FCOULC, XSI, XSIF, ZZX,
     & ZZ, V3120
      include 'pegscommons/mimsd.f'
      include 'pegscommons/mixdat.f'
      include 'pegscommons/mxdatc.f'
      include 'pegscommons/molvar.f'
      include 'pegscommons/pmcons.f'
      include 'pegscommons/dercon.f'
      include 'pegscommons/radlen.f'
      include 'pegscommons/mscom.f'
      dimension XSI(20),ZZX(20),FZC(20),FCOUL(20),ZZ(20)
      write(  26,100)
100   format(/' In subroutine mix: '/)
      if (GASP.eq.0.0) then
        write(  26,110) NE,RHO
110     format(' Number of elements = ',I3,',  density=',1P,G15.6,
     *         ' (g/cm**3)')
      else
        write(  26,120) NE,RHO,GASP
120     format(' Number of elements = ',I3,',  density=',1P,G15.6,
     *    ' (g/cm**3) at ntp', '  gas pressure=',1P,G15.6,' atm.')
      end if
      write(  26,130) (i,Z(i),WA(i),PZ(i),RHOZ(i),i=1,NE)
130   format('   i       Z(i)           WA(i)          PZ(i)         RHO
     *Z(i) '/ ' Index   Periodic        Atomic       Proportion     Prop
     *ortion '/ '          number         weight        by number      b
     *y weight '// (I5,1P,4G15.6))
      if (GASP.ne.0.0) then
        RHO=GASP*RHO
      end if
      AL183 = DLOG(A183)
      TPZ=0.0
      WM=0.0
      ZC=0.0
      ZT=0.0
      ZB=0.0
      ZF=0.0
      ZS=0.0
      ZE=0.0
      ZX=0.0
      ZAB=0.0
      do i=1,NE
        TPZ = TPZ + PZ(i)
        WM = WM + PZ(i)*WA(i)
        ZC = ZC + PZ(i)*Z(i)
        FZC(i) =(FSC*Z(i))**2
        FCOUL(i) = FCOULC(Z(i))
        XSI(i) = XSIF (Z(i))
        ZZX(i) = PZ(i)*Z(i)*(Z(i)+XSI(i))
        if (Z(i).le.4.0) then
          IZZ=Z(i)
          ZAB=ZAB+ZZX(i)*ALRAD(IZZ)
        else
          ZAB=ZAB+ZZX(i)*(AL183+DLOG(Z(i)**(-1./3.)))
        end if
        ZT = ZT + ZZX(i)
        ZB = ZB + ZZX(i)*dlog(Z(i)**(-1./3.))
        ZF = ZF + ZZX(i)*FCOUL(i)
        ZZ(i) = PZ(i)*Z(i)*(Z(i)+fudgeMS)
        ZS = ZS + ZZ(i)
        ZE = ZE + ZZ(i)*((-2./3.)*DLOG(Z(i)))
        ZX = ZX + ZZ(i)*DLOG(1.d0+3.34*FZC(i))
      end do
      EZ = ZC/TPZ
      ZA = AL183*ZT
      ZG = ZB/ZT
      ZP = ZB/ZA
      ZV = (ZB-ZF)/ZT
      ZU = (ZB-ZF)/ZA
      EDEN=AN*RHO/WM*ZC
      RLC = 1./( (AN*RHO/WM)*4.0*FSC*R0**2*(ZAB-ZF) )
      write(  26,140) WM,ZC,ZT,ZA,ZB,ZAB,ZF,ZG,ZP,ZV,ZU,ZS,ZE,ZX,RLC,
     *(I,XSI(I),ZZX(I),FZC(I),FCOUL(I),ZZ(I),I=1,NE)
140   format(' Z variables--WM,ZC,ZT,ZA,ZB,ZAB'/1P,6E14.6/ ' ZF,ZG,ZP,ZV
     *,ZU,ZS'/1P,6E14.6/' ZE,ZX,RLC'/1P,3E14.6/ '0(I,XSI,ZZX,FZC,FCOUL,Z
     *Z,I=1,NE)'/ (I5,1P,5E14.6))
      V3120=EDEN
      write(  26,150) V3120
150   FORMAT(' Eden=',1P,G15.7)
      BLCC= A6680*RHO*ZS*DEXP(ZE/ZS)*RLC / (WM*DEXP(ZX/ZS))
      TEFF0 = ( DEXP(BMIN)/BMIN )/BLCC
      XCC= (A22P9/RADDEG) * DSQRT( ZS*RHO*RLC/WM )
      XR0 = XCC*DSQRT(TEFF0*BMIN)
      write(  26,160) BLCC,XCC,TEFF0,XR0
160   format(' BLCC,XCC,TEFF0,XR0=',1P,4E14.5)
      return
      end

      subroutine molier
      implicit none
      integer I, IS, J, L, JLR, ITOT, IP1, IDIF, IP2, N, IFLG, I01,
     & I02, ISWP, IDA, INC, IALL, II, IXTR, ISU, ISL, IUECHO, IPUN,
     & MST
      double precision BLCMIN, B, BLCA, BA, B1, PTOT, P, Q, PPP, PP
      include 'pegscommons/mimsd.f'
      dimension P(29,16),Q(29,16),IP1(29,16),IP2(29), IXTR(29,16),IALL(2
     *9),BLCA(16),BA(16)
      double precision TH(29),DTH(29),F0(29),F1(29),F2(29),BOLD,BLC
      data TH/.05,.2,.4,.6,.8,1.,1.2,1.4,1.6,1.8,2.,2.2,2.4,2.6,2.8, 3.,
     *3.2,3.4,3.6,3.8,4.07,4.5,5.,5.5,6.13,7.,8.,9.,9.75/
      data DTH/.1,19*0.2,0.35,3*0.5,0.75,3*1.0,0.5/
      data F0/2.,1.9216,1.7214,1.4094,1.0546,.7338,.4738,.2817 ,.1546,.0
     *783,.0366,.01581,.0063,.00232,7.9D-4,2.5D-4,7.3D-5, 1.9D-5,4.7D-6,
     *1.1D-6,2.3D-7,3.D-9,2.D-11,2.D-13,5.D-16,1.D-21, 3.D-28,1.D-35,1.1
     *8D-38/
      data F1/.8456,.7038,.3437,-0.0777,-0.3981,-0.5285,-0.4770, -.3183,
     *-.1396,-.0006,+0.0782,.1054,.1008,.08262,.06247,.0455, .03288,.024
     *02,.01791,.01366,.010638,.00614,.003831,.002527, .001739,.000908,.
     *0005211,.0003208,.0002084/
      data F2/2.4929,2.0694,1.0488,-.0044,-.6068,-.6359,-.3086,.0525 ,.2
     *423,.2386,.1316,.0196,-.0467,-.0649,-.0546,-.03568,-.01923, -.0084
     *7,-.00264,5.D-5,.0010741,.0012294,.0008326,.0005368, .0003495,.000
     *1584,7.83D-5,4.17D-5,2.37D-5/
      write(  26,100) (TH(i),DTH(i),F0(i),F1(i),F2(i),i=1,29)
100   format(' Bethe table used for input'/(1X,0P,2F10.2,1P,3E18.5))
      IS=1
        go to 120
110     IS=IS+1
120     if(IS-(MSTEPS-1).gt.0) go to 160
        J=FSTEP(IS)
          go to 140
130       J=J+1
140       IF(J-(FSTEP(IS+1)-1).gt.0) go to 150
          MSMAP(J)=IS
        go to 130
150     continue
      go to 110
160   continue
      MSMAP(JRMAX)=MSTEPS
      BLCMIN = BMIN - DLOG(BMIN)
      do 260 IS=1,MSTEPS
        BLC=BLCMIN+DLOG(FSTEP(IS))
        B=BLC+DLOG(BLC)
170     continue
          BOLD=B
          B=BOLD - (BOLD-DLOG(BOLD)-BLC)/(1.0-1.0/BOLD)
          if (dabs((B-BOLD)/BOLD) .lt. 1.D-5) go to 180
        go to 170 
180     continue
        BLCA(IS)=BLC
        BA(IS)=B
        FSQR(IS)=DSQRT(FSTEP(IS)*B/BMIN)
        B1=1.0/B
        PTOT=0.0
        do i=1,29
          P(i,IS)=TH(i)*DTH(i)*(F0(i)+B1*(F1(i)+B1*F2(i)))
          PTOT=PTOT+P(i,IS)
        end do
        do i=1,29
          P(i,IS)=P(i,IS)/PTOT
        end do
        do i=1,29
          Q(i,IS)=P(i,IS)
        end do
        i=29
190     continue
          l=1
200       if(Q(i,IS).ge.0.001.or.i.le.l) go to 210
            Q(i,IS)=Q(i,IS)+Q(i-l,IS)
            Q(i-l,IS)=0.0
            l=l+1
          go to 200
210       continue
          i=i-l
          if (i.le.0) go to 220
        go to 190
220     continue
        PPP=0.5
        PP=0.5
        do JLR=1,10
          ITOT=0
          do i=1,29
            IP1(i,IS)=Q(i,IS)*1000.0+PP
            ITOT=ITOT+IP1(i,IS)
          end do
          IDIF=ITOT-1000
          if (IDIF.eq.0) go to 260
          PPP=PPP*0.5
          if (IDIF.lt.0) then
            PP=PP+PPP
          else
            PP=PP-PPP
          end if
        end do
        do i=1,29
          IP2(i)=1
        end do
        n=29
230     continue
          n=n-1
          IFLG=0
          do j=1,n
            I01=IP2(j)
            I02=IP2(j+1)
            if (IP1(I01,IS).lt.IP1(I02,IS))  then
              ISWP=IP2(j)
              IP2(j)=IP2(j+1)
              IP2(j+1)=ISWP
              IFLG=1
            end if
          end do
          if (IFLG.eq.0) go to 240
        go to 230
240     continue
        write(  26,250) ITOT
250     FORMAT(' Rounding failed, itot has',I6,' entries')
        if (IDIF.lt.0) then
          IDA=-IDIF
          INC=1
        else
          IDA=IDIF
          INC=-1
        end if
        do i=1,IDA
          I01=IP2(i)
          IP1(I01,IS)=IP1(I01,IS)+INC
        end do
260   continue
      MXV1=0
      do i=1,29
        IALL(i)=IP1(i,1)
        do is=2,MSTEPS
          IALL(i)=MIN0(IALL(i),IP1(i,is))
        end do
        MXV1=MXV1+IALL(I)
      end do
      MXV2=1000-MXV1
      ii=0
      do i=1,29
        j=1
          go to 280
270       j=j+1
280       if(j-(IALL(i)).gt.0) go to 290
          ii=ii+1
          VERT1(ii)=TH(i)
        go to 270
290     continue
      end do
      do is=1,MSTEPS
        ii=0
        do i=1,29
          IXTR(i,is)=IP1(i,is)-IALL(i)
          j=1
            go to 310
300         j=j+1
310         if(j-(IXTR(i,is)).gt.0) go to 320
            ii=ii+1
            VERT2(ii,is)=TH(i)
          go to 300
320       continue
        end do
      end do
      write(  26,330) BMIN,MSTEPS,JRMAX,MXV1,MXV2
330   format(' BMIN,MSTEPS,JRMAX,MXV1,MXV2=', F11.5,4I8)
      ISU=0
340   continue
        ISL=ISU+1
        ISU=MIN0(ISL+9,MSTEPS)
        write(  26,350) ISL,ISU
350     format('  Data for steps ',I3,' to ',I3)
        write(  26,360) (IS,IS=ISL,ISU)
360     format(11X,'ISTEP',I6,9I11)
        write(  26,370) (FSTEP(IS),IS=ISL,ISU)
370     format(11X,'FSTEP',10F11.0)
        write(  26,380) (FSQR(IS),IS=ISL,ISU)
380     format(11X,'FSQR ',10F11.5)
        write(  26,390) (BLCA(IS),IS=ISL,ISU)
390     format(11X,'BLC  ',10F11.5)
        write(  26,400) (BA (IS),IS=ISL,ISU)
400     format(11X,'B    ',10F11.5)
        write(  26,410)
410     format('0I  TH IALL')
        do i=1,29
          if ((i.eq.11).or.(i.eq.23)) then
            write(  26,420)
420         format('1I  TH IALL')
          end if
          write(  26,430) i,TH(i),IALL(i),(P(i,IS),IS=ISL,ISU)
430       format(1X,I2,F5.2,I4,' PR ',10F11.8)
          write(  26,440) (Q(I,IS),IS=ISL,ISU)
440       format(11X,'  Q  ',10F11.8)
          write(  26,450) (IP1(I,IS),IS=ISL,ISU)
450       format(11X,' IP1 ',I7,9I11)
          write(  26,460) (IXTR(I,IS),IS=ISL,ISU)
460       format(11X,'EXTRA',I7,9I11)
        end do
        if (ISU.ge.MSTEPS) go to 470
      go to 340
470   continue
480   format(1X,14I5)
490   format(1X,14F5.2)
      iuecho=  26
      ipun=7
      write(iuecho,500)
500   format(' $echo write:')
      write(ipun,510)
      write(iuecho,510)
510   format(' Material independent multiple scattering data')
      write(iuecho,520)
520   format(' $echo write:JRMAX,MSTEPS,MXV1,MXV2')
      write(ipun,480) JRMAX,MSTEPS,MXV1,MXV2
      write(iuecho,480) JRMAX,MSTEPS,MXV1,MXV2
      write(iuecho,530)
530   format(' $echo write:(FSTEP(i),FSQR(i),i=1,MSTEPS)')
      write(ipun,540) (FSTEP(i),FSQR(i),i=1,MSTEPS)
      write(iuecho,540) (FSTEP(i),FSQR(i),i=1,MSTEPS)
540   format((1X,4(F5.0,F11.6)))
      write(iuecho,550)
550   format(' $echo write:(MSMAP(I),I=1,JRMAX)')
      write(ipun,480) (MSMAP(i),i=1,JRMAX)
      write(iuecho,480) (MSMAP(i),i=1,JRMAX)
      write(iuecho,560)
560   format(' $echo write:(VERT1(I),I=1,MXV1)')
      write(ipun,490) (VERT1(i),i=1,MXV1)
      write(iuecho,490) (VERT1(i),i=1,MXV1)
      do MST=1,MSTEPS
        write(iuecho,570) MST
570     format(' MST=',I5)
        write(iuecho,580)
580     format(' $echo write:(VERT2(I,MST),I=1,MXV2)')
        write(ipun,490) (VERT2(i,MST),i=1,MXV2)
        write(iuecho,490) (VERT2(i,MST),i=1,MXV2)
      end do
      return
      end

      double precision function pairdr(ka,e)
      implicit none
      integer LS
      double precision pairfr, e
      include 'pegscommons/lpairr.f'
      double precision ka
      K=ka
      if (K.lt.50.) then
        LE=1
        LS=0
      else
        LE=2
        LS=3
      end if
      LA=LS+1
      LC=LS+3
      PAIRDR=PAIRFR(e)
      return
      end

      double precision function pairdz(Z,KA,E)
      implicit none
      double precision KA, Z, xsif, dlog, fcoulc, pairfz, E
      include 'pegscommons/pmcons.f'
      include 'pegscommons/dercon.f'
      include 'pegscommons/molvar.f'
      include 'pegscommons/lpairz.f'
      k=ka
      DELC=136.*z**(-1./3.)*RM/K
      CONST=(AN*RHO/WM)*R0**2*FSC*z*(Z+XSIF(Z))*RLC/K**3
      XLNZ=4./3.*dlog(Z)
      if (K.ge.50) XLNZ=XLNZ+4.*FCOULC(Z)
      DELTAM=dexp((21.12-XLNZ)/4.184)-0.952
      PAIRDZ=PAIRFZ(E)
      return
      end

      double precision function pairfr(E)
      implicit none
      double precision EPS, E, DEL, DELTA, A, CC
      include 'pegscommons/bremp2.f'
      include 'pegscommons/dbrpr.f'
      include 'pegscommons/lpairr.f'
      EPS=E/K
      DEL=1./(K*EPS*(1.-EPS))
      if (DEL.gt.DELPOS(LE)) then
        pairfr=0.0
      else
        DELTA=DELCM*DEL
        if (DELTA.le.1.) then
          A=DL1(LA)+DELTA*(DL2(LA)+DELTA*DL3(LA))
          CC=DL1(LC)+DELTA*(DL2(LC)+DELTA*DL3(LC))
        else
          A=DL4(LA)+DL5(LA)*DLOG(DELTA+DL6(LA))
          CC=DL4(LC)+DL5(LC)*DLOG(DELTA+DL6(LC))
        end if
        pairfr=(ALFP1(LE)*CC+ALFP2(LE)*12.*(E/K-0.5)**2*A)/K
      end if
      return
      end

      double precision function pairfz(E)
      implicit none
      double precision EPS, E, ONEEPS, DELTA, SB1, SB2, DLOG, EPLUS
      include 'pegscommons/lpairz.f'
      EPS=E/K
      ONEEPS=1.-EPS
      if (ONEEPS.eq.0.0) then
        ONEEPS=1.18D-38
      end if
      DELTA=DELC/(EPS*ONEEPS)
      if (DELTA.ge.DELTAM) then
        pairfz=0.0
      else
        if (DELTA.le.1.) then
          SB1=20.867+DELTA*(-3.242+DELTA*0.625)-XLNZ
          SB2=20.209+DELTA*(-1.930+DELTA*(-0.086))-XLNZ
        else
          SB1=21.12-4.184*DLOG(DELTA+0.952)-XLNZ
          SB2=SB1
        end if
        EPLUS=K-E
        pairfz=CONST*((E**2+EPLUS**2)*SB1+0.666667*E*EPLUS*SB2 )
      end if
      return
      end

      double precision function pairrm(K,E1,E2)
      implicit none
      integer i
      double precision pairrz, E1, E2
      double precision K
      include 'pegscommons/mixdat.f'
      include 'pegscommons/mxdatc.f'
      pairrm=0.
      do i=1,NE
        pairrm=pairrm+PZ(i)*pairrz(Z(i),K,E1,E2)
      end do
      return
      end

      double precision function pairrr(K,E1,E2)
      implicit none
      double precision DUMMY, pairdr, E1, QD, E2
      double precision K
      external pairfr
      DUMMY=pairdr(K,E1)
      PAIRRR=QD(PAIRFR,E1,E2,'PAIRFR')
      return
      end

      double precision function pairrz(Z,K,E1,E2)
      implicit none
      double precision DUMMY, pairdz, Z, E1, QD, E2
      double precision K
      external pairfz
      DUMMY=pairdz(Z,K,E1)
      pairrz=QD(PAIRFZ,E1,E2,'PAIRFZ')
      return
      end

      double precision function pairte(K)
      implicit none
      integer i
      double precision pairtz
      double precision K
      include 'pegscommons/dercon.f'
      include 'pegscommons/mixdat.f'
      include 'pegscommons/mxdatc.f'
      pairte=0.0
      if (K.le.2.0*RM) return
      do i=1,NE
        pairte=pairte+PZ(i)*pairtz(Z(i),K)
      end do
      return
      end

      double precision function pairtm(K0)
      implicit none
      double precision pairrm
      double precision K0
      include 'pegscommons/dercon.f'
      if (K0.le.2.*RM) then
        pairtm=0.0
      else
        pairtm=pairrm(K0,RM,K0-RM)
      end if
      return
      end

      double precision function pairtr(K0)
      implicit none
      double precision pairrr
      double precision K0
      include 'pegscommons/dercon.f'
      if (K0.le.2.*RM) then
        pairtr=0.0
      else
        pairtr=pairrr(K0,RM,K0-RM)
      end if
      return
      end

      double precision function pairtu(K)
      implicit none
      double precision pairte, pairtm
      double precision K
      if (K.lt.50) then
        pairtu=pairte(K)
      else
        pairtu=pairtm(K)
      end if
      return
      end

      double precision function pairtz(Z,K)
      implicit none
      integer IZ
      double precision PCON, Z, AINTP
      double precision K
      include 'pegscommons/dercon.f'
      include 'pegscommons/phpair.f'
      include 'pegscommons/pmcons.f'
      include 'pegscommons/molvar.f'
      if (K.le.RMT2) then
        pairtz=0.0
      else
        PCON=1.D-24*(AN*RHO/WM)*RLC
        IZ=Z
        pairtz=PCON*AINTP(K,PRE,17,PRD(1,IZ),1,.TRUE.,.FALSE.)
      end if
      return
      end

      double precision function pbr1(E)
      implicit none
      double precision BREM, BREMTM, E, BHABTM, ANIHTM
      BREM=BREMTM(E)
      pbr1=BREM/(BREM+BHABTM(E)+ANIHTM(E))
      return
      end

      double precision function pbr2(E)
      implicit none
      double precision BRBH, BREMTM, E, BHABTM, ANIHTM
      BRBH=BREMTM(E)+BHABTM(E)
      pbr2=BRBH/(BRBH+ANIHTM(E))
      return
      end

      double precision function pdedx(E)
      implicit none
      double precision SPTOTP, E
      include 'pegscommons/thres2.f'
      pdedx=SPTOTP(E,AE,AP)
      return
      end

      subroutine pegs5
      implicit none
      integer NOPT, NPTS, IDF, IFUN, IV, ISUB, IN, IZ, I,
     & ISSBS, ICH, IOPT, IMIXT, I01, IZZ, ILOC, ITEMP,
     & J, ISHELL, IFUNT, NA, ID, NTIMES, NBINS, IQI, IRNFLG, IBIN
      integer ibounds,incohs,icprofs,irayls,impacts,iunrsts,
     & nleg0s,epstfls,iepsts,iaprims,iaprfls
      double precision gasps,efracHs,efracLs, fudgeMSs, ievs
      integer ib, medIdx
      
      DOUBLE PRECISION VLO, VHI, EI, AFACTS, CBARS, SKS, X0S, X1S,
     & ZTBL, EBIND, AX, BX, QD, SCSUM, ZSUM, CPSUM,
     & PZSUM, STEP, CAPIL, VALUE, FI, RNLO, RNHI, PINC, AVE
      include 'include/egs5_h.f'
      include 'include/egs5_media.f'
      include 'pegscommons/bremp2.f'
      include 'pegscommons/dbrpr.f'
      include 'pegscommons/dercon.f'
      include 'pegscommons/elemtb.f'
      include 'pegscommons/elmtbc.f'
      include 'pegscommons/funcs.f'
      include 'pegscommons/funcsc.f'
      include 'pegscommons/lspion.f'
      include 'pegscommons/mimsd.f'
      include 'pegscommons/mixdat.f'
      include 'pegscommons/mxdatc.f'
      include 'pegscommons/molvar.f'
      include 'pegscommons/phpair.f'
      include 'pegscommons/pmcons.f'
      include 'pegscommons/pwlfin.f'
      include 'pegscommons/cohcom.f'
      include 'pegscommons/rslts.f'
      include 'pegscommons/thres2.f'
      include 'pegscommons/epstar.f'
      include 'pegscommons/bcom.f'
      include 'pegscommons/cpcom.f'
      include 'pegscommons/eimpact.f'
      include 'pegscommons/sfcom.f'
      include 'pegscommons/mscom.f'
      include 'pegscommons/legacy.f'
      include 'pegscommons/dcsstr.f'
      double precision XP(4),WASAV(20)
      logical MEDSET,ENGSET
      character*4 OPTION(4,14),OPT(4),BLKW,NAME(6)
      character*4 NAMESB(12),IDFNAM(6)
      integer NH(200)
      external ALKE,ALKEI,CFUNS,EFUNS,GFUNS,RFUNS,ALIN,ALINI,AFFACT,SFUN
     *S, CFUNS2,CFUNS3,CFUNS4,CPRFIL,RFUNS2,EIIFUNS
      intrinsic DLOG,DEXP
      data OPTION/'E','L','E','M','M','I','X','T','C','O','M','P','E','N
     *','E','R','M','I','M','S','P','W','L','F', 'D','E','C','K','T','E'
     *,'S','T','D','B','U','G','C','A','L','L','P','L','T','N','S','T','
     *O','P','P','L','T','I', 'H','P','L','T'/
      data NOPT/15/,BLKW/' '/
      data MEDSET/.FALSE./,ENGSET/.FALSE./
      data NPTS/50/,IDF/6/
      namelist/INP/NE,PZ,RHO,RHOZ,WA,AE,UE,AP,UP, IFUN,XP,IV,VLO,VHI,IDF
     *,NPTS, EPE,ZTHRE,ZEPE,NIPE,NALE,EPG,ZTHRG,ZEPG,NIPG,NALG, EI,ISUB,
     *GASP,IUNRST,IRAYL,AFACT,SK,X0,X1,IEV,CBAR,ISSB,EPSTFL, IAPRIM,IBOU
     *ND,INCOH,ICPROF,IMPACT, efracH, efracL, fudgeMS, nleg0,
     * EPR,ZTHRR,ZEPR,NIPR,NALR,EPSF,ZTHRS,ZEPS,NIPS,NALS, EPCP,ZTHRC,ZE
     *PC,NIPC,NALC
      namelist/PWLFNM/EPE,ZTHRE,ZEPE,NIPE,NALE,EPG,ZTHRG,ZEPG,NIPG,NALG,
     * EPR,ZTHRR,ZEPR,NIPR,NALR,EPSF,ZTHRS,ZEPS,NIPS,NALS, EPCP,ZTHRC,ZE
     *PC,NIPC,NALC
      namelist/BCOMDT/BCOMP,PBC,NPBC
      namelist/ISCADT/SCATF,XSVAL
      namelist/SCPRDT/MXRAW,MXSHEL,ELECNI,NSHELL,CAPIN,SCPROF,QCAP
!  PEGS input data files
      open(UNIT=28,FILE='data/pgs5phtx.dat',STATUS='old')
      open(UNIT= 9,FILE='data/pgs5form.dat',STATUS='old')
      open(UNIT=22,FILE='data/aprime.data',STATUS='old')
!  PEGS material specific input data files (not always used)
      open(UNIT=20,FILE='epstar.dat',STATUS='unknown')
!  KEK LScat input data files
      open(UNIT=11,FILE='data/bcomp.dat',STATUS='old')
      open(UNIT=13,FILE='data/incoh.dat',STATUS='old')
!  PEGS problem input file
      open(UNIT=25,FILE='pgs5job.pegs5inp',STATUS='old')
!  PEGS output files
      open(UNIT=26,FILE='pgs5job.pegs5lst',STATUS='unknown')
      open(UNIT= 7,FILE='pgs5job.pegs5dat',STATUS='unknown')
      open(UNIT=10,FILE='pgs5job.pegs5err',STATUS='unknown')
      open(UNIT=21,FILE='pgs5job.pegs5plot',STATUS='unknown')
!  KEK LScat material specific input data files (not always used)
      open(UNIT=15,FILE='scp.dat',STATUS='unknown')
      open(UNIT=18,FILE='iff.dat',STATUS='unknown')
      open(UNIT=19,FILE='ics.dat',STATUS='unknown')
!  New EGS transport mechanics data files
      open(UNIT=17,FILE='pgs5job.msfit',STATUS='unknown')
      write(  26,100)
100   FORMAT('1',20X,'Pegs5 listing file'/ 20X,'(with nrcc modifications
     *, Jan 13,1988)')
      call pmdcon
      write(  26,110)
110   FORMAT(/' This version reads units 8 and 9 in free format'/)
      read(  28,*) NPHE, ((PHE(in,iz),in=1,61),iz=1,100), ((PHD(in,iz),i
     *n=1,61),iz=1,100), EKEDGE, PRE, ((PRD(in,iz),in=1,17),iz=1,100), (
     *(COHE(in,iz),in=1,61),iz=1,100)
      read(9,*) XVAL, ((AFAC(in,iz),in=1,100),iz=1,100)
      do i=1,100
        XVAL(i)=XVAL(i)**2.
      end do
      ISSBS=ISSB
      AFACTS=AFACT
      CBARS=CBAR
      SKS=SK
      X0S=X0
      X1S=X1
      IEVS=IEV
      ibounds=IBOUND
      incohs=INCOH
      icprofs=ICPROF
      gasps=GASP
      irayls=IRAYL 
      impacts=IMPACT
      iunrsts=IUNRST
      fudgeMSs=fudgeMS
      efracHs=efracH
      efracLs=efracL
      nleg0s=nleg0
      epstfls=EPSTFL
      iepsts=IEPST
      iaprims=IAPRIM
      iaprfls=IAPRFL
1040  continue
      do i=1,20
        WASAV(i)=WA(i)
        WA(i)=0.
      end do
      RHOSAV=RHO
      RHO=0.0
      read(  25,130,end=1060) OPT
130   FORMAT(4A1)
      write(  26,140) OPT
140   FORMAT(//,1X,60('*'),/,' *',T61,'*',/,' *  OPT = ',4A1,T61,'*',/,
     *' *',T61,'*',/,1X,60('*'),//)
      read(  25,INP,end=8990)
      if (RHO.eq.0) then
        do ICH=1,4
          if (OPT(ICH).ne.OPTION(ICH,1)) then
            RHO=RHOSAV
            go to 150
          end if
        end do
150     continue
      end if
      do 160 IOPT=1,NOPT
        do ICH=1,4
          if (OPT(ICH).ne.OPTION(ICH,IOPT)) go to 160
        end do
        go to 1120
160   continue
      write(  26,1130)
1130  format(' Option not found, job aborted.')
      close(26)
      stop 16
1120  if (IOPT.gt.3) then
        RHO=RHOSAV
        do i=1,20
          WA(i)=WASAV(i)
        end do
      else
        do i=1,4
          MTYP(i)=OPT(i)
        end do
      end if
      go to(1160,1170,1180,1190,1200,1210,1220,1230, 1240,1250,1260,1070
     *,1270,1280),IOPT
! ***********>  OPT = ELEM  <**************
1160  NE=1
      PZ(1)=1
      IMIXT=0
      go to 1290
! ***********>  OPT = MIXT  <**************
1170  if (NE.le.1) then
        write(  26,1300) NE
1300    format(//,' NE=',I6,' is improperly defined for a mixture.')
        close(26)
        stop
      end if
      IMIXT=1
      go to 1290
! ***********>  OPT = COMP  <**************
1180  if (NE.le.1) then
        write(  26,1310) NE
1310    format(//,' NE=',I6,' is improperly defined for a compound.')
        close(26)
        stop
      end if
      IMIXT=0
1290  read(  25,1320) MEDIUM,IDSTRN
1320  FORMAT(24A1,6X,24A1)
      read(  25,1330) (ASYM(i),i=1,NE)
1330  FORMAT(24(A2,1X))
      if (IDSTRN(1).eq.BLKW) then
        do i=1,LMED
          IDSTRN(i)=MEDIUM(i)
        end do
      end if
      write(  26,1350) MEDIUM,IDSTRN,(ASYM(i),i=1,NE)
1350  format(1X,60('-')/' Medium=',24A1,',Sternheimer ID=',24A1,/1X,60('
     *-')// ,' Atomic symbols are: ',(1X,24(A2,1X) ))

!  see if this medium is being used in the current problem.
!  this needs to be done so we can later affirm the integrity
!  of the pegs data file in case it is reused.

      do 16 medIdx = 1, nmed
        do ib = 1, 24
          if(MEDIUM(ib) .ne. media(ib,medIdx)) go to 16
          if(ib .eq. 24) go to 17
        end do
16    continue
17    oldK1run = .true.
      if(medIDx.le.nmed) then
        if(charD(medIdx).ne.0.d0) then
          oldK1run = .false.
        endif
      endif

      if (IUNRST.eq.1) then
        write(  26,1360)
1360    format(/T10,'***Calculates unrestricted collision', ' stopping p
     *ower***  IUNRST=1'//)
      else if (IUNRST.eq.2) then
        write(  26,1370)
1370    format(/T10,'****Data set for a csda calculation', '******  IUNR
     *ST=2'//)
      else if (IUNRST.eq.3) then
        write(  26,1380)
1380    format(/T10,'****Data set for a csda calculation', ' but with BR
     *EM events******  IUNRST = 3'//)
      else if (IUNRST.eq.4) then
        write(  26,1390)
1390    format(/T10,'****Data set for a calculation', 'with DELTAS DISCR
     *ETE,BREM CSDA******  IUNRST = 4'//)
      else if (IUNRST.eq.5) then
        write(  26,1400)
1400    format(/T10,'****Calculates unrestricted radiative', ' stopping
     *power***** IUNRST = 5')
      else if (IUNRST.eq.6) then
        write(  26,1410)
1410    format(/T10,'****Calculates restricted radiative', ' stopping po
     *wer***** IUNRST = 6')
      else if (IUNRST.eq.7) then
        write(  26,1420)
1420    format(/T10,'****Calculates restricted collision', ' stopping po
     *wer***** IUNRST = 7')
      end if
      do i=1,NE
        Z(i)=ZTBL(ASYM(i))
        if (Z(i).eq.0.0) then
          write(  26,1440)
1440      format(' Bad atomic symbol....job aborted.')
          close(26)
          stop 16
        end if
        if (WA(I).eq.0.) then
          I01=Z(i)
          WA(i)=WATBL(I01)
        end if
        if (IMIXT.ne.0) then
          PZ(i)=RHOZ(i)/WA(I)
        else
          RHOZ(i)=PZ(i)*WA(i)
        end if
      end do
      if (NE.eq.1.and.RHO.eq.0.) then
        I01=Z(1)
        RHO=RHOTBL(I01)
      end if
      call MIX
      call SPINIT
      call DIFFER
      ! Read in the DCS and get the scattering power
      call elinit(z,pz,ne)
      MEDSET=.true.
      read(11,BCOMDT)
      rewind 11
      read(  13,ISCADT)
      rewind   13
      if (ICPROF.eq.3.or.ICPROF.eq.4) then
        write(  26,1450) ICPROF
1450    format(' Reading shellwise Compton profile as ICPROF=',I3)
        read(15,SCPRDT,ERR=8991)
      end if
      if (ICPROF.eq.-3.or.ICPROF.eq.-4) then
        write(  26,1460) ICPROF
1460    format(' Reading shellwise Compton profile as ICPROF=',I3)
        MXRAW=0
        MXSHEL=0
        call RDSCPR
        open(UNIT=31,FILE='pgs5job.ssl',STATUS='old')
        read(31,SCPRDT)
        close(31)
        ICPROF=ABS(ICPROF)
      end if
      if (IRAYL.eq.2) then
        do i=1,100
          read(18,*,ERR=8992) XVALFAC(i),BFAC2(i)
          write(  26,1480) XVALFAC(i),BFAC2(i)
1480      format(2G10.5)
        end do
        write(  26,1490)
1490    format(/'Reading interference form factors for use with IRAYL=2'
     *  ,/)
        do i=1,61
          read(19,*,ERR=8993) XVALINT(i),COHEINT(i,1)
          write(  26,1510) XVALINT(i),COHEINT(i,1)
1510      format(2G10.5)
        end do
        write(  26,1520)
1520    format(/'Reading interference coherent cross sections for use 
     *  with IRAYL=2',/)
      end if
      write(  26,1530)
1530  format(//,' End of elem, mixt, or comp option',///)
      go to 1040
! ***********>  OPT = ENER  <**************
1190  if (AE.lt.0) AE=-AE*RM
      if (UE.lt.0) UE=-UE*RM
      if (AP.lt.0) AP=-AP*RM
      if (UP.lt.0) UP=-UP*RM
      TE=AE-RM
      TET2=TE*2.0
      TEM=TE/RM
      THBREM=RM+AP
      THMOLL=AE+TE
      write(  26,1540) AE,UE,AP,UP,TE,TET2,TEM,THBREM,THMOLL
1540  format(' AE,UE,AP,UP,TE,TET2,TEM,THBREM,THMOLL'/1X,1P,5E15.7/1X,1P
     *,4E15.7)
!     can now initialize energy grids for computing scattering strength
      call inigrd(ae,ue,1)
      ENGSET=.true.
!     get estepe limits
      call esteplim
      go to 1040
! ***********>  OPT = MIMS  <**************
! this is no longer supported
1200  call MOLIER
      go to 1040
! ***********>  OPT = PWLF  <**************
1210  if (MEDSET.and.ENGSET) go to 1550
      write(  26,1560) MEDSET,ENGSET
1560  format(' MEDSET,ENGSET=',2L2,',PWLF req. ignored.')
      close(26)
      stop 16
1550  EBINDA=EBIND(AP)
      write(  26,PWLFNM)
      write(  26,1570) EBINDA
1570  format(' Average K-ionization energy=',F10.6,'(MeV)')

      if(efracH .gt. 0.5d0) then
        efracH = .5d0
        write(6,1541) efracH
        write(26,1541) efracH
      else if(efracH .le. 1.d-6) then
        efracH = 5.d-2
        write(6,1542) efracH
        write(26,1542) efracH
      endif
      if(efracL .gt. 0.5d0) then
        efracL = .5d0
        write(6,1543) efracL
        write(26,1543) efracL
      else if(efracL .le. 1.d-6) then
        efracL = 5.d-2
        write(6,1544) efracL
        write(26,1544) efracL
      endif
1541  format('Warning:  EFRAC_HIGH > MAX.  setting to ',F6.4)
1542  format('Warning:  EFRAC_HIGH < MIN.  setting to ',F6.4)
1543  format('Warning:  EFRAC_LOW > MAX.  setting to ',F6.4)
1544  format('Warning:  EFRAC_LOW < MIN.  setting to ',F6.4)
      if(oldK1run) call makek1(ae,ue)

      write(  26,1580)
1580  format(' Do PWLF to electron data sets.'/)
      call PWLF1(NEL,NALE,AE,UE,THMOLL,EPE,ZTHRE,ZEPE,NIPE,ALKE,ALKEI, A
     *XE,BXE,150,15,AFE,BFE,EFUNS)
      if (IMPACT.ge.1) then
        write(  26,1590) IMPACT
1590    format(' Do pwlf to EII/MOLLER. IMPACT=',I3/)
        call PWLF1(NEII,NALE,AE,UE,THMOLL,EPE,ZTHRE,ZEPE,NIPE,ALKE,ALKEI
     *  , AXEII,BXEII,150,20,AFEII,BFEII,EIIFUNS)
      end if
      write(  26,1600)
1600  format(' Do pwlf to photon data sets.'/)
      if (IBOUND.eq.1) then
        write(  26,1610)
1610    format(/' Total Compton cross section of bound electron in'/ ' S
     *torm-Israel is used'/)
      end if
      call PWLF1(NGL,NALG,AP,UP,RMT2,EPG,ZTHRG,ZEPG,NIPG,DLOG,DEXP,AXG,B
     *XG,1000,4,AFG,BFG,GFUNS)
      if (IRAYL.eq.1) then
        write(  26,1620)
1620    format(//,' ***** IRAYL=1: Rayleigh data included *****',//)
        write(  26,1630)
1630    format(' Do PWLF to Rayleigh distribution.'/)
        do i=1,100
          AFAC2(i)=0.0
          do in=1,NE
            IZZ=Z(in)
            AFAC2(i)=AFAC2(i)+PZ(in)*AFAC(i,IZZ)**2
          end do
        end do
        AFFI(1)=0.0
        do i=2,97
          AX=XVAL(i-1)
          BX=XVAL(i)
          AFFI(i)=QD(AFFACT,AX,BX,'AFFACT')
        end do
        do i=2,97
          AFFI(i)=AFFI(i)+AFFI(i-1)
        end do
        do i=1,97
          AFFI(i)=AFFI(i)/AFFI(97)
        end do
        call PWLF1(NGR,NALR,0.D0,1.D0,0.D0,EPR,ZTHRR,ZEPR,NIPR,ALIN,
     *  ALINI,AXR,BXR,100,1,AFR,BFR,RFUNS)
      end if
      if (IRAYL.eq.2) then
        write(  26,1690)
1690    format(//,' IRAYL=2: Rayleigh data with interference effects inc
     *luded.',//)
        write(  26,1700)
1700    format(' Do PWLF to Rayleigh distribution.'/)
        do i=1,100
          AFAC2(i)=(BFAC2(i)*DSQRT(WM))**2
        end do
        do i=1,100
          XVAL(i)=XVALFAC(i)**2
        end do
        AFFI(1)=0.0
        do i=2,97
          AX=XVAL(i-1)
          BX=XVAL(i)
          AFFI(i)=QD(AFFACT,AX,BX,'AFFACT')
        end do
        do i=2,97
          AFFI(i)=AFFI(i)+AFFI(i-1)
        end do
        do i=1,97
          AFFI(i)=AFFI(i)/AFFI(97)
        end do
        call PWLF1(NGR,NALR,0.D0,1.D0,0.D0,EPR,ZTHRR,ZEPR,NIPR,ALIN,
     *  ALINI,AXR,BXR,100,1,AFR,BFR,RFUNS)
      end if
      if (IRAYL.eq.3) then
        write(  26,1760)
1760    format(//,' ***** IRAYL=3: Rayleigh data included *****',//)
        write(  26,1770)
1770    format(' Do PWLF to Rayleigh distribution. X-F2(X,Z) tabulation
     *'/)
        do i=1,100
          AFAC2(i)=0.0
          do IN=1,NE
            IZZ=Z(IN)
            AFAC2(I)=AFAC2(I)+PZ(IN)*AFAC(I,IZZ)**2
          end do
        end do
        call PWLF1(NGR,NALR,1.0D-3,1.0D2,1.0D-3,EPR,ZTHRR,ZEPR,NIPR,DLOG
     *  ,DEXP, AXR,BXR,100,1,AFR,BFR,RFUNS2)
      end if
      if (INCOH.eq.1) then
        write(  26,1800)
1800    format(//,' ***** INCOH=1: S(X,Z) data included *****',//)
        write(  26,1810)
1810    format(' Do PWLF to S(X,Z)'/)
        do i=1,45
          SCSUM=0.0
          ZSUM=0.0
          do in=1,NE
            IZZ=Z(in)
            SCSUM=SCSUM+PZ(in)*SCATF(i,IZZ)
            ZSUM=ZSUM+PZ(in)*Z(in)
          end do
          SCATZ(i)=SCSUM/ZSUM
        end do
        call PWLF1(NGS,NALS,5.0D-3,80.D0,80.D0,EPSF,ZTHRS,ZEPS,NIPS,
     *  DLOG,DEXP,AXS,BXS,100,1,AFS,BFS,SFUNS)
      end if
!  Trap to stop ICPROF of 1 or 2, since EGS5 does not process this data
!  properly.  The PEGS code for these options is left in place in case 
!  they are to be resurrected later.
      if(ICPROF.eq.1) then
        write(  26,1830) ICPROF
1830    format(//,' ***** ICPROF=',I2,':T-Compton profile data not ',
     *'currently supported.  Setting ICPROF=0',//)
        ICPROF=0
      else if(ICPROF.eq.2) then
        write(  26,1830) ICPROF
        ICPROF=0
      endif
      if (ICPROF.eq.1.or.ICPROF.eq.2) then
        CPIMEV=IEV*1.0D-6
        write(  26,1840) CPIMEV
1840    format(' CPIMEV=',E12.5)
        write(  26,1850) ICPROF
1850    format(//,' ***** ICPROF=',I2,':T-Compton profile data included
     ******',//)
        if (ICPROF.eq.1) then
          write(  26,1860)
1860      format(' Do PWLF to Q vs int J(Q)'/)
        end if
        if (ICPROF.eq.2) then
          write(  26,1870)
1870      format(' Do PWLF to J(Q)'/)
        end if
        do i=1,31
          CPSUM=0.0
          PZSUM=0.0
          do in=1,NE
            IZZ=Z(in)
            CPSUM=CPSUM+PZ(in)*CPROF(i,IZZ)
            PZSUM=PZSUM+PZ(in)
          end do
          AVCPRF(i)=CPSUM/PZSUM
          write(  26,1900) QCAP(I),AVCPRF(I)
1900      format(' QCAP= ',1P,E9.2,' AVCPRF= ',1P,E9.2)
        end do
        if (ICPROF.eq.2) then
          call PWLF1(NGC,NALC,1.D-2,1.D2,1.D2,EPCP,ZTHRC,ZEPC,NIPC,DLOG
     *    ,DEXP, AXC,BXC,2000,1,AFC,BFC,CFUNS)
        end if
        if (ICPROF.eq.1) then
          CPROFI(1)=0.0
          QCAP10(1)=0.0
          do i=2,301
            ILOC=(i-2)/10+1
            STEP=(QCAP(ILOC+1)-QCAP(ILOC))*0.1
            AX=QCAP(ILOC)+STEP*FLOAT(i-2-(ILOC-1)*10)
            BX=AX+STEP
            QCAP10(i)=BX
            CPROFI(i)=QD(CPRFIL,AX,BX,'CPRFIL')
            write(  26,1920) i,CPROFI(i)
1920        format(1X,I5,'-th interval CPROFI=',1P,E12.5)
          end do
          write(  26,1930)
1930      format(' Sum of integration of CPROF')
          do i=2,301
            CPROFI(i)=CPROFI(i)+CPROFI(i-1)
            write(  26,1950)I,CPROFI(i)
1950        format(1X,I5,'-th interval. Sum CPROFI=',1P,E12.5)
          end do
          write(  26,1960)
1960      format(' Normalized sum of integration of CPROF')
          do i=1,301
            CPROFI(i)=CPROFI(i)/CPROFI(301)
            write(  26,1980) i,CPROFI(i)
1980        format(1X,I5,'-th interval. NORM.SUM CPROFI=',1P,E12.5)
          end do
          call PWLF1(NGC,NALC,0.D0,1.D0,0.D0,EPCP,ZTHRC,ZEPC,NIPC,ALIN,
     *    ALINI, AXC,BXC,2000,1,AFC,BFC,CFUNS2)
        end if
        write(  26,1990) NGC
1990    format(' Compton profile was PWLF-ED, NGC= ',I5)
      end if
      if (ICPROF.eq.3.OR.ICPROF.eq.4) then
        write(  26,2000) ICPROF
2000    format(//,' ***** ICPROF=',I2,': S-Compton profile data included
     * *****',//)
        if (ICPROF.eq.3) then
          write(  26,2010)
2010      format(' Do pwlf to Q vs int J(Q)'/)
        end if
        if (ICPROF.eq.4) then
          write(  26,2020)
2020      format(' Do PWLF to J(Q)'/)
        end if
        do i=1,MXSHEL
          ELECNO(i)=0.
          ELECNJ(i)=0.
        end do
        do i=1,MXRAW
          ITEMP=NSHELL(i)
          ELECNO(ITEMP)=ELECNO(ITEMP)+ELECNI(i)
        end do
        do i=1,MXSHEL
          write(  26,2060) i,ELECNO(i)
2060      format(' Shell ,ELECNO=',I5,E12.5)
        end do
        do i=1,MXSHEL
          CAPIO(i)=0.0
          CAPILS(i)=0.0
        end do
        do i=1,MXRAW
          if (CAPIN(i).gt.0.0) then
            CAPIL=DLOG(CAPIN(i))
            J=NSHELL(i)
            CAPILS(J)=CAPILS(J)+CAPIL*ELECNI(i)
            ELECNJ(J)=ELECNJ(J)+ELECNI(i)
          end if
        end do
        do i=1,MXSHEL
          if (ELECNJ(i).gt.0.) then
            CAPIO(i)=DEXP(CAPILS(i)/ELECNJ(i))
          end if
        end do
        do i=1,MXRAW
          write(  26,2110) i,CAPIN(i)
2110      format('i,CAPIN(i)= ',I5,E12.5)
        end do
        do i=1,MXSHEL
          write(  26,2130) i,CAPIO(i)
2130      format(' i,CAPIO(i)= ',I5,E12.5)
        end do
        do i=1,31
          do ishell=1,MXSHEL
            SCPSUM(i,ishell)=0.0
          end do
          do in=1,MXRAW
            ISHELL=NSHELL(in)
            SCPSUM(i,ishell)=SCPSUM(i,ishell)+SCPROF(i,in)*ELECNI(in)
          end do
        end do
        do ishell=1,MXSHEL
          write(  26,2180) ishell
2180      format(' ',I5,'-th shell. SCPSUM')
          write(  26,2190) (SCPSUM(i,ishell),I=1,31)
2190      format(' ',1P,7E9.2)
        end do
        do i=2,MXSHEL
          ELECNO(i)=ELECNO(i)+ELECNO(i-1)
        end do
        do i=1,MXSHEL
          ELECNO(i)=ELECNO(i)/ELECNO(MXSHEL)
        end do
        do i=1,MXSHEL
          write(  26,2230) i,ELECNO(i)
2230      format(' Shell ,ELECNO=',I5,E12.5)
        end do
        if (ICPROF.eq.4) then
          call PWLF1(NGCS,NALC,1.D-2,1.D2,1.D2,EPCP,ZTHRC,ZEPC,NIPC,DLOG
     *    ,DEXP, AXCS,BXCS,2000,MXSHEL,AFCS,BFCS,CFUNS4)
        end if
        if (ICPROF.eq.3) then
          do ishell=1,MXSHEL
            CPROFI(1)=0.0
            QCAP10(1)=0.0
            write(  26,2250) ishell
2250        format(' ICPROF=3,ISHELL=',I5)
            do i=1,31
              AVCPRF(i)=SCPSUM(i,ishell)
            end do
            write(  26,2270)
2270        format(' AVCPROF')
            write(  26,2280) (AVCPRF(i),i=1,31)
2280        FORMAT(' ',7E12.5)
            do i=2,301
              ILOC=(i-2)/10+1
              STEP=(QCAP(ILOC+1)-QCAP(ILOC))*0.1
              AX=QCAP(ILOC)+STEP*FLOAT(I-2-(ILOC-1)*10)
              BX=AX+STEP
              QCAP10(i)=BX
              CPROFI(i)=QD(CPRFIL,AX,BX,'CPRFIL')
            end do
            do i=2,301
              CPROFI(i)=CPROFI(i)+CPROFI(i-1)
            end do
            do i=1,301
              SCPROI(i,ishell)=CPROFI(i)/CPROFI(301)
            end do
          end do
          call PWLF1(NGCS,NALC,0.D0,1.D0,0.D0,EPCP,ZTHRC,ZEPC,NIPC,ALIN,
     *    ALINI, AXCS,BXCS,2000,MXSHEL,AFCS,BFCS,CFUNS3)
        end if
        write(  26,2320) NGCS,NALC
2320    format(' S-Compton profile was PWLF-ED, NGCS= ',I5,' NALC=',I5)
      end if
      go to 1040
! ***********>  OPT = DECK  <**************
1220  call LAY
      ISSB=ISSBS
      AFACT=AFACTS
      CBAR=CBARS
      SK=SKS
      X0=X0S
      X1=X1S
      IEV=IEVS
      write(  26,2330) ISSB,AFACT,CBAR,SK,X0,X1,IEV
2330  format(' After DECK. ISSB,AFACT,CBAR,SK,X0,X1,IEV=',I5,6E11.4)
      IBOUND=ibounds
      INCOH=incohs
      ICPROF=icprofs
      GASP=gasps
      IRAYL=irayls
      IMPACT=impacts
      IUNRST=iunrsts
      fudgeMS=fudgeMSs
      efracH=efracHs
      efracL=efracLs
      nleg0=nleg0s
      EPSTFL=epstfls
      IEPST=iepsts
      IAPRIM=iaprims
      IAPRFL=iaprfls
      go to 1040
! ***********>  OPT = TEST  <**************
1230  continue
      call PLOT(49,XP,1,AE,UE,NPTS,9)
      call PLOT(71,XP,1,AE,UE,NPTS,9)
      call PLOT(47,XP,1,AE,UE,NPTS,9)
      call PLOT(68,XP,1,AE,UE,NPTS,9)
      call PLOT(46,XP,1,AE,UE,NPTS,9)
      call PLOT(66,XP,1,AE,UE,NPTS,9)
      call PLOT(67,XP,1,AE,UE,NPTS,9)
      call PLOT(77,XP,1,AE,UE,NPTS,9)
      call PLOT(78,XP,1,AE,UE,NPTS,9)
      call PLOT(53,XP,1,AP,UP,NPTS,6)
      call PLOT(51,XP,1,AP,UP,NPTS,6)
      call PLOT(52,XP,1,AP,UP,NPTS,6)
      call PLOT(44,XP,1,AP,UP,NPTS,6)
      write(  26,2340)
2340  format('1')
      go to 1040
! ***********>  OPT = DBUG  <**************
1240  continue
      go to 1040
! ***********>  OPT = CALL  <**************
1250  read(  25,2350) NAME
2350  format(6A1)
      IFUN=IFUNT(NAME)
      if (IFUN.le.0) go to 1040
      VALUE=FI(IFUN,XP(1),XP(2),XP(3),XP(4))
      NA=NFARG(IFUN)
      write(  26,2360) VALUE,(FNAME(i,IFUN),i=1,6),(XP(i),i=1,NA)
2360  format(' Function call: ',1P,G15.6,' = ',6A1,' OF ',4G15.6)
      go to 1040
! ***********>  OPT = PLTN  <**************
1260  read(  25,2370) NAME,IDFNAM
2370  format(12A1)
      IFUN=IFUNT(NAME)
      if (IFUN.le.0) go to 1040
      ID=IFUNT(IDFNAM)
      if (ID.lt.0) go to 1040
      if (ID.ne.0) IDF=ID
! ***********>  OPT = PLTI  <**************
1270  call PLOT(IFUN,XP,IV,VLO,VHI,NPTS,IDF)
      go to 1040
! ***********>  OPT = HPLT  <**************
1280  read(  25,2380) NAMESB,NTIMES,NBINS,IQI,RNLO,RNHI,IRNFLG,
     * (NH(IBIN),IBIN=1,NBINS)
2380  format(' Test data for routine=',12A1,',#SAMPLES=',I10,',NBINS=',I
     *5 /' IQI=',I2,',RNLO,RNHI=',2F12.8,',IRNFLG=',I2/(9I8))
      write(  26,2380) NAMESB,NTIMES,NBINS,IQI,RNLO,RNHI,IRNFLG,
     * (NH(IBIN),IBIN=1,NBINS)
      write(  26,2390) EI,ISUB
2390  FORMAT(' EI=',F14.3,',ISUB=',I3)
      go to (2400,2410,2420,2430,2440,2450),ISUB
2400  call HPLT1(EI,RM,EI-RM,NAMESB,NTIMES,NBINS,NH, 1,54,59,63 )
      go to 1040
2410  call HPLT1(EI,EI/(1.0+2.0*EI/RM),EI,NAMESB,NTIMES,NBINS,NH, 6,40,4
     *2,43 )
      go to 1040
2420  call HPLT1(EI,AP,EI-RM,NAMESB,NTIMES,NBINS,NH, 6,24,30,34 )
      go to 1040
2430  call HPLT1(EI,AE,RM+(EI-RM)*0.5,NAMESB,NTIMES,NBINS,NH, 3,11,13,14
     * )
      go to 1040
2440  call HPLT1(EI,AE,EI,NAMESB,NTIMES,NBINS,NH, 3,20,22,23 )
      go to 1040
2450  PINC=DSQRT(EI**2-RM**2)
      AVE=EI+RM
      call HPLT1(EI,AVE*RM/(AVE+PINC),AVE*0.5,NAMESB,NTIMES,NBINS,NH, 6,
     *15,17,18 )
      go to 1040

! ***********>  standard PEGS termination  <**************
! Print the new mscat options parameters.  GS distribution computation 
! moved to egs so that we can use it with chard method.
1060  call prelastino

! ***********>  OPT = STOP or standard PEGS termination  <**************
1070  write(  26,2470)
2470  format(///' End of file read - exit from pegs5'/'1')
      close(UNIT=25)
      close(UNIT=26)
      close(UNIT=7)
      close(UNIT=28)
      close(UNIT=9)
      close(UNIT=10)
      close(UNIT=11)
      close(UNIT=13)
      close(UNIT=14)
      close(UNIT=15)
      close(UNIT=17)
      close(UNIT=18)
      close(UNIT=19)
      close(UNIT=21)
      close(UNIT=22)
      return

! abnormal termination because of end-of-file on an expected
! user-supplied input data file

8990  write(26,9990)
9990  format(' Stopped in pegs5 because namelist/INP/',
     *' data was missing.')
      go to 9995
8991  write(26,9991)
9991  format(//,'EOF on user supplied Compton profile data - stopping.')
      go to 9995
8992  write(26,9992)
9992  format(//,'EOF on user supplied interference cs data - stopping.')
      go to 9995
8993  write(26,9993)
9993  format(//,'EOF on user supplied interference ff data - stopping.')

9995  close(26)
      stop

      end

      block data PEGSDAT
!     Use Revised Sternheimer Density Effects Coeeficients
!     Atomic Data Nuclear Data Tables 30, 261(1984) by 
!     R. M. Sternheimer et al. 
!     Reference KEK Internal 95-17 (1995)
      double precision STDAT1, STDAT2, STDAT3, STDAT4, STDAT5,
     *                 STDAT6, STDAT7, STDAT8, STDAT9, STDA10,
     *                 STDA11, STDA12, STDA13, STDA14
      include 'include/egs5_h.f'
      include 'pegscommons/bremp2.f'
      include 'pegscommons/dbrpr.f'
      include 'pegscommons/dercon.f'
      include 'pegscommons/elemtb.f'
      include 'pegscommons/elmtbc.f'
      include 'pegscommons/funcs.f'
      include 'pegscommons/funcsc.f'
      include 'pegscommons/lspion.f'
      include 'pegscommons/mimsd.f'
      include 'pegscommons/mixdat.f'
      include 'pegscommons/mxdatc.f'
      include 'pegscommons/molvar.f'
      include 'pegscommons/phpair.f'
      include 'pegscommons/pmcons.f'
      include 'pegscommons/pwlfin.f'
      include 'pegscommons/radlen.f'
      include 'pegscommons/cohcom.f'
      include 'pegscommons/spcomm.f'
      include 'pegscommons/spcomc.f'
      include 'pegscommons/rslts.f'
      include 'pegscommons/thres2.f'
      include 'pegscommons/epstar.f'
      include 'pegscommons/eimpact.f'
      include 'pegscommons/bcom.f'
      include 'pegscommons/cpcom.f'
      include 'pegscommons/sfcom.f'
      include 'pegscommons/mscom.f'
      include 'pegscommons/dcsstr.f'
      character*4 MEDTB1(24,20),MEDTB2(24,20),MEDTB3(24,20),
     *            MEDTB4(24,20),MEDTB5(24,20),MEDTB6(24,20),
     *            MEDTB7(24,10),MEDTB8(24,10),MEDTB9(24,10),
     *            MEDT10(24,10),MEDT11(24,10),MEDT12(24,10),
     *            MEDT13(24,10),MEDT14(24,10),MEDT15(24,10),
     *            MEDT16(24,10),MEDT17(24,10),MEDT18(24,10),
     *            MEDT19(24,10),MEDT20(24,10),MEDT21(24,10),
     *            MEDT22(24,8)
      equivalence (MEDTBL(1,1),MEDTB1(1,1))
      equivalence (MEDTBL(1,21),MEDTB2(1,1))
      equivalence (MEDTBL(1,41),MEDTB3(1,1))
      equivalence (MEDTBL(1,61),MEDTB4(1,1))
      equivalence (MEDTBL(1,81),MEDTB5(1,1))
      equivalence (MEDTBL(1,101),MEDTB6(1,1))
      equivalence (MEDTBL(1,121),MEDTB7(1,1))
      equivalence (MEDTBL(1,131),MEDTB8(1,1))
      equivalence (MEDTBL(1,141),MEDTB9(1,1))
      equivalence (MEDTBL(1,151),MEDT10(1,1))
      equivalence (MEDTBL(1,161),MEDT11(1,1))
      equivalence (MEDTBL(1,171),MEDT12(1,1))
      equivalence (MEDTBL(1,181),MEDT13(1,1))
      equivalence (MEDTBL(1,191),MEDT14(1,1))
      equivalence (MEDTBL(1,201),MEDT15(1,1))
      equivalence (MEDTBL(1,211),MEDT16(1,1))
      equivalence (MEDTBL(1,221),MEDT17(1,1))
      equivalence (MEDTBL(1,231),MEDT18(1,1))
      equivalence (MEDTBL(1,241),MEDT19(1,1))
      equivalence (MEDTBL(1,251),MEDT20(1,1))
      equivalence (MEDTBL(1,261),MEDT21(1,1))
      equivalence (MEDTBL(1,271),MEDT22(1,1))
      dimension STDAT1(7,20),STDAT2(7,20),STDAT3(7,20),STDAT4(7,20),
     *          STDAT5(7,20),STDAT6(7,20),STDAT7(7,20),STDAT8(7,20),
     *          STDAT9(7,20),STDA10(7,20),STDA11(7,20),STDA12(7,20),
     *          STDA13(7,20),STDA14(7,18)
      equivalence (STDATA(1,1),STDAT1(1,1))
      equivalence (STDATA(1,21),STDAT2(1,1))
      equivalence (STDATA(1,41),STDAT3(1,1))
      equivalence (STDATA(1,61),STDAT4(1,1))
      equivalence (STDATA(1,81),STDAT5(1,1))
      equivalence (STDATA(1,101),STDAT6(1,1))
      equivalence (STDATA(1,121),STDAT7(1,1))
      equivalence (STDATA(1,141),STDAT8(1,1))
      equivalence (STDATA(1,161),STDAT9(1,1))
      equivalence (STDATA(1,181),STDA10(1,1))
      equivalence (STDATA(1,201),STDA11(1,1))
      equivalence (STDATA(1,221),STDA12(1,1))
      equivalence (STDATA(1,241),STDA13(1,1))
      equivalence (STDATA(1,261),STDA14(1,1))
!     Data for common bloack lspion
      data AFACT/0.0/,SK/0.0/,X0/0.0/,X1/0.0/,CBAR/0.0/,DELTA0/0.0/,
     *IEV/0.0/,ISSB/0/
      data LMED/24/,NMED/278/
      data MEDTB1/'H','2','-','G','A','S',18*' ','H','2','-','L','I','Q'
     *,'U','I','D',15*' ','H','E','-','G','A','S',18*' ','L','I',22*' ',
     * 'B','E',22*' ','B',23*' ','C','-','2','.','2','6','5',' ','G','/'
     *,'C','M','*','*','3',9*' ', 'C','-','2','.','0','0',' ','G','/','C
     *','M','*','*','3',10*' ','C','-','1','.','7','0',' ','G','/','C','
     *M','*','*','3',10*' ','N','2','-','G','A','S',18*' ', 'O','2','-',
     *'G','A','S',18*' ','F',23*' ','N','E','-','G','A','S',18*' ','N','
     *A',22*' ', 'M','G',22*' ','A','L',22*' ','S','I',22*' ','P',23*' '
     *,'S',23*' ', 'C','L',22*' '/
      data MEDTB2/'A','R','-','G','A','S',18*' ','K',23*' ','C','A',22*'
     * ','S','C',22*' ', 'T','I',22*' ','V',23*' ','C','R',22*' ','M','N
     *',22*' ', 'F','E',22*' ','C','O',22*' ','N','I',22*' ','C','U',22*
     *' ', 'Z','N',22*' ','G','A',22*' ','G','E',22*' ','A','S',22*' ', 
     *'S','E',22*' ','B','R',22*' ','K','R','-','G','A','S',18*' ','R','
     *B',22*' '/
      data MEDTB3/'S','R',22*' ','Y',23*' ','Z','R',22*' ','N','I',22*' 
     *','M','O',22*' ', 'T','C',22*' ','R','U',22*' ','R','H',22*' ','P'
     *,'D',22*' ', 'A','G',22*' ','C','D',22*' ','I','N',22*' ','S','N',
     *22*' ', 'S','B',22*' ','T','E',22*' ','I',23*' ','X','E','-','G','
     *A','S',18*' ', 'C','S',22*' ','B','A',22*' ','L','A',22*' '/      
      data MEDTB4/'C','E',22*' ','P','R',22*' ','N','D',22*' ','P','M',2
     *2*' ', 'S','M',22*' ','E','U',22*' ','G','D',22*' ','T','B',22*' '
     *,'D','Y',22*' ', 'H','O',22*' ','E','R',22*' ','T','M',22*' ','Y',
     *'B',22*' ','L','U',22*' ', 'H','F',22*' ','T','A',22*' ','W',23*' 
     *','R','E',22*' ','O','S',22*' ', 'I','R',22*' '/
      data MEDTB5/'P','T',22*' ','A','U',22*' ','H','G',22*' ','T','L',2
     *2*' ', 'P','B',22*' ','B','I',22*' ','P','O',22*' ','R','N','-','G
     *','A','S',18*' ', 'R','A',22*' ','A','C',22*' ','T','H',22*' ','P'
     *,'A',22*' ', 'U',23*' ','N','P',22*' ','P','U',22*' ','A','M',22*'
     * ', 'C','M',22*' ','B','K',22*' ','A',' ','1','5','0','-',
     *'P','L','A','S','T','I','C',11*' ', 'A','C','E','T','O','N','E'
     *,17*' '/
      data MEDTB6/'A','C','E','T','Y','L','E','N','E',15*' ','A','D','E'
     *,'N','I','N','E',17*' ','A','D','I','P','O','S','E',' ','T','I','S
     *','S','U','E',10*' ', 'A','I','R','-','G','A','S',17*' ','A','L','
     *A','N','I','N','E',17*' ','A','L','U','M','I','N','I','U','M',' ',
     *'O','X','I','D','E',9*' ', 'A','M','B','E','R',19*' ','A','M','M',
     *'O','N','I','A',17*' ','A','N','I','L','I','N','E',17*' ', 'A','N'
     *,'T','H','R','A','C','E','N','E',14*' ','B','-','1','0','0',' ','B
     *','O','N','E','-','E','Q','.',' ','P','L','A','S','T','I','C',2*' 
     *', 'B','A','K','E','L','I','T','E',16*' ','B','A','R','I','U','M',
     *' ','F','L','U','O','R','I','D','E',9*' ', 'B','A','R','I','U','M'
     *,' ','S','U','L','F','A','T','E',10*' ','B','E','N','Z','E','N','E
     *',17*' ', 'B','E','R','Y','L','L','I','U','M',' ','O','X','I','D',
     *'E',9*' ','B','G','O',21*' ','B','L','O','O','D',' ','(','I','C',
     *'R','P',')',12*' ','B','O','N','E',',',' ','C','O','M',
     *'P','A','C','T',' ','(','I','C','R','U',')',4*' ', 'B','O','N','E'
     *,' ','C','O','R','T','I','C','A','L',' ','(','I','C','R','P',')',4
     **' '/
      data MEDTB7/'B','O','R','O','N',' ','C','A','R','B','I','D','E',11
     **' ','B','O','R','N',' ','O','X','I','D','E',14*' ', 'B','R','A','
     *I','N',' ','(','I','C','R','P',')',12*' ','B','U','T','A','N','E',
     *18*' ', 'N','-','B','U','T','Y','L',' ','A','L','C','H','O','L',10
     **' ','C','-','5','5','2',' ','A','I','R','-','E','Q','.',' ','P','
     *L','A','S','T','I','C',' ',' ',' ', 'C','A','D','M','I','U','M',' 
     *','T','E','L','L','U','R','I','D','E',7*' ','C','A','D','M','I','U
     *','M',' ','T','U','N','G','S','T','A','T','E',7*' ', 'C','A','L','
     *C','I','U','M',' ','C','A','R','B','O','N','I','T','E',7*' ','C','
     *A','F','2',20*' '/
      data MEDTB8/'C','A','L','C','I','U','M',' ','O','X','I','D','E',11
     **' ','C','A','L','C','I','U','M',' ','S','U','L','F','A','T','E',9
     **' ', 'C','A','L','C','I','U','M',' ','T','U','N','G','S','T','A',
     *'T','E',7*' ','C','A','R','B','O','N',' ','D','I','O','X','I','D',
     *'E',10*' ', 'C','A','R','B','O','N',' ','T','E','T','R','A','C','H
     *','L','O','R','I','D','E',4*' ','C','E','L','L','O','P','H','A','N
     *','E',14*' ', 'C','E','L','L','U','L','O','S','E',' ','A','C','E',
     *'T','A','T','E',' ','B','U','T','Y','R','A','C','E','L','L','U','L
     *','O','S','E',' ','N','I','T','R','A','T','E',7*' ', 'C','E','R','
     *I','C',' ','S','U','R','F','A','R','E',' ','D','O','S','I','M','E'
     *,'T','E','R',1*' ','C','E','S','I','U','M',' ','F','L','U','O','R'
     *,'I','D','E',9*' '/
      data MEDTB9/'C','S','I',21*' ','C','H','L','O','R','O','B','E',
     *'N','Z','E','N','E',11*' ','C',
     *'H','L','O','R','O','F','O','R','M',14*' ','C','O','N','C','R',
     *'E','T','E',',',' ','P','O','R','T','L','A','N','D',6*' ', 'C','Y'
     *,'C','L','O','H','E','X','A','N','E',13*' ','1',',','2','-','D','I
     *','C','H','L','O','R','O','B','E','N','Z','E','N','E',5*' ', 'D','
     *I','C','H','L','O','R','O','D','I','E','T','H','Y','L',' ','E','T'
     *,'H','E','R',3*' ','1',',','2','-','D','I','C','H','L','O','R','O'
     *,'E','T','H','A','N','E',6*' ', 'D','I','E','T','H','Y','L',' ','E
     *','T','H','E','R',11*' ','N',',','N','-','D','I','M','E','T','H','
     *Y','L',' ','F','O','R','M','A','M','I','D','E',' ',' '/
      data MEDT10/'D','I','M','E','T','H','Y','L',' ','S','U','L','F','O
     *','X','I','D','E',6*' ','E','T','H','A','N','E',18*' ', 'E','T','H
     *','Y','L',' ','A','L','C','O','H','O','L',11*' ','E','T','H','Y','
     *L',' ','C','E','L','L','U','L','O','S','E',9*' ', 'E','T','H','Y',
     *'L','E','N','E',16*' ','E','Y','E',' ','L','E','N','S',' ','(','I'
     *,'C','R','P',')',9*' ', 'F','E','R','R','I','C',' ','O','X','I','D
     *','E',12*' ','F','E','R','R','O','B','O','R','I','D','E',13*' ', '
     *F','E','R','R','O','U','S',' ','O','X','I','D','E',11*' ','F','E',
     *'R','R','O','U','S',' ','S','U','L','F','A','T','E',' ','D','O','S
     *','I','M','E','T','E'/
      data MEDT11/'F','R','E','O','N','-','1','2',16*' ','F','R','E','O'
     *,'N','-','1','2','B','2',14*' ','F','R','E','O','N','-','1','3',16
     **' ', 'F','R','E','O','N','-','1','3','B','1',14*' ','F','R','E','
     *O','N','-','1','3','I','1',14*' ', 'G','A','D','O','L','I','N','I'
     *,'U','M',' ','O','X','Y','S','U','L','F','I','D','E',' ',' ',' ','
     *G','A','L','L','I','U','M',' ','A','R','S','E','N','I','D','E',8*'
     * ', 'G','E','L',' ','I','N',' ','P','H','O','T','O','G','R','A','P
     *','H','I','C',' ','E','M','U','L','P','Y','R','E','X','-','G','L',
     *'A','S',14*' ', 'G','L','A','S','S',',',' ','L','E','A','D',
     *13*' '/
      data MEDT12/'G','L','A','S','S',',',' ','P','L','A','T','E',12*' '
     *, 'G','L','U','C','O','S','E',17*' ','G','L','U','T','A','M','I','
     *N','E',15*' ','G','L','Y','C','E','R','O','L',16*' ', 'G','U','A',
     *'N','I','N','E',17*' ','G','Y','P','S','U','M',',',' ','P','L','A'
     *,'S','T','E','R',' ','O','F',' ','P','A','R','I','S', 'N','-','H',
     *'E','P','T','A','N','E',15*' ','N','-','H','E','X','A','N','E',16*
     *' ', 'K','A','P','T','O','N',18*' ',
     *'L','A','N','T','H','A','N','U'
     *,'M',' ','O','X','Y','B','R','O','M','I','D','E',4*' '/
      data MEDT13/'L','A','N','T','H','A','N','U','M',' ','O','X','Y','S
     *','U','L','F','I','D','E',4*' ','L','E','A','D',' ','O','X','I','D
     *','E',14*' ', 'L','I','T','H','I','U','M',' ','A','M','I','D','E',
     *11*' ','L','I','T','H','I','U','M',' ','C','A','R','B','O','N','A'
     *,'T','E',7*' ','L','I','F',21*' ',
     *'L','I','T','H','I','U','M',' ','H','Y','D','R','I','D','E',9*' ',
     *'L','I','I',21*' ','L','I','T','H','I','U','M',' ','O','X','I',
     *'D','E',11*' ', 'L','I','T','H','I','U','M',' ','T','E','T','R',
     *'A','B','O','R','A','T','E',5*' ','L','U','N','G',' ',
     *'(','I','C','R','P',')',13*' '/
      data MEDT14/'M','3',' ','W','A','X',18*' ','M','A','G','N','E','S'
     *,'I','U','M',' ','C','A','R','B','O','N','A','T','E',5*' ', 'M','A
     *','N','E','S','I','U','M',' ','F','L','U','O','R','I','D','E',7*' 
     *','M','A','G','N','E','S','I','U','M',' ','O','X','I','D','E',9*' 
     *', 'M','A','G','N','E','S','I','U','M',' ','T','E','T','R','A','B'
     *,'O','R','A','T','E',3*' ','M','E','R','C','U','R','I','C',' ','I'
     *,'O','D','I','D','E',9*' ', 'M','E','T','H','A','N','E',17*' ','M'
     *,'E','T','H','A','N','O','L',16*' ', 'M','I','X',' ','D',' ','W','
     *A','X',15*' ','M','S','2','0',' ','T','I','S','S','U','E',' ','S',
     *'U','B','S','T','I','T','U','T','E',' ',' '/
      data MEDT15/'M','U','S','C','L','E',',',' ','S','K','E','L','E','T
     *','A','L',' ','(','I','C','R','P',')',' ','M','U','S','C','L','E',
     *',',' ','S','T','R','I','A','T','E','D',' ','(','I','C','R','U',')
     *',' ', 'M','U','S','C','L','E','-','E','Q','.',' ','L','I','Q','.'
     *,' ','W',' ','S','U','C','R','O','S','M','U','S','C','L','E','-','
     *E','Q','.',' ','L','I','Q','.',' ','W','/','O',' ','S','U','C','R'
     *, 'N','A','P','T','H','A','L','E','N','E',14*' ','N','I','T','R','
     *O','B','E','N','Z','E','N','E',12*' ', 'N','I','T','R','O','U','S'
     *,' ','O','X','I','D','E',11*' ','N','Y','L','O','N',',',' ','D','U
     *',' ','P','O','N','T',10*' ', 'N','Y','L','O','N',',',' ','T','Y',
     *'P','E',' ','6',' ','A','N','D',' ','6','/','6',3*' ','N','Y','L',
     *'O','N',',',' ','T','Y','P','E',' ','6','/','1','0',8*' '/
      data MEDT16/'N','Y','L','O','N',',',' ','T','Y','P','E',' ','1','1
     *',10*' ','O','C','T','A','N','E',',',' ','L','I','Q','U','I','D',1
     *0*' ', 'P','A','R','A','F','F','I','N',' ','W','A','X',12*' ','N',
     *'-','P','E','N','T','A','N','E',15*' ', 'P','H','O','T','O',
     *'E','M','U','L','S','I','O','N',11*' ','P','L','A','S','T','I',
     *'C',' ','S','C','I','N','T','.',10*' ', 'P',
     *'L','U','T','O','N','I','U','M',' ','D','I','O','X','I','D','E',7*
     *' ','P','O','L','Y','C','R','Y','L','O','N','I','T','R','I','L','E
     *',8*' ', 'P','O','L','Y','C','A','R','B','O','N','A','T','E',11*' 
     *','P','O','L','Y','C','H','L','O','R','O','S','T','Y','R','W','N',
     *'E',7*' '/
      data MEDT17/'P','O','L','Y','E','T','H','Y','L','E','N','E',12*' '
     *,'M','Y','L','A','R',19*' ','L','U','C','I','T','E',18*' ', 'P','O
     *','L','Y','O','X','Y','M','E','T','H','Y','L','E','N','E',8*' ','P
     *','O','L','Y','P','R','O','P','Y','L','E','N','E',11*' ', 'P','O',
     *'L','Y','S','T','Y','R','E','N','E',13*' ','T','E','F','L','O','N'
     *,18*' ', 'P','O','L','Y','T','R','I','F','L','U','O','R','O','C','
     *H','L','O','R','O','E','T','H','Y','.','P','O','L','Y','V','I','N'
     *,'Y','L',' ','A','C','E','T','A','T','E',7*' ', 'P','O','L','Y','V
     *','I','N','Y','L',' ','A','L','C','O','H','O','L',7*' '/
      data MEDT18/'P','O','L','Y','V','I','N','Y','L',' ','B','U','T','Y
     *','R','A','L',7*' ', 'P','O','L','Y','V','I','N','Y','L',' ','C','
     *H','L','O','R','I','D','E',6*' ','S','A','R','A','N',19*' ', 'P','
     *L','O','Y','V','I','N','Y','L','I','D','E','N','E',' ','F','L','U'
     *,'O','R','I','D','E',' ','P','O','L','Y','V','I','N','Y','L',' ','
     *P','Y','R','R','O','L','I','D','O','N','E',' ',' ',' ', 'P','O','T
     *','A','S','S','I','U','M',' ','I','O','D','I','N','E',8*' ','P','O
     *','T','A','S','S','I','U','M',' ','O','X','I','D','E',9*' ', 'P','
     *R','O','P','A','N','E',17*' ','P','R','O','P','A','N','E',',',' ',
     *'L','I','Q','U','I','D',9*' ', 'N','-','P','R','O','P','Y','L',' '
     *,'A','L','C','O','H','O','L',8*' '/
      data MEDT19/'P','Y','R','I','D','I','N','E',16*' ','R','U','B','B'
     *,'E','R',',',' ','B','U','T','Y','L',11*' ', 'R','U','B','B','E','
     *R',',',' ','N','A','T','U','R','A','L',9*' ','R','U','B','B','E','
     *R',',',' ','N','E','O','P','R','E','N','E',8*' ', 'S','I','O','2',
     *20*' ','A','G','B','R',20*' ', 'A','G','C','L',20*' ',
     *'S','I','L','V','E','R',' ','H',
     *'A','L','I','D','E','S',' ','I','N',' ','E','M','U',
     *'L','.',' ', 'S','I','L','V','E','R',' ','I','O','D','I','D','E',1
     *1*' ','S','K','I','N',' ','(','I','C','R','P',')',13*' '/
      data MEDT20/'S','O','D','I','U','M',' ','C','A','R','B','O','N','A
     *','T','E',8*' ','N','A','I',21*' ', 'S','O','D','I','U','M',' ',
     *'M','O','N','O','X','I','D','E',9*' ',
     *'S','O','D','I','U','M',' ','N','I','T','R','A','T','E',
     *10*' ', 'S','T','I','L','B','E','N','E',16*' ','S','U','C','R','O'
     *,'S','E',17*' ', 'T','R','R','P','H','E','N','Y','L',15*' ','T','E
     *','S','T','E','S',' ','(','I','C','R','P',')',11*' ', 'T','E','T',
     *'R','A','C','H','L','O','R','O','E','T','H','T','L','E','N','E',5*
     *' ','T','H','A','L','L','I','U','M',' ','C','H','L','O','R','I','D
     *','E',7*' '/
      data MEDT21/'T','I','S','S','U','E',',',' ','S','O','F','T',' ','(
     *','I','C','R','P',')',5*' ','I','C','R','U',' ','F','O','U','R','-
     *','C','O','M','P','.',' ','T','I','S','S','U','E',' ',' ', 'T','I'
     *,'S','S','U','E','-','E','Q','.',' ','G','A','S',' ','(','M','E','
     *T','H','A','N','E',')','T','I','S','S','U','E','-','E','Q','.',' '
     *,'G','A','S',' ','(','P','R','O','P','A','N','E',')', 'T','I','T',
     *'A','N','I','U','M',' ','D','I','O','X','I','D','E',8*' ','T','O',
     *'L','U','E','N',18*' ', 'T','R','I','C','H','L','O','R','O','E','T
     *','H','Y','L','E','N','E',7*' ','T','R','I','E','T','H','Y','L',' 
     *','P','H','O','S','P','H','A','T','E',6*' ', 'T','U','N','G','S','
     *T','E','N',' ','H','E','X','A','F','L','U','O','R','I','D','E',3*'
     * ', 'U','R','A','N','I','U','M',' ','D','I','C','A','R','B','I','D
     *','E',7*' '/
      data MEDT22/'U','R','A','N','I','U','M',' ','M','O','N','O','C','A
     *','R','B','I','D','E',5*' ','U','R','A','N','I','U','M',' ','O','X
     *','I','D','E',11*' ', 'U','R','E','A',20*' ','V','A','L','I','N','
     *E',18*' ','V','I','T','O','N',19*' ', 'H','2','O',21*' ','H','2',
     *'O',' ','V','A','P','O','R',15*' ', 
     *'X','Y','L','E','N','E',18*' '/
      data STDAT1/0.14092,5.7273,1.8639,3.2718,19.2,9.5835,0.0, 0.13483,
     *5.6249,0.4759,1.9215,21.8,3.2632,0.0, 0.13443,5.8347,2.2017,3.6122
     *,41.8,11.1393,0.0, 0.95136,2.4993,0.1304,1.6397,40.0,3.1221,0.14, 
     *0.80392,2.4339,0.0592,1.6922,63.7,2.7847,0.14, 0.56224,2.4512,0.03
     *05,1.9688,76.0,2.8477,0.14, 0.26142,2.8697,-0.0178,2.3415,78.0,2.8
     *680,0.12, 0.20240,3.0036,-0.0351,2.4860,78.0,2.9925,0.10, 0.20762,
     *2.9532,0.0480,2.5387,78.0,3.1550,0.14, 0.15349,3.2125,1.7378,4.132
     *3,82.0,10.5400,0.0, 0.11778,3.2913,1.7541,4.3213,95.0,10.7004,0.0,
     * 0.11083,3.2962,1.8433,4.4096,115.0,10.9653,0.0, 0.08064,3.5771,2.
     *0735,4.6421,137.0,11.9041,0.0, 0.07772,3.6452,0.2880,3.1962,149.0,
     *5.0526,0.08, 0.08163,3.6166,0.1499,3.0668,156.0,4.5297,0.08, 0.080
     *24,3.6345,0.1708,3.0127,166.0,4.2395,0.12, 0.14921,3.2546,0.2014,2
     *.8715,173.0,4.4351,0.14, 0.23610,2.9158,0.1696,2.7815,173.0,4.5214
     *,0.14, 0.33992,2.6456,0.1580,2.7159,180.0,4.6659,0.14, 0.19849,2.9
     *702,1.5555,4.2994,174.0,11.1421,0.0/
      data STDAT2/0.19714,2.9618,1.7635,4.4855,188.0,11.9480,0.0, 0.1982
     *7,2.9233,0.3851,3.1724,190.0,5.6423,0.10, 0.15643,3.0745,0.3228,3.
     *1191,191.0,5.0396,0.14, 0.15754,3.0517,0.1640,3.0593,216.0,4.6949,
     *0.10, 0.15662,3.0302,0.0957,3.0386,233.0,4.4450,0.12, 0.15436,3.01
     *63,0.0691,3.0322,245.0,4.2659,0.14, 0.15419,2.9896,0.0340,3.0451,2
     *57.0,4.1781,0.14, 0.14973,2.9796,0.0447,3.1074,272.0,4.2702,0.14, 
     *0.14680,2.9632,-0.0012,3.1531,286.0,4.2911,0.12, 0.14474,2.9502,-0
     *.0187,3.1790,297.0,4.2601,0.12, 0.16496,2.8430,-0.0566,3.1851,311.
     *0,4.3115,0.10, 0.14339,2.9044,-0.0254,3.2792,322.0,4.4190,0.08, 0.
     *14714,2.8652,0.0049,3.3668,330.0,4.6906,0.08, 0.09440,3.1314,0.226
     *7,3.5434,334.0,4.9353,0.14, 0.07188,3.3306,0.3376,3.6096,350.0,5.1
     *411,0.14, 0.06633,3.4176,0.1767,3.5702,347.0,5.0510,0.08, 0.06568,
     *3.4317,0.2258,3.6264,348.0,5.3210,0.10, 0.06335,3.4670,1.5262,4.98
     *99,343.0,11.7307,0.0, 0.07446,3.4051,1.7158,5.0748,352.0,12.5115,0
     *.0, 0.07261,3.4177,0.5737,3.7995,363.0,6.4776,0.14/
      data STDAT3/0.07165,3.4435,0.4585,3.6778,366.0,5.9867,0.14, 0.0713
     *8,3.4585,0.3608,3.5542,379.0,5.4801,0.14, 0.07177,3.4533,0.2957,3.
     *4890,393.0,5.1774,0.14, 0.13883,3.0930,0.1785,3.2201,417.0,5.0141,
     *0.14, 0.10525,3.2549,0.2267,3.2784,424.0,4.8793,0.14, 0.16572,2.97
     *38,0.0949,3.1253,428.0,4.7769,0.14, 0.19342,2.8707,0.0599,3.0834,4
     *41.0,4.7694,0.14, 0.19205,2.8633,0.0576,3.1069,449.0,4.8008,0.14, 
     *0.24178,2.7239,0.0563,3.0555,470.0,4.9358,0.14, 0.24585,2.6899,0.0
     *657,3.1074,470.0,5.0630,0.14, 0.24609,2.6772,0.1281,3.1667,469.0,5
     *.2727,0.14, 0.23879,2.7144,0.2406,3.2032,488.0,5.5211,0.14, 0.1868
     *9,2.8576,0.2879,3.2959,488.0,5.5340,0.14, 0.16652,2.9519,0.3189,3.
     *3489,487.0,5.6241,0.14, 0.13815,3.0354,0.3296,3.4418,485.0,5.7131,
     *0.14, 0.23766,2.7276,0.0549,3.2596,491.0,5.9488,0.0, 0.23314,2.741
     *4,1.5630,4.7371,482.0,12.7281,0.0, 0.18233,2.8866,0.5473,3.5914,48
     *8.0,6.9135,0.14, 0.18268,2.8906,0.4190,3.4547,491.0,6.3153,0.14, 0
     *.18591,2.8828,0.3161,3.3293,501.0,5.7850,0.14/
      data STDAT4/0.18885,2.8592,0.2713,3.3432,523.0,5.7837,0.14, 0.2326
     *5,2.7331,0.2333,3.2773,535.0,5.8096,0.14, 0.23530,2.7050,0.1984,3.
     *3063,546.0,5.8290,0.14, 0.24280,2.6674,0.1627,3.3199,560.0,5.8224,
     *0.14, 0.24698,2.6403,0.1520,3.3460,574.0,5.8597,0.14, 0.24448,2.62
     *45,0.1888,3.4633,580.0,6.2278,0.14, 0.25109,2.5977,0.1058,3.3932,5
     *91.0,5.8738,0.14, 0.24453,2.6056,0.0947,3.4224,614.0,5.9045,0.14, 
     *0.24665,2.5849,0.0822,3.4474,628.0,5.9183,0.14, 0.24638,2.5726,0.0
     *761,3.4782,650.0,5.9587,0.14, 0.24823,2.5573,0.0648,3.4922,658.0,5
     *.9521,0.14, 0.24889,2.5469,0.0812,3.5085,674.0,5.9677,0.14, 0.2529
     *5,2.5141,0.1199,3.6246,684.0,6.3325,0.14, 0.24033,2.5643,0.1560,3.
     *5218,694.0,5.9785,0.14, 0.22918,2.6155,0.1965,3.4337,705.0,5.7139,
     *0.14, 0.17798,2.7623,0.2117,3.4805,718.0,5.5262,0.14, 0.15509,2.84
     *47,0.2167,3.4960,727.0,5.4059,0.14, 0.15184,2.8627,0.0559,3.4845,7
     *36.0,5.3445,0.08, 0.12751,2.9608,0.0891,3.5414,746.0,5.3083,0.10, 
     *0.12690,2.9658,0.0819,3.5480,757.0,5.3418,0.10/
      data STDAT5/0.11128,3.0417,0.1484,3.6212,790.0, 5.4732,0.12, 0.097
     *56,3.1101,0.2021,3.6979,790.0, 5.5747,0.14, 0.11014,3.0519,0.2756,
     *3.7275,800.0, 5.9605,0.14, 0.09455,3.1450,0.3491,3.8044,810.0, 6.1
     *365,0.14, 0.09359,3.1608,0.3776,3.8073,823.0, 6.2018,0.14, 0.09410
     *,3.1671,0.4152,3.8248,823.0, 6.3505,0.14, 0.09282,3.1830,0.4267,3.
     *8293,830.0, 6.4003,0.14, 0.20798,2.7409,1.5368,4.9889,794.0,13.283
     *9,0.0, 0.08804,3.2454,0.5991,3.9428,826.0, 7.0452,0.14, 0.08567,3.
     *2683,0.4559,3.7966,841.0, 6.3742,0.14, 0.08655,3.2610,0.4202,3.768
     *1,847.0, 6.2473,0.14, 0.14770,2.9845,0.3144,3.5079,878.0, 6.0327,0
     *.14, 0.19677,2.8171,0.2260,3.3721,890.0, 5.8694,0.14, 0.19741,2.80
     *82,0.1869,3.3690,902.0, 5.8149,0.14, 0.20419,2.7679,0.1557,3.3981,
     *921.0, 5.8748,0.14, 0.20308,2.7615,0.2274,3.5021,934.0, 6.2813,0.1
     *4, 0.20257,2.7579,0.2484,3.5160,939.0, 6.3097,0.14, 0.20192,2.7560
     *,0.2378,3.5186,952.0, 6.2912,0.14, 0.10783,3.4442,0.1329,2.6234, 6
     *5.1, 3.1100,0.0, 0.11100,3.4047,0.2197,2.6028, 64.2, 3.4341,0.0/
      data STDAT6/0.12167,3.4277, 1.6017,4.0074, 58.2, 9.8419,0.0, 0.209
     *08,3.0271, 0.1295,2.4219, 71.4, 3.1724,0.0, 0.10278,3.4817, 0.1827
     *,2.6530, 63.2, 3.2367,0.0, 0.10914,3.3994, 1.7418,4.2759, 85.7,10.
     *5961,0.0, 0.11484,3.3526, 0.1354,2.6336, 71.9, 3.0965,0.0, 0.08500
     *,3.5458, 0.0402,2.8665,145.2, 3.5682,0.0, 0.11934,3.4098, 0.1335,2
     *.5610, 63.2, 3.0701,0.0, 0.08315,3.6464, 1.6822,4.1158, 53.7, 9.87
     *63,0.0, 0.13134,3.3434, 0.1618,2.5805, 66.2, 3.2622,0.0, 0.14677,3
     *.2831, 0.1146,2.5213, 69.5, 3.1514,0.0, 0.05268,3.7365, 0.1252,3.0
     *420, 85.9, 3.4528,0.0, 0.12713,3.3470, 0.1471,2.6055, 72.4, 3.2582
     *,0.0, 0.15991,2.8867,-0.0098,3.3871,375.9, 5.4122,0.0, 0.11747,3.0
     *427,-0.0128,3.4069,285.7, 4.8923,0.0, 0.16519,3.2174, 0.1710,2.509
     *1, 63.4, 3.3269,0.0, 0.10755,3.4927, 0.0241,2.5846, 93.2, 2.9801,0
     *.0, 0.09569,3.0781, 0.0456,3.7816,534.1, 5.7409,0.0, 0.08492,3.540
     *6, 0.2239,2.8017, 75.2, 3.4581,0.0, 0.05822,3.6419, 0.0944,3.0201,
     * 91.9, 3.3390,0.0, 0.06198,3.5919, 0.1161,3.0919,106.4, 3.6488,0.0
     */
      data STDAT7/0.37087,2.8076,0.0093,2.1006,84.7,2.9859,0.0, 0.11548,
     *3.3832,0.1843,2.7379,99.6,3.6027,0.0, 0.08255,3.5585,0.2206,2.8021
     *,73.3,3.4279,0.0, 0.10852,3.4884,1.3788,3.7524,48.3,8.5633,0.0, 0.
     *10081,3.5139,0.1937,2.6439,59.9,3.2425,0.0, 0.10492,3.4344,0.1510,
     *2.7083,86.8,3.3338,0.0, 0.24840,2.6665,0.0438,3.2836,539.3,5.9096,
     *0.0, 0.12861,2.9150,0.0123,3.5941,468.3,5.3594,0.0, 0.08301,3.4120
     *,0.0492,3.0549,136.4,3.7738,0.0, 0.06942,3.5263,0.0676,3.1683,166.
     *0,4.0653,0.0, 0.12128,3.1936,-0.0172,3.0171,176.1,4.1209,0.0, 0.07
     *708,3.4495,0.0587,3.1229,152.3,3.9388,0.0, 0.06210,3.2649,0.0323,3
     *.8932,395.0,5.2603,0.0, 0.11768,3.3227,1.6294,4.1825,85.0,10.1537,
     *0.0, 0.19018,3.0116,0.1773,2.9165,166.3,4.7712,0.0, 0.11151,3.3810
     *,0.1580,2.6778,77.6,3.2647,0.0, 0.11444,3.3738,0.1794,2.6809,74.6,
     *3.3497,0.0, 0.11813,3.3237,0.1897,2.7253,87.0,3.4762,0.0, 0.07666,
     *3.5607,0.2363,2.8769,76.7,3.5212,0.0, 0.22052,2.7280,0.0084,3.3374
     *,440.7,5.9046,0.0/
      data STDAT8/0.25381,2.6657,0.0395,3.3353,553.1,6.2807,0.0, 0.09856
     *,3.3797,0.1714,2.9272,89.1,3.8201,0.0, 0.16959,3.0627,0.1786,2.958
     *1,156.0,4.7055,0.0, 0.07515,3.5467,0.1301,3.0466,135.2,3.9464,0.0,
     * 0.12035,3.4278,0.1728,2.5549,56.4,3.1544,0.0, 0.16010,3.0836,0.15
     *87,2.8276,106.5,4.0348,0.0, 0.06799,3.5250,0.1773,3.1586,103.5,4.0
     *135,0.0, 0.13383,3.1675,0.1375,2.9529,111.9,4.1849,0.0, 0.10550,3.
     *4586,0.2231,2.6745,60.0,3.3721,0.0, 0.11470,3.3710,0.1977,2.6686,6
     *6.6,3.3311,0.0, 0.06619,3.5708,0.2021,3.1263,98.6,3.9844,0.0, 0.09
     *627,3.6095,1.5107,3.8743,45.4,9.1043,0.0, 0.09878,3.4834,0.2218,2.
     *7052,62.9,3.3699,0.0, 0.11077,3.4098,0.1683,2.6257,69.3,3.2415,0.0
     *, 0.10636,3.5387,1.5528,3.9327,50.7,9.4380,0.0, 0.09690,3.4550,0.2
     *070,2.7446,73.3,3.3720,0.0, 0.10478,3.1313,-0.0074,3.2573,227.3,4.
     *2245,0.0, 0.12911,3.0240,-0.0988,3.1749,261.0,4.2057,0.0, 0.12959,
     *3.0168,-0.0279,3.2002,248.6,4.3175,0.0, 0.08759,3.4923,0.2378,2.82
     *54,76.4,3.5183,0.0/
      data STDAT9/0.07978,3.4626,0.3035,3.2659,143.0,4.8251,0.0, 0.05144
     *,3.5565,0.3406,3.7956,284.9,5.7976,0.0, 0.07238,3.5551,0.3659,3.23
     *37,126.6,4.7483,0.0, 0.03925,3.7194,0.3522,3.7554,210.5,5.3555,0.0
     *, 0.09112,3.1658,0.2847,3.7280,293.5,5.8774,0.0, 0.22161,2.6300,-0
     *.1774,3.4045,493.3,5.5347,0.0, 0.07152,3.3356,0.1764,3.6420,384.9,
     *5.3299,0.0, 0.10102,3.4418,0.1709,2.7058,74.8,3.2687,0.0, 0.08270,
     *3.5224,0.1479,2.9933,134.0,3.9708,0.0, 0.09544,3.0740,0.0614,3.814
     *6,526.4,5.8476,0.0, 0.07678,3.5381,0.1237,3.0649,145.4,4.0602,0.0,
     * 0.10783,3.3946,0.1411,2.6700,77.2,3.1649,0.0, 0.11931,3.3254,0.13
     *47,2.6301,73.3,3.1167,0.0, 0.10168,3.4481,0.1653,2.6862,72.6,3.226
     *7,0.0, 0.20530,3.0186,0.1163,2.4296,75.0,3.1171,0.0, 0.06949,3.513
     *4,0.0995,3.1206,129.7,3.8382,0.0, 0.11255,3.4885,0.1928,2.5706,54.
     *4,3.1978,0.0, 0.11085,3.5027,0.1984,2.5757,54.0,3.2156,0.0, 0.1597
     *2,3.1921,0.1509,2.5631,79.6,3.3497,0.0, 0.17830,2.8457,-0.0350,3.3
     *288,439.7,5.4666,0.0/
      data STDA10/0.21501,2.7298,-0.0906,3.2664,421.2,5.4470,0.0, 0.1964
     *5,2.7299,0.0356,3.5456,766.7,6.2162,0.0, 0.08740,3.7534,0.0198,2.5
     *152,55.5,2.7961,0.0, 0.09936,3.5417,0.0551,2.6598,87.9,3.2029,0.0,
     * 0.07593,3.7478,0.0171,2.7049,94.0,3.1667,0.0, 0.90567,2.5849,-0.0
     *988,1.4515,36.5,2.3580,0.0, 0.23274,2.7146,0.0892,3.3702,485.1,6.2
     *671,0.0, 0.08035,3.7878,-0.0511,2.5874,73.6,2.9340,0.0, 0.11075,3.
     *4389,0.0737,2.6502,94.6,3.2093,0.0, 0.08588,3.5353,0.2261,2.8001,7
     *5.3,3.4708,0.0, 0.07864,3.6412,0.1523,2.7529,67.9,3.2540,0.0, 0.09
     *219,3.5003,0.0860,2.7997,118.0,3.4319,0.0, 0.07934,3.6485,0.1369,2
     *.8630,134.3,3.7105,0.0, 0.08313,3.5968,0.0575,2.8580,143.8,3.6404,
     *0.0, 0.09703,3.4893,0.1147,2.7635,108.3,3.4328,0.0, 0.21513,2.7264
     *,0.1040,3.4728,684.5,6.3787,0.0, 0.09253,3.6257,1.6263,3.9716,41.7
     *,9.5243,0.0, 0.08970,3.5477,0.2529,2.7639,67.6,3.5160,0.0, 0.07490
     *,3.6823,0.1371,2.7145,60.9,3.0780,0.0, 0.08294,3.6061,0.1997,2.803
     *3,75.1,3.5341,0.0/
      data STDA11/0.08636,3.5330,0.2282,2.7999,75.3,3.4809,0.0, 0.08507,
     *3.5383,0.2249,2.8032,74.7,3.4636,0.0, 0.09481,3.4699,0.2098,2.7550
     *,74.3,3.3910,0.0, 0.09143,3.4982,0.2187,2.7680,74.2,3.4216,0.0, 0.
     *14766,3.2654,0.1374,2.5429,68.4,3.2274,0.0, 0.12727,3.3091,0.1777,
     *2.6630,75.8,3.4073,0.0, 0.11992,3.3318,1.6477,4.1565,84.9,10.1575,
     *0.0, 0.11513,3.4044,0.1503,2.6004,64.3,3.1250,0.0, 0.11818,3.3826,
     *0.1336,2.5834,63.9,3.0634,0.0, 0.11852,3.3912,0.1304,2.5681,63.2,3
     *.0333,0.0, 0.14868,3.2576,0.0678,2.4281,61.6,2.7514,0.0, 0.11387,3
     *.4776,0.1882,2.5664,54.7,3.1834,0.0, 0.12087,3.4288,0.1289,2.5084,
     *55.9,2.9551,0.0, 0.10809,3.5265,0.2086,2.5855,53.6,3.2504,0.0, 0.1
     *2399,3.0094,0.1009,3.4866,331.0,5.3319,0.0, 0.16101,3.2393,0.1464,
     *2.4855,64.7,3.1997,0.0, 0.20594,2.6522,-0.2311,3.5554,746.5,5.9719
     *,0.0, 0.16275,3.1975,0.1504,2.5159,69.6,3.2459,0.0, 0.12860,3.3288
     *,0.1606,2.6225,73.1,3.3201,0.0, 0.07530,3.5441,0.1238,2.9241,81.7,
     *3.4659,0.0/
      data STDA12/0.12108,3.4292,0.1370,2.5177,57.4,3.0016,0.0, 0.12679,
     *3.3076,0.1562,2.6507,78.7,3.3262,0.0, 0.11433,3.3836,0.1824,2.6681
     *,74.0,3.3297,0.0, 0.10808,3.4002,0.1584,2.6838,77.4,3.2514,0.0, 0.
     *15045,3.2855,0.1534,2.4822,59.2,3.1252,0.0, 0.16454,3.2224,0.1647,
     *2.5031,68.7,3.2999,0.0, 0.10606,3.4046,0.1648,2.7404,99.1,3.4161,0
     *.0, 0.07727,3.5085,0.1714,3.0265,120.7,3.8551,0.0, 0.11442,3.3762,
     *0.1769,2.6747,73.7,3.3309,0.0, 0.11178,3.3893,0.1401,2.6315,69.7,3
     *.1115,0.0, 0.11544,3.3983,0.1555,2.6186,67.2,3.1865,0.0, 0.12438,3
     *.2104,0.1559,2.9415,108.2,4.0532,0.0, 0.15466,3.1020,0.1314,2.9009
     *,134.3,4.2506,0.0, 0.10316,3.4200,0.1717,2.7375,88.8,3.3793,0.0, 0
     *.12504,3.3326,0.1324,2.5867,67.7,3.1017,0.0, 0.22053,2.7558,0.1044
     *,3.3442,431.9,6.1088,0.0, 0.16789,3.0121,0.0480,3.0110,189.9,4.646
     *3,0.0, 0.09916,3.5920,1.4326,3.7998,47.1,8.7878,0.0, 0.10329,3.562
     *0,0.2861,2.6568,52.0,3.5529,0.0, 0.09644,3.5415,0.2046,2.6681,61.1
     *,3.2915,0.0/
      data STDA13/0.16399,3.1977,0.1670,2.5245,66.2,3.3148,0.0, 0.12108,
     *3.4296,0.1347,2.5154,56.5,2.9915,0.0, 0.15058,3.2879,0.1512,2.4815
     *,59.8,3.1272,0.0, 0.09763,3.3632,0.1501,2.9461,93.0,3.7911,0.0, 0.
     *08408,3.5064,0.1385,3.0025,139.2,4.0029,0.0, 0.24582,2.6820,0.0352
     *,3.2109,486.6,5.6139,0.0, 0.22968,2.7041,-0.0139,3.2022,398.4,5.34
     *37,0.0, 0.24593,2.6814,0.0353,3.2117,487.1,5.6166,0.0, 0.25059,2.6
     *572,0.0148,3.2908,543.5,5.9342,0.0, 0.09459,3.4643,0.2019,2.7526,7
     *2.7,3.3546,0.0, 0.08715,3.5638,0.1287,2.8591,125.0,3.7178,0.0, 0.1
     *2516,3.0398,0.1203,3.5920,452.0,6.0572,0.0, 0.07501,3.6943,0.1652,
     *2.9793,148.8,4.1892,0.0, 0.09391,3.5097,0.1534,2.8221,114.6,3.6502
     *,0.0, 0.16659,3.2168,0.1734,2.5142,67.7,3.3680,0.0, 0.11301,3.3630
     *,0.1341,2.6558,77.5,3.1526,0.0, 0.14964,3.2685,0.1322,2.5429,71.7,
     *3.2639,0.0, 0.08533,3.5428,0.2274,2.7988,75.0,3.4698,0.0, 0.18595,
     *3.0156,0.1713,2.9083,159.2,4.6619,0.0, 0.18599,2.7690,0.0705,3.571
     *6,690.3,6.3009,0.0/
      data STDA14/0.08926,3.5110,0.2211,2.7799,72.3,3.4354,0.0, 0.09629,
     *3.4371,0.2377,2.7908,74.9,3.5087,0.0, 0.09946,3.4708,1.6442,4.1399
     *,61.2,9.9500,0.0, 0.09802,3.5159,1.5139,3.9916,59.5,9.3529,0.0, 0.
     *08569,3.3267,-0.0119,3.1647,179.5,3.9522,0.0, 0.13284,3.3558,0.172
     *2,2.5728,62.5,3.3026,0.0, 0.18272,3.0137,0.1803,2.9140,148.1,4.614
     *8,0.0, 0.06922,3.6302,0.2054,2.9428,81.2,3.6242,0.0, 0.03658,3.513
     *4,0.3020,4.2602,354.4,5.9881,0.0, 0.21120,2.6577,-0.2191,3.5208,75
     *2.0,6.0247,0.0, 0.22972,2.6169,-0.2524,3.4941,862.0,6.1210,0.0, 0.
     *20463,2.6711,-0.1938,3.5292,720.6,5.9605,0.0, 0.11609,3.3461,0.160
     *3,2.6525,72.8,3.2032,0.0, 0.11386,3.3774,0.1441,2.6227,67.7,3.1059
     *,0.0, 0.09965,3.4556,0.2106,2.7874,98.6,3.5943,0.0, 0.09116,3.4773
     *,0.2400,2.8004,75.0,3.5017,0.0, 0.08101,3.5901,1.7952,4.3437,71.6,
     *10.5962,0.0, 0.13216,3.3564,0.1695,2.5675,61.8,3.2698,0.0/
      data EPE/.01/,ZTHRE,ZEPE/80*0.0/,NIPE/20/,NALE/150/,EPG/.01/, ZTHR
     *G/0.0,.1,38*0.0/,ZEPG/0.0,.01,38*0.0/,NIPG/20/,NALG/1000/, EPR/.
     *01/,ZTHRR,ZEPR/80*0.0/,NIPR/20/,NALR/100/,EPSF/.01/,ZTHRS,ZEPS/80*
     *0.0/,NIPS/20/,NALS/100/, EPCP/.03/,ZTHRC,ZEPC/400*0.0/,NIPC/20/,NA
     *LC/2000/
      data NET/100/
      data ASYMT/'H','HE','LI','BE','B','C','N','O','F','NE', 'NA','MG',
     *'AL','SI','P','S','CL','AR','K','CA','SC','TI', 'V','CR','MN','FE'
     *,'CO','NI','CU','ZN','GA','GE','AS','SE','BR', 'KR','RB','SR','Y',
     *'ZR','NB','MO','TC','RU','RH','PD','AG','CD', 'IN','SN','SB','TE',
     *'I','XE','CS','BA','LA','CE','PR','ND', 'PM','SM','EU','GD','TB','
     *DY','HO','ER','TM','YB','LU','HF','TA', 'W','RE','OS','IR','PT','A
     *U','HG','TL','PB','BI','PO','AT','RN', 'FR','RA','AC','TH','PA','U
     *','NP','PU','AM','CM','BK','CF','ES', 'FM'/
      data WATBL/1.00797,4.0026,6.939,9.0122,10.811,12.01115,14.0067, 15
     *.9994,18.9984,20.183,22.9898,24.312,26.9815,28.088,30.9738, 32.064
     *,35.453,39.948,39.102,40.08,44.956,47.90,50.942,51.998, 54.9380,55
     *.847,58.9332,58.71,63.54,65.37,69.72,72.59,74.9216, 78.96,79.808,8
     *3.80,85.47,87.62,88.905,91.22,92.906,95.94,99.0, 101.07,102.905,10
     *6.4,107.87,112.4,114.82,118.69,121.75,127.60, 126.9044,131.30,132.
     *905,137.34,138.91, 140.12,140.907,144.24,147.,150.35,151.98,157.25
     *,158.924,162.50, 164.930,167.26,168.934,173.04,174.97,178.49,180.9
     *48,183.85, 186.2,190.2,192.2,195.08,196.987,200.59,204.37,207.19,2
     *08.980, 210.,210.,222.,223.,226.,227.,232.036,231.,238.03,237.,242
     *., 243.,247.,247.,248.,254.,253./
      data RHOTBL/0.0808,0.19,0.534,1.85,2.5,2.26,1.14,1.568,1.5,1.0, 0.
     *9712,1.74,2.702,2.4,1.82,2.07,2.2,1.65,0.86,1.55,3.02,4.54, 5.87,7
     *.14,7.3,7.86,8.71,8.90,8.9333,7.140,5.91,5.36,5.73,4.80, 4.2,3.4,1
     *.53,2.6,4.47,6.4,8.57,9.01,11.50,12.20,12.50,12.,10.5, 8.65,7.30,7
     *.31,6.684,6.24,4.93,2.7,1.873,3.5,6.15,6.90,6.769, 7.007, 1. ,7.54
     *,5.17,7.87,8.25,8.56,8.80,9.06,9.32,6.96,9.85, 11.40,16.60,19.30,2
     *0.53,22.48,22.42,21.45,19.30,14.19,11.85, 11.34,9.78,9.30, 1. ,4.,
     * 1. ,5., 1. ,11.0,15.37,18.90, 20.5,19.737,11.7,7.,1. , 1. , 1. ,
     *1./
      data ITBL/19.2,41.8,40.,63.7,76.0,78.0,82.0,95.0,115.,137., 149.,1
     *56.,166.,173.,173.,180.,174.,188.,190.,191.,216.,233.,245., 257.,2
     *72.,286.,297.,311.,322.,330.,334.,350.,347.,348.,357.,352., 363.,3
     *66.,379.,393.,417.,424.,428.,441.,449.,470.,470.,469.,488., 488.,4
     *87.,485.,491.,482.,488.,491.,501.,523.,535.,546.,560.,574., 580.,5
     *91.,614.,628.,650.,658.,674.,684.,694.,705.,718.,727.,736., 746.,7
     *57.,790.,790.,800.,810.,823.,823.,830.,825.,794.,827.,826., 841.,8
     *47.,878.,890.,902.,921.,934.,939.,952.,966.,980.,994./
      data ISTATB/1,1,0,0,0,0,1,1,1,1,0,0,0,0,0,0,1,1,0,0,0,0,0,0,0, 0,0
     *,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0, 0,0,0,
     *0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0, 0,0,0,0,0
     *,0,0,0,0,0,0,0,0/
      data ALRAD/5.31,4.79,4.74,4.71/,ALRADP/6.144,5.621,5.805,5.924/, A
     *1440/1194.0/,A183/184.15/
      data NFUNS/79/
      data NFARG/1,1,1,1,1,1,1,1,1,1,2,1,3,1,2,1,3,1,2,2,1,3,1,2,1,3,3,1
     *,1,3,3,4,1,1,3,4,2,1,2,2,1,3,1,1,1,1,1,2,1,1,1,1,1,2,1,3,1,3,3,4,1
     *,1,1,1,2,1,1,1,2,1,1,2,2,3,3,1,1,1,1/
      data FNAME(1,1),FNAME(2,1), FNAME(3,1), FNAME(4,1),FNAME(5,1),FNAM
     *E(6,1) / 'A','L','I','N', 2*' '/
      data FNAME(1,2),FNAME(2,2), FNAME(3,2), FNAME(4,2),FNAME(5,2),FNAM
     *E(6,2) / 'A','L','I','N','I', 1*' '/
      data FNAME(1,3),FNAME(2,3), FNAME(3,3), FNAME(4,3),FNAME(5,3),FNAM
     *E(6,3) / 'A','D','F','M','O','L'/
      data FNAME(1,4),FNAME(2,4), FNAME(3,4), FNAME(4,4),FNAME(5,4),FNAM
     *E(6,4) / 'A','D','I','M','O','L'/
      data FNAME(1,5),FNAME(2,5), FNAME(3,5), FNAME(4,5),FNAME(5,5),FNAM
     *E(6,5) / 'A','D','D','M','O','L'/
      data FNAME(1,6),FNAME(2,6), FNAME(3,6), FNAME(4,6),FNAME(5,6),FNAM
     *E(6,6) / 'A','L','O','G', 2*' '/
      data FNAME(1,7),FNAME(2,7), FNAME(3,7), FNAME(4,7),FNAME(5,7),FNAM
     *E(6,7) / 'E','X','P', 3*' '/
      data FNAME(1,8),FNAME(2,8), FNAME(3,8), FNAME(4,8),FNAME(5,8),FNAM
     *E(6,8) / 'A','R','E','C', 2*' '/
      data FNAME(1,9),FNAME(2,9), FNAME(3,9), FNAME(4,9),FNAME(5,9),FNAM
     *E(6,9) / 'A','L','K','E', 2*' '/
      data FNAME(1,10),FNAME(2,10), FNAME(3,10), FNAME(4,10),FNAME(5,10)
     *,FNAME(6,10) / 'A','L','K','E','I', 1*' '/
      data FNAME(1,11),FNAME(2,11), FNAME(3,11), FNAME(4,11),FNAME(5,11)
     *,FNAME(6,11) / 'A','M','O','L','D','M'/
      data FNAME(1,12),FNAME(2,12), FNAME(3,12), FNAME(4,12),FNAME(5,12)
     *,FNAME(6,12) / 'A','M','O','L','F','M'/
      data FNAME(1,13),FNAME(2,13), FNAME(3,13), FNAME(4,13),FNAME(5,13)
     *,FNAME(6,13) / 'A','M','O','L','R','M'/
      data FNAME(1,14),FNAME(2,14), FNAME(3,14), FNAME(4,14),FNAME(5,14)
     *,FNAME(6,14) / 'A','M','O','L','T','M'/
      data FNAME(1,15),FNAME(2,15), FNAME(3,15), FNAME(4,15),FNAME(5,15)
     *,FNAME(6,15) / 'A','N','I','H','D','M'/
      data FNAME(1,16),FNAME(2,16), FNAME(3,16), FNAME(4,16),FNAME(5,16)
     *,FNAME(6,16) / 'A','N','I','H','F','M'/
      data FNAME(1,17),FNAME(2,17), FNAME(3,17), FNAME(4,17),FNAME(5,17)
     *,FNAME(6,17) / 'A','N','I','H','R','M'/
      data FNAME(1,18),FNAME(2,18), FNAME(3,18), FNAME(4,18),FNAME(5,18)
     *,FNAME(6,18) / 'A','N','I','H','T','M'/
      data FNAME(1,19),FNAME(2,19), FNAME(3,19), FNAME(4,19),FNAME(5,19)
     *,FNAME(6,19) / 'A','P','R','I','M', 1*' '/
      data FNAME(1,20),FNAME(2,20), FNAME(3,20), FNAME(4,20),FNAME(5,20)
     *,FNAME(6,20) / 'B','H','A','B','D','M'/
      data FNAME(1,21),FNAME(2,21), FNAME(3,21), FNAME(4,21),FNAME(5,21)
     *,FNAME(6,21) / 'B','H','A','B','F','M'/
      data FNAME(1,22),FNAME(2,22), FNAME(3,22), FNAME(4,22),FNAME(5,22)
     *,FNAME(6,22) / 'B','H','A','B','R','M'/
      data FNAME(1,23),FNAME(2,23), FNAME(3,23), FNAME(4,23),FNAME(5,23)
     *,FNAME(6,23) / 'B','H','A','B','T','M'/
      data FNAME(1,24),FNAME(2,24), FNAME(3,24), FNAME(4,24),FNAME(5,24)
     *,FNAME(6,24) / 'B','R','E','M','D','R'/
      data FNAME(1,25),FNAME(2,25), FNAME(3,25), FNAME(4,25),FNAME(5,25)
     *,FNAME(6,25) / 'B','R','E','M','F','R'/
      data FNAME(1,26),FNAME(2,26), FNAME(3,26), FNAME(4,26),FNAME(5,26)
     *,FNAME(6,26) / 'B','R','E','M','D','Z'/
      data FNAME(1,27),FNAME(2,27), FNAME(3,27), FNAME(4,27),FNAME(5,27)
     *,FNAME(6,27) / 'B','R','M','S','D','Z'/
      data FNAME(1,28),FNAME(2,28), FNAME(3,28), FNAME(4,28),FNAME(5,28)
     *,FNAME(6,28) / 'B','R','E','M','F','Z'/
      data FNAME(1,29),FNAME(2,29), FNAME(3,29), FNAME(4,29),FNAME(5,29)
     *,FNAME(6,29) / 'B','R','M','S','F','Z'/
      data FNAME(1,30),FNAME(2,30), FNAME(3,30), FNAME(4,30),FNAME(5,30)
     *,FNAME(6,30) / 'B','R','E','M','R','R'/
      data FNAME(1,31),FNAME(2,31), FNAME(3,31), FNAME(4,31),FNAME(5,31)
     *,FNAME(6,31) / 'B','R','E','M','R','M'/
      data FNAME(1,32),FNAME(2,32), FNAME(3,32), FNAME(4,32),FNAME(5,32)
     *,FNAME(6,32) / 'B','R','E','M','R','Z'/
      data FNAME(1,33),FNAME(2,33), FNAME(3,33), FNAME(4,33),FNAME(5,33)
     *,FNAME(6,33) / 'B','R','E','M','T','M'/
      data FNAME(1,34),FNAME(2,34), FNAME(3,34), FNAME(4,34),FNAME(5,34)
     *,FNAME(6,34) / 'B','R','E','M','T','R'/
      data FNAME(1,35),FNAME(2,35), FNAME(3,35), FNAME(4,35),FNAME(5,35)
     *,FNAME(6,35) / 'B','R','M','S','R','M'/
      data FNAME(1,36),FNAME(2,36), FNAME(3,36), FNAME(4,36),FNAME(5,36)
     *,FNAME(6,36) / 'B','R','M','S','R','Z'/
      data FNAME(1,37),FNAME(2,37), FNAME(3,37), FNAME(4,37),FNAME(5,37)
     *,FNAME(6,37) / 'B','R','M','S','T','M'/
      data FNAME(1,38),FNAME(2,38), FNAME(3,38), FNAME(4,38),FNAME(5,38)
     *,FNAME(6,38) / 'C','O','H','E','T','M'/
      data FNAME(1,39),FNAME(2,39), FNAME(3,39), FNAME(4,39),FNAME(5,39)
     *,FNAME(6,39) / 'C','O','H','E','T','Z'/
      data FNAME(1,40),FNAME(2,40), FNAME(3,40), FNAME(4,40),FNAME(5,40)
     *,FNAME(6,40) / 'C','O','M','P','D','M'/
      data FNAME(1,41),FNAME(2,41), FNAME(3,41), FNAME(4,41),FNAME(5,41)
     *,FNAME(6,41) / 'C','O','M','P','F','M'/
      data FNAME(1,42),FNAME(2,42), FNAME(3,42), FNAME(4,42),FNAME(5,42)
     *,FNAME(6,42) / 'C','O','M','P','R','M'/
      data FNAME(1,43),FNAME(2,43), FNAME(3,43), FNAME(4,43),FNAME(5,43)
     *,FNAME(6,43) / 'C','O','M','P','T','M'/
      data FNAME(1,44),FNAME(2,44), FNAME(3,44), FNAME(4,44),FNAME(5,44)
     *,FNAME(6,44) / 'C','R','A','T','I','O'/
      data FNAME(1,45),FNAME(2,45), FNAME(3,45), FNAME(4,45),FNAME(5,45)
     *,FNAME(6,45) / 'E','B','I','N','D', 1*' '/
      data FNAME(1,46),FNAME(2,46), FNAME(3,46), FNAME(4,46),FNAME(5,46)
     *,FNAME(6,46) / 'E','B','R','1', 2*' '/
      data FNAME(1,47),FNAME(2,47), FNAME(3,47), FNAME(4,47),FNAME(5,47)
     *,FNAME(6,47) / 'E','D','E','D','X', 1*' '/
      data FNAME(1,48),FNAME(2,48), FNAME(3,48), FNAME(4,48),FNAME(5,48)
     *,FNAME(6,48) / 'E','I','I','T','M', 1*' '/
      data FNAME(1,49),FNAME(2,49), FNAME(3,49), FNAME(4,49),FNAME(5,49)
     *,FNAME(6,49) / 'E','S','I','G', 2*' '/
      data FNAME(1,50),FNAME(2,50), FNAME(3,50), FNAME(4,50),FNAME(5,50)
     *,FNAME(6,50) / 'F','C','O','U','L','C'/
      data FNAME(1,51),FNAME(2,51), FNAME(3,51), FNAME(4,51),FNAME(5,51)
     *,FNAME(6,51) / 'G','B','R','1', 2*' '/
      data FNAME(1,52),FNAME(2,52), FNAME(3,52), FNAME(4,52),FNAME(5,52)
     *,FNAME(6,52) / 'G','B','R','2', 2*' '/
      data FNAME(1,53),FNAME(2,53), FNAME(3,53), FNAME(4,53),FNAME(5,53)
     *,FNAME(6,53) / 'G','M','F','P', 2*' '/
      data FNAME(1,54),FNAME(2,54), FNAME(3,54), FNAME(4,54),FNAME(5,54)
     *,FNAME(6,54) / 'P','A','I','R','D','R'/
      data FNAME(1,55),FNAME(2,55), FNAME(3,55), FNAME(4,55),FNAME(5,55)
     *,FNAME(6,55) / 'P','A','I','R','F','R'/
      data FNAME(1,56),FNAME(2,56), FNAME(3,56), FNAME(4,56),FNAME(5,56)
     *,FNAME(6,56) / 'P','A','I','R','D','Z'/
      data FNAME(1,57),FNAME(2,57), FNAME(3,57), FNAME(4,57),FNAME(5,57)
     *,FNAME(6,57) / 'P','A','I','R','F','Z'/
      data FNAME(1,58),FNAME(2,58), FNAME(3,58), FNAME(4,58),FNAME(5,58)
     *,FNAME(6,58) / 'P','A','I','R','R','M'/
      data FNAME(1,59),FNAME(2,59), FNAME(3,59), FNAME(4,59),FNAME(5,59)
     *,FNAME(6,59) / 'P','A','I','R','R','R'/
      data FNAME(1,60),FNAME(2,60), FNAME(3,60), FNAME(4,60),FNAME(5,60)
     *,FNAME(6,60) / 'P','A','I','R','R','Z'/
      data FNAME(1,61),FNAME(2,61), FNAME(3,61), FNAME(4,61),FNAME(5,61)
     *,FNAME(6,61) / 'P','A','I','R','T','E'/
      data FNAME(1,62),FNAME(2,62), FNAME(3,62), FNAME(4,62),FNAME(5,62)
     *,FNAME(6,62) / 'P','A','I','R','T','M'/
      data FNAME(1,63),FNAME(2,63), FNAME(3,63), FNAME(4,63),FNAME(5,63)
     *,FNAME(6,63) / 'P','A','I','R','T','R'/
      data FNAME(1,64),FNAME(2,64), FNAME(3,64), FNAME(4,64),FNAME(5,64)
     *,FNAME(6,64) / 'P','A','I','R','T','U'/
      data FNAME(1,65),FNAME(2,65), FNAME(3,65), FNAME(4,65),FNAME(5,65)
     *,FNAME(6,65) / 'P','A','I','R','T','Z'/
      data FNAME(1,66),FNAME(2,66), FNAME(3,66), FNAME(4,66),FNAME(5,66)
     *,FNAME(6,66) / 'P','B','R','1', 2*' '/
      data FNAME(1,67),FNAME(2,67), FNAME(3,67), FNAME(4,67),FNAME(5,67)
     *,FNAME(6,67) / 'P','B','R','2', 2*' '/
      data FNAME(1,68),FNAME(2,68), FNAME(3,68), FNAME(4,68),FNAME(5,68)
     *,FNAME(6,68) / 'P','D','E','D','X', 1*' '/
      data FNAME(1,69),FNAME(2,69), FNAME(3,69), FNAME(4,69),FNAME(5,69)
     *,FNAME(6,69) / 'P','H','O','T','T','Z'/
      data FNAME(1,70),FNAME(2,70), FNAME(3,70), FNAME(4,70),FNAME(5,70)
     *,FNAME(6,70) / 'P','H','O','T','T','E'/
      data FNAME(1,71),FNAME(2,71), FNAME(3,71), FNAME(4,71),FNAME(5,71)
     *,FNAME(6,71) / 'P','S','I','G', 2*' '/
      data FNAME(1,72),FNAME(2,72), FNAME(3,72), FNAME(4,72),FNAME(5,72)
     *,FNAME(6,72) / 'S','P','I','O','N','E'/
      data FNAME(1,73),FNAME(2,73), FNAME(3,73), FNAME(4,73),FNAME(5,73)
     *,FNAME(6,73) / 'S','P','I','O','N','P'/
      data FNAME(1,74),FNAME(2,74), FNAME(3,74), FNAME(4,74),FNAME(5,74)
     *,FNAME(6,74) / 'S','P','T','O','T','E'/
      data FNAME(1,75),FNAME(2,75), FNAME(3,75), FNAME(4,75),FNAME(5,75)
     *,FNAME(6,75) / 'S','P','T','O','T','P'/
      data FNAME(1,76),FNAME(2,76), FNAME(3,76), FNAME(4,76),FNAME(5,76)
     *,FNAME(6,76) / 'T','M','X','B', 2*' '/
      data FNAME(1,77),FNAME(2,77), FNAME(3,77), FNAME(4,77),FNAME(5,77)
     *,FNAME(6,77) / 'T','M','X','S', 2*' '/
      data FNAME(1,78),FNAME(2,78), FNAME(3,78), FNAME(4,78),FNAME(5,78)
     *,FNAME(6,78) / 'T','M','X','D','E','2'/
      data FNAME(1,79),FNAME(2,79), FNAME(3,79), FNAME(4,79),FNAME(5,79)
     *,FNAME(6,79) / 'X','S','I','F', 2*' '/
      data IBOUND/0/
      data INCOH/0/
      data ICPROF/0/
      data GASP/0.0/
      data IRAYL/0/
      data IMPACT/0/
      data IUNRST/0/
      data efracH/5.d-2/
      data efracL/2.d-1/
      data nleg0/1000/
      data fudgeMS/1/
      data BMIN/4.5/,MSTEPS/16/,JRMAX/200/, FSTEP/1.,2.,3.,4.,6.,8.,10.,
     *15.,20.,30.,40.,60.,80.,100.,150.,200./
      data EPSTFL/0/,IEPST/1/,IAPRIM/1/,IAPRFL/0/
      data                                              ! common/DCSSTR/
     * NSDCS/0/                           ! Number of stored media DCS's
      end

      double precision function photte(K)
      implicit none
      integer i
      double precision phottz
      double precision K
      include 'pegscommons/mixdat.f'
      include 'pegscommons/mxdatc.f'
      photte=0.0
      do i=1,NE
        photte=PHOTTE+PZ(i)*PHOTTZ(Z(i),K)
      end do
      return
      end

      double precision function phottz(Z,K)
      implicit none
      integer IZ
      double precision PCON, Z, AINTP
      double precision K
      include 'pegscommons/phpair.f'
      include 'pegscommons/pmcons.f'
      include 'pegscommons/molvar.f'
      PCON=1.D-24*(AN*RHO/WM)*RLC
      IZ=Z
      phottz=PCON*AINTP(K,PHE(1,IZ),NPHE(IZ),PHD(1,IZ),1,.TRUE.,.TRUE.)
      return
      end

      subroutine plot(IFUN,XP,IV,EL,EH,NPT,IDF)
      implicit none
      integer IXTABF, NMAX, NUPL, itab, ixt1, ixt2, NU, IDFI, IDF, NA,
     & IFUN, ip, IV, ja, ia, i, IBIN, J, NPT
      double precision DFL, FI, EL, DFH, EH, BDF, YMAX, X, DY
      include 'pegscommons/funcs.f'
      include 'pegscommons/funcsc.f'
      include 'pegscommons/molvar.f'
      include 'pegscommons/dercon.f'
      include 'pegscommons/epstar.f'
      character*4 PBUF(101),ID(5),ORDNL(3,4),ICOM,IRPAR,ICOL,IX,IBL
      double precision XTAB(200),XTABA(18),YSAV(200),XP(4),XQ(5)
      data XTABA/1. ,1.25 ,1.5 ,1.75 ,2. ,2.5 ,3. ,3.5 ,4. ,4.5,4.9488 ,
     *5. ,5.5 ,6. ,7. ,8. ,9. ,10./
      data IXTABF/0/
      data ICOM/','/,IRPAR/')'/,ICOL/':'/,ORDNL/'1','S','T','2','N','D',
     *'3','R','D','4','T','H'/
      data PBUF/'I',100*' '/,NMAX/200/,NUPL/  26/,IX/'X'/,IBL/' '/
      if (IXTABF.eq.0) then
        itab= 0
        do ixt1=0,6
          do ixt2=1,17
            if (ixt2.ne.11) then
              itab=itab+ 1
              XTAB(itab)=XTABA(ixt2)*10.**(ixt1-3)
            else if (ixt2.eq.11 .and. ixt1.eq.4) then
              itab=itab+ 1
              XTAB(itab)=XTABA(ixt2)*10.**(ixt1-3)
            end if
          end do
        end do
        itab=itab+1
        XTAB(itab)=XTABA(18)*10.**(6-3)
        IXTABF=1
        NU=ITAB
      end if
      IDFI=IDF+1
      NA=NFARG(IFUN)
      DFL=FI(IDF,EL,0.d0,0.d0,0.d0)
      DFH=FI(IDF,EH,0.d0,0.d0,0.d0)
      BDF=(DFH-DFL)/FLOAT(NU-1)
      YMAX=0.0
      do ip=1,NU
        X=XTAB(ip)+RM
        XP(IV)=X
        YSAV(ip)=FI(IFUN,XP(1),XP(2),XP(3),XP(4))/(RLC*RHO)
        YMAX=DMAX1(YSAV(ip),YMAX)
      end do
      DY=YMAX/100.
      ja=0
      do ia=1,NA
        ja=ja+1
        if (ia.ne.IV) then
          XQ(ja)=XP(ia)
          ID(ja)=ICOM
        else
          XQ(ja)=EL
          ID(ja)=ICOL
          ja=ja+1
          XQ(ja)=EH
          ID(ja)=ICOM
        end if
      end do
      ID(ja)=IRPAR
      write(NUPL,100) (FNAME(i,IFUN),i=1,6),(XQ(i),ID(i),i=1,JA)
100   format (' Plot of function ',6A1,'(',5(1P,G15.6,1X,A1) )
      write(NUPL,110) (ORDNL(i,IV),i=1,3),NU,EL,EH,
     *(FNAME(i,IDF),i=1,6),(FNAME(i,IDFI),i=1,6),DY
110   format (' The ',3A1,' argument is chosen at ',I4, ' points from ',
     *1P,G15.6, ' to ', 1P,G15.6/' using distribution function ',6A1,' a
     *nd inverse ', 'distribution function ',6A1,'.  EACH X=',1P,G15.6/
     *'0    X(OR E)    Y1')
      write(NUPL,120) RLC,RHO
120   format (/' ***Changed version of pegs which has divided the values
     * by', ' RLC*RHO to get to MeV/g/cm**2'/'  RLC=',E12.4,'  RHO=',E12
     *.4)
      do ip=1,NU
        X=XTAB(ip)
        if (DY.ne.0.0) then
          IBIN=YSAV(ip)/DY+1.0
        else
          IBIN=1
        end if
        if (IBIN.ge.2) PBUF(IBIN)=IX
        write(NUPL,130) IP,X,YSAV(ip),(PBUF(j),j=1,IBIN)
130     format (1X,I3,1P,2G13.6,1X,101A1)
        if (IBIN.ge.2) PBUF(IBIN)=IBL
      end do
      write(21,*) NU
      write(21,140) (XTAB(ip),ip=1,NU)
      write(21,*) NU
      write(21,140) (YSAV(ip),ip=1,NU)
140   FORMAT(5(1P,E15.7))
      return
      end

      subroutine PLOT1(IFUN,XP,IV,EL,EH,NPT,IDF)
      implicit none
      integer NMAX, NUPL, NU, NPT, IDFI, IDF, NA, IFUN, ip, i, IV, ja,
     & ia, IBIN, J
      double precision DFL, FI, EL, DFH, EH, BDF, YMAX, DF, X, DY
      include 'pegscommons/funcs.f'
      include 'pegscommons/funcsc.f'
      character*4 PBUF(101),ID(5),ORDNL(3,4),ICOM,IRPAR,ICOL,IX,IBL
      double precision YSAV(200),XP(4),XQ(5)
      data ICOM/','/,IRPAR/')'/,ICOL/':'/,ORDNL/'1','S','T','2','N','D',
     *'3','R','D','4','T','H'/
      data PBUF/'I',100*' '/,NMAX/200/,NUPL/6/,IX/'X'/,IBL/' '/
      NU=MIN0(NPT,NMAX)
      IDFI=IDF+1
      NA=NFARG(IFUN)
      DFL=FI(IDF,EL,0.d0,0.d0,0.d0)
      DFH=FI(IDF,EH,0.d0,0.d0,0.d0)
      BDF=(DFH-DFL)/FLOAT(NU-1)
      YMAX=0.0
      do ip=1,NU
        i=ip-1
        DF=DFL+BDF*FLOAT(i)
        X=FI(IDFI,DF,0.d0,0.d0,0.d0)
        XP(IV)=X
        YSAV(ip)=FI(IFUN,XP(1),XP(2),XP(3),XP(4))
        YMAX=DMAX1(YSAV(ip),YMAX)
      end do
      DY=YMAX/100.
      ja=0
      do ia=1,NA
        ja=ja+1
        if (ia.ne.IV) then
          XQ(ja)=XP(ia)
          ID(ja)=ICOM
        else
          XQ(ja)=EL
          ID(ja)=ICOL
          ja=ja+1
          XQ(ja)=EH
          ID(ja)=ICOM
        end if
      end do
      ID(ja)=IRPAR
      write(NUPL,100) (FNAME(i,IFUN),i=1,6),(XQ(i),ID(i),i=1,ja)
100   format (' Plot of function ',6A1,'(',5(1P,G15.6,1X,A1) )
      write(NUPL,110) (ORDNL(i,IV),i=1,3),NU,EL,EH, (FNAME(i,IDF),
     *i=1,6),(FNAME(i,IDFI),i=1,6),DY
110   format (' The ',3A1,' argument is chosen at ',I4, ' points from ',
     *1P,G15.6, ' to ', 1P,G15.6/' using distribution function ',6A1,' a
     *nd inverse ', 'distribution function ',6A1,'.  Each X=',1P,G15.6/
     *'0    X(or E)    Y1')
      do ip=1,NU
        i=ip-1
        X=FI(IDFI,DFL+BDF*FLOAT(i),0.d0,0.d0,0.d0)
        if (DY.ne.0.0) then
          IBIN=YSAV(ip)/DY+1.0
        else
          IBIN=1
        end if
        if (IBIN.ge.2) PBUF(IBIN)=IX
        write(NUPL,120) ip,X,YSAV(ip),(PBUF(j),j=1,IBIN)
120     format (1X,I3,1P,2G13.6,1X,101A1)
        if (IBIN.ge.2) PBUF(IBIN)=IBL
      end do
      return
      end

      subroutine PMDCON
      implicit none
      double precision FSCI
      include 'pegscommons/pmcons.f'
      include 'pegscommons/dercon.f'
      PI=3.1415926535897932D+0
      C=2.99792458D+10
      RME=9.10938188D-28
      HBAR=1.054571596E-27
      ECGS=4.8032068D-10
      EMKS=1.602176462D-19
      AN=6.02214199D+23
      RADDEG=180./PI
      FSC = ECGS**2/(HBAR*C)
      FSCI=1./FSC
      ERGMEV = (1.D+6)*(EMKS*1.D+7)
      R0 = (ECGS**2)/(RME*C**2)
      RM = RME*C**2/ERGMEV
      RMT2 = RM*2.0
      RMSQ = RM*RM
      A22P9 = RADDEG*DSQRT(4.*PI*AN)*ECGS**2/ERGMEV
      A6680 = 4.0*PI*AN*(HBAR/(RME*C))**2*(0.885**2/(1.167*1.13))
      return
      end

      double precision function PSIG(E)
      implicit none
      double precision BREMTM, E, BHABTM, ANIHTM
      PSIG=BREMTM(E)+BHABTM(E)+ANIHTM(E)
      return
      end

      subroutine PWLF1(NI,NIMX,XL,XU,XR,EP,ZTHR,ZEP,NIP,XFUN,XFI, AX,BX,
     *NALM,NFUN,AF,BF,VFUNS)
      implicit none
      integer NALM, NFUN, NL, NU, IPRN, NJ, NIMX, NIP, NI, NK
      double precision XL, XU, XR, EP, ZTHR, ZEP, REM, AX, BX, AF, BF
      external XFUN,XFI,VFUNS
      dimension AF(NALM,NFUN),BF(NALM,NFUN),ZTHR(NFUN),ZEP(NFUN)
      logical QFIT
      NL=0
      NU=1
      IPRN=0
100   continue
        NJ=MIN0(NU,NIMX)
        if (QFIT(NJ,XL,XU,XR,EP,ZTHR,ZEP,REM,NIP,XFUN,XFI, AX,BX,NALM,NF
     *  UN,AF,BF,VFUNS,0)) go to 120
        if (NU.ge.NIMX) then
          write(  26,110) NIMX,EP
110       format(' Number of allocated intervals(=',I5,') was insufficie
     *nt' ,/ ,' to get maximum relative error less than ',1P,G14.6)
          NI=NJ
          return
        end if
        NL=NU
        NU=NU*2
      go to 100
120   continue
      NU=NJ
130   if (NU.le.NL+1) go to 140
        NJ=(NL+NU)/2
        NK=NJ
        if (QFIT(NJ,XL,XU,XR,EP,ZTHR,ZEP,REM,NIP,XFUN,XFI, AX,BX,NALM,
     *  NFUN,AF,BF,VFUNS,0)) then
          NU=NJ
        else
          NL=NK
        end if
      go to 130
140   continue
      NI=NU
      if (NI.eq.NJ) return
      if (.not.QFIT(NI,XL,XU,XR,EP,ZTHR,ZEP,REM,NIP,XFUN,XFI, AX,BX,NALM
     *,NFUN,AF,BF,VFUNS,0)) write(  26,150) NI
150   format(' Catastrophe---does not fit when it should,NI=',I5)
      return
      end

      double precision function QD(F,A,B,MSG)
      implicit none
      integer IER
      double precision A, B
      external F
      double precision DCADRE,ADUM,BDUM,ERRDUM
      character*6 MSG
      ADUM=A
      BDUM=B
      QD=DCADRE(F,ADUM,BDUM,1.D-16,1.D-5,ERRDUM,IER)
      if (IER.gt.66) then
        write(10,100) IER,MSG,A,B,QD,ERRDUM
100     formaT (' DCADRE code=',I4,' for integral ',A6,' from ',1P,G14.6
     *  ,' to ',G14.6, ',QD=',G14.6,'+-',G14.6)
      end if
      return
      end

      logical function QFIT(NJ,XL,XH,XR,EP,ZTHR,ZEP,REM,NJP,XFUN,XFI, AX
     *,BX,NALM,NFUN,AF,BF,VFUNS,IPRN)
      implicit none
      integer NALM, NFUN, NKP, NI, NJ, NIP, NJP, isub, IPRN, ifun,
     & JSUB, ip
      double precision XH, XL, XS, XR, XFL, XFUN, XFH, XFS, XM, DX, W,
     & XLL, AX, BX, REM, SXFL, XSXF, XFI, FSXL, SXFH, FSXH, DSXF, AF, BF
     & , WIP, SXFIP, XIP, FIP, FFIP, AFIP, AER, RE, ZTHR, ZEP, EP, EPS1
      external XFUN,XFI,VFUNS
      dimension FSXL(79),FSXH(79),FIP(79),FFIP(79),AFIP(79)
      dimension RE(79),AER(79)
      dimension AF(NALM,NFUN),BF(NALM,NFUN),ZTHR(NFUN),ZEP(NFUN)
      data EPS1/1.d-15/
      data NKP/3/
      if (XH.le.XL) then
        write(  26,100) XL,XH
100     format(' QFIT error:XL should be < XH. XL,XH=',2G14.6)
        QFIT=.false.
        return
      end if
      XS=DMAX1(XL,DMIN1(XH,XR))
      NI=NJ-2
      if (((XS.eq.XL.or.XS.eq.XH).and.NI.ge.1).or.NI.ge.2) then
        XFL=XFUN(XL)
      else
        QFIT=.false.
        return
      end if
      XFH=XFUN(XH)
      XFS=XFUN(XS)
      XM=DMAX1(XFH-XFS,XFS-XFL)
      DX=XFH-XFL
! patch to eliminate g77 optimization level dependence -- YN
!      W=XM/DMAX1(1.d0,AINT(NI*XM/DX))
      IF(DABS(XM-DX).LE.EPS1*DX) THEN
        W=XM/DMAX1(1.d0,AINT(NI*1.d0))
      else
        W=XM/DMAX1(1.d0,AINT(NI*XM/DX))
      end if

      NI=NI-AINT(NI-DX/W)
      NIP=MAX0(NKP,(NJP+NI-1)/NI)
      NIP=(NIP/2)*2+1
      if (XFH-XFS.le.XFS-XFL) then
        XLL=XFL
      else
        XLL=XFH-NI*W
      end if
      AX=1./W
      BX=2.-XLL*AX
      REM=0.0
      QFIT=.true.
      SXFL=DMAX1(XLL,XFL)
      ISUB=0
      XSXF=XFI(SXFL)
      call VFUNS(XSXF,FSXL)
      if (IPRN.ne.0) WRITE(  26,110) isub,SXFL,XSXF, (FSXL(IFUN),IFUN=1
     *,NFUN)
110   format(' QFIT:ISUB,SXF,XSXF,FSX()=',I4,1P,9G11.4/(1X,12G11.4))
      do isub=1,NI
        JSUB=isub+1
        SXFH=DMIN1(XLL+W*isub,XH)
        XSXF=XFI(SXFH)
        call VFUNS(XSXF,FSXH)
        if (IPRN.ne.0) write(  26,110) isub,SXFH,XSXF, (FSXH(ifun),
     *   ifun=1,NFUN)
        DSXF=SXFH-SXFL
        do ifun=1,NFUN
          AF(JSUB,ifun)=(FSXH(ifun)-FSXL(ifun))/DSXF
          BF(JSUB,ifun)=(FSXL(ifun)*SXFH-FSXH(ifun)*SXFL)/DSXF
        end do
        WIP=DSXF/(NIP+1)
        do ip=1,NIP
          SXFIP=SXFL+ip*WIP
          XIP=XFI(SXFIP)
          call VFUNS(XIP,FIP)
          do ifun=1,NFUN
            FFIP(ifun)=AF(JSUB,ifun)*SXFIP+BF(JSUB,ifun)
            AFIP(ifun)=dabs(FIP(ifun))
            AER(ifun)=dabs(FFIP(ifun)-FIP(ifun))
            RE(ifun)=0.0
            if (FIP(ifun).ne.0.0) then
              RE(ifun)=AER(ifun)/AFIP(ifun)
            end if
            if (AFIP(ifun).ge.ZTHR(ifun)) then
              REM=dmax1(REM,RE(ifun))
            else if (AER(ifun).gt.ZEP(ifun)) then
              QFIT=.false.
            end if
          end do
          if (IPRN.ne.0) then
            write(  26,120) ISUB,ip,SXFIP,XIP,REM,QFIT,(FIP(ifun),
     *      FFIP(ifun), RE(ifun),AER(ifun),ifun=1,NFUN)
120         format(1X,2I4,1P,2G12.5,6P,F12.0,L2,1P,2G11.4,6P,F11.0,1P,G1
     *      1.4/ (1X,3(1P,2G11.4,6P,F11.0,1P,G11.4)))
          end if
        end do
        SXFL=SXFH
        do ifun=1,NFUN
          FSXL(ifun)=FSXH(ifun)
        end do
      end do
      do ifun=1,NFUN
        AF(1,ifun)=AF(2,ifun)
        BF(1,ifun)=BF(2,ifun)
        AF(NI+2,ifun)=AF(NI+1,ifun)
        BF(NI+2,ifun)=BF(NI+1,ifun)
      end do
      QFIT=QFIT.AND.REM.LE.EP
      NJ=NI+2
      return
      end

      subroutine RDSCPR
      implicit none
      integer MXRAW, MXSHEL, NSHELL, i
      double precision ELECNI, CAPIN, SCPROF, QCAP, PZT
      include 'pegscommons/mixdat.f'
      include 'pegscommons/mxdatc.f'
      character FILENM*60,NOZ*3
      dimension ELECNI(200),NSHELL(200),CAPIN(200),SCPROF(31,200), QCAP(
     *31)
      NAMELIST/SCPRDT/MXRAW,MXSHEL,ELECNI,NSHELL,CAPIN,SCPROF,QCAP
      do i=1,NE
        WRITE(NOZ,'(I3.3)') NINT(Z(i))
        FILENM='data/shellwise_Compton_profile/z'//NOZ//'.dat'
        write(  26,100) FILENM
100     format(' Reading ',A50)
        open(UNIT=30,FILE=FILENM,STATUS='old')
        PZT=PZ(i)
        read(30,SCPRDT)
        call ADSCPR(MXRAW,MXSHEL,ELECNI,NSHELL,CAPIN,SCPROF,QCAP,PZT)
        close(30)
      end do
      call WTSCPR
      return
      end

      subroutine RFUNS(E,V)
      implicit none
      double precision AINTP, E
      include 'pegscommons/cohcom.f'
      double PRECISION V(1)
      V(1)=AINTP(E,AFFI(1),97,XVAL(1),1,.TRUE.,.TRUE.)
      return
      end

      subroutine RFUNS2(E,V)
      implicit none
      double precision AINTP, E
      include 'pegscommons/cohcom.f'
      double precision V(1)
      V(1)=AINTP(E,XVAL(1),97,AFAC2(1),1,.TRUE.,.TRUE.)
      return
      end

      subroutine SFUNS(E,V)
      implicit none
      double precision AINTP, E
      include 'pegscommons/bcom.f'
      include 'pegscommons/sfcom.f'
      double precision V(1)
      V(1)=AINTP(E,XSVAL(1),41,SCATZ(1),1,.TRUE.,.TRUE.)
      return
      end

      subroutine SPINIT
      implicit none
!     Use Revised Sternheimer Density Effects Coeeficients
!     Atomic Data Nuclear Data Tables 30, 261(1984) by 
!     R. M. Sternheimer et al.
      integer im, j, IZ, ie, i, ICHECK, IESPEL, IPEGEL
      double precision VPLASM, ALIADG, DEXP, EDENL, ALGASP, 
     & EPSTRH, TLRNCE, EPSTWT, V4110, V4130, V4150, V4170, V4190, 
     & V4210, V4230, V4240, V4250, V4270
      include 'pegscommons/pmcons.f'
      include 'pegscommons/spcomm.f'
      include 'pegscommons/spcomc.f'
      include 'pegscommons/dercon.f'
      include 'pegscommons/molvar.f'
      include 'pegscommons/mixdat.f'
      include 'pegscommons/mxdatc.f'
      include 'pegscommons/elemtb.f'
      include 'pegscommons/elmtbc.f'
      include 'pegscommons/lspion.f'
      include 'pegscommons/epstar.f'
      include 'pegscommons/thres2.f'
      double precision IMEV
      TOLN10=2.0*DLOG(10.d0)
      IM=-100
      if (EPSTFL .lt. 0 .or. EPSTFL .gt. 1) then
        EPSTFL = 0
      end if
      write(26,9) EPSTFL
9     format(' EPSTFL=',I15)
      if (EPSTFL.eq.0) then
        if (ISSB.ne.0) then
          if ( AFACT.eq.0.0 .or. CBAR.le.0.0 .or. SK.eq.0.0 .or. X0.eq.
     *    0.0 .or. X1.eq.0.0 .or. IEV.eq.0.0 ) then
            write(  26,100)
100         format(//' *****User error -not all density effect paramters
     * input', '   code stopped in SPINIT****'//)
            close(26)
            stop
          end if
          IMEV=IEV*1.D-6
          VPLASM=DSQRT(EDEN*R0*C**2/PI)
          IM=-1
        else
          if (ISSB.eq.0.and.(AFACT.ne.0.0.or.CBAR.ne.0.0.or.SK.ne.0.0.
     *    or.X0.ne.0.0.or.X1.ne.0.0.or.IEV.ne.0.0)) then
            write(  26,110)
110         format(//,' Stopped in SPINIT: incorrect user-override of SS
     *B-DATA')
            close(26)
            stop
          end if
          DO 120 im=1,NMED
            do j=1,LMED
              if (IDSTRN(j).ne.MEDTBL(j,im)) go to 120
            end do
!           Calculation follows if a match is found
            AFACT=STDATA(1,im)
            SK=STDATA(2,im)
            X0=STDATA(3,im)
            X1=STDATA(4,im)
            IEV=STDATA(5,im)
            CBAR=STDATA(6,im)
!           Define DELATA0
            DELTA0=STDATA(7,im)
            IMEV=IEV*1.0D-6
            VPLASM=DSQRT(EDEN*R0*C**2/PI)
            go to 150
120       continue
!      Sternheimer-Peierls (S-P) general formula section
          DELTA0=0.0
          IM=0
          if (NE.eq.1) then
            IZ=Z(1)
            if (IZ.eq.1.or.IZ.eq.7.or.IZ.eq.8) then
              write(  26,130)
130           format(' Stopped in subroutine SPINIT because this',/, ' e
     *lement (H, N, OR O) can only exist as a diatomic molecule.',/, ' R
     *EMEDY:  use comp option for H2, N2, OR O2 with NE=2,PZ=1,1'/, '
     *       and, in the case of a gas, define Sternheimer ID',/, '
     *     (I.E., IDSTRN) like H2-GAS')
              close(26)
              stop
            end if
            IEV=ITBL(IZ)
          else
            ALIADG=0.0
            do ie=1,NE
              IZ=Z(ie)
              if (IZ.eq.1) then
                IEV=19.2
              else if (IZ.eq.6) then
                if (GASP.eq.0.0) then
                  IEV=81.0
                else
                  IEV=70.0
                end if
              else if (IZ.eq.7) then
                IEV=82.0
              else if (IZ.eq.8) then
                if (GASP.eq.0.0) then
                  IEV=106.0
                else
                  IEV=97.0
                end if
              else if (IZ.eq.9) then
                IEV=112.0
              else if (IZ.eq.17) then
                IEV=180.0
              else
                IEV=1.13*ITBL(IZ)
              end if
              ALIADG=ALIADG + PZ(IE)*Z(IE)*DLOG(IEV)
            end do
            ALIADG=ALIADG/ZC
            IEV=DEXP(ALIADG)
          end if
          IMEV=IEV*1.0D-6
          if (GASP.eq.0.0) then
            EDENL=EDEN
          else
            EDENL=EDEN/GASP
          end if
          VPLASM = DSQRT(EDENL*R0*C**2/PI)
          CBAR=1. + 2.*DLOG(IMEV/(HBAR*2*PI*VPLASM/ERGMEV))
          if (NE.eq.1.and.IDINT(Z(1)).eq.2.and.GASP.ne.0.0) then
            X0=2.191
            X1=3.0
            SK=3.297
          else if
     *      (NE.eq.2.and.IDINT(Z(1)).eq.1.and.IDINT(Z(2)).eq.1) then
            if (GASP.eq.0.0) then
              X0=0.425
              X1=2.0
              SK=5.949
            else
              X0=1.837
              X1=3.0
              SK=4.754
            end if
          else
            SK=3.0
            if (GASP.eq.0.0) then
              if (IEV.lt.100.0) then
                if (CBAR.lt.3.681) then
                  X0=0.2
                  X1=2.0
                else
                  X0=0.326*CBAR - 1.0
                  X1=2.0
                end if
              else
                if (CBAR.lt.5.215) then
                  X0=0.2
                  X1=3.0
                else
                  X0=0.326*CBAR - 1.5
                  X1=3.0
                end if
              end if
              if (X0.ge.X1) then
                write(  26,140) X0,X1,CBAR
140             format(' Stopped in SPINIT due to X0.ge.X1 , X0,X1,CBAR=
     *',3G15.5,/ ,' If this is gas, you must define GASP(ATM)')
                close(26)
                stop
              end if
            else
              if (CBAR.lt.10.0) then
                X0=1.6
                X1=4.0
              else if (CBAR.lt.10.5) then
                X0=1.7
                X1=4.0
              else if (CBAR.lt.11.0) then
                X0=1.8
                X1=4.0
              else if (CBAR.lt.11.5) then
                X0=1.9
                X1=4.0
              else if (CBAR.lt.12.25) then
                X0=2.0
                X1=4.0
              else if (CBAR.lt.13.804) then
                X0=2.0
                X1=5.0
              else
                X0=0.326*CBAR - 2.5
                X1=5.0
              end if
            end if
          end if
        end if
150     if (GASP.ne.0.0) then
          ALGASP=DLOG(GASP)
          CBAR=CBAR - ALGASP
          X0=X0 - ALGASP/TOLN10
          X1=X1 - ALGASP/TOLN10
        end if
        if (IM.eq.0) then
          AFACT=(CBAR - TOLN10*X0)/(X1 - X0)**SK
        end if
      else
        read(20,160,ERR=9991) EPSTTL
160     format(80A1)
        read(20,*,ERR=9991) NEPST,IEV,EPSTRH,NELEPS,(ZEPST(i),
     *          WEPST(i),i=1,NELEPS)
        read(20,*,ERR=9991) (EPSTEN(i),EPSTD(i),i=1,NEPST)
        go to 9993
9991    write(26,9992)
9992    format(/,/,' *****END-OF-FILE on epstar.dat ')
        close(26)
        stop
9993    if (NEPST.gt.150) then
          write(  26,170) NEPST
170       format(//' *****NEPST=',I4,' is greater than the 150 allowed')
          close(26)
          stop
        end if
        do i=1,NEPST
          EPSTEN(i) = EPSTEN(i) + RM
        end do
        IMEV = IEV*1.D-06
        if ( AE .lt. EPSTEN(1)) then
          write(  26,180) EPSTEN(1),AE
180       format(//' ****Lowest energy input for density effect is',1P,E
     *    10.3/ T20,'which is higher than the value of AE=',1P,E10.3,' M
     *eV'/ ' ***It has been set to AE***'//)
          EPSTEN(1) = AE
        end if
        if ( UE .gt. EPSTEN(NEPST)) then
          write(  26,190) EPSTEN(NEPST),UE
190       format(//' ****Highest energy input for density effect is',1P,
     *    E10.3/ T20,'which is lower than the value of UE=',1P,E10.3,' M
     *eV'/ ' ***It has been set to UE***'//)
          EPSTEN(NEPST) = UE
        end if
        ICHECK=0
        TLRNCE=0.01
        if (NELEPS.ne.NE) ICHECK=1
        if ((ICHECK.eq.0) .and. ( (EPSTRH.lt.((1.0-TLRNCE)*RHO)) .or. (E
     *  PSTRH.GT.((1.0+TLRNCE)*RHO)) )) ICHECK=1
        EPSTWT = 0.0
        do i=1,NE
          EPSTWT = EPSTWT + RHOZ(i)
        end do
        if (EPSTWT.eq.0.0) then
          write(  26,200)
200       format(//' *****In SPINIT***something wrong, molecular weight
     *of', 'molecule is zero (I.E. sum of RHOZ)***'//)
        end if
        if (ICHECK.eq.0) then
          IESPEL=0
          ICHECK=1
210       continue
            IESPEL=IESPEL+1
            IPEGEL=0
220         continue
              IPEGEL=IPEGEL+1
              if (DINT(Z(IPEGEL)).eq.ZEPST(IESPEL)) then
                ICHECK=0
                go to 230
              end if
              if (IPEGEL.ge.NE) go to 230
            go to 220
230         continue
            if ((ICHECK.eq.0)  .and. ( (WEPST(IESPEL).lt.((1.0-TLRNCE)*R
     *      HOZ(IPEGEL)/EPSTWT)) .or. (WEPST(IESPEL).gt.((1.0+TLRNCE)*RH
     *      OZ(IPEGEL)/EPSTWT)) )) ICHECK=1
            if (IESPEL.GE.NELEPS) go to 240
          go to 210
240       continue
        end if
        if (ICHECK.eq.1) then
          write(  26,250)
250       format(////' *** Composition in input density file does not ma
     *tch ', ' that being used by pegs'//' ***** Quitting early***'////)
          close(26)
          stop
        end if
      end if
      SPC1=2.*PI*R0**2*RM*EDEN*RLC
      SPC2=DLOG((IMEV/RM)**2/2.0)
      write(  26,260)
260   format(//' Parameters computed in SPINIT.'//1X,64('-'))
      if (IM.eq.0) then
        write(  26,270)
270     format(' Sternheimer-Peierls general formula used for the densit
     *y effect,')
      else if (IM.gt.0) then
        write(  26,280)
280     format(' Sternheimer-Seltzer-Berger table used for density effec
     *t')
      else if (IM .eq. -1) then
        write(  26,290)
290     format(' Sternheimer-Seltzer-Berger density effect data supplied
     * by user')
      else
        write(  26,300) EPSTTL
300     format(' Density effect read in directly:'/T10,80A1)
      end if
      write(  26,310)
310   format(1X,64('-')/)
      write(  26,320) IEV
320   format(/' Adjusted mean ionization = ',F8.2,' eV'/1X,38('-')//)
      if (EPSTFL .eq. 0) then
        V4110=IEV
        write(  26,330) V4110
330     format(' IEV=',1P,G15.7)
        V4130=VPLASM
        write(  26,340) V4130
340     format(' VPLASM=',1P,G15.7)
        V4150=CBAR
        write(  26,350) V4150
350     format(' CBAR=',1P,G15.7)
        V4170=X0
        write(  26,360) V4170
360     format(' X0=',1P,G15.7)
        V4190=X1
        write(  26,370) V4190
370     format(' X1=',1P,G15.7)
        V4210=SK
        write(  26,380) V4210
380     format(' SK=',1P,G15.7)
        V4230=AFACT
        write(  26,390) V4230
390     format(' AFACT=',1P,G15.7)
        V4240=DELTA0
        WRITE(  26,400)V4240
400     FORMAT(' DELTA0=',1P,G15.7)
      end if
      V4250=SPC1
      write(  26,410) V4250
410   format(' SPC1=',1P,G15.7)
      V4270=SPC2
      write(  26,420) V4270
420   format(' SPC2=',1P,G15.7)
      return
      end

      double precision function SPIONB(E0,EE,POSITR)
!     Use Revised Sternheimer Density Effects Coeeficients
!     Atomic Data Nuclear Data Tables 30, 261(1984) by 
!     R. M. Sternheimer et al.
!     Use DELTA0 
      implicit none
      integer i
      double precision G, E0, EEM, EE, T, ETA2, BETA2, ALETA2, DLOG,
     & X, D, FTERM, TP2, D2, D3, D4, DELTA
      logical POSITR
      include 'pegscommons/dercon.f'
      include 'pegscommons/lspion.f'
      include 'pegscommons/epstar.f'
      G=E0/RM
      EEM=EE/RM-1.
      T=G-1
      ETA2=T*(G+1.)
      BETA2=ETA2/G**2
      ALETA2=DLOG(ETA2)
      X=0.21715*ALETA2
      if (.NOT.POSITR) then
        D=DMIN1(EEM,0.5*T)
        FTERM=-1.-BETA2+DLOG((T-D)*D)+T/(T-D) +(D*D/2.+(2.*T+1.)*DLOG(1.
     *  -D/T))/(G*G)
      else
        D=DMIN1(EEM,T)
        TP2=T+2.
        D2=D*D
        D3=D*D2
        D4=D*D3
        FTERM=DLOG(T*D)-(BETA2/T)*( T + 2.*D - (3.*D2/2.)/TP2 -(D-D3/3.)
     *  /(TP2*TP2)-(D2/2.-T*D3/3.+D4/4.)/TP2**3)
      end if
      if (EPSTFL .eq. 0) then
        if (X.le.X0) then
          DELTA=DELTA0*10**(2.0*(X-X0))
        else if (X.lt.X1) then
          DELTA=TOLN10*X - CBAR + AFACT*(X1 - X)**SK
        else
          DELTA=TOLN10*X - CBAR
        end if
      else
        if (E0 .ge. EPSTEN(IEPST)) then
          if (E0 .eq. EPSTEN(IEPST)) then
            go to 100
          end if
          do i=IEPST,NEPST-1
            if (E0.lt.EPSTEN(i+1)) then
              IEPST = I
              go to 100
            end if
          end do
          IEPST = NEPST
          go to 100
        else
          do i=IEPST,2,-1
            if (E0 .ge. EPSTEN(i-1)) then
              IEPST = I-1
              go to 100
            end if
          end do
          IEPST = 1
        END IF
100    if (IEPST .lt. NEPST) then
          DELTA = EPSTD(IEPST) + (E0 - EPSTEN(IEPST))/ (EPSTEN(IEPST+1)
     *    - EPSTEN(IEPST)) * (EPSTD(IEPST+1) - EPSTD(IEPST))
        else
          DELTA = EPSTD(NEPST)
        end if
      end if
      SPIONB=(SPC1/BETA2)*(DLOG(T + 2.) - SPC2 + FTERM - DELTA)
      return
      end

      double precision function spione(E0,EE)
      implicit none
      double precision spionb, E0, EE
      spione=spionb(E0,EE,.FALSE.)
      return
      end

      double precision function spionp(E0,EE)
      implicit none
      double precision spionb, E0, EE
      spionp=spionb(E0,EE,.TRUE.)
      return
      end

      double precision function sptote(E0,EE,EG)
      implicit none
      double precision spione, E0, EE, brmstm, EG
      include 'pegscommons/thres2.f'
      if (IUNRST.eq.0) then
        sptote=spione(E0,EE)+brmstm(E0,EG)
      else if (IUNRST.eq.1) then
        sptote=spione(E0,E0)
      else if (IUNRST.eq.2) then
        sptote=spione(E0,E0)+brmstm(E0,E0)
      else if (IUNRST.eq.3) then
        sptote=spione(E0,E0)+brmstm(E0,EG)
      else if (IUNRST.eq.4) then
        sptote=spione(E0,EE)+brmstm(E0,E0)
      else if (IUNRST.eq.5)  then
        sptote=brmstm(E0,E0)
      else if (IUNRST.eq.6) then
        sptote=brmstm(E0,EG)
      else if (IUNRST.eq.7) then
        sptote=spione(E0,EE)
      end if
      return
      end

      double precision function sptotp(E0,EE,EG)
      implicit none
      double precision spionp, E0, EE, brmstm, EG
      include 'pegscommons/thres2.f'
      if (IUNRST.eq.0) then
        sptotp=spionp(E0,EE)+brmstm(E0,EG)
      else if (IUNRST.eq.1) then
        sptotp=spionp(E0,E0)
      else if (IUNRST.eq.2) then
        sptotp=spionp(E0,E0)+brmstm(E0,E0)
      else if (IUNRST.eq.3) then
        sptotp=spionp(E0,E0)+brmstm(E0,EG)
      else if (IUNRST.eq.4) then
        sptotp=spionp(E0,EE)+brmstm(E0,E0)
      else if (IUNRST.eq.5) then
        sptotp=brmstm(E0,E0)
      else if (IUNRST.eq.6) then
        sptotp=brmstm(E0,EG)
      else if (IUNRST.eq.7) then
        sptotp=spionp(E0,EE)
      end if
      return
      end

      double precision function tmxb(E)
      implicit none
      double precision ESQ, E, BETA2, PX2
      include 'pegscommons/dercon.f'
      include 'pegscommons/molvar.f'
      ESQ=E**2
      BETA2=1.0-RMSQ/ESQ
      PX2=ESQ*BETA2/XCC**2
      tmxb=PX2*BETA2/DLOG(BLCC*PX2)
      return
      end

      double precision function tmxde2(E)
      implicit none
      double precision ESQ, E, BETASQ, TMXB
      include 'pegscommons/dercon.f'
      ESQ=E**2
      BETASQ=1.0-RMSQ/ESQ
      tmxde2=TMXB(E)/(ESQ*BETASQ**2)
      return
      end

      double precision function tmxs(E)
      implicit none
      double precision SAFETY, TABSMX, TMXB, E
      data SAFETY/0.8/,TABSMX/10.0/
      tmxs=DMIN1(TMXB(E)*SAFETY,TABSMX)
      return
      end

      subroutine wtscpr
      implicit none
      integer i, j
      include 'pegscommons/cpcom.f'
      open(UNIT=31,FILE='pgs5job.ssl',STATUS='unknown')
      write(31,'(1H ,A)') '&SCPRDT'
      write(31,'(1H ,A)') 'QCAP='
      write(31,'((1H ,7(F9.2,A)))') (QCAP(I),',',I=1,31)
      write(31,'(1H ,A,I5)') 'MXRAW=',MXRAW
      Write(31,'(1H ,A)') 'ELECNI='
      write(31,'((1H ,7(1PE9.3,A)))') (ELECNI(I),',',I=1,MXRAW)
      write(31,'(1H ,A,I5)') 'MXSHEL=',MXSHEL
      write(31,'(1H ,A)') 'NSHELL='
      write(31,'((1H ,14(I4,A)))') (NSHELL(I),',',I=1,MXRAW)
      write(31,'(1H ,A)') 'CAPIN='
      write(31,'((1H ,5(1PE11.5,A)))') (CAPIN(I),',',I=1,MXRAW)
      write(31,'(1H ,A)') 'SCPROF='
      do i=1,MXRAW+1
        write(31,'((1H ,7(1PE9.3,A)))') (SCPROF(j,i),',',j=1,31)
      end do
      write(31,'(1H ,A)') '/END'
      endfile 31
      close(31)
      return
      end

      double precision function xsif (Z)
      implicit none
      integer IZ
      double precision Z, FCOULC
      include 'pegscommons/radlen.f'
      if (Z.le.4.0) then
        IZ=Z
        xsif=ALRADP(IZ)/(ALRAD(IZ)-FCOULC(Z))
      else
        xsif=dlog(A1440*Z**(-2./3.))/(dlog(A183*Z**(-1./3.))-FCOULC(Z))
      end if
      return
      end

      double precision function ztbl(IASYM)
      implicit none
      integer ie
      include 'pegscommons/elemtb.f'
      include 'pegscommons/elmtbc.f'
      character*4 IASYM,IA
      data IA/'A'/
      if (IASYM.eq.IA) then
        ztbl=18.0
        return
      end if
      do ie=1,NET
        if (IASYM.eq.ASYMT(ie)) then
          ztbl=ie
          return
        end if
      end do
      write(  26,100) IASYM,NET
100   format(1X,A2,' Not an atomic symbol for an element with Z LE ',I3)
      ZTBL=0.0
      return
      end

!-------------------------last line of pegs5.f--------------------------
!-----------------------------------------------------------------------
!                             SUBROUTINE PELASTINO  
!  Version: 060314-0810
!-----------------------------------------------------------------------
!23456789|123456789|123456789|123456789|123456789|123456789|123456789|12

      subroutine prelastino
C
C  This is subroutine simply writes out the current problem multiple
C  scattering parameters so that they can be compared in hatch/rmsfit
C  with the data in the gsdist.dat file, if GS dist is requested.
C
      implicit none

      include 'include/egs5_h.f'
      include 'include/egs5_media.f'
      include 'pegscommons/dcsstr.f'

      integer i,n,didGS

!  write just the material listing for this file, even when
!  GS is not being used

      didGS = 0
      write(17,*) nsdcs
      do n = 1, nsdcs
        write(17,5001) (mednam(n,i),i=1,24)
        write(17,*)  didGS,charD(n),efrch(n),efrcl(n),
     *              egrdhi(n),egrdlo(n)
      end do
5001  format(' MEDIUM=',24A1)

      return
      END
!-----------------------------------------------------------------------
!                       SUBROUTINE SPLINE
!  Version: 051219-1435
!  Reference:  Based on code developed and provided by F. Salvat
!              for computing GS Mult Scat with PW cross sections
!-----------------------------------------------------------------------
!23456789|123456789|123456789|123456789|123456789|123456789|123456789|12

      SUBROUTINE SPLINE(X,Y,A,B,C,D,S1,SN,N)
C
C  Cubic spline interpolation of tabulated data.
C
C  Input:
C     X(I) (I=1:N) ... grid points (the X values must be in increasing
C                      order).
C     Y(I) (I=1:N) ... corresponding function values.
C     S1,SN .......... second derivatives at X(1) and X(N). The natural
C                      spline corresponds to taking S1=SN=0.
C     N .............. number of grid points.
C  Output:
C     A(1:N),B(1:N),C(1:N),D(1:N) ... spline coefficients.
C
C  The interpolating cubic polynomial in the I-th interval, from X(I) to
C  X(I+1), is
C               P(x) = A(I)+x*(B(I)+x*(C(I)+x*D(I)))
C
C  Reference: M.J. Maron, 'Numerical Analysis: a Practical Approach',
C             MacMillan Publ. Co., New York, 1982.
C
      implicit none

      integer N
      double precision  X(N),Y(N),A(N),B(N),C(N),D(N), S1, SN

      integer i, k, n1, n2
      double precision r, h, hi, si, si1 
C
      IF(N.LT.4) THEN
        WRITE(6,10) N
   10   FORMAT(5X,'Spline interpolation cannot be performed with',
     1    I4,' points. Stop.')
        STOP
      ENDIF
      N1=N-1
      N2=N-2
C  ****  Auxiliary arrays H(=A) and DELTA(=D).
      DO I=1,N1
        IF(X(I+1)-X(I).LT.1.0D-25) THEN
          WRITE(6,11)
   11     FORMAT(5X,'Spline x values not in increasing order. Stop.')
          STOP
        ENDIF
        A(I)=X(I+1)-X(I)
        D(I)=(Y(I+1)-Y(I))/A(I)
      ENDDO
C  ****  Symmetric coefficient matrix (augmented).
      DO I=1,N2
        B(I)=2.0D0*(A(I)+A(I+1))
        K=N1-I+1
        D(K)=6.0D0*(D(K)-D(K-1))
      ENDDO
      D(2)=D(2)-A(1)*S1
      D(N1)=D(N1)-A(N1)*SN
C  ****  Gauss solution of the tridiagonal system.
      DO I=2,N2
        R=A(I)/B(I-1)
        B(I)=B(I)-R*A(I)
        D(I+1)=D(I+1)-R*D(I)
      ENDDO
C  ****  The sigma coefficients are stored in array D.
      D(N1)=D(N1)/B(N2)
      DO I=2,N2
        K=N1-I+1
        D(K)=(D(K)-A(K)*D(K+1))/B(K-1)
      ENDDO
      D(N)=SN
C  ****  Spline coefficients.
      SI1=S1
      DO I=1,N1
        SI=SI1
        SI1=D(I+1)
        H=A(I)
        HI=1.0D0/H
        A(I)=(HI/6.0D0)*(SI*X(I+1)**3-SI1*X(I)**3)
     1      +HI*(Y(I)*X(I+1)-Y(I+1)*X(I))
     2      +(H/6.0D0)*(SI1*X(I)-SI*X(I+1))
        B(I)=(HI/2.0D0)*(SI1*X(I)**2-SI*X(I+1)**2)
     1      +HI*(Y(I+1)-Y(I))+(H/6.0D0)*(SI-SI1)
        C(I)=(HI/2.0D0)*(SI*X(I+1)-SI1*X(I))
        D(I)=(HI/6.0D0)*(SI1-SI)
      ENDDO
      RETURN
      END
!-----------------------------------------------------------------------
!                       FUNCTION SUMGA
!  Version: 051219-1435
!  Reference:  Based on code developed and provided by F. Salvat
!              for computing GS Mult Scat with PW cross sections
!-----------------------------------------------------------------------
!23456789|123456789|123456789|123456789|123456789|123456789|123456789|12

      double precision FUNCTION SUMGA(FCT,XL,XU,TOL)
C
C  This function calculates the value SUMGA of the integral of the
C  (external) function FCT over the interval (XL,XU) using the 20-point
C  Gauss quadrature method with an adaptive bipartition scheme.
C
C  TOL is the tolerance, i.e. maximum allowed relative error; it should
C  not exceed 1.0D-13. A warning message in written in unit 6 when the
C  required accuracy is not attained.
C
C                             Francesc Salvat. Barcelona, December 2000.
C
      implicit none

      integer NP, NST, NCALLS, I1, I2, I3, ICALL, LH, I, LHN
      PARAMETER(NP=10,NST=128,NCALLS=20000)
      double precision X(NP),W(NP),S(NST),SN(NST),XR(NST),XRN(NST),
     & CTOL, PTOL, ERR, XL, XU, TOL, A, B, C, D, H, HO, SUMR, FCT,
     & SI, XA, XB, XC, S1, S2, S12
      
      external FCT
C  ****  Gauss 20-point integration formula.
C  Abscissas.
      DATA X/7.6526521133497334D-02,2.2778585114164508D-01,
     1       3.7370608871541956D-01,5.1086700195082710D-01,
     2       6.3605368072651503D-01,7.4633190646015079D-01,
     3       8.3911697182221882D-01,9.1223442825132591D-01,
     4       9.6397192727791379D-01,9.9312859918509492D-01/
C  Weights.
      DATA W/1.5275338713072585D-01,1.4917298647260375D-01,
     1       1.4209610931838205D-01,1.3168863844917663D-01,
     2       1.1819453196151842D-01,1.0193011981724044D-01,
     3       8.3276741576704749D-02,6.2672048334109064D-02,
     4       4.0601429800386941D-02,1.7614007139152118D-02/
C  ****  Error control.
      CTOL=MIN(MAX(TOL,1.0D-13),1.0D-2)
      PTOL=0.01D0*CTOL
      ERR=1.0D35
C  ****  Gauss integration from XL to XU.
      H=XU-XL
      SUMGA=0.0D0
      A=0.5D0*(XU-XL)
      B=0.5D0*(XL+XU)
      C=A*X(1)
      D=W(1)*(FCT(B+C)+FCT(B-C))
      DO I1=2,NP
        C=A*X(I1)
        D=D+W(I1)*(FCT(B+C)+FCT(B-C))
      ENDDO
      ICALL=NP+NP
      LH=1
      S(1)=D*A
      XR(1)=XL
C  ****  Adaptive bipartition scheme.
    1 CONTINUE
      HO=H
      H=0.5D0*H
      SUMR=0.0D0
      LHN=0
      DO I=1,LH
        SI=S(I)
        XA=XR(I)
        XB=XA+H
        XC=XA+HO
        A=0.5D0*(XB-XA)
        B=0.5D0*(XB+XA)
        C=A*X(1)
        D=W(1)*(FCT(B+C)+FCT(B-C))
        DO I2=2,NP
          C=A*X(I2)
          D=D+W(I2)*(FCT(B+C)+FCT(B-C))
        ENDDO
        S1=D*A
        A=0.5D0*(XC-XB)
        B=0.5D0*(XC+XB)
        C=A*X(1)
        D=W(1)*(FCT(B+C)+FCT(B-C))
        DO I3=2,NP
          C=A*X(I3)
          D=D+W(I3)*(FCT(B+C)+FCT(B-C))
        ENDDO
        S2=D*A
        ICALL=ICALL+4*NP
        S12=S1+S2
        IF(ABS(S12-SI).LE.MAX(PTOL*ABS(S12),1.0D-25)) THEN
          SUMGA=SUMGA+S12
        ELSE
          SUMR=SUMR+S12
          LHN=LHN+2
          IF(LHN.GT.NST) GO TO 2
          SN(LHN)=S2
          XRN(LHN)=XB
          SN(LHN-1)=S1
          XRN(LHN-1)=XA
        ENDIF
        IF(ICALL.GT.NCALLS) GO TO 2
      ENDDO
      ERR=ABS(SUMR)/MAX(ABS(SUMR+SUMGA),1.0D-25)
      IF(ERR.LT.CTOL.OR.LHN.EQ.0) RETURN
      LH=LHN
      DO I=1,LH
        S(I)=SN(I)
        XR(I)=XRN(I)
      ENDDO
      GO TO 1
C  ****  Warning (low accuracy) message.
    2 CONTINUE
      WRITE(6,11)
   11 FORMAT(/2X,'>>> SUMGA. Gauss adaptive-bipartition quadrature.')
      WRITE(6,12) XL,XU,TOL
   12 FORMAT(2X,'XL =',1P,E19.12,',  XU =',E19.12,',  TOL =',E8.1)
      WRITE(6,13) ICALL,SUMGA,ERR,LHN
   13 FORMAT(2X,'NCALLS = ',I5,',  SUMGA =',1P,E20.13,',  ERR =',E8.1,
     1      /2X,'NUMBER OF OPEN SUBINTERVALS =',I3)
      WRITE(6,14)
   14 FORMAT(2X,'WARNING: the required accuracy has not been ',
     1  'attained.'/)
      RETURN
      END
!-----------------------------wmsfit.f----------------------------------
! Version: 060313-1255
!-----------------------------------------------------------------------
!23456789|123456789|123456789|123456789|123456789|123456789|123456789|12

      subroutine wmsfit(k1start,dk1)

      implicit none

      double precision k1start(2), dk1(2)
C
      include 'include/egs5_h.f'
      include 'include/egs5_cdcsep.f'
      include 'include/egs5_mscon.f'

      include 'pegscommons/mxdatc.f'

      integer ipart, iener, iang, ik1

      write(17,5001) medium
5001  format(' MEDIUM=',24A1)
      write(17,*) 'MS fitting coefficients for this media'
      write(17,*) 'Total number of energy steps:'
      write(17,*) nmscate
      write(17,*) 'First energy decade:'
      write(17,*) decade1
      write(17,*) 'Number of energy steps inside each decade:'
      write(17,*) NDEC
      write(17,*) 'Number of energy steps to skip in first decade:'
      write(17,*) joffset
      write(17,*) 'Number of equally probably angle bins'
      write(17,*) NBFIT
      write(17,*) 'Number of equally spaced angle bins'
      write(17,*) NEXFIT

!  ***  Loop over the two particle types
      do ipart = 1,2
        if(ipart.eq.1) then
          write(17,*) 'Electrons'
        else
          write(17,*) 'Positrons'
        endif

!  ***  Loop over the energy steps
        do iener = 1,nmscate
          write(17,*) mscate(iener)*1.e-6, ' MeV => ladder energy'

!  ***  Loop over the scattering strength intervals
          do ik1 = 1,NK1
          write(17,*) 'Fits at K1 = ',
     &                 k1start(ipart) * dk1(ipart) ** (ik1-1)
          write(17,*) probns(ipart,iener,ik1), ' => No Scat Prob'
          write(17,*) '    amu     -       amu         b        eta'
!  ***  Loop over the angle intervals
            do iang = 1,NFIT
              write(17,'(1x,4(1pe12.5,1x))') 
     1          amums(ipart,iener,ik1,iang), 
     1          amums(ipart,iener,ik1,iang+1),
     1          bms(ipart,iener,ik1,iang),
     1          etams(ipart,iener,ik1,iang)
            end do

!  ***  loop over the equally spaced angles and print the cdf
            write(17,*) 'CDF for the current region'
            write(17,*) ' 0.0000000E+00'
            do iang = 1,NEXFIT
              write(17,'(1x,1pe14.7,1x)') cumdist(ipart,iener,ik1,iang)
            end do
          end do               !-->  K1 steps
        end do                 !-->  Energy grid
      end do                   !-->  Particle types

      return
      end
!-------------------------last line of wmsfit.f-------------------------
