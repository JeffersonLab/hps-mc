!***********************************************************************
!                                                                       
!                     **************                                    
!                     *            *                                    
!                     *  tutor5.f  *                                    
!                     *            *                                    
!                     **************                                    
!                                                                       
!  An EGS5 user code which scores the number and average energy of the  
!  primary, Rayleigh scattered and Compton scattered photons passing    
!  through a  5 cm thick slab of water when a 50 keV pencil beam of     
!  photons is incident normally                                         
!                                                                       
!                                                                       
!  For SLAC-R-730/KEK Report 2005-8:  Example of including Rayleigh 
!         scattering, and use of the LATCH feature
!                                                                       
!  The following units are used: unit 6 for output           
!***********************************************************************
!23456789|123456789|123456789|123456789|123456789|123456789|123456789|12
!-----------------------------------------------------------------------
!------------------------------- main code -----------------------------
!-----------------------------------------------------------------------

!-----------------------------------------------------------------------
! Step 1: Initialization
!-----------------------------------------------------------------------

      implicit none

!     ------------
!     EGS5 COMMONs
!     ------------
      include 'include/egs5_h.f'                ! Main EGS "header" file

      include 'include/egs5_bounds.f'
      include 'include/egs5_epcont.f'
      include 'include/egs5_media.f'
      include 'include/egs5_misc.f'
      include 'include/egs5_stack.f'
      include 'include/egs5_thresh.f'
      include 'include/egs5_useful.f'
      include 'include/egs5_usersc.f'
      include 'include/randomm.f'

!     bounds contains ecut and pcut
!     epcont contains iausfl
!     media contains the array media
!     misc contains med
!     stack contains latchi
!     thresh contains ae and ap
!     useful contains RM
!     usersc contains emaxe

      common/geom/zbound
      real*8 zbound
!     geom passes info to our howfar routine

      common/score/count(3),entot(3)
      real*8 count,entot

      real*8 ein,xin,yin,zin,             ! Arguments
     *       uin,vin,win,wtin
      integer iqin,irin

      real*8 anorm                             ! Local variables
      integer i,j,ncase
      character*24 medarr(1)

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
      nmed=1
      medarr(1)='H2O                     '

      do j=1,nmed
        do i=1,24
          media(i,j)=medarr(j)(i:i)
        end do
      end do  

! nmed and dunit default to 1, i.e. one medium and we work in cm

      chard(1) = 0.5d0       !  optional, but recommended to invoke
                             !  automatic step-size control

!     ---------------------------------------------
!     Run KEK version of PEGS5 before calling HATCH
!     (method was developed by Y. Namito - 010306)
!     ---------------------------------------------
      write(6,100)
100   FORMAT(' PEGS5-call comes next'/)

!     ==========
      call pegs5
!     ==========

!-----------------------------------------------------------------------
! Step 3: Pre-hatch-call-initialization
!-----------------------------------------------------------------------
      nreg=3
!     nreg : number of region

      med(1)=0
      med(3)=0
      med(2)=1
! Regions 1 and 3 are vacuum, region 2, H2O
      ecut(2)=1.5
!     Terminate electron histories at 1.5 MeV in the slab
      pcut(2)=0.010
!     Terminate   photon histories at 0.01 MeV in the slab
      iraylr(2)=1
!     Turn on rayleigh scattering in the slab
! Note, above three parameters need to be set for all regions in which
! there is particle transport - just region 2 in this case

!     --------------------------------------------------------
!     Random number seeds.  Must be defined before call hatch
!     or defaults will be used.  inseed (1- 2^31)
!     --------------------------------------------------------
      luxlev=1
      inseed=1
      write(6,120) inseed
120   FORMAT(/,' inseed=',I12,5X,
     *         ' (seed for generating unique sequences of Ranlux)')

!     =============
      call rluxinit  ! Initialize the Ranlux random-number generator
!     =============


!-----------------------------------------------------------------------
! Step 4:  Determination-of-incident-particle-parameters
!-----------------------------------------------------------------------
! Define initial variables for 50 keV beam of photons normally incident
! on the slab
      iqin=0
!     Incident photons
!             50 keV
      ein=0.050
      xin=0.0
      yin=0.0
      zin=0.0
!     Incident at origin
      uin=0.0
      vin=0.0
      win=1.0
!     Moving along z axis
      irin=2
!     Starts in region 2, could be 1
      wtin=1.0
!     weight = 1 since no variance reduction used
      latchi=0
!     latch set to zero at start of each history

!-----------------------------------------------------------------------
! Step 5:   hatch-call
!-----------------------------------------------------------------------
! Maximum total energy of an electron for this problem must be
! defined before hatch call
      emaxe = ein + RM

      write(6,130)
130   format(/' Start tutor5'/' Call hatch to get cross-section data')

!     ------------------------------
!     Open files (before HATCH call)
!     ------------------------------
      open(UNIT=KMPI,FILE='pgs5job.pegs5dat',STATUS='old')
      open(UNIT=KMPO,FILE='egs5job.dummy',STATUS='unknown')

      write(6,140)
140   format(/,' HATCH-call comes next',/)

!     ==========
      call hatch
!     ==========

!     ------------------------------
!     Close files (after HATCH call)
!     ------------------------------
      close(UNIT=KMPI)
      close(UNIT=KMPO)

!    Pick up cross section data for water
      write(6,150) ae(1)-RM, ap(1)
150   format(/' Knock-on electrons can be created and any electron ',
     *'followed down to' /T40,F8.3,' MeV kinetic energy'/
     *' Brem photons can be created and any photon followed down to',
     */T40,F8.3,' MeV')
! Compton events can create electrons and photons below these cutoffs

!-----------------------------------------------------------------------
! Step 6:  Initialization-for-howfar
!-----------------------------------------------------------------------
      zbound=0.5
!     Plate is 0.5 cm thick

!-----------------------------------------------------------------------
! Step 7:  Initialization-for-ausgab
!-----------------------------------------------------------------------
      do i=1,3
        count(i)=0.0
        entot(i)=0.0
!  Zero scoring array at start
      end do

!  We want to set flags in ausgab every time a rayleigh scattering
!  or Compton scattering occurs. Set the flags in iausfl(comin
!  epcont) to signal the  egs system to make the appropriate calls
      iausfl(18)=1
      iausfl(24)=1

!-----------------------------------------------------------------------
! Step 8:  Shower-call
!-----------------------------------------------------------------------
! Initiate the shower ncase times
      ncase=10000
      do i=1,NCASE
        call shower(iqin,ein,xin,yin,zin,uin,vin,win,irin,wtin)
      end do

!-----------------------------------------------------------------------
! Step 9:  Output-of-results
!-----------------------------------------------------------------------
! Normalize to % of photon number
      anorm = 100./float(ncase)
      do i=1,3
        if (count(i).ne.0) then
          entot(i)=entot(i)/count(i)
!    Get average energies
        end if
      end do
      write(6,160) ein*1000.,zbound, pcut(2), (anorm*count(i),entot(i),
     *i=1,3)
160   format(/' For',F6.1,' keV photons incident on',F4.1,'cm of H2O',
     *' with PCUT=',F5.3,' MeV' //' Transmitted primaries=',T40,F8.2,
     *'%  ave energy=',F10.3,' MeV'// ' Fraction Rayleigh scattering=',
     *T40,F8.2,'%  ave energy=',F10.3,' MeV' //
     *' Fraction Compton scattering only=',T40,F8.2,'%  ave energy=',
     *F10.3, ' MeV'//)
      
      stop
      end
!-------------------------last line of main code------------------------

!-------------------------------ausgab.f--------------------------------
!-----------------------------------------------------------------------
!23456789|123456789|123456789|123456789|123456789|123456789|123456789|12
! ----------------------------------------------------------------------
! Required subroutine for use with the EGS5 Code System
! ----------------------------------------------------------------------
!***********************************************************************
!
!  In this AUSGAB routine for TUTOR5 we both set flags whenever there is
!  a scattering event and then count histories when they have come
!  through the slab , according to what kind of scattering they have
!  undergone.
!  The logic is as follows
!   set FLAG1 if a Compton event occurs
!   set FLAG2 if a Rayleigh event occurs
!  The FLAGS are the units and thousands digits in the parameter LATCH
! 
!  When a history is terminated, increment various counters according
!  to whether no flags are set - i.e. its a primary, FLAG2 is set,
!  i.e. it has Rayleigh scattered or FLAG1 is set and FLAG2 is not set
!   i.e. only  Compton scattering has occurred.
!***********************************************************************
      subroutine ausgab(iarg)

      implicit none

      include 'include/egs5_h.f'                ! Main EGS "header" file

      include 'include/egs5_stack.f'      ! COMMONs required by EGS5 code

      common/score/count(3),entot(3)
      real*8 count,entot

      integer iarg                                          ! Arguments
      
      integer jj                                       ! Local variable

      if (iarg.eq.17) then
!  A Compton scatter is about to occur
        latch(np)=latch(np)+1
      else if (iarg.eq.23) then
!  A Rayleigh scatter is about to occur
        latch(np)=latch(np)+1000
!   If a history has terminated because leaving the slab, score it
!  Particle has left slab
      else if (iarg .eq. 3) then
        if (ir(np).eq.3 .or. ir(np) .eq. 1) then
!    It is transmitted or reflected
          jj=0
          if (latch(np) .eq. 0) then
!      No scattering - a primary
            jj=1
          else if (mod(latch(np),10000)-mod(latch(np),100) .ne. 0) then
!      at least one Rayleigh scatter
            jj=2
          else if (mod(latch(np),100) .ne. 0) then
!      at least one Compton scatter without Rayleigh
            jj=3
!      debug
          else
            write(6,1080) jj,latch(np)
1080        format(' jj,latch(np)=',2I10)
          end if
          if (jj .ne. 0) then
            count(jj)=count(jj) + 1.
            entot(jj) = entot(jj) + e(np)
          end if
!    End region 3 block
        end if
!  End iarg 3 block
      end if
      return
      end

!--------------------------last line of ausgab.f------------------------
!-------------------------------howfar.f--------------------------------
!-----------------------------------------------------------------------
!23456789|123456789|123456789|123456789|123456789|123456789|123456789|12
! ----------------------------------------------------------------------
! Required (geometry) subroutine for use with the EGS5 Code System
!***********************************************************************
!                                                                       
! The following is a general specification of howfar
!   given a particle at (x,y,z) in region ir and going in direction     
!   (u,v,w), this routine answers the question, can the particle go     
!   a distance ustep without crossing a boundary                        
!           If yes, it merely returns                                   
!           If no, it sets ustep=distance to boundary in the current    
!           direction and sets irnew to the region number   on the      
!           far side of the boundary (this can be messy in general!)    
!                                                                       
!   The user can terminate a history by setting idisc>0. here we
!   terminate all histories which enter region 3 or are going
!   backwards in region 1
!
!                   |               |
!   Region 1        |   Region 2    |       Region 3
!                   |               |
!   e- =========>   |               | e- or photon ====> 
!                   |               |
!   vacuum          |     Ta        |       vacuum 
!                                                                       
!***********************************************************************
      subroutine howfar

      implicit none

      include 'include/egs5_h.f'                ! Main EGS "header" file

      include 'include/egs5_epcont.f'    ! COMMONs required by EGS5 code
      include 'include/egs5_stack.f'

      common/geom/zbound
      real*8 zbound
!     geom passes info to our howfar routine

      real*8 tval                              ! Local variable

      if (ir(np).eq.3) then
        idisc=1
        return
!  Terminate this history: it is past the plate
!  We are in the Ta plate - check the geometry
      else if (ir(np).eq.2) then
        if (w(np).gt.0.0) then
!  Going forward - consider first since  most frequent
!  tval is dist to boundary in this direction
          tval=(zbound-z(np))/w(np)
          if (tval.gt.ustep) then
            return
!  Can take currently requested step
          else
            ustep=tval
            irnew=3
            return
          end if
!    end of w(np)>0 case
!    Going back towards origin
        else if (w(np).lt.0.0) then
!    Distance to plane at origin
          tval=-z(np)/w(np)
          if (tval.gt.ustep) then
            return
!    Can take currently requested step
          else
            ustep=tval
            irnew=1
            return
          end if
!    End w(np)<0 case
!    Cannot hit boundary
        else if (w(np).eq.0.0) then
          return
        end if
!  End of region 2 case
!  In regon with source
!  This must be a source particle on z=0 boundary
      else if (ir(np).eq.1) then
        if (w(np).gt.0.0) then
          ustep=0.0
          irnew=2 
          return 
        else
!  It must be a reflected particle-discard it
          idisc=1
          return
        end if
!  End region 1 case
      end if
      end

!--------------------------last line of howfar.f------------------------
