!***********************************************************************
!                                                                       
!                     **************                                    
!                     *            *                                    
!                     *  tutor3.f  *                                    
!                     *            *                                    
!                     **************                                    
!                                                                       
! An EGS5 user code which scores the spectrum of energy deposited in a  
! 2.54 cm thick slab of NaI when a 5 MeV beam of photons is incident    
! on it i.e. it computes the fraction of histories which deposit a      
! certain amount of energy in the slab                                  
!                                                                       
!  For SLAC-R-730/KEK Report 2005-8:  Example of calculating a detector 
!  response function 
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
      include 'include/egs5_media.f'
      include 'include/egs5_misc.f'
      include 'include/egs5_thresh.f'
      include 'include/egs5_useful.f'
      include 'include/egs5_usersc.f'
      include 'include/randomm.f'

!     bounds contains ecut and pcut
!     media contains the array media
!     misc contains med
!     thresh contains ae and ap
!     useful contains RM
!     usersc contains emaxe

      common/geom/zbound
      real*8 zbound
!     geom passes info to our howfar routine

      common/score/ehist
      real*8 ehist

      real*8 ein,xin,yin,zin,               ! Arguments
     *       uin,vin,win,wtin
      integer iqin,irin

      real*8 binmax,bwidth,ebin(25)              ! Local variables
      integer i,ibin,icol,j,ncase
      character*24 medarr(1)
      character*4 line(48)

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
      medarr(1)='NAI                     '

      do j=1,nmed
        do i=1,24
          media(i,j)=medarr(j)(i:i)
        end do
      end do  

! nmed and dunit default to 1, i.e. one medium and we work in cm

      chard(1) = 2.54d0      !  optional, but recommended to invoke
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
! Regions 1,3 are vacuum, region 2, NaI
      ecut(2)=0.7
! Terminate electron histories at 0.7 MeV in the plate
      pcut(2)=0.1
! Terminate   photon histories at 0.1 MeV in the plate

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
! Define initial variables for 5 MeV beam of photons normally incident
! on the slab
      iqin=0
!     Incident charge - photons
!     5 MeV kinetic energy
      ein=5.0
      xin=0.0
      yin=0.0
      zin=0.0
!     Incident at origin
      uin=0.0
      vin=0.0
      win=1.0
! Moving along z axis
      irin=2
!     Starts in region 2, could be 1
      wtin=1.0
!     weight = 1 since no variance reduction used

!-----------------------------------------------------------------------
! Step 5:   hatch-call
!-----------------------------------------------------------------------
! Maximum total energy of an electron for this problem must be
! defined before HATCH call
      emaxe = ein + RM

      write(6,130)
130   format(/' Start tutor3'/' Call hatch to get cross-section data')

!     ------------------------------
!     Open files (before HATCH call)
!     ------------------------------
      open(UNIT=KMPI,FILE='pgs5job.pegs5dat',STATUS='old')
      open(UNIT=KMPO,FILE='egs5job.dummy',STATUS='unknown')

      write(6,140)
140   FORMAT(/,' HATCH-call comes next',/)

!     ==========
      call hatch
!     ==========

!     ------------------------------
!     Close files (after HATCH call)
!     ------------------------------
      close(UNIT=KMPI)
      close(UNIT=KMPO)

!    Pick up cross section data for nai
      write(6,150) ae(1)-RM, ap(1)
150   format(/' Knock-on electrons can be created and any electron ',
     *'followed down to' /T40,F8.3,' MeV kinetic energy'/
     *' Brem photons can be created and any photon followed down to',
     */T40,F8.3,' MeV')
! Compton events can create electrons and photons below these cutoffs

!-----------------------------------------------------------------------
! Step 6:  Initialization-for-howfar
!-----------------------------------------------------------------------
      zbound= 2.54
!     Plate is 2.54 cm thick

!-----------------------------------------------------------------------
! Step 7:  Initialization-for-ausgab
!-----------------------------------------------------------------------
      do i=1,25
        ebin(i) = 0.0
!  Zero scoring array before starting
      end do
      bwidth = 0.2
! Energy spectrum will have 200 keV width

!-----------------------------------------------------------------------
! Step 8:  Shower-call
!-----------------------------------------------------------------------
! Initiate the shower ncase times
      ncase=10000
      do i=1,ncase
        ehist = 0.0
!  Zero energy deposited in this history
        call shower(iqin,ein,xin,yin,zin,uin,vin,win,irin,wtin)
!  Increment bin corresponding to  energy deposited in this history
        ibin= min0 (int(ehist/bwidth + 0.999), 25)
        if (ibin.ne.0) then
          ebin(ibin)=ebin(ibin)+1
        end if
      end do

!-----------------------------------------------------------------------
! Step 9:  Output-of-results
!-----------------------------------------------------------------------
! Pick up maximum bin for normalization
      binmax=0.0
      do j=1,25
        binmax=max(binmax,ebin(j))
      end do
      write(6,160) ein,zbound
160   format(/' Response function'/' for a',F8.2,' MeV pencil beam of',
     *'photons on a',F7.2,' cm thick slab of NaI'/ T6,
     *'Energy  counts/incident photon')
      do j=1,48
        line(j)=' '
      end do
! Blank entire output array
      do j=1,25
        icol=int(ebin(j)/binmax*48.0+0.999)
        if (icol.eq.0) icol=1
        line(icol)='*'
!  Load output array at desired location
        write(6,170) bwidth*j,ebin(j)/float(ncase),line
170     format(F10.2,F10.4,48A1)
        line(icol)=' '
!  Reblank
      end do
      
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
! In this AUSGAB routine for TUTOR3, we score the energy deposited      
! in the detector region, region 2                                      
!                                                                       
!  For IARG=0, an electron or photon step is about to occur and we      
!  score the energy deposited, if any. Note that only electrons         
!  deposit energy during a step, and due to our geometry, electrons     
!  only take steps in region 2 - however there is no need to check      
!   this here                                                           
!  For IARG=1,2 and 4,particles have been discarded for falling below   
!  various energy cutoffs and all their energy is deposited locally     
!  (in fact EDEP = particles kinetic energy). This only happens in      
!  region 2.  For IARG=3, we are discarding the particle since it is    
!   in region 1 or 3, so we do not score its energy                     
!                                                                       
!  EHIST keeps track of the total energy deposited during each          
!  history. In the main routine it is zeroed at the start of each       
!  history and binned at the end of each history.                       
!***********************************************************************
      subroutine ausgab(iarg)

      implicit none

      include 'include/egs5_h.f'                ! Main EGS "header" file

      include 'include/egs5_epcont.f'     ! COMMONs required by EGS5 code
      include 'include/egs5_stack.f'

      common/score/ehist
      real*8 ehist

      integer iarg                                          ! Arguments

      if (iarg.le.2 .or. iarg.eq.4) then
        ehist=ehist + edep
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
