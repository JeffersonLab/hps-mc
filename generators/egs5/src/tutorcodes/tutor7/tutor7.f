!***********************************************************************
!                                                                       
!                     **************                                    
!                     *            *                                    
!                     *  tutor7.f  *                                    
!                     *            *                                    
!                     **************                                    
!                                                                       
! An EGS5 user code which scores the spectrum of reflection from
! 1.0 cm thick slab of lead when a 100 keV beam of photons is incident
! on it with or without fluorescence photons.
!
!  For SLAC-R-730/KEK Report 2005-8:  Example of including fluorescence
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
      include 'include/egs5_edge.f'
      include 'include/egs5_epcont.f'
      include 'include/egs5_media.f'
      include 'include/egs5_misc.f'
      include 'include/egs5_thresh.f'
      include 'include/egs5_useful.f'
      include 'include/egs5_usersc.f'
      include 'include/randomm.f'

!     bounds contains ecut and pcut
!     edge contains iedgfl
!     epcont contains iausfl
!     media contains the array media
!     misc contains med
!     thresh contains ae and ap
!     useful contains RM
!     usersc contains emaxe

!     ----------------------
!     Auxiliary-code COMMONs
!     ----------------------
      include 'auxcommons/aux_h.f'   ! Auxiliary-code "header" file

      include 'auxcommons/pladta.f'

      common/score/bwidth,ebin(50)
      real*8 bwidth,ebin

      real*8 ein,xin,yin,zin,               ! Arguments
     *       uin,vin,win,wtin
      integer iqin,irin

      real*8 binmax                              ! Local variables
      integer i,icol,j,ncase
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
      medarr(1)='PB                      '

      do j=1,nmed
        do i=1,24
          media(i,j)=medarr(j)(i:i)
        end do
      end do  

! nmed and dunit default to 1, i.e. one medium and we work in cm

      chard(1) = 1.0d0       !  optional, but recommended to invoke
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
! Regions 1 and 3 are vacuum, region 2, lead
      iraylr(2)=1
!     Turn on rayleigh scattering in the slab
      iedgfl(2)=1
!     1: Turn on fluorescence production in the slab
!     0: Turn off fluorescence production in the slab
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
! Define initial variables for 100 keV beam of photons normally incident
! on the slab
      iqin=0
!     Incident photons
!             100 keV
      ein=0.100
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

!-----------------------------------------------------------------------
! Step 5:   hatch-call
!-----------------------------------------------------------------------
! Maximum total energy of an electron for this problem must be
! defined before hatch call
      emaxe = ein + RM

      write(6,130)
130   format(/' Start tutor7'/' Call hatch to get cross-section data')

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

!    Pick up cross section data for lead
      write(6,150) ae(1)-RM, ap(1)
150   format(/' Knock-on electrons can be created and any electron ',
     *'followed down to' /T40,F8.3,' MeV kinetic energy'/
     *' Brem photons can be created and any photon followed down to', 
     */T40,F8.3,' MeV')
! Compton events can create electrons and photons below these cutoffs

!-----------------------------------------------------------------------
! Step 6:  Initialization-for-howfar
!-----------------------------------------------------------------------
! Define the coordinates and the normal vectors for the two planes.
! Information required by howfar (and auxiliary geometry subprograms)
! and passed through common/pladta/
!
! First plane (the x-y plane through the origin)
      pcoord(1,1)=0.0
      pcoord(2,1)=0.0
      pcoord(3,1)=0.0
! Coordinates         
      pnorm(1,1) =0.0
      pnorm(2,1) =0.0
      pnorm(3,1)= 1.0
! Normal vectors      
! Second plane (note: slab is 1 cm thick)
      pcoord(1,2)=0.0
      pcoord(2,2)=0.0
      pcoord(3,2)=1.0
! Coordinates         
      pnorm(1,2) =0.0
      pnorm(2,2) =0.0
      pnorm(3,2)= 1.0
! Normal vectors      

!-----------------------------------------------------------------------
! Step 7:  Initialization-for-ausgab
!-----------------------------------------------------------------------
      do i=1,50
        ebin(i) = 0.0
!  Zero scoring array before starting
      end do
      bwidth = 0.002

!-----------------------------------------------------------------------
! Step 8:  Shower-call
!-----------------------------------------------------------------------
! Initiate the shower ncase times
      ncase=10000
      do i=1,NCASE
        call shower(iqin,ein,xin,yin,zin,uin,vin,win,irin,wtin)
      end do

!-----------------------------------------------------------------------
! Step 8:  Output-of-results
!-----------------------------------------------------------------------
! Use log10(10000.0) as maximum value
      binmax=dlog10(10000.d0)

      if (iedgfl(2).eq.1) then
        write(6,160) ein,pcoord(3,2)
160     format(/' Reflected photon spectrum'/' for a',F8.2,
     *  ' MeV pencil beam of photons on a',F7.2,
     *  ' cm thick slab of lead'/' with fluorescence photon'//T6,
     *  'Energy  counts/incident photon'/
     *  25X,' log(counts for 10^4 incident photons)')
      else
        write(6,170) ein,pcoord(3,2)
170     format(' Reflected photon spectrum'/' for a',F8.2,
     *  ' MeV pencil beam of photons on a',F7.2,
     *  ' cm thick slab of lead'/' without fluorescence photon'//T6,
     *  'Energy  counts/incident photon'/
     *  25X,' log(counts for 10^4 incident photons)')
      end if
      
      do j=1,48
        line(j)=' '
      end do
! Blank entire output array
      do j=1,50
        if(ebin(j).gt.0) then
          icol=
     *       int(dlog10(ebin(j)*10000.0/float(ncase))/binmax*48.0+0.999)
          if (icol.eq.0) icol=1
        else
          icol = 1
        endif
        line(icol)='*'
!  Load output array at desired location
        write(6,180) bwidth*j,ebin(j)/float(ncase),line
180     format(F10.4,F10.4,48A1)
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
!  In this AUSGAB routine for TUTOR7 we score photons reflected
!  from the slab (ir(np)=1 and iq(np)=0).
!***********************************************************************
      subroutine ausgab(iarg)

      implicit none

      include 'include/egs5_h.f'                ! Main EGS "header" file
      include 'include/egs5_stack.f'     ! COMMONs required by EGS5 code

      common/score/bwidth,ebin(50)
      real*8 bwidth,ebin

      integer iarg                                           ! Arguments
      
      integer ibin,irl                                  ! Local variable

      irl=ir(np)                     ! Local variable
      if(irl.eq.1.and.iq(np).eq.0) then   ! Photon is reflected
!     Increment bin corresponding to photon energy
        ibin= min0 (int(e(np)/bwidth + 0.999), 50)
        if (ibin.ne.0) then
          ebin(ibin)=ebin(ibin)+1
        end if
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
! The following is a general specification of howfar.  Essentially
! it is the same as that given in tutor1.f with the following 
! exception: 1) Particles must be initially begin in region 2 and are
!               discarded when that enter region 1 or 3 (no check
!               is made on w(np)).
!            2) The coding is much simplified (i.e., modular)
!               As a result of using auxiliary geometry subprogram
!               plan2p, plane1 and chgtr which require commons
!               epcont, pladta, and stack).
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
! DESCRIPTION - PLAN2P is generally called from subroutine HOWFAR       
!   whenever a particle is in a region bounded by two planes that       
!   ARE parallel.  Both subroutines PLANE1 and CHGTR are called         
!   by PLAN2P (the second PLANE1 call is not made if the first          
!   plane is not hit, or if the trajectory is parallel).                
!------------------------------------------------------------------     
!     NPL1   = ID number assigned to plane called first (input)         
!     NRG1   = ID number assigned to region particle trajectory         
!              will lead into                                           
!     ISD1   =  1 normal points towards current region (input)          
!            = -1 normal points away from current region (input)        
!     NPL2   = Same (but for plane called second)                       
!     NRG2   = Same (but for plane called second)                       
!     ISD2   = Same (but for plane called second)                       
!***********************************************************************
      subroutine howfar

      implicit none

      include 'include/egs5_h.f'                ! Main EGS "header" file

      include 'include/egs5_epcont.f'    ! COMMONs required by EGS5 code
      include 'include/egs5_stack.f'

!     ----------------------
!     Auxiliary-code COMMONs
!     ----------------------
      include 'auxcommons/aux_h.f'   ! Auxiliary-code "header" file

      include 'auxcommons/pladta.f'

      integer irl                                 ! Local variable

      irl=ir(np)              ! Set local variable
      if (irl.ne.2) then
        idisc=1       ! Terminate this history if not in plate
      else            ! We are in the Ta plate - check the geometry
        call plan2p(irl,irl+1,1,irl-1,irl-1,-1)
      end if
      
      return
      end

!--------------------------last line of howfar.f------------------------
