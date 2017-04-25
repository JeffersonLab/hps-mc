!***********************************************************************
!                                                                       
!                     **************                                    
!                     *            *                                    
!                     *  tutor6.f  *                                    
!                     *            *                                    
!                     **************                                    
!                                                                       
!  An EGS5 user code. It lists the particles escaping from the back     
!  of a 1 mm Ta plate when a pencil beam of  20 MeV electrons           
!  is incident on it normally.                                          
!                                                                       
!  NOTE: This program is the same as TUTOR1.MOR except that the         
!        geometry subroutine (HOWFAR) is simplified by the use of       
!        the general purpose geometry subroutines PLAN2P, PLANE1, and   
!        CHGTR (see comments in subprograms for details).               
!                                                                       
!  For SLAC-R-730/KEK Report 2005-8: A simple example which 'scores' 
!  by listing particles  
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

!     ----------------------
!     Auxiliary-code COMMONs
!     ----------------------
      include 'auxcommons/aux_h.f'   ! Auxiliary-code "header" file

      include 'auxcommons/pladta.f'

      real*8 ein,xin,yin,zin,                   ! Arguments
     *       uin,vin,win,wtin
      integer iqin,irin

      integer i,j                                ! Local variables
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
      medarr(1)='TA                      '

      do j=1,nmed
        do i=1,24
          media(i,j)=medarr(j)(i:i)
        end do
      end do  

! nmed and dunit default to 1, i.e. one medium and we work in cm

      chard(1) = 0.1d0       !  optional, but recommended to invoke
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
! Vacuum in regions 1 and 3, ta in region 2
      ecut(2)=1.5
! Terminate electron histories at 1.5 MeV in the plate
      pcut(2)=0.1
! Terminate   photon histories at 0.1 MeV in the plate
!             Only needed for region 2 since no transport elsewhere
!             ecut is total energy = 0.989   MeV kinetic energy

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
! Define initial variables for 20 MeV beam of electrons incident
! perpendicular to the slab
      iqin=-1
!            Incident charge - electrons
!            20 MeV kinetic energy
      ein=20.d0 + RM
      xin=0.0
      yin=0.0
      zin=0.0
!     Incident at origin
      uin=0.0
      vin=0.0
      win=1.0
!            Moving along z axis
      irin=2
!            Starts in region 2, could be 1
!            weight = 1 since no variance reduction used
      wtin=1.0
!     Weight = 1 since no variance reduction used

!-----------------------------------------------------------------------
! Step 5:   hatch-call
!-----------------------------------------------------------------------
! Maximum total energy of an electron for this problem must be
! defined before hatch call
      emaxe = ein

      write(6,130)
130   format(/' Start tutor6'/' Call hatch to get cross-section data')

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

!    Pick up cross section data for ta
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
! Second plane (note: slab is 1 mm thick still)
      pcoord(1,2)=0.0
      pcoord(2,2)=0.0
      pcoord(3,2)=0.1
! Coordinates         
      pnorm(1,2) =0.0
      pnorm(2,2) =0.0
      pnorm(3,2)= 1.0
! Normal vectors      

!-----------------------------------------------------------------------
! Step 7:  Initialization-for-ausgab
!-----------------------------------------------------------------------
! Print header for output - which is all ausgab does in this case
      write(6,160)
160   format(/T19,'Kinetic energy(MeV)',T40,'charge',T48, 
     *'angle w.r.t. z axis-degrees')
     
!-----------------------------------------------------------------------
! Step 8:  Shower-call
!-----------------------------------------------------------------------
! Initiate the shower 10 times
      do i=1,10
        write(6,170) i
170     format(' Start history',I4)
        call shower(iqin,ein,xin,yin,zin,uin,vin,win,irin,wtin)
        
!-----------------------------------------------------------------------
! Step 9:  Output-of-results
!-----------------------------------------------------------------------
!  Note output is at the end of each history in subroutine ausgab
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
!  In general, ausgab is a routine which is called under a series       
!  of well defined conditions specified by the value of iarg (see the   
!  egs5 manual for the list).  This is a particularly simple ausgab.   
!  Whenever this routine is called with iarg=3 , a particle has         
!  been discarded by the user in howfar                                 
!  we get ausgab to print the required information at that point        
!                                                                       
!***********************************************************************
      subroutine ausgab(iarg)

      implicit none

      include 'include/egs5_h.f'                ! Main EGS "header" file

      include 'include/egs5_stack.f'     ! COMMONs required by EGS5 code
      include 'include/egs5_useful.f'

      integer iarg                                          ! Arguments

      real*8 angle,ekine                         ! Local variables

      if (iarg.eq.3) then
!  Angle w.r.t. z axis in degrees
        angle=acos(w(np))*180./3.14159
        if (iq(np).eq.0) then
          ekine=e(np)
        else
          ekine=e(np)-RM
!  Get kinetic energy
        end if
        write(6,100) ekine,iq(np),angle
100     format(T21,F10.3,T33,I10,T49,F10.1)
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
!               As a result of using auxiliar geometry subprogram
!               plan2p (which calls plane1 and chgtr which require
!               commons epcont, pladta, and stack).
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
