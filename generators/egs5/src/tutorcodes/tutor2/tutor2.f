!***********************************************************************
!                                                                       
!                     **************                                    
!                     *            *                                    
!                     *  tutor2.f  *                                    
!                     *            *                                    
!                     **************                                    
!                                                                       
!  An EGS5 user code. It lists the particles escaping from the back     
!  of a 1 mm Ta plate when a pencil beam of  20 MeV electrons           
!  is incident on it normally.                                          
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

      common/geom/zbound
      real*8 zbound
!     geom passes info to our howfar routine

      common/score/escore(3)
      real*8 escore

      real*8 ein,xin,yin,zin,                  ! Arguments
     *       uin,vin,win,wtin
      integer iqin,irin

      real*8 anorm,total                       ! Local variables
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
130   format(/' Start tutor2'/' Call hatch to get cross-section data')

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
      CLOSE(UNIT=KMPI)
      CLOSE(UNIT=KMPO)

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
      zbound=0.1
!     plate is 1 mm thick

!-----------------------------------------------------------------------
! Step 7:  Initialization-for-ausgab
!-----------------------------------------------------------------------
      do i=1,3
        escore(i)=0.0
!  Zero scoring array before starting
      end do

!-----------------------------------------------------------------------
! Step 8:  Shower-call
!-----------------------------------------------------------------------
! Initiate the shower ncase times
      ncase=1000
      do i=1,ncase
        call shower(iqin,ein,xin,yin,zin,uin,vin,win,irin,wtin)
      end do

!-----------------------------------------------------------------------
! Step 9:  Output-of-results
!-----------------------------------------------------------------------
      anorm = 100./((ein-RM)*float(ncase))
! Normalize to % of total input energy
      total=0.0
      do i=1,3
        total=total+escore(i)
      end do
      write(6,160) (escore(i)*anorm,i=1,3),total*anorm
160   format(/' Fraction of energy reflected from plate=',T50,F10.3,'%'
     */ ' Fraction of energy deposited in plate=',T50,F10.3,'%'/
     *' Fraction of energy transmitted through plate=',T50,F10.3,'%'/
     *T50,11('-')/' Total fraction of energy accounted for=', T50,
     *F10.3,'%'/)  
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
! In this AUSGAB routine for TUTOR2, we score the energy deposited      
!  in the various regions. This amounts to the total energy             
!  reflected, deposited and transmitted by the slab.                     
!                                                                       
!  For IARG=0, an electron or photon step is about to occur and we      
!  score the energy deposited, if any. Note that only electrons         
!  deposit energy during a step, and due to our geometry, electrons     
!  only take steps in region 2 - however there is no need to check.     
!  For IARG=1,2 and 4, particles have been discarded for falling        
!  below various energy cutoffs and all their energy is deposited       
!  locally (in fact EDEP = particles kinetic energy).                   
!  For IARG=3, we are discarding the particle since it is in            
!  region 1 or 3, so score its energy.                                  
!                                                                       
!***********************************************************************
      subroutine ausgab(iarg)

      implicit none

      include 'include/egs5_h.f'                ! Main EGS "header" file

      include 'include/egs5_epcont.f'    ! COMMONs required by EGS5 code
      include 'include/egs5_stack.f'

      common/score/escore(3)
      real*8 escore

      integer iarg                                          ! Arguments

      integer irl                                     ! Local variables

      if (iarg.le.4) then
        irl=ir(np)
!   Pick up current region number
        escore(irl)=escore(irl)+edep
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
