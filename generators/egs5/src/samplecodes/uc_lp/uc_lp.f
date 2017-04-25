!***********************************************************************
!                                                                       
!                     ***********                                    
!                     *         *                                    
!                     * uc_lp.f *                                    
!                     *         *                                    
!                     ***********                                    
!                                                                       
!  An EGS5 user code. It lists the particles escaping from the back     
!  of a 30cm NaI plate when a pencil beam of  100 GeV photons       
!  is incident on it normally.                                          
!                                                                       
!  For SLAC-R-730/KEK Report 2005-8: An example of scoring energy 
!     deposited in various regions with or without Leading Particle 
!     biasing
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

      include 'include/egs5_bounds.f'  ! bounds contains ecut and pcut
      include 'include/egs5_epcont.f'  ! epcont contains iausfl
      include 'include/egs5_media.f'   ! media contains the array media
      include 'include/egs5_misc.f'    ! misc contains med
      include 'include/egs5_thresh.f'  ! thresh contains ae and ap
      include 'include/egs5_useful.f'  ! useful contains RM
      include 'include/egs5_usersc.f'  ! usersc contains emaxe
      include 'include/randomm.f'

      include 'auxcommons/aux_h.f'   ! Auxiliary-code "header" file

      include 'auxcommons/pladta.f'

      common/score/escore(3)
      real*8 escore

      real*8 ein,xin,yin,zin,             ! Arguments
     *       uin,vin,win,wtin
      integer iqin,irin

      real*8 anorm,total                       ! Local variables
      real
     * tarray(2),tt,tt0,tt1,cputime
      integer i,j,ncase
      character*24 medarr(1)
      real etime

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

      chard(1) = 3.0d1       !  optional, but recommended to invoke
                             !  automatic step-size control

!     -------------------------------
!     Run PEGS5 before calling HATCH
!     -------------------------------
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
! Vacuum in regions 1 and 3, NaI in region 2
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
! Define initial variables for 100 GeV beam of photons incident
! perpendicular to the slab
      iqin=0             ! Incident charge - photons
      ein=100000.0       ! 100 GeV kinetic energy
      xin=0.0            ! Incident at origin
      yin=0.0
      zin=0.0
      uin=0.0            ! Moving along z axis
      vin=0.0
      win=1.0
      irin=2             ! Starts in region 2, could be 1
      wtin=1.0           ! Weight = 1 since no variance reduction used

!-----------------------------------------------------------------------
! Step 5:   hatch-call
!-----------------------------------------------------------------------
! Maximum total energy of an electron for this problem must be
! defined before hatch call
      emaxe = ein + RM

      write(6,130)
130   format(/' Start uc_lp'/' Call hatch to get cross-section data')

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

!    Pick up cross section data for NaI
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
! Second plane (note: slab is 30 cm thick)
      pcoord(1,2)=0.0
      pcoord(2,2)=0.0
      pcoord(3,2)=30.0
! Coordinates         
      pnorm(1,2) =0.0
      pnorm(2,2) =0.0
      pnorm(3,2)= 1.0
! Normal vectors      

!-----------------------------------------------------------------------
! Step 7:  Initialization-for-ausgab
!-----------------------------------------------------------------------
      do i=1,3
        escore(i)=0.0
!  Zero scoring array before starting
      end do

!  We want to set flags in ausgab every time after a bremsstrahlung
!  or pairproduction event to aaply Leading Particle Biasing.
!  If iausfl is 0, Leading Particle Biasing is not applied.
!  Set the flags in iausfl(comin epcont) to signal the  egs system 
!  to make the appropriate calls
      iausfl(8)=0   ! After bremsstrahlung
      iausfl(17)=0  ! After pair production

!-----------------------------------------------------------------------
! Step 8:  Shower-call
!-----------------------------------------------------------------------
      tt=etime(tarray)
      tt0=tarray(1)

! Initiate the shower ncase times
      ncase=100
      do i=1,ncase
        call shower(iqin,ein,xin,yin,zin,uin,vin,win,irin,wtin)
      end do

      tt=etime(tarray)
      tt1=tarray(1)
      cputime=tt1-tt0

!-----------------------------------------------------------------------
! Step 9:  Output-of-results
!-----------------------------------------------------------------------
      if(iausfl(8).eq.1.and.iausfl(17).eq.1) then
        write(6,160) cputime,ncase
160     format(/1X,G15.5,' sec for ',I8,' cases'/
     *  ' With Leading Particle Biasing.')
      else 
        write(6,170) cputime,ncase
170     format(/1X,G15.5,' sec for ',I8,' cases'/
     *  ' Without Leading Particle Biasing.')
      end if
      
      anorm = 100./((ein-RM)*float(ncase))
! Normalize to % of total input energy
      total=0.0
      do i=1,3
        total=total+escore(i)
      end do
      write(6,180) (escore(i)*anorm,i=1,3),total*anorm
180   format(/' Fraction of energy reflected from plate=',T50,F10.3,'%'
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
!  In this AUSGAB routine for UC_LP, we score the energy deposited   
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
!  For IARG=8 or 17, Leading Particle Biasing is applied to select
!  only one particle with a weight. 
!                                                                       
!***********************************************************************
      subroutine ausgab(iarg)

      implicit none

      include 'include/egs5_h.f'                ! Main EGS "header" file

      include 'include/egs5_epcont.f'    ! epcont contains edep
      include 'include/egs5_stack.f'     ! stack contains x, y, z, u, v,
                                         ! w, ir and np
      include 'include/egs5_useful.f'    ! useful contains RM
      include 'include/randomm.f'

      common/score/escore(3)
      real*8 escore

      integer iarg                                          ! Arguments

      integer irl                                     ! Local variables
      real*8 eks,ekenp,rnnolp

      if (iarg.le.4) then
        irl=ir(np)
!   Pick up current region number
        escore(irl)=escore(irl)+edep*wt(np)
      end if
      
      if (iarg.eq.7) then   ! Apply Leading Particle Biasing for bremss.
        eks=e(np)+e(np-1)-RM   ! Kinetic energy before bremss.
        ekenp=e(np)
        if(iq(np).ne.0) ekenp=e(np)-RM
        call randomset(rnnolp)
        if (rnnolp.lt.ekenp/eks) then  ! Follow np
          e(np-1)=e(np)
          iq(np-1)=iq(np)
          u(np-1)=u(np)
          v(np-1)=v(np)
          w(np-1)=w(np)
        end if
        ekenp=e(np-1)
        if (iq(np-1).ne.0) ekenp=e(np-1)-RM
        wt(np-1)=wt(np-1)*eks/ekenp
        np=np-1
      end if
      
      if (iarg.eq.16) then  ! Apply Leading Particle Biasing for pair.
        eks=e(np)+e(np-1)-2.0*RM
        ekenp=e(np)-RM
        call randomset(rnnolp)
        if (rnnolp.lt.ekenp/eks) then ! Follow np
          e(np-1)=e(np)
          iq(np-1)=iq(np)
          u(np-1)=u(np)
          v(np-1)=v(np)
          w(np-1)=w(np)
        end if
        ekenp=e(np-1)-RM
        wt(np-1)=wt(np-1)*eks/ekenp
        np=np-1
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
!   The user can terminate a history by setting idisc>0. Here we
!   terminate all histories which enter region 3 or are going
!   backwards in region 1
!
!                   |               |
!   Region 1        |   Region 2    |       Region 3
!                   |               |
!   photon =====>   |               | e- or photon ====> 
!                   |               |
!   vacuum          |     NaI       |       vacuum 
!                                                                       
! DESCRIPTION - PLAN2P is generally called from subroutine HOWFAR       
!   whenever a particle is in a region bounded by two planes that       
!   ARE parallel.  Both subroutines PLANE1 and CHGTR are called         
!   by PLAN2P (the second PLANE1 call is not made if the first          
!   plane is hit, or if the trajectory is parallel).                
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

      include 'include/egs5_epcont.f'    ! epcont contains irnew, ustep
                                         ! and idisc
      include 'include/egs5_stack.f'     ! stack contains x, y, z, u, v,
                                         ! w, ir and np

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
