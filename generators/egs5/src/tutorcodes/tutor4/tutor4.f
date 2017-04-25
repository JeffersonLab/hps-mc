!***********************************************************************
!                                                                       
!                     **************                                    
!                     *            *                                    
!                     *  tutor4.f  *                                    
!                     *            *                                    
!                     **************                                    
!                                                                       
!  An EGS5 user code. It lists the particles escaping from the back     
!  of a 2 mm Si plate when a pencil beam of 2 MeV electrons           
!  is incident on it normally.                                          
!                                                                       
!  For SLAC-R-730/KEK Report 2005-8: A simple example which scores 
!  reflected, deposited, and transmitted particles and energy and 
!  demonstrates step-size selection  
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

      common/score/escore(3),iscore(3)
      real*8 escore
      integer iscore

      real*8 ein,xin,yin,zin,                        ! Arguments
     *       uin,vin,win,wtin
      integer iqin,irin

      real*8 anorm,total                       ! Local variables
      real
     * tarray(2),tt,tt0,tt1,cputime
      integer loop,i,j,ncase
      character*24 medarr(2)

      real etime

!     ----------
!     Open files
!     ----------
      open(UNIT= 6,FILE='egs5job.out',STATUS='unknown')

      do loop = 1,3

!     ====================
      call counters_out(0)
!     ====================

!-----------------------------------------------------------------------
! Step 2: pegs5-call
!-----------------------------------------------------------------------
!     ==============
      call block_set                 ! Initialize some general variables
!     ==============

! nmed and dunit default to 1, i.e. one medium and we work in cm

      if(loop.eq.3) then
        chard(1) = 0.20d0      !  optional, but recommended to invoke
        chard(2) = 0.20d0      !  automatic step-size control
      else 
        chard(1) = 0.00d0      !  optional, but recommended to invoke
        chard(2) = 0.00d0      !  automatic step-size control
      endif

      write(6,100) loop, chard(1)
100   FORMAT(72('*'),/,
     *'Initializing EGS5, loop = ',I1,': charD = ',f5.2,/,
     *72('*'),/)

      if(loop.eq.1) then
      
!     ---------------------------------
!     define media before calling PEGS5
!     ---------------------------------
      nmed=2
      medarr(1)='SI with long steps      '
      medarr(2)='SI with short steps     '

      do j=1,nmed
        do i=1,24
          media(i,j)=medarr(j)(i:i)
        end do
      end do  
!     ---------------------------------------------
!     Run KEK version of PEGS5 before calling HATCH
!     (method was developed by Y. Namito - 010306)
!     ---------------------------------------------

      write(6,110)
110   FORMAT(' PEGS5-call comes next'/)

!     ==========
      call pegs5
!     ==========

      endif

      if(loop.lt.3) then
        write(6,120) loop,medarr(loop)
120   FORMAT(' Using media number ',i1,', ',a24,' for this run',/)
      endif

!-----------------------------------------------------------------------
! Step 3: Pre-hatch-call-initialization
!-----------------------------------------------------------------------
      nreg=3
!     nreg : number of region

      med(1)=0
      med(3)=0
      if(loop.eq.2) then
        med(2)=2
      else
        med(2)=1
      endif
! Vacuum in regions 1 and 3, Si in region 2
      ecut(2)=0.700
! Terminate electron histories at .700 MeV in the plate
      pcut(2)=0.010
! Terminate   photon histories at 0.01 MeV in the plate
!             Only needed for region 2 since no transport elsewhere
!             ecut is total energy = 0.189   MeV kinetic energy

!     --------------------------------------------------------
!     Random number seeds.  Must be defined before call hatch
!     or defaults will be used.  inseed (1- 2^31)
!     --------------------------------------------------------
      luxlev=1
      inseed=1
      kount=0
      mkount=0
      do i = 1, 25
        isdext(i) = 0
      end do
      write(6,150) inseed
150   FORMAT(/,' inseed=',I12,5X,
     *         ' (seed for generating unique sequences of Ranlux)')

!     =============
      call rluxinit  ! Initialize the Ranlux random-number generator
!     =============

!-----------------------------------------------------------------------
! Step 4:  Determination-of-incident-particle-parameters
!-----------------------------------------------------------------------
! Define initial variables for 2 MeV beam of electrons incident
! perpendicular to the slab
      iqin=-1
!            Incident charge - electrons
!            2 MeV kinetic energy
      ein=2.d0 + RM
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

      write(6,160)
160   FORMAT(/' Start tutor4'/' Call hatch to get cross-section data')

!     ------------------------------
!     Open files (before HATCH call)
!     ------------------------------
      open(UNIT=KMPI,FILE='pgs5job.pegs5dat',STATUS='old')
      open(UNIT=KMPO,FILE='egs5job.dummy',STATUS='unknown')

      write(6,170)
170   FORMAT(/,' HATCH-call comes next',/)

!     ==========
      call hatch
!     ==========

!     ------------------------------
!     Close files (after HATCH call)
!     ------------------------------
      close(UNIT=KMPI)
      close(UNIT=KMPO)

!    Pick up cross section data for ta
      write(6,180) ae(1)-RM, ap(1)
180   FORMAT(/' Knock-on electrons can be created and any electron ',
     *'followed down to' /T40,F8.3,' MeV kinetic energy'/
     *' Brem photons can be created and any photon followed down to',
     */T40,F8.3,' MeV')
! Compton events can create electrons and photons below these cutoffs

!-----------------------------------------------------------------------
! Step 6:  Initialization-for-howfar
!-----------------------------------------------------------------------
      zbound=0.2
!     plate is 2 mm thick

!-----------------------------------------------------------------------
! Step 7:  Initialization-for-ausgab
!-----------------------------------------------------------------------
      do i=1,3
        iscore(i)=0
        escore(i)=0.d0
!  Zero scoring array before starting
      end do

!-----------------------------------------------------------------------
! Step 8:  Shower-call
!-----------------------------------------------------------------------
      tt=etime(tarray)
      tt0=tarray(1)

! Initiate the shower ncase times
      ncase=50000
      do i=1,ncase
        call shower(iqin,ein,xin,yin,zin,uin,vin,win,irin,wtin)
      end do

      tt=etime(tarray)
      tt1=tarray(1)
      cputime=tt1-tt0

!-----------------------------------------------------------------------
! Step 9:  Output-of-results
!-----------------------------------------------------------------------
      write(6,190) cputime,ncase
190   FORMAT('CPU time = ',1X,G15.5,' sec for ',I8,' cases')

      anorm = 100./float(ncase)
      write(6,200) iscore(1)*anorm,iscore(3)*anorm
200   FORMAT(/,
     *' Fraction of electrons reflected from plate=',T50,F10.1,'%',/,
     *' Fraction of electrons transmitted through plate=',T50,F10.1,'%')

! Normalize to % of total input energy
      anorm = 100./((ein-RM)*float(ncase))
      total=0.0
      do i=1,3
        total=total+escore(i)
      end do
      write(6,210) (escore(i)*anorm,i=1,3),total*anorm
210   FORMAT(/,/,
     *  ' Fraction of energy reflected from plate=',T50,F10.1,'%'
     */ ' Fraction of energy deposited in plate=',T50,F10.1,'%'/
     *' Fraction of energy transmitted through plate=',T50,F10.1,'%'/
     *T50,11('-')/' Total fraction of energy accounted for=', T50,
     *F10.1,'%'/)  

      end do  ! do four times through

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
! In this AUSGAB routine for TUTOR4, we score the energy deposited      
!  in the various regions and count transmitted and reflected
!  electrons.
!                                                                       
!  For IARG=0, an electron or photon step is about to occur and we      
!  score the energy deposited, if any. Note that only electrons         
!  deposit energy during a step, and due to our geometry, electrons     
!  only take steps in region 2 - however there is no need to check.     
!  For IARG=1,2 and 4, particles have been discarded for falling        
!  below various energy cutoffs and all their energy is deposited       
!  locally (in fact EDEP = particles kinetic energy).                   
!  For IARG=3, we are discarding the particle since it is in            
!  region 1 or 3, so score its energy, and if it is an electron,
!  score it's region.
!                                                                       
!***********************************************************************
      subroutine ausgab(iarg)

      implicit none

      include 'include/egs5_h.f'                ! Main EGS "header" file

      include 'include/egs5_epcont.f'    ! COMMONs required by EGS5 code
      include 'include/egs5_stack.f'

      common/score/escore(3), iscore(3)
      real*8 escore
      integer iscore

      integer iarg                                          ! Arguments

      integer irl                                     ! Local variables

      if (iarg.le.4) then
        irl=ir(np)
!   Pick up current region number
        escore(irl)=escore(irl)+edep
!   Pick up energy deposition/transmission/reflection
        if (iarg.eq.3 .and. iq(np).eq.-1) then
          iscore(irl)=iscore(irl)+1
!   Pick up electron transmission/reflection
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
!   vacuum          |     Si        |       vacuum 
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
!  We are in the Si plate - check the geometry
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
