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
