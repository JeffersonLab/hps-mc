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
