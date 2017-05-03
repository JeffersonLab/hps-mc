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
