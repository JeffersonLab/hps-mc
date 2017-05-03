!-----------------------------egs5_photo.f------------------------------
! Version: 051219-1435
!          080425-1100   Add time as the time after start.
! Reference: SLAC-R-730/KEK-2005-8
!-----------------------------------------------------------------------
!23456789|123456789|123456789|123456789|123456789|123456789|123456789|12

      subroutine photo

      implicit none

      include 'include/egs5_h.f'               ! Main EGS5 "header" file

      include 'include/egs5_bounds.f'    ! COMMONs required by EG55 code
      include 'include/egs5_brempr.f'
      include 'include/egs5_edge.f'
      include 'include/egs5_epcont.f'
      include 'include/egs5_media.f'
      include 'include/egs5_misc.f'
      include 'include/egs5_photin.f'
      include 'include/egs5_stack.f'
      include 'include/egs5_uphiot.f'
      include 'include/egs5_useful.f'
      include 'include/egs5_uservr.f'
      include 'include/egs5_userxt.f'

      include 'include/counters.f'       ! Additional (non-EGS5) COMMONs

      real*8 rnnow                                           ! Arguments
      integer iarg

      real*8                                           ! Local variables
     * crosk(MXEL),crosl1(MXEL),crosl2(MXEL),
     * crosl3(MXEL),crosm(MXEL),tcros(MXEL),bshk(MXEL),
     * bshl1(MXEL),bshl2(MXEL),bshl3(MXEL),pbran(MXEL),
     * eig,eigk,phol,pholk,pholk2,pholk3,total,eelec,
     * alpha,beta,gamma,ratio,rnpht,fkappa,xi,sinth2

      integer
     * irl,i,noel,iphot,ielec

      iphoto = iphoto + 1                  ! Count entry into subroutine

      nxray = 0
      nauger = 0
      irl = ir(np)
      eig = e(np)
      phol = log(eig)
      medium = med(irl)

! Calculate energy dependent sub-shell ratio
      eigk = eig*1000.D0
      pholk = log(eigk)
      pholk2 = pholk*pholk
      pholk3 = pholk2*pholk
      total = 0.

      do i=1,nne(medium)          ! Shell-wise photoelectric calculation
        iz = zelem(medium,i)
        if (eigk .le. eedge(1,iz)) then
          crosk(i) = 0.
        else
          crosk(i) = exp(pm0(1,iz) + pm1(1,iz)*pholk +
     *                   pm2(1,iz)*pholk2 + pm3(1,iz)*pholk3)
        end if
        if (pm0(2,iz) .eq. 0. .or. eigk .le. eedge(2,iz)) then
          crosl1(i) = 0.
        else
          crosl1(i) = exp(pm0(2,iz) + pm1(2,iz)*pholk +
     *                    pm2(2,iz)*pholk2 + pm3(2,iz)*pholk3)
        end if
        if (pm0(3,iz) .eq. 0. .or. eigk .le. eedge(3,iz)) then
          crosl2(i) = 0.
        else
          crosl2(i) = exp(pm0(3,iz) + pm1(3,iz)*pholk +
     *                    pm2(3,iz)*pholk2 + pm3(3,iz)*pholk3)
        end if
        if (pm0(4,iz) .eq. 0. .or. eigk .le. eedge(4,iz)) then
          crosl3(i) = 0.
        else
          crosl3(i) = exp(pm0(4,iz) + pm1(4,iz)*pholk +
     *                    pm2(4,iz)*pholk2 + pm3(4,iz)*pholk3)
        end if
        if (eigk .le. embind(iz)) then
          tcros(i) = 0.
        else
          if (pm0(5,iz) .eq. 0.) then
            crosm(i) = 0.
          else
            crosm(i) = exp(pm0(5,iz) + pm1(5,iz)*pholk +
     *                     pm2(5,iz)*pholk2 + pm3(5,iz)*pholk3)
          end if
          tcros(i) = crosk(i) + crosl1(i) + crosl2(i) +
     *               crosl3(i) + crosm(i)
          bshk(i) = crosk(i)/tcros(i)
          bshl1(i) = (crosk(i) + crosl1(i))/tcros(i)
          bshl2(i) = (crosk(i) + crosl1(i) + crosl2(i))/tcros(i)
          bshl3(i) = (tcros(i) - crosm(i))/tcros(i)
        end if
        tcros(i) = tcros(i)*pz(medium,i)
        total = total + tcros(i)
      end do

      if (total .eq. 0.) then            ! Below M-edge for all elements
        edep = eig
        iarg = 4                    ! Deposit all of the photon's energy

!                                  =================
        if (iausfl(iarg+1) .ne. 0) call ausgab(iarg)
!                                  =================
        e(np) = 0.
        return
      end if

      if (nne(medium) .eq. 1) then
        iz = zelem(medium,1)
        noel = 1
        go to 1
      end if

      do i=1,nne(medium)-1
        if (i .eq. 1) then
          pbran(i) = tcros(i)/total
        else
          pbran(i) = pbran(i-1) + tcros(i)/total
        end if
      end do

      call randomset(rnnow)
      do i=1,nne(medium)-1
        if (rnnow .le. pbran(i)) then
          iz = zelem(medium,i)
          noel = i
          go to 1
        end if
      end do

      iz = zelem(medium,nne(medium))
      noel = nne(medium)

 1    continue                  ! Determine K, L x-rays for each element
      if (eigk .le. eedge(4,iz)) then        ! Below L3 edge, treat as M
        ebind = embind(iz)*1.D-3
        edep = ebind
        go to 2                                          ! Below L3 edge
      end if

      call randomset(rnnow)                     ! Sample to decide shell
      if (rnnow .gt. bshl3(noel)) then   ! M,N,...absorption, treat as M
        ebind = embind(iz)*1.D-3
        edep = ebind
        go to 2
      else if(rnnow .le. bshk(noel)) then                 ! K absorption
!       ===========
        call kshell            
!       ===========
        ebind = eedge(1,iz)*1.D-3
      else if(rnnow .le. bshl1(noel)) then               ! L1 absorption
!       ==============
        call lshell(1)
!       ==============
      else if(rnnow .le. bshl2(noel)) then               ! L2 absorption
!       =============
        call lshell(2)
!       ==============
      else                                               ! L3 absorption
!       ==============
        call lshell(3)
!       ==============
      end if

      if (iedgfl(irl) .le. 0) nxray = 0
      if (iauger(irl) .le. 0) nauger = 0

      edep = ebind

      if (nxray .ge. 1) then
        do iphot=1,nxray
          edep = edep - exray(iphot)
        end do
      end if

      if (nauger .ge. 1) then
        do ielec=1,nauger
          edep = edep - eauger(ielec)
        end do
      end if

      if (edep .lt. 0.) edep = 0. ! To avoid numerical precision problem

 2    continue
      e(np) = edep
      iarg = 4                     ! Part of binding energy is deposited

!                                =================
      if (iausfl(iarg+1) .ne. 0) call ausgab(iarg)
!                                =================
! Set up particles
      iq(np) = -1                        ! Photoelectron (always set up)
      e(np) = eig - ebind + RM

      if (iphter(ir(np)) .eq. 1) then   ! Select photoelectron direction
        eelec = e(np)
        if (eelec .gt. ecut(ir(np))) then
          beta = sqrt((eelec - RM)*(eelec + RM))/eelec
          gamma = eelec/RM
          alpha = 0.5D0*gamma - 0.5D0 + 1.D0/gamma
          ratio = beta/alpha

 3        continue
            call randomset(rnnow)
            rnpht = 2.D0*rnnow - 1.D0
            if (ratio .le. 0.2D0) then
              fkappa = rnpht + 0.5D0*ratio*(1.D0 - rnpht)*(1.D0 + rnpht)
              costhe = (beta + fkappa)/(1.D0 + beta*fkappa)
              xi = 1.D0/(1.D0 - beta*costhe)
            else
              xi = gamma*gamma*(1.D0 + alpha*(sqrt(1.D0 +
     *             ratio*(2.D0*rnpht + ratio))- 1.D0))
              costhe = (1.D0 - 1.D0/xi)/beta
            end if
            sinth2 = max(0.D0,(1.D0 - costhe)*(1.D0 + costhe))
            call randomset(rnnow)
            if(rnnow .le. 0.5D0*(1.D0 + gamma)*sinth2*xi/gamma) go to 4
          go to 3

 4        continue
          sinthe = sqrt(sinth2)
          call uphi(2,1)
        end if
      end if
      
      if (nauger .ne. 0) then                   ! Set up Auger electrons
        do ielec=1,nauger
          np = np + 1
          e(np) = eauger(ielec) + RM
          iq(np) = -1
          call randomset(rnnow)
          costhe = 2.D0*rnnow - 1.D0
          sinthe = sqrt(1.D0 -costhe*costhe)
          u(np) = 0.
          v(np) = 0.
          w(np) = 1.D0
          call uphi(2,1)
          x(np) = x(np-1)
          y(np) = y(np-1)
          z(np) = z(np-1)
          ir(np) = ir(np-1)
          wt(np) = wt(np-1)
          time(np) = time(np-1)
          dnear(np) = dnear(np-1)
          latch(np) = latch(np-1)
          k1step(np) = 0.
          k1init(np) = 0.
          k1rsd(np) = 0.
        end do
      end if

      if (nxray .ne. 0) then                ! Set up fluorescent photons
        do iphot=1,nxray
          np = np + 1
          e(np) = exray(iphot)
          iq(np) = 0
          call randomset(rnnow)
          costhe = 2.D0*rnnow - 1.D0
          sinthe = sqrt(1.D0 - costhe*costhe)
          u(np) = 0.
          v(np) = 0.
          w(np) = 1.D0
          call uphi(2,1)
          x(np) = x(np-1)
          y(np) = y(np-1)
          z(np) = z(np-1)
          ir(np) = ir(np-1)
          wt(np) = wt(np-1)
          time(np) = time(np-1)
          dnear(np) = dnear(np-1)
          latch(np) = latch(np-1)
          k1step(np) = 0.
          k1init(np) = 0.
          k1rsd(np) = 0.
        end do
      end if
                                                      ! ----------------
      return                                          ! Return to PHOTON
                                                      ! ----------------
      end

!-----------------------last line of egs5_photo.f-----------------------
