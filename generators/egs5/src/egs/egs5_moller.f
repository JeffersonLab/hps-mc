!-----------------------------egs5_moller.f-----------------------------
! Version: 051219-1435
! Reference: SLAC-R-730/KEK-2005-8
!-----------------------------------------------------------------------
!23456789|123456789|123456789|123456789|123456789|123456789|123456789|12

      subroutine moller
      
      implicit none

      include 'include/egs5_h.f'               ! Main EGS5 "header" file

      include 'include/egs5_brempr.f'    ! COMMONs required by EGS5 code
      include 'include/egs5_edge.f'
      include 'include/egs5_eiicom.f'
      include 'include/egs5_elecin.f'
      include 'include/egs5_epcont.f'
      include 'include/egs5_misc.f'
      include 'include/egs5_stack.f'
      include 'include/egs5_thresh.f'
      include 'include/egs5_uphiot.f'
      include 'include/egs5_useful.f'

      include 'include/counters.f'       ! Additional (non-EGS5) COMMONs

      real*8 rnnow                                           ! Arguments
      integer iarg

      real*8                                           ! Local variables
     * eie,                          ! Total energy of incident electron
     * ekin,                       ! Kinetic energy of incident electron
     * ekse2,                  ! Kinetic energy of secondary electron #2
     * ese1,                     ! Total energy of secondary electron #1
     * ese2,                     ! Total energy of secondary electron #2
     * t0,e0,extrae,e02,ep0,g2,g3,gmax,br,r,rejf,h1,dcosth,eiir,elke2

      integer ifun,lelke2

      imoller = imoller + 1                ! Count entry into subroutine
 
      eie   = e(np)
      ekin  = eie - RM
      t0    = ekin/RM
      e0    = t0 + 1.0
      extrae= eie - thmoll(medium)
      e02   = e0*e0 
      ep0   = te(medium)/ekin
      g2    = t0*t0/e02
      g3    = (2.0*t0 + 1.0)/e02
      gmax  = (1.0 + 1.25*g2)
                                            ! Sample/reject to obtain br
1     continue
        call randomset(rnnow)
        br = te(medium)/(ekin - extrae*rnnow)
         r = br/(1.0 - br)

        ! Decide whether or not to accept
        call randomset(rnnow)
        rejf = 1.0 + g2*br*br + r*(r - g3)
        rnnow = gmax*rnnow
        if (rnnow .gt. rejf) go to 1
                                                  ! Divide up the energy
      ekse2 = br*ekin
      ese1 = eie - ekse2
      ese2 = ekse2 + RM
      e(np) = ese1
      e(np+1) = ese2
                                            ! Moller angles are uniquely
                                            ! determined by kinematics
      h1 = (eie + RM)/ekin
      dcosth = h1*(ese1 - RM)/(ese1 + RM)
      sinthe = sqrt(1.0 - dcosth)
      costhe = sqrt(dcosth)
      call uphi(2,1)                             ! Set direction cosines
      np = np + 1
      iq(np)=-1
      dcosth = h1*(ese2 - RM)/(ese2 + RM)
      sinthe = - sqrt(1.0 - dcosth)
      costhe = sqrt(dcosth)
      call uphi(3,2)                             ! Set direction cosines
      k1step(np) = 0.
      k1init(np) = 0.
      k1rsd(np) = 0.

!     --------------------------
!     Electron impact ionization
!     --------------------------
      if (impacr(ir(np)) .eq. 1.and. iedgfl(ir(np)) .ne. 0) then
        call randomset(rnnow)
        eke = eie - RM
        elke2 = log(eke)
        lelke2 = eico1(medium)*elke2 + eico0(medium)
        do ifun=1,nepm(medium)
          eiir = eii1(lelke2,ifun,medium)*elke2 + 
     *           eii0(lelke2,ifun,medium)
          if (rnnow .lt. eiir) then

            iarg = 25                                  ! BEFORE call eii
!                                      =================
            if (iausfl(iarg+1) .ne. 0) call ausgab(iarg)
!                                      =================
            iz = zelem(medium,ifun)
!           ========
            call eii
!           ========
            iarg = 26                                   ! AFTER call eii
!                                      =================
            if (iausfl(iarg+1) .ne. 0) call ausgab(iarg)
!                                      =================
            return
          end if
        end do
      end if
                                                      ! ----------------
      return                                          ! Return to ELECTR
                                                      ! ----------------
      end

!-----------------------last line of egs5_moller.f----------------------
