!-----------------------------egs5_bhabha.f-----------------------------
! Version: 051219-1435
! Reference: SLAC-R-730/KEK-2005-8
!-----------------------------------------------------------------------
!23456789|123456789|123456789|123456789|123456789|123456789|123456789|12

      subroutine bhabha
      
      implicit none

      include 'include/egs5_h.f'               ! Main EGS5 "header" file

      include 'include/egs5_stack.f'     ! COMMONs required by EGS5 code
      include 'include/egs5_thresh.f'
      include 'include/egs5_uphiot.f'
      include 'include/egs5_useful.f'

      include 'include/counters.f'       ! Additional (non-EGS5) COMMONs

      real*8 rnnow                                           ! Arguments

      real*8                                           ! Local variables
     * eip,                          ! Total energy of incident positron
     * ekin,                       ! Kinetic energy of incident positron
     * ekse2,                  ! Kinetic energy of secondary electron #2
     * ese1,                     ! Total energy of secondary electron #1
     * ese2,                     ! Total energy of secondary electron #2
     * t0,e0,yy,e02,betai2,ep0,ep0c,y2,yp,yp2,b4,b3,b2,b1,h1,
     * br,rejf,dcosth,re1,re2,remax,ttt

      ibhabha = ibhabha + 1                ! Count entry into subroutine
 
      eip   = e(np)
      ekin  = eip - RM      
      t0    = ekin/RM
      e0    = t0 + 1.0
      yy    = 1.0/(t0 + 2.0)
      e02   = e0*e0 
      betai2= e02/(e02-1.0)
      ep0   = te(medium)/ekin
      ep0c  = 1.0 - ep0
      y2    = yy*yy
      yp    = 1.0 - 2.0*yy
      yp2   = yp*yp
      b4    = yp2*yp
      b3    = b4 + yp2
      b2    = yp*(3.0 + y2)
      b1    = 2.0 - y2
      br    = ep0
      re1   = ep0c*(betai2-br*(b1-br*(b2-br*(b3-br*b4))))
      br    = 1.0
      re2   = ep0c*(betai2-br*(b1-br*(b2-br*(b3-br*b4))))
      remax = max(re1,re2)

                                            ! Sample/reject to obtain br
1     continue
        call randomset(rnnow)
        br = ep0/(1.0 - ep0c*rnnow)

                                       ! Decide whether or not to accept
        call randomset(rnnow)
        rejf = ep0c*(betai2-br*(b1-br*(b2-br*(b3-br*b4))))
        rejf = rejf/remax

        if (rnnow .gt.rejf) go to 1

                            ! If e- got more energy than e+, move the e+
                            ! pointer and reflect br (this puts e+ on
                            ! top of stack if it has less energy)
      if (br .lt. 0.5) then
        iq(np+1) = -1
        k1step(np+1) = 0.
        k1init(np+1) = 0.
        k1rsd(np+1) = 0.

      else
        iq(np) = -1
        iq(np+1) = 1
        br = 1.0 - br
        k1step(np+1) = k1step(np)
        k1init(np+1) = k1init(np)
        k1rsd(np+1) = k1rsd(np)
        k1step(np) = 0.
        k1init(np) = 0.
        k1rsd(np) = 0.
      end if

                                                  ! Divide up the energy
      br = max(br,0.D0)    ! Avoids possible negative energy (round off)
      ekse2 = br*ekin          ! Kinetic energy of secondary electron #2
      ese1 = eip - ekse2         ! Total energy of secondary electron #1
      ese2 = ekse2 + RM          ! Total energy of secondary electron #2
      e(np) = ese1
      e(np+1) = ese2
                                           ! Bhabha angles are uniquely 
                                           ! determined by kinematics
      h1 = (eip + RM)/ekin
      dcosth = h1*(ese1 - RM)/(ese1 + RM)
      ttt = 1.0 - dcosth
      if (ttt.le.0.0) then
        sinthe = 0.0
      else
        sinthe = sqrt(1.0 - dcosth)
      end if
      costhe = sqrt(dcosth)
      call uphi(2,1)                             ! Set direction cosines
      np = np + 1
      dcosth = h1*(ese2 - RM)/(ese2 + RM)
      ttt = 1.0 - dcosth
      if (ttt.le.0.0) then
        sinthe = 0.0
      else
        sinthe = -sqrt(1.0 - dcosth)
      end if
      costhe = sqrt(dcosth)
      call uphi(3,2)                             ! Set direction cosines
                                                      ! ----------------
      return                                          ! Return to ELECTR
                                                      ! ----------------
      end

!-----------------------last line of egs5_bhabha.f----------------------
