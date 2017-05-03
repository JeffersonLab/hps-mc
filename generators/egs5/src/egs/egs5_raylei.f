!-----------------------------egs5_raylei.f-----------------------------
! Version: 051219-1435
! Reference: SLAC-R-730/KEK-2005-8
!-----------------------------------------------------------------------
!23456789|123456789|123456789|123456789|123456789|123456789|123456789|12

      subroutine raylei

      implicit none

      include 'include/egs5_h.f'               ! Main EGS5 "header" file

      include 'include/egs5_misc.f'      ! COMMONs required by EGS5 code
      include 'include/egs5_photin.f'
      include 'include/egs5_stack.f'
      include 'include/egs5_thresh.f'
      include 'include/egs5_uphiot.f'
      include 'include/egs5_useful.f'

      include 'include/counters.f'       ! Additional (non-EGS5) COMMONs

      real*8 rnnow                                           ! Arguments

      real*8 x2,q2,csqthe,rejf,br                      ! Local variables
      integer lxxx

      iraylei = iraylei + 1                ! Count entry into subroutine

 1    continue
        call randomset(rnnow)
        lxxx = rco1(medium)*rnnow + rco0(medium)
        x2 = rsct1(lxxx,medium)*rnnow + rsct0(lxxx,medium)
        q2 = x2*RMSQ/(20.60744*20.60744)
        costhe = 1.-q2/(2.*e(np)*e(np))
        if (abs(costhe) .gt. 1.) go to 1
        csqthe = costhe*costhe
        rejf = (1. + csqthe)/2.
        call randomset(rnnow)
        if (rnnow .le. rejf) go to 2
      go to 1
 2    continue
      sinthe = sqrt(1. - csqthe)
      if (lpolar(ir(np)) .eq. 0) then
        call uphi(2,1)
      else
        br = 1.
        call aphi(br)
        call uphi(3,1)
      end if
                                                      ! ----------------
      return                                          ! Return to PHOTON
                                                      ! ----------------
      end

!-----------------------last line of egs5_raylei.f----------------------
