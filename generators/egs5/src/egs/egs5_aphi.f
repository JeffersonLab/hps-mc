!------------------------------egs5_aphi.f------------------------------
! Version: 051219-1435
! Reference: SLAC-R-730/KEK-2005-8
!-----------------------------------------------------------------------
!23456789|123456789|123456789|123456789|123456789|123456789|123456789|12

      subroutine aphi(br)

      implicit none

      include 'include/egs5_h.f'               ! Main EGS5 "header" file

      include 'include/egs5_epcont.f'    ! COMMONs required by EGS5 code
      include 'include/egs5_stack.f'
      include 'include/egs5_uphiin.f'
      include 'include/egs5_uphiot.f'

      include 'include/counters.f'       ! Additional (non-EGS5) COMMONs

      real*8 br,rnnow                                        ! Arguments
      integer iarg

      real*8                                           ! Local variables
     * sinomg,cosomg,sindel,sinpsi,sinps2,comg,enorm,
     * cosdel,omg,cosph0,cph0,val,valloc,valmax,pnorm0,
     * sinph0,ph0,coseta,ceta,anormr,anorm2,sineta,eta,
     * ufa,vfa,wfa,ufb,vfb,wfb,asav,bsav,csav
      integer ldpola

      iaphi = iaphi + 1                    ! Count entry into subroutine

      iarg = 21
!                                =================
      if (iausfl(iarg+1) .ne. 0) call ausgab(iarg)
!                                =================

      pnorm0 = sqrt(u(np)*u(np) + v(np)*v(np) + w(np)*w(np))
      u(np) = u(np)/pnorm0
      v(np) = v(np)/pnorm0
      w(np) = w(np)/pnorm0
      valmax = br + 1./br

 1    continue
        call randomset(rnnow)
        ph0 = rnnow*twopi
        sinph0 = sin(ph0)
        cph0 = pi5d2 - ph0
        cosph0 = sin(cph0)
        valloc = sqrt(sinph0*sinph0 + cosph0*cosph0)
        sinph0 = sinph0/valloc
        cosph0 = cosph0/valloc
        val = (valmax - 2.*sinthe*sinthe*cosph0*cosph0)/valmax
        call randomset(rnnow)
        if (rnnow .le. val) go to 2
      go to 1

 2    continue
      anorm2 =costhe*costhe*cosph0*cosph0 + sinph0*sinph0
      call randomset(rnnow)
      if ((valmax - 2.)/(valmax - 2. + 2.*anorm2) .gt. rnnow .or.
     *     anorm2 .lt. 1.E-10) then
        ldpola = 1
      else
        ldpola = 0
      end if

      if (ldpola .eq. 1) then
        call randomset(rnnow)
        eta = rnnow*twopi
        sineta = sin(eta)
        ceta = pi5d2 - eta
        coseta = sin(ceta)
      else
        anormr = 1./sqrt(anorm2)
        sineta = -anormr*sinph0
        coseta = anormr*costhe*cosph0
      end if

      ufa = costhe*cosph0*coseta - sinph0*sineta
      vfa = costhe*sinph0*coseta + cosph0*sineta
      wfa = -sinthe*coseta

      asav = u(np)
      bsav = v(np)
      csav = w(np)

      sinps2 = asav*asav + bsav*bsav
      if (sinps2 .lt. 1.E-20) then
        cosomg = uf(np)
        sinomg = vf(np)
      else
        sinpsi = sqrt(sinps2)
        sindel = bsav/sinpsi
        cosdel = asav/sinpsi
        cosomg = cosdel*csav*uf(np) + sindel*csav*vf(np) - sinpsi*wf(np)
        sinomg = -sindel*uf(np) + cosdel*vf(np)
      end if

      enorm = sqrt(uf(np)*uf(np) + vf(np)*vf(np) + wf(np)*wf(np))
      if (enorm .lt. 1.E-4) then
        call randomset(rnnow)
        omg = rnnow*twopi
        sinomg = sin(omg)
        comg = pi5d2 - omg
        cosomg = sin(comg)
      end if

      cosphi = cosomg*cosph0 - sinomg*sinph0
      sinphi = sinomg*cosph0 + cosomg*sinph0

      ufb = cosomg*ufa - sinomg*vfa
      vfb = sinomg*ufa + cosomg*vfa
      wfb = wfa

      if (sinps2 .lt. 1.E-20) then
        uf(np) = ufb
        vf(np) = vfb
        wf(np) = wfb
      else
        uf(np) = cosdel*csav*ufb - sindel*vfb + asav*wfb
        vf(np) = sindel*csav*ufb + cosdel*vfb + bsav*wfb
        wf(np) = -sinpsi*ufb + csav*wfb
      end if
      enorm = sqrt(uf(np)*uf(np) + vf(np)*vf(np) + wf(np)*wf(np))
      uf(np) = uf(np)/enorm
      vf(np) = vf(np)/enorm
      wf(np) = wf(np)/enorm

      return

      end

!------------------------last line of egs5_aphi.f-----------------------
