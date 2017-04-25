!-----------------------------egs5_compt.f------------------------------
! Version: 051219-1435
! Reference: SLAC-R-730/KEK-2005-8
!-----------------------------------------------------------------------
!23456789|123456789|123456789|123456789|123456789|123456789|123456789|12

      subroutine compt
      
      implicit none

      include 'include/egs5_h.f'               ! Main EGS5 "header" file

      include 'include/egs5_bcomp.f'     ! COMMONs required by EGS5 code
      include 'include/egs5_epcont.f'
      include 'include/egs5_misc.f'
      include 'include/egs5_stack.f'
      include 'include/egs5_thresh.f'
      include 'include/egs5_uphiot.f'
      include 'include/egs5_useful.f'

      include 'include/counters.f'       ! Additional (non-EGS5) COMMONs

      real*8 rnnow,rnnow1,rnnow2,rnnow3                      ! Arguments
      integer iarg

      real*8                                           ! Local variables
     * eig,                                  ! Energy of incident photon
     * esg,                                 ! Energy of secondary photon
     * ese,                         ! Total energy of secondary electron
     * egp,br0i,alph1,alph2,sumalp,
     * br,a1mibr,temp,rejf3,psq,t,
     * f1,f2,f3,f4,f5,eps1,cpr,esg1,esg2,
     * etmp,xval,xlv,alamb,esedef,valloc,
     * qvalmx,esgmax,sxz
      integer ishell,iqtmp,lvallc,irloc,lxlv

      icompt = icompt + 1                  ! Count entry into subroutine

      irloc = ir(np)
      if (incohr(irloc) .ne. 1 .and. iprofr(irloc) .eq. 1) then
        write(6,101)
 101    FORMAT(' STOPPED IN SUBROUTINE COMPT',/, ' INCOHR(IR(NP)) should
     * be 1 whenever IPROFR(IR(NP)) = 1')
        stop
      end if

      eig = e(np)
      egp = eig/RM
      br0i =1. + 2.*egp
      alph1 = log(br0i)
      alph2 = egp*(br0i + 1.)/(br0i*br0i)
      sumalp = alph1 + alph2
                                    ! Start main sampling-rejection loop
1     continue
        call randomset(rnnow)
                                              ! Start of 1/br
                                              ! subdistribution sampling
        if (alph1 .ge. sumalp*rnnow) then
          call randomset(rnnow)
          br = exp(alph1*rnnow)/br0i
                                              ! Start of br
                                              ! subdistribution sampling
        else
          call randomset(rnnow1)
          call randomset(rnnow)
          if (egp .ge. (egp + 1.)*rnnow) then
            call randomset(rnnow)
            rnnow1 = max(rnnow1,rnnow)
          end if
          br = ((br0i - 1.)*rnnow1 + 1.)/br0i
        end if

        esg = br*eig                           ! Set up secondary photon

        a1mibr = 1. - br
        esedef = eig*a1mibr + RM
        temp = RM*a1mibr/esg
        sinthe = max(0.D0,temp*(2. - temp))         ! Prevent sinthe < 0
        call randomset(rnnow)
        rejf3 = 1. - br*sinthe/(1. + br*br)

        irloc = ir(np)
        if (incohr(irloc) .eq. 1) then
          medium = med(irloc)
          alamb = 0.012398520
          xval = sqrt(temp/2.)*eig/alamb
          if (xval .ge. 5.E-3 .and. xval .le. 80.) then
            xlv = log(xval)
            lxlv = sco1(medium)*xlv + sco0(medium)
            sxz = sxz1(lxlv,medium)*xlv + sxz0(lxlv,medium)
          else if (xval .gt. 80.) then
            sxz = 1.
          else
            sxz = 0.
          end if
          rejf3 = rejf3*sxz
        end if
        if (rnnow .gt. rejf3) go to 1

      sinthe = sqrt(sinthe)
      costhe = 1. - temp

      irloc = ir(np)
      if (iprofr(irloc) .eq. 1) then
        esgmax = eig - cpimev
        valloc = sqrt(eig*eig + esgmax*esgmax - 2.*eig*esgmax*costhe)
        qvalmx = (eig - esgmax - eig*esgmax*(1. - costhe)/RM)*
     *           137./valloc
        if (qvalmx .ge. 100.) then
          esg = eig/(1. + eig/RM*(1. - costhe))
          go to 2
        end if

 3      continue
        medium = med(irloc)
        call randomset(rnnow)

        if (icprof(medium) .eq. 1) then
          lvallc = cco1(medium)*rnnow + cco0(medium)
          cpr = cpr1(lvallc,medium)*rnnow + cpr0(lvallc,medium)
        end if

        if (icprof(medium) .eq. 3) then
          call randomset(rnnow1)
          do ishell=1,mxshel(medium)
            if (rnnow1 .le. elecno(ishell,medium)) go to 4
          end do
 4        continue
          lvallc = ccos1(medium)*rnnow + ccos0(medium)
          cpr = cprs1(lvallc,ishell,medium)*rnnow + 
     *          cprs0(lvallc,ishell,medium)
        end if

        f1 = (cpr/137.)*(cpr/137.)
        f2 = (1. - costhe)/RM
        f3 = (1. + f2*eig)*(1. + f2*eig) - f1
        f4 = (f1*costhe - 1. - f2*eig)
        f5 = f4*f4 - f3 + f1*f3
        eps1 = 0.0
        if (f5 .lt. eps1) go to 3
        esg1 = (-f4 - sqrt(f5))/f3*eig
        esg2 = (-f4 + sqrt(f5))/f3*eig
        call randomset(rnnow2)
        if (rnnow2 .lt. 0.5) then
          esg = esg1
        else
          esg = esg2
        end if
        if (icprof(medium) .eq. 3) esgmax = eig - capio(ishell,medium)
        if (esg .gt. esgmax .or. esg .lt. 0.) go to 3
        call randomset(rnnow3)
        if (esg .lt. esgmax*rnnow3) go to 3
 2      continue
      end if

      ese = eig - esg + RM

      if (iprofr(irloc) .eq. 1) then
        ese = ese - capio(ishell,medium)
        edep = capio(ishell,medium)
        etmp = e(np)
        e(np) = edep
        iqtmp = iq(np)
        iq(np) = 0

        iarg = 4
!                                  =================
        if (iausfl(iarg+1) .ne. 0) call ausgab(iarg)
!                                  =================
        e(np) = etmp
        iq(np) = iqtmp
      end if

      if (lpolar(irloc).eq. 0) then
!       ==============
        call uphi(2,1)
!       ==============
      else
!       =============
        call aphi(br)
!       =============
!       ==============
        call uphi(3,1)
!       ==============
      end if

      np = np + 1                            ! Set up secondary electron
      psq = esedef**2 - RMSQ

      if (psq .le. 0.0) then                 ! To avoid division by zero
        costhe = 0.0
        sinthe = -1.0
      else
        costhe = (ese + esg)*a1mibr/sqrt(psq)
        sinthe = -sqrt(max(0.D0,1.D0 - costhe*costhe))
      end if

      call uphi(3,2)                             ! Set direction cosines

                                            ! Put lowest energy particle
                                            ! on top of stack
      uf(np) = 0.
      vf(np) = 0.
      wf(np) = 0.
      k1step(np) = 0.
      k1init(np) = 0.
      k1rsd(np) = 0.
      k1step(np-1) = 0.
      k1init(np-1) = 0.
      k1rsd(np-1) = 0.
      
      if (ese .le. esg) then
        iq(np) = -1
        e(np) = ese
        e(np-1) = esg
      else
        iq(np) = 0
        iq(np-1) = -1
        e(np) = esg
        e(np-1) = ese
        t = u(np)
        u(np) = u(np-1)
        u(np-1) = t
        t = v(np)
        v(np) = v(np-1)
        v(np-1) = t
        t = w(np)
        w(np) = w(np-1)
        w(np-1) = t
        t=uf(np)
        uf(np) = uf(np-1)
        uf(np-1) = t
        t = vf(np)
        vf(np) = vf(np-1)
        vf(np-1) = t
        t = wf(np)
        wf(np) = wf(np-1)
        wf(np-1) = t
      end if
                                                      ! ----------------
      return                                          ! Return to PHOTON
                                                      ! ----------------
      end

!-----------------------last line of egs5_compt.f-----------------------
