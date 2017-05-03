!------------------------------egs5_pair.f------------------------------
! Version: 051219-1435
! Reference: SLAC-R-730/KEK-2005-8
!-----------------------------------------------------------------------
!23456789|123456789|123456789|123456789|123456789|123456789|123456789|12

      subroutine pair

      implicit none

      include 'include/egs5_h.f'               ! Main EGS5 "header" file

      include 'include/egs5_brempr.f'    ! COMMONs required by EGS5 code
      include 'include/egs5_stack.f'
      include 'include/egs5_thresh.f'
      include 'include/egs5_uphiot.f'
      include 'include/egs5_useful.f'

      include 'include/counters.f'       ! Additional (non-EGS5) COMMONs

      real*8 rnnow,rnnow1,rnnow2,rnnow3                      ! Arguments

      real*8                                           ! Local variables
     * eig,                                  ! Energy of incident photon
     * ese1,                     ! Total energy of secondary electron #1
     * ese2,                     ! Total energy of secondary electron #2
     * br,del,delta,rejf,ese,pse,ztarg,tteig,ttese,ttpse,esedei,eseder,
     * ximin,rejmin,ya,xitry,galpha,gbeta,ximid,rejmid,rejtop,xitst,
     * rejtst,rtest
      integer lvx,lvl0,lvl,ichrg

      ipair = ipair + 1                    ! Count entry into subroutine

      eig = e(np)
      if (eig .le. 2.1) then               ! Below 2.1 MeV (approximate)
        call randomset(rnnow)                 ! KEK method for smoothing
        ese2 = RM + rnnow*(eig/2. - RM)       !   connection at boundary
      else
        if (eig .lt. 50.) then                  ! Above 2.1 MeV - sample
          lvx = 1
          lvl0 = 0
        else
          lvx = 2
          lvl0 = 3
        end if
                                 ! Start of main sampling-rejection loop
1       continue
          call randomset(rnnow1)
          call randomset(rnnow)
                                              ! Start of 12(br-0.5)**2
                                              ! subdistribution sampling
          if (rnnow .ge. bpar(lvx,medium)) then
            lvl = lvl0 + 1
            call randomset(rnnow2)
            call randomset(rnnow3)
            br = 0.5*(1.0 - max(rnnow1,rnnow2,rnnow3))

                             ! Start of uniform subdistribution sampling
          else
            lvl = lvl0 + 3
            br = rnnow1*0.5
          end if
                                                 ! Check that br, Adelta
                                                 ! and Cdelta > 0
          if(eig*br .lt. RM) go to 1
          del = 1.0/(eig*br*(1.0 - br))
          if (del .ge. delpos(lvx,medium)) go to 1
          delta = delcm(medium)*del
          if (delta .lt. 1.0) then
            rejf = dl1(lvl,medium) + delta*(dl2(lvl,medium) +
     *             delta*dl3(lvl,medium))
          else
            rejf = dl4(lvl,medium) + dl5(lvl,medium)*
     *             log(delta + dl6(lvl,medium))
          end if
          call randomset(rnnow)
          if (rnnow .gt. rejf) go to 1

        ese2 = br*eig
      end if
                                        ! Set up secondary electron #1
                                        ! (electron #2 has lower energy)
      ese1 = eig - ese2
      e(np) = ese1
      e(np+1) = ese2
      k1step(np) = 0.
      k1init(np) = 0.
      k1rsd(np) = 0.
      k1step(np+1) = 0.
      k1init(np+1) = 0.
      k1rsd(np+1) = 0.
                                            ! Sample to get polar angles
                                            ! of secondary electrons
                              ! Sample lowest-order angular distribution
      if ((iprdst .eq. 1) .or.
     *     ((iprdst .eq. 2) .and. (eig .lt. 4.14))) then

        do ichrg=1,2
          if (ichrg .eq. 1) then
            ese = ese1
          else
            ese = ese2
          end if
          pse = sqrt(max(0.D0,(ese - RM)*(ese + RM)))
          call randomset(rnnow)
          costhe = 1.0 - 2.0*rnnow
          sinthe = RM*sqrt((1.0 - costhe)*(1.0 + costhe))/
     *             (pse*costhe + ese)
          costhe = (ese*costhe + pse)/(pse*costhe + ese)
          if (ichrg .eq. 1) then
            call uphi(2,1)
          else
            np = np + 1
            sinthe = -sinthe
            call uphi(3,2)
          end if
        end do

        call randomset(rnnow)
        if (rnnow .le. 0.5) then
          iq(np) = 1
          iq(np-1) = -1
        else
          iq(np) = -1
          iq(np-1) = 1
        end if
        return
                                           ! Sample from Motz-Olsen-Koch
                                           ! (1969) distribution
      else if ((iprdst .eq. 2) .and.
     *         (eig .ge. 4.14)) then
        ztarg = zbrang(medium)
        tteig = eig/RM

        do ichrg=1,2
          if (ichrg .eq. 1) then
            ese = ese1
          else
            ese = ese2
          end if
          ttese = ese/RM
          ttpse = sqrt((ttese - 1.0)*(ttese + 1.0))
          esedei = ttese/(tteig - ttese)
          eseder = 1.0/esedei
          ximin = 1.0/(1.0 + (PI*ttese)**2)
          rejmin = 2.0 + 3.0*(esedei + eseder) - 4.00*(esedei +
     *             eseder + 1.0 - 4.0*(ximin - 0.5)**2)*(1.0 +
     *             0.25*log(((1.0 + eseder)*(1.0 + esedei)/
     *             (2.0*tteig))**2 + ztarg*ximin**2))
          ya = (2.0/tteig)**2
          xitry = max(0.01D0,max(ximin,min(0.5D0,sqrt(ya/ztarg))))
          galpha = 1.0 + 0.25*log(ya + ztarg*xitry**2)
          gbeta = 0.5*ztarg*xitry/(ya + ztarg*xitry**2)
          galpha = galpha - gbeta*(xitry - 0.5)
          ximid = galpha/(3.0*gbeta)
          if (galpha .ge. 0.0) then
            ximid = 0.5 - ximid + sqrt(ximid**2 + 0.25)
          else
            ximid = 0.5 - ximid - sqrt(ximid**2+0.25)
          end if
          ximid = max(0.01D0,max(ximin,min(0.5D0,ximid)))
          rejmid = 2.0 + 3.0*(esedei + eseder) - 4.0*(esedei +
     *             eseder + 1.0 - 4.0*(ximid - 0.5)**2)*(1.0 +
     *             0.25*log(((1.0 + eseder)*(1.0 + esedei)/
     *             (2.0*tteig))**2 + ztarg*ximid**2))
          rejtop = 1.02*max(rejmin,rejmid)

2         continue
            call randomset(xitst)
            rejtst = 2.0 + 3.0*(esedei + eseder) - 4.0*(esedei +
     *               eseder + 1.0 - 4.0*(xitst - 0.5)**2)*(1.0 +
     *               0.25*log(((1.0 + eseder)*(1.0 + esedei)/
     *               (2.0*tteig))**2 + ztarg*xitst**2))
            call randomset(rtest)
            theta = sqrt(1.0/xitst - 1.0)/ttese
            if ((rtest .gt. (rejtst/rejtop) .or.
     *          (theta .ge. PI))) go to 2

          sinthe=sin(theta)
          costhe=cos(theta)
          if (ichrg .eq. 1) then
            call uphi(2,1)
          else
            np = np+1
            sinthe = -sinthe
            call uphi(3,2)
          end if
          end do

        call randomset(rnnow)
        if (rnnow .le. 0.5) then
          iq(np) = 1
          iq(np-1) = -1
        else
          iq(np) = -1
          iq(np-1) = 1
        end if
        return

      ! Polar angle is m/k (default)
      else
        theta=RM/eig
      end if

      call uphi(1,1)             ! Set direction cosines for electron #1

      np = np + 1
      sinthe = -sinthe

      call uphi(3,2)             ! Set direction cosines for electron #2

                          ! Randomly decide which particle is "positron"
      call randomset(rnnow)
      if (rnnow .le. 0.5) then
        iq(np) = 1
        iq(np-1) = -1
      else
        iq(np) = -1
        iq(np-1) = 1
      end if
                                                      ! ----------------
      return                                          ! Return to PHOTON
                                                      ! ----------------
      end

!------------------------last line of egs5_pair.f-----------------------
