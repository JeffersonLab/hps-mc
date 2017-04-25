!------------------------------egs5_brems.f-----------------------------
! Version: 051219-1435
! Reference: SLAC-R-730/KEK-2005-8
!-----------------------------------------------------------------------
!23456789|123456789|123456789|123456789|123456789|123456789|123456789|12

      subroutine brems
      
      implicit none

      include 'include/egs5_h.f'               ! Main EGS5 "header" file

      include 'include/egs5_brempr.f'    ! COMMONs required by EGS5 code
      include 'include/egs5_stack.f'
      include 'include/egs5_thresh.f'
      include 'include/egs5_uphiot.f'
      include 'include/egs5_useful.f'

      include 'include/counters.f'       ! Additional (non-EGS5) COMMONs

      real*8 rnnow,rnnow1,rnnow2                             ! Arguments

      real*8                                           ! Local variables
     * eie,                          ! Total energy of incident electron
     * esg,                                 ! Energy of secondary photon
     * ese,                         ! Total energy of secondary electron
     * abrems,p,h,br,del,delta,rejf,ztarg,tteie,ttese,esedei,y2max,
     * rjarg1,rjarg2,rjarg3,y2tst,y2tst1,rejmin,rejmid,rejmax,rejtop,
     * rejtst,t

      integer lvx,lvl0,lvl,idistr

      real*8 AILN2,AI2LN2                             ! Local parameters
      data
     * AILN2/1.44269E0/,                                         ! 1/ln2
     * AI2LN2/0.7213475E0/                                      ! 1/2ln2

      ibrems = ibrems + 1                  ! Count entry into subroutine
 
      eie = e(np)
      np = np + 1
      if (eie .lt. 50.0) then        ! Choose Bethe-Heitler distribution
        lvx = 1
        lvl0 = 0      
      else         ! Choose Coulomb-corrected Bethe Heitler distribution
        lvx = 2
        lvl0 = 3
      end if
      abrems = float(int(AILN2*log(eie/ap(medium))))

                                 ! Start of main sampling-rejection loop
1     continue
        call randomset(rnnow)
                                              ! Start of (1-br)/br
                                              ! subdistribution sampling
        if (0.5 .lt. (abrems*alphi(lvx,medium) + 0.5)*rnnow) then
          call randomset(rnnow)
          idistr = abrems*rnnow
          p = pwr2i(idistr+1)
          lvl = lvl0 + 1 
          call randomset(rnnow)
          if (rnnow .ge. AI2LN2) then
2           continue
              call randomset(rnnow)
              call randomset(rnnow1)
              call randomset(rnnow2)
              h = max(rnnow1,rnnow2)
              br = 1.0 - 0.5*h
              if (rnnow .gt. 0.5/br) go to 2
          else
            call randomset(rnnow)
            br = rnnow*0.5
          end if
          br = br*p
                                 ! Start of 2br subdistribution sampling
        else
          call randomset(rnnow1)
          call randomset(rnnow2)
          br = max(rnnow1,rnnow2)
          lvl = lvl0 + 2
        end if

        esg = eie*br
        if (esg .lt. ap(medium)) go to 1

        ese = eie - esg
        if (ese .lt. RM) go to 1
        del = br/ese
                                                     ! Check that Adelta
                                                     ! and Bdelta > 0
        if (del .ge. delpos(lvx,medium)) go to 1
        delta = delcm(medium)*del
        if (delta .lt. 1.0) then
          rejf = dl1(lvl,medium) + delta*(dl2(lvl,medium) + 
     *           delta*dl3(lvl,medium))
        else
          rejf = dl4(lvl,medium) + dl5(lvl,medium)*
     *           log(delta + dl6(lvl,medium))
        end if
        call randomset(rnnow)
        if (rnnow .gt. rejf) go to 1
                                                     ! Set up new photon
      if (ibrdst .ne. 1) then             ! Polar angle is m/E (default)
        theta = RM/eie
      else                                          ! Sample polar angle
        ztarg = zbrang(medium)
        tteie = eie/RM
        ttese = ese/RM
        esedei = ttese/tteie
        y2max = (PI*tteie)**2
        rjarg1 = 1.0 + esedei**2
        rjarg2 = 3.0*rjarg1 - 2.0*esedei
        rjarg3 = ((1.0 - esedei)/(2.0*tteie*esedei))**2
        y2tst1 = (1.0 + 0.0e0)**2
        rejmin = (4.0 + log(rjarg3 + ztarg/y2tst1))*
     *           (4.0*esedei*0.0e0/y2tst1 - rjarg1) + rjarg2
        y2tst1 = (1.0 + 1.0e0)**2
        rejmid = (4.0 + log(rjarg3 + ztarg/y2tst1))*
     *           (4.0*esedei*1.0e0/y2tst1 - rjarg1) + rjarg2
        y2tst1 = (1.0 + y2max)**2
        rejmax = (4.0 + log(rjarg3 + ztarg/y2tst1))*
     *           (4.0*esedei*y2max/y2tst1 - rjarg1) + rjarg2
        rejtop = max(rejmin,rejmid,rejmax)

5       continue
          call randomset(rnnow)
          y2tst =rnnow/(1.0 - rnnow + 1.0/y2max)
          y2tst1 = (1.0 + y2tst)**2
          rejtst = (4.0 + log(rjarg3 + ztarg/y2tst1))*
     *             (4.0*esedei*y2tst/y2tst1 - rjarg1) + rjarg2
          call randomset(rnnow)
          if (rnnow .gt. (rejtst/rejtop)) go to 5
        theta = sqrt(y2tst)/tteie
      end if

      call uphi(1,3)                  ! Set direction cosines for photon
                            ! Put lowest energy particle on top of stack
      if (esg .le. ese) then
        iq(np) = 0
        e(np) = esg
        e(np-1) = ese
        k1step(np) = 0.
        k1init(np) = 0.
        k1rsd(np) = 0.
      else
        iq(np) = iq(np-1)
        iq(np-1) = 0
        e(np) = ese
        e(np-1) = esg
        t = u(np)
        u(np) = u(np-1)
        u(np-1) = t
        t = v(np)
        v(np) = v(np-1)
        v(np-1) = t
        t = w(np)
        w(np) = w(np-1)
        w(np-1) = t
        k1step(np) = k1step(np-1)
        k1step(np-1) = 0.
        k1init(np) = k1init(np-1)
        k1init(np-1) = 0.
        k1rsd(np) = k1rsd(np-1)
        k1rsd(np-1) = 0.

      end if
                                                      ! ----------------
      return                                          ! Return to ELECTR
                                                      ! ----------------
      end

!------------------------last line of egs5_brems.f----------------------
