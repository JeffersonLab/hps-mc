!------------------------------egs5_mscat.f-----------------------------
! Version: 060313-1005
!          091105-0835   Replaced tvstep with tmstep
! Reference: SLAC-R-730/KEK-2005-8
!-----------------------------------------------------------------------
!23456789|123456789|123456789|123456789|123456789|123456789|123456789|12

      subroutine mscat

      implicit none

      include 'include/egs5_h.f'               ! Main EGS5 "header" file

      include 'include/egs5_elecin.f'    ! COMMONs required by EGS5 code
      include 'include/egs5_epcont.f'
      include 'include/egs5_misc.f'
      include 'include/egs5_ms.f'
      include 'include/egs5_mults.f'
      include 'include/egs5_media.f'
      include 'include/egs5_stack.f'
      include 'include/egs5_thresh.f'
      include 'include/egs5_uphiin.f'
      include 'include/egs5_uphiot.f'
      include 'include/egs5_useful.f'
      include 'include/egs5_userpr.f'
      include 'include/egs5_usersc.f'

      include 'include/counters.f'       ! Additional (non-EGS5) COMMONs

      real*8 rms1,rms2,rms3,rms4,rms5,rms6,rms7,rms8         ! Arguments

      real*8                                           ! Local variables
     * g21,g22,g31,g32,g2,g3,
     * bm1,bm2,bi,bmd,xr,eta,thr,cthet
      integer i21,i22,i31,i32

      integer iegrid, iamu, iprt, ik1, im
      real*8 decade,delog,demod, xmu, xi,  b1,c1, x1,x2, fject,fmax
      real*8 ktot

      imscat = imscat + 1                  ! Count entry into subroutine
      im = medium

!     GS multiple scattering distribution.  Optional, 
!     and kinetic energy must be less than 100 MeV.

      if(useGSD(im).ne.0 .and. eke.lt.msgrid(nmsgrd(im),im)) then

        if(iq(np) .eq. -1) then
          iprt = 1
        else
          iprt = 2
        endif

        !-->  find the correct energy interval
        delog = DLOG10(eke*1.d6)
        demod = MOD(delog,1.d0)
        decade = delog - demod
        iegrid = nmsdec(im) * (decade - initde(im) + demod) - jskip(im)

        if(iegrid .ge. nmsgrd(im)) iegrid = nmsgrd(im) - 1

        !-->  randommly select which energy point to use
        fject = (eke - msgrid(iegrid,im)) / 
     &                  (msgrid(iegrid+1,im) - msgrid(iegrid,im))
        call randomset(xi)
        if(xi .lt. fject) iegrid = iegrid +1

        !-->  find the correct K1 interval
        ktot = k1rsd(np) + k1init(np)
        ik1 = DLOG(ktot/k1grd(iprt,1)) / dk1log(iprt) + 1
        if(ik1 .gt. NK1) then
          ik1 = NK1-1
        else if(ik1 .le. 0) then
          ik1 = 1
        endif

        !-->  randommly select which interval to use
        fject = (ktot - k1grd(iprt,ik1)) / 
     &                  (k1grd(iprt,ik1+1) - k1grd(iprt,ik1))
        call randomset(xi)
        if(xi .lt. fject) ik1 = ik1 +1

        !-->  first check for no-scatter probability
        call randomset(xi)
        if(xi.lt.pnoscat(iprt,iegrid,ik1,im)) return

        !-->  get the angular interval 
        call randomset(xi)
        iamu = xi * neqp(im) + 1
        !-->  if we're in the last bin, get the sub-bin number
        if(iamu .eq. neqp(im)) then
          call randomset(xi)
          if(xi .lt. ecdf(2,iprt,iegrid,ik1,im)) then
            iamu = 1
          else
            call findi(ecdf(1,iprt,iegrid,ik1,im),xi,neqa(im)+1,iamu)
          endif
          iamu = iamu + neqp(im) - 1
        endif

        b1 = ebms(iamu,iprt,iegrid,ik1,im)
        eta = eetams(iamu,iprt,iegrid,ik1,im)
        x1 = eamu(iamu,iprt,iegrid,ik1,im)
        x2 = eamu(iamu+1,iprt,iegrid,ik1,im)

        c1 = (x2 + eta) / (x2 - x1)
!        fmax = 1.d0 + b1 * (x2 - x1)**2
        fmax = 1.d0 + .25d0 * b1 * (x2 - x1)**2

        !-->  rejection loop
 6      continue
          !-->  sample Wentzel shape part of fit
          call randomset(xi)
          xmu = ((eta * xi) + (x1 * c1)) / (c1 - xi)
          !-->  rejection test
          fject = 1.d0 + b1 * (xmu - x1) * (x2 - xmu)
          call randomset(xi)
          if(xi * fmax .gt. fject) go to 6 

        costhe = 1.d0 - 2.d0 * xmu 
        sinthe =  DSQRT(1.d0 - costhe * costhe)
        iskpms = 0
        return

      !--> user or lower limit initiated skip of MS
      else if (nomsct(ir(np)).eq.1 .or. iskpms.ne.0) then
        sinthe = 0.
        costhe = 1.
        theta = 0.
        noscat = noscat + 1
        iskpms = 0
        return

      end if

      xr = sqrt(gms*tmstep*b)

!   Set bi (B-inverse) that will be used in sampling
!   (bi must not be larger than 1/lambda=1/2)
      if (b .gt. 2.) then
        bi = 1./b
      else
        bi = 0.5
      end if
      bmd = 1. + 1.75*bi
      bm1 = (1. - 2./b)/bmd
      bm2 = (1. + 0.025*bi)/bmd

                 ! -----------------------------------------------------
 1    continue   ! Loop for Bethe correction factor (or other) rejection
                 ! -----------------------------------------------------
        call randomset(rms1)
        if (rms1 .le. bm1) then                           ! Gaussian, F1
          call randomset(rms2)
          if (rms2 .eq. 0.) then
            rms2 = 1.E-30
          end if
          thr = sqrt(max(0.D0,-log(rms2)))
        else if (rms1 .le. bm2) then                          ! Tail, F3
          call randomset(rms3)
          call randomset(rms4)
          eta = max(rms3,rms4)
          i31 = b0g31 + eta*b1g31
          g31 = g310(i31) + eta*(g311(i31) + eta*g312(i31))
          i32 = b0g32 + eta*b1g32
          g32 = g320(i32) + eta*(g321(i32) + eta*g322(i32))
          g3 = g31 + g32*bi                         ! Rejection function
          call randomset(rms5)
          if (rms5 .gt. g3) go to 1
          thr = 1./eta
        else           ! Central correction, F2
          call randomset(rms6)
          thr = rms6
          i21 = b0g21 + thr*b1g21
          g21 = g210(i21) + thr*(g211(i21) + thr*g212(i21))
          i22 = b0g22 + thr*b1g22
          g22 = g220(i22) + thr*(g221(i22) + thr*g222(i22))
          g2  = g21 + g22*bi                        ! Rejection function
          call randomset(rms7)
          if (rms7 .gt. g2) go to 1
        end if

        theta = thr*xr           ! Real angle (thr is the reduced angle)
        if (theta .ge. PI) go to 1
        sinthe = sin(theta)
        call randomset(rms8)
        if (rms8**2*theta .le. sinthe) go to 2
      go to 1

 2    continue
      cthet = PI5D2 - theta
      costhe = sin(cthet)
                                                      ! ----------------
      return                                          ! Return to ELECTR
                                                      ! ----------------
      end

!-----------------------last line of egs5_mscat.f-----------------------
