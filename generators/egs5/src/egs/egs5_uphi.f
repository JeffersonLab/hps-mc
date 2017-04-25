!------------------------------egs5_uphi.f------------------------------
! Version: 070720-1515
!          080425-1100
! Reference: SLAC-R-730/KEK-2005-8
!-----------------------------------------------------------------------
!23456789|123456789|123456789|123456789|123456789|123456789|123456789|12

      subroutine uphi(ientry,lvl)                                               

      implicit none

      include 'include/egs5_h.f'               ! Main EGS5 "header" file
      
      include 'include/egs5_epcont.f'    ! COMMONs required by EGS5 code
      include 'include/egs5_stack.f'
      include 'include/egs5_uphiin.f' ! Probably don't need this anymore
      include 'include/egs5_uphiot.f'

      include 'include/counters.f'       ! Additional (non-EGS5) COMMONs

      integer ientry,lvl,iarg                                ! Arguments

      real*8                                           ! Local variables
     * usav,vsav,wsav,sinpsi,sinps2,cosdel,sindel,
     * us,vs,cthet,cphi,phi,rnnow

      iuphi = iuphi + 1                    ! Count entry into subroutine

      iarg=21
      if (iausfl(iarg+1) .ne. 0) then
        call ausgab(iarg)
      end if

      go to (1,2,3),ientry
      go to 10

1     continue
      sinthe = sin(theta)
      cthet = PI5D2 - theta
      costhe = sin(cthet)

2     call randomset(rnnow)
      phi = rnnow*TWOPI
      sinphi = sin(phi)
      cphi = PI5D2 - phi
      cosphi = sin(cphi)

3     go to (4,5,6),lvl
      go to 10

4     usav = u(np)
      vsav = v(np)
      wsav = w(np)
      go to 7

6     usav = u(np-1)
      vsav = v(np-1)
      wsav = w(np-1)

5     x(np) = x(np-1)
      y(np) = y(np-1)
      z(np) = z(np-1)
      ir(np) = ir(np-1)
      wt(np) = wt(np-1)
      dnear(np) = dnear(np-1)
      latch(np) = latch(np-1)
      time(np) = time(np-1)
      k1step(np) = 0.d0
      k1init(np) = 0.d0
      k1rsd(np) = 0.d0

7     sinps2 = usav*usav + vsav*vsav
      if (sinps2 .lt. 1.e-20) then
        u(np) = sinthe*cosphi
        v(np) = sinthe*sinphi
        w(np) = wsav*costhe
      else
        sinpsi = sqrt(sinps2)
        us = sinthe*cosphi
        vs = sinthe*sinphi
        sindel = vsav/sinpsi
        cosdel = usav/sinpsi
        u(np) = wsav*cosdel*us - sindel*vs + usav*costhe
        v(np) = wsav*sindel*us + cosdel*vs + vsav*costhe
        w(np) = -sinpsi*us + wsav*costhe
      end if

      iarg=22
      if (iausfl(iarg+1) .ne. 0) then
        call ausgab(iarg)
      end if

      return

10    write(6,100) ientry,lvl
100   format(' Stopped in uphi with ientry,lvl=',2I6)

      stop
      end

!-----------------------last line of egs5_uphi.f------------------------
