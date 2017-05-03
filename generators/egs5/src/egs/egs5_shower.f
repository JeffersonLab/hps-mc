!-----------------------------egs5_shower.f-----------------------------
! Version: 080425-1100
! Reference: SLAC-R-730/KEK-2005-8
!-----------------------------------------------------------------------
!23456789|123456789|123456789|123456789|123456789|123456789|123456789|12

      subroutine shower(iqi,ei,xi,yi,zi,ui,vi,wi,iri,wti)

      implicit none

      include 'include/egs5_h.f'               ! Main EGS5 "header" file

      include 'include/egs5_stack.f'     ! COMMONs required by EGS5 code
      include 'include/egs5_uphiot.f'

      include 'include/counters.f'       ! Additional (non-EGS5) COMMONs

      real*8 ei,xi,yi,zi,ui,vi,wi,wti,rnnow                  ! Arguments
      integer iqi,iri,ircode

      real*8                                           ! Local variables
     * dneari,deg,dpgl,dei,dpi,dcsth,dcosth

      real*8 PI0MSQ
      data PI0MSQ/1.8215416E4/      ! Pi-zero rest mass squared (MeV**2)

      ishower = ishower + 1                ! Count entry into subroutine

      np = 1
      dneari = 0.0
      iq(1) = iqi
      e(1) = ei
      u(1) = ui
      v(1) = vi
      w(1) = wi
      x(1) = xi
      y(1) = yi
      z(1) = zi
      ir(1) = iri
      wt(1) = wti
      dnear(1) = dneari
      latch(1) = latchi
      deresid = 0.d0
      deinitial = 0.d0
      denstep = 0.d0
      k1step(1) = 0.d0
      k1init(1) = 0.d0
      k1rsd(1) = 0.d0
      time(1) = 0.d0

      if (iqi .eq. 2) then
        if (ei**2 .le. PI0MSQ) then
          write(6,10) ei
10        format(//,' Stopped in Subroutine Shower---pi-zero option invo
     *ked', /,' but the total energy was too small (ei=',g15.5,' MeV)')
          stop
        end if
        call randomset(rnnow)
        dcsth = rnnow
        dei = ei
        dpi = dsqrt(dei*dei - PI0MSQ)
        deg = dei + dpi*dcsth
        dpgl = dpi + dei*dcsth
        dcosth = dpgl/deg
        costhe = dcosth
        sinthe = dsqrt(1.d0 - dcosth*dcosth)
        iq(1) = 0
        e(1) = deg/2.
        call uphi(2,1)
        np = 2
        deg = dei - dpi*dcsth
        dpgl = dpi - dei*dcsth
        dcosth = dpgl/deg
        costhe = dcosth
        sinthe = -dsqrt(1.d0 - dcosth*dcosth)
        iq(2) = 0
        e(2) = deg/2.
        call uphi(3,2)
      end if

      !  For first-step scaling for electrons
      ircode = -1

1     continue
        if (iq(np) .eq. 0) go to 3
2       continue
          call electr(ircode)  
          if (ircode .eq. 2) go to 4
3         call photon(ircode)
          if (ircode .eq. 2) go to 4
        go to 2
4       continue
        if (np .gt. 0) go to 1

      return
      end

!-----------------------last line of egs5_shower.f----------------------
