!-----------------------------------------------------------------------
! Standalone test of the moller_v5 bound-electron routines (bndini,
! bndmol).  Generates exact free Moller collisions of a beam electron
! of total energy EBEAM along +z (secondary kinetic-energy fraction
! uniform in [XLO, XHI], random azimuth), puts them on the EGS5 stack
! as EGS5's moller routine does (np-1 = primary, np = secondary), calls
! bndmol and writes one line per collision to test_bound_moller.txt:
!   shell  Et  tx ty tz  E1 u1 v1 w1  E2 u2 v2 w2  E1' u1' v1' w1'  E2' u2' v2' w2'
! Usage (in a directory with the EGS5 'data' link and bound.dat):
!   echo "NEVT EBEAM XLO XHI SEED" | ./test_bound_moller.exe
!-----------------------------------------------------------------------
      program tbound

      implicit none

      include 'include/egs5_h.f'
      include 'include/egs5_stack.f'
      include 'include/egs5_useful.f'
      include 'include/randomm.f'

      integer NFINE, MSH
      parameter (NFINE = 8001, MSH = 200)
      integer ibound, nshb, ndebug, nbnd, nbskp, nbsoft, bslast
      real*8 ekbmin, bocc, bbind, bpgrid, bcdf, bpau, btlast
      common /bndcom/ bocc(MSH), bbind(MSH), bpgrid(NFINE),
     *                bcdf(NFINE,MSH), ekbmin, bpau, btlast(4),
     *                ibound, nshb, ndebug, nbnd, nbskp, nbsoft, bslast

      integer nevt, iev, iseed
      real*8 ebeam, xlo, xhi, rn, ekin, br, e1, e2, h1, c1, c2,
     *       s1, s2, phi, twopi, b1(4), b2(4)
      parameter (twopi = 6.283185307179586d0)

      read(5,*) nevt, ebeam, xlo, xhi, iseed
      RM = 0.51099895d0
      inseed = iseed
      luxlev = 1
      call rluxinit
      call bndini
      open(56, file='test_bound_moller.txt', status='unknown')

      do iev = 1, nevt
        call randomset(rn)
        ekin = ebeam - RM
        br = xlo + (xhi - xlo) * rn
        e2 = br * ekin + RM
        e1 = ebeam - br * ekin
        h1 = (ebeam + RM) / ekin
        c1 = sqrt(h1 * (e1 - RM) / (e1 + RM))
        c2 = sqrt(h1 * (e2 - RM) / (e2 + RM))
        s1 = sqrt(max(1.0d0 - c1**2, 0.0d0))
        s2 = sqrt(max(1.0d0 - c2**2, 0.0d0))
        call randomset(rn)
        phi = twopi * rn
        np = 2
        iq(1) = -1
        iq(2) = -1
        e(1) = e1
        u(1) = s1 * cos(phi)
        v(1) = s1 * sin(phi)
        w(1) = c1
        e(2) = e2
        u(2) = -s2 * cos(phi)
        v(2) = -s2 * sin(phi)
        w(2) = c2
        b1(1) = u(1)
        b1(2) = v(1)
        b1(3) = w(1)
        b1(4) = e(1)
        b2(1) = u(2)
        b2(2) = v(2)
        b2(3) = w(2)
        b2(4) = e(2)
        call bndmol
        write(56,100) bslast, btlast(4), btlast(1), btlast(2),
     *    btlast(3), b1(4), b1(1), b1(2), b1(3),
     *    b2(4), b2(1), b2(2), b2(3),
     *    e(1), u(1), v(1), w(1), e(2), u(2), v(2), w(2)
      end do
      close(56)
      call bndsum
100   format(i3,1p,20e24.16)
      end
