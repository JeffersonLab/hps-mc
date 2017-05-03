!----------------------------dcsstor.f----------------------------------
! Version: 060314-0825
!-----------------------------------------------------------------------
!23456789|123456789|123456789|123456789|123456789|123456789|123456789|12
!
!  To use the partial wave multiple scattering distribution from
!  Salvat in an efficient way, we need to call the precomputation
!  routine "elastino" after all the materials have been LAY'ed by
!  PEGS.  "elastino" typically treats one material at a time, and
!  uses data that is read in previously and loaded into commons
!  by "elinit", called once for each material by PEGS.  Thus, these 
!  two routines, dcsstor, and dcsload, are needed to first store
!  the material dependent differential cross sections, and then
!  load them back into the elastino commons as needed.

      subroutine dcsstor(ecut,emax)

      implicit none

      real*8 ecut, emax

      include 'include/egs5_h.f'
      include 'include/egs5_cdcsep.f'
      include 'include/egs5_mscon.f'

      include 'pegscommons/dcsstr.f'
      include 'pegscommons/molvar.f'
      include 'pegscommons/pmcons.f'
      include 'pegscommons/mxdatc.f'
      include 'pegscommons/mscom.f'

      integer i,j
C
      nsdcs = nsdcs + 1
      atomd(nsdcs) = an * rho / wm
      efrch(nsdcs) = efrach
      efrcl(nsdcs) = efracl
      do i = 1, 24
        mednam(nsdcs,i) = medium(i)
      end do
      do i = 1, negrid
        secs(nsdcs,i) = ecs(i)
        setcs1(nsdcs,i) = etcs1(i)
        setcs2(nsdcs,i) = etcs2(i)
        spcs(nsdcs,i) = pcs(i)
        sptcs1(nsdcs,i) = ptcs1(i)
        sptcs2(nsdcs,i) = ptcs2(i)
        do j= 1, nreda
          sedcs(nsdcs,i,j) = edcs(i,j)
          spdcs(nsdcs,i,j) = pdcs(i,j)
        end do
      end do

      egrdlo(nsdcs) = ecut
      egrdhi(nsdcs) = emax
      nlegmd(nsdcs) = nleg0

      return
      end
!-------------------------last line of dcsstor.f------------------------

!----------------------------dcsload.f----------------------------------
! Version: 051219-1435
!-----------------------------------------------------------------------
!23456789|123456789|123456789|123456789|123456789|123456789|123456789|12
      subroutine dcsload(n,ecut,emax,nleg0)
C
      include 'include/egs5_h.f'
      include 'include/egs5_cdcsep.f'

      include 'pegscommons/dcsstr.f'
      include 'pegscommons/mxdatc.f'

      integer n, nleg0
      double precision ecut, emax

      integer i,j
C
      do i = 1, 24
        medium(i) = mednam(n,i)
      end do
      do i = 1, negrid
        ecs(i) = secs(n,i)
        etcs1(i) = setcs1(n,i)
        etcs2(i) = setcs2(n,i)
        pcs(i) = spcs(n,i)
        ptcs1(i) = sptcs1(n,i)
        ptcs2(i) = sptcs2(n,i)
        do j= 1, nreda
          edcs(i,j) = sedcs(n,i,j)
          pdcs(i,j) = spdcs(n,i,j)
        end do
      end do

      ecut = egrdlo(n)
      emax = egrdhi(n)
      nleg0 = nlegmd(n)

      return
      end
!-------------------------last line of dcsload.f------------------------
