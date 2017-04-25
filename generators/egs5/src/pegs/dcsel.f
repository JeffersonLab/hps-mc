!-----------------------------------------------------------------------
!                       FUNCTION DCSEL
!  Version: 051219-1435
!  Reference:  Based on code developed and provided by F. Salvat
!              for computing GS Mult Scat with PW cross sections
!-----------------------------------------------------------------------
!23456789|123456789|123456789|123456789|123456789|123456789|123456789|12

      double precision function DCSEL(RMU)
!
!  This function computes the DCS in (cm**2/sr) by cubic spline inter-
!  polation in RMU=(1-cos(theta))/2.
!
!  includes required:
!    egs5_h
!    cdcsep
!    cdcspl

!  calls:
!    findi - get index

!  returns:
!    dcsel  - differential cross secion at given angle

      implicit none

      include 'include/egs5_h.f'
      include 'include/egs5_cdcsep.f'
      include 'include/egs5_cdcspl.f'

      double precision rmu
      integer i

      CALL FINDI(XMU,RMU,NREDA,I)
      DCSEL=EXP(RA(I)+RMU*(RB(I)+RMU*(RC(I)+RMU*RD(I))))

      RETURN
      END
