!-----------------------------------------------------------------------
!                       FUNCTION DCSN
!  Version: 051219-1435
!  Reference:  Based on code developed and provided by F. Salvat
!              for computing GS Mult Scat with PW cross sections
!-----------------------------------------------------------------------
!23456789|123456789|123456789|123456789|123456789|123456789|123456789|12

      double precision function DCSN(RMU)
C
C     Integrand of the IL-th transport coefficient.

      implicit none

      include 'include/egs5_csplcf.f'

      double precision  rmu, x, pl(1000)

      X=1.0D0-2.0D0*RMU
      CALL LEGENP(X,PL,IL)
      DCSN=EXP(ASPL+RMU*(BSPL+RMU*(CSPL+RMU*DSPL)))*PL(IL)

      RETURN
      END
