!-----------------------------------------------------------------------
!                       SUBROUTINE LEGENP
!  Version: 051219-1435
!  Reference:  Based on code developed and provided by F. Salvat
!              for computing GS Mult Scat with PW cross sections
!-----------------------------------------------------------------------
!23456789|123456789|123456789|123456789|123456789|123456789|123456789|12

      SUBROUTINE LEGENP(X,PL,NL)
C
C  This subroutine computes the first NL Legendre polynomials for the
C  argument X, using their recurrence relation. PL is an array of phys-
C  ical dimension equal to NL or larger. On output PL(J), J=1:NL, con-
C  tains the value of the Legendre polynomial of degree (order) J-1.
C
      implicit none

      integer nl
      double precision x,  PL(NL)

      integer j
      double precision twox, f1, f2, d

      PL(1)=1.0D0
      PL(2)=X
      IF(NL.GT.2) THEN
        TWOX=2.0D0*X
        F1=X
        D=1.0D0
        DO J=3,NL
          F1=F1+TWOX
          F2=D
          D=D+1.0D0
          PL(J)=(F1*PL(J-1)-F2*PL(J-2))/D
        ENDDO
      ENDIF
      RETURN
      END
