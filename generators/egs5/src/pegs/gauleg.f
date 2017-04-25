!-----------------------------------------------------------------------
!                       SUBROUTINE GAULEG
!  Version: 051219-1435
!  Reference:  Based on code developed and provided by F. Salvat
!              for computing GS Mult Scat with PW cross sections
!-----------------------------------------------------------------------
!23456789|123456789|123456789|123456789|123456789|123456789|123456789|12

      SUBROUTINE GAULEG(X,W,N)
C
C  This subroutine returns the abscissas X(1:N) and weights W(1:N) of
C  the Gauss-Legendre N-point quadrature formula.
C
      implicit none

      integer N
      double precision  X(N),W(N)

      integer i,j,m
      double precision xm,xl, z,z1, p1,p2,p3,pp

      double precision eps
      PARAMETER (EPS=1.0D-15)

      M=(N+1)/2
      XM=0.0d0
      XL=1.0d0
      DO I=1,M
        Z=COS(3.141592654D0*(I-0.25D0)/(N+0.5D0))
    1   CONTINUE
          P1=1.0D0
          P2=0.0D0
          DO J=1,N
            P3=P2
            P2=P1
            P1=((2.0D0*J-1.0D0)*Z*P2-(J-1.0D0)*P3)/J
          ENDDO
          PP=N*(Z*P1-P2)/(Z*Z-1.0D0)
          Z1=Z
          Z=Z1-P1/PP
        IF(ABS(Z-Z1).GT.EPS*ABS(Z)+EPS) GO TO 1
        X(I)=XM-XL*Z
        X(N+1-I)=XM+XL*Z
        W(I)=2.0D0*XL/((1.0D0-Z*Z)*PP*PP)
        W(N+1-I)=W(I)
      ENDDO
      RETURN
      END
