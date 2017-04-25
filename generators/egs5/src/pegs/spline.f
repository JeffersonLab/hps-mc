!-----------------------------------------------------------------------
!                       SUBROUTINE SPLINE
!  Version: 051219-1435
!  Reference:  Based on code developed and provided by F. Salvat
!              for computing GS Mult Scat with PW cross sections
!-----------------------------------------------------------------------
!23456789|123456789|123456789|123456789|123456789|123456789|123456789|12

      SUBROUTINE SPLINE(X,Y,A,B,C,D,S1,SN,N)
C
C  Cubic spline interpolation of tabulated data.
C
C  Input:
C     X(I) (I=1:N) ... grid points (the X values must be in increasing
C                      order).
C     Y(I) (I=1:N) ... corresponding function values.
C     S1,SN .......... second derivatives at X(1) and X(N). The natural
C                      spline corresponds to taking S1=SN=0.
C     N .............. number of grid points.
C  Output:
C     A(1:N),B(1:N),C(1:N),D(1:N) ... spline coefficients.
C
C  The interpolating cubic polynomial in the I-th interval, from X(I) to
C  X(I+1), is
C               P(x) = A(I)+x*(B(I)+x*(C(I)+x*D(I)))
C
C  Reference: M.J. Maron, 'Numerical Analysis: a Practical Approach',
C             MacMillan Publ. Co., New York, 1982.
C
      implicit none

      integer N
      double precision  X(N),Y(N),A(N),B(N),C(N),D(N), S1, SN

      integer i, k, n1, n2
      double precision r, h, hi, si, si1 
C
      IF(N.LT.4) THEN
        WRITE(6,10) N
   10   FORMAT(5X,'Spline interpolation cannot be performed with',
     1    I4,' points. Stop.')
        STOP
      ENDIF
      N1=N-1
      N2=N-2
C  ****  Auxiliary arrays H(=A) and DELTA(=D).
      DO I=1,N1
        IF(X(I+1)-X(I).LT.1.0D-25) THEN
          WRITE(6,11)
   11     FORMAT(5X,'Spline x values not in increasing order. Stop.')
          STOP
        ENDIF
        A(I)=X(I+1)-X(I)
        D(I)=(Y(I+1)-Y(I))/A(I)
      ENDDO
C  ****  Symmetric coefficient matrix (augmented).
      DO I=1,N2
        B(I)=2.0D0*(A(I)+A(I+1))
        K=N1-I+1
        D(K)=6.0D0*(D(K)-D(K-1))
      ENDDO
      D(2)=D(2)-A(1)*S1
      D(N1)=D(N1)-A(N1)*SN
C  ****  Gauss solution of the tridiagonal system.
      DO I=2,N2
        R=A(I)/B(I-1)
        B(I)=B(I)-R*A(I)
        D(I+1)=D(I+1)-R*D(I)
      ENDDO
C  ****  The sigma coefficients are stored in array D.
      D(N1)=D(N1)/B(N2)
      DO I=2,N2
        K=N1-I+1
        D(K)=(D(K)-A(K)*D(K+1))/B(K-1)
      ENDDO
      D(N)=SN
C  ****  Spline coefficients.
      SI1=S1
      DO I=1,N1
        SI=SI1
        SI1=D(I+1)
        H=A(I)
        HI=1.0D0/H
        A(I)=(HI/6.0D0)*(SI*X(I+1)**3-SI1*X(I)**3)
     1      +HI*(Y(I)*X(I+1)-Y(I+1)*X(I))
     2      +(H/6.0D0)*(SI1*X(I)-SI*X(I+1))
        B(I)=(HI/2.0D0)*(SI1*X(I)**2-SI*X(I+1)**2)
     1      +HI*(Y(I+1)-Y(I))+(H/6.0D0)*(SI-SI1)
        C(I)=(HI/2.0D0)*(SI*X(I+1)-SI1*X(I))
        D(I)=(HI/6.0D0)*(SI1-SI)
      ENDDO
      RETURN
      END
