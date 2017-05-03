!-----------------------------------------------------------------------
!                       SUBROUTINE INTEG
!  Version: 051219-1435
!  Reference:  Based on code developed and provided by F. Salvat
!              for computing GS Mult Scat with PW cross sections
!-----------------------------------------------------------------------
!23456789|123456789|123456789|123456789|123456789|123456789|123456789|12

      SUBROUTINE INTEG(X,A,B,C,D,XL,XU,SUM,N)
C
C  Computes the integral of a cubic spline function.
C
C  Input:
C     X(I) (I=1:N) ... grid points (the X values must be in increasing
C                      order).
C     A(1:N),B(1:N),C(1:N),D(1:N) ... spline coefficients.
C     N  ............. number of grid points.
C     XL ............. lower limit in the integral.
C     XU ............. upper limit in the integral.
C  Output:
C     SUM ............ value of the integral.
C
      implicit none

      integer n
      double precision  X(N),A(N),B(N),C(N),D(N), xl, xu, sum

      integer i, il, iu
      double precision x1,x2, xll, xuu, sign, sump

C  ****  Set integration limits in increasing order.
      IF(XU.GT.XL) THEN
        XLL=XL
        XUU=XU
        SIGN=1.0D0
      ELSE
        XLL=XU
        XUU=XL
        SIGN=-1.0D0
      ENDIF
C  ****  Check integral limits.
      IF(XLL.LT.X(1).OR.XUU.GT.X(N)) THEN
        WRITE(6,10)
   10   FORMAT(5X,'Integral limits out of range. Stop.')
      ENDIF
C  ****  Find involved intervals.
      SUM=0.0D0
      CALL FINDI(X,XLL,N,IL)
      CALL FINDI(X,XUU,N,IU)
C
      IF(IL.EQ.IU) THEN
C  ****  Only a single interval involved.
        X1=XLL
        X2=XUU
        SUM=X2*(A(IL)+X2*((B(IL)/2)+X2*((C(IL)/3)+X2*D(IL)/4)))
     1     -X1*(A(IL)+X1*((B(IL)/2)+X1*((C(IL)/3)+X1*D(IL)/4)))
      ELSE
C  ****  Contributions from several intervals.
        X1=XLL
        X2=X(IL+1)
        SUM=X2*(A(IL)+X2*((B(IL)/2)+X2*((C(IL)/3)+X2*D(IL)/4)))
     1     -X1*(A(IL)+X1*((B(IL)/2)+X1*((C(IL)/3)+X1*D(IL)/4)))
        IL=IL+1
        DO I=IL,IU
          X1=X(I)
          X2=X(I+1)
          IF(I.EQ.IU) X2=XUU
          SUMP=X2*(A(I)+X2*((B(I)/2)+X2*((C(I)/3)+X2*D(I)/4)))
     1        -X1*(A(I)+X1*((B(I)/2)+X1*((C(I)/3)+X1*D(I)/4)))
          SUM=SUM+SUMP
        ENDDO
      ENDIF
      SUM=SIGN*SUM
      RETURN
      END
