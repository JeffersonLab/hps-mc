!-----------------------------------------------------------------------
!                       FUNCTION SUMGA
!  Version: 051219-1435
!  Reference:  Based on code developed and provided by F. Salvat
!              for computing GS Mult Scat with PW cross sections
!-----------------------------------------------------------------------
!23456789|123456789|123456789|123456789|123456789|123456789|123456789|12

      double precision FUNCTION SUMGA(FCT,XL,XU,TOL)
C
C  This function calculates the value SUMGA of the integral of the
C  (external) function FCT over the interval (XL,XU) using the 20-point
C  Gauss quadrature method with an adaptive bipartition scheme.
C
C  TOL is the tolerance, i.e. maximum allowed relative error; it should
C  not exceed 1.0D-13. A warning message in written in unit 6 when the
C  required accuracy is not attained.
C
C                             Francesc Salvat. Barcelona, December 2000.
C
      implicit none

      integer NP, NST, NCALLS, I1, I2, I3, ICALL, LH, I, LHN
      PARAMETER(NP=10,NST=128,NCALLS=20000)
      double precision X(NP),W(NP),S(NST),SN(NST),XR(NST),XRN(NST),
     & CTOL, PTOL, ERR, XL, XU, TOL, A, B, C, D, H, HO, SUMR, FCT,
     & SI, XA, XB, XC, S1, S2, S12
      
      external FCT
C  ****  Gauss 20-point integration formula.
C  Abscissas.
      DATA X/7.6526521133497334D-02,2.2778585114164508D-01,
     1       3.7370608871541956D-01,5.1086700195082710D-01,
     2       6.3605368072651503D-01,7.4633190646015079D-01,
     3       8.3911697182221882D-01,9.1223442825132591D-01,
     4       9.6397192727791379D-01,9.9312859918509492D-01/
C  Weights.
      DATA W/1.5275338713072585D-01,1.4917298647260375D-01,
     1       1.4209610931838205D-01,1.3168863844917663D-01,
     2       1.1819453196151842D-01,1.0193011981724044D-01,
     3       8.3276741576704749D-02,6.2672048334109064D-02,
     4       4.0601429800386941D-02,1.7614007139152118D-02/
C  ****  Error control.
      CTOL=MIN(MAX(TOL,1.0D-13),1.0D-2)
      PTOL=0.01D0*CTOL
      ERR=1.0D35
C  ****  Gauss integration from XL to XU.
      H=XU-XL
      SUMGA=0.0D0
      A=0.5D0*(XU-XL)
      B=0.5D0*(XL+XU)
      C=A*X(1)
      D=W(1)*(FCT(B+C)+FCT(B-C))
      DO I1=2,NP
        C=A*X(I1)
        D=D+W(I1)*(FCT(B+C)+FCT(B-C))
      ENDDO
      ICALL=NP+NP
      LH=1
      S(1)=D*A
      XR(1)=XL
C  ****  Adaptive bipartition scheme.
    1 CONTINUE
      HO=H
      H=0.5D0*H
      SUMR=0.0D0
      LHN=0
      DO I=1,LH
        SI=S(I)
        XA=XR(I)
        XB=XA+H
        XC=XA+HO
        A=0.5D0*(XB-XA)
        B=0.5D0*(XB+XA)
        C=A*X(1)
        D=W(1)*(FCT(B+C)+FCT(B-C))
        DO I2=2,NP
          C=A*X(I2)
          D=D+W(I2)*(FCT(B+C)+FCT(B-C))
        ENDDO
        S1=D*A
        A=0.5D0*(XC-XB)
        B=0.5D0*(XC+XB)
        C=A*X(1)
        D=W(1)*(FCT(B+C)+FCT(B-C))
        DO I3=2,NP
          C=A*X(I3)
          D=D+W(I3)*(FCT(B+C)+FCT(B-C))
        ENDDO
        S2=D*A
        ICALL=ICALL+4*NP
        S12=S1+S2
        IF(ABS(S12-SI).LE.MAX(PTOL*ABS(S12),1.0D-25)) THEN
          SUMGA=SUMGA+S12
        ELSE
          SUMR=SUMR+S12
          LHN=LHN+2
          IF(LHN.GT.NST) GO TO 2
          SN(LHN)=S2
          XRN(LHN)=XB
          SN(LHN-1)=S1
          XRN(LHN-1)=XA
        ENDIF
        IF(ICALL.GT.NCALLS) GO TO 2
      ENDDO
      ERR=ABS(SUMR)/MAX(ABS(SUMR+SUMGA),1.0D-25)
      IF(ERR.LT.CTOL.OR.LHN.EQ.0) RETURN
      LH=LHN
      DO I=1,LH
        S(I)=SN(I)
        XR(I)=XRN(I)
      ENDDO
      GO TO 1
C  ****  Warning (low accuracy) message.
    2 CONTINUE
      WRITE(6,11)
   11 FORMAT(/2X,'>>> SUMGA. Gauss adaptive-bipartition quadrature.')
      WRITE(6,12) XL,XU,TOL
   12 FORMAT(2X,'XL =',1P,E19.12,',  XU =',E19.12,',  TOL =',E8.1)
      WRITE(6,13) ICALL,SUMGA,ERR,LHN
   13 FORMAT(2X,'NCALLS = ',I5,',  SUMGA =',1P,E20.13,',  ERR =',E8.1,
     1      /2X,'NUMBER OF OPEN SUBINTERVALS =',I3)
      WRITE(6,14)
   14 FORMAT(2X,'WARNING: the required accuracy has not been ',
     1  'attained.'/)
      RETURN
      END
