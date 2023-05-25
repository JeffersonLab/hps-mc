ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c      written by the UFO converter
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

C     Parameters that should not be recomputed event by event.
C     
      IF(READLHA) THEN

        G = 2 * DSQRT(AS*PI)  ! for the first init

        MDL_PROTONMU = 2.790000D+00

        MDL_MPROTON = 9.380000D-01

        MDL_PROTONMU__EXP__2 = MDL_PROTONMU**2

        MDL_MPROTON__EXP__2 = MDL_MPROTON**2

        MDL_INELASTIC1 = (MDL_PROTONMU__EXP__2-1.000000D+00)
     $   /(4.000000D+00*MDL_MPROTON__EXP__2)

        MDL_INELASTIC2 = 1.000000D+00/7.100000D-01

        MDL_AVAL = 1.110000D+02*MDL_ZNUC**(-1.000000D+00/3.000000D+00)
     $   /MDL_YMEL

        MDL_DVAL = 1.640000D-01*MDL_ANUC**(-2.000000D+00/3.000000D+00)

        MDL_APVAL = 7.730000D+02*MDL_ZNUC**(-2.000000D+00/3.000000D+00)
     $   /MDL_YMEL

        MDL_COS__CABI = COS(MDL_CABI)

        MDL_CKM11 = MDL_COS__CABI

        MDL_SIN__CABI = SIN(MDL_CABI)

        MDL_CKM12 = MDL_SIN__CABI

        MDL_CKM21 = -MDL_SIN__CABI

        MDL_CKM22 = MDL_COS__CABI

        MDL_EPSILON__EXP__2 = MDL_EPSILON**2

        MDL_ETA = MDL_EPSILON/(SQRT(DCMPLX(1.000000D+00
     $   -MDL_EPSILON__EXP__2/(1.000000D+00-MDL_SWSQSM)))
     $   *SQRT(DCMPLX(1.000000D+00-MDL_SWSQSM)))

        MDL_MCHI1 = -MDL_DMCHI/2.000000D+00+MDL_MCHI

        MDL_MCHI2 = MDL_DMCHI/2.000000D+00+MDL_MCHI

        MDL_AX = 1.000000D+00/MDL_AXM1

        MDL_NB__2__EXP__0_25 = 2.000000D+00**2.500000D-01

        MDL_SQRT__GF = SQRT(DCMPLX(MDL_GF))

        MDL_V = 1.000000D+00/(MDL_NB__2__EXP__0_25*MDL_SQRT__GF)

        MDL_ETA__EXP__2 = MDL_ETA**2

        MDL_CHI = MDL_ETA/SQRT(DCMPLX(1.000000D+00+MDL_ETA__EXP__2))

        MDL_MW__EXP__2 = MDL_MW**2

        MDL_MAP__EXP__2 = MDL_MAP**2

        MDL_MZ__EXP__2 = MDL_MZ**2

        MDL_DZ = ((1.000000D+00+MDL_ETA__EXP__2)*(MDL_ETA__EXP__2
     $   *MDL_MW__EXP__2+MDL_MAP__EXP__2+MDL_MZ__EXP__2+(2.000000D+00
     $   *ATAN(DBLE(1.000000D+100*(MDL_MAP-MDL_MZ)))*SQRT(DCMPLX(
     $   -4.000000D+00*(1.000000D+00+MDL_ETA__EXP__2)*MDL_MAP__EXP__2
     $   *MDL_MZ__EXP__2+(MDL_ETA__EXP__2*MDL_MW__EXP__2
     $   +MDL_MAP__EXP__2+MDL_MZ__EXP__2)**2)))/PI))/(MDL_ETA__EXP__2
     $   *MDL_MW__EXP__2+MDL_MAP__EXP__2+MDL_MZ__EXP__2-(2.000000D+00
     $   *ATAN(DBLE(1.000000D+100*(MDL_MAP-MDL_MZ)))*SQRT(DCMPLX(
     $   -4.000000D+00*(1.000000D+00+MDL_ETA__EXP__2)*MDL_MAP__EXP__2
     $   *MDL_MZ__EXP__2+(MDL_ETA__EXP__2*MDL_MW__EXP__2
     $   +MDL_MAP__EXP__2+MDL_MZ__EXP__2)**2)))/PI)

        MDL_SQRT__2 = SQRT(DCMPLX(2.000000D+00))

        MDL_MZ0 = SQRT(DCMPLX((MDL_ETA__EXP__2*MDL_MW__EXP__2
     $   +MDL_MAP__EXP__2+MDL_MZ__EXP__2-(2.000000D+00
     $   *ATAN(DBLE(1.000000D+100*(MDL_MAP-MDL_MZ)))*SQRT(DCMPLX(
     $   -4.000000D+00*(1.000000D+00+MDL_ETA__EXP__2)*MDL_MAP__EXP__2
     $   *MDL_MZ__EXP__2+(MDL_ETA__EXP__2*MDL_MW__EXP__2
     $   +MDL_MAP__EXP__2+MDL_MZ__EXP__2)**2)))/PI)/(1.000000D+00
     $   +MDL_ETA__EXP__2)))/MDL_SQRT__2

        MDL_MH__EXP__6 = MDL_MH**6

        MDL_MT__EXP__6 = MDL_MT**6

        MDL_MH__EXP__4 = MDL_MH**4

        MDL_MT__EXP__4 = MDL_MT**4

        MDL_MH__EXP__2 = MDL_MH**2

        MDL_MT__EXP__2 = MDL_MT**2

        MDL_SQRT__AX = SQRT(DCMPLX(MDL_AX))

        MDL_GX = 2.000000D+00*MDL_SQRT__AX*SQRT(DCMPLX(PI))

        MDL_YB = (MDL_YMB*MDL_SQRT__2)/MDL_V

        MDL_YC = (MDL_YMC*MDL_SQRT__2)/MDL_V

        MDL_YE = (MDL_YMEL*MDL_SQRT__2)/MDL_V

        MDL_YM = (MDL_YMMU*MDL_SQRT__2)/MDL_V

        MDL_YT = (MDL_YMT*MDL_SQRT__2)/MDL_V

        MDL_YTAU = (MDL_YMTAU*MDL_SQRT__2)/MDL_V

        MDL_CW = MDL_MW/MDL_MZ0

        MDL_SQRT__DZ = SQRT(DCMPLX(MDL_DZ))

        MDL_MX = MDL_MZ0*MDL_SQRT__DZ

        MDL_MH__EXP__12 = MDL_MH**12

        MDL_MW__EXP__12 = MDL_MW**12

        MDL_MH__EXP__10 = MDL_MH**10

        MDL_MW__EXP__10 = MDL_MW**10

        MDL_MH__EXP__8 = MDL_MH**8

        MDL_MW__EXP__8 = MDL_MW**8

        MDL_MW__EXP__6 = MDL_MW**6

        MDL_MW__EXP__4 = MDL_MW**4

        MDL_CW__EXP__2 = MDL_CW**2

        MDL_SW = SQRT(DCMPLX(1.000000D+00-MDL_CW__EXP__2))

        MDL_CHI__EXP__2 = MDL_CHI**2

        MDL_XI = (MDL_MX*SQRT(DCMPLX(1.000000D+00-MDL_CHI__EXP__2)))
     $   /MDL_GX

        MDL_SW__EXP__2 = MDL_SW**2

        MDL_TA = -(-1.000000D+00+MDL_DZ+MDL_ETA__EXP__2*MDL_SW__EXP__2
     $   -(2.000000D+00*ATAN(DBLE(1.000000D+100*(-1.000000D+00+MDL_DZ))
     $   )*SQRT(DCMPLX(4.000000D+00*MDL_ETA__EXP__2*MDL_SW__EXP__2+(
     $   -1.000000D+00+MDL_DZ+MDL_ETA__EXP__2*MDL_SW__EXP__2)**2)))/PI)
     $   /(2.000000D+00*MDL_ETA*MDL_SW)

        MDL_MHS__EXP__2 = MDL_MHS**2

        MDL_KAP__EXP__2 = MDL_KAP**2

        MDL_V__EXP__2 = MDL_V**2

        MDL_XI__EXP__2 = MDL_XI**2

        MDL_TH = (-MDL_MH__EXP__2+MDL_MHS__EXP__2+(2.000000D+00
     $   *ATAN(DBLE(1.000000D+100*(MDL_MH-MDL_MHS)))
     $   *SQRT(DCMPLX((MDL_MH__EXP__2-MDL_MHS__EXP__2)**2-4.000000D+00
     $   *MDL_KAP__EXP__2*MDL_V__EXP__2*MDL_XI__EXP__2)))/PI)
     $   /(2.000000D+00*MDL_KAP*MDL_V*MDL_XI)

        MDL_LAM = (MDL_MH__EXP__2+MDL_MHS__EXP__2+(2.000000D+00
     $   *ATAN(DBLE(1.000000D+100*(MDL_MH-MDL_MHS)))
     $   *SQRT(DCMPLX((MDL_MH__EXP__2-MDL_MHS__EXP__2)**2-4.000000D+00
     $   *MDL_KAP__EXP__2*MDL_V__EXP__2*MDL_XI__EXP__2)))/PI)
     $   /(4.000000D+00*MDL_V__EXP__2)

        MDL_RHO = (MDL_MH__EXP__2+MDL_MHS__EXP__2-(2.000000D+00
     $   *ATAN(DBLE(1.000000D+100*(MDL_MH-MDL_MHS)))
     $   *SQRT(DCMPLX((MDL_MH__EXP__2-MDL_MHS__EXP__2)**2-4.000000D+00
     $   *MDL_KAP__EXP__2*MDL_V__EXP__2*MDL_XI__EXP__2)))/PI)
     $   /(4.000000D+00*MDL_XI__EXP__2)

        MDL_TA__EXP__2 = MDL_TA**2

        MDL_CA = 1.000000D+00/SQRT(DCMPLX(1.000000D+00+MDL_TA__EXP__2))

        MDL_TH__EXP__2 = MDL_TH**2

        MDL_CH = 1.000000D+00/SQRT(DCMPLX(1.000000D+00+MDL_TH__EXP__2))

        MDL_MUH2 = (MDL_KAP*MDL_V__EXP__2)/2.000000D+00+MDL_RHO
     $   *MDL_XI__EXP__2

        MDL_MUSM2 = MDL_LAM*MDL_V__EXP__2+(MDL_KAP*MDL_XI__EXP__2)
     $   /2.000000D+00

        MDL_SA = MDL_TA/SQRT(DCMPLX(1.000000D+00+MDL_TA__EXP__2))

        MDL_SH = MDL_TH/SQRT(DCMPLX(1.000000D+00+MDL_TH__EXP__2))

        MDL_COMPLEXI = DCMPLX(0.000000D+00,1.000000D+00)

        MDL_CA__EXP__2 = MDL_CA**2

        MDL_SA__EXP__2 = MDL_SA**2

        MDL_CH__EXP__3 = MDL_CH**3

        MDL_SH__EXP__3 = MDL_SH**3

        MDL_CH__EXP__4 = MDL_CH**4

        MDL_CH__EXP__2 = MDL_CH**2

        MDL_SH__EXP__2 = MDL_SH**2

        MDL_SH__EXP__4 = MDL_SH**4

        MDL_GX__EXP__2 = MDL_GX**2

        MDL_CONJG__CKM11 = CONJG(DCMPLX(MDL_CKM11))

        MDL_CONJG__CKM12 = CONJG(DCMPLX(MDL_CKM12))

        MDL_CONJG__CKM21 = CONJG(DCMPLX(MDL_CKM21))

        MDL_CONJG__CKM22 = CONJG(DCMPLX(MDL_CKM22))

        MDL_AEW = 1.000000D+00/AEWM1

        MDL_SQRT__AEW = SQRT(DCMPLX(MDL_AEW))

        MDL_EE = 2.000000D+00*MDL_SQRT__AEW*SQRT(DCMPLX(PI))

        MDL_EE__EXP__2 = MDL_EE**2

        MDL_AH = (4.700000D+01*MDL_EE__EXP__2*(1.000000D+00-(2.000000D
     $   +00*MDL_MH__EXP__4)/(9.870000D+02*MDL_MT__EXP__4)-(1.400000D
     $   +01*MDL_MH__EXP__2)/(7.050000D+02*MDL_MT__EXP__2)+(2.130000D
     $   +02*MDL_MH__EXP__12)/(2.634632D+07*MDL_MW__EXP__12)
     $   +(5.000000D+00*MDL_MH__EXP__10)/(1.197560D+05*MDL_MW__EXP__10)
     $   +(4.100000D+01*MDL_MH__EXP__8)/(1.809500D+05*MDL_MW__EXP__8)
     $   +(8.700000D+01*MDL_MH__EXP__6)/(6.580000D+04*MDL_MW__EXP__6)
     $   +(5.700000D+01*MDL_MH__EXP__4)/(6.580000D+03*MDL_MW__EXP__4)
     $   +(3.300000D+01*MDL_MH__EXP__2)/(4.700000D+02*MDL_MW__EXP__2)))
     $   /(7.200000D+01*PI**2*MDL_V)

        MDL_G1 = MDL_EE/MDL_CW

        MDL_GW = MDL_EE/MDL_SW

        MDL_GW__EXP__2 = MDL_GW**2

      ENDIF
C     
C     Parameters that should be recomputed at an event by even basis.
C     
      AS = G**2/4/PI

      MDL_SQRT__AS = SQRT(DCMPLX(AS))

      MDL_G__EXP__2 = G**2

      MDL_GH = -(MDL_G__EXP__2*(1.000000D+00+(1.300000D+01
     $ *MDL_MH__EXP__6)/(1.680000D+04*MDL_MT__EXP__6)+MDL_MH__EXP__4
     $ /(1.680000D+02*MDL_MT__EXP__4)+(7.000000D+00*MDL_MH__EXP__2)
     $ /(1.200000D+02*MDL_MT__EXP__2)))/(1.200000D+01*PI**2*MDL_V)

      MDL_GPHI = -(MDL_G__EXP__2*(1.000000D+00+MDL_MH__EXP__6
     $ /(5.600000D+02*MDL_MT__EXP__6)+MDL_MH__EXP__4/(9.000000D+01
     $ *MDL_MT__EXP__4)+MDL_MH__EXP__2/(1.200000D+01*MDL_MT__EXP__2)))
     $ /(8.000000D+00*PI**2*MDL_V)

C     
C     Parameters that should be updated for the loops.
C     
C     
C     Definition of the EW coupling used in the write out of aqed
C     
      GAL(1) = 3.5449077018110318D0 / DSQRT(ABS(AEWM1))
      GAL(2) = 1D0

