ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c      written by the UFO converter
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

C     Parameters that should not be recomputed event by event.
C     
      IF(READLHA) THEN

        G = 2 * DSQRT(AS*PI)  ! for the first init

        MDL_COS__CABI = COS(MDL_CABI)

        MDL_CKM1X1 = MDL_COS__CABI

        MDL_SIN__CABI = SIN(MDL_CABI)

        MDL_CKM1X2 = MDL_SIN__CABI

        MDL_CKM1X3 = 0.000000D+00

        MDL_CKM2X1 = -MDL_SIN__CABI

        MDL_CKM2X2 = MDL_COS__CABI

        MDL_CKM2X3 = 0.000000D+00

        MDL_CKM3X1 = 0.000000D+00

        MDL_CKM3X2 = 0.000000D+00

        MDL_CKM3X3 = 1.000000D+00

        MDL_MZ__EXP__2 = MDL_MZ**2

        MDL_MZ__EXP__4 = MDL_MZ**4

        MDL_SQRT__2 = SQRT(DCMPLX(2.000000D+00))

        MDL_MH__EXP__2 = MDL_MH**2

        MDL_CONJG__CKM1X1 = CONJG(DCMPLX(MDL_CKM1X1))

        MDL_CONJG__CKM2X1 = CONJG(DCMPLX(MDL_CKM2X1))

        MDL_CONJG__CKM3X1 = CONJG(DCMPLX(MDL_CKM3X1))

        MDL_CONJG__CKM1X2 = CONJG(DCMPLX(MDL_CKM1X2))

        MDL_CONJG__CKM2X2 = CONJG(DCMPLX(MDL_CKM2X2))

        MDL_CONJG__CKM3X2 = CONJG(DCMPLX(MDL_CKM3X2))

        MDL_CONJG__CKM1X3 = CONJG(DCMPLX(MDL_CKM1X3))

        MDL_CONJG__CKM2X3 = CONJG(DCMPLX(MDL_CKM2X3))

        MDL_CONJG__CKM3X3 = CONJG(DCMPLX(MDL_CKM3X3))

        MDL_COMPLEXI = DCMPLX(0.000000D+00,1.000000D+00)

        MDL_AEW = 1.000000D+00/AEWM1

        MDL_MW = SQRT(DCMPLX(MDL_MZ__EXP__2/2.000000D+00
     $   +SQRT(DCMPLX(MDL_MZ__EXP__4/4.000000D+00-(MDL_AEW*PI
     $   *MDL_MZ__EXP__2)/(MDL_GF*MDL_SQRT__2)))))

        MDL_SQRT__AEW = SQRT(DCMPLX(MDL_AEW))

        MDL_EE = 2.000000D+00*MDL_SQRT__AEW*SQRT(DCMPLX(PI))

        MDL_MW__EXP__2 = MDL_MW**2

        MDL_SW2 = 1.000000D+00-MDL_MW__EXP__2/MDL_MZ__EXP__2

        MDL_CW = SQRT(DCMPLX(1.000000D+00-MDL_SW2))

        MDL_SQRT__SW2 = SQRT(DCMPLX(MDL_SW2))

        MDL_SW = MDL_SQRT__SW2

        MDL_G1 = MDL_EE/MDL_CW

        MDL_GW = MDL_EE/MDL_SW

        MDL_VEV = (2.000000D+00*MDL_MW*MDL_SW)/MDL_EE

        MDL_VEV__EXP__2 = MDL_VEV**2

        MDL_LAM = MDL_MH__EXP__2/(2.000000D+00*MDL_VEV__EXP__2)

        MDL_YB = (MDL_YMB*MDL_SQRT__2)/MDL_VEV

        MDL_YC = (MDL_YMC*MDL_SQRT__2)/MDL_VEV

        MDL_YDO = (MDL_YMDO*MDL_SQRT__2)/MDL_VEV

        MDL_YE = (MDL_YME*MDL_SQRT__2)/MDL_VEV

        MDL_YM = (MDL_YMM*MDL_SQRT__2)/MDL_VEV

        MDL_YS = (MDL_YMS*MDL_SQRT__2)/MDL_VEV

        MDL_YT = (MDL_YMT*MDL_SQRT__2)/MDL_VEV

        MDL_YTAU = (MDL_YMTAU*MDL_SQRT__2)/MDL_VEV

        MDL_YUP = (MDL_YMUP*MDL_SQRT__2)/MDL_VEV

        MDL_MUH = SQRT(DCMPLX(MDL_LAM*MDL_VEV__EXP__2))

        MDL_I1A11 = MDL_YDO*MDL_CONJG__CKM1X1

        MDL_I1A12 = MDL_YDO*MDL_CONJG__CKM2X1

        MDL_I1A13 = MDL_YDO*MDL_CONJG__CKM3X1

        MDL_I1A21 = MDL_YS*MDL_CONJG__CKM1X2

        MDL_I1A22 = MDL_YS*MDL_CONJG__CKM2X2

        MDL_I1A23 = MDL_YS*MDL_CONJG__CKM3X2

        MDL_I1A31 = MDL_YB*MDL_CONJG__CKM1X3

        MDL_I1A32 = MDL_YB*MDL_CONJG__CKM2X3

        MDL_I1A33 = MDL_YB*MDL_CONJG__CKM3X3

        MDL_I2A11 = MDL_YUP*MDL_CONJG__CKM1X1

        MDL_I2A12 = MDL_YC*MDL_CONJG__CKM2X1

        MDL_I2A13 = MDL_YT*MDL_CONJG__CKM3X1

        MDL_I2A21 = MDL_YUP*MDL_CONJG__CKM1X2

        MDL_I2A22 = MDL_YC*MDL_CONJG__CKM2X2

        MDL_I2A23 = MDL_YT*MDL_CONJG__CKM3X2

        MDL_I2A31 = MDL_YUP*MDL_CONJG__CKM1X3

        MDL_I2A32 = MDL_YC*MDL_CONJG__CKM2X3

        MDL_I2A33 = MDL_YT*MDL_CONJG__CKM3X3

        MDL_I3A11 = MDL_CKM1X1*MDL_YUP

        MDL_I3A12 = MDL_CKM1X2*MDL_YUP

        MDL_I3A13 = MDL_CKM1X3*MDL_YUP

        MDL_I3A21 = MDL_CKM2X1*MDL_YC

        MDL_I3A22 = MDL_CKM2X2*MDL_YC

        MDL_I3A23 = MDL_CKM2X3*MDL_YC

        MDL_I3A31 = MDL_CKM3X1*MDL_YT

        MDL_I3A32 = MDL_CKM3X2*MDL_YT

        MDL_I3A33 = MDL_CKM3X3*MDL_YT

        MDL_I4A11 = MDL_CKM1X1*MDL_YDO

        MDL_I4A12 = MDL_CKM1X2*MDL_YS

        MDL_I4A13 = MDL_CKM1X3*MDL_YB

        MDL_I4A21 = MDL_CKM2X1*MDL_YDO

        MDL_I4A22 = MDL_CKM2X2*MDL_YS

        MDL_I4A23 = MDL_CKM2X3*MDL_YB

        MDL_I4A31 = MDL_CKM3X1*MDL_YDO

        MDL_I4A32 = MDL_CKM3X2*MDL_YS

        MDL_I4A33 = MDL_CKM3X3*MDL_YB

        MDL_EE__EXP__2 = MDL_EE**2

        MDL_SW__EXP__2 = MDL_SW**2

        MDL_CW__EXP__2 = MDL_CW**2

      ENDIF
C     
C     Parameters that should be recomputed at an event by even basis.
C     
      AS = G**2/4/PI

      MDL_SQRT__AS = SQRT(DCMPLX(AS))

      MDL_G__EXP__2 = G**2

C     
C     Parameters that should be updated for the loops.
C     
C     
C     Definition of the EW coupling used in the write out of aqed
C     
      GAL(1) = 3.5449077018110318D0 / DSQRT(ABS(AEWM1))
      GAL(2) = 1D0

