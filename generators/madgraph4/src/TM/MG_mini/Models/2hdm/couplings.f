      subroutine coup

      implicit none
      include 'input.inc'
      include 'coupl.inc'
      double precision  alphas,alfa,alfaw,mfrun
      external          alphas,alfa,alfaw,mfrun
      double precision cw, sw, ee, Sqrt2, sin2w, wm
      double precision  Pi, Fourpi, Zero
      parameter( Pi = 3.14159265358979323846d0 )
      parameter( Sqrt2   = 1.414213562d0 )
      parameter( Zero = 0d0 )
      parameter( Fourpi = 4d0 * Pi )

      wm = sqrt(zmass**2/2d0+
     $     sqrt(zmass**4/4d0-Pi/Sqrt2*alpha/gfermi*zmass**2))
      sin2w  = 1d0-(wm/zmass)**2
      cw  = sqrt( 1d0 - sin2w )
      sw  = sqrt( sin2w )
      ee  = sqrt( alpha * Fourpi )
c      G = DSQRT(4d0*PI*alfas)
      v   = 2d0*wm*sw/ee   ! the wmass is used to calculate v

C#######LIST OF STANDARD MODEL COUPLINGS###################
      gg(1) = -G*dcmplx(+1,Zero )
      gg(2) = -G*dcmplx(+1,Zero )
      gad(1) = +ee/3*dcmplx(+1,Zero )
      gad(2) = +ee/3*dcmplx(+1,Zero )
      gau(1) = -2*ee/3*dcmplx(+1,Zero )
      gau(2) = -2*ee/3*dcmplx(+1,Zero )
      gal(1) = +ee*dcmplx(+1,Zero )
      gal(2) = +ee*dcmplx(+1,Zero )
      gzd(1) = -ee/(12*cw*sw)*2*dcmplx(-(3-2*sw**2)*1,Zero )
      gzd(2) = -ee/(12*cw*sw)*2*dcmplx(+2*sw**2*1,Zero )
      gzu(1) = -ee/(12*cw*sw)*2*dcmplx((3-4*sw**2)*1,Zero )
      gzu(2) = -ee/(12*cw*sw)*2*dcmplx(-4*sw**2*1,Zero )
      gzl(1) = +ee/(4*cw*sw)*2*dcmplx((1-2*sw**2)*1,Zero )
      gzl(2) = +ee/(4*cw*sw)*2*dcmplx(-2*sw**2*1,Zero )
      gzn(1) = -ee/(4*cw*sw)*2*dcmplx(+1,Zero )
      gzn(2) = -ee/(4*cw*sw)*2*dcmplx(Zero,Zero )

      gwfud(1) = -ee*Sqrt2*Vud/(4*sw)*2*dcmplx(+1,Zero )
      gwfud(2) = -ee*Sqrt2*Vud/(4*sw)*2*dcmplx(Zero,Zero )
      gwfcs(1) = -ee*Sqrt2*Vcs/(4*sw)*2*dcmplx(+1,Zero )
      gwfcs(2) = -ee*Sqrt2*Vcs/(4*sw)*2*dcmplx(Zero,Zero )
      gwftb(1) = -ee*Sqrt2/(4*sw)*2*dcmplx(+1,Zero )
      gwftb(2) = -ee*Sqrt2/(4*sw)*2*dcmplx(Zero,Zero )
      gwfdu(1) = -ee*Sqrt2*Vud/(4*sw)*2*dcmplx(+1,Zero )
      gwfdu(2) = -ee*Sqrt2*Vud/(4*sw)*2*dcmplx(Zero,Zero )
      gwfsc(1) = -ee*Sqrt2*Vcs/(4*sw)*2*dcmplx(+1,Zero )
      gwfsc(2) = -ee*Sqrt2*Vcs/(4*sw)*2*dcmplx(Zero,Zero )
      gwfbt(1) = -ee*Sqrt2/(4*sw)*2*dcmplx(+1,Zero )
      gwfbt(2) = -ee*Sqrt2/(4*sw)*2*dcmplx(Zero,Zero )

      gwfcd(1) = -ee*Sqrt2*Vcd/(4*sw)*2*dcmplx(+1,Zero )
      gwfcd(2) = -ee*Sqrt2*Vcd/(4*sw)*2*dcmplx(Zero,Zero )
      gwfus(1) = +ee*Sqrt2*Vus/(4*sw)*2*dcmplx(+1,Zero )
      gwfus(2) = +ee*Sqrt2*Vus/(4*sw)*2*dcmplx(Zero,Zero )
      gwfsu(1) = +ee*Sqrt2*Vus/(4*sw)*2*dcmplx(+1,Zero )
      gwfsu(2) = +ee*Sqrt2*Vus/(4*sw)*2*dcmplx(Zero,Zero )
      gwfdc(1) = -ee*Sqrt2*Vcd/(4*sw)*2*dcmplx(+1,Zero )
      gwfdc(2) = -ee*Sqrt2*Vcd/(4*sw)*2*dcmplx(Zero,Zero )

      gwf(1) = -ee*Sqrt2/(4*sw)*2*dcmplx(+1,Zero )
      gwf(2) = -ee*Sqrt2/(4*sw)*2*dcmplx(Zero,Zero )
      gwwa=ee
      gwwz=cw*ee/sw


C########################LIST OF 2HDM COUPLINGS############

      
        gh1mumu(1) = -ee/(2*sw*wm)*dcmplx(+MSMUMASS*TH1+TN1*Y2MU
     +R-TA1*Y2MUI,-TA1*Y2MUR-TN1*Y2MUI )
      gh1mumu(2) = -ee/(2*sw*wm)*dcmplx(+MSMUMASS*TH1+TN1*Y2MU
     +R-TA1*Y2MUI,+TA1*Y2MUR+TN1*Y2MUI )
      gh2mumu(1) = -ee/(2*sw*wm)*dcmplx(+MSMUMASS*TH2+TN2*Y2MU
     +R-TA2*Y2MUI,-TA2*Y2MUR-TN2*Y2MUI )
      gh2mumu(2) = -ee/(2*sw*wm)*dcmplx(+MSMUMASS*TH2+TN2*Y2MU
     +R-TA2*Y2MUI,+TA2*Y2MUR+TN2*Y2MUI )
      gh3mumu(1) = -ee/(2*sw*wm)*dcmplx(+MSMUMASS*TH3+TN3*Y2MU
     +R-TA3*Y2MUI,-TA3*Y2MUR-TN3*Y2MUI )
      gh3mumu(2) = -ee/(2*sw*wm)*dcmplx(+MSMUMASS*TH3+TN3*Y2MU
     +R-TA3*Y2MUI,+TA3*Y2MUR+TN3*Y2MUI )
      
      gh1tata(1) = -ee/(2*sw*wm)*dcmplx(+MSTAMASS*TH1+TN1*Y3TA
     +R-TA1*Y3TAI,-TA1*Y3TAR-TN1*Y3TAI )
      gh1tata(2) = -ee/(2*sw*wm)*dcmplx(+MSTAMASS*TH1+TN1*Y3TA
     +R-TA1*Y3TAI,+TA1*Y3TAR+TN1*Y3TAI )
      gh2tata(1) = -ee/(2*sw*wm)*dcmplx(+MSTAMASS*TH2+TN2*Y3TA
     +R-TA2*Y3TAI,-TA2*Y3TAR-TN2*Y3TAI )
      gh2tata(2) = -ee/(2*sw*wm)*dcmplx(+MSTAMASS*TH2+TN2*Y3TA
     +R-TA2*Y3TAI,+TA2*Y3TAR+TN2*Y3TAI )
      gh3tata(1) = -ee/(2*sw*wm)*dcmplx(+MSTAMASS*TH3+TN3*Y3TA
     +R-TA3*Y3TAI,-TA3*Y3TAR-TN3*Y3TAI )
      gh3tata(2) = -ee/(2*sw*wm)*dcmplx(+MSTAMASS*TH3+TN3*Y3TA
     +R-TA3*Y3TAI,+TA3*Y3TAR+TN3*Y3TAI )
      
      gh1cc(1) = -ee/(2*sw*wm)*dcmplx(+MSCMASS*TH1+TN1*Y2CR+TA
     +1*Y2CI,+TA1*Y2CR-TN1*Y2CI )
      gh1cc(2) = -ee/(2*sw*wm)*dcmplx(+MSCMASS*TH1+TN1*Y2CR+TA
     +1*Y2CI,-TA1*Y2CR+TN1*Y2CI )
      gh2cc(1) = -ee/(2*sw*wm)*dcmplx(+MSCMASS*TH2+TN2*Y2CR+TA
     +2*Y2CI,+TA2*Y2CR-TN2*Y2CI )
      gh2cc(2) = -ee/(2*sw*wm)*dcmplx(+MSCMASS*TH2+TN2*Y2CR+TA
     +2*Y2CI,-TA2*Y2CR+TN2*Y2CI )
      gh3cc(1) = -ee/(2*sw*wm)*dcmplx(+MSCMASS*TH3+TN3*Y2CR+TA
     +3*Y2CI,+TA3*Y2CR-TN3*Y2CI )
      gh3cc(2) = -ee/(2*sw*wm)*dcmplx(+MSCMASS*TH3+TN3*Y2CR+TA
     +3*Y2CI,-TA3*Y2CR+TN3*Y2CI )
      
      gh1tt(1) = -ee/(2*sw*wm)*dcmplx(+MSTMASS*TH1+TN1*Y3TR+TA
     +1*Y3TI,+TA1*Y3TR-TN1*Y3TI )
      gh1tt(2) = -ee/(2*sw*wm)*dcmplx(+MSTMASS*TH1+TN1*Y3TR+TA
     +1*Y3TI,-TA1*Y3TR+TN1*Y3TI )
      gh2tt(1) = -ee/(2*sw*wm)*dcmplx(+MSTMASS*TH2+TN2*Y3TR+TA
     +2*Y3TI,+TA2*Y3TR-TN2*Y3TI )
      gh2tt(2) = -ee/(2*sw*wm)*dcmplx(+MSTMASS*TH2+TN2*Y3TR+TA
     +2*Y3TI,-TA2*Y3TR+TN2*Y3TI )
      gh3tt(1) = -ee/(2*sw*wm)*dcmplx(+MSTMASS*TH3+TN3*Y3TR+TA
     +3*Y3TI,+TA3*Y3TR-TN3*Y3TI )
      gh3tt(2) = -ee/(2*sw*wm)*dcmplx(+MSTMASS*TH3+TN3*Y3TR+TA
     +3*Y3TI,-TA3*Y3TR+TN3*Y3TI )

      
      gh1ss(1) = -ee/(2*sw*wm)*dcmplx(+MSSMASS*TH1+TN1*Y2SR-TA
     +1*Y2SI,-TA1*Y2SR-TN1*Y2SI )
      gh1ss(2) = -ee/(2*sw*wm)*dcmplx(+MSSMASS*TH1+TN1*Y2SR-TA
     +1*Y2SI,+TA1*Y2SR+TN1*Y2SI )
      gh2ss(1) = -ee/(2*sw*wm)*dcmplx(+MSSMASS*TH2+TN2*Y2SR-TA
     +2*Y2SI,-TA2*Y2SR-TN2*Y2SI )
      gh2ss(2) = -ee/(2*sw*wm)*dcmplx(+MSSMASS*TH2+TN2*Y2SR-TA
     +2*Y2SI,+TA2*Y2SR+TN2*Y2SI )
      gh3ss(1) = -ee/(2*sw*wm)*dcmplx(+MSSMASS*TH3+TN3*Y2SR-TA
     +3*Y2SI,-TA3*Y2SR-TN3*Y2SI )
      gh3ss(2) = -ee/(2*sw*wm)*dcmplx(+MSSMASS*TH3+TN3*Y2SR-TA
     +3*Y2SI,+TA3*Y2SR+TN3*Y2SI )
      
      gh1bb(1) = -ee/(2*sw*wm)*dcmplx(+MSBMASS*TH1+TN1*Y3BR-TA
     +1*Y3BI,-TA1*Y3BR-TN1*Y3BI )
      gh1bb(2) = -ee/(2*sw*wm)*dcmplx(+MSBMASS*TH1+TN1*Y3BR-TA
     +1*Y3BI,+TA1*Y3BR+TN1*Y3BI )
      gh2bb(1) = -ee/(2*sw*wm)*dcmplx(+MSBMASS*TH2+TN2*Y3BR-TA
     +2*Y3BI,-TA2*Y3BR-TN2*Y3BI )
      gh2bb(2) = -ee/(2*sw*wm)*dcmplx(+MSBMASS*TH2+TN2*Y3BR-TA
     +2*Y3BI,+TA2*Y3BR+TN2*Y3BI )
      gh3bb(1) = -ee/(2*sw*wm)*dcmplx(+MSBMASS*TH3+TN3*Y3BR-TA
     +3*Y3BI,-TA3*Y3BR-TN3*Y3BI )
      gh3bb(2) = -ee/(2*sw*wm)*dcmplx(+MSBMASS*TH3+TN3*Y3BR-TA
     +3*Y3BI,+TA3*Y3BR+TN3*Y3BI )

      
      ghmdc(1) = ee*Sqrt2/(4*sw*wm)*dcmplx(Zero,1)*2*dcmplx(-Vud*Y1CI-Vcd*Y2C
     +I,-1*Vud*Y1CR-1*Vcd*Y2CR)
      ghmdc(2) = ee*Sqrt2/(4*sw*wm)*dcmplx(Zero,1)*2*dcmplx(-Vcd*Y1DI-Vcs*Y2D
     +I,+1*Vcd*Y1DR+1*Vcs*Y2DR)
      ghmsu(1) = ee*Sqrt2/(4*sw*wm)*dcmplx(Zero,1)*2*dcmplx(+Vus*Y1UI-Vcs*Y2U
     +I,+1*Vus*Y1UR-1*Vcs*Y2UR)
      ghmsu(2) = ee*Sqrt2/(4*sw*wm)*dcmplx(Zero,1)*2*dcmplx(-Vud*Y1SI-Vus*Y2S
     +I,+1*Vud*Y1SR+1*Vus*Y2SR)
      ghmsc(1) = ee*Sqrt2/(4*sw*wm)*dcmplx(Zero,1)*2*dcmplx(+Vus*Y1CI-Vcs*Y2C
     +I,+1*Vus*Y1CR-1*Vcs*Y2CR)
      ghmsc(2) = ee*Sqrt2/(4*sw*wm)*dcmplx(Zero,1)*2*dcmplx(-Vcd*Y1SI-Vcs*Y2S
     +I,+1*Vcd*Y1SR+1*Vcs*Y2SR)
      ghmbt(1) = ee*Sqrt2/(4*sw*wm)*dcmplx(Zero,1)*2*dcmplx(-Vtb*Y3TI,-1*Vtb*
     +Y3TR)
      ghmbt(2) = ee*Sqrt2/(4*sw*wm)*dcmplx(Zero,1)*2*dcmplx(-Vtb*Y3BI,+1*Vtb*
     +Y3BR)

      
      ghpus(1) = -ee*Sqrt2/(4*sw*wm)*dcmplx(Zero,-1)*2*dcmplx(+Vud*Y1SI-Vus*Y2
     +SI,+1*Vud*Y1SR-1*Vus*Y2SR)
      ghpus(2) = -ee*Sqrt2/(4*sw*wm)*dcmplx(Zero,-1)*2*dcmplx(-Vus*Y1UI+Vcs*Y2
     +UI,+1*Vus*Y1UR-1*Vcs*Y2UR)
      ghpcd(1) = -ee*Sqrt2/(4*sw*wm)*dcmplx(Zero,-1)*2*dcmplx(+Vcd*Y1DI+Vcs*Y2
     +DI,+1*Vcd*Y1DR+1*Vcs*Y2DR)
      ghpcd(2) = -ee*Sqrt2/(4*sw*wm)*dcmplx(Zero,-1)*2*dcmplx(+Vud*Y1CI+Vcd*Y2
     +CI,-1*Vud*Y1CR-1*Vcd*Y2CR)
      ghpcs(1) = -ee*Sqrt2/(4*sw*wm)*dcmplx(Zero,-1)*2*dcmplx(+Vcd*Y1SI+Vcs*Y2
     +SI,+1*Vcd*Y1SR+1*Vcs*Y2SR)
      ghpcs(2) = -ee*Sqrt2/(4*sw*wm)*dcmplx(Zero,-1)*2*dcmplx(-Vus*Y1CI+Vcs*Y2
     +CI,+1*Vus*Y1CR-1*Vcs*Y2CR)
      ghptb(1) = -ee*Sqrt2/(4*sw*wm)*dcmplx(Zero,-1)*2*dcmplx(+Vtb*Y3BI,+1*Vtb
     +*Y3BR)
      ghptb(2) = -ee*Sqrt2/(4*sw*wm)*dcmplx(Zero,-1)*2*dcmplx(+Vtb*Y3TI,-1*Vtb
     +*Y3TR)


      ghmmuvm(1) = ee*dcmplx(Zero,1)*Sqrt2/(4*sw*wm)*2*dcmplx(Zero,Zero )
      ghmmuvm(2) = ee*dcmplx(Zero,1)*Sqrt2/(4*sw*wm)*2*dcmplx(-Y2MUI,+1*Y2MUR
     + )
      ghmtavt(1) = ee*dcmplx(Zero,1)*Sqrt2/(4*sw*wm)*2*dcmplx(Zero,Zero )
      ghmtavt(2) = ee*dcmplx(Zero,1)*Sqrt2/(4*sw*wm)*2*dcmplx(-Y3TAI,+1*Y3TAR
     + )
      ghpvmmu(1) = -ee*dcmplx(Zero,-1)*Sqrt2/(4*sw*wm)*2*dcmplx(+Y2MUI,+1*Y2MU
     +R )
      ghpvmmu(2) = -ee*dcmplx(Zero,-1)*Sqrt2/(4*sw*wm)*2*dcmplx(Zero,Zero )
      ghpvtta(1) = -ee*dcmplx(Zero,-1)*Sqrt2/(4*sw*wm)*2*dcmplx(+Y3TAI,+1*Y3TA
     +R )
      ghpvtta(2) = -ee*dcmplx(Zero,-1)*Sqrt2/(4*sw*wm)*2*dcmplx(Zero,Zero )

      gwwh1=ee*TH1*wm/sw*dcmplx(+1,Zero)
      gwwh2=ee*TH2*wm/sw*dcmplx(+1,Zero)
      gwwh3=ee*TH3*wm/sw*dcmplx(+1,Zero)

      gzzh1=ee*TH1*wm/(cw**2*sw)*dcmplx(+1,Zero)
      gzzh2=ee*TH2*wm/(cw**2*sw)*dcmplx(+1,Zero)
      gzzh3=ee*TH3*wm/(cw**2*sw)*dcmplx(+1,Zero)

      gzh1h3=dcmplx(0,1)*ee/(2*cw*sw)*dcmplx(-TA1*TN3*1+TA3*TN1*1
     +,Zero)
      gzh2h3=dcmplx(0,1)*ee/(2*cw*sw)*dcmplx(-TA2*TN3*1+TA3*TN2*1
     +,Zero)
      gzh1h2=dcmplx(0,1)*ee/(2*cw*sw)*dcmplx(-TA1*TN2*1+TA2*TN1*1
     +,Zero)
      gahchc=ee*dcmplx(-1,Zero)
      gzhchc=dcmplx(-1,Zero)*ee/(2*cw*sw)*dcmplx(+(1-2*sw**2)*1,Zero)
      gwhch1=-ee/(2*sw)*dcmplx(0,1)*dcmplx(+TA1*1,TN1*1)
      gwh1hc=ee/(2*sw)*dcmplx(0,1)*dcmplx(+TA1*1,-TN1*1)
      gwhch2=-ee/(2*sw)*dcmplx(0,1)*dcmplx(+TA2*1,+TN2*1)
      gwh2hc=ee/(2*sw)*dcmplx(0,1)*dcmplx(+TA2*1,-TN2*1)
      gwhch3=-ee/(2*sw)*dcmplx(0,1)*dcmplx(+TA3*1,+TN3*1)
      gwh3hc=ee/(2*sw)*dcmplx(0,1)*dcmplx(+TA3*1,-TN3*1)

      gh111=-6*sw*wm/ee*dcmplx(+2*TH1**3*lambda1+TH1*TN1**2*la
     +mbda3+TA1**2*TH1*lambda3+TH1*TN1**2*lambda4+TA1**2*TH1*lambda4+2*
     +TH1*TN1**2*lambda5-2*TA1**2*TH1*lambda5+3*Rlambda6*TH1**2*TN1-3*I
     +lambda6*TA1*TH1**2+Rlambda7*TN1**3+Rlambda7*TA1**2*TN1-Ilambda7*T
     +A1*TN1**2-Ilambda7*TA1**3,Zero)
      gh112=-2*sw*wm/ee*dcmplx(+6*TH1**2*TH2*lambda1+2*TH1*TN1
     +*TN2*lambda3+2*TA1*TA2*TH1*lambda3+TH2*TN1**2*lambda3+TA1**2*TH2*
     +lambda3+TH2*TN1**2*lambda4+2*TH1*TN1*TN2*lambda4+TA1**2*TH2*lambd
     +a4+2*TA1*TA2*TH1*lambda4+4*TH1*TN1*TN2*lambda5+2*TH2*TN1**2*lambd
     +a5-4*TA1*TA2*TH1*lambda5-2*TA1**2*TH2*lambda5+3*Rlambda6*TH1**2*T
     +N2+6*Rlambda6*TH1*TH2*TN1-3*Ilambda6*TA2*TH1**2-6*Ilambda6*TA1*TH
     +1*TH2+3*Rlambda7*TN1**2*TN2+2*Rlambda7*TA1*TA2*TN1+Rlambda7*TA1**
     +2*TN2-Ilambda7*TA2*TN1**2-2*Ilambda7*TA1*TN1*TN2-3*Ilambda7*TA1**
     +2*TA2,Zero)
      gh113=-2*sw*wm/ee*dcmplx(+6*TH1**2*TH3*lambda1+2*TH1*TN1
     +*TN3*lambda3+2*TA1*TA3*TH1*lambda3+TH3*TN1**2*lambda3+TA1**2*TH3*
     +lambda3+TH3*TN1**2*lambda4+2*TH1*TN1*TN3*lambda4+TA1**2*TH3*lambd
     +a4+2*TA1*TA3*TH1*lambda4+4*TH1*TN1*TN3*lambda5+2*TH3*TN1**2*lambd
     +a5-4*TA1*TA3*TH1*lambda5-2*TA1**2*TH3*lambda5+3*Rlambda6*TH1**2*T
     +N3+6*Rlambda6*TH1*TH3*TN1-3*Ilambda6*TA3*TH1**2-6*Ilambda6*TA1*TH
     +1*TH3+3*Rlambda7*TN1**2*TN3+2*Rlambda7*TA1*TA3*TN1+Rlambda7*TA1**
     +2*TN3-Ilambda7*TA3*TN1**2-2*Ilambda7*TA1*TN1*TN3-3*Ilambda7*TA1**
     +2*TA3,Zero)
      gh122=-2*sw*wm/ee*dcmplx(+6*TH1*TH2**2*lambda1+TH1*TN2**
     +2*lambda3+TA2**2*TH1*lambda3+2*TH2*TN1*TN2*lambda3+2*TA1*TA2*TH2*
     +lambda3+2*TH2*TN1*TN2*lambda4+TH1*TN2**2*lambda4+2*TA1*TA2*TH2*la
     +mbda4+TA2**2*TH1*lambda4+4*TH2*TN1*TN2*lambda5+2*TH1*TN2**2*lambd
     +a5-4*TA1*TA2*TH2*lambda5-2*TA2**2*TH1*lambda5+6*Rlambda6*TH1*TH2*
     +TN2+3*Rlambda6*TH2**2*TN1-6*Ilambda6*TA2*TH1*TH2-3*Ilambda6*TA1*T
     +H2**2+3*Rlambda7*TN1*TN2**2+2*Rlambda7*TA1*TA2*TN2+Rlambda7*TA2**
     +2*TN1-2*Ilambda7*TA2*TN1*TN2-Ilambda7*TA1*TN2**2-3*Ilambda7*TA1*T
     +A2**2,Zero)
      gh123=-2*sw*wm/ee*dcmplx(+6*TH1*TH2*TH3*lambda1+TH1*TN2*
     +TN3*lambda3+TA2*TA3*TH1*lambda3+TH2*TN1*TN3*lambda3+TA1*TA3*TH2*l
     +ambda3+TH3*TN1*TN2*lambda3+TA1*TA2*TH3*lambda3+TH3*TN1*TN2*lambda
     +4+TH2*TN1*TN3*lambda4+TH1*TN2*TN3*lambda4+TA1*TA2*TH3*lambda4+TA1
     +*TA3*TH2*lambda4+TA2*TA3*TH1*lambda4+2*TH2*TN1*TN3*lambda5+2*TH3*
     +TN1*TN2*lambda5+2*TH1*TN2*TN3*lambda5-2*TA1*TA3*TH2*lambda5-2*TA1
     +*TA2*TH3*lambda5-2*TA2*TA3*TH1*lambda5+3*Rlambda6*TH1*TH2*TN3+3*R
     +lambda6*TH1*TH3*TN2+3*Rlambda6*TH2*TH3*TN1-3*Ilambda6*TA3*TH1*TH2
     +-3*Ilambda6*TA2*TH1*TH3-3*Ilambda6*TA1*TH2*TH3+3*Rlambda7*TN1*TN2
     +*TN3+Rlambda7*TA1*TA3*TN2+Rlambda7*TA1*TA2*TN3+Rlambda7*TA2*TA3*T
     +N1-Ilambda7*TA3*TN1*TN2-Ilambda7*TA2*TN1*TN3-Ilambda7*TA1*TN2*TN3
     +-3*Ilambda7*TA1*TA2*TA3,Zero)
      gh133=-2*sw*wm/ee*dcmplx(+6*TH1*TH3**2*lambda1+TH1*TN3**
     +2*lambda3+TA3**2*TH1*lambda3+2*TH3*TN1*TN3*lambda3+2*TA1*TA3*TH3*
     +lambda3+2*TH3*TN1*TN3*lambda4+TH1*TN3**2*lambda4+2*TA1*TA3*TH3*la
     +mbda4+TA3**2*TH1*lambda4+4*TH3*TN1*TN3*lambda5+2*TH1*TN3**2*lambd
     +a5-4*TA1*TA3*TH3*lambda5-2*TA3**2*TH1*lambda5+6*Rlambda6*TH1*TH3*
     +TN3+3*Rlambda6*TH3**2*TN1-6*Ilambda6*TA3*TH1*TH3-3*Ilambda6*TA1*T
     +H3**2+3*Rlambda7*TN1*TN3**2+2*Rlambda7*TA1*TA3*TN3+Rlambda7*TA3**
     +2*TN1-2*Ilambda7*TA3*TN1*TN3-Ilambda7*TA1*TN3**2-3*Ilambda7*TA1*T
     +A3**2,Zero)
      gh222=-6*sw*wm/ee*dcmplx(+2*TH2**3*lambda1+TH2*TN2**2*la
     +mbda3+TA2**2*TH2*lambda3+TH2*TN2**2*lambda4+TA2**2*TH2*lambda4+2*
     +TH2*TN2**2*lambda5-2*TA2**2*TH2*lambda5+3*Rlambda6*TH2**2*TN2-3*I
     +lambda6*TA2*TH2**2+Rlambda7*TN2**3+Rlambda7*TA2**2*TN2-Ilambda7*T
     +A2*TN2**2-Ilambda7*TA2**3,Zero)
      gh223=-2*sw*wm/ee*dcmplx(+6*TH2**2*TH3*lambda1+2*TH2*TN2
     +*TN3*lambda3+2*TA2*TA3*TH2*lambda3+TH3*TN2**2*lambda3+TA2**2*TH3*
     +lambda3+TH3*TN2**2*lambda4+2*TH2*TN2*TN3*lambda4+TA2**2*TH3*lambd
     +a4+2*TA2*TA3*TH2*lambda4+4*TH2*TN2*TN3*lambda5+2*TH3*TN2**2*lambd
     +a5-4*TA2*TA3*TH2*lambda5-2*TA2**2*TH3*lambda5+3*Rlambda6*TH2**2*T
     +N3+6*Rlambda6*TH2*TH3*TN2-3*Ilambda6*TA3*TH2**2-6*Ilambda6*TA2*TH
     +2*TH3+3*Rlambda7*TN2**2*TN3+2*Rlambda7*TA2*TA3*TN2+Rlambda7*TA2**
     +2*TN3-Ilambda7*TA3*TN2**2-2*Ilambda7*TA2*TN2*TN3-3*Ilambda7*TA2**
     +2*TA3,Zero)
      gh233=-2*sw*wm/ee*dcmplx(+6*TH2*TH3**2*lambda1+TH2*TN3**
     +2*lambda3+TA3**2*TH2*lambda3+2*TH3*TN2*TN3*lambda3+2*TA2*TA3*TH3*
     +lambda3+2*TH3*TN2*TN3*lambda4+TH2*TN3**2*lambda4+2*TA2*TA3*TH3*la
     +mbda4+TA3**2*TH2*lambda4+4*TH3*TN2*TN3*lambda5+2*TH2*TN3**2*lambd
     +a5-4*TA2*TA3*TH3*lambda5-2*TA3**2*TH2*lambda5+6*Rlambda6*TH2*TH3*
     +TN3+3*Rlambda6*TH3**2*TN2-6*Ilambda6*TA3*TH2*TH3-3*Ilambda6*TA2*T
     +H3**2+3*Rlambda7*TN2*TN3**2+2*Rlambda7*TA2*TA3*TN3+Rlambda7*TA3**
     +2*TN2-2*Ilambda7*TA3*TN2*TN3-Ilambda7*TA2*TN3**2-3*Ilambda7*TA2*T
     +A3**2,Zero)
      gh333=-6*sw*wm/ee*dcmplx(+2*TH3**3*lambda1+TH3*TN3**2*la
     +mbda3+TA3**2*TH3*lambda3+TH3*TN3**2*lambda4+TA3**2*TH3*lambda4+2*
     +TH3*TN3**2*lambda5-2*TA3**2*TH3*lambda5+3*Rlambda6*TH3**2*TN3-3*I
     +lambda6*TA3*TH3**2+Rlambda7*TN3**3+Rlambda7*TA3**2*TN3-Ilambda7*T
     +A3*TN3**2-Ilambda7*TA3**3,Zero)
      gh1hmhp=-2*sw*wm/ee*dcmplx(+TH1*lambda3+Rlambda7*TN1-Ila
     +mbda7*TA1,Zero)
      gh2hmhp=-2*sw*wm/ee*dcmplx(+TH2*lambda3+Rlambda7*TN2-Ila
     +mbda7*TA2,Zero)
      gh3hmhp=-2*sw*wm/ee*dcmplx(+TH3*lambda3+Rlambda7*TN3-Ila
     +mbda7*TA3,Zero)

      gzzh1h1=ee**2/(2*cw**2*sw**2)*dcmplx(+TH1**2*1+TN1**2*1+TA1
     +**2*1,Zero)
      gzzh2h2=ee**2/(2*cw**2*sw**2)*dcmplx(+TH2**2*1+TN2**2*1+TA2
     +**2*1,Zero)
      gzzh3h3=ee**2/(2*cw**2*sw**2)*dcmplx(+TH3**2*1+TN3**2*1+TA3
     +**2*1,Zero)
      gzzh1h2=ee**2/(2*cw**2*sw**2)*dcmplx(+TH1*TH2*1+TN1*TN2*1+T
     +A1*TA2*1,Zero)
      gzzh1h3=ee**2/(2*cw**2*sw**2)*dcmplx(+TH1*TH3*1+TN1*TN3*1+T
     +A1*TA3*1,Zero)
      gzzh2h3=ee**2/(2*cw**2*sw**2)*dcmplx(+TH2*TH3*1+TN2*TN3*1+T
     +A2*TA3*1,Zero)
      gwwh1h1=ee**2/(2*sw**2)*dcmplx(+TH1**2*1+TN1**2*1+TA1**2*1,
     +Zero)
      gwwh2h2=ee**2/(2*sw**2)*dcmplx(+TH2**2*1+TN2**2*1+TA2**2*1,
     +Zero)
      gwwh3h3=ee**2/(2*sw**2)*dcmplx(+TH3**2*1+TN3**2*1+TA3**2*1,
     +Zero)
      gwwh1h2=ee**2/(2*sw**2)*dcmplx(+TH1*TH2*1+TN1*TN2*1+TA1*TA2
     +*1,Zero)
      gwwh1h3=ee**2/(2*sw**2)*dcmplx(+TH1*TH3*1+TN1*TN3*1+TA1*TA3
     +*1,Zero)
      gwwh2h3=ee**2/(2*sw**2)*dcmplx(+TH2*TH3*1+TN2*TN3*1+TA2*TA3
     +*1,Zero)
      gaahchc=2*ee**2*dcmplx(+1,Zero)
      gazhchc=ee**2/(cw*sw)*dcmplx(+(1-2*sw**2)*1,Zero)
      gzzhchc=ee**2/(2*cw**2*sw**2)*dcmplx(+(1-2*sw**2)**2*1,Zero
     +)
      gwwhchc=ee**2/(2*sw**2)*dcmplx(+1,Zero)
      gwah1hc=-ee**2/(2*sw)*dcmplx(+TA1*1,-TN1*1)
      gwah2hc=-ee**2/(2*sw)*dcmplx(+TA2*1,-TN2*1)
      gwah3hc=-ee**2/(2*sw)*dcmplx(+TA3*1,-TN3*1)
      gwahch1=ee**2/(2*sw)*dcmplx(-TA1*1,-TN1*1)
      gwahch2=ee**2/(2*sw)*dcmplx(-TA2*1,-TN2*1)
      gwahch3=ee**2/(2*sw)*dcmplx(-TA3*1,-TN3*1)
      gwzh1hc=ee**2/(2*cw)*dcmplx(+TA1*1,-TN1*1)
      gwzh2hc=ee**2/(2*cw)*dcmplx(+TA2*1,-TN2*1)
      gwzh3hc=ee**2/(2*cw)*dcmplx(+TA3*1,-TN3*1)
      gwzhch1=-ee**2/(2*cw)*dcmplx(-TA1*1,-TN1*1)
      gwzhch2=-ee**2/(2*cw)*dcmplx(-TA2*1,-TN2*1)
      gwzhch3=-ee**2/(2*cw)*dcmplx(-TA3*1,-TN3*1)
      
C############################END LIST OF COUPLINGS##############################

C############################HIGGS EFFECTIVE COUPLINGS##########################

      Gggh1(1)=dcmplx((TH1+TN1)*g**2/4d0/PI/(3d0*PI*V),zero)
      Gggh1(2)=dcmplx(TA1      *g**2/4d0/PI/(2d0*PI*V),zero)

      Gggh2(1)=dcmplx((TH2+TN2)*g**2/4d0/PI/(3d0*PI*V),zero)
      Gggh2(2)=dcmplx(TA2      *g**2/4d0/PI/(2d0*PI*V),zero)

      Gggh3(1)=dcmplx((TH3+TN3)*g**2/4d0/PI/(3d0*PI*V),zero)
      Gggh3(2)=dcmplx(TA3      *g**2/4d0/PI/(2d0*PI*V),zero)

C############################END LIST OF COUPLINGS##############################

c----------------------------
c end subroutine coupsm
c----------------------------


      return
      end
