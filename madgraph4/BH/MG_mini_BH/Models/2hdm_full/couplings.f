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

      gh1ee(1) = -ee/(2*sw*wm)*dcmplx(+TN1*Y1ER-TA1*Y1EI,-TA1*
     +Y1ER-TN1*Y1EI )
      gh1ee(2) = -ee/(2*sw*wm)*dcmplx(+TN1*Y1ER-TA1*Y1EI,+TA1*
     +Y1ER+TN1*Y1EI )
      gh2ee(1) = -ee/(2*sw*wm)*dcmplx(+TN2*Y1ER-TA2*Y1EI,-TA2*
     +Y1ER-TN2*Y1EI )
      gh2ee(2) = -ee/(2*sw*wm)*dcmplx(+TN2*Y1ER-TA2*Y1EI,+TA2*
     +Y1ER+TN2*Y1EI )
      gh3ee(1) = -ee/(2*sw*wm)*dcmplx(+TN3*Y1ER-TA3*Y1EI,-TA3*
     +Y1ER-TN3*Y1EI )
      gh3ee(2) = -ee/(2*sw*wm)*dcmplx(+TN3*Y1ER-TA3*Y1EI,+TA3*
     +Y1ER+TN3*Y1EI )

      gh1emu(1) = -ee/(4*sw*wm)*2*dcmplx(+TN1*Y1MUR-TA1*Y1MUI,
     +-1*TA1*Y1MUR-1*TN1*Y1MUI )
      gh1emu(2) = -ee/(4*sw*wm)*2*dcmplx(+TN1*Y2ER-TA1*Y2EI,+1
     +*TA1*Y2ER+1*TN1*Y2EI )
      gh2emu(1) = -ee/(4*sw*wm)*2*dcmplx(+TN2*Y1MUR-TA2*Y1MUI,
     +-1*TA2*Y1MUR-1*TN2*Y1MUI )
      gh2emu(2) = -ee/(4*sw*wm)*2*dcmplx(+TN2*Y2ER-TA2*Y2EI,+1
     +*TA2*Y2ER+1*TN2*Y2EI )
      gh3emu(1) = -ee/(4*sw*wm)*2*dcmplx(+TN3*Y1MUR-TA3*Y1MUI,
     +-1*TA3*Y1MUR-1*TN3*Y1MUI )
      gh3emu(2) = -ee/(4*sw*wm)*2*dcmplx(+TN3*Y2ER-TA3*Y2EI,+1
     +*TA3*Y2ER+1*TN3*Y2EI )

      gh1eta(1) = -ee/(4*sw*wm)*2*dcmplx(+TN1*Y1TAR-TA1*Y1TAI,
     +-1*TA1*Y1TAR-1*TN1*Y1TAI )
      gh1eta(2) = -ee/(4*sw*wm)*2*dcmplx(+TN1*Y3ER-TA1*Y3EI,+1
     +*TA1*Y3ER+1*TN1*Y3EI )
      gh2eta(1) = -ee/(4*sw*wm)*2*dcmplx(+TN2*Y1TAR-TA2*Y1TAI,
     +-1*TA2*Y1TAR-1*TN2*Y1TAI )
      gh2eta(2) = -ee/(4*sw*wm)*2*dcmplx(+TN2*Y3ER-TA2*Y3EI,+1
     +*TA2*Y3ER+1*TN2*Y3EI )
      gh3eta(1) = -ee/(4*sw*wm)*2*dcmplx(+TN3*Y1TAR-TA3*Y1TAI,
     +-1*TA3*Y1TAR-1*TN3*Y1TAI )
      gh3eta(2) = -ee/(4*sw*wm)*2*dcmplx(+TN3*Y3ER-TA3*Y3EI,+1
     +*TA3*Y3ER+1*TN3*Y3EI )

      gh1mue(1) = -ee/(4*sw*wm)*2*dcmplx(+TN1*Y2ER-TA1*Y2EI,-1
     +*TA1*Y2ER-1*TN1*Y2EI )
      gh1mue(2) = -ee/(4*sw*wm)*2*dcmplx(+TN1*Y1MUR-TA1*Y1MUI,
     ++1*TA1*Y1MUR+1*TN1*Y1MUI )
      gh2mue(1) = -ee/(4*sw*wm)*2*dcmplx(+TN2*Y2ER-TA2*Y2EI,-1
     +*TA2*Y2ER-1*TN2*Y2EI )
      gh2mue(2) = -ee/(4*sw*wm)*2*dcmplx(+TN2*Y1MUR-TA2*Y1MUI,
     ++1*TA2*Y1MUR+1*TN2*Y1MUI )
      gh3mue(1) = -ee/(4*sw*wm)*2*dcmplx(+TN3*Y2ER-TA3*Y2EI,-1
     +*TA3*Y2ER-1*TN3*Y2EI )
      gh3mue(2) = -ee/(4*sw*wm)*2*dcmplx(+TN3*Y1MUR-TA3*Y1MUI,
     ++1*TA3*Y1MUR+1*TN3*Y1MUI )

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

      gh1muta(1) = -ee/(4*sw*wm)*2*dcmplx(+TN1*Y2TAR-TA1*Y2TAI
     +,-1*TA1*Y2TAR-1*TN1*Y2TAI )
      gh1muta(2) = -ee/(4*sw*wm)*2*dcmplx(+TN1*Y3MUR-TA1*Y3MUI
     +,+1*TA1*Y3MUR+1*TN1*Y3MUI )
      gh2muta(1) = -ee/(4*sw*wm)*2*dcmplx(+TN2*Y2TAR-TA2*Y2TAI
     +,-1*TA2*Y2TAR-1*TN2*Y2TAI )
      gh2muta(2) = -ee/(4*sw*wm)*2*dcmplx(+TN2*Y3MUR-TA2*Y3MUI
     +,+1*TA2*Y3MUR+1*TN2*Y3MUI )
      gh3muta(1) = -ee/(4*sw*wm)*2*dcmplx(+TN3*Y2TAR-TA3*Y2TAI
     +,-1*TA3*Y2TAR-1*TN3*Y2TAI )
      gh3muta(2) = -ee/(4*sw*wm)*2*dcmplx(+TN3*Y3MUR-TA3*Y3MUI
     +,+1*TA3*Y3MUR+1*TN3*Y3MUI )

      gh1tae(1) = -ee/(4*sw*wm)*2*dcmplx(+TN1*Y3ER-TA1*Y3EI,-1
     +*TA1*Y3ER-1*TN1*Y3EI )
      gh1tae(2) = -ee/(4*sw*wm)*2*dcmplx(+TN1*Y1TAR-TA1*Y1TAI,
     ++1*TA1*Y1TAR+1*TN1*Y1TAI )
      gh2tae(1) = -ee/(4*sw*wm)*2*dcmplx(+TN2*Y3ER-TA2*Y3EI,-1
     +*TA2*Y3ER-1*TN2*Y3EI )
      gh2tae(2) = -ee/(4*sw*wm)*2*dcmplx(+TN2*Y1TAR-TA2*Y1TAI,
     ++1*TA2*Y1TAR+1*TN2*Y1TAI )
      gh3tae(1) = -ee/(4*sw*wm)*2*dcmplx(+TN3*Y3ER-TA3*Y3EI,-1
     +*TA3*Y3ER-1*TN3*Y3EI )
      gh3tae(2) = -ee/(4*sw*wm)*2*dcmplx(+TN3*Y1TAR-TA3*Y1TAI,
     ++1*TA3*Y1TAR+1*TN3*Y1TAI )

      gh1tamu(1) = -ee/(4*sw*wm)*2*dcmplx(+TN1*Y3MUR-TA1*Y3MUI
     +,-1*TA1*Y3MUR-1*TN1*Y3MUI )
      gh1tamu(2) = -ee/(4*sw*wm)*2*dcmplx(+TN1*Y2TAR-TA1*Y2TAI
     +,+1*TA1*Y2TAR+1*TN1*Y2TAI )
      gh2tamu(1) = -ee/(4*sw*wm)*2*dcmplx(+TN2*Y3MUR-TA2*Y3MUI
     +,-1*TA2*Y3MUR-1*TN2*Y3MUI )
      gh2tamu(2) = -ee/(4*sw*wm)*2*dcmplx(+TN2*Y2TAR-TA2*Y2TAI
     +,+1*TA2*Y2TAR+1*TN2*Y2TAI )
      gh3tamu(1) = -ee/(4*sw*wm)*2*dcmplx(+TN3*Y3MUR-TA3*Y3MUI
     +,-1*TA3*Y3MUR-1*TN3*Y3MUI )
      gh3tamu(2) = -ee/(4*sw*wm)*2*dcmplx(+TN3*Y2TAR-TA3*Y2TAI
     +,+1*TA3*Y2TAR+1*TN3*Y2TAI )

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


      gh1uu(1) = -ee/(2*sw*wm)*dcmplx(+TN1*Y1UR+TA1*Y1UI,+TA1*
     +Y1UR-TN1*Y1UI )
      gh1uu(2) = -ee/(2*sw*wm)*dcmplx(+TN1*Y1UR+TA1*Y1UI,-TA1*
     +Y1UR+TN1*Y1UI )
      gh2uu(1) = -ee/(2*sw*wm)*dcmplx(+TN2*Y1UR+TA2*Y1UI,+TA2*
     +Y1UR-TN2*Y1UI )
      gh2uu(2) = -ee/(2*sw*wm)*dcmplx(+TN2*Y1UR+TA2*Y1UI,-TA2*
     +Y1UR+TN2*Y1UI )
      gh3uu(1) = -ee/(2*sw*wm)*dcmplx(+TN3*Y1UR+TA3*Y1UI,+TA3*
     +Y1UR-TN3*Y1UI )
      gh3uu(2) = -ee/(2*sw*wm)*dcmplx(+TN3*Y1UR+TA3*Y1UI,-TA3*
     +Y1UR+TN3*Y1UI )
      gh1uc(1) = -ee/(4*sw*wm)*2*dcmplx(+TN1*Y1CR+TA1*Y1CI,+1*
     +TA1*Y1CR-1*TN1*Y1CI )
      gh1uc(2) = -ee/(4*sw*wm)*2*dcmplx(+TN1*Y2UR+TA1*Y2UI,-1*
     +TA1*Y2UR+1*TN1*Y2UI )
      gh2uc(1) = -ee/(4*sw*wm)*2*dcmplx(+TN2*Y1CR+TA2*Y1CI,+1*
     +TA2*Y1CR-1*TN2*Y1CI )
      gh2uc(2) = -ee/(4*sw*wm)*2*dcmplx(+TN2*Y2UR+TA2*Y2UI,-1*
     +TA2*Y2UR+1*TN2*Y2UI )
      gh3uc(1) = -ee/(4*sw*wm)*2*dcmplx(+TN3*Y1CR+TA3*Y1CI,+1*
     +TA3*Y1CR-1*TN3*Y1CI )
      gh3uc(2) = -ee/(4*sw*wm)*2*dcmplx(+TN3*Y2UR+TA3*Y2UI,-1*
     +TA3*Y2UR+1*TN3*Y2UI )
      gh1ut(1) = -ee/(4*sw*wm)*2*dcmplx(+TN1*Y1TR+TA1*Y1TI,+1*
     +TA1*Y1TR-1*TN1*Y1TI )
      gh1ut(2) = -ee/(4*sw*wm)*2*dcmplx(+TN1*Y3UR+TA1*Y3UI,-1*
     +TA1*Y3UR+1*TN1*Y3UI )
      gh2ut(1) = -ee/(4*sw*wm)*2*dcmplx(+TN2*Y1TR+TA2*Y1TI,+1*
     +TA2*Y1TR-1*TN2*Y1TI )
      gh2ut(2) = -ee/(4*sw*wm)*2*dcmplx(+TN2*Y3UR+TA2*Y3UI,-1*
     +TA2*Y3UR+1*TN2*Y3UI )
      gh3ut(1) = -ee/(4*sw*wm)*2*dcmplx(+TN3*Y1TR+TA3*Y1TI,+1*
     +TA3*Y1TR-1*TN3*Y1TI )
      gh3ut(2) = -ee/(4*sw*wm)*2*dcmplx(+TN3*Y3UR+TA3*Y3UI,-1*
     +TA3*Y3UR+1*TN3*Y3UI )

      gh1cu(1) = -ee/(4*sw*wm)*2*dcmplx(+TN1*Y2UR+TA1*Y2UI,+1*
     +TA1*Y2UR-1*TN1*Y2UI )
      gh1cu(2) = -ee/(4*sw*wm)*2*dcmplx(+TN1*Y1CR+TA1*Y1CI,-1*
     +TA1*Y1CR+1*TN1*Y1CI )
      gh2cu(1) = -ee/(4*sw*wm)*2*dcmplx(+TN2*Y2UR+TA2*Y2UI,+1*
     +TA2*Y2UR-1*TN2*Y2UI )
      gh2cu(2) = -ee/(4*sw*wm)*2*dcmplx(+TN2*Y1CR+TA2*Y1CI,-1*
     +TA2*Y1CR+1*TN2*Y1CI )
      gh3cu(1) = -ee/(4*sw*wm)*2*dcmplx(+TN3*Y2UR+TA3*Y2UI,+1*
     +TA3*Y2UR-1*TN3*Y2UI )
      gh3cu(2) = -ee/(4*sw*wm)*2*dcmplx(+TN3*Y1CR+TA3*Y1CI,-1*
     +TA3*Y1CR+1*TN3*Y1CI )
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
      gh1ct(1) = -ee/(4*sw*wm)*2*dcmplx(+TN1*Y2TR+TA1*Y2TI,+1*
     +TA1*Y2TR-1*TN1*Y2TI )
      gh1ct(2) = -ee/(4*sw*wm)*2*dcmplx(+TN1*Y3CR+TA1*Y3CI,-1*
     +TA1*Y3CR+1*TN1*Y3CI )
      gh2ct(1) = -ee/(4*sw*wm)*2*dcmplx(+TN2*Y2TR+TA2*Y2TI,+1*
     +TA2*Y2TR-1*TN2*Y2TI )
      gh2ct(2) = -ee/(4*sw*wm)*2*dcmplx(+TN2*Y3CR+TA2*Y3CI,-1*
     +TA2*Y3CR+1*TN2*Y3CI )
      gh3ct(1) = -ee/(4*sw*wm)*2*dcmplx(+TN3*Y2TR+TA3*Y2TI,+1*
     +TA3*Y2TR-1*TN3*Y2TI )
      gh3ct(2) = -ee/(4*sw*wm)*2*dcmplx(+TN3*Y3CR+TA3*Y3CI,-1*
     +TA3*Y3CR+1*TN3*Y3CI )

      gh1tu(1) = -ee/(4*sw*wm)*2*dcmplx(+TN1*Y3UR+TA1*Y3UI,+1*
     +TA1*Y3UR-1*TN1*Y3UI )
      gh1tu(2) = -ee/(4*sw*wm)*2*dcmplx(+TN1*Y1TR+TA1*Y1TI,-1*
     +TA1*Y1TR+1*TN1*Y1TI )
      gh2tu(1) = -ee/(4*sw*wm)*2*dcmplx(+TN2*Y3UR+TA2*Y3UI,+1*
     +TA2*Y3UR-1*TN2*Y3UI )
      gh2tu(2) = -ee/(4*sw*wm)*2*dcmplx(+TN2*Y1TR+TA2*Y1TI,-1*
     +TA2*Y1TR+1*TN2*Y1TI )
      gh3tu(1) = -ee/(4*sw*wm)*2*dcmplx(+TN3*Y3UR+TA3*Y3UI,+1*
     +TA3*Y3UR-1*TN3*Y3UI )
      gh3tu(2) = -ee/(4*sw*wm)*2*dcmplx(+TN3*Y1TR+TA3*Y1TI,-1*
     +TA3*Y1TR+1*TN3*Y1TI )
      gh1tc(1) = -ee/(4*sw*wm)*2*dcmplx(+TN1*Y3CR+TA1*Y3CI,+1*
     +TA1*Y3CR-1*TN1*Y3CI )
      gh1tc(2) = -ee/(4*sw*wm)*2*dcmplx(+TN1*Y2TR+TA1*Y2TI,-1*
     +TA1*Y2TR+1*TN1*Y2TI )
      gh2tc(1) = -ee/(4*sw*wm)*2*dcmplx(+TN2*Y3CR+TA2*Y3CI,+1*
     +TA2*Y3CR-1*TN2*Y3CI )
      gh2tc(2) = -ee/(4*sw*wm)*2*dcmplx(+TN2*Y2TR+TA2*Y2TI,-1*
     +TA2*Y2TR+1*TN2*Y2TI )
      gh3tc(1) = -ee/(4*sw*wm)*2*dcmplx(+TN3*Y3CR+TA3*Y3CI,+1*
     +TA3*Y3CR-1*TN3*Y3CI )
      gh3tc(2) = -ee/(4*sw*wm)*2*dcmplx(+TN3*Y2TR+TA3*Y2TI,-1*
     +TA3*Y2TR+1*TN3*Y2TI )
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

  
      gh1dd(1) = -ee/(2*sw*wm)*dcmplx(+TN1*Y1DR-TA1*Y1DI,-TA1*
     +Y1DR-TN1*Y1DI )
      gh1dd(2) = -ee/(2*sw*wm)*dcmplx(+TN1*Y1DR-TA1*Y1DI,+TA1*
     +Y1DR+TN1*Y1DI )
      gh2dd(1) = -ee/(2*sw*wm)*dcmplx(+TN2*Y1DR-TA2*Y1DI,-TA2*
     +Y1DR-TN2*Y1DI )
      gh2dd(2) = -ee/(2*sw*wm)*dcmplx(+TN2*Y1DR-TA2*Y1DI,+TA2*
     +Y1DR+TN2*Y1DI )
      gh3dd(1) = -ee/(2*sw*wm)*dcmplx(+TN3*Y1DR-TA3*Y1DI,-TA3*
     +Y1DR-TN3*Y1DI )
      gh3dd(2) = -ee/(2*sw*wm)*dcmplx(+TN3*Y1DR-TA3*Y1DI,+TA3*
     +Y1DR+TN3*Y1DI )
      gh1ds(1) = -ee/(4*sw*wm)*2*dcmplx(+TN1*Y1SR-TA1*Y1SI,-1*
     +TA1*Y1SR-1*TN1*Y1SI )
      gh1ds(2) = -ee/(4*sw*wm)*2*dcmplx(+TN1*Y2DR-TA1*Y2DI,+1*
     +TA1*Y2DR+1*TN1*Y2DI )
      gh2ds(1) = -ee/(4*sw*wm)*2*dcmplx(+TN2*Y1SR-TA2*Y1SI,-1*
     +TA2*Y1SR-1*TN2*Y1SI )
      gh2ds(2) = -ee/(4*sw*wm)*2*dcmplx(+TN2*Y2DR-TA2*Y2DI,+1*
     +TA2*Y2DR+1*TN2*Y2DI )
      gh3ds(1) = -ee/(4*sw*wm)*2*dcmplx(+TN3*Y1SR-TA3*Y1SI,-1*
     +TA3*Y1SR-1*TN3*Y1SI )
      gh3ds(2) = -ee/(4*sw*wm)*2*dcmplx(+TN3*Y2DR-TA3*Y2DI,+1*
     +TA3*Y2DR+1*TN3*Y2DI )
      gh1db(1) = -ee/(4*sw*wm)*2*dcmplx(+TN1*Y1BR-TA1*Y1BI,-1*
     +TA1*Y1BR-1*TN1*Y1BI )
      gh1db(2) = -ee/(4*sw*wm)*2*dcmplx(+TN1*Y3DR-TA1*Y3DI,+1*
     +TA1*Y3DR+1*TN1*Y3DI )
      gh2db(1) = -ee/(4*sw*wm)*2*dcmplx(+TN2*Y1BR-TA2*Y1BI,-1*
     +TA2*Y1BR-1*TN2*Y1BI )
      gh2db(2) = -ee/(4*sw*wm)*2*dcmplx(+TN2*Y3DR-TA2*Y3DI,+1*
     +TA2*Y3DR+1*TN2*Y3DI )
      gh3db(1) = -ee/(4*sw*wm)*2*dcmplx(+TN3*Y1BR-TA3*Y1BI,-1*
     +TA3*Y1BR-1*TN3*Y1BI )
      gh3db(2) = -ee/(4*sw*wm)*2*dcmplx(+TN3*Y3DR-TA3*Y3DI,+1*
     +TA3*Y3DR+1*TN3*Y3DI )

      gh1sd(1) = -ee/(4*sw*wm)*2*dcmplx(+TN1*Y2DR-TA1*Y2DI,-1*
     +TA1*Y2DR-1*TN1*Y2DI )
      gh1sd(2) = -ee/(4*sw*wm)*2*dcmplx(+TN1*Y1SR-TA1*Y1SI,+1*
     +TA1*Y1SR+1*TN1*Y1SI )
      gh2sd(1) = -ee/(4*sw*wm)*2*dcmplx(+TN2*Y2DR-TA2*Y2DI,-1*
     +TA2*Y2DR-1*TN2*Y2DI )
      gh2sd(2) = -ee/(4*sw*wm)*2*dcmplx(+TN2*Y1SR-TA2*Y1SI,+1*
     +TA2*Y1SR+1*TN2*Y1SI )
      gh3sd(1) = -ee/(4*sw*wm)*2*dcmplx(+TN3*Y2DR-TA3*Y2DI,-1*
     +TA3*Y2DR-1*TN3*Y2DI )
      gh3sd(2) = -ee/(4*sw*wm)*2*dcmplx(+TN3*Y1SR-TA3*Y1SI,+1*
     +TA3*Y1SR+1*TN3*Y1SI )
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
      gh1sb(1) = -ee/(4*sw*wm)*2*dcmplx(+TN1*Y2BR-TA1*Y2BI,-1*
     +TA1*Y2BR-1*TN1*Y2BI )
      gh1sb(2) = -ee/(4*sw*wm)*2*dcmplx(+TN1*Y3SR-TA1*Y3SI,+1*
     +TA1*Y3SR+1*TN1*Y3SI )
      gh2sb(1) = -ee/(4*sw*wm)*2*dcmplx(+TN2*Y2BR-TA2*Y2BI,-1*
     +TA2*Y2BR-1*TN2*Y2BI )
      gh2sb(2) = -ee/(4*sw*wm)*2*dcmplx(+TN2*Y3SR-TA2*Y3SI,+1*
     +TA2*Y3SR+1*TN2*Y3SI )
      gh3sb(1) = -ee/(4*sw*wm)*2*dcmplx(+TN3*Y2BR-TA3*Y2BI,-1*
     +TA3*Y2BR-1*TN3*Y2BI )
      gh3sb(2) = -ee/(4*sw*wm)*2*dcmplx(+TN3*Y3SR-TA3*Y3SI,+1*
     +TA3*Y3SR+1*TN3*Y3SI )

      gh1bd(1) = -ee/(4*sw*wm)*2*dcmplx(+TN1*Y3DR-TA1*Y3DI,-1*
     +TA1*Y3DR-1*TN1*Y3DI )
      gh1bd(2) = -ee/(4*sw*wm)*2*dcmplx(+TN1*Y1BR-TA1*Y1BI,+1*
     +TA1*Y1BR+1*TN1*Y1BI )
      gh2bd(1) = -ee/(4*sw*wm)*2*dcmplx(+TN2*Y3DR-TA2*Y3DI,-1*
     +TA2*Y3DR-1*TN2*Y3DI )
      gh2bd(2) = -ee/(4*sw*wm)*2*dcmplx(+TN2*Y1BR-TA2*Y1BI,+1*
     +TA2*Y1BR+1*TN2*Y1BI )
      gh3bd(1) = -ee/(4*sw*wm)*2*dcmplx(+TN3*Y3DR-TA3*Y3DI,-1*
     +TA3*Y3DR-1*TN3*Y3DI )
      gh3bd(2) = -ee/(4*sw*wm)*2*dcmplx(+TN3*Y1BR-TA3*Y1BI,+1*
     +TA3*Y1BR+1*TN3*Y1BI )
      gh1bs(1) = -ee/(4*sw*wm)*2*dcmplx(+TN1*Y3SR-TA1*Y3SI,-1*
     +TA1*Y3SR-1*TN1*Y3SI )
      gh1bs(2) = -ee/(4*sw*wm)*2*dcmplx(+TN1*Y2BR-TA1*Y2BI,+1*
     +TA1*Y2BR+1*TN1*Y2BI )
      gh2bs(1) = -ee/(4*sw*wm)*2*dcmplx(+TN2*Y3SR-TA2*Y3SI,-1*
     +TA2*Y3SR-1*TN2*Y3SI )
      gh2bs(2) = -ee/(4*sw*wm)*2*dcmplx(+TN2*Y2BR-TA2*Y2BI,+1*
     +TA2*Y2BR+1*TN2*Y2BI )
      gh3bs(1) = -ee/(4*sw*wm)*2*dcmplx(+TN3*Y3SR-TA3*Y3SI,-1*
     +TA3*Y3SR-1*TN3*Y3SI )
      gh3bs(2) = -ee/(4*sw*wm)*2*dcmplx(+TN3*Y2BR-TA3*Y2BI,+1*
     +TA3*Y2BR+1*TN3*Y2BI )
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

      ghmdu(1) = ee*Sqrt2/(4*sw*wm)*2*dcmplx(-Vud*Y1UI-Vcd*Y2U
     +I,-1*Vud*Y1UR-1*Vcd*Y2UR )*dcmplx(Zero,1)
      ghmdu(2) = ee*Sqrt2/(4*sw*wm)*2*dcmplx(-Vud*Y1DI+Vus*Y2D
     +I,+1*Vud*Y1DR-1*Vus*Y2DR )*dcmplx(Zero,1)
      ghmdc(1) = ee*Sqrt2/(4*sw*wm)*2*dcmplx(-Vud*Y1CI-Vcd*Y2C
     +I,-1*Vud*Y1CR-1*Vcd*Y2CR )*dcmplx(Zero,1)
      ghmdc(2) = ee*Sqrt2/(4*sw*wm)*2*dcmplx(-Vcd*Y1DI-Vcs*Y2D
     +I,+1*Vcd*Y1DR+1*Vcs*Y2DR )*dcmplx(Zero,1)
      ghmdt(1) = ee*Sqrt2/(4*sw*wm)*2*dcmplx(-Vud*Y1TI-Vcd*Y2T
     +I,-1*Vud*Y1TR-1*Vcd*Y2TR )*dcmplx(Zero,1)
      ghmdt(2) = ee*Sqrt2/(4*sw*wm)*2*dcmplx(-Y3DI,+1*Y3DR )*d
     +cmplx(Zero,1)
      ghmsu(1) = ee*Sqrt2/(4*sw*wm)*2*dcmplx(+Vus*Y1UI-Vcs*Y2U
     +I,+1*Vus*Y1UR-1*Vcs*Y2UR )*dcmplx(Zero,1)
      ghmsu(2) = ee*Sqrt2/(4*sw*wm)*2*dcmplx(-Vud*Y1SI+Vus*Y2S
     +I,+1*Vud*Y1SR-1*Vus*Y2SR )*dcmplx(Zero,1)
      ghmsc(1) = ee*Sqrt2/(4*sw*wm)*2*dcmplx(+Vus*Y1CI-Vcs*Y2C
     +I,+1*Vus*Y1CR-1*Vcs*Y2CR )*dcmplx(Zero,1)
      ghmsc(2) = ee*Sqrt2/(4*sw*wm)*2*dcmplx(-Vcd*Y1SI-Vcs*Y2S
     +I,+1*Vcd*Y1SR+1*Vcs*Y2SR )*dcmplx(Zero,1)
      ghmst(1) = ee*Sqrt2/(4*sw*wm)*2*dcmplx(+Vus*Y1TI-Vcs*Y2T
     +I,+1*Vus*Y1TR-1*Vcs*Y2TR )*dcmplx(Zero,1)
      ghmst(2) = ee*Sqrt2/(4*sw*wm)*2*dcmplx(-Y3SI,+1*Y3SR )*d
     +cmplx(Zero,1)
      ghmbu(1) = ee*Sqrt2/(4*sw*wm)*2*dcmplx(-Y3UI,-1*Y3UR )*d
     +cmplx(Zero,1)
      ghmbu(2) = ee*Sqrt2/(4*sw*wm)*2*dcmplx(-Vud*Y1BI+Vus*Y2B
     +I,+1*Vud*Y1BR-1*Vus*Y2BR )*dcmplx(Zero,1)
      ghmbc(1) = ee*Sqrt2/(4*sw*wm)*2*dcmplx(-Y3CI,-1*Y3CR )*d
     +cmplx(Zero,1)
      ghmbc(2) = ee*Sqrt2/(4*sw*wm)*2*dcmplx(-Vcd*Y1BI-Vcs*Y2B
     +I,+1*Vcd*Y1BR+1*Vcs*Y2BR )*dcmplx(Zero,1)
      ghmbt(1) = ee*Sqrt2/(4*sw*wm)*2*dcmplx(-Y3TI,-1*Y3TR )*d
     +cmplx(Zero,1)
      ghmbt(2) = ee*Sqrt2/(4*sw*wm)*2*dcmplx(-Y3BI,+1*Y3BR )*d
     +cmplx(Zero,1)

      ghpud(1) = -ee*Sqrt2/(4*sw*wm)*2*dcmplx(+Vud*Y1DI-Vus*Y2
     +DI,+1*Vud*Y1DR-1*Vus*Y2DR )*dcmplx(Zero,-1)
      ghpud(2) = -ee*Sqrt2/(4*sw*wm)*2*dcmplx(+Vud*Y1UI+Vcd*Y2
     +UI,-1*Vud*Y1UR-1*Vcd*Y2UR )*dcmplx(Zero,-1)
      ghpus(1) = -ee*Sqrt2/(4*sw*wm)*2*dcmplx(+Vud*Y1SI-Vus*Y2
     +SI,+1*Vud*Y1SR-1*Vus*Y2SR )*dcmplx(Zero,-1)
      ghpus(2) = -ee*Sqrt2/(4*sw*wm)*2*dcmplx(-Vus*Y1UI+Vcs*Y2
     +UI,+1*Vus*Y1UR-1*Vcs*Y2UR )*dcmplx(Zero,-1)
      ghpub(1) = -ee*Sqrt2/(4*sw*wm)*2*dcmplx(+Vud*Y1BI-Vus*Y2
     +BI,+1*Vud*Y1BR-1*Vus*Y2BR )*dcmplx(Zero,-1)
      ghpub(2) = -ee*Sqrt2/(4*sw*wm)*2*dcmplx(+Y3UI,-1*Y3UR )*
     +dcmplx(Zero,-1)
      ghpcd(1) = -ee*Sqrt2/(4*sw*wm)*2*dcmplx(+Vcd*Y1DI+Vcs*Y2
     +DI,+1*Vcd*Y1DR+1*Vcs*Y2DR )*dcmplx(Zero,-1)
      ghpcd(2) = -ee*Sqrt2/(4*sw*wm)*2*dcmplx(+Vud*Y1CI+Vcd*Y2
     +CI,-1*Vud*Y1CR-1*Vcd*Y2CR )*dcmplx(Zero,-1)
      ghpcs(1) = -ee*Sqrt2/(4*sw*wm)*2*dcmplx(+Vcd*Y1SI+Vcs*Y2
     +SI,+1*Vcd*Y1SR+1*Vcs*Y2SR )*dcmplx(Zero,-1)
      ghpcs(2) = -ee*Sqrt2/(4*sw*wm)*2*dcmplx(-Vus*Y1CI+Vcs*Y2
     +CI,+1*Vus*Y1CR-1*Vcs*Y2CR )*dcmplx(Zero,-1)
      ghpcb(1) = -ee*Sqrt2/(4*sw*wm)*2*dcmplx(+Vcd*Y1BI+Vcs*Y2
     +BI,+1*Vcd*Y1BR+1*Vcs*Y2BR )*dcmplx(Zero,-1)
      ghpcb(2) = -ee*Sqrt2/(4*sw*wm)*2*dcmplx(+Y3CI,-1*Y3CR )*
     +dcmplx(Zero,-1)
      ghptd(1) = -ee*Sqrt2/(4*sw*wm)*2*dcmplx(+Y3DI,+1*Y3DR )*
     +dcmplx(Zero,-1)
      ghptd(2) = -ee*Sqrt2/(4*sw*wm)*2*dcmplx(+Vud*Y1TI+Vcd*Y2
     +TI,-1*Vud*Y1TR-1*Vcd*Y2TR )*dcmplx(Zero,-1)
      ghpts(1) = -ee*Sqrt2/(4*sw*wm)*2*dcmplx(+Y3SI,+1*Y3SR )*
     +dcmplx(Zero,-1)
      ghpts(2) = -ee*Sqrt2/(4*sw*wm)*2*dcmplx(-Vus*Y1TI+Vcs*Y2
     +TI,+1*Vus*Y1TR-1*Vcs*Y2TR )*dcmplx(Zero,-1)
      ghptb(1) = -ee*Sqrt2/(4*sw*wm)*2*dcmplx(+Y3BI,+1*Y3BR )*
     +dcmplx(Zero,-1)
      ghptb(2) = -ee*Sqrt2/(4*sw*wm)*2*dcmplx(+Y3TI,-1*Y3TR )*
     +dcmplx(Zero,-1)

      ghmeve(1) = ee*Sqrt2/(4*sw*wm)*2*dcmplx(Zero,Zero )*dcmp
     +lx(Zero,1)
      ghmeve(2) = ee*Sqrt2/(4*sw*wm)*2*dcmplx(-Y1EI,+1*Y1ER )*
     +dcmplx(Zero,1)
      ghmevm(1) = ee*Sqrt2/(4*sw*wm)*2*dcmplx(Zero,Zero )*dcmp
     +lx(Zero,1)
      ghmevm(2) = ee*Sqrt2/(4*sw*wm)*2*dcmplx(-Y2EI,+1*Y2ER )*
     +dcmplx(Zero,1)
      ghmevt(1) = ee*Sqrt2/(4*sw*wm)*2*dcmplx(Zero,Zero )*dcmp
     +lx(Zero,1)
      ghmevt(2) = ee*Sqrt2/(4*sw*wm)*2*dcmplx(-Y3EI,+1*Y3ER )*
     +dcmplx(Zero,1)
      ghmmuve(1) = ee*Sqrt2/(4*sw*wm)*2*dcmplx(Zero,Zero )*dcm
     +plx(Zero,1)
      ghmmuve(2) = ee*Sqrt2/(4*sw*wm)*2*dcmplx(-Y1MUI,+1*Y1MUR
     + )*dcmplx(Zero,1)
      ghmmuvm(1) = ee*Sqrt2/(4*sw*wm)*2*dcmplx(Zero,Zero )*dcm
     +plx(Zero,1)
      ghmmuvm(2) = ee*Sqrt2/(4*sw*wm)*2*dcmplx(-Y2MUI,+1*Y2MUR
     + )*dcmplx(Zero,1)
      ghmmuvt(1) = ee*Sqrt2/(4*sw*wm)*2*dcmplx(Zero,Zero )*dcm
     +plx(Zero,1)
      ghmmuvt(2) = ee*Sqrt2/(4*sw*wm)*2*dcmplx(-Y3MUI,+1*Y3MUR
     + )*dcmplx(Zero,1)
      ghmtave(1) = ee*Sqrt2/(4*sw*wm)*2*dcmplx(Zero,Zero )*dcm
     +plx(Zero,1)
      ghmtave(2) = ee*Sqrt2/(4*sw*wm)*2*dcmplx(-Y1TAI,+1*Y1TAR
     + )*dcmplx(Zero,1)
      ghmtavm(1) = ee*Sqrt2/(4*sw*wm)*2*dcmplx(Zero,Zero )*dcm
     +plx(Zero,1)
      ghmtavm(2) = ee*Sqrt2/(4*sw*wm)*2*dcmplx(-Y2TAI,+1*Y2TAR
     + )*dcmplx(Zero,1)
      ghmtavt(1) = ee*Sqrt2/(4*sw*wm)*2*dcmplx(Zero,Zero )*dcm
     +plx(Zero,1)
      ghmtavt(2) = ee*Sqrt2/(4*sw*wm)*2*dcmplx(-Y3TAI,+1*Y3TAR
     + )*dcmplx(Zero,1)

      ghpvee(1) = -ee*Sqrt2/(4*sw*wm)*2*dcmplx(+Y1EI,+1*Y1ER )
     +*dcmplx(Zero,-1)
      ghpvee(2) = -ee*Sqrt2/(4*sw*wm)*2*dcmplx(Zero,Zero )*dcm
     +plx(Zero,-1)
      ghpvemu(1) = -ee*Sqrt2/(4*sw*wm)*2*dcmplx(+Y1MUI,+1*Y1MU
     +R )*dcmplx(Zero,-1)
      ghpvemu(2) = -ee*Sqrt2/(4*sw*wm)*2*dcmplx(Zero,Zero )*dc
     +mplx(Zero,-1)
      ghpveta(1) = -ee*Sqrt2/(4*sw*wm)*2*dcmplx(+Y1TAI,+1*Y1TA
     +R )*dcmplx(Zero,-1)
      ghpveta(2) = -ee*Sqrt2/(4*sw*wm)*2*dcmplx(Zero,Zero )*dc
     +mplx(Zero,-1)
      ghpvme(1) = -ee*Sqrt2/(4*sw*wm)*2*dcmplx(+Y2EI,+1*Y2ER )
     +*dcmplx(Zero,-1)
      ghpvme(2) = -ee*Sqrt2/(4*sw*wm)*2*dcmplx(Zero,Zero )*dcm
     +plx(Zero,-1)
      ghpvmmu(1) = -ee*Sqrt2/(4*sw*wm)*2*dcmplx(+Y2MUI,+1*Y2MU
     +R )*dcmplx(Zero,-1)
      ghpvmmu(2) = -ee*Sqrt2/(4*sw*wm)*2*dcmplx(Zero,Zero )*dc
     +mplx(Zero,-1)
      ghpvmta(1) = -ee*Sqrt2/(4*sw*wm)*2*dcmplx(+Y2TAI,+1*Y2TA
     +R )*dcmplx(Zero,-1)
      ghpvmta(2) = -ee*Sqrt2/(4*sw*wm)*2*dcmplx(Zero,Zero )*dc
     +mplx(Zero,-1)
      ghpvte(1) = -ee*Sqrt2/(4*sw*wm)*2*dcmplx(+Y3EI,+1*Y3ER )
     +*dcmplx(Zero,-1)
      ghpvte(2) = -ee*Sqrt2/(4*sw*wm)*2*dcmplx(Zero,Zero )*dcm
     +plx(Zero,-1)
      ghpvtmu(1) = -ee*Sqrt2/(4*sw*wm)*2*dcmplx(+Y3MUI,+1*Y3MU
     +R )*dcmplx(Zero,-1)
      ghpvtmu(2) = -ee*Sqrt2/(4*sw*wm)*2*dcmplx(Zero,Zero )*dc
     +mplx(Zero,-1)
      ghpvtta(1) = -ee*Sqrt2/(4*sw*wm)*2*dcmplx(+Y3TAI,+1*Y3TA
     +R )*dcmplx(Zero,-1)
      ghpvtta(2) = -ee*Sqrt2/(4*sw*wm)*2*dcmplx(Zero,Zero )*dc
     +mplx(Zero,-1)

      

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

c----------------------------
c end subroutine coupsm
c----------------------------


      return
      end
