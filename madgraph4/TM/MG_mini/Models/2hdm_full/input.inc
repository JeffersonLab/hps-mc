c
c     Input values 
c
c
c     Input values
c
      double precision alpha, gfermi, alpha_s,alfas
      common/sm_constants/ alpha, gfermi, alpha_s,alfas


      double precision      vud,vus,vub,vcd,vcs,vcb,vtd,vts,vtb
      common/sm_ckm/  vud,vus,vub,vcd,vcs,vcb,vtd,vts,vtb

      double precision lambda1, lambda2, lambda3,lambda4,lambda5,Rlambda6,
     +                  Ilambda6,Rlambda7,Ilambda7
      common/lambdas/ lambda1, lambda2, lambda3,lambda4,lambda5,Rlambda6,
     +                  Ilambda6,Rlambda7,Ilambda7



      double precision TH1, TH2, TH3, TN1, TN2, TN3,TA1, TA2, TA3
      common/t_matrix/ TH1, TH2, TH3, TN1, TN2, TN3,TA1, TA2, TA3

c     Yukawa masses


      double precision MSDMASS,MSBMASS,MSTMASS,MSSMASS,MSUMASS,MSCMASS,
     +MSMUMASS,MSTAMASS,MSEMASS
      common/ms_masses/ MSDMASS,MSBMASS,MSTMASS,MSSMASS,MSUMASS,MSCMASS,
     +MSMUMASS,MSTAMASS,MSEMASS



c     Coupling parameters
      double precision  Y1DR
      double precision  Y1DI
      double precision  Y2DR
      double precision  Y2DI
      double precision  Y3DR
      double precision  Y3DI
      double precision  Y1SR
      double precision  Y1SI
      double precision  Y2SR
      double precision  Y2SI
      double precision  Y3SR
      double precision  Y3SI
      double precision  Y1BR
      double precision  Y1BI
      double precision  Y2BR
      double precision  Y2BI
      double precision  Y3BR
      double precision  Y3BI
      double precision  Y1UR
      double precision  Y1UI
      double precision  Y2UR
      double precision  Y2UI
      double precision  Y3UR
      double precision  Y3UI
      double precision  Y1CR
      double precision  Y1CI
      double precision  Y2CR
      double precision  Y2CI
      double precision  Y3CR
      double precision  Y3CI
      double precision  Y1TR
      double precision  Y1TI
      double precision  Y2TR
      double precision  Y2TI
      double precision  Y3TR
      double precision  Y3TI
      double precision  Y1ER
      double precision  Y1EI
      double precision  Y2ER
      double precision  Y2EI
      double precision  Y3ER
      double precision  Y3EI
      double precision  Y1MUR
      double precision  Y1MUI
      double precision  Y2MUR
      double precision  Y2MUI
      double precision  Y3MUR
      double precision  Y3MUI
      double precision  Y1TAR
      double precision  Y1TAI
      double precision  Y2TAR
      double precision  Y2TAI
      double precision  Y3TAR
      double precision  Y3TAI
      common/param_Y/   Y1DR,Y1DI,Y2DR,Y2DI,Y3DR
     +,Y3DI,Y1SR,Y1SI,Y2SR,Y2SI,Y3SR,Y3SI,Y1BR
     +,Y1BI,Y2BR,Y2BI,Y3BR,Y3BI,Y1UR,Y1UI,Y2UR
     +,Y2UI,Y3UR,Y3UI,Y1CR,Y1CI,Y2CR,Y2CI,Y3CR
     +,Y3CI,Y1TR,Y1TI,Y2TR,Y2TI,Y3TR,Y3TI,Y1ER
     +,Y1EI,Y2ER,Y2EI,Y3ER,Y3EI,Y1MUR,Y1MUI,Y2MUR
     +,Y2MUI,Y3MUR,Y3MUI,Y1TAR,Y1TAI,Y2TAR,Y2TAI
     +,Y3TAR,Y3TAI
     

