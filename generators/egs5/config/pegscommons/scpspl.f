!----------------------------------scpspl.f-----------------------------
!  Version: 051219-1435
!-----------------------------------------------------------------------
!23456789|123456789|123456789|123456789|123456789|123456789|123456789|12

!  stores data from tabulated cross section data:

      COMMON/SCPSPL/ 
     +       ETL(NEGRDS),        !  log of energy grid
     +       AG1E(NEGRDS),       !  first electron spline coefficient
     +       BG1E(NEGRDS),       !  second electron spline coefficient
     +       CG1E(NEGRDS),       !  third electron spline coefficient
     +       DG1E(NEGRDS),       !  fourth electron spline coefficient
     +       AG1P(NEGRDS),       !  first positron spline coefficient
     +       BG1P(NEGRDS),       !  second positron spline coefficient
     +       CG1P(NEGRDS),       !  third positron spline coefficient
     +       DG1P(NEGRDS)        !  fourth positron spline coefficient

      double precision ETL, AG1E,BG1E,CG1E,DG1E, AG1P,BG1P,CG1P,DG1P 

!--------------------------last line------------------------------------
