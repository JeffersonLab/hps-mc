!---------------------------------rngspl.f------------------------------
!  Version: 060316-1500
!-----------------------------------------------------------------------
!23456789|123456789|123456789|123456789|123456789|123456789|123456789|12

      COMMON/RNGSPL/ 
     +       ERNG(NESCPW),       !  energy grid (kinetic)
     +       ARNGE(NESCPW),      !  first electron spline coefficient
     +       BRNGE(NESCPW),      !  second electron spline coefficient
     +       CRNGE(NESCPW),      !  third electron spline coefficient
     +       DRNGE(NESCPW),      !  fourth electron spline coefficient
     +       ARNGP(NESCPW),      !  first positron spline coefficient
     +       BRNGP(NESCPW),      !  second positron spline coefficient
     +       CRNGP(NESCPW),      !  third positron spline coefficient
     +       DRNGP(NESCPW),      !  fourth positron spline coefficient
     +       ESTEPL(NESCPW),     !  limit computed for energy loss steps
     +       AESTE(NESCPW),      !  first estepe spline coefficient
     +       BESTE(NESCPW),      !  second estepe spline coefficient
     +       CESTE(NESCPW),      !  third estepe spline coefficient
     +       DESTE(NESCPW),      !  fourth estepe spline coefficient
     +       NRNG                !  number of csda ranges 

      integer NRNG
      double precision ERNG, ESTEPL,  ARNGE,BRNGE,CRNGE,DRNGE, 
     +                                ARNGP,BRNGP,CRNGP,DRNGP,
     +                                AESTE,BESTE,CESTE,DESTE

!--------------------------last line------------------------------------
