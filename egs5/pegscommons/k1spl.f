!----------------------------------k1spl.f------------------------------
!  Version: 060316-1330
!-----------------------------------------------------------------------
!23456789|123456789|123456789|123456789|123456789|123456789|123456789|12

      COMMON/K1SPL/ 
     +       EHINGE(NESCPW),     !  energy grid (kinetic)
     +       AK1E(NESCPW),       !  first electron spline coefficient
     +       BK1E(NESCPW),       !  second electron spline coefficient
     +       CK1E(NESCPW),       !  third electron spline coefficient
     +       DK1E(NESCPW),       !  fourth electron spline coefficient
     +       AK1P(NESCPW),       !  first positron spline coefficient
     +       BK1P(NESCPW),       !  second positron spline coefficient
     +       CK1P(NESCPW),       !  third positron spline coefficient
     +       DK1P(NESCPW),       !  fourth positron spline coefficient
     +       NHINGE              !  number of hinges

      integer NHINGE
      double precision EHINGE, AK1E,BK1E,CK1E,DK1E, AK1P,BK1P,CK1P,DK1P 

!--------------------------last line------------------------------------
