!----------------------------mscom.f------------------------------------
!  Version: 060314-0825
!-----------------------------------------------------------------------
!23456789|123456789|123456789|123456789|123456789|123456789|123456789|12

! ----------------------------------------
! PEGS5 common file for MSCOM - medium dependent flags
! ----------------------------------------

      COMMON/MSCOM/
     & efracH,                           !  High energy ESTEPE
     & efracL,                           !  Low energy ESTEPE
     & fudgeMS,                          !  e- contribution to MScat
     & rhosav,                           !  density
     & nleg0                             !  # Legendre coeffs in GS

      integer nleg0
      double precision efracH, efracL, fudgeMS, rhosav
      
!--------------------------last line------------------------------------
