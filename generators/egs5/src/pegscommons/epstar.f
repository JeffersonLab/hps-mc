!---------------------------epstar.f------------------------------------
!  Version: 051219-1435
!-----------------------------------------------------------------------
!23456789|123456789|123456789|123456789|123456789|123456789|123456789|12

! ----------------------------------------
! PEGS5 common file for EPSTAR
! ----------------------------------------

      character*1 epsttl(80)
      integer epstfl, nepst, iepst, neleps, zepst, iaprim, iaprfl
      double precision epsten, epstd, wepst
      COMMON/EPSTAR/EPSTEN(150),EPSTD(150),EPSTFL,EPSTTL,NEPST,IEPST, 
     &              NELEPS,ZEPST(20),WEPST(20),IAPRIM,IAPRFL

!--------------------------last line------------------------------------
