!----------------------------pwlfin.f-----------------------------------
!  Version: 051219-1435
!-----------------------------------------------------------------------
!23456789|123456789|123456789|123456789|123456789|123456789|123456789|12

! ----------------------------------------
! PEGS5 common file for PWLFIN
! ----------------------------------------

      integer nipe,nale,nipg,nalg,nipr,nalr,nips,nals,nipc,nalc
      double precision epe,zthre,zepe, epg,zthrg,zepg, epr,zthrr,zepr,
     &                 epsf,zthrs,zeps, epcp,zthrc,zepc
      COMMON/PWLFIN/EPE,ZTHRE(40),ZEPE(40),EPG,ZTHRG(40),ZEPG(40),EPR,
     &              ZTHRR(40),ZEPR(40),EPSF,ZTHRS(40),ZEPS(40),EPCP,
     &              ZTHRC(200),ZEPC(200),
     &              NIPE,NALE,NIPG,NALG,NIPR,NALR,NIPS,NALS,NIPC,NALC

!--------------------------last line------------------------------------
