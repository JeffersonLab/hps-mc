!--------------------------------instuf.f-------------------------------
! Version: 051219-1435
! Reference:
!-----------------------------------------------------------------------
!23456789|123456789|123456789|123456789|123456789|123456789|123456789|12

      common/INSTUF/           ! Input data read in (UNIT=5) by getrtz.f
     * title(80),                                    ! Title card (80A1) 
     * ekein,                            ! Incident kinetic energy (MeV)
     * xin,yin,zin,                          ! Incident coordinates (cm)
     * uin,vin,win,                         ! Incident direction cosines
     * iin,jin,kin,                                   ! Incident indices
     * irin,        ! Indicent region (determined with incident indices) 
     * iqin,                                           ! Incident charge
     * ncases,                      ! Number of cases (histories) to run
     * isamp                                    ! Energy sampling switch

      real*8 ekein,xin,yin,zin,uin,vin,win
      integer iin,jin,kin,irin,iqin,ncases,isamp
      character*1 title

!---------------------------last line of instuf.f-----------------------
