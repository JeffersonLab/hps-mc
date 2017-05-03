!----------------------------cpcom.f------------------------------------
!  Version: 051219-1435
!-----------------------------------------------------------------------
!23456789|123456789|123456789|123456789|123456789|123456789|123456789|12

! ----------------------------------------
! PEGS5 common file for CPCOM (Compton Profile Commons)
! ----------------------------------------

      integer nshell, icprof, mxshel, mxraw
      double precision cprof, avcprf, cprofi, qcap, qcap10, scprof,
     &                 scpsum, scproi, capin, capio, capils, elecni,
     &                 elecnj, elecno, cpimev
      COMMON/CPCOM/CPROF(31,102),AVCPRF(31),CPROFI(301),QCAP(31), 
     &             QCAP10(301),SCPROF(31,200),SCPSUM(31,200),
     &             SCPROI(301,200),CAPIN(200),CAPIO(200),CAPILS(200),
     &             ELECNI(200),ELECNJ(200),ELECNO(200),CPIMEV,
     &             NSHELL(200),ICPROF,MXSHEL,MXRAW

!--------------------------last line------------------------------------
