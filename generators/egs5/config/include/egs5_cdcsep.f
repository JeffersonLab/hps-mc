!------------------------------egs5_cdcsep.f----------------------------
! Version: 051219-1435
! Reference from source developed and provided by F. Salvat, Dec 2001
!-----------------------------------------------------------------------
!23456789|123456789|123456789|123456789|123456789|123456789|123456789|12

!  Stores data from tabulated PW cross section data:

      common/CDCSEP/ 
     +       ET(NEGRDS),         !  Energy grid
     +       TH(NREDA),          !  Angle grid
     +       THR(NREDA),         !  reduced angle grid
     +       XMU(NREDA),         !  (1-cos)/2 grid
     +       ECS(NEGRID),        !  electron cross section
     +       ETCS1(NEGRDS),      !  electron 1st transport cross section
     +       ETCS2(NEGRID),      !  electron 2nd transport cross section
     +       EDCS(NEGRID,NREDA), !  electron differential cross section
     +       PCS(NEGRID),        !  positron cross section
     +       PTCS1(NEGRDS),      !  positron 1st transport cross section
     +       PTCS2(NEGRID),      !  positron 2nd transport cross section
     +       PDCS(NEGRID,NREDA), !  positron differential cross section
     +       DCSI(NREDA),        !  differential cross section
     +       DCSIL(NREDA)        !  differential cross section

      double precision ET, TH, THR, XMU, ECS, ETCS1, ETCS2, EDCS,
     +       PCS, PTCS1, PTCS2, PDCS, DCSI, DCSIL

!-----------------------last line of egs5_cdcsep.f----------------------
