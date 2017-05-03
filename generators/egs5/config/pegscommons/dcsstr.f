!----------------------------------dcsstr.f-----------------------------
!  Version: 060308-2100
!-----------------------------------------------------------------------
!23456789|123456789|123456789|123456789|123456789|123456789|123456789|12

!  stores material dependent data from tabulated cross section data:

      COMMON/DCSSTR/ 
     +     MEDNAM(MXMED,24),          !  names of the media
     +     ATOMD(MXMED),              !  atom density
     +     SECS(MXMED,NEGRID),        !  e- cross section
     +     SETCS1(MXMED,NEGRID),      !  e- 1st transport cross section
     +     SETCS2(MXMED,NEGRID),      !  e- 2nd transport cross section
     +     SEDCS(MXMED,NEGRID,NREDA), !  e- differential cross section
     +     SPCS(MXMED,NEGRID),        !  e+ cross section
     +     SPTCS1(MXMED,NEGRID),      !  e+ 1st transport cross section
     +     SPTCS2(MXMED,NEGRID),      !  e+ 2nd transport cross section
     +     SPDCS(MXMED,NEGRID,NREDA), !  e+ differential cross section
     +     EGRDHI(MXMED),             !  essentially, UE
     +     EGRDLO(MXMED),             !  essentially, AE
     +     NLEGMD(MXMED)              !  # legendres for GS dist

      character*4 MEDNAM
      integer NLEGMD
      double precision ATOMD, 
     +                 SECS, SETCS1, SETCS2, SEDCS,
     +                 SPCS, SPTCS1, SPTCS2, SPDCS, EGRDHI, EGRDLO

      COMMON/DCSSTR2/ 
     +     EFRCH(MXMED),              !  efrach for this medium
     +     EFRCL(MXMED),              !  efracl for this medium
     +     NSDCS                      !  number of materials stored

      integer NSDCS
      double precision EFRCH, EFRCL

!--------------------------last line------------------------------------
