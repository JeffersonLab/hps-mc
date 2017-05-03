!-------------------------------egs5_mscon.f----------------------------
! Version: 060313-1030
!-----------------------------------------------------------------------
!23456789|123456789|123456789|123456789|123456789|123456789|123456789|12

! Store output for scattering power and ms fitting constants in PEGS

      COMMON/MSCON/ 
     +       MSCATE(NESCPW),            !  Energy grid
     +       SCPOW(2,NESCPW),           !  Scattering power
     +       AMUMS(2,NMSE,NK1,NFIT1),   !  endpoint of the fit interval
     +       AMS(2,NMSE,NK1,NFIT),      !  First fitting coeff
     +       BMS(2,NMSE,NK1,NFIT),      !  Second fitting coeff
     +       CMS(2,NMSE,NK1,NFIT),      !  Third fitting coeff
     +       ETAMS(2,NMSE,NK1,NFIT),    !  Fitted 'screening parameter'
     +       CUMDIST(2,NMSE,NK1,NEXFIT),!  Cum msdist over each interval
     +       PROBNS(2,NMSE,NK1),        !  No Scatter probabilities
     +       K1MINE,                    !  e- min scat strngth, all med
     +       K1MAXE,                    !  e- max scat strngth, all med
     +       K1MINP,                    !  e+ min scat strngth, all med
     +       K1MAXP,                    !  e+ max scat strngth, all med
     +       GSOPT(MXMED),              !  flag to use Salvat GS MS
     +       NMSCPW,                    !  Actual # of scat pow points
     +       NMSCATE,                   !  Actual # of mscat points
     +       DECADE1, DECADE2,          !  The decade range of grid
     +       JOFFSET                    !  1st decade points to skip

      double precision MSCATE, SCPOW, AMUMS, AMS, BMS, CMS, ETAMS, 
     +                 CUMDIST, PROBNS, K1MINE, K1MAXE, K1MINP, K1MAXP
      integer GSOPT, NMSCPW, NMSCATE, DECADE1, DECADE2, JOFFSET

!-----------------------last line of egs5_mscon.f-----------------------
