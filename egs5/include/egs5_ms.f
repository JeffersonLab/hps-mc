!-------------------------------egs5_ms.f-------------------------------
! Version: 060313-1000
!-----------------------------------------------------------------------
!23456789|123456789|123456789|123456789|123456789|123456789|123456789|12

!  Holds multiple scattering fitting parameters from PEGS

      COMMON/MS/ 
     +   eamu(NFIT1,2,NMSE,NK1,MXMED),   ! endpoints of the e- intervals
     +   ebms(NFIT,2,NMSE,NK1,MXMED),    ! Second e- fitting coeff
     +   eetams(NFIT,2,NMSE,NK1,MXMED),  ! Fitted e- screening parameter
     +   ecdf(NEXFIT1,2,NMSE,NK1,MXMED), ! e- cdf for final interval
     +   pnoscat(2,NMSE,NK1,MXMED),      ! non-scatter probability
     +   msgrid(NMSE,MXMED),             ! energy grid
     +   k1grd(2,NK1),                   ! K1 grid
     +   dk1log(2),                      ! K1 grid spacing constant
     +   nmsgrd(MXMED),                  ! number of energy grid pts.
     +   initde(MXMED),                  ! initial energy decade
     +   nmsdec(MXMED),                  ! energy grid points per decade
     +   jskip(MXMED),                   ! number to skip in 1st decade
     +   neqp(MXMED),                    ! actual # eq prob angles
     +   neqa(MXMED),                    ! actual # eq spaced angles
     +   tmxset

      logical tmxset
      double precision eamu, ebms, eetams, ecdf, pnoscat,
     +                 msgrid, k1grd, dk1log
      integer nmsgrd, initde, nmsdec, jskip, neqp, neqa

!-----------------------last line of egs5_ms----------------------------
