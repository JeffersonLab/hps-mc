!------------------------------egs5_thresh.f----------------------------
! Version: 051219-1435
!-----------------------------------------------------------------------
!23456789|123456789|123456789|123456789|123456789|123456789|123456789|12

      common/THRESH/                    ! Threshold (and other) energies
     * RMT2,                              ! Two times electron rest mass
     * RMSQ,                                ! Electron rest mass squared
     * thdum3,           !**** Dummy position marker to prevent mismatch
     * ap(MXMED),              ! PEGS lower photon cutoff (total) energy
     * ae(MXMED),            ! PEGS lower electron cutoff (total) energy
     * up(MXMED),              ! PEGS upper photon cutoff (total) energy
     * ue(MXMED),            ! PEGS upper electron cutoff (total) energy
     * te(MXMED),            ! PEGS lower electron cutoff (total) energy
     * thmoll(MXMED)                   ! Moller threshold (total) energy

      real*8 RMT2,RMSQ,thdum3,ap,ae,up,ue,te,thmoll

!-----------------------last line of egs5_thresh.f---------------------
