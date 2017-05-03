!---------------------------egs5_scpw.f---------------------------------
! Version: 051219-1435
!-----------------------------------------------------------------------
!23456789|123456789|123456789|123456789|123456789|123456789|123456789|12

      common/SCPW/                               ! scattering power data
     * escpw0(MXEKE,MXMED),             ! first e- scpow interp constant
     * escpw1(MXEKE,MXMED),            ! second e- scpow interp constant
     * pscpw0(MXEKE,MXMED),             ! first e+ scpow interp constant
     * pscpw1(MXEKE,MXMED),            ! second e+ scpow interp constant
     * ekini0(MXEKE,MXMED),             ! first e- kinit interp constant
     * ekini1(MXEKE,MXMED),            ! second e- kinit interp constant
     * pkini0(MXEKE,MXMED),             ! first e+ kinit interp constant
     * pkini1(MXEKE,MXMED),            ! second e+ kinit interp constant
     * ek1s0(MXEKE,MXMED),             ! first e- k1s0 interp constant
     * ek1s1(MXEKE,MXMED),            ! second e- k1s0 interp constant
     * pk1s0(MXEKE,MXMED),             ! first e+ k1s0 interp constant
     * pk1s1(MXEKE,MXMED),            ! second e+ k1s0 interp constant
     * meke(MXMED)                       ! number of energy ladder steps

      real*8 escpw0, escpw1, pscpw0, pscpw1, 
     *       ekini0, ekini1, pkini0, pkini1,
     *       ek1s0, ek1s1, pk1s0, pk1s1
      integer meke

!-----------------------last line of egs5_scpw.f------------------------
