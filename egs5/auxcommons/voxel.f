
!--------------------------------voxel.f--------------------------------
! Version: 040630-1730
!-----------------------------------------------------------------------
!23456789|123456789|123456789|123456789|123456789|123456789|123456789|12

! ----------------------------------------------------------------------
! The indices including VOXEL are set at subroutine getvoxcel from 
! input data 'Record 10' (il,iu, jl,ju, kl,ku,izscan).
! The results of region from il to iu (in X-direction), jl to ju (in 
! Y-direction) and kl to ku (in Z-direction) will be printed.
! IZSCAN non-zero is set to get z-scan per page, and otherwise is set
! to get x-scan per page.
! idgrp is number of groups of output.
!
! ----------------------------------------------------------------------

      common/VOXEL/
     *            idosl(LMXDOS),idosu(LMXDOS),jdosl(LMXDOS),
     *            jdosu(LMXDOS),kdosl(LMXDOS),kdosu(LMXDOS),
     *            izscan(LMXDOS),idgrp
      integer area,idgrp,idosl,idosu,jdosl,jdosu,kdosl,kdosu,
     *        izscan

!---------------------------last line of voxel.f-----------------------
