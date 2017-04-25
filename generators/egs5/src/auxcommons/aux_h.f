!-------------------------------aux_h.f---------------------------------
! Version: 051219-1435
! Reference:
!-----------------------------------------------------------------------
!23456789|123456789|123456789|123456789|123456789|123456789|123456789|12

! ---------------------------------------------
! Auxiliary-code "header" containing PARAMETERS
! ---------------------------------------------

! Maximum radius bin size (common/RDATA/)
      integer MXRBIN
      parameter (MXRBIN = 1000)

! Maximum energy bin size (common/EDATA/)
      integer MXEBIN
      parameter (MXEBIN = 1000)

! Maximum radius tally bins (common/ETALY3/)
      integer MXRTB
      parameter (MXRTB = 200)

! Maximum z tally bins (common/ETALY3/)
      integer MXZTB
      parameter (MXZTB = 400)

! Maximum number of cylinders (common/CYLDTA/)
      integer MXCYLS
      parameter (MXCYLS = 100)

! Maximum number of spheres (common/SPHDTA/)
      integer MXSPHE
      parameter (MXSPHE = 75)      

! Maximum number of cones (common/CONDTA/)
      integer MXCONES
      parameter (MXCONES = 100)

! Maximum number of ALL planes (X, Y and Z OR Z and T) (common/PLADTA)
      integer MXPLNS
      parameter (MXPLNS = 1000)

! Maximum T (theta) index
      integer MXTPLNS
      parameter (MXTPLNS = 1)

! Maximum X (x-direction) index
      integer MXXPLNS
      parameter (MXXPLNS = 100)

! Maximum Y (y-direction) index
      integer MXYPLNS
      parameter (MXYPLNS = 100)

! Maximum Z (z-direction) index
      integer MXZPLNS
      parameter (MXZPLNS = 100)
      

! ---------------------------------
! Voxcel geometry related parameter
! ---------------------------------
! Maximum number of x-bin size (common/score)
      integer LIMAX
      parameter (LIMAX = 22)

! Maximum number of y-bin size (common/score/)
      integer LJMAX
      parameter (LJMAX = 22)

! Maximum number of z-bin size (common/score/)
      integer LKMAX
      parameter (LKMAX = 22)

! Number of batch (common/score/)
      integer LSTAT
      parameter (LSTAT = 10)      

! Maximum number of LITMAX (common/score/)
      integer LITMAX
      parameter (LITMAX = 1)

! Maximum number of groups of region to analyse)
      integer LMXDOS
      parameter (LMXDOS = 5)

!-------------------------last line of aux_h.f--------------------------
