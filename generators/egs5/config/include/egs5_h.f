!------------------------------egs5_h.f---------------------------------
! Version: 060313-0945
!-----------------------------------------------------------------------
!23456789|123456789|123456789|123456789|123456789|123456789|123456789|12

! EGS5 "header" file containing PARAMETERS

! Maximum stack size
      integer MXSTACK
      parameter (MXSTACK = 125)

! Maximum number of different media (excluding vacuum)
      integer MXMED
!      parameter (MXMED = 4)
      parameter (MXMED = 10)

! Maximum number of regions allocated
      integer MXREG
      parameter (MXREG = 5000)
!      parameter (MXREG = 2097153)

! Maximum number of gamma-mapped energy intervals
      integer MXGE
      parameter (MXGE = 1000)

! Maximum number of Rayleigh atomic form-factor intervals
      integer MXRAYFF
      parameter (MXRAYFF = 100)

! Maximum number of elements in medium
      integer MXEPERMED
      parameter (MXEPERMED = 20)

! Maximum number of gamma small-energy intervals
      integer MXSGE
!      parameter (MXSGE = 1)
      parameter (MXSGE = MXGE)   !  for UCTESTSR

! Maximum number iarg values (calls to AUSGAB)
      integer MXAUS
      parameter (MXAUS = 31)

! Set this to MXAUS value less 5
      integer MXAUSM5
      parameter (MXAUSM5 = 26)

! Maximum number of elements in a medium (from PEGS5)
      integer MXEL
      parameter (MXEL = 50)

! Maximum number of angle intervals in (0,5*PI/2) for sine
      integer MXSINC
      parameter (MXSINC = 1002)

! Size of table of inverse powers of two
      integer MXPWR2I
      parameter (MXPWR2I = 50)

! Cumulative electron mean-free-path
      integer MXCMFP
      parameter (MXCMFP = 1)

! Electron mapped energy intervals
      integer MXEKE
      parameter (MXEKE = 150)

! Electron energy intervals below ekelim
      integer MXLEKE
      parameter (MXLEKE = 1)

! Moliere's lower case b
      integer MXBLC
      parameter (MXBLC = 1)

! Random number for submedian angles
      integer MXRNTH
      parameter (MXRNTH = 1)

! Random number for supermedian angles
      integer MXRNTHI
      parameter (MXRNTHI = 1)

! Number of multiple scattering step sizes
      integer MSSTEPS
      parameter (MSSTEPS = 16)

! Number of representative angles in vert1
      integer MXVRT1
      parameter (MXVRT1 = 1000)

! Distribution of  non-overlapping parts of vert
      integer MXVRT2
      parameter (MXVRT2 = 100)

! Electron small energy intervals
      integer MXSEKE
      parameter (MXSEKE = 1)

! Size of multiple scattering jreff map
      integer MXJREFF
      parameter (MXJREFF = 200)

! LSCAT parameter
      integer MXSCTFF
      parameter (MXSCTFF = 100)

! LSCAT parameter
      integer MXCP
      parameter (MXCP = 2000)

! LSCAT parameter
      integer MXNS
      parameter (MXNS = 200)

! Parameters used in EGS side GS MS Dist routines

! Maximum number of log-spaced energies per decade in scat power table
      integer NDEC
      parameter (NDEC = 32)

! Maximum total number of energy grid points in scat power table
      integer NESCPW
      parameter (NESCPW = 10 * NDEC + 1)

! Maximum number of energy grids in PW mscat table 
      integer NMSE
      parameter (NMSE = 5 * NDEC + 1)

! Number of equally probable angle bins in PW mscat table
      integer NBFIT
      parameter (NBFIT = 128)

! Number of additional angle bins, last bin in PW mscat table
      integer NEXFIT
      parameter (NEXFIT = 32)

! Number of bin endpoints, last bin in PW mscat table
      integer NEXFIT1
      parameter (NEXFIT1 = NEXFIT + 1)

! Total number of bns to be fitted in PW mscat table
      integer NFIT
      parameter (NFIT = NBFIT + NEXFIT - 1)

! Total number of bin endpoints in PW mscat table
      integer NFIT1
      parameter (NFIT1 = NFIT + 1)

!  Number of K1 values for each PW mscat bin
      integer NK1
      parameter (NK1 = 64)

!  Parameters used in PEGS GS MS Dist routines

      integer NEGRID        ! number of energy grid points in tabulated
      parameter (NEGRID=91) ! differential cross section data
     
      integer NEGRDS        ! number of energy grid points 
      parameter (NEGRDS=93) ! used for smoothing with splines
     
      integer NREDA         ! number of reduce angle grid points in 
      parameter (NREDA=606) ! tabulated differential cross section data

      integer NGT           ! number of terms in GS expansion
      parameter (NGT=15000)

!-------------------------last line of egs5_h.f-------------------------
