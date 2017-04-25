!-------------------------------randomm.f-------------------------------
! Version: 051219-1435
!          060721-1500  Modify **-48 --> **(-48)
! Reference: RANLUX, after James,
!            Computer Phys. Commun. 79 (1994) 111-114.
!-----------------------------------------------------------------------
!23456789|123456789|123456789|123456789|123456789|123456789|123456789|12

!  random number seed commons and parameters

      real twom48
      parameter (twom48=2.**(-48))
      integer maxlev, igiga
      parameter (maxlev=4, igiga=1000000000)

      common/RLUXCOM/
     & seeds,
     & carry,
     & next,
     & i24,
     & j24,
     & in24

      real seeds(24), carry
      integer next(24), i24, j24, in24

      common/RLUXDAT/
     & twom12,
     & twom24,
     & ndskip,
     & luxlev,
     & nskip,
     & inseed,
     & kount,
     & mkount,
     & isdext,
     & rluxset
     
      real twom12, twom24
      integer luxlev, nskip, inseed, kount, mkount, isdext(25) 
      integer ndskip(0:maxlev)
      logical rluxset

!--------------------------last line of randomm.f-----------------------
