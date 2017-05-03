!----------------------------egs5_epcont.f------------------------------
! Version: 051219-1435
!          091105-0835      tvstep/ustep equivalence added
!-----------------------------------------------------------------------
!23456789|123456789|123456789|123456789|123456789|123456789|123456789|12

      common/EPCONT/                 ! Electron-photon control variables
     * edep,                                          ! Energy deposited
     * tstep,                             ! Distance to next interaction
     * ustep,        ! User (straight line) step requested (and granted)
     * tmstep,                ! Total multiple scattering hinge distance
     * rhof,                      ! Value density correction (default=1)
     * eold,          ! Charged particle (total) energy at start of step
     * enew,            ! Charged particle (total) energy at end of step
     * eke,                         ! Kinetic energy of charged particle
     * elke,                                  ! Natural logarithm of EKE
     * beta2,           ! Beta (v/c) squared of present charged particle
     * gle,                         ! Natural logarithm of photon energy
     * idisc,        ! User-discard flag: 0=no, >0=immediately, <0=later
     * irold,                                 ! Index of previous region
     * irnew,                                     ! Index of new regions
     * iausfl(MXAUS)              ! Flags for turning on calls to AUSGAB

      real*8 edep,tstep,ustep,tmstep,rhof,eold,enew,eke,elke,beta2,gle
      integer idisc,irold,irnew,iausfl

      ! local variable, created for legacy 
      real*8 tvstep              ! Electron straight-line step distance
      equivalence (tvstep, ustep)

!------------------------last line of egs5_epcont.f---------------------
