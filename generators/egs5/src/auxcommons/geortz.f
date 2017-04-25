!--------------------------------geortz.f-------------------------------
! Version: 051219-1435
! Reference:
!-----------------------------------------------------------------------
!23456789|123456789|123456789|123456789|123456789|123456789|123456789|12
! ----------------------------------------------------------------------
! The indices (imax, jmax and kmax) are typically read in on UNIT=5 by
! subroutine getrtz.f, where they are checked against the array-size 
! parameters MXCYLS, MXTPLNS, MXZPLNS (defined in aux.f) to make sure
! that they are not larger.  
! The following are also checked in  getrtz.f:
!
!             MXREG  >= irmax  = imax*jmax*kmax + 1
!             MXPLNS >= iplmax = jmax + kmax + 1
!
! If any violations occur, getrtz.f will writeout error messages and the
! job will be terminated.
! ----------------------------------------------------------------------

      common/GEORTZ/                           ! RTZ-geometry parameters
     * imax,                          ! Index along R (radial direction)
     * jmax,                           ! Index along T (theta direction)
     * kmax,                               ! Index along Z (z direction)
     * ijmax                                         ! ijmax = imax*jmax

      integer imax,jmax,kmax,ijmax

!---------------------------last line of geortz.f-----------------------
