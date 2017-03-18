!--------------------------------geoxyz.f-------------------------------
! Version: 051219-1435
! Reference:
!-----------------------------------------------------------------------
!23456789|123456789|123456789|123456789|123456789|123456789|123456789|12
! ----------------------------------------------------------------------
! The indices (imax, jmax and kmax) are typically read in on UNIT=5 by
! subroutine getxyz.f, where they are checked against the array-size 
! parameters MXXPLNS, MXYPLNS, MXZPLNS (defined in aux.f) to make sure
! that they are not larger.  The following are also checked in getxyz.f.
!
!             MXREG  >= irmax  = imax*jmax*kmax + 1
!             MXPLNS >= iplmax = imax + jmax + kmax + 3
!
! If any violations occur, getrtz.f will writeout error messages and the
! job will be terminates.
! ----------------------------------------------------------------------

      common/GEOXYZ/                           ! XYZ-geometry parameters
     * xbound(MXXPLNS+1),                       ! x-boundary coordinates
     * ybound(MXYPLNS+1),                       ! y-boundary coordinates
     * zbound(MXZPLNS+1),                       ! z-boundary coordinates
     * xinl,xinu,xindel,
     * yinl,yinu,yindel,
     * imax,                                             ! Index along x
     * jmax,                                             ! Index along y
     * kmax,                                             ! Index along z
     * ijmax,                                        ! ijmax = imax*jmax
     * irmax,                               ! irmax = imax*jmax*kmax + 1
     * ixinl,jyinl

      real*8 
     * xbound,ybound,zbound,
     * xinl,xinu,xindel,
     * yinl,yinu,yindel
      integer imax,jmax,kmax,ijmax,irmax,ixinl,jyinl

!---------------------------last line of geoxyz.f-----------------------
