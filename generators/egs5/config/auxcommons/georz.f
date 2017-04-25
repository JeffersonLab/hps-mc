!--------------------------------georz.f--------------------------------
! Version: 051219-1435
! Reference:
!-----------------------------------------------------------------------
!23456789|123456789|123456789|123456789|123456789|123456789|123456789|12
! ----------------------------------------------------------------------
! The indices ncyl (number of cylinder) and nplan (number of plane) are
! read from input file (copyed to egs5job.data) where they are checked 
! against the array-size parameters MXCYLS, MXTPLNS (defined in aux.f) 
! to make sure that they are not larger.  
! The following are also checked in  getrtz.f:
!
!             MXCYLS >= ncyl
!             MXREG  >= (nplan-1)*ncyl+3
!             MXPLNS >= nplan
!
! ----------------------------------------------------------------------

      common/GEORZ/                           ! RZ-geometry parameters
     * ncyl,                                  ! Number of cylinder
     * nplan,                                 ! Number of plane
     * irz                                    ! nreg - 3
      integer ncyl,nplan,irz

!---------------------------last line of georz.f-----------------------
