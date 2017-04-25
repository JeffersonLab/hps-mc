!-----------------------------egs5_uservr.f-----------------------------
! Version: 051219-1435
! Reference:  Note, only cexptr is active in this release
!-----------------------------------------------------------------------
!23456789|123456789|123456789|123456789|123456789|123456789|123456789|12

      common/USERVR/                           ! User-Variance-Reduction
     * cexptr,    ! c variable for exponential pathlength transformation
     * gwait,          ! Weight adjustment in forcing interactions macro
     * iforce,      ! Only force photon interactions if this is non-zero
     * nfmin,      ! Following three used with forced interactions macro
     * nfmax,
     * nftime, 
     * isourc,
     * ifpb,                                ! Flags if isourc = 0,2 or 4
     * iqinc,                                          ! Incident charge
     * monoen      ! Flag (0 if monoenergetic, 1 if source distribution)

      real*8 cexptr,gwait
      integer iforce,nfmin,nfmax,nftime,isourc,ifpb,iqinc,monoen

!-----------------------last line of egs5_uservr.f----------------------
