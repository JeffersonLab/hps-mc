!-----------------------------egs5_media.f------------------------------
! Version: 051219-1435
!-----------------------------------------------------------------------
!23456789|123456789|123456789|123456789|123456789|123456789|123456789|12

      common/MEDIA/      ! Names and data for media currently being used
     * rlcm(MXMED),
     * rldu(MXMED),
     * rhom(MXMED),
     * charD(MXMED),
     * iraylm(MXMED),
     * incohm(MXMED),
     * iprofm(MXMED),
     * impacm(MXMED),
     * useGSD(MXMED),
     * media(24,MXMED),
     * nmed

      real*8 rlcm,rldu,rhom,charD
      integer iraylm,incohm,iprofm,impacm,useGSD,nmed
      character*4 media

!------------------------last line of egs5_media.f----------------------
