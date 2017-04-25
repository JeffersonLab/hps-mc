!-----------------------------egs5_eiicom.f-----------------------------
! Version: 051219-1435
!-----------------------------------------------------------------------
!23456789|123456789|123456789|123456789|123456789|123456789|123456789|12

      common/EIICOM/             ! Electron impact ionization parameters
     * eico0(MXMED),eico1(MXMED),
     * eii0(MXEKE,MXEPERMED,MXMED),eii1(MXEKE,MXEPERMED,MXMED),
     * feispl,ieispl,neispl

      real*8 eico0,eico1,eii0,eii1,feispl

      integer ieispl,neispl

!-----------------------last line of egs5_eiicom.f----------------------
