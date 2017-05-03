!-------------------------------swatch.f--------------------------------
! Version: 051219-1435
! Reference: SLAC version of SUBROUTINE WATCH (by W. R. Nelson)
!            (WATCH was written by D. W. O. Rogers (NRCC, Jan 1984))
!-----------------------------------------------------------------------
!23456789|123456789|123456789|123456789|123456789|123456789|123456789|12

! ----------------------------------------------------------------------
! Auxiliary subroutine for use with the EGS5 Code System
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
! SWATCH is the SLAC version of a code to print out particle-transport
! information.
!
!   IWATCH = 1   Information printed for EACH DISCRETE INTERACTION.
!   IWATCH = 2   Information printed for EACH STEP as well.
!
! The following calls should be made:
!
!   Before SHOWER loop:     IF(IWATCH>0) CALL SWATCH(-99,IWATCH)
!   Beginning of AUSGAB:    IF(IWATCH>0) CALL SWATCH(IARG,IWATCH)
!   After each CALL SHOWER: IF(IWATCH>0) CALL SWATCH(-1,IWATCH)
!   After SHOWER loop:      IF(IWATCH>0) CALL SWATCH(-88,IWATCH)
!
! with IWATCH passed to AUSGAB in COMMON/WATCH/IWATCH.  Note that
! OUTPUT is on UNIT=77 and the file that is produced is egs5job.out77 .
! ----------------------------------------------------------------------

      subroutine swatch(iarg,iwatch)

      implicit none

      include 'include/egs5_h.f'               ! Main EGS5 "header" file

      include 'include/egs5_epcont.f'    ! COMMONs required by EGS5 code
      include 'include/egs5_media.f'
      include 'include/egs5_misc.f'
      include 'include/egs5_stack.f'
      include 'include/egs5_useful.f'

      integer iarg,iwatch                                    ! Arguments

      integer ihstry,jr,n                              ! Local variables

      data ihstry/1/

!     ------------------------------------------
!     Initialize flags for extra calls to AUSGAB
!     ------------------------------------------
      if (iarg .eq. -99) then                       ! Before SHOWER loop
        open(UNIT=77,FILE='egs5job.out77',STATUS='unknown')
        do jr=1,25
          iausfl(jr) = 1
        end do

        iausfl(6) = 0
        iausfl(22) = 0
        iausfl(23) = 0
        iausfl(24) = 0

        write(77,101) iwatch
 101    FORMAT(' +---------------+',/, ' | SWATCH Output |',/,
     *         ' |  (IWATCH=',I1,')   |',/, ' +---------------+',/)

                                                        ! --------------
        return                                          ! Return to MAIN
                                                        ! --------------

      else if(iarg .eq. -88) then                    ! After SHOWER loop
        close(unit=77)
                                                        ! --------------
        return                                          ! Return to MAIN
                                                        ! --------------

      else if(iarg .eq. -1) then                ! After each CALL SHOWER
        write(77,102) ihstry
 102    FORMAT('*************** End-of-history',I3,' ***************',/)
        ihstry = ihstry + 1
                                                        ! --------------
        return                                          ! Return to MAIN
                                                        ! --------------
      end if

!     ---------------------------------
!     Calls made at beginning of AUSGAB
!     ---------------------------------
                                                      ! ----------------
      if (iarg .eq. 5 .or. iarg .lt. 0) return        ! Return to AUSGAB
                                                      ! ----------------
      if (iarg .eq. 0 .and. iwatch .eq. 2) then
        write(77,103)
 103    FORMAT(/,' Step about to occur (IARG=0):')
      else if (iarg .eq. 0) then
                                                      ! ----------------
        return                                        ! Return to AUSGAB
                                                      ! ----------------
      else if (iarg .eq. 1) then
        write(77,104)
 104    FORMAT(/,' Discard (IARG=1) -- AE<E<ECUT or AP<E<PCUT:')

      else if (iarg .eq. 2) then
        write(77,105)
 105    FORMAT(/,' Discard (IARG=2) -- E<AE or E<AP:')

      else if (iarg .eq. 3) then
        write(77,106)
 106    FORMAT(/,' Discard (IARG=3) -- user requested:')

      else if (iarg .eq. 4) then
        write(77,107)
 107    FORMAT(/,' Discard (IARG=4) -- EBINDA:')

      else if (iarg .eq. 6) then
        write(77,108)
 108    FORMAT(/,' Bremsstrahlung about to occur (IARG=6):')

      else if (iarg .eq. 7) then
        write(77,109)
 109    FORMAT(2X,' with resulting electron/photon (IARG=7):')

      else if (iarg .eq. 8) then
        write(77,110)
 110    FORMAT(/,' Moller about to occur (IARG=8):')

      else if (iarg .eq. 9) then
        write(77,111)
 111    FORMAT(2X,' with resulting electrons (IARG=9):')

      else if (iarg .eq. 10) then
        write(77,112)
 112    FORMAT(/,' Bhabha about to occur (IARG=10):')

      else if (iarg .eq. 11) then
        write(77,113)
 113    FORMAT(2X,' with resulting e- or e+ (IARG=11):')

      else if (iarg .eq. 12) then
        write(77,114)
 114    FORMAT(/,' Positron about to decay in flight (IARG=12):')

      else if (iarg .eq. 13) then
        write(77,115)
 115    FORMAT(2X,' with resulting photons (IARG=13):')

      else if (iarg .eq. 14) then
        write(77,116)
 116    FORMAT(/,' Positron annihilates at rest (IARG=14):')

      else if (iarg .eq. 15) then
        write(77,117)
 117    FORMAT(/,' Pair production about to occur (IARG=15):')

      else if (iarg .eq. 16) then
        write(77,118)
 118    FORMAT(2X,' with resulting pair (IARG=16):')

      else if (iarg .eq. 17) then
        write(77,119)
 119    FORMAT(/,' Compton about to occur (IARG=17):')

      else if (iarg .eq. 18) then
        write(77,120)
 120    FORMAT(2X,' with resulting photon/electron (IARG=18):')

      else if (iarg .eq. 19) then
        write(77,121)
 121    FORMAT(/,' Photoelectric about to occur (IARG=19):')

      else if (iarg .eq. 20) then
        write(77,122)
 122    FORMAT(2X,' with resulting electron (IARG=20):')

      else if (iarg .eq. 24) then
        write(77,123)
 123    FORMAT(/,' Rayleigh scattering occured (IARG=24):')
        write(77,124) iraylm(med(ir(np))),iraylr(ir(np))
 124    FORMAT(2X,' with IRAYLM=',I5,' and IRAYLR=',I5)
      end if

      eke = e(np)
      if (iq(np) .ne. 0) then
        eke = e(np) - RM
      end if

      write(77,125) np,iq(np),ir(np)
 125  FORMAT(' NP=',I3,7X,'IQ=',I3,3X,'IR=',I5)
      write(77,126) eke,wt(np)
 126  FORMAT(10X,'EKE/WT=',2G15.7)
      write(77,127) x(np),y(np),z(np)
 127  FORMAT(10X,' X/Y/Z=',3G15.7)
      write(77,128) u(np),v(np),w(np)
 128  FORMAT(10X,' U/V/W=',3G15.7)

      if (iarg .eq. 0 .and. iwatch .eq. 2) then
        write(77,129) ustep,tvstep,edep
 129    FORMAT(11X,'USTEP         TVSTEP         EDEP',
     *  /,9X,3G15.7)
      end if
                                                      ! ----------------
      if (np.eq.1 .or. iarg.eq.0) return              ! Return to AUSGAB
                                                      ! ----------------
      if (iarg .eq. 7 .or. iarg .eq. 9 .or.
     *    iarg .eq. 11 .or. iarg .eq. 13 .or.
     *    iarg .eq. 14 .or. iarg .eq. 16 .or.
     *    iarg .eq. 18 .or. iarg .le. 3) then

        n = np - 1
        eke = e(n)
        if (iq(n) .ne. 0) then
          eke = e(n) - RM
        end if
        if (iarg .le. 3) then
          write(77,130)
 130      FORMAT(' Now on top of stack:')
        else
          write(77,131)
 131      FORMAT('                    :')
        end if

        write(77,132) n,iq(n),ir(n)
 132    FORMAT(' NP=',I3,3X,'IQ=',I3,3X,'IR=',I5)
        write(77,133)eke,wt(n)
 133    FORMAT(10X,'EKE/WT=',2G15.7)
        write(77,134) x(n),y(n),z(n)
 134    FORMAT(10X,' X/Y/Z=',3G15.7)
        write(77,135) u(n),v(n),w(n)
 135    FORMAT(10X,' U/V/W=',3G15.7)

      end if
                                                      ! ----------------
      return                                          ! Return to AUSGAB
                                                      ! ----------------
      end

!--------------------------last line of swatch.f------------------------
