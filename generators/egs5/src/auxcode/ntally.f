!-------------------------------ntally.f--------------------------------
! Version: 060130-1415
!          091105-0930  initialize gsum
!-----------------------------------------------------------------------
!23456789|123456789|123456789|123456789|123456789|123456789|123456789|12

! ----------------------------------------------------------------------
! Auxiliary subroutine for use with the EGS5 Code System
! ----------------------------------------------------------------------
! Subroutine for tallying up the number of times edep-data has been
! entered into the ecnsv1 array.  The results can also be used to get
! a rough idea of whether certain (iq,ir,iarg) events are rare or not.
! Caution: Do not rely on these numbers for variance estimates.
! ----------------------------------------------------------------------

      subroutine ntally(ntree,nreg)

      implicit none

      include 'include/egs5_h.f'               ! Main EGS5 "header" file

      include 'auxcommons/ntaly1.f'              ! Auxiliary-code COMMON

      integer ntree,nreg                                     ! Arguments

      integer*8 rowsum(4,MXREG),                       ! Local variables
     * colsum(4,5),
     * sumsum(4),
     * gsum
      integer i,j,k

      if (nreg .gt. MXREG) then
        write(6,101) nreg,MXREG
 101    FORMAT(///,' ***** NOTE: STOPPED IN SUBROUTINE NTALLY ',/,
     *             'BECAUSE NREG= ', I5,' IS LARGER THAN MXREG= ',
     *         I5,' *****')
        stop
      end if

!     --------------------------------------------
!     Initialize various sums to zero (and return)
!     --------------------------------------------
      if (ntree .eq. 0) then
        gsum=0.D0
        do i=1,4              ! Step over particle types (including ALL)
        sumsum(i) = 0
          do j=1,nreg                                ! Step over regions
            rowsum(i,j) = 0
            do k=1,5                             ! Step over iarg values
              nsum(i,j,k) = 0
              colsum(i,k) = 0
            end do
          end do
        end do
                                                        ! --------------
        return                                          ! Return to MAIN
                                                        ! --------------
      end if

!     -----------------------------------------------
!     Reach this point when final tally is to be made
!     -----------------------------------------------
!     --------------------------------------------------------------
!     Sum i=1,2,3 into i=4 of nsum for (all regions and iarg-values)
!     --------------------------------------------------------------
      do j=1,nreg                                    ! Step over regions
        do k=1,5                                 ! Step over iarg values
          do i=1,3                            ! Step over particle types
            nsum(4,j,k) = nsum(4,j,k) + nsum(i,j,k)
          end do
        end do
      end do

!     ------------------------------------------------------
!     Sum-up and rows, columns, row-column sum and grand sum
!     ------------------------------------------------------
      do i=1,4                ! Step over particle types (including ALL)
        do j=1,nreg                                  ! Step over regions
          do k=1,5                               ! Step over iarg values
            rowsum(i,j) = rowsum(i,j) + nsum(i,j,k)
            colsum(i,k) = colsum(i,k) + nsum(i,j,k)
            sumsum(i) = sumsum(i) + nsum(i,j,k)
            if (i .le. 3) gsum = gsum + nsum(i,j,k)
          end do
        end do
      end do

!     ----------------------------------------------------
!     Now write-out the results of the event count summary
!     ----------------------------------------------------
!     -------------------------------------------
!     Check if suppressed write-out was requested
!     -------------------------------------------
                                                        ! --------------
      if (ntree .eq. 2) return                          ! Return to MAIN
                                                        ! --------------

!     ---------------------------------------------
!     Otherwise perform normal (complete) write-out
!     ---------------------------------------------
      do i=1,4
        if (i .le. 3) then
          write(6,103) i-2
 103      FORMAT(//,' SUMMARY OF EVENT COUNT FOR PARTICLES WITH IQ=',I2,
     *           /, 55X,'IARG',/,19X,'0',15X,'1',13X,'2',14X,'3',14X,
     *           '4',16X,'ROW SUM', /,3X,'REGION',/)
        else
          write(6,104)
 104      FORMAT(//,' SUMMARY OF EVENT COUNT FOR ALL PARTICLES:',
     *           /, 55X,'IARG',/,19X,'0',15X,'1',13X,'2',14X,'3',14X,
     *           '4',16X,'ROW SUM', /,3X,'REGION',/)
        end if

        do j=1,nreg
          write(6,105) j,(nsum(i,j,k),k=1,5),rowsum(i,j)
 105      FORMAT(I7,5X,5I15,5X,I15)
        end do

        write(6,106) (colsum(i,k),k=1,5),sumsum(i)
 106    FORMAT(/,3X,'COL SUM',2X,5I15,5X,I15)

      end do

      write(6,107) gsum
 107  FORMAT(//,' TOTAL NUMBER OF EVENTS=',I15)
                                                        ! --------------
      return                                            ! Return to MAIN
                                                        ! --------------
      end

!--------------------------last line of ntally.f------------------------
