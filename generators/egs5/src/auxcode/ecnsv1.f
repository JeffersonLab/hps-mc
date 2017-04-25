!-------------------------------ecnsv1.f--------------------------------
! Version: 060130-1415
!          091105-0930  initialize gsum
!-----------------------------------------------------------------------
!23456789|123456789|123456789|123456789|123456789|123456789|123456789|12

! ----------------------------------------------------------------------
! Auxiliary subroutine for use with the EGS5 Code System
! ----------------------------------------------------------------------
! Subroutine for keeping track of energy conservation.  When ntree=0,
! the program is entered in order to initialize the esum array to zero.
! Otherwise, it is entered for totaling and outputing the results.  The
! esum array is needed in subroutine ausgab, where the energy  that is
! being deposited is added to the element of the array corresponding to
! the current value of iq, ir, and iarg.
! ----------------------------------------------------------------------
! Subroutine ecnsv1 should be called with:
!
!   ntree = 0 before the CALL SHOWER loop (to initialize)
!         = 1 after the CALL SHOWER loop (to normalize to totke)
!
! ----------------------------------------------------------------------

      subroutine ecnsv1(ntree,nreg,totke)

      implicit none

      include 'include/egs5_h.f'               ! Main EGS5 "header" file

      include 'auxcommons/etaly1.f'              ! Auxiliary-code COMMON

      real*8 totke                                           ! Arguments
      integer ntree,nreg

      real*8 rowsum(4,MXREG),                          ! Local variables
     * colsum(4,5),
     * sumsum(4),
     * gsum
      integer i,j,k

      if (nreg .gt. MXREG) then
        write(6,101) nreg,MXREG
 101    FORMAT(///,' ***** NOTE: STOPPED IN SUBROUTINE ECNSV1 ',/,
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
        sumsum(i) = 0.D0
          do j=1,nreg                                ! Step over regions
            rowsum(i,j) = 0.D0
            do k=1,5                             ! Step over iarg values
              esum(i,j,k) = 0.D0
              colsum(i,k) = 0.D0
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
!     Sum i=1,2,3 into i=4 of esum for (all regions and iarg-values)
!     --------------------------------------------------------------
      do j=1,nreg                                    ! Step over regions
        do k=1,5                                 ! Step over iarg values
          do i=1,3                            ! Step over particle types
            esum(4,j,k) = esum(4,j,k) + esum(i,j,k)
          end do
        end do
      end do

!     -----------------------
!     Normalize data to totke
!     -----------------------
      do i=1,4                ! Step over particle types (including ALL)
        do j=1,nreg                                  ! Step over regions
          do k=1,5                               ! Step over iarg values
            esum(i,j,k) = esum(i,j,k)/totke
          end do
        end do
      end do

!     ------------------------------------------------------
!     Sum-up and rows, columns, row-column sum and grand sum
!     ------------------------------------------------------
      do i=1,4                ! Step over particle types (including ALL)
        do j=1,nreg                                  ! Step over regions
          do k=1,5                               ! Step over iarg values
            rowsum(i,j) = rowsum(i,j) + esum(i,j,k)
            colsum(i,k) = colsum(i,k) + esum(i,j,k)
            sumsum(i) = sumsum(i) + esum(i,j,k)
            if (i .le. 3) gsum = gsum + esum(i,j,k)
          end do
        end do
      end do

!     ----------------------------------------------------------
!     Now write-out the results of the energy deposition summary
!     ----------------------------------------------------------
!     -------------------------------------------
!     Check if suppressed write-out was requested
!     -------------------------------------------
      if (ntree .eq. 2) then
        write(6,102) gsum
 102    FORMAT(//,' ECNSV1 output has been suppressed',//,
     *            ' TOTAL FRACTION=',G15.7,
     *            '     NOTE: THIS NUMBER SHOULD BE VERY CLOSE TO UNITY'
     *  )
                                                        ! --------------
        return                                          ! Return to MAIN
                                                        ! --------------
      end if

!     ---------------------------------------------
!     Otherwise perform normal (complete) write-out
!     ---------------------------------------------
      do i=1,4
        if (i .le. 3) then
          write(6,103) i-2
 103      FORMAT(//,' ENERGY DEPOSITION SUMMARY FOR PARTICLES WITH IQ=',
     *           I2,
     *           /, 55X,'IARG',/,19X,'0',15X,'1',13X,'2',14X,'3',14X,
     *           '4',16X,'ROW SUM', /,3X,'REGION',/)
        else
          write(6,104)
 104      FORMAT(//,' ENERGY DEPOSITION SUMMARY FOR ALL PARTICLES:',
     *           /, 55X,'IARG',/,19X,'0',15X,'1',13X,'2',14X,'3',14X,
     *           '4',16X,'ROW SUM', /,3X,'REGION',/)
        end if

        do j=1,nreg
          write(6,105) j,(esum(i,j,k),k=1,5),rowsum(i,j)
 105      FORMAT(I7,5X,5G15.7,5X,G15.7)
        end do

        write(6,106) (colsum(i,k),k=1,5),sumsum(i)
 106    FORMAT(/,3X,'COL SUM',2X,5G15.7,5X,G15.7)

      end do

      write(6,107) gsum
 107  FORMAT(//,' TOTAL FRACTION=',G15.7,
     *        '     NOTE: THIS NUMBER SHOULD BE VERY CLOSE TO UNITY',//)

                                                        ! --------------
      return                                            ! Return to MAIN
                                                        ! --------------
      end

!--------------------------last line of ecnsv1.f------------------------
