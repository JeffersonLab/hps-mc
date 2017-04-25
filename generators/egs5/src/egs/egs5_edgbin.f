!-----------------------------egs5_edgbin.f-----------------------------
! Version: 051219-1435
! Reference: SLAC-R-730/KEK-2005-8
!-----------------------------------------------------------------------
!23456789|123456789|123456789|123456789|123456789|123456789|123456789|12

      subroutine edgbin

      implicit none

      include 'include/egs5_h.f'               ! Main EGS5 "header" file

      include 'include/egs5_edge.f'      ! COMMONs required by EGS5 code
      include 'include/egs5_brempr.f'
      include 'include/egs5_epcont.f'
      include 'include/egs5_media.f'
      include 'include/egs5_misc.f'
      include 'include/egs5_photin.f'
      include 'include/egs5_stack.f'
      include 'include/egs5_thresh.f'
      include 'include/egs5_uphiot.f'
      include 'include/egs5_useful.f'

      include 'include/counters.f'       ! Additional (non-EGS5) COMMONs

      real*8 eig,eee                                   ! Local variables
      integer ii,jj,izn,ner,iz1,ikl

      iedgbin = iedgbin + 1                ! Count entry into subroutine

      do medium=1,nmed
        ner = nepm(medium)

        if (ner .gt. 20) then
          write(6,101)
 101      FORMAT(' Number of elements in medium must be less than 20 !')
          stop
        end if

        nedgb(medium) = 0
        do izn=1,ner
          iz1 = zelem(medium,izn)
          do ikl=1,4
            eee = eedge(ikl,iz1)/1000.0
            if (eee .gt. ap(medium)) then
              nedgb(medium) = nedgb(medium) + 1
              eig = log(eee)
              ledgb(nedgb(medium),medium) = ge1(medium)*eig +
     *                                      ge0(medium)
              edgb(nedgb(medium),medium) = eee
            end if
          end do
        end do

        if (nedgb(medium) .gt. 0) then
          do ii=1,nedgb(medium)
            do jj=1,nedgb(medium)
              if (ii .ne. jj) then
                if (ledgb(ii,medium) .eq. ledgb(jj,medium)) then
                  write(6,102)medium
 102              FORMAT(' K- or L-edge exists in the same fitting bin a
     *t MEDIUM=',I2,'!'/ ' It is better to produce material having a sma
     *ll UE.')
                end if
              end if
            end do
          end do
        end if
      end do
!                                                      ! ---------------
      return                                           ! Return to HATCH
!                                                      ! ---------------
      end

!-----------------------last line of egs5_edgbin.f----------------------
