!-------------------------------egs5_hardx.f----------------------------
! Version: 090303-1415
! Get hard collision cross section for electr
!-----------------------------------------------------------------------
!23456789|123456789|123456789|123456789|123456789|123456789|123456789|12

      subroutine hardx(charge,kEnergy,keIndex,keFraction,sig0)

      implicit none

      integer charge
      integer keIndex
      double precision kEnergy
      double precision keFraction

      include 'include/egs5_h.f'               ! Main EGS5 "header" file

      include 'include/egs5_edge.f'      ! COMMONs required by EGS5 code
      include 'include/egs5_elecin.f'
      include 'include/egs5_thresh.f'
      include 'include/egs5_useful.f'

      include 'include/counters.f'       ! Additional (non-EGS5) COMMONs

      double precision sig0

      integer mollerIndex
      double precision mollerThresh
      double precision logMollerThresh

      ihardx = ihardx + 1                  ! Count entry into subroutine

      if (charge .lt. 0) then                                  ! e-
        iextp = 0
        !  correction for Moller threshold ?
        if (kEnergy .lt. (thmoll(medium)-RM)*1.5) then
          mollerThresh = thmoll(medium) - RM
          logMollerThresh = log(mollerThresh)
          mollerIndex = eke1(medium)*logMollerThresh + eke0(medium)
          if (mollerIndex .eq. keIndex) then
            if (thmoll(medium)-RM .le. kEnergy) then
              iextp = 1
            else
              iextp = -1
            end if
          end if
        end if
        sig0 = esig1(keIndex+iextp,medium)*keFraction + 
     &                              esig0(keIndex+iextp,medium)

       else                                                    ! e+
         sig0 = psig1(keIndex,medium)*keFraction + 
     &                                    psig0(keIndex,medium)
       end if
      if(sig0.le.0.0)sig0=1.e-10
 
       return
       end

!-----------------------last line of egs5_hardx.f-----------------------
