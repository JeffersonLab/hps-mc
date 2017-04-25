!-----------------------------wmsfit.f----------------------------------
! Version: 060313-1255
!-----------------------------------------------------------------------
!23456789|123456789|123456789|123456789|123456789|123456789|123456789|12

      subroutine wmsfit(k1start,dk1)

      implicit none

      double precision k1start(2), dk1(2)
C
      include 'include/egs5_h.f'
      include 'include/egs5_cdcsep.f'
      include 'include/egs5_mscon.f'

      include 'pegscommons/mxdatc.f'

      integer ipart, iener, iang, ik1

      write(17,5001) medium
5001  format(' MEDIUM=',24A1)
      write(17,*) 'MS fitting coefficients for this media'
      write(17,*) 'Total number of energy steps:'
      write(17,*) nmscate
      write(17,*) 'First energy decade:'
      write(17,*) decade1
      write(17,*) 'Number of energy steps inside each decade:'
      write(17,*) NDEC
      write(17,*) 'Number of energy steps to skip in first decade:'
      write(17,*) joffset
      write(17,*) 'Number of equally probably angle bins'
      write(17,*) NBFIT
      write(17,*) 'Number of equally spaced angle bins'
      write(17,*) NEXFIT

!  ***  Loop over the two particle types
      do ipart = 1,2
        if(ipart.eq.1) then
          write(17,*) 'Electrons'
        else
          write(17,*) 'Positrons'
        endif

!  ***  Loop over the energy steps
        do iener = 1,nmscate
          write(17,*) mscate(iener)*1.e-6, ' MeV => ladder energy'

!  ***  Loop over the scattering strength intervals
          do ik1 = 1,NK1
          write(17,*) 'Fits at K1 = ',
     &                 k1start(ipart) * dk1(ipart) ** (ik1-1)
          write(17,*) probns(ipart,iener,ik1), ' => No Scat Prob'
          write(17,*) '    amu     -       amu         b        eta'
!  ***  Loop over the angle intervals
            do iang = 1,NFIT
              write(17,'(1x,4(1pe12.5,1x))') 
     1          amums(ipart,iener,ik1,iang), 
     1          amums(ipart,iener,ik1,iang+1),
     1          bms(ipart,iener,ik1,iang),
     1          etams(ipart,iener,ik1,iang)
            end do

!  ***  loop over the equally spaced angles and print the cdf
            write(17,*) 'CDF for the current region'
            write(17,*) ' 0.0000000E+00'
            do iang = 1,NEXFIT
              write(17,'(1x,1pe14.7,1x)') cumdist(ipart,iener,ik1,iang)
            end do
          end do               !-->  K1 steps
        end do                 !-->  Energy grid
      end do                   !-->  Particle types

      return
      end
!-------------------------last line of wmsfit.f-------------------------
