!-------------------------------csdar.f---------------------------------
! Version: 060316-1345
!-----------------------------------------------------------------------
!23456789|123456789|123456789|123456789|123456789|123456789|123456789|12

!  returns the csda range for an electron or positron

      double precision function csdar(iq,e)

      implicit none

      include 'include/egs5_h.f'

      include 'pegscommons/rngspl.f'
      include 'pegscommons/dercon.f'

      integer iq
      double precision e

!  locals

      integer i
      double precision ke

      ke = e - RM
      call findi(erng,ke,nrng,i)
      if(iq .eq. -1) then
       csdar = arnge(i)+ke*(brnge(i)+ke*(crnge(i)+ke*drnge(i)))
      else
       csdar = arngp(i)+ke*(brngp(i)+ke*(crngp(i)+ke*drngp(i)))
      endif

      return
      end
!------------------------last line of csdar.f---------------------------
