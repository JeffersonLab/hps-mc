!---------------------------------k1e.f---------------------------------
! Version: 060316-1345
!-----------------------------------------------------------------------
!23456789|123456789|123456789|123456789|123456789|123456789|123456789|12

!  This function gets the initial scattering strength for electrons or 
!  positrons in a media as a function of energy.  

      double precision function k1e(iq,e)

      implicit none

      include 'include/egs5_h.f'

      include 'pegscommons/k1spl.f'
      include 'pegscommons/dercon.f'

      integer iq
      double precision e

!  locals

      integer i
      double precision ke

      ke = e - RM
      call findi(ehinge,ke,nhinge,i)
      if(iq .eq. -1) then
        k1e = ak1e(i)+ke*(bk1e(i)+ke*(ck1e(i)+ke*dk1e(i)))
      else
        k1e = ak1p(i)+ke*(bk1p(i)+ke*(ck1p(i)+ke*dk1p(i)))
      endif

      return
      end
!-------------------------last line of k1e.f----------------------------
