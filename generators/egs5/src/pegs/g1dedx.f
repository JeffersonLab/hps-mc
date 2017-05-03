!-----------------------------g1ededx.f---------------------------------
! Version: 060306-1000
! Reference: SLAC-R-730/KEK-2005-8
!-----------------------------------------------------------------------
!23456789|123456789|123456789|123456789|123456789|123456789|123456789|12
!
!   functions which returns the product of 1/dedx and g1, 1st transport
!   cross section for electrons
!
      double precision function g1ededx(e)
!
      implicit none

      include 'pegscommons/thres2.f'
      include 'pegscommons/molvar.f'

!  globals
      real*8 e, sptote, g1e

      g1ededx = g1e(-1,e,0) * (rlc / sptote(e,e,e))
      
      return
      end
!-------------------------last line of g1ededx.f------------------------

!-----------------------------g1pdedx.f---------------------------------
! Version: 051219-1435
! Reference: SLAC-R-730/KEK-2005-8
!-----------------------------------------------------------------------
!23456789|123456789|123456789|123456789|123456789|123456789|123456789|12
!
!   functions which returns the product of 1/dedx and g1, 1st transport
!   cross section for positrons
!
      double precision function g1pdedx(e)
!
      implicit none

      include 'pegscommons/thres2.f'
      include 'pegscommons/molvar.f'

!  globals
      real*8 e, sptotp, g1e

      g1pdedx = g1e(+1,e,0) * (rlc / sptotp(e,e,e))
      
      return
      end
!-------------------------last line of g1pdedx.f------------------------
