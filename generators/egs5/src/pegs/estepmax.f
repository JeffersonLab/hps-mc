!----------------------------estepmax.f---------------------------------
! Version: 060317-1045
!-----------------------------------------------------------------------
!23456789|123456789|123456789|123456789|123456789|123456789|123456789|12

!  returns the computed value for estepe to assure a 0.1% tolerance in
!  integrating G1 over an energy hinge

      double precision function estepmax(e)

      implicit none

      include 'include/egs5_h.f'

      include 'pegscommons/rngspl.f'
      include 'pegscommons/dercon.f'

      double precision e

!  locals

      integer i
      double precision ke

      ke = e - RM
      call findi(erng,ke,nrng,i)
      estepmax = aeste(i)+ke*(beste(i)+ke*(ceste(i)+ke*deste(i)))

      return
      end
!----------------------last line of estepmax.f--------------------------
