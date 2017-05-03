!-----------------------------g1e.f-------------------------------------
! Version: 060317-1630
!          060721-1500  Modify **-2 --> **(-2)
! Reference: SLAC-R-730/KEK-2005-8
!-----------------------------------------------------------------------
!23456789|123456789|123456789|123456789|123456789|123456789|123456789|12
!
!  This function gets the scattering power for electrons or positrons
!  in a media as a function of energy.  For kinetic energies less than
!  100 MeV, tables provided by Salvat and a log-log cubic spline 
!  are used.  At kinetic energies greater than 100 MeV, an analytic
!  intergal over the Moliere cross section is employed.

      double precision function g1e(iq,e,iflag)

      implicit none

      include 'include/egs5_h.f'

      include 'pegscommons/scpspl.f'
      include 'pegscommons/molvar.f'
      include 'pegscommons/pmcons.f'
      include 'pegscommons/dercon.f'

      integer iq,iflag
      double precision e

!  locals

      integer i
      double precision ke, xa2, onemb2, beta2, g1mol, logke

      ke = e - RM
      logke = dlog(ke*1.d6)

      if(logke.le.etl(negrds)) then
!  spline interpolation from Salvat PW data
        call findi(etl,logke,negrds,i)
        if(iq .eq. -1) then
          g1e = 
     +      dexp(ag1e(i)+logke*(bg1e(i)+logke*(cg1e(i)+logke*dg1e(i))))
        else
          g1e = 
     +      dexp(ag1p(i)+logke*(bg1p(i)+logke*(cg1p(i)+logke*dg1p(i))))
        endif
      else
!  from Moliere cross section
        onemb2 = (1.d0 + (ke/RM) )**(-2)
        beta2 = 1.d0 - onemb2
        xa2 = fsc*fsc * 1.13 /(.885*.885) * dexp(zx/zs) / dexp(ze/zs)
        xa2 = xa2 * onemb2 / beta2
        g1mol = dlog((pi*pi + xa2)/xa2) - pi*pi/(pi*pi + xa2)
        g1e = r0*r0 * zs * 2*pi * g1mol * onemb2 / (beta2*beta2)
      endif

!  multiply by atom density

      if(iflag.ne.-1) g1e = g1e * an * rho / wm

      return
      end
!-------------------------last line of g1e.f----------------------------
