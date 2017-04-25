!-----------------------------egs5_annih.f------------------------------
! Version: 051219-1435
! Reference: SLAC-R-730/KEK-2005-8
!-----------------------------------------------------------------------
!23456789|123456789|123456789|123456789|123456789|123456789|123456789|12

      subroutine annih
      
      implicit none

      include 'include/egs5_h.f'               ! Main EGS5 "header" file

      include 'include/egs5_stack.f'     ! COMMONs required by EGS5 code
      include 'include/egs5_uphiot.f'
      include 'include/egs5_useful.f'

      include 'include/counters.f'       ! Additional (non-EGS5) COMMONs

      real*8 rnnow                                           ! Arguments

      real*8                                           ! Local variables
     * avip,                     ! Available energy of incident positron
     * esg1,                              ! Energy of secondary gamma #1
     * esg2,                              ! Energy of secondary gamma #2
     * a,ep0,ep,g,t,p,pot,rejf

      iannih = iannih + 1                  ! Count entry into subroutine
 
      avip = e(np) + RM
      a    = avip/RM    
      g    = a - 1.0
      t    = g - 1.0
      p    = sqrt(a*t)  
      pot  = p/t        
      ep0  = 1.0/(a + p)  

1     continue                    ! Sample 1/ep from ep = ep0 to 1 - ep0
        call randomset(rnnow)
        ep = ep0*exp(rnnow*log((1.0 - ep0)/ep0))

                                       ! Decide whether or not to accept
        call randomset(rnnow)
        rejf = 1.0 - ep + (2.0*g - 1.0/ep)/a**2
        if (rnnow .gt. rejf) go to 1

                 ! This completes sampling of a distribution which is
                 ! asymmetric about ep = 1/2, but which when symmetrized
                 ! is the symmetric annihilation distribution.

                                  ! Set up energies, place them on stack
      ep        = max(ep,1.D0 - ep)          ! Pick ep in (1/2, 1 - ep0)
      esg1      = avip*ep                 ! Energy of secondary gamma #1
      e(np)     = esg1           ! Place energy of gamma #1 on the stack
      esg2      = avip - esg1             ! Energy of secondary gamma #2
      e(np + 1) = esg2           ! Place energy of gamma #2 on the stack
      iq(np)    = 0                          ! Make particle np a photon

                             ! Set up angles for the higher energy gamma
      costhe = (esg1 - RM)*pot/esg1
      costhe = min(1.D0,costhe)                      ! Fix (860724/dwor)
      sinthe = sqrt((1.0 - costhe)*(1.0 + costhe))
      call uphi(2,1)                             ! Set direction cosines

                                             ! Set up lower energy gamma
      np     = np + 1                          ! Increase the stack size
      iq(np) = 0                                      ! Make it a photon
      costhe = (esg2 - RM)*pot/esg2
      costhe = min(1.D0,costhe)                      ! Fix (860724/dwor)
      sinthe = - sqrt((1.0 - costhe)*(1.0 + costhe))
      call uphi(3,2)                             ! Set direction cosines
                                                      ! ----------------
      return                                          ! Return to ELECTR
                                                      ! ----------------
      end
!-----------------------last line of egs5_annih.f-----------------------
