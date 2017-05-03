!------------------------------egs5_eii.f-------------------------------
! Version: 070117-1210
!          080425-1100   Add time as the time after start.
! Reference: SLAC-R-730/KEK-2005-8
!-----------------------------------------------------------------------
!23456789|123456789|123456789|123456789|123456789|123456789|123456789|12

      subroutine eii

      implicit none

      include 'include/egs5_h.f'               ! Main EGS5 "header" file

      include 'include/egs5_edge.f'      ! COMMONs required by EGS5 code
      include 'include/egs5_epcont.f'
      include 'include/egs5_eiicom.f'
      include 'include/egs5_misc.f'
      include 'include/egs5_stack.f'
      include 'include/egs5_uphiot.f'
      include 'include/egs5_useful.f'

      include 'include/counters.f'       ! Additional (non-EGS5) COMMONs

      real*8 rnnow                                           ! Arguments
      integer iarg

      real*8 capbind,etmp,ese1,ese2,ratio,esesum       ! Local variables
      integer iqtmp,jnp,ieie

      ieii = ieii + 1                      ! Count entry into subroutine

      nxray = 0
      nauger = 0
      capbind = eedge(1,iz)*0.001
      ese1 = e(np-1)
      ese2 = e(np)
      esesum = e(np) + e(np-1) - 2.*RM
      if (esesum.lt.capbind) return   ! Added to fix bug 10Dec2012/YN,HH
      if (ese1 - RM .gt. capbind .or. ese2 - RM .gt. capbind) then
        call randomset(rnnow)
        if (rnnow .gt. 0.5) then
          e(np-1) = ese1 - capbind
          e(np) = ese2
        else
          e(np-1) = ese1
          e(np) = ese2 - capbind
        end if
        if (e(np-1) .le. RM .or. e(np) .le. RM) then
          if (rnnow .le. 0.5) then
            e(np-1) = ese1 - capbind
            e(np) = ese2
          else
            e(np-1) = ese1
            e(np) = ese2 - capbind
          end if
        end if
      else
        ratio = 1. - capbind/esesum
        e(np-1) = (ese1 - rm)*ratio + RM
        e(np) = (ese2 - rm)*ratio + RM
      end if

      if (ieispl .eq. 1) then
!       -------------------
!       Set feispl for user
!       -------------------
        if(feispl.eq.0.d0) then
          if(neispl.gt.0) then
            feispl = 1.d0/float(neispl)
          else
            write(6,105)
 105        FORMAT(' *** ERROR ***.  EII splitting requested but',
     *      ' number of splits .le. 0.  Stopping')
           stop
          endif
        endif

        if (neispl .gt. 1 .and. (np + neispl) .ge. MXSTACK) then
 1        continue
            write(6,100) MXSTACK,neispl,(2*neispl+1)/3
 100        FORMAT('0*** WARNING ***. STACK SIZE = ',I4,' MIGHT OVERFLOW
     *'/ '                 NEISPL BEING REDUCED, ',I4,'-->',I4/)                
            neispl = (2*neispl + 1)/3
            feispl = 1./float(neispl)
            if (neispl .eq. 1) then
              write(6,200) MXSTACK
 200          FORMAT('0*** WARNING ***. STACK SIZE = ',I4,' IS TOO SMALL
     *'/ '                 EII SPLITTING NOW SHUT OFF'/)                        
              ieispl = 0
            end if
            if (np + neispl .lt. MXSTACK) go to 2
          go to 1
 2        continue
        end if
      else
        neispl = 1
        feispl = 1.
      end if

!     ===========
      call kshell
!     ===========

      if (nxray .ge. 1 .and. exray(1) .gt. eedge(2,iz)*0.001) then
        enew = exray(1)
        ieie = 1
      else
        enew = 0.0
        ieie = 0
      end if
      edep = capbind - enew
      etmp = e(np)
      e(np)= edep
      iqtmp = iq(np)
      iq(np) = -1

      iarg=4

!                                =================
      if (iausfl(iarg+1) .ne. 0) call ausgab(iarg)
!                                =================
      e(np) = etmp
      iq(np) = iqtmp
      if (ieie .eq. 1) then
        np = np + 1
        e(np) = enew
        iq(np) = 0
        wt(np-1) = wt(np-1)*feispl
        do jnp=1,neispl
          iq(np-1+jnp) = iq(np)
          e(np-1+jnp) = e(np)
          call randomset(rnnow)
          costhe = 2.*rnnow - 1.
          sinthe = sqrt(1. - costhe*costhe)
          u(np) = 0.
          v(np) = 0.
          w(np) = 1.

          call uphi(2,1)

          u(np-1+jnp) = u(np)
          v(np-1+jnp) = v(np)
          w(np-1+jnp) = w(np)
          x(np-1+jnp) = x(np-1)
          y(np-1+jnp) = y(np-1)
          z(np-1+jnp) = z(np-1)
          ir(np-1+jnp) = ir(np-1)
          wt(np-1+jnp) = wt(np-1)
          time(np-1+jnp) = time(np-1)
          dnear(np-1+jnp) = dnear(np-1)
          latch(np-1+jnp) = latch(np-1)
          k1step(np-1+jnp) = 0.
          k1init(np-1+jnp) = 0.
          k1rsd(np-1+jnp) = 0.
        end do
        wt(np-1) = wt(np-1)/feispl
        np = np + neispl - 1
      end if
                                                      ! ----------------
      return                                          ! Return to MOLLER
                                                      ! ----------------
      end

!------------------------last line of egs5_eii.f------------------------
