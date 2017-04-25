!----------------------------esteplim.f---------------------------------
! Version: 060318-1800
! Reference: SLAC-R-730/KEK-2005-8
!-----------------------------------------------------------------------
!23456789|123456789|123456789|123456789|123456789|123456789|123456789|12
!
!  get energy dependent values of estepe, the fractional energy loss
!  which assures a tolerance of "etol" in the total energy loss, the
!  scattering power, the total hard scattering probability, and the 
!  hard collision mean free path
!
!  (loops over hard collision variables not yet implemented)
!
      subroutine esteplim

!  globals

      implicit none

      real*8 g1e, sptote, sptotp, csdar

      include 'include/egs5_h.f'
      include 'include/egs5_mscon.f'

      include 'pegscommons/dercon.f'
      include 'pegscommons/molvar.f'
      include 'pegscommons/rngspl.f'
      include 'pegscommons/thres2.f'
      
!  locals

      integer i, ivar
      real*8 e1, e2, e1t, e2t, etol, fspe(NESCPW), rnge(NESCPW),
     *       aspe(NESCPW), bspe(NESCPW), cspe(NESCPW), dspe(NESCPW), 
     *       deltaE, trap, anal, dloge
      data etol/1.d-3/

      !  load the energy array 
      nrng = NESCPW
      erng(1) = log(mscate(1)*1.d-6)
      erng(nrng) = log(mscate(nmscpw)*1.d-6)
      dloge = (erng(nrng) - erng(1)) / (nrng - 1)
      do i = 2, nrng-1
        erng(i) = erng(1) + (i-1) * dloge
      end do

!  do 5 loops:
!  1.)  load electron dedx total and use that to get a range table
!  2.)  load positron dedx total and use that to get a range table
!  3.)  load dedx restricted and get the first limit
!  4.)  load the scattering power, and use that to get limit 2
!  5.)  do the mfp and total scattering limit together (not implemented)

      do ivar = 1,4
        do i=1,nrng
          if (ivar.eq.1) then
            erng(i) = exp(erng(i))
            estepl(i) = .50
          end if 
          e1t = erng(i) + RM

          if (ivar.eq.1) then
            fspe(i) = rlc / sptote(e1t,ae,ap)
          else if (ivar.eq.2) then
            fspe(i) = rlc / sptotp(e1t,ae,ap)
          else if (ivar.eq.4) then
            fspe(i) = g1e(-1,e1t,0) * (rlc / sptote(e1t,ae,ap))
          else if (ivar.eq.5) then
            fspe(i) = 1.d0
          end if
        end do

        !  spline the function for local use

        if (ivar.ne.3) then
          call spline(erng,fspe,aspe,bspe,cspe,dspe,0.d0,0.d0,nrng)
        end if

        !  get the electron or positron CSDA range
        if (ivar.le.2) then
          e1 = erng(1)
          if (ivar.eq.1) then
            rnge(1) = rlc / sptote(e1t,ae,ap) * e1
          else
            rnge(1) = rlc / sptotp(e1t,ae,ap) * e1
          end if
          do i = 2, nrng
            e2 = erng(i)
            call integ(erng,aspe,bspe,cspe,dspe,e1,e2,anal,nrng)
            rnge(i) = rnge(i-1) + dabs(anal)
            e1 = e2
          end do
          !  get global splines for use later is csdar
          if (ivar.eq.1) then
            call spline(erng,rnge,arnge,brnge,crnge,drnge,
     *                                         0.d0,0.d0,nrng)
          else
            call spline(erng,rnge,arngp,brngp,crngp,drngp,
     *                                         0.d0,0.d0,nrng)
          end if

        !  do the energy limit 
        else 
          !  start at 4 because of spline issues at ends
          do i = 4, nrng-3
            e1 = erng(i)
            !  trap possible glitches around 100 MeV
            if (ivar.eq.4 .and. e1.gt.60.d0 .and. e1.lt.140.d0) then
              if (i.gt.4) then 
                estepl(i) = estepl(i-1)
                go to 200
              end if
            end if
            e2 = e1 * (1.d0 - estepl(i))
            if (e2.lt.erng(1)) then
              e2 = erng(1)
              estepl(i) = 1.d0 - e2/e1
            endif
               
            e1t = e1 + RM
 100        e2t = e2 + RM
            if (ivar.le.4) then
              deltaE = (e1 - e2)
              if (ivar.eq.3) then
                trap = rlc / sptote(e1t,ae,ap) + rlc / sptote(e2t,ae,ap)
                anal = csdar(-1,e1t) - csdar(-1,e2t)
              else 
                trap = rlc * g1e(-1,e1t,0) / sptote(e1t,ae,ap)
                trap = trap + rlc * g1e(-1,e2t,0) / sptote(e2t,ae,ap)
                call integ(erng,aspe,bspe,cspe,dspe,e2,e1,anal,nrng)
              end if
              trap = deltaE * trap / 2.d0
              !  this should never happen now except in development
              if (anal.eq.0.d0) then
                write(6,*) 'ERROR in esteplim -- integral = 0.d0'
                stop
              ! if we're within the tolerance, get next energy
              else if (dabs((trap - anal) / anal) .le. etol) then
                ! convergence -- ignore the discreteness from using 5% 
                go to 200
              else
              ! else, store previous, and try something shorter
                e2 = e1 * (1.d0 - estepl(i)*.95)
                estepl(i) = 1.d0 - e2/e1
                !  trap numerical problems -- estepe is smooth
                if(i.gt.4 .and. estepl(i).lt.estepl(i-1)/2.d0) go to 200
                go to 100
              endif
            else
          !  do the mfp limit 
            end if 
 200        continue
          end do
          !  skip regions where splines may be bad
          do i = 1, 3
            estepl(i) = estepl(4)
            estepl(nrng-3+i) = estepl(nrng-3)
          end do
        end if 

      end do
           
      ! get splines for use in pegs
      call spline(erng,estepl,aeste,beste,ceste,deste,0.d0,0.d0,nrng)
      
      return
      end
!-----------------------last line of esteplim.f-------------------------
