!-----------------------------egs5_rk1.f--------------------------------
! Version: 060313-0945
! Read the input data tables of K1 vs E, charD and Z, and construct
! PWL of K1 vs. E for all materials in this problem
!-----------------------------------------------------------------------
!23456789|123456789|123456789|123456789|123456789|123456789|123456789|12

      subroutine rk1

      implicit none

      include 'include/egs5_h.f'         ! COMMONs required by EGSD5 code
      include 'include/egs5_useful.f'
      include 'include/egs5_brempr.f'
      include 'include/egs5_thresh.f'
      include 'include/egs5_elecin.f'
      include 'include/egs5_media.f'
      include 'include/egs5_misc.f'
      include 'include/egs5_scpw.f'
      include 'include/egs5_mscon.f'
      
      real*8                                           ! Local variables
     * ek0k1(50,20),
     * slopek1(50,20), bk1(50,20), 
     * dlowk1(50,20), dhighk1(50,20),
     * k1low(50,20), k1high(50,20),
     * z2k1w(20), rhok1(20)

      integer nmatk1,nek1(20)

      real*8
     * lcharD, eke, elke, delke, z2w, z, watot, 
     * zfrac, frace, k1ez(2),
     * k1z(2), k1new, k1old, elkeold, scpe, scpp, scprat, 
     * c1, c2, extrape, scpeMax(2),
     * k1sold, k1s, k1sz(2), k1sez(2)

      integer nz2, idx, iek1, iedx, niek, neke, iz2
      integer i,im,ie,j,n, m

      open(UNIT=17,FILE='data/K1.dat',STATUS='old')

!  Read the K1 data file, and compute the interpolated values 
!  of K1 corresponding to the characteristic dimension at
!  each energy for each material

      read(17,*) nmatk1
      do i = 1, nmatk1
        read(17,*) z, z2k1w(i), watot, rhok1(i)
        z2k1w(i) = z2k1w(i) / watot
        read(17,*) nek1(i)
        do j = nek1(i), 1, -1
          read(17,*) ek0k1(j,i), slopek1(j,i), bk1(j,i), 
     *              dlowk1(j,i),k1low(j,i),dhighk1(j,i),k1high(j,i)
          ! prep for interp in D * rho
          bk1(j,i) = bk1(j,i) - slopek1(j,i) * DLOG(rhok1(i))
          dhighk1(j,i) = dhighk1(j,i) * rhok1(i)
          dlowk1(j,i) = dlowk1(j,i) * rhok1(i)
        end do
      end do

      close(17)

!  Get Z^2 and then loop over energies and do two-way 
!  interpolation of charD rho in Z^2 and E

      do 10 im = 1, nmed

        if(charD(im).eq.0.d0) go to 10
        lcharD = dlog(charD(im)*rhom(im))

        watot = 0.d0
        z2w = 0.d0
        do ie = 1, nne(im)
          z2w = z2w + pz(im,ie) * zelem(im,ie) * (zelem(im,ie) + 1.d0)
          watot = watot + pz(im,ie) * wa(im,ie)
        end do
        z2w = z2w / watot

        if(z2w.gt.z2k1w(nmatk1)) then
          nz2 = 1
          iz2 = nmatk1
          zfrac = 0.d0
        else if(z2w.lt.z2k1w(1)) then
          nz2 = 1
          iz2 = 1
          zfrac = 0.d0
        else 
          nz2 = 2
          call findi(z2k1w,z2w,nmatk1,iz2)
          zfrac = (z2w - z2k1w(iz2)) / (z2k1w(iz2+1) - z2k1w(iz2))
        end if

        !-->  get scpow at max E of the reference materials,
        !-->  in case we have to extrapolate
        do  n = 1, nz2
          idx = iz2 + n - 1
          eke = ek0k1(nek1(idx),idx)
          if(ue(im)-RM.gt.eke) then
            elke = log(eke)
            j = eke0(im) + elke * eke1(im)
            scpeMax(n) = escpw1(j,im)*elke + escpw0(j,im)
          else
            scpeMax(n) = 0.0
          end if
        end do

        neke = meke(im)
        do j = 1,neke-1
          if(j.eq.1) then
            eke = ae(im) - RM
            elke = log(eke)
          else if(j.eq.neke-1) then
            eke = ue(im) - RM
            elke = log(eke)
          else
            elke = (j + 1 - eke0(im)) / eke1(im)
            eke = DEXP(elke)
          end if

          scpe = escpw1(j,im)*elke + escpw0(j,im)
          scpp = pscpw1(j,im)*elke + pscpw0(j,im)
          scprat = scpp/scpe
          do n = 1, nz2
            extrape = 1.d0
            idx = iz2 + n - 1
            if(eke.gt.ek0k1(nek1(idx),idx)) then
              niek = 1
              iek1 = nek1(idx)
              frace = 0.d0
              extrape = scpe/scpeMax(n)
            else if(eke.lt.ek0k1(1,idx)) then
              niek = 1
              iek1 = 1
              frace = 0.d0
            else 
              niek = 2
              call findi(ek0k1(1,idx),eke,nek1(idx),iek1)
              frace = (eke - ek0k1(iek1,idx)) / 
     *                    (ek0k1(iek1+1,idx) - ek0k1(iek1,idx))
            end if
        
            do m = 1, niek
              iedx = iek1 + m - 1
              if(charD(im)*rhom(im).gt.dhighk1(iedx,idx)) then
                k1ez(m) = k1high(iedx,idx)
              else if(charD(im)*rhom(im).lt.dlowk1(iedx,idx)) then
                k1ez(m) = k1low(iedx,idx)
              else
                k1ez(m) = DEXP(slopek1(iedx,idx)*lcharD + bk1(iedx,idx))
              end if
              k1sez(m) =  k1low(iedx,idx)
            end do
            k1z(n) = k1ez(1) + frace * (k1ez(2) - k1ez(1))
            k1z(n) = extrape * k1z(n)
            k1sz(n) = k1sez(1) + frace * (k1sez(2) - k1sez(1))
            k1sz(n) = extrape * k1sz(n)
          end do
          k1new = k1z(1) + zfrac * (k1z(2) - k1z(1))
          k1s = k1sz(1) + zfrac * (k1sz(2) - k1sz(1))

          if(k1new.gt.k1maxe) then
            k1maxe = k1new
          else if(k1new.lt.k1mine) then
            k1mine = k1new
          end if
          if(k1new*scprat.gt.k1maxp) then
            k1maxp = k1new*scprat
          else if(k1new*scprat.lt.k1minp) then
            k1minp = k1new*scprat
          end if
          if(k1s.lt.k1mine) then
            k1mine = k1s
          end if
          if(k1s*scprat.lt.k1minp) then
            k1minp = k1s*scprat
          end if

          if(j.gt.1) then
            delke = elke - elkeold
            ekini1(j,im) =  (k1new - k1old) / delke
            ekini0(j,im) = (k1old * elke - k1new * elkeold) / delke
            pkini1(j,im) = ekini1(j,im) * scprat
            pkini0(j,im) = ekini0(j,im) * scprat
            ek1s1(j,im) =  (k1s - k1sold) / delke
            ek1s0(j,im) = (k1sold * elke - k1s * elkeold) / delke
            pk1s1(j,im) = ek1s1(j,im) * scprat
            pk1s0(j,im) = ek1s0(j,im) * scprat
          end if
          elkeold = elke
          k1old = k1new
          k1sold = k1s
        end do
            
        ekini1(1,im) = ekini1(2,im)
        ekini0(1,im) = ekini0(2,im)
        pkini1(1,im) = pkini1(2,im)
        pkini0(1,im) = pkini0(2,im)
        ekini1(neke,im) = ekini1(neke-1,im)
        ekini0(neke,im) = ekini0(neke-1,im)
        pkini1(neke,im) = pkini1(neke-1,im)
        pkini0(neke,im) = pkini0(neke-1,im)
        ek1s1(1,im) = ek1s1(2,im)
        ek1s0(1,im) = ek1s0(2,im)
        pk1s1(1,im) = pk1s1(2,im)
        pk1s0(1,im) = pk1s0(2,im)
        ek1s1(neke,im) = ek1s1(neke-1,im)
        ek1s0(neke,im) = ek1s0(neke-1,im)
        pk1s1(neke,im) = pk1s1(neke-1,im)
        pk1s0(neke,im) = pk1s0(neke-1,im)

10    continue

!  compute constants for user requested K1 scaling

      do j = 1, nreg
        if(k1Lscl(j).gt.0.d0 .and. k1Hscl(j).gt.0.d0) then
          im = med(j)
          c2 = (k1Lscl(j) - k1Hscl(j)) /
     *           dlog( (ae(im) - RM)/(ue(im) - RM) )
          c1 = k1Hscl(j) - dlog(ue(im) - RM) * c2
          k1Lscl(j) = c1
          k1Hscl(j) = c2
        else if(k1Lscl(j).ne.0.d0) then
          k1Lscl(j) = 0.d0 
        end if 
      end do


      return
      end

!-----------------------last line of egs5_rk1.f-------------------------
