!-----------------------------egs5_hatch.f------------------------------
! Version: 060318-1555
! Reference: SLAC-R-730/KEK-2005-8
!-----------------------------------------------------------------------
!23456789|123456789|123456789|123456789|123456789|123456789|123456789|12

      subroutine hatch

      implicit none

      include 'include/egs5_h.f'               ! Main EGS5 "header" file

      include 'include/egs5_bcomp.f'     ! COMMONs required by EGS5 code
      include 'include/egs5_bounds.f'
      include 'include/egs5_brempr.f'
      include 'include/egs5_edge.f'
      include 'include/egs5_eiicom.f'
      include 'include/egs5_elecin.f'
      include 'include/egs5_media.f'
      include 'include/egs5_misc.f'
      include 'include/egs5_photin.f'
      include 'include/egs5_scpw.f'
      include 'include/egs5_stack.f'
      include 'include/egs5_thresh.f'
      include 'include/egs5_uphiin.f' ! Probably don't need this anymore
      include 'include/egs5_uphiot.f'
      include 'include/egs5_useful.f'
      include 'include/egs5_userpr.f'
      include 'include/egs5_usersc.f'
      include 'include/egs5_uservr.f'
      include 'include/egs5_userxt.f'

      include 'include/counters.f'       ! Additional (non-EGS5) COMMONs
      include 'include/randomm.f'

      real*8 rnnow                                           ! Arguments

      real*8                                           ! Local variables
     * zeros(3),
     * acd,asd,cost,s2c2,p,dfact,dfactr,dunitr,dfacti,pznorm,
     * sint,wss,fnsss,wid,dunito,ys,del,xs,adev,s2c2mx,
     * cthet,rdev,s2c2mn,sxx,sxy,sx,sy,xs0,xsi,xs1, tebinda,
     * ecutmn, eke, elke

      integer
     * lok(MXMED),ngs(MXMED),ngc(MXMED),neii(MXMED),
     * msge(MXMED),mge(MXMED),mseke(MXMED),
     * mleke(MXMED),mcmfp(MXMED),mrange(MXMED),
     * ib,im,nm,il,nsge,irayl,ie,i,irn,id,md,jr,neke,nseke,
     * nge,nleke,ngrim,nrange,ncmfp,j,nisub,isub,lmdn,lmdl,
     * i1st,istest,nrna,nsinss,mxsinc_loc,iss,izz,ner,is,
     * mxsim,ii,ifun,ngcim,incoh,impact,ibound,ngsim,lelke

      character mbuf(72),mdlabl(8)

      data 
     * mdlabl/' ','M','E','D','I','U','M','='/,
     * lmdl/8/,
     * lmdn/24/,
     * dunito/1./,
     * i1st/1/,
     * nsinss/37/,
     * mxsinc_loc/MXSINC/,
     * istest/0/,
     * nrna/1000/

! ---------------------
! I/O format statements
! ---------------------
1250  FORMAT(1X,14I5)
1260  FORMAT(1X,1PE14.5,4E14.5)
1270  FORMAT(72A1)
!-----------------------------------------------------------------------
1340  FORMAT(1PE20.7,4E20.7)
1350  FORMAT(' SINE TESTS,MXSINC,NSINSS=',2I5)
1360  FORMAT(' ADEV,RDEV,S2C2(MN,MX) =',1PE16.8,3E16.8)
1380  FORMAT(' TEST AT ',I7,' RANDOM ANGLES IN (0,5*PI/2)')
1390  FORMAT(' ADEV,RDEV,S2C2(MN,MX) =',1PE16.8,3E16.8)
!-----------------------------------------------------------------------
1440  FORMAT(' RAYLEIGH OPTION REQUESTED FOR MEDIUM NUMBER',I3,/)
!-----------------------------------------------------------------------
1510  FORMAT(' DATA FOR MEDIUM #',I3,', WHICH IS:',72A1)
1540  FORMAT (5A1,',RHO=',1PG11.4,',NE=',I2,',COMPOSITION IS :')
1560  FORMAT (6A1,2A1,3X,F3.0,3X,F9.0,4X,F12.0,6X,F12.0)
1570  FORMAT (6A1,2A1,',Z=',F3.0,',A=',F9.3,',PZ=',1PE12.5,',RHOZ=',
     *    1PE12.5)
1580  FORMAT(' ECHO READ:$LGN(RLC,AE,AP,UE,UP(IM))')
!-----------------------------------------------------------------------
1600  FORMAT(' ECHO READ:($LGN(DL(I,IM)/1,2,3,4,5,6/),I=1,6)')
1610  FORMAT(' ECHO READ:DELCM(IM),($LGN(ALPHI,BPAR, DELPOS(I,IM)),I=1
     *,2)')
1620  FORMAT(' ECHO READ:$LGN(XR0,TEFF0,BLCC,XCC(IM))')
1630  FORMAT(' ECHO READ:$LGN(EKE(IM)/0,1/)')
1640  FORMAT(' ECHO READ:($LGN(ESIG,PSIG,EDEDX,PDEDX,EBR1,PBR1,PBR2, T
     *MXS(I,IM)/0,1/),I=1,NEKE)')
1660  FORMAT(' ECHO READ:($LGN(GMFP,GBR1,GBR2(I,IM)/0,1/),I=1,NGE)')
1680  FORMAT(' ECHO READ:NGR(IM)')
1690  FORMAT(' ECHO READ:$LGN(RCO(IM)/0,1/)')
!-----------------------------------------------------------------------
1700  FORMAT(' ECHO READ:($LGN(RSCT(I,IM)/0,1/),I=1,NGRIM)')
1710  FORMAT(' ECHO READ:($LGN(COHE(I,IM)/0,1/),I=1,NGE)')
1720  FORMAT(' RAYLEIGH DATA AVAILABLE FOR MEDIUM',I3, ' BUT OPTION NOT 
     *REQUESTED.',/)
1730  FORMAT(' DUNIT REQUESTED&USED ARE:',1PE14.5,E14.5,'(CM.)')
!-----------------------------------------------------------------------
1830  FORMAT(' EGS SUCCESSFULLY ''HATCHED'' FOR ONE MEDIUM.')
1840  FORMAT(' EGS SUCCESSFULLY ''HATCHED'' FOR ',I5,' MEDIA.')
1850  FORMAT(' END OF FILE ON UNIT ',I2,//, ' PROGRAM STOPPED IN HATCH B
     *ECAUSE THE',/, ' FOLLOWING NAMES WERE NOT RECOGNIZED:',/)
1870  FORMAT(40X,'''',24A1,'''')
!-----------------------------------------------------------------------
2000  FORMAT(5A1,5X,F11.0,4X,I2,9X,I1,9X,I1,9X,I1)
2001  FORMAT(5A1,5X,F11.0,4X,I2,26X,I1,9X,I1,9X,I1)
!-----------------------------------------------------------------------
4090  FORMAT(' INCOHERENT OPTION REQUESTED FOR MEDIUM NUMBER',I3,/)
4110  FORMAT(' COMPTON PROFILE OPTION REQUESTED FOR MEDIUM NUMBER',I3,/)
4130  FORMAT(' E- IMPACT IONIZATION OPTION REQUESTED FOR MEDIUM NUMBER',
     *I3,/)
4280  FORMAT(' ECHO READ:$LGN(MSGE,MGE,MSEKE,MEKE,MLEKE,MCMFP,MRANGE(I
     *M)),IRAYL,IBOUND,INCOH, ICPROF(IM),IMPACT')
4340  FORMAT(' ECHO READ:TEBINDA,$LGN(GE(IM)/0,1/)')
4360  FORMAT(' STOPPED IN HATCH: REQUESTED RAYLEIGH OPTION FOR MEDIUM',
     *I3, /,' BUT RAYLEIGH DATA NOT INCLUDED IN DATA CREATED BY PEGS.')
4370  FORMAT(' STOPPED IN HATCH: REQUESTED INCOHERENT OPTION FOR MEDIUM'
     *,I3, /,' BUT INCOHERENT DATA NOT INCLUDED IN DATA CREATED BY PEGS.
     *')
4375  FORMAT(' STOPPED IN HATCH: REQUESTED INCOHERENT OPTION FOR MEDIUM'
     *,I3, /,' BUT BOUND COMPTON COMPTON CROSS SECTION NOT INCLUDED IN '
     *,'DATA CREATED',/,' BY PEGS -- USE IBOUND=1 IN PEGS.')
4380  FORMAT(' STOPPED IN HATCH: REQUESTED COMPTON PROFILE OPTION FOR ME
     *DIUM',I3, /,' BUT CORRECT COMPTON PROFILE DATA NOT INCLUDED IN ',
     *'DATA CREATED',/,' BY PEGS -- USE ICPROF=-3 IN PEGS.')
4390  FORMAT(' STOPPED IN HATCH: REQUESTED COMPTON PROFILE OPTION FOR ME
     *DIUM',I3, /,' BUT INCOHERENT DATA NOT REQUESTED IN USER CODE.',/,
     *' THIS IS PHYSICALLY INCONSISTENT -- USE INCOHR(I)=1 WHEN IPROFR('
     *,'I)=1.')
4400  FORMAT(' STOPPED IN HATCH: REQUESTED COMPTON PROFILE OPTION FOR ME
     *DIUM',I3, /,' BUT BOUND COMPTON CROSS SECTION NOT USED IN DATA ',
     *'CREATED BY PEGS.',/,' YOU MUST USE IBOUND=1 IN PEGS.')
4410  FORMAT(' STOPPED IN HATCH: REQUESTED e- IMPACT IONIZATION OPTION F
     *OR MEDIUM', I3,/,' BUT e- IMPACT IONIZATION DATA NOT INCLUDED IN D
     *ATA CREATED BY PEGS.')
4450  FORMAT(' ECHO READ:NGS(IM)')
4460  FORMAT(' ECHO READ:$LGN(SCO(IM)/0,1/)')
4470  FORMAT(' ECHO READ:($LGN(SXZ(I,IM)/0,1/),I=1,NGSIM)')
4480  FORMAT(' INCOHERENT DATA AVAILABLE FOR MEDIUM',I3, ' BUT OPTION NO
     *T REQUESTED.',/)
4490  FORMAT(' ECHO READ:NGC(IM)')
4500  FORMAT(' ECHO READ:$LGN(CCO(IM)/0,1/),CPIMEV')
4510  FORMAT(' ECHO READ:($LGN(CPR(I,IM)/0,1/),I=1,NGCIM)')
4520  FORMAT(' TOTAL COMPTON PROFILE DATA AVAILABLE FOR MEDIUM',I3,
     *' BUT OPTION NOT REQUESTED.',/)
4530  FORMAT(' ECHO READ:MXSHEL(IM),NGC(IM)')
4540  FORMAT(' ECHO READ:(ELECNO(I,IM),I=1,MXSIM)')
4550  FORMAT(' ECHO READ:(CAPIO(I,IM),I=1,MXSIM)')
4560  FORMAT(' ECHO READ:$LGN(CCOS(IM)/0,1/)')
4570  FORMAT(' ECHO READ:(($LGN(CPRS(I,IS,IM)/0,1/),IS=1,MXSIM),I=1,NGCI
     *M)')
4580  FORMAT(' SHELL COMPTON PROFILE DATA AVAILABLE FOR MEDIUM',I3,
     *' BUT OPTION NOT REQUESTED.',/)
4590  FORMAT(' ECHO READ:NEPM(IM)')
4610  FORMAT(' ECHO READ:NEII(IM)')
4620  FORMAT(' ECHO READ:$LGN(EICO(IM)/0,1/)')
4630  FORMAT(' ECHO READ:(($LGN(EII(I,IFUN,IM)/0,1/),IFUN=1,NER), I=1,NE
     *II(IM))')
4640  FORMAT(' ELECTRON IMPACT IONIZATION DATA AVAILABLE FOR MEDIUM',I3,
     *' BUT OPTION NOT REQUESTED.',/)
5000  FORMAT(' in HATCH: subroutine block_set has not been called.',/,
     * 'Please check your user code.  Aborting.')
5005  FORMAT(' Warning: Initial Energy less than 10% of UE for medium '
     * ,24a1,/ ' multiple scattering step sizes may be very large') 
5006  FORMAT( ' EMAXE set in HATCH to MIN(UE,UP+RM), = ',1pe13.4)
5007  FORMAT( ' Stopped in HATCH with EMAXE < 100 eV, = ',1pe13.4)
5008  FORMAT( ' Stopped in HATCH with EMAXE = ',1pe13.4,' > UE of ',
     *1pe13.4,' for matierial ',i4)
5009  FORMAT( ' Stopped in HATCH with EKEMAX = ',1pe13.4,' > UP+RM of ',
     *1pe13.4,' for matierial ',i4)


!-----------------------------------------------------------------------

      ihatch = ihatch + 1                  ! Count entry into subroutine

      if(iblock.ne.1) then            ! Check for block_set initizations
        write(6,5000) 
        stop
      endif

      if (i1st .ne. 0) then
        i1st=0
        if (.not.rluxset) then
          write(6,*) 'RNG ranlux not initialized:  doing so in HATCH'
          call rluxgo
        end if
        latchi = 0.0

        nisub = mxsinc_loc - 2
        fnsss = nsinss
        wid = PI5D2/float(nisub)
        wss = wid/(fnsss - 1.0)
        zeros(1) = 0.
        zeros(2) = PI
        zeros(3) = TWOPI
        do isub=1,mxsinc_loc
          sx = 0.
          sy = 0.
          sxx = 0.
          sxy = 0.
          xs0 = wid*float(isub - 2)
          xs1 = xs0 + wid
          iz = 0
          do izz=1,3
            if (xs0 .le. zeros(izz) .and. zeros(izz) .le. xs1) then
              iz = izz
              go to 1
            end if
          end do
 1        continue
          if (iz .eq. 0) then
            xsi = xs0
          else
            xsi = zeros(iz)
          end if
          do iss=1,nsinss
            xs = wid*float(isub - 2) + wss*float(iss - 1) -xsi
            ys = sin(xs + xsi)
            sx = sx + xs
            sy = sy + ys
            sxx = sxx + xs*xs
            sxy = sxy + xs*ys
          end do
          if (iz .ne. 0) then
            sin1(isub) = sxy/sxx
            sin0(isub) = -sin1(isub)*xsi
          else
            del = fnsss*sxx-sx*sx
            sin1(isub) = (fnsss*sxy - sy*sx)/del
            sin0(isub) = (sy*sxx - sx*sxy)/del - sin1(isub)*xsi
          end if
        end do

        sinc0 = 2.
        sinc1 = 1./wid
        if (istest .ne. 0) then
          adev = 0.
          rdev = 0.
          s2c2mn = 10.
          s2c2mx = 0.
          do isub=1,nisub
            do iss=1,nsinss
              theta = wid*float(isub - 1) + wss*float(iss - 1)
              cthet = PI5D2 - theta
              sinthe = sin(theta)
              costhe = sin(cthet)
              sint = sin(theta)
              cost = cos(theta)
              asd = abs(sinthe - sint)
              acd = abs(costhe - cost)
              adev = max(adev,asd,acd)
              if (sint .ne. 0.) rdev = max(rdev,asd/abs(sint))
              if (cost .ne. 0.) rdev = max(rdev,acd/abs(cost))
              s2c2 = sinthe**2 + costhe**2
              s2c2mn = min(s2c2mn,s2c2)
              s2c2mx = max(s2c2mx,s2c2)
              if (isub .lt. 11) write(6,1340) theta,sinthe,sint,
     *                                        costhe,cost
            end do
          end do
          write(6,1350) mxsinc_loc,nsinss
          write(6,1360) adev,rdev,s2c2mn,s2c2mx
          adev = 0.
          rdev = 0.
          s2c2mn = 10.
          s2c2mx = 0.
          do irn=1,nrna
            call randomset(rnnow)
            theta = rnnow*PI5D2
            cthet = PI5D2 - theta
            sinthe = sin(theta)
            costhe = sin(cthet)
            sint = sin(theta)
            cost = cos(theta)
            asd = abs(sinthe - sint)
            acd = abs(costhe - cost)
            adev = max(adev,asd,acd)
            if (sint .ne. 0.) rdev = max(rdev,asd/abs(sint))
            if (cost .ne. 0.) rdev = max(rdev,acd/abs(cost))
            s2c2 = sinthe**2 + costhe**2
            s2c2mn = min(s2c2mn,s2c2)
            s2c2mx = max(s2c2mx,s2c2)
          end do
          write(6,1380) nrna
          write(6,1390) adev,rdev,s2c2mn,s2c2mx
        end if
        p = 1.
        do i=1,50
          pwr2i(i) = p
          p = p/2.
        end do
      end if

      do j=1,nmed
        do i=1,nreg
          if (iraylr(i) .eq. 1 .and. med(i) .eq. j) then
            iraylm(j) = 1
            go to 2
          end if
        end do
 2      continue
      end do

      do j=1,nmed
        do i=1,nreg
          if (incohr(i) .eq. 1 .and. med(i).eq.j) then
            incohm(j) = 1
            go to 20
          end if
        end do
 20     continue
      end do

      do j=1,nmed
        do i=1,nreg
          if (iprofr(i) .eq. 1 .and. med(i) .eq. j) then
            iprofm(j) = 1
            go to 21
          end if
        end do
 21     continue
      end do

      do j=1,nmed
        do i=1,nreg
          if (impacr(i) .eq. 1 .and. med(i) .eq. j) then
            impacm(j) = 1
            go to 22
          end if
        end do
 22     continue
      end do

      rewind kmpi

      nm = 0
      do im=1,nmed
        lok(im) = 0
        if (iraylm(im) .eq. 1) write(6,1440) im
      end do

      do im=1,nmed
        if (incohm(im) .eq. 1) write(6,4090) im
      end do

      do im=1,nmed
        if (iprofm(im) .eq. 1) write(6,4110) im
      end do

      do im=1,nmed
        if (impacm(im) .eq. 1) write(6,4130) im
      end do

 5    continue
      read(kmpi,1270,end=1470) mbuf
      do ib=1,lmdl
        if (mbuf(ib) .ne. mdlabl(ib)) go to 5
      end do
      do 6 im=1,nmed
        do ib=1,lmdn
          il = lmdl + ib
          if (mbuf(il) .ne. media(ib,im)) go to 6
          if (ib .eq. lmdn) go to 7
        end do
 6    continue
      go to 5

 7    continue
      if (lok(im) .ne. 0) go to 5
      lok(im) = 1
      nm = nm + 1

      write(kmpo,1510) im,mbuf
      read(kmpi,2000,err=1000) (mbuf(i),i=1,5),rhom(im),nne(im),
     * iunrst(im),epstfl(im),iaprim(im)
      go to 8

 1000 backspace(kmpi)
      read(kmpi,2001) (mbuf(i),i=1,5),rhom(im),nne(im),iunrst(im),
     * epstfl(im),iaprim(im)

 8    continue
      write(kmpo,1540) (mbuf(i),i=1,5),rhom(im),nne(im)

      do ie=1,nne(im)
        read(kmpi,1560) (mbuf(i),i=1,6),(asym(im,ie,i),i=1,2),
     *   zelem(im,ie),wa(im,ie),pz(im,ie),rhoz(im,ie)
        write(kmpo,1570) (mbuf(i),i=1,6),(asym(im,ie,i),i=1,2),
     *   zelem(im,ie),wa(im,ie),pz(im,ie),rhoz(im,ie)
      end do

      write(kmpo,1580)
      read(kmpi,1260) rlcm(im),ae(im),ap(im),ue(im),up(im)
      write(kmpo,1260)rlcm(im),ae(im),ap(im),ue(im),up(im)

      te(im) = ae(im) - RM
      thmoll(im) = te(im)*2. + RM

      write(kmpo,4280)
      read(kmpi,1250) msge(im),mge(im),mseke(im),meke(im),mleke(im),
     * mcmfp(im),mrange(im),irayl,ibound,incoh,icprof(im),impact
      write(kmpo,1250) msge(im),mge(im),mseke(im),meke(im),mleke(im),
     * mcmfp(im),mrange(im),irayl,ibound,incoh,icprof(im),impact

      nsge = msge(im)
      nge = mge(im)
      nseke = mseke(im)
      neke = meke(im)
      nleke = mleke(im)
      ncmfp = mcmfp(im)
      nrange = mrange(im)

      write(kmpo,1600)
      read(kmpi,1260) (dl1(i,im),dl2(i,im),dl3(i,im),dl4(i,im),
     * dl5(i,im),dl6(i,im),i=1,6)
      write(kmpo,1260) (dl1(i,im),dl2(i,im),dl3(i,im),dl4(i,im),
     * dl5(i,im),dl6(i,im),i=1,6)
      write(kmpo,1610)
      read(kmpi,1260) delcm(im),(alphi(i,im),bpar(i,im),
     * delpos(i,im),i=1,2)
      write(kmpo,1260) delcm(im),(alphi(i,im),bpar(i,im),
     * delpos(i,im),i=1,2)
      write(kmpo,1620)
      read(kmpi,1260) xr0(im),teff0(im),blcc(im),xcc(im)
      write(kmpo,1260) xr0(im),teff0(im),blcc(im),xcc(im)
      write(kmpo,1630)
      read(kmpi,1260) eke0(im),eke1(im)
      write(kmpo,1260) eke0(im),eke1(im)
      write(kmpo,1640)
      read(kmpi,1260) (esig0(i,im),esig1(i,im),psig0(i,im),psig1(i,im),
     * ededx0(i,im),ededx1(i,im),pdedx0(i,im),pdedx1(i,im),ebr10(i,im),
     * ebr11(i,im),pbr10(i,im),pbr11(i,im),pbr20(i,im),pbr21(i,im),
     * tmxs0(i,im),tmxs1(i,im),escpw0(i,im),escpw1(i,im),pscpw0(i,im),
     * pscpw1(i,im),ekini0(i,im),ekini1(i,im),pkini0(i,im),pkini1(i,im),
     * erang0(i,im),erang1(i,im),prang0(i,im),prang1(i,im),estep0(i,im),
     * estep1(i,im),i=1,neke)
      write(kmpo,1260) (esig0(i,im),esig1(i,im),psig0(i,im),psig1(i,im),
     * ededx0(i,im),ededx1(i,im),pdedx0(i,im),pdedx1(i,im),ebr10(i,im),
     * ebr11(i,im),pbr10(i,im),pbr11(i,im),pbr20(i,im),pbr21(i,im),
     * tmxs0(i,im),tmxs1(i,im),escpw0(i,im),escpw1(i,im),pscpw0(i,im),
     * pscpw1(i,im),ekini0(i,im),ekini1(i,im),pkini0(i,im),pkini1(i,im),
     * erang0(i,im),erang1(i,im),prang0(i,im),prang1(i,im),estep0(i,im),
     * estep1(i,im),i=1,neke)

      write(kmpo,4340)
      read(kmpi,1260) tebinda,ge0(im),ge1(im)
      write(kmpo,1260) tebinda,ge0(im),ge1(im)

      write(kmpo,1660)
      read(kmpi,1260) (gmfp0(i,im),gmfp1(i,im),gbr10(i,im),gbr11(i,im),
     * gbr20(i,im),gbr21(i,im),i=1,nge)
      write(kmpo,1260) (gmfp0(i,im),gmfp1(i,im),gbr10(i,im),gbr11(i,im),
     * gbr20(i,im),gbr21(i,im),i=1,nge)

      if (iraylm(im) .eq. 1 .and. (irayl .lt. 1 .or. irayl .gt. 2)) then
        write(6,4360) im
        stop
      end if

      if (incohm(im) .eq. 1 .and. incoh .ne. 1) then
        write(6,4370) im
        stop
      end if

      if (incohm(im) .eq. 1 .and. ibound .ne. 1) then
        write(6,4375) im
        stop
      end if

      if (iprofm(im) .eq. 1) then
        if(icprof(im) .ne. 3) then
          write(6,4380) im
          stop
        else if(incohm(im) .eq. 0) then
          write(6,4390) im
          stop
        else if(ibound .eq. 0) then
          write(6,4400) im
          stop
        endif
      end if

      if (impacm(im) .eq. 1 .and. impact .eq. 0) then
        write(6,4410) im
        stop
      end if

      if (irayl .eq. 1 .or. irayl .eq. 2 .or. irayl .eq. 3) then
        write(kmpo,1680)
        read(kmpi,1250) ngr(im)
        write(kmpo,1250) ngr(im)

        ngrim = ngr(im)

        write(kmpo,1690)
        read(kmpi,1260) rco0(im),rco1(im)
        write(kmpo,1260) rco0(im),rco1(im)
        write(kmpo,1700)
        read(kmpi,1260) (rsct0(i,im),rsct1(i,im),i=1,ngrim)
        write(kmpo,1260) (rsct0(i,im),rsct1(i,im),i=1,ngrim)
        write(kmpo,1710)
        read(kmpi,1260) (cohe0(i,im),cohe1(i,im),i=1,nge)
        write(kmpo,1260) (cohe0(i,im),cohe1(i,im),i=1,nge)

        if (iraylm(im) .ne. 1) write(6,1720) im
      end if

      if (incoh .eq. 1) then
        write(kmpo,4450)
        read(kmpi,1250) ngs(im)
        write(kmpo,1250) ngs(im)
        ngsim = ngs(im)
        write(kmpo,4460)
        read(kmpi,1260) sco0(im),sco1(im)
        write(kmpo,1260) sco0(im),sco1(im)
        write(kmpo,4470)
        read(kmpi,1260) (sxz0(i,im),sxz1(i,im),i=1,ngsim)
        write(kmpo,1260) (sxz0(i,im),sxz1(i,im),i=1,ngsim)
        if (incohm(im) .ne. 1) write(6,4480) im
      end if

        if (icprof(im) .eq. 1 .or. icprof(im) .eq. 2) then
          write(kmpo,4490)
          read(kmpi,1250) ngc(im)
          write(kmpo,1250) ngc(im)
          ngcim = ngc(im)
          write(kmpo,4500)
          read(kmpi,1260) cco0(im),cco1(im),cpimev
          write(kmpo,1260) cco0(im),cco1(im),cpimev
          write(kmpo,4510)
          read(kmpi,1260) (cpr0(i,im),cpr1(i,im),i=1,ngcim)
          write(kmpo,1260) (cpr0(i,im),cpr1(i,im),i=1,ngcim)
          if (iprofm(im) .ne. 1) write(6,4520) im
        end if

        if (icprof(im) .eq. 3 .or. icprof(im) .eq. 4) then
          write(kmpo,4530)
          read(kmpi,1250) mxshel(im),ngc(im)
          write(kmpo,1250) mxshel(im),ngc(im)
          ngcim = ngc(im)
          mxsim = mxshel(im)
          write(kmpo,4540)
          read(kmpi,1260) (elecno(i,im),i=1,mxsim)
          write(kmpo,1260) (elecno(i,im),i=1,mxsim)
          write(kmpo,4550)
          read(kmpi,1260) (capio(i,im),i=1,mxsim)
          write(kmpo,1260) (capio(i,im),i=1,mxsim)
          write(kmpo,4560)
          read(kmpi,1260) ccos0(im),ccos1(im)
          write(kmpo,1260) ccos0(im),ccos1(im)
          write(kmpo,4570)
          read(kmpi,1260) ((cprs0(i,is,im),cprs1(i,is,im),is=1,mxsim),
     *                    i=1,ngcim)
          write(kmpo,1260) ((cprs0(i,is,im),cprs1(i,is,im),is=1,mxsim),
     *                     i=1,ngcim)
          if (iprofm(im) .ne. 1) write(6,4580) im
        end if

        if (impact .ge. 1) then
          write(kmpo,4590)
          read(kmpi,1250) nepm(im)
          write(kmpo,1250) nepm(im)
          ner = nepm(im)
          write(kmpo,4610)
          read(kmpi,1250) neii(im)
          write(kmpo,1250) neii(im)
          write(kmpo,4620)
          read(kmpi,1260) eico0(im),eico1(im)
          write(kmpo,1260) eico0(im),eico1(im)
          write(kmpo,4630)
          read(kmpi,1260) ((eii0(i,ifun,im),eii1(i,ifun,im),
     *                    ifun=1,ner),i=1,neii(im))
          write(kmpo,1260) ((eii0(i,ifun,im),eii1(i,ifun,im),
     *                     ifun=1,ner),i=1,neii(im))
          if (impacm(im) .ne. 1) write(6,4640) im
        end if

      if (nm .ge. nmed) go to 9
      go to 5

 9    continue

!----------------------------------------------
! Finished reading all media data at this point
!----------------------------------------------

!----------------------------------------------------------
! Check to make sure that the incident total energy is
! below the limits in PEGS (i.e., UE and UP) for all media.
!----------------------------------------------------------
      if(emaxe .eq. 0.d0) then
        emaxe=1.d50
        do j=1,nmed
          emaxe=min(ue(j),up(j)+RM,emaxe)
        end do
        write(6,5006) emaxe
      else if(emaxe .lt. 1.d-4) then
        write(6,5007) emaxe
        stop
      else
        do j=1,nmed
          if (emaxe .gt. ue(j)) then
            write(6,5008) emaxe, ue(j), j
            stop
          end if
          if (emaxe-RM .gt. up(j)) then
            write(6,5009) emaxe-RM, up(j), j
            stop
          end if
        end do
      end if

      dunitr = dunit
      if (dunit .lt. 0.) then
        id = max0(1,min0(MXMED,int(-dunit)))
        dunit = rlcm(id)
      end if
      if (dunit .ne. 1.) write(6,1730) dunitr,dunit

      do im=1,nmed
        dfact = rlcm(im)/dunit
        dfacti = 1.0/dfact

        i = 1
        go to 11
 10     i = i + 1
 11     if (i - meke(im) .gt. 0) go to 12
        esig0(i,im) = esig0(i,im)*dfacti
        esig1(i,im) = esig1(i,im)*dfacti
        psig0(i,im) = psig0(i,im)*dfacti
        psig1(i,im) = psig1(i,im)*dfacti
        ededx0(i,im) = ededx0(i,im)*dfacti
        ededx1(i,im) = ededx1(i,im)*dfacti
        pdedx0(i,im) = pdedx0(i,im)*dfacti
        pdedx1(i,im) = pdedx1(i,im)*dfacti
        tmxs0(i,im) = tmxs0(i,im)*dfact
        tmxs1(i,im) = tmxs1(i,im)*dfact
        go to 10
 12     continue

        i = 1
        go to 14
 13     i = i + 1
 14     if (i - mleke(im) .gt. 0) go to 15
        if (dunitr.eq.1) then
          dfactr = 1.d0
        else
          dfactr = 1.d0 / dunit
        end if
        escpw0(i,im) = escpw0(i,im)*dfactr
        escpw1(i,im) = escpw1(i,im)*dfactr
        pscpw0(i,im) = pscpw0(i,im)*dfactr
        pscpw1(i,im) = pscpw1(i,im)*dfactr
        ekini0(i,im) = ekini0(i,im)*dfactr
        ekini1(i,im) = ekini1(i,im)*dfactr
        pkini0(i,im) = pkini0(i,im)*dfactr
        pkini1(i,im) = pkini1(i,im)*dfactr
        erang0(i,im) = erang0(i,im)*dfactr
        erang1(i,im) = erang1(i,im)*dfactr
        prang0(i,im) = prang0(i,im)*dfactr
        prang1(i,im) = prang1(i,im)*dfactr
        go to 13
 15     continue

        teff0(im) = teff0(im)*dfact
        blcc(im) = blcc(im)*dfacti
        xcc(im) = xcc(im)*sqrt(dfacti)
        rldu(im) = rlcm(im)/dunit

        i = 1
        go to 17
 16     i = i + 1
 17     if (i - mge(im) .gt. 0) go to 18
        gmfp0(i,im) = gmfp0(i,im)*dfact
        gmfp1(i,im) = gmfp1(i,im)*dfact
        go to 16
 18     continue
      end do

      ecutmn = 1.d20
      vacdst = vacdst*dunito/dunit
      dunito = dunit
      do jr=1,nreg
        md = med(jr)
        if (md .ge. 1 .and. md .le. nmed) then
          ecut(jr) = max(ecut(jr),ae(md))
          pcut(jr) = max(pcut(jr),ap(md))
          ecutmn = min(ecutmn,ecut(jr))
          !  get the range at cutoff for e- and e+, so we don't
          !  have to comput it continually
          eke = ecut(jr) - RM
          elke = log(eke)
          lelke = eke1(md)*elke + eke0(md)
          ectrng(jr) = erang1(lelke,md)*elke + erang0(lelke,md)
          pctrng(jr) = prang1(lelke,md)*elke + prang0(lelke,md)
          if (rhor(jr) .eq. 0.0) rhor(jr) = rhom(md)
        end if
      end do

      if (ibrdst .eq. 1 .or. iprdst .gt. 0) then
        do im=1,nmed
          zbrang(im) = 0.
          pznorm = 0.
          do ie=1,nne(im)
            zbrang(im) = zbrang(im) + pz(im,ie)*zelem(im,ie)*
     *                   (zelem(im,ie) + 1.0)
            pznorm = pznorm + pz(im,ie)
          end do
          zbrang(im) = (8.116224E-05)*(zbrang(im)/pznorm)**(1./3.)
        end do
      end if

      do ii=1,nreg
        if (iedgfl(ii) .ne. 0 .or. iauger(ii) .ne. 0) then
!         ===========
          call edgbin
!         ===========
          go to 19
        end if
      end do

 19   continue

      if (nmed .eq. 1) then
        write(6,1830)
      else
        write(6,1840) nmed
      end if

!     ===========
      call rk1                         ! get problem scattering strength
!     ===========

!     ===========
      call rmsfit                        ! read multiple scattering data
!     ===========

!  Warning here about EFRAC in case UE << emaxe and using
!  old algorithm without characteristic dimension.  

      do i=1,nmed
        if(emaxe/ue(i).lt.0.1d0 .and. charD(i).eq.0.d0) then
          write(6,5005) (media(j,i),j=1,24)
        endif
      end do

      return

1470  write(6,1850) kmpi
      do im=1,nmed
        if (lok(im) .ne. 1) write(6,1870) (media(i,im),i=1,lmdn)
      end do

      stop
      end

!----------------------last line of egs5_hatch.f------------------------
