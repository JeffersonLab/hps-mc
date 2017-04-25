!***********************************************************************
!***************************                                           *
!*** u c _ e x a m i n *****            EGS5.0 USER CODE - 060622-1015 *
!***************************                                           *
!***********************************************************************
!* This is a User Code based on EXAMIN.MOR from EGS4                   *
!***********************************************************************
!                                                                      *
!***********************************************************************
!23456789|123456789|123456789|123456789|123456789|123456789|123456789|12
!-----------------------------------------------------------------------
!------------------------------- main code -----------------------------
!-----------------------------------------------------------------------

      implicit none

!     ------------
!     EGS5 COMMONs
!     ------------
      include 'include/egs5_h.f'                ! Main EGS "header" file

      include 'include/egs5_epcont.f'
      include 'include/egs5_media.f'
      include 'include/egs5_misc.f'
      include 'include/egs5_thresh.f'
      include 'include/egs5_useful.f'
      include 'include/egs5_elecin.f'
      include 'include/egs5_photin.f'
      include 'include/egs5_usersc.f'

      real*8                                    !  Local variables
     *       ge,gmfp,raycor,compt,tp,cohfac,gbr1,gbr2
      real*8 photoe,toticm,totgcm
      real*8 eie,efract,tmxs,dedxe,dedxp
      real*8 sige,sigp,breme,bremp
      real*8 dy(300)

      character*60 title,xaxis,yaxispcom,yaxispmfp,yaxise,outfile,yaxise
     *mfp, subtitle,series
      character*50 filename
      character*24 material
      character*100 script(3)
      character*8 timen
      character*11 daten
      character*24 dntime

      integer epopt,pltopt,ncurve
      integer iopt,indiv,ifile,iplote,iplotg,nunit
      integer ido,irl
      integer iflag1,iflag2,i1,i2,icurvE
      integer filelength

      integer i,j,lelke,lgle

      real*8 etab(16),plote(100),plotem(100),ploteen(100), plotg(100),
     *       plotgen(100), plotemp(100), plotems(100), phote_comp(300),
     *       comp_comp(300),pair_comp(300), coher_comp(300)

      data etab
     *     /1.,1.25,1.5,1.75,2.,2.5,3.,3.5,4.,4.5,5.,5.5,6.,7.,8.,9./

!     ==============
      call block_set                 ! Initialize some general variables
!     ==============

      yaxispcom = 'fractional component of photon cross section'
      yaxispmfp = 'mean free path / cm'
      yaxisemfp = 'mean free path / cm'
      yaxise = 'dE/drhoX MeV/g/cm\\S2\\N'
      xaxis = 'kinetic energy / MeV'

      script(1)='#!/bin/csh'
      script(2)='#This is xmgr_script used with examin'

      do i=1,300
        dy(i) = 0.d0
      end do

      write(6,1020)
1020  FORMAT(' Material Identifier: ',$)
      read(5,1030)(media(j,1),j=1,24)
1030  FORMAT(24a1)

      nmed = 1

!     ==========
      call pegs5
!     ==========

      do j=1,24
        material(j:j)=media(j,1)
      end do

      filename(1:20)=material(1:20)
      write(6,1050) filename(1:20)
1050  FORMAT(///' Title after  ', a20, $)
      read(5,1060) filename(21:50)
1060  FORMAT(a30)
      write(6,1070)
1070  FORMAT(' Electrons and photons(0), only electrons(1) or photons(2)
     *: ',$)
      read(5,1080) iopt
1080  FORMAT(i3)
      if (iopt.eq.0 .or. iopt .eq. 2) then
        write(6,1090)
1090    FORMAT(' Include Rayleigh(coherent)scattering(1) or not(0): ',$)
        read(5,1100) iraylr(1)
1100    FORMAT(i10)
      else
        iraylr(1) = 0
      end if
      write(6,1110)
1110  FORMAT(' Tables(0) or individual energies(1): ',$)
      read(5,1120) indiv
1120  FORMAT(i10)
      write(6,1130)
1130  FORMAT(' Output  terminal(0),  disk file(1), terminal&plot(2), dis
     *k&plot(3): ',$)
      read(5,1140) ifile
1140  FORMAT(I10)
      if ((ifile.lt.0 .or. ifile.gt.4)) then
        ifile=0
      end if
      iplote=0
      iplotg=0

!     ==================
      call fdate(dntime)
!     ==================
      daten(1:7)=dntime(5:12)
      daten(8:11)=dntime(21:24)
!     ==================
      call fdate(dntime)
!     ==================
      timen=dntime(12:19)

      if ((ifile .eq. 0 .or. ifile .eq. 2)) then
        nunit=6
      else
        nunit=1
        open(UNIT=nunit,FILE='egs5job.out',STATUS='unknown')
      end if

!-----------------------------------------------------------------------
! Pre-hatch-call-initialization
!-----------------------------------------------------------------------

      nreg = 1
      med(1) = 1

!     ------------------------------
!     Open files (before HATCH call)
!     ------------------------------
      open(UNIT=KMPI,FILE='pgs5job.pegs5dat',STATUS='old')
      open(UNIT=KMPO,FILE='egs5job.dummy',STATUS='unknown')

      write(6,1150)
1150  FORMAT(/' call hatch')
      
!     ==========
      call hatch
!     ==========

      write(6,1160)
1160  FORMAT(' call to hatch completed'/)
      
      medium=med(1)
      irl=1

      if ((iopt .ne. 1)) then
        if ((ap(1).lt.up(1))) then
1170      write(nunit,1180) (media(j,1),j=1,10),filename(21:50),timen,da
     *    ten
1180      FORMAT(' Summary photon data: medium = ',10a1, ' file= ',a30/t
     *    60,a8,1x,a11/' ',79('='))
          write(nunit,1190) ae(1)-RM,ue(1)-RM,ap(1),up(1)
1190      FORMAT(/' Electron kinetic energy range:',t35,2f12.3,' MeV'/ '
     * Photon energy range:          ',t35,2f12.3,' meV')
          write(nunit,1200) rlcm(medium),rhom(medium)
1200      FORMAT(/' Radiation length = ',t35,f13.6,' cm'/ ' density = ',
     *    t35,f13.6,' g/cm**3')
          if ((iraylr(1) .eq. 0)) then
            write(nunit,1210)
1210        FORMAT(//' ','Photon en  ', 'gmfp ', '  photoelec ', ' compt
     *on  ', '  pair      ', '        total')
            write(nunit,1220)
1220        FORMAT(t13,'(cm)', t52,'cm**2/g',t63,'cm**-1')
          else
            write(nunit,1230)
1230        FORMAT(//' ','Photon en  ', 'gmfp ', '            photoelec 
     *', ' compton  ', '  pair      ', '        total')
            write(nunit,1240)
1240        FORMAT(t13,'(cm) (Rayleigh)',t60,'cm**2/g',t71,'cm**-1')
          end if

          iflag1=0
          iflag2=0
            do i1=1,8
              do i2=1,16
1270          if ((indiv .eq. 1)) then
                write(6,1280)
1280            FORMAT(' Photon energy (MeV, 0=>go to electrons): ',$)
                read(5,'(f12.0)',err=1270) ge
                if ((ge .gt. up(1) .or. ge .lt. ap(1))) then
                  write(6,1290)ap(1),up(1)
1290              FORMAT(t20,'   must be in range',2(1pe10.3))
                end if
                if ((ge .le. 0.0)) then
                  go to 1300
                end if
              else
                ge=etab(i2)*10.**(i1-4)
              end if
              if ((ge .le. ap(1))) then
                if ((iflag1 .eq. 0)) then
                  iflag1=1
                  ge=ap(1)
                else
                  ge=0.0
                end if
              end if
              if ((ge .ge. up(1))) then
                if ((iflag2 .eq. 0)) then
                  iflag2=1
                  ge = up(1)
                else
                  ge=1.e30
                end if
              end if
              if ((ge .ge. ap(1)-0.00001 .and. ge .le. up(1)+0.001)) the
     *        n
                gle=dlog(ge)
                lgle=ge1(medium)*gle+ge0(medium)
                if ((iraylr(1) .eq. 1)) then
                  gmfp=1.0
                  if ((iraylr(irl).eq.1)) then
                    cohfac=cohe1(lgle,medium)*gle+cohe0(lgle,medium)
                    gmfp=gmfp*cohfac
                  end if
                  raycor=gmfp
                end if
                gmfp=gmfp1(lgle,medium)*gle+gmfp0(lgle,medium)
                if ((iraylr(irl).eq.1)) then
                  cohfac=cohe1(lgle,medium)*gle+cohe0(lgle,medium)
                  gmfp=gmfp*cohfac
                end if
                gbr1=gbr11(lgle,medium)*gle+gbr10(lgle,medium)
                gbr2=gbr21(lgle,medium)*gle+gbr20(lgle,medium)
                compt = dmax1(gbr2-gbr1, 0.d0)
                photoe= dmax1(1.-gbr2, 0.d0)
                toticm=1./gmfp
                totgcm=toticm/rhom(1)
                if ((i2 .eq. 1)) then
                  write(nunit,1310)
                end if
1310            FORMAT('   ')
                if ((iraylr(1) .eq. 0)) then
                  write(nunit,1320) ge,gmfp,photoe,compt,gbr1,totgcm,tot
     *            icm
1320              FORMAT(' ',f7.3,2x,f7.4,2x,f8.5,2x,f8.5,2x,f8.5,2x, 1p
     *            e10.3,1x,1pe10.3)
                else
                  write(nunit,1330) ge,gmfp,(1.-raycor),photoe*raycor, c
     *            ompt*raycor,gbr1*raycor,totgcm,toticm
1330              FORMAT(' ',e8.3,' ',e8.3,' ',f6.4,' ',2x, f8.5,2x,f8.5
     *            ,2x,f8.5,2x,1pe10.3,1x,1pe10.3)
                end if
                if ((ifile .ge. 2)) then
                  iplotg=iplotg+1
                  if ((iraylr(1).eq.0)) then
                    raycor = 1.00
                  end if
                  plotgen(iplotg)=ge
                  plotg(iplotg)=gmfp
                  phote_comp(iplotg) = photoe*raycor
                  comp_comp(iplotg) = compt*raycor
                  pair_comp(iplotg) = gbr1*raycor
                  coher_comp(iplotg) = 1.0 - raycor
                end if
              end if
            end do
          end do
        end if
      end if
1300  if ((iopt .ne. 2)) then
        write(nunit,1340) (media(j,1),j=1,10),filename(21:50),timen,date
     *  n
1340    FORMAT(////' Summary electron data: medium=',10a1, ' file:',a30/
     *   t60,a8,1x,a11/' ',79('='))
        write(nunit,1350) ae(1)-RM,ue(1)-RM,ap(1),up(1)
1350    FORMAT(/' Electron kinetic energy range:',t35,2f12.3,' MeV'/ ' P
     *hoton energy range:' ,t35,2f12.3,' MeV')
        write(nunit,1360) rlcm(medium),rhom(medium),200.*teff0(1)
1360    FORMAT(/' Radiation length = ',t35,f13.6,' cm'/ ' density = ' ,t
     *  35,f13.6,' g/cm**3'/ ' 200.*teffo = ' ,t35,f13.6,' cm')
        if ((iaprim(1) .eq. 0)) then
          write(nunit,1370)
1370      FORMAT(/' Koch and Motz radiative stopping' )
        else if((iaprim(1) .eq. 1)) then
          write(nunit,1380)
1380      FORMAT(/' ICRU radiative stopping')
        else
          write(nunit,1390)
1390      FORMAT(/' No corrections in radiative cross sections ')
        end if
        if ((epstfl(1) .eq. 0)) then
          write(nunit,1400)
1400      FORMAT(' Standard PEGS4 density effect for collision stopping'
     *    )
        else
          write(nunit,1410)
        end if
1410    FORMAT(' ICRU or other density effect  (EPSTFL not zero)' )
        if ((iunrst(1) .eq. 0)) then
          write(nunit,1420)
1420      FORMAT(' Standard PEGS4 data set (restricted collision +', ' r
     *adiative (IUNRST=0))' )
        else if((iunrst(1) .eq. 1)) then
          write(nunit,1430)
1430      FORMAT(' Unrestricted collision stopping power (IUNRST=1)')
        else if((iunrst(1) .eq. 2)) then
          write(nunit,1440)
1440      FORMAT(' Unrestricted collison + radiative stopping power', '
     *(IUNRST=2)' )
        else if((iunrst(1) .eq. 3)) then
          write(nunit,1450)
1450      FORMAT(' Unrestricted collision + restricted radiative', ' sto
     *pping power (IUNRST=3)' )
        else if((iunrst(1) .eq. 4)) then
          write(nunit,1460)
1460      FORMAT(' Restricted collision + unrestricted radiative', ' sto
     *pping power (IUNRST=4)' )
        else if((iunrst(1) .eq. 5)) then
          write(nunit,1470)
1470      FORMAT(' Unrestricted radiative stopping power (IUNRST=5)' )
        end if
        write(nunit,1490)
1490    FORMAT(///' Kin En',t13,'e(-),     e(+) dE/dX' ,t38,'e(-),   e(+
     *) mean free path (brem fraction)', /t5,'MeV',t17,'MeV/(g/cm**2)',t
     *  56,'cm')
        iflag1=0
        iflag2=0
          do i1=1,8
            do i2=1,16
1520        if ((indiv .eq. 1)) then
              write(6,1530)
1530          FORMAT(' electron kinetic energy( MeV): ',$)
              read(5,'(f12.0)',err=1520,end= 1300) eke
              if ((eke .gt. ue(1)-RM .or. eke .lt. ae(1)-RM)
     *        ) then
                write(6,1540)ae(1)-RM,ue(1)-RM
1540            FORMAT(t20,'   must be in range',2(1pe10.3))
              end if
              if ((eke .eq. 0.0)) then
                stop
              end if
            else
              eke=etab(i2)*10.**(i1-4)
            end if
            if ((eke .le. ae(1)-RM)) then
              if ((iflag1 .eq. 0)) then
                iflag1=1
                eke=ae(1)-RM
              else
                eke=0.0
              end if
            end if
            if ((eke .gt. ue(1)-RM)) then
              if ((iflag2 .eq. 0)) then
                iflag2=1
                eke=ue(1)-RM
              else
                eke=1.e30
              end if
            end if
            eie=eke+RM
            tmxs=0.0
            dedxe=0.0
            dedxp=0.0
            efract=0.0
            if ((eie .ge. ae(1)-0.0001 .and. eie .le. ue(1)+0.001)) then
              elke=dlog(eke)
              lelke=eke1(medium)*elke+eke0(medium)
              tmxs=tmxs1(lelke,medium)*elke+tmxs0(lelke,medium)
              tp=200*teff0(medium)
              if ((estepr(irl).ne.0)) then
                tmxs=tmxs*estepr(irl)
              end if
              tmxs = dmin1(tmxs,tp)
              dedxe=ededx1(lelke,medium)*elke+ededx0(lelke,medium)
              dedxp=pdedx1(lelke,medium)*elke+pdedx0(lelke,medium)
              efract= tmxs*dedxe/eke
              sige=esig1(lelke,medium)*elke+esig0(lelke,medium)
              if ((sige .eq. 0.0)) then
                sige=1.e-10
              end if
              sigp=psig1(lelke,medium)*elke+psig0(lelke,medium)
              if ((sigp .eq. 0.0)) then
                sigp=1.e-10
              end if
              breme=ebr11(lelke,medium)*elke+ebr10(lelke,medium)
              bremp=pbr11(lelke,medium)*elke+pbr10(lelke,medium)
              if ((i2 .eq. 1)) then
                write(nunit,1550)
              end if
              write(nunit,1560) eke,dedxe/rhom(medium), 
     *                  dedxp/rhom(medium),1./sige,breme,1./sigp,bremp
1560           FORMAT(' ',e8.3,t11,0pf9.4,t24,0pf9.4,t37, 1pe9.3,1X,' ',
     *         0pf7.5,' ',t59,1pe9.3,1X,' ',0pf7.5,' ')
              if ((ifile .ge. 2)) then
                iplote=iplote+1
                ploteen(iplote)=eke
                plote(iplote)=dedxe/rhom(medium)
                plotem(iplote)=1./sige
                plotemp(iplote)=(1./sige)*breme
                plotems(iplote)=(1./sige)*(1.-breme)
              end if
            end if
          end do
        end do
      end if
      if ((ifile .eq. 1)) then
        close(nunit)
      end if
      if ((ifile .ge. 2)) then
        title=filename
1570    if ((iopt.eq.2)) then
1581      continue
            write(6,1590)
1590        FORMAT(/' Plot for photons (0) or exit (2):',$)
            read(5,1600)pltopt
1600        FORMAT(i4)
            if(((pltopt.eq.0 .or. pltopt.eq.2)))go to1582
          go to 1581
1582      continue
        else if((iopt.eq.1)) then
1611      continue
            write(6,1620)
1620        FORMAT(/' Plot for electrons (1) or exit (2):',$)
            read(5,1630)pltopt
1630        FORMAT(i4)
            if(((pltopt.eq.1 .or. pltopt.eq.2)))go to1612
          go to 1611
1612      continue
        else
1641      continue
            write(6,1650)
1650        FORMAT(/' Plot for photons (0), electrons (1) or exit(2):',$
     *      )
            read(5,1660)pltopt
1660        FORMAT(i4)
            if(((pltopt.eq.0 .or. pltopt.eq.1 .or. pltopt.eq.2)))go to16
     *      42
          go to 1641
1642      continue
        end if
        if ((pltopt.eq.0)) then
          if ((iplotg.gt.0)) then
1671        continue
              write(6,1680)
1680          FORMAT(/' Plot the relative components(0) or mfp(1):',$)
              read(5,1690)ido
1690          FORMAT(i10)
              if(((ido.eq.0 .or. ido.eq.1)))go to1672
            go to 1671
1672        continue
            if ((ido .eq. 0)) then
              outfile ='ph_comp_'//material
              filelength = 0
1701          continue
                filelength = filelength + 1
                if(((outfile(filelength:filelength) .eq. ' ')))go to1702
              go to 1701
1702          continue
              outfile(filelength:filelength+5)='.xvgr'
              subtitle='Photons: Relative Components'
              open(UNIT=7,FILE=outfile,STATUS='unknown')
              series='photoelectric component'
              icurve = 1
              call xvgrplot(plotgen,phote_comp,dy,iplotg,(icurve-1),seri
     *        es, xaxis,yaxispcom,title,subtitle,7,0,0.d0,2)
              series='Compton component'
              icurve = 2
              call xvgrplot(plotgen,comp_comp,dy,iplotg,(icurve-1),serie
     *        s, xaxis,yaxispcom,title,subtitle,7,0,0.d0,2)
              series='pair component'
              icurve = 3
              calL xvgrplot(plotgen,pair_comp,dy,iplotg,(icurve-1),serie
     *        s, xaxis,yaxispcom,title,subtitle,7,0,0.d0,2)
              if ((iraylr(1) .eq. 1)) then
                series='Rayleigh/coherent component'
                icurve = 4
                call xvgrplot(plotgen,coher_comp,dy,iplotg,(icurve-1),se
     *          ries, xaxis,yaxispCOM,title,subtitle,7,0,0.d0,2)
              end if
              close(7)
              script(3)='xmgrace '//outfile//' 2> /dev/null'
              open(4,FILE='xmgr_script',FORM='formATTED',STATUS='unknown
     *')
              write(6,1710)script(3)
1710          FORMAT(a80)
              write (4,*)script(1)(1:60)
              write (4,*)script(2)(1:60)
              write (4,*)script(3)
              close(4)
              call system('chmod +x xmgr_script')
              call system('xmgr_script &')
            else
              outfile ='ph_mfp_'//material
              ncurve = 1
              filelength = 0
1721          continue
                filelength = filelength + 1
                IF(((outfile(filelength:filelength) .eq. ' ')))go to1722
              go to 1721
1722          continue
              outfile(filelength:filelength+5)='.xvgr'
              open(UNIT=7,FILE=outfile,STATUS='unknown')
              series = ' photon mean free path'
              subtitle='Photons:  Mean Free Path'
              call xvgrplot(plotgen,plotg,dy,iplotg,0,series, xaxis,yaxi
     *        spmfp,title,subtitle,7,0,0.d0,0)
              close(7)
              script(3)='xmgrace '//outfile//' 2> /dev/null'
              open(4,file='xmgr_script',FORM='formatted',STATUS='unknown
     *')
              write(6,1730)script(3)
1730          FORMAT(a80)
              write (4,*)script(1)(1:60)
              write (4,*)script(2)(1:60)
              write (4,*)script(3)
              close(4)
              call system('chmod +x xmgr_script')
              call system('xmgr_script &')
            end if
          end if
        else if((pltopt.eq.1)) then
          ncurve = 1
1741      continue
            write(6,1750)
1750        FORMAT(/' Plot st.power(0), mfp to discrete int(1), brem(2),
     * sec.el(3):',$)
            read(5,1760)epopt
1760        FORMAT(i4)
            if(((epopt.ge.0 .and. epopt.le.3)))go to1742
          go to 1741
1742      continue
          if ((epopt .eq. 0)) then
            write(6,1770)
1770        FORMAT('   doing stopping powers')
            if ((iplote.gt.0)) then
              outfile ='e_sp_'//material
              series='restricted stopping power'
              subtitle='Electrons:  Stopping Powers'
              filelength = 0
1781          continue
                filelength = filelength + 1
                if(((outfile(filelength:filelength) .eq. ' ')))go to1782
              go to 1781
1782          continue
              outfile(filelength:filelength+5)='.xvgr'
              open(UNIT=7,FILE=outfile,STATUS='unknown')
              call xvgrplot(ploteen,plote,dy,iplote,0,series, xaxis,yaxi
     *        se,title,subtitle,7,0,0.d0,2)
              close(7)
              script(3)='xmgrace '//outfile//' 2> /dev/null'
              open(4,FILE='xmgr_script',FORM='formatted',STATUS='unknown
     *')
              write(6,1790)script(3)
1790          FORMAT(a80)
              write (4,*)script(1)(1:60)
              write (4,*)script(2)(1:60)
              write (4,*)script(3)
              close(4)
              call system('chmod +x xmgr_script')
              call system('xmgr_script &')
            end if
          else if((epopt .eq. 1)) then
            if ((iplote.gt.0)) then
              outfile ='e_mfpd_'//material
              series='mfp to discrete interaction'
              subtitle='Electrons:  MFP to discrete interactions'
              filelength = 0
1801          continue
                filelength = filelength + 1
                if(((outfile(filelength:filelength) .eq. ' ')))go to1802
              go to 1801
1802          continue
              outfile(filelength:filelength+5)='.xvgr'
              open(UNIT=7,FILE=outfile,STATUS='unknown')
              call xvgrplot(ploteen,plotem,dy,iplote,0,series, xaxis,yax
     *        iseMFP,title,subtitle,7,0,0.d0,0)
              close(7)
              script(3)='xmgrace '//outfile//' 2> /dev/null'
              open(4,FILE='xmgr_script',FORM='formatted',STATUS='unknown
     *')
              write(6,1810)script(3)
1810          FORMAT(a80)
              write (4,*)script(1)(1:60)
              write (4,*)script(2)(1:60)
              write (4,*)script(3)
              close(4)
              call system('chmod +x xmgr_script')
              call system('xmgr_script &')
            end if
          else if((epopt .eq. 2)) then
            if ((iplote.gt.0)) then
              outfile ='e_mfpp_'//material
              series='mfp to brem interaction'
              subtitle='Electrons:  MFP to brem interactions'
              filelength = 0
1821          continue
                filelength = filelength + 1
                if(((outfile(filelength:filelength) .eq. ' ')))go to1822
              go to 1821
1822          continue
              outfile(filelength:filelength+5)='.xvgr'
              open(UNIT=7,FILE=outfile,STATUS='unknown')
              call xvgrplot(ploteen,plotemp,dy,iplote,0,series, xaxis,ya
     *        xispMFP,title,subtitle,7,0,0.d0,0)
              close(7)
              script(3)='xmgrace '//outfile//' 2> /dev/null'
              open(4,file='xmgr_script',form='formatted',status='unknown
     *')
              write(6,1830)script(3)
1830          FORMAT(a80)
              write (4,*)script(1)(1:60)
              write (4,*)script(2)(1:60)
              write (4,*)script(3)
              close(4)
              call system('chmod +x xmgr_script')
              call system('xmgr_script &')
            end if
          else if((epopt .eq. 3)) then
            if ((iplote.gt.0)) then
              outfile ='e_mfps_'//material
              series='mfp to delta interaction'
              subtitle='Electrons:  MFP to secondary electron interactio
     *ns'
              filelength = 0
1841          continue
                filelength = filelength + 1
                if(((outfile(filelength:filelength) .eq. ' ')))go to1842
              go to 1841
1842          continue
              outfile(filelength:filelength+5)='.xvgr'
              open(UNIT=7,FILE=outfile,STATUS='unknown')
              call xvgrplot(ploteen,plotems,dy,iplote,0,series, xaxis,ya
     *        xispMFP,title,subtitle,7,0,0.d0,2)
              close(7)
              script(3)='xmgrace '//outfile//' 2> /dev/null'
              open(4,file='xmgr_script',form='formatted',status='unknown
     *')
              write(6,1850)script(3)
1850          FORMAT(a80)
              write (4,*)script(1)(1:60)
              write (4,*)script(2)(1:60)
              write (4,*)script(3)
              close(4)
              call system('chmod +x xmgr_script')
              call system('xmgr_script &')
            end if
          end if
        end if
        if ((pltopt .ne. 2)) then
          go to 1570
        end if
      end if
1860  FORMAT(///' What fraction should tmxs be(<=0.0 => exit): ')               
1550  FORMAT('   ')                                                             
      stop
      end
!-------------------------last line of main code------------------------

!-------------------------------ausgab----------------------------------
!-----------------------------------------------------------------------
!23456789|123456789|123456789|123456789|123456789|123456789|123456789|12
! ----------------------------------------------------------------------
! Required subroutine for use with the EGS5 Code System
! ----------------------------------------------------------------------
!***********************************************************************
!
! Dummy version for this code
!***********************************************************************
      subroutine ausgab(iarg)

      implicit none

      integer iarg

      return
      end
!-------------------------last line of ausgab---------------------------

!-------------------------------howfar----------------------------------
!-----------------------------------------------------------------------
!23456789|123456789|123456789|123456789|123456789|123456789|123456789|12
! ----------------------------------------------------------------------
! Required subroutine for use with the EGS5 Code System
! ----------------------------------------------------------------------
!***********************************************************************
!
! Dummy version for this user code
!
!***********************************************************************
      subroutine howfar

      implicit none

      return
      end
!-------------------------last line of howfar---------------------------

!-----------------------------xvgrplot----------------------------------
!-----------------------------------------------------------------------
!23456789|123456789|123456789|123456789|123456789|123456789|123456789|12
! ----------------------------------------------------------------------
! User subroutine for examining PEGS data
! ----------------------------------------------------------------------
!***********************************************************************

      subroutine xvgrplot (x, y, erry, npts, curvenum, seriestitle, xtit
     *le, ytitle, graphtitle, subtitle, unitnum, type, histxmin, axistyp
     *e)

      implicit none
      integer max, idebug
      parameter (max = 300)
      real*8 x(max),y(max),erry(max),ymin,histxmin,erryold,
     *       smallest,fudge
      integer npts,curvenum, count,unitnum,type,axistype
      integer titlelength,sublength,xaxislength,yaxislength,serieslength
      integer logx, logy, logdy
      character*80 subtitle
      character*60 graphtitle,xtitle,ytitle,seriestitle
      character*10 index
      character*3 indexnum
      logical testfile, allpos
      fudge = 1.E-10
      idebug = 0
      if ((idebug .eq. 1)) then
        write(6,'('' Entering xvgrplot SID 1.22 ''/)')
        write(6,'('' Curve'',i3,'' to go to unit'',i3)')curvenum,unitnum
        write(6,'(a60)') seriestitle
        write(6,'(a60)') xtitle
        write(6,'(a60)') ytitle
        write(6,'(a60)') graphtitle
        write(6,'(a80)') subtitle
      end if
      inquire(UNIT = unitnum,OPENED=testfile)
      if ((.not.testfile)) then
        write(6,1870) unitnum
1870    FORMAT (//'  ---------Error in subroutine xvgrplot---------' ,/'
     *   Unit specified (',I2,') is not open.' ,/'   Unit must be opened
     * before using subroutine.' ,/'   Data not written to file.' ,/'  -
     *---------------------------------------------'//)
        return
      end if
      if ((graphtitle .eq. ' ')) then
        graphtitle = 'Untitled Graph - No title specified in subroutine'
      end if
      if ((xtitle .eq. ' ')) then
        xtitle = 'X-axis not titled in subroutine'
      end if
      if ((ytitle .eq. ' ')) then
        ytitle = 'Y-axis not titled in subroutine'
      end if
      if ((seriestitle .eq. ' ')) then
        seriestitle = 'series # '
        index = '0123456789'
        indexnum = index(curvenum+1:curvenum+1)
        seriestitle(9:9) = indexnum
      end if
      titlelength = 61
      sublength = 61
      xaxislength = 61
      yaxislength = 61
      serieslenGTH = 61
1881  continue
        titlelength = titlelength - 1
        if(((graphtitle(titlelength:titlelength) .ne. ' ')))go to1882
      go to 1881
1882  continue
1891  continue
        sublength = sublength - 1
        if(((subtitle(sublength:sublength) .ne. ' ')))go to1892
      go to 1891
1892  continue
1901  continue
        xaxislength = xaxislength - 1
        if(((xtitle(xaxislength:xaxislength) .ne. ' ')))go to1902
      go to 1901
1902  continue
1911  continue
        yaxislength = yaxislength - 1
        if(((ytitle(yaxislength:yaxislength) .ne. ' ')))go to1912
      go to 1911
1912  continue
1921  continue
        serieslength = serieslength - 1
        if(((seriestitle(serieslength:serieslength) .ne. ' ')))go to1922
      go to 1921
1922  continue
      if (( idebug .eq. 1)) then
        write(6,1930)serieslength,yaxislength,xaxislength,sublength,titl
     *  elength
1930    FORMAT(' serieslength,yaxislength,xaxislength,sublength,titlelen
     *gth'/ 5i5)
      end if
      logx = 0
      logy = 0
      logdy = 0
      allpos=.true.
      if (( x(1).eq.0.0 )) then
        smallest = 0.1
      else
        smallest=x(1)
      end if
        do count=1,npts
        if (((x(count) .lt. smallest) .and. (x(count).ne.0.))) then
          smallest=x(count)
        end if
        if (((y(count) .lt. smallest) .and. (y(count).ne.0.))) then
          smallest=y(count)
        end if
        if (((x(count) .lt. 0.).or.(y(count) .lt. 0.))) then
          allpos=.false.
        end if
      end do
      if ((allpos)) then
        do count=1,npts
          if ((x(count).eq.0.)) then
            x(count)=smallest*fudge
          end if
          if ((y(count).eq.0.)) then
            y(count)=smallest*fudge
          end if
        end do
      end if
      if ((axistype .gt. 0)) then
        do count=1,npts
          if ((x(count) .le. 0.)) then
            logx = 1
          end if
          if ((y(count) .le. 0.)) then
            logy = 1
          end if
          if (((y(count)-erry(count)) .le. 0.)) then
            logdy = 1
          end if
        end do
      end if
      if ((curvenum .eq. 0)) then
        if ((axistype .eq. 0)) then
          write(unitnum,1970) 'xy'
        else if((axistype .eq. 1)) then
          write(unitnum,1970) 'logy'
          write(unitnum,1980)
        else if((axistype .eq. 2)) then
          write(unitnum,1970) 'logx'
          write(unitnum,1980)
        else if((axistype .eq. 3)) then
          write(unitnum,1970) 'logxy'
          write(unitnum,1980)
          write(unitnum,1990)
        else
          write(6,2000) axistype
2000      FORMAT (//'  ------------Error in subroutine xvgrplot---------
     *--' ,/'   AXISTYPE specified (',I2,') is not a valid option.' ,/' 
     * ----------------------------------------------'//)
          return
        end if
1970    FORMAT ('@g0 type ',A,' ')
1980    FORMAT ('@    xaxis  ticklabel format exponential')
1990    FORMAT ('@    yaxis  ticklabel format exponential')
        write(unitnum,2010) graphtitle(1:titlelength) ,subtitle(1:sublen
     *  gth) ,xtitle(1:xaxislength) ,ytitle(1:yaxislength)
2010    FORMAT ('@    title "',A,'"'/ ,'@    subtitle "',A,'"'/ ,'@    l
     *egend on'/ ,'@    legend x1 0.6'/ ,'@    legend y1 0.75'/ ,'@    v
     *iew xmin 0.250000'/ ,'@    xaxis  label "',A,'"'/ ,'@    yaxis  la
     *bel "',A,'"')
      end if
      if ((axistype .eq. 1 .and. logy .eq. 1)) then
        write(unitnum,1970) 'xy'
        write(6,2020)
2020    FORMAT (/' ----------WARNING from subroutine xvgrplot---------' 
     *  ,/'  Log scale requested for Y axis when one or more   ' ,/'  Y 
     *data points are 0 or negative.                  ' ,//'  Y axis sca
     *le changed to linear.                   ' ,/' --------------------
     *-------------------------------'/)
      end if
      if ((axistype .eq. 2 .and. logx .eq. 1)) then
        write(unitnum,1970) 'xy'
        write(6,2030)
2030    FORMAT (/' ----------WARNING from subroutine xvgrplot---------' 
     *  ,/'  Log scale requested for X axis when one or more   ' ,/'  X 
     *data points are 0 or negative.                  ' ,//'  X axis sca
     *le changed to linear.                   ' ,/' --------------------
     *-------------------------------'/)
      end if
      if ((axistype .eq. 3 .and. (logx .eq. 1 .or. logy .eq. 1))) then
        if ((logx .eq. 1 .and. logy .eq. 1)) then
          write(unitnum,1970) 'xy'
          write(6,2040)
2040      FORMAT (/' ----------WARNING from subroutine xvgrplot---------
     *' ,/'  Log scale requested for X axis and Y axis when    ' ,/'  on
     *e or more X and Y data points are 0 or negative.' ,//'  X and Y ax
     *es scales changed to linear.            ' ,/' --------------------
     *-------------------------------'/)
        else if((logx .eq. 1)) then
          write(unitnum,1970) 'logy'
          write(6,2030)
        else
          write(unitnum,1970) 'logx'
          write(6,2020)
        end if
      end if
      if ((logdy .eq. 1 .and. logy .ne. 1 .and. (axistype .eq. 3 .or. ax
     *istype .eq. 1))) then
        write(6,2050)
2050    FORMAT (/' ------------WARNING from subroutine xvgrplot---------
     *--' ,/'  Log scale requested for Y axis, and Y value less      ' ,
     *  /'  error gives 0 or negative value.                      ' ,//'
     *  Error adjusted to aviod negavite values on log scale. ' ,/' ----
     *---------------------------------------------------'/)
        do count=1,npts
          if ((y(count)-erry(count) .le. 0.)) then
            erryold = erry(count)
            erry(count) = y(count)-1.e-5
            write(6,2070) count,erryold,erry(count)
2070        FORMAT (/'  Error adjusted on point #',I2,' from',1PE10.3, '
     * to' /'        ',1PE10.3,'.')
          end if
        end do
        write(6,2080)
2080    FORMAT (/' -----------------------------------------------------
     *--'/)
      end if
      write(unitnum,2090) curvenum,seriestitle(1:serieslength)
2090  FORMAT ('@    legend string ',I2,' "',A,'"')
      if ((type .eq. 0)) then
        do count=1,npts
          if ((erry(count) .ne. 0)) then
            goto 2110
          end if
        end do
        write(unitnum,2120)
2120    FORMAT ('@TYPE xy')
        if ((curvenum .lt. 10)) then
          write(unitnum,2130) curvenum
          if ((curvenum .eq. 9)) then
            write(unitnum,2140) curvenum, curvenum+1
          else
            write(unitnum,2150) curvenum, curvenum+1
          end if
        else
          write(unitnum,2160) curvenum
          write(unitnum,2170) curvenum, curvenum+1
        end if
2130    FORMAT ('@    s',I1,' errorbar length 0.000000')
2160    FORMAT ('@    s',I2,' errorbar length 0.000000')
2140    FORMAT ('@    s',I1,' symbol color ',I2)
2150    FORMAT ('@    s',I1,' symbol color ',I1)
2170    FORMAT ('@    s',I2,' symbol color ',I2)
        do count=1,npts
          write(unitnum,2190) x(count),y(count)
        end do
2190    FORMAT (1pe15.4,1pe15.4)
        goto 2200
2110    continue
        write(unitnum,2210)
2210    FORMAT ('@TYPE xydy')
        if ((curvenum .LT. 10)) then
          write(unitnum,2130) curvenum
          if ((curvenum .EQ. 9)) then
            write(unitnum,2140) curvenum, curvenum+1
          else
            write(unitnum,2150) curvenum, curvenum+1
          end if
        else
          write(unitnum,2160) curvenum
          write(unitnum,2170) curvenum, curvenum+1
        end if
        do count=1,npts
          write(unitnum,2230) x(count),y(count),erry(count)
        end do
2230    FORMAT (1pe15.4,1pe15.4,1pe15.4)
2200    continue
      else
        ymin = abs(1.e5 * y(1))
        do count=1,npts
          if ((abs(y(count)) .lt. ymin)) then
            ymin = abs(y(count))
          end if
        end do
        ymin = sign(1.d0,y(1)) * 1.e-5 * ymin
        y(npts+1) = ymin
        if (((axistype .eq. 2 .or. axistype .eq. 3) .and. histxmin .eq.
     *  0)) then
          if ((x(1) .eq. (x(2)-x(1)))) then
            histxmin = x(1)-0.5*(x(2)-x(1))
          else
            histxmin = x(1)-(x(2)-x(1))
          end if
          write(6,2250) histxmin
2250      FORMAT (/' ---------WARNING from subroutine xvgrplot--------'
     *    ,/'  Minimum bin for X specified as 0 with log scale  ' ,/'  o
     *n X axis.  Minimum X bin set to ',1pE10.3,'.' ,/' ----------------
     *---------------------------------'/)
        end if
        do count=1,npts
          if ((erry(count) .ne. 0.)) then
            goto 2270
          end if
        end do
        if ((curvenum .lt. 10)) then
          write(unitnum,2130) curvenum
          if ((curvenum .eq. 9)) then
            write(unitnum,2140) curvenum, curvenum+1
          else
            write(unitnum,2150) curvenum, curvenum+1
          end if
        else
          write(unitnum,2160) curvenum
          write(unitnum,2170) curvenum, curvenum+1
        end if
        write(unitnum,2120)
        write(unitnum,2190) histxmin,ymin
        write(unitnum,2190) histxmin,y(1)
        do count=1,npts
          write(unitnum,2190) x(count),y(count)
          write(unitnum,2190) x(count),y(count+1)
        end do
        goto 2290
2270    continue
        erry(npts+1) = 0.0
        if ((curvenum .lt. 10)) then
          write(unitnum,2130) curvenum
          if ((curvenum .eq. 9)) then
            write(unitnum,2140) curvenum, curvenum+1
          else
            write(unitnum,2150) curvenum, curvenum+1
          end if
        else
          write(unitnum,2160) curvenum
          write(unitnum,2170) curvenum, curvenum+1
        end if
        write(unitnum,2210)
        if ((histxmin .eq. 0.0)) then
          histxmin = smallest*fudge
        end if
        write(unitnum,2230) histxmin, ymin, 0.
        write(unitnum,2230) histxmin, y(1), 0.
        write(unitnum,2230) (x(1)+histxmin)/2., y(1), erry(1)
        do count=1,npts
          write (unitnum,2230) x(count),y(count),0.
          write (unitnum,2230) x(count),y(count+1),0.
          if ((count .lt. npts)) then
            write (unitnum,2230) (x(count)+x(count+1))/2.,y(count+1),err
     *      y(count+1)
          end if
        end do
2290    continue
      end if
      write(unitnum,'(''&'')')
      return
      end
