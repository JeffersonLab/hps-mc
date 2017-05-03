!***********************************************************************
!**************************                                            *
!*** u c t e s t s r  *****             EGS5.0 USER CODE - 050719-1330 *
!**************************                                            *
!***********************************************************************
!* This is a User Code based on UCTESTSR.MOR from EGS4                 *
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
      include 'include/egs5_usersc.f'

      real*8                                    !  Local variables
     *   ein, precn
      integer
     *   i, j, iqin, isub, irnflg, nbins
      character*24 medarr(1)

!     ----------
!     Open files
!     ----------
      open(UNIT= 6,FILE='egs5job.out',STATUS='unknown')

!     ==============
      call block_set                 ! Initialize some general variables
!     ==============

!-----------------------------------------------------------------------
! Pre-pegs-call-initialization
!-----------------------------------------------------------------------

      nmed=1
      medarr(1)='AL                      '

      do j=1,nmed
        do i=1,24
          media(i,j)=medarr(j)(i:i)
        end do
      end do

      write(6,100)
100   FORMAT(' PEGS5-call comes next'/)

!     ==========
      call pegs5
!     ==========

!-----------------------------------------------------------------------
! Pre-hatch-call-initialization
!-----------------------------------------------------------------------

      nreg=1
      med(1)=1

! Define initial variables for 10 MeV photons
      iqin=0         !     Incident charge - photons
      ein=10.d0      !     10 MeV kinetic energy

! Maximum total energy of an electron for this problem must be
! defined before HATCH call
      emaxe = ein + RM

!     ------------------------------
!     Open files (before HATCH call)
!     ------------------------------
      open(UNIT=KMPI,FILE='pgs5job.pegs5dat',STATUS='old')
      open(UNIT=KMPO,FILE='egs5job.dummy',STATUS='unknown')

      write(6,200)
200   FORMAT(/,' HATCH-call comes next',/)

!     ==========
      call hatch
!     ==========

      isub=2
      irnflg=0
      precn=1000000.
      nbins=20

!     ====================================================
      call select(isub,ein,iqin,med(1),irnflg,precn,nbins)
!     ====================================================

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

!-------------------------------select----------------------------------
!-----------------------------------------------------------------------
!23456789|123456789|123456789|123456789|123456789|123456789|123456789|12
! ----------------------------------------------------------------------
! User subroutine for testing EGS sampling
! ----------------------------------------------------------------------
!***********************************************************************
!
!
!***********************************************************************
      subroutine select(isub,ei,iqi,imed,irnflg,precn,nbins)

      implicit none

      real*8 ei, precn                          ! Arguments
      integer isub, iqi, imed, irnflg, nbins

!     ------------
!     EGS5 COMMONs
!     ------------
      include 'include/egs5_h.f'                ! Main EGS "header" file

      include 'include/egs5_elecin.f'
      include 'include/egs5_photin.f'
      include 'include/egs5_thresh.f'
      include 'include/egs5_uphiot.f'
      include 'include/egs5_useful.f'

      real*8                                    ! Local variables
     *       eln, rnlo, rnhi, pinc
      integer meln, leln

      external pair,compt,brems,moller,bhabha,  ! Functions
     *         annih,alin,amold,amoli,mscat
      intrinsic dlog,dexp
      real*8 pair,compt,brems,moller,bhabha,
     *       annih,alin,amold,amoli,mscat

      medium=imed
      if (isub.le.2) then
        eln=dlog(ei)
        meln=ge1(medium)*eln+ge0(medium)
!        leln=mpgem(meln,medium)
        leln=meln
      else
        eln=dlog(ei-RM)
        meln=eke1(medium)*eln+eke0(medium)
!        leln=mpeem(meln,medium)
        leln=meln
      end if

      if(isub .eq. 1) then
        rnlo=0.0
        rnhi=gbr11(leln,medium)*eln+gbr10(leln,medium)
        call sample(isub,pair,alin,alin,RM,ei-RM, 
     *              ei,iqi,rnlo,rnhi,irnflg,precn,nbins)

      else if(isub .eq. 2) then
    
        rnlo=gbr11(leln,medium)*eln+gbr10(leln,medium)
        rnhi=gbr21(leln,medium)*eln+gbr20(leln,medium)
        call sample(isub,compt,dlog,dexp,ei/(1.+2.*ei/RM),ei, 
     *              ei,iqi,rnlo,rnhi,irnflg,precn,nbins)

      else if (isub .eq. 3) then
    
        rnlo=0.D0
        if (iqi.lt.0) then
          rnhi=ebr11(leln,medium)*eln+ebr10(leln,medium)
        else
          rnhi=pbr11(leln,medium)*eln+pbr10(leln,medium)
        end if
        call sample(isub,brems,dlog,dexp,ap(medium),ei-RM, 
     *              ei,iqi,rnlo,rnhi,irnflg,precn,nbins)

      else if (isub .eq. 4) then
    
        rnlo=ebr11(leln,medium)*eln+ebr10(leln,medium)
        rnhi=1.0
        call sample(isub,moller,amold,amoli,ae(medium),RM+(ei-RM)/2.d0, 
     *              ei,iqi,rnlo,rnhi,irnflg,precn,nbins)

      else if(isub .eq. 5) then
    
        rnlo=pbr11(leln,medium)*eln+pbr10(leln,medium)
        rnhi=pbr21(leln,medium)*eln+pbr20(leln,medium)
        call sample(isub,bhabha,amold,amoli,ae(medium),ei, 
     *              ei,iqi,rnlo,rnhi,irnflg,precn,nbins)

      else if(isub .eq. 6) then
    
        rnlo=pbr21(leln,medium)*eln+pbr20(leln,medium)
        rnhi=1.0
        pinc=sqrt(ei**2-RM**2)
        call sample(isub,annih,dlog,dexp,(ei+RM)*RM/(ei+RM+pinc),
     *              (ei+RM)/2.d0,ei,iqi,rnlo,rnhi,irnflg,precn,nbins)

      else if(isub .eq. 7) then
    
        call sample(isub,mscat,alin,alin,0.,PI/5.d0, 
     *              ei,iqi,rnlo,rnhi,irnflg,precn,nbins)

      else

        write(6,1000) isub
1000    FORMAT(' illegal isub=',i10,/,'Aborting')
        stop

      endif

      return
      end
!-------------------------last line of select---------------------------

!-------------------------------sample----------------------------------
!-----------------------------------------------------------------------
!23456789|123456789|123456789|123456789|123456789|123456789|123456789|12
! ----------------------------------------------------------------------
! User subroutine for testing EGS sampling
! ----------------------------------------------------------------------
!***********************************************************************
!
!
!***********************************************************************

      subroutine sample(isub,fun,xdf,xdfi,elo,ehi,
     *                  ei,iqi,rnlo,rnhi, irnflg,p,n)

      implicit none

      real*8  fun, xdf,xdfi,elo,ehi, ei,rnlo,rnhi, p         ! Arguments
      integer isub, iqi, irnflg, n

!     ------------
!     EGS5 COMMONs
!     ------------
      include 'include/egs5_h.f'                ! Main EGS "header" file

      include 'include/egs5_epcont.f'
      include 'include/egs5_stack.f'
      include 'include/egs5_uphiot.f'

      real*8                                    ! Locals
     *       xi,yi,zi,ui,vi,wi,dneari,wti,xfmn,xfmx,ax,bx,scly,xv,re,
     *       rnnow,fx,xv1,xv2,omega,fxn
      integer iqf(2,6),name(12),namet(12,7),nmax,nh(500),b(100),xc,blnk,
     *        is,iri,ich,iprec,np1,np2,ibin,ntimes,nloop,imin,imax, icol
      logical prflg

      data nmax/500/,xc/'x'/,blnk/' '/
      data namet/'P','A','I','R',' ',' ',' ','E','+',' ',' ',' ','C','O'
     *,'M','P','T',' ','G',' ',' ',' ',' ',' ', 'B','R','E','M','S',' ',
     *' ','G',' ',' ',' ',' ','M','O','L','L','E','R',' ','E','L','O',
     *' ',' ', 'B','H','A','B','H','A',' ','E','-',' ',' ',' ','A','N',
     *'N','I','H',' ',' ','G',' ',' ',' ',' ','M','U','L','T',' ','S',
     *'C','A','T',' ',' ',' '/
      
      data xi/0./,yi/0./,zi/0./,ui/1./,vi/0./,wi/0./,dneari/0./,wti/1./
      data iri/1/
      data iqf/1, -1, 0,-1, 0, -1, -1, -1, -1, 1, 0, 0/

      iqf(2,3)=iqi

      do ich=1,12
        name(ich)=namet(ich,isub)
      end do

      prflg=p.lt.1.d0
      iprec=p
      np1=n+1
      np2=n+2
      do ibin=1,np2
        nh(ibin)=0
      end do

      ntimes=0
      xfmn=xdf(elo)
      xfmx=xdf(ehi)
      ax=n/(xfmx-xfmn)
      bx=2.0-xfmn*ax

1     continue
        if (prflg) then
          nloop=200
        else
          nloop=min0(200,iprec-ntimes)
        end if

        do is=1,nloop
          np=1
          e(1)=ei
          iq(1)=iqi
          u(1)=ui
          v(1)=vi
          w(1)=wi
          x(1)=xi
          y(1)=yi
          z(1)=zi
          ir(1)=iri
          wt(1)=wti
          dnear(1)=dneari
          latch(1)=latchi

          !  restrict random numbers to a given interval
          if (irnflg.ne.0) then
2           call randomset(rnnow)
            if((rnlo.le.rnnow).and.(rnnow.le.rnhi)) go to 3
            go to 2
          end if

         !========
3         call fun
         !========

          if (np.ne.2.and.isub.lt.7.or.np.ne.1.and.isub.eq.7)
     *                                              write(6,1000) np
          if (isub.lt.7) then
            if (iqf(1,isub).eq.iq(np)) then
              if (iqf(2,isub).ne.iq(1)) then
                write(6,1100) iq(1),iq(2)
                stop
              end if
              xv=e(2)
            else
              if((iqf(1,isub).ne.iq(1)).or.(iqf(2,isub).ne.iq(2))) then
                write(6,1100) iq(1),iq(2)
                stop
              endif
              xv=e(1)
            end if
          else
            xv=theta
          end if
          ibin=max0(1,min0(np2,int(ax*xdf(xv)+bx)))
          nh(ibin)=nh(ibin)+1

        end do

        ntimes=ntimes+nloop

        if (prflg) then
          imin=nh(2)
          do ibin=3,np1
            imin=min0(imin,nh(ibin))
          end do

          re=1./sqrt(float(max0(imin,1)))
          if (re.le.p) go to 4
        else
          if (ntimes.ge.iprec) go to 4
        end if
      
      go to 1         !  get more samples

4     continue        !  done

      imax=nh(1)
      do ibin=2,np2
        imax=max0(imax,nh(ibin))
      end do

      scly=100./float(imax)
      write(6,1200)ei,isub,name,ntimes,n,iqi,rnlo,rnhi,irnflg,tvstep, 
     *             (nh(ibin+1),ibin=1,n)
      write(6,1300)name,ntimes,ei,elo,ehi,p,n,iqi,rnlo,rnhi,irnflg
      do icol=1,100
        b(icol)=blnk
      end do

      do ibin=1,np2
        icol=max0(1,min0(100,int(scly*nh(ibin))+1))
        b(icol)=xc
        xv=0.0
        if (ibin.gt.1) xv=xdfi((ibin-bx)/ax)
        fx=nh(ibin)/float(ntimes)
        write(6,1400)xv,nh(ibin),fx,b
        b(icol)=blnk
      end do

      if (isub.eq.7) then
        write(6,1500)
        do ibin=2,np1
          xv1=xdfi((ibin-bx)/ax)
          xv2=xdfi((ibin+1-bx)/ax)
          omega=2*PI*(cos(xv1)-cos(xv2))*(180./PI)**2
          fx=nh(ibin)/float(ntimes)
          fxn=fx/omega
          xv1=xv1*180./PI
          xv2=xv2*180./PI
          write(6,1600)xv1,xv2,fx,fxn
        end do
      end if

1000  FORMAT(' NP error in SAMPLE, final np = ',i10)
1100  FORMAT(' IQ error in SAMPLE, iqs = ',2i3)
1200  FORMAT ('HPLT',/,' &INP EI=',F10.3,',ISUB=',I2,',&END' / ' TEST DA
     *TA FOR ROUTINE=',12A1,',#SAMPLES=',I10,',NBINS=',I5/ ' IQI=',I2,',
     *RNLO,RNHI=',2F12.8,',IRNFLG=',I2,',TVSTEP=', G15.7/(9I8))
1300  FORMAT(' Plot of test data for routine ',12a1,',#samples=',i10/ 1x
     *,'ei=',1pg10.5,',elo=',g12.5,'ehi=',g12.5,',req''d precn=', g9.2,'
     *,nbins=',i3/ ' iqi=',i2,',rnlo,rnhi=',2f12.8,',irnflg=',i2/ 3x,'ab
     *cissa',6x,' counts',3x,'f(x)   ....5...10...15...20..', '.25...30.
     *..35...40...45...50...55...60...65...70...75..', '.80...85...90...
     *95..100'/)
1400  FORMAT(1x,1pe14.5,i7,0pf10.6,100a1)
1500  FORMAT(' Multiple scattering table of fraction/sq.degree:' ,//)
1600  FORMAT(1x,g15.7,' to ',g15.7,' degrees',5x,'fraction=',g15.7,5
     *    x, 'fraction/sq.deg.=',g15.7)

      return
      end
!-----------------------last line of sample-----------------------------

!-------------------------------amold-----------------------------------
!-----------------------------------------------------------------------
!23456789|123456789|123456789|123456789|123456789|123456789|123456789|12
! ----------------------------------------------------------------------
! User subroutine for testing EGS sampling
! ----------------------------------------------------------------------
!***********************************************************************

      double precision function amold(x)

      implicit none

      real*8  x                                              ! Arguments

!     ------------
!     EGS5 COMMONs
!     ------------
      include 'include/egs5_h.f'                ! Main EGS "header" file

      include 'include/egs5_useful.f'           

      amold = 1.d0 / (x - RM)

      return
      end
!-----------------------last line of amold------------------------------

!-------------------------------amoli-----------------------------------
!-----------------------------------------------------------------------
!23456789|123456789|123456789|123456789|123456789|123456789|123456789|12
! ----------------------------------------------------------------------
! User subroutine for testing EGS sampling
! ----------------------------------------------------------------------
!***********************************************************************

      double precision function amoli(x)

      implicit none

      real*8  x                                              ! Arguments

!     ------------
!     EGS5 COMMONs
!     ------------
      include 'include/egs5_h.f'                ! Main EGS "header" file

      include 'include/egs5_useful.f'           

      amoli = 1.d0 / x + RM

      return
      end
!-----------------------last line of amoli------------------------------
