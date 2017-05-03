!***********************************************************************
!                                                                       
!                     **************                                    
!                     *            *                                    
!                     *  ucbend.f  *                                    
!                     *            *                                    
!                     **************                                    
!                                                                       
! An EGS5 user code which treats charged particle transport in a 
! magnetic field.  Simulation of An RF cavity.
!
!  For SLAC-R-730/KEK Report 2005-8:  Example of cahrged particle
!      transport in a magnetic foeld.
!
!  The following units are used: unit 6 for output           
!***********************************************************************
!23456789|123456789|123456789|123456789|123456789|123456789|123456789|12
!-----------------------------------------------------------------------
!------------------------------- main code -----------------------------
!-----------------------------------------------------------------------

!-----------------------------------------------------------------------
! Step 1: Initialization
!-----------------------------------------------------------------------

      implicit none

!     ------------
!     EGS5 COMMONs
!     ------------
      include 'include/egs5_h.f'                ! Main EGS "header" file

      include 'include/egs5_bounds.f'  ! bounds contains Ecut and pcut
      include 'include/egs5_edge.f'    ! edge contains iedgfl
      include 'include/egs5_epcont.f'  ! epcont contains iausfl
      include 'include/egs5_media.f'   ! media contains the array media
      include 'include/egs5_misc.f'    ! misc contains med
      include 'include/egs5_thresh.f'  ! thresh contains ae and ap
      include 'include/egs5_uphiot.f'
      include 'include/egs5_useful.f'  ! useful contains RM
      include 'include/egs5_usersc.f'  ! usersc contains emaxe
      include 'include/randomm.f'

!     ----------------------
!     Auxiliary-code COMMONs
!     ----------------------
      include 'auxcommons/aux_h.f'   ! Auxiliary-code "header" file

      include 'auxcommons/etaly1.f'
      include 'auxcommons/instuf.f'
      include 'auxcommons/lines.f'
      include 'auxcommons/nfac.f'
      include 'auxcommons/ntaly1.f'
      include 'auxcommons/pladta.f'

      common/passit/bfield,ifield,nregd,ncup
      real*8 bfield
      integer ifield,nregd,ncup

      common/plotcon/iplot,nplot,ibatch
      integer iplot,nplot,ibatch

      real*8 ein,ekin,totke,wtin,availe
      real*8 dist,tlead,tvac,twind,wgap,xiamp
      integer i,ibtch,idinc,j,jevnt,nbatch,nevnt,nplan,nplanx,
     *        nplany,nplanz,nregcv
      character*24 medarr(3)

!     ----------
!     Open files
!     ----------
!     open(UNIT= 6,FILE='egs5job.out',STATUS='unknown')
      open(39,FILE='egs5job.pic',STATUS='unknown')

!-----------------------------------------------
!     Define pict data mode.
!-----------------------------------------------
!     npreci 1: for PICT32
!            2: for CGview
      npreci=2
!     ====================
      call counters_out(0)
!     ====================

!-----------------------------------------------------------------------
! Step 2: pegs5-call
!-----------------------------------------------------------------------
!     ==============
      call block_set                 ! Initialize some general variables
!     ==============

!     ---------------------------------
!     define media before calling PEGS5
!     ---------------------------------
      nmed=3
      medarr(1)='CU                      '
      medarr(2)='PB                      '
      medarr(3)='AIR AT NTP              '

      do j=1,nmed
        do i=1,24
          media(i,j)=medarr(j)(i:i)
        end do
      end do  

      chard(1) = 0.015*2.54  !  optional, but recommended to invoke
      chard(2) = 5.08d0      !  automatic step-size control
      chard(3) = 10.8d0

!     -------------------------------
!     Run PEGS5 before calling HATCH
!     -------------------------------
      write(6,100)
100   FORMAT(' PEGS5-call comes next'/)

!     ==========
      call pegs5
!     ==========

!-----------------------------------------------------------------------
! Step 3: Pre-hatch-call-initialization
!-----------------------------------------------------------------------
      nreg=12
      nregd=10
      nplan=11
      nplanz=6
      nplanx=5
      nplany=0
      
      do i=1,12
        med(i)=0
      end do
      med(2)=1
      do i=3,5
        med(i)=3
      end do
      med(7)=3
      med(6)=2
      med(8)=2

!     --------------------------------------------------------
!     Random number seeds.  Must be defined before call hatch
!     or defaults will be used.  inseed (1- 2^31)
!     --------------------------------------------------------
      luxlev=1
      inseed=1
      write(6,120) inseed
120   FORMAT(/,' inseed=',I12,5X,
     *         ' (seed for generating unique sequences of Ranlux)')

!     =============
      call rluxinit   ! Initialize the Ranlux random-number generator
!     =============

!-----------------------------------------------------------------------
! Step 4:  Determination-of-incident-particle-parameters
!-----------------------------------------------------------------------
      iqin=-1            ! Incident electrons

      ein=8.5+RM            ! 8.5 MeV
!     ein=3.5+RM            ! 3.5 MeV
      ekin=ein+iqin*RM
      availe=ekin
      xin=0.0            ! Incident at origin
      yin=0.0
      zin=0.0
      uin=0.0            ! Moving along z axis
      vin=0.0
      win=1.0
      irin=2             ! Starts in region 2, could be 1
      wtin=1.0           ! Weight = 1 since no variance reduction used
      idinc=-1

!-----------------------------------------------------------------------
! Step 5:   hatch-call
!-----------------------------------------------------------------------
! Total energy of incident source particle must be defined before hatch
! Define posible maximum total energy of electron before hatch
      if (iqin.ne.0) then
        emaxe = ein              ! charged particle
      else 
        emaxe = ein + RM         ! photon
      end if

      write(6,130)
130   format(/' Start ucbend'/' Call hatch to get cross-section data')

!     ------------------------------
!     Open files (before HATCH call)
!     ------------------------------
      open(UNIT=KMPI,FILE='pgs5job.pegs5dat',STATUS='old')
      open(UNIT=KMPO,FILE='egs5job.dummy',STATUS='unknown')

      write(6,140)
140   FORMAT(/,' HATCH-call comes next',/)

!     ==========
      call hatch
!     ==========

!     ------------------------------
!     Close files (after HATCH call)
!     ------------------------------
      close(UNIT=KMPI)
      close(UNIT=KMPO)

      write(6,150)
150   format(' Quantities associated with each media:',/)
      do j=1,nmed
        write(6,160) (media(i,j),i=1,24)
160     format(/,1X,24A1)
        write(6,170) rhom(j),rlcm(j)
170     format(5X,' Rho=',G15.7,' g/cm**3     rlc=', G15.7,' cm')
        write(6,180) ae(j),ue(j)
180     format(5X,' AE=',G15.7,' MeV    UE=',G15.7,' MeV')
        write(6,190) ap(j),up(j)
190     format(5X,' AP=',G15.7,' MeV    UP=',G15.7,' MeV')
      end do

!-----------------------------------------------------------------------
! Step 6:  Initialization-for-howfar
!-----------------------------------------------------------------------
      twind=0.015*2.54
      tlead=2.0*2.54
      wgap=1.5
      dist=10.8
      tvac=2.0
      do j=1,nplan
        do i=1,3
          pcoord(i,j)=0.0
          pnorm(i,j)=0.0
        end do
      end do
      do j=1,nplanz
        pnorm(3,j)=1
      end do
      pcoord(3,2)=twind
      pcoord(3,3)=twind + dist
      pcoord(3,4)=twind + 2.0*dist - wgap/2
      pcoord(3,5)=twind + 2.0*dist + wgap/2
      pcoord(3,6)=twind + 3.0*dist
      do j=1,nplanx
        pnorm(1,nplanz+j)=1.0
      end do
      pcoord(1,7)=dist
      pcoord(1,8)=2.0*dist
      pcoord(1,9)=2.0*dist + tlead
      pcoord(1,10)=pcoord(1,9) + tvac
      pcoord(1,11)=-dist
      write(6,200)
200   format(' Ppcoord and pnorm values for each j-plane (i=1,3):',/)
      do j=1,nplan
        write(6,210) j,(pcoord(i,j),i=1,3),(pnorm(i,j),i=1,3)
210     format(I5,6G15.7)
      end do

!     --------------------------------------------
!     Write plane information for geometry drawing
!     --------------------------------------------
      if(npreci.eq.1) then 
!     ------------------------
!     Set parameter for PICT32
!     ------------------------
        xmin=pcoord(1,11)-10.0
        xmax=pcoord(1,10)+10.0
        ymin=-10.0
        ymax=10.0
        zmin=-10.0+pcoord(3,1)
        zmax=pcoord(3,6)+10.0

        call geomout(0,11)
        fnorm=dmax1(xmax-xmin+2,ymax-ymin+2,zmax-zmin)
        write(39,1200) xmin,xmax,ymin,ymax,zmin,zmax,fnorm
1200    FORMAT(7E10.3)
      else
        write(39,1210)
1210    FORMAT('GSTA')
        write(39,1220)
1220    FORMAT('SLAB')
        nplany=2
        write(39,1230) nplanx,nplany,nplanz
1230    FORMAT(3I6)
        write(39,1240) (pcoord(1,6+j),j=1,5)
1240    FORMAT(4F15.4)
        write(39,1240) -10.0,10.0
        write(39,1240) (pcoord(3,j),j=1,6)
        write(39,1250)
1250    FORMAT('GEND')
        
        write(39,1260)
1260    FORMAT('MSTA')
        nregcv=(nplanx-1)*(nplany-1)*(nplanz-1)+3
        if(nreg.ge.nregcv) then
          write(39,1270) nreg
1270      FORMAT(I4)
          write(39,1280) (med(i),i=1,nreg)
1280      FORMAT(15I4)
        else
          do i=nreg+1,nregcv
            med(i)=0
          end do
          write(39,1270) nregcv
          write(39,1280) (med(i),i=1,nregcv)
        end if
        write(39,1290)
1290    FORMAT('MEND')
      end if

!-----------------------------------------------------------------------
! Step 7:  Initialization-for-ausgab
!-----------------------------------------------------------------------
      iplot=1
      ifield=1
      xiamp=65.0
      bfield=ifield*(3.54/92.0)*xiamp ! 2.5 
       bfield=2.6
!      bfield=1.0
!     bfield=0.0
      call ecnsv1(0,nreg,totke)
      call ntally(0,nreg)
      ncount=0
      ilines=0
      nbatch=1
      nevnt=100
      ibatch=0
      nwrite=10
      nplot=10
      nlines=100

!-----------------------------------------------------------------------
! Step 8:  Shower-call
!-----------------------------------------------------------------------
      do ibtch=1,nbatch
        if (ncount.le.nwrite.and.ilines.le.nlines) then
          write(6,220) ein,xin,yin,zin,uin,vin,win,iqin,irin,idinc
220       format(7G15.7,3I5)
          ilines=ilines+1
        end if
        ibatch=ibatch + 1
        ncup=0 
        if (iplot.eq.1.and.ibatch.le.nplot) then
          write(39,230) ibatch
230       format('0',I5)
        end if
        do jevnt=1,nevnt
          call shower(iqin,ein,xin,yin,zin,uin,vin,win,irin,wtin)
          ncount=ncount + 1
        end do
        if (iplot.eq.1.and.ibatch.le.nplot) then
          write(39,240)
240       format('9')
          call plotxyz(99,0,0,0.D0,0.D0,0.D0,0.D0,0,0.D0) 
          close(UNIT=39,status='keep')
        end if
        write(6,250) ibtch,ncup,nevnt,ncount
250     format(' IBTCH=',I5,3X,'NCUP=',I6,3X,'NEVNT=',I6,3X,
     *         'NCOUNT=',I10)
      end do
      totke=ncount*availe
      write(6,260) ibatch,nbatch,nevnt,ncount
260   format('1',I10,' Batches out of ',I10,' (requested) were run (with
     * ', I10,' events per batch)',/,' for a total of ',I10, ' incident 
     *electrons')
      write(6,270) ekin,xiamp,bfield
270   formaT(//,' EKIN=',G15.5,' MeV',/,' XIAMP=',G15.5,' amps',/, ' BFI 
     *ELD=',G15.5,' kg')
      write(6,280) availe,totke
280   format(//,' Available k.e.=', G15.5,' MeV',/
     *      ,' TOTKE=',E15.5,' MeV',//)
      call ecnsv1(1,nreg,totke)
      call ntally(1,nreg)
      stop
      end
!-------------------------last line of main code------------------------

!-------------------------------ausgab.f--------------------------------
!-----------------------------------------------------------------------
!23456789|123456789|123456789|123456789|123456789|123456789|123456789|12
! ----------------------------------------------------------------------
! Required subroutine for use with the EGS5 Code System
! ----------------------------------------------------------------------
!***********************************************************************
      subroutine ausgab(iarg)

      implicit none

      include 'include/egs5_h.f'                ! Main EGS "header" file

      include 'include/egs5_epcont.f'  ! epcont contains iausfl
      include 'include/egs5_misc.f'    ! misc contains med
      include 'include/egs5_stack.f'     ! stack contains x, y, z, u, v,
                                         ! w, ir and np
      include 'auxcommons/aux_h.f'   ! Auxiliary-code "header" file

      include 'auxcommons/etaly1.f'        ! Auxiliary-code COMMONs
      include 'auxcommons/georz.f'
      include 'auxcommons/lines.f'
      include 'auxcommons/ntaly1.f'

      integer                                                ! Arguments
     * iarg

      common/passit/bfield,ifield,nregd,ncup
      real*8 bfield
      integer ifield,nregd,ncup
      common/plotcon/iplot,nplot,ibatch
      integer iplot,nplot,ibatch

      integer irl                                   ! Local variable

      irl=ir(np)
      esum(iq(np)+2,irl,iarg+1)=esum(iq(np)+2,irl,iarg+1) + edep
      nsum(iq(np)+2,irl,iarg+1)=nsum(iq(np)+2,irl,iarg+1) + 1
      if (ncount.le.nwrite.and.ilines.le.nlines) then
        write(6,100) e(np),x(np),y(np),z(np),u(np),v(np),w(np),
     *               iq(np),irl,iarg
100     format(7G15.7,3I5)
        ilines=ilines+1
      end if
      if (iarg.eq.3.and.iq(np).eq.-1.and.irl.eq.10) then
        ncup=ncup + 1
      end if
      if (iplot.eq.1.and.ibatch.le.nplot) then
        call plotxyz(iarg,np,iq(np),x(np),y(np),z(np),e(np),ir(np),
     *       wt(np))
      end if
      return
      end
!--------------------------last line of ausgab.f------------------------
!-------------------------------howfar.f--------------------------------
!-----------------------------------------------------------------------
!23456789|123456789|123456789|123456789|123456789|123456789|123456789|12
! ----------------------------------------------------------------------
! Required (geometry) subroutine for use with the EGS5 Code System
!***********************************************************************
      subroutine howfar

      implicit none

      include 'include/egs5_h.f'                ! Main EGS "header" file

      include 'include/egs5_epcont.f'    ! epcont contains irnew, ustep
                                         ! and idisc
      include 'include/egs5_stack.f'     ! stack contains x, y, z, u, v,
                                         ! w, ir and np
      include 'include/egs5_thresh.f'    ! thresh contains rmsq

!     ----------------------
!     Auxiliary-code COMMONs
!     ----------------------
      include 'auxcommons/aux_h.f'   ! Auxiliary-code "header" file

      include 'auxcommons/pladta.f'

      common/passit/bfield,ifield,nregd,ncup
      real*8 bfield
      integer ifield,nregd,ncup


      real*8 alpha,p,poverb,psq,rcurv,renorf,tval,     ! Local variables
     *       u0,ustep0,v0,w0,xf,yf,zf
!    *       u0,ustep0,ustepo,v0,w0,xf,yf,zf ! ustepo was removed on 19Apr2007.
      integer irl,ihit,irnxt

      irl=ir(np)
      if (irl.eq.1.or.irl.eq.6.or.irl.eq.8.or.irl.ge.nregd) then
        idisc=1
      else if (irl.eq.2) then
        call plan2p(2,3,1,1,1,-1)
        call plan2p(8,8,1,11,12,-1)
      else if (irl.eq.3) then
        call plane1(3,1,ihit,tval)
        if (ihit.eq.1) then
          call finval(tval,xf,yf,zf)
          if (xf.ge.pcoord(1,7)) then
            irnxt=5
          else
            irnxt=4
          end if
          call chgtr(tval,irnxt)
        else if (ihit.eq.0) then
          call plane1(2,-1,ihit,tval)
          if (ihit.eq.1) then
            call chgtr(tval,2)
          else
            write(6,1240)irl
1240        format(/,' Conflict in howfar with irl=',I3)
            stop
          end if
        end if
        call plan2p(8,8,1,11,12,-1)
      else if (irl.eq.4) then
        if (ifield.ne.0.and.iq(np).ne.0) then
          psq=e(np)*e(np)-RMSQ
          if (psq.lt.0.0) psq=1.0E-9
          p=sqrt(psq)
          poverb=p/(bfield*0.3)
          u0=u(np)
          v0=v(np)
          w0=w(np)
          rcurv=-iq(np)*poverb
          if (ustep.gt.0.1) then
            ustep=dmin1(ustep,0.1*abs(poverb))
          end if
            ustep0=ustep
            irnew=irl
            alpha=ustep/rcurv
            renorf=1./sqrt(1.+alpha*alpha)
            w(np)=(w0-u0*alpha)*renorf
            u(np)=(u0+w0*alpha)*renorf
            v(np)=v0
            call plan2p(6,11,1,3,3,-1)
            call plan2p(7,5,1,11,12,-1)
        else
          call plan2p(6,11,1,3,3,-1)
          call plan2p(7,5,1,11,12,-1)
        end if
      else if (irl.eq.5) then
        call plane1(8,1,ihit,tval)
        if (ihit.eq.1) then
          call finval(tval,xf,yf,zf)
          if (zf.gt.pcoord(3,5)) then
            irnxt=6
          else if (zf.gt.pcoord(3,4)) then
            irnxt=7
          else
            irnxt=8
          end if
          call chgtr(tval,irnxt)
        else if (ihit.eq.0) then
          call plane1(7,-1,ihit,tval)
          if (ihit.eq.1) then
            call chgtr(tval,4)
          else
            write(6,1260) irl
1260        format(/,' Conflict in howfar with irl=',I3)
            stop
          end if
        end if
        call plan2p(6,11,1,3,3,-1)
      else if (irl.eq.6) then
        call plan2p(6,11,1,5,7,-1)
        call plan2p(9,9,1,8,5,-1)
      else if (irl.eq.7) then
        call plan2p(5,6,1,4,8,-1)
        call plan2p(9,9,1,8,5,-1)
      else if (irl.eq.8) then
        call plane1(9,1,ihit,tval)
        if (ihit.eq.1) then
          call chgtr(tval,9)
        else if (ihit.eq.0) then
          call plane1(8,-1,ihit,tval) 
          if (ihit.eq.1) then
            call finval(tval,xf,yf,zf)
            if (zf.gt.pcoord(3,3)) then
              irnxt=5
            else if((zf.gt.pcoord(3,2))) then
              irnxt=3
            else
              irnxt=2
            end if
            call chgtr(tval,irnxt)
          else
            write(6,1270) irl
1270        format(/,' Conflict in howfar with irl=',I3)
            stop
          end if
        end if
        call plan2p(4,7,1,1,1,-1)
      else if (irl.eq.9) then
        call plane1(10,1,ihit,tval)
        if (ihit.eq.1) then
          call chgtr(tval,10)
        else if (ihit.eq.0) then
          call plane1(9,-1,ihit,tval)
          if (ihit.eq.1) then
            call finval(tval,xf,yf,zf)
            if (zf.gt.pcoord(3,5)) then
              irnxt=6
            else if (zf.gt.pcoord(3,4)) then
              irnxt=7
            else
              irnxt=8
            end if
            call chgtr(tval,irnxt)
          else
            write(6,1280) irl
1280        format(/,' Conflict in howfar with irl=',I3)
            stop
          end if
        end if
        call plan2p(6,11,1,1,1,-1)
      end if
      return
      end
!--------------------------last line of howfar.f------------------------
