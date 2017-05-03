!***********************************************************************
!*****************************  KEK, High Energy Accelerator Research  *
!*****************************  Organization                           *
!*** u c s a m p c g *********                                         *
!*****************************     EGS5.0 USER CODE - 06 Feb 2009/1145 *
!***********************************************************************
!* This is a general User Code based on the cg geometry scheme.        *
!***********************************************************************
!                                                                      *
!***********************************************************************
!***********************************************************************
! The ucsampcg.f User Code requires a cg-input file only               *
! (e.g., ucsampcg.data).                                               *
! Input data for CG geometry must be written at the top of data-input  *
! file together with material assignment to each region.  Cg-data can  *
! be checked by CGview.                                                *
! Material for each region is assigned in user code.                   *
! Material assignment in CG data is ignored in this user code.         *
! Particle trajectory and geometry is output for egs5job.pic.          *
! egs5job.pic can be read in by Cgview.                                *
! The following shows the geometry.                                    *
!***********************************************************************
!                                                                      *
!                Y (X into page)                                       *
!                     ^                                                *
!                     |                                                *
!             2.5 +---+---------+                                      *
!                 |Vac|  Vac    |                                      *
!             1.5 |   +-----+   |                                      *
!                 |   |  Fe |   |                                      *
!     1 GeV       |   |     |   |                                      *
!      ===========+==>+-----+---+---------------------> Z              *
!     electron  -1.0  0    3.0  4.0                                    *
!                                                                      *
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

      include 'include/egs5_bounds.f'
      include 'include/egs5_edge.f'
      include 'include/egs5_media.f'
      include 'include/egs5_misc.f'
      include 'include/egs5_thresh.f'
      include 'include/egs5_useful.f'
      include 'include/egs5_usersc.f'
      include 'include/egs5_userxt.f'
      include 'include/randomm.f'

!     ----------------------
!     Auxiliary-code COMMONs
!     ----------------------
      include 'auxcommons/aux_h.f'   ! Auxiliary-code "header" file

      include 'auxcommons/lines.f'
      include 'auxcommons/nfac.f'

!     ------------------
!     CG related COMMONs
!     ------------------
      include 'auxcommons/geom_common.f' 
      integer irinn,ifti,ifto            

      common/totals/esum(MXREG),maxpict,i
      real*8 esum
      integer maxpict

      real*8 ei,ekin,etot,totke,xi,yi,zi,ui,vi,wi,wti   ! Arguments
      real tarray(2)

      real t0,t1,timecpu,tt,etime              ! Local variables
      integer i,idinc,iqi,iri,j,ncases
      character*24 medarr(2)

!     ----------
!     Open files
!     ----------
!----------------------------------------------------------------
!     Units 7-26 are used in pegs and closed.  It is better not
!     to use as output file. If they are used, they must be opened
!     after getcg etc. Unit for pict must be 39.
!----------------------------------------------------------------

      open(UNIT=4,FILE='egs5job.inp',STATUS='old')
      open(UNIT=6,FILE='egs5job.out',STATUS='unknown')
      open(UNIT=39,FILE='egs5job.pic',STATUS='unknown')

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
!     Define media before calling PEGS5
!     ---------------------------------

      nmed=2
      medarr(1)='FE-RAYLEIGH             '
      medarr(2)='AIR AT NTP              '

      do j=1,nmed
        do i=1,24
          media(i,j)=medarr(j)(i:i)
        end do
      end do  

      chard(1) = 3.0
      chard(2) = 3.0

!     ------------------------------
!     Run PEGS5 before calling HATCH
!     ------------------------------
      write(6,100)
100   FORMAT(' PEGS5-call comes next'/)

!     ==========
      call pegs5
!     ==========

!-----------------------------------------------------------------------
! Step 3: Pre-hatch-call-initialization
!-----------------------------------------------------------------------
!-----------------------------------------------
!     Initialize CG related parameters 
!-----------------------------------------------
      npreci=3     ! PICT data mode for CGView in free format

      ifti = 4     ! Input unit number for cg-data
      ifto = 39    ! Output unit number for PICT

      write(6,fmt="(' CG data')")
      call geomgt(ifti,6)  ! Read in CG data
      write(6,fmt="(' End of CG data',/)")

      if(npreci.eq.3) write(ifto,fmt="('CSTA-FREE-TIME')")
      if(npreci.eq.2) write(ifto,fmt="('CSTA-TIME')")

      rewind ifti
      call geomgt(ifti,ifto)! Dummy call to write geom info for ifto
      write(ifto,110)
110   FORMAT('CEND')

      nreg=izonin           ! Get nreg from cg input data

!     --------------------------
!     Set medium for each region
!     --------------------------
      med(1)=1
      med(2)=0
      read(ifti,*) (med(i),i=1,nreg)

!     --------------------------------
!     Set option flag for region 2-3
!     1: on, 0: off
!     --------------------------------

      do i=1,nreg
        ecut(i)=100.0       ! egs cut off energy for electrons
        pcut(i)=100.0       ! egs cut off energy for photons
        iphter(i) = 0       ! Switches for PE-angle sampling
        iedgfl(i) = 0       ! K & L-edge fluorescence
        iauger(i) = 0       ! K & L-Auger
        iraylr(i) = 0       ! Rayleigh scattering
        lpolar(i) = 0       ! Linearly-polarized photon scattering
        incohr(i) = 0       ! S/Z rejection
        iprofr(i) = 0       ! Doppler broadening
        impacr(i) = 0       ! Electron impact ionization
      end do

!     --------------------------------------------------------
!     Random number seeds.  Must be defined before call hatch
!     or defaults will be used.  inseed (1- 2^31)
!     --------------------------------------------------------
      luxlev = 1
      inseed=1

!     =============
      call rluxinit  ! Initialize the Ranlux random-number generator
!     =============

!-----------------------------------------------------------------------
! Step 4:  Determination-of-incident-particle-parameters
!-----------------------------------------------------------------------
      iqi=-1
      xi=0.0
      yi=0.0
      zi=0.0
      ui=0.0
      vi=0.0
      wi=1.0
      iri=0          ! Input 0 to use srzone for automatic region search
      wti=1.0
!
      ncases=1000
      idinc=-1
      ei=1000.D0
      ekin=ei+iqi*RM

!-----------------------------------------
!     Get source region from cg input data
!-----------------------------------------
!
          if(iri.le.0.or.iri.gt.nreg) then
            call srzone(xi,yi,zi,iqi+2,0,irinn)
            if(irinn.le.0.or.irinn.ge.nreg) then
              write(6,fmt="(' Stopped in MAIN. irinn = ',i5)")irinn
              stop
            end if
            call rstnxt(iqi+2,0,irinn)
          else
            irinn=iri
          end if

!-----------------------------------------------------------------------
! Step 5:   hatch-call
!-----------------------------------------------------------------------
! Total energy of incident source particle must be defined before hatch
! Define posible maximum total energy of electron before hatch
      if (iqi.ne.0) then
        emaxe = ei              ! charged particle
      else
        emaxe = ei + RM         ! photon
      end if

!     ------------------------------
!     Open files (before HATCH call)
!     ------------------------------
      open(UNIT=KMPI,FILE='pgs5job.pegs5dat',STATUS='old')
      open(UNIT=KMPO,FILE='egs5job.dummy',STATUS='unknown')

      write(6,130)
130   FORMAT(/,' HATCH-call comes next',/)

!     ==========
      call hatch
!     ==========

!     ------------------------------
!     Close files (after HATCH call)
!     ------------------------------
      close(UNIT=KMPI)
      close(UNIT=KMPO)

      write(ifto,310)
310   FORMAT('MSTA')
      write(ifto,320) nreg
320   FORMAT(I4)
      write(ifto,330) (med(i),i=1,nreg)
330   FORMAT(15I4)
      write(ifto,340)
340   FORMAT('MEND')

! ----------------------------------------------------------
! Print various data associated with each media (not region)
! ----------------------------------------------------------
      write(6,140)
140   FORMAT(/,' Quantities associated with each MEDIA:')
      do j=1,nmed
        write(6,150) (media(i,j),i=1,24)
150     FORMAT(/,1X,24A1)
        write(6,160) rhom(j),rlcm(j)
160     FORMAT(5X,' rho=',G15.7,' g/cu.cm     rlc=',G15.7,' cm')
        write(6,170) ae(j),ue(j)
170     FORMAT(5X,' ae=',G15.7,' MeV    ue=',G15.7,' MeV')
        write(6,180) ap(j),up(j)
180     FORMAT(5X,' ap=',G15.7,' MeV    up=',G15.7,' MeV',/)
      end do

!-----------------------------------------------------------------------
! Step 6:  Initialization-for-howfar
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
! Step 7:  Initialization-for-ausgab
!-----------------------------------------------------------------------
      do i=1,nreg
        esum(i)=0.D0
      end do

      nlines=0
      nwrite=15
      maxpict=20    ! : Max history to output particle trajectory info
!-----------------------------------------------------------------------
! Step 8:  Shower-call
!-----------------------------------------------------------------------
      tt=etime(tarray)
      t0=tarray(1)

      write(6,190)
190   format(/,' Shower Results:',///,7X,'e',14X,'z',14X,'w',10X,
     1   'iq',3X,'ir',2X,'iarg',/)

      do i=1,ncases

!         Uncomment out following 2 lines to show particle trajectory 
!         1 case by 1 case using cgview
         if (npreci.eq.3.and.i.le.maxpict) write(ifto,195) i 
195      format('0',I12)                     

        if (nlines.lt.nwrite) then
        write(6,200) i,ei,zi,wi,iqi,irinn,idinc
200     format(i2,3G15.7,3I5)
        nlines=nlines+1
        end if

        call shower(iqi,ei,xi,yi,zi,ui,vi,wi,irinn,wti)

        if (i.le.maxpict) call plotxyz(99,0,0,0.D0,0.D0,0.D0,0.D0,0,
     *  0.D0,0.D0)

!        Uncomment out following 2 lines to show particle trajectory 
!        1 case by 1 case using cgview 
        if (npreci.eq.3.and.i.le.maxpict) write(ifto,205) 
205     FORMAT('9    1') ! 9 is to set end of batch. 1 is for latch-on.

      end do

      tt=etime(tarray)
      t1=tarray(1)

      timecpu=t1-t0
      write(6,210) timecpu
210   format(/,' Elapsed Time (sec)=',1PE12.5)

!-----------------------------------------------------------------------
! Step 9:  Output-of-results
!-----------------------------------------------------------------------
      totke=ncases*ekin
      write(6,220) ei,ncases
220   format(//,' Incident total energy of electron=',F12.1,' MeV',/,
     *' Number of cases in run=',I7,
     *//,' Energy deposition summary:',/)

      etot=0.D0
      do i=1,nreg
        etot=etot+esum(i)
        esum(i)=esum(i)/totke 
        write(6,230) i, esum(i)
230     format(' Fraction in region ',I3,'=',F10.7)
      end do

      etot=etot/totke
      write(6,240) etot
240   FORMAT(//,' Total energy fraction in run=',G15.7,/, 
     *'   Which should be close to unity')
!     -----------
!     Close files
!     -----------
      close(UNIT=6)
      close(UNIT=ifto)

      stop
      end

!-------------------------last line of main code------------------------
!-------------------------------ausgab.f--------------------------------
! Version:   050701-1615
! Reference: SLAC-730, KEK-2005-8 (Appendix 2)
!-----------------------------------------------------------------------
!23456789|123456789|123456789|123456789|123456789|123456789|123456789|12
! ----------------------------------------------------------------------
! Required subroutine for use with the EGS5 Code System
! ----------------------------------------------------------------------
! A simple AUSGAB to:
!
!   1) Score energy deposition
!   2) Print out stack information
!   3) Print out particle transport information (if switch is turned on)
!

! ----------------------------------------------------------------------

      subroutine ausgab(iarg)

      implicit none

      include 'include/egs5_h.f'                ! Main EGS "header" file

      include 'include/egs5_epcont.f'    ! COMMONs required by EGS5 code
      include 'include/egs5_stack.f'
      include 'auxcommons/lines.f'

      common/totals/esum(MXREG),maxpict,i
      real*8 esum
      integer maxpict,i

      integer iarg                                           ! Arguments

!     ----------------------
!     Add deposition energy
!     ----------------------

      esum(ir(np))=esum(ir(np)) + edep

!     ----------------------------------------------------------------
!     Print out stack information (for limited number cases and lines)
!     ----------------------------------------------------------------

      if (nlines.lt.nwrite) then
        write(6,1240) e(np),z(np),w(np),iq(np),ir(np),iarg
1240    FORMAT(3G15.7,3I5)
        nlines=nlines+1
      end if
      
!     ------------------------------------
!     Output particle information for plot
!     ------------------------------------
      if (i.le.maxpict) then
        call plotxyz(iarg,np,iq(np),x(np),y(np),z(np),e(np),ir(np),
     *       wt(np),time(np))
      end if

      return
      end

!--------------------------last line of ausgab.f------------------------
!-------------------------------howfar.f--------------------------------
! Version:   060620-1400
! Reference: T. Torii and T. Sugita, "Development of PRESTA-CG 
! Incorporating Combinatorial Geometry in EGS4/PRESTA", JNC TN1410 2002-201,
! Japan Nuclear Cycle Development Institute (2002).
! Improved version is provided by T. Sugita. 7/27/2004
!-----------------------------------------------------------------------
!23456789|123456789|123456789|123456789|123456789|123456789|123456789|12

! ----------------------------------------------------------------------
! Required (geometry) subroutine for use with the EGS5 Code System
! ----------------------------------------------------------------------
! This is a CG-HOWFAR. 
! ----------------------------------------------------------------------

      subroutine howfar
      implicit none
c
      include 'include/egs5_h.f'       ! Main EGS "header" file
      include 'include/egs5_epcont.f'  ! COMMONs required by EGS5 code
      include 'include/egs5_stack.f'
      include 'auxcommons/geom_common.f' ! geom-common file
c
c
      integer i,j,jjj,ir_np,nozone,jty,kno
      integer irnear,irnext,irlold,irlfg,itvlfg,ihitcg
      double precision xidd,yidd,zidd,x_np,y_np,z_np,u_np,v_np,w_np
      double precision tval,tval0,tval00,tval10,tvalmn,delhow
      double precision atvaltmp
      integer iq_np
c
      ir_np = ir(np)
      iq_np = iq(np) + 2
c
      if(ir_np.le.0) then
        write(6,*) 'Stopped in howfar with ir(np) <=0'
        stop
      end if
c
      if(ir_np.gt.izonin) then
        write(6,*) 'Stopped in howfar with ir(np) > izonin'
        stop
      end if
c
      if(ir_np.EQ.izonin) then
        idisc=1
        return
      end if
c
      tval=1.d+30
      itvalm=0
c
c     body check
      u_np=u(np)
      v_np=v(np)
      w_np=w(np)
      x_np=x(np)
      y_np=y(np)
      z_np=z(np)
c
      do i=1,nbbody(ir_np)
        nozone=ABS(nbzone(i,ir_np))
        jty=itblty(nozone)
        kno=itblno(nozone)
c     rpp check
        if(jty.eq.ityknd(1)) then
          if(kno.le.0.or.kno.gt.irppin) go to 190
          call rppcg1(kno,x_np,y_np,z_np,u_np,v_np,w_np)
c     sph check
        elseif(jty.eq.ityknd(2)) then
          if(kno.le.0.or.kno.gt.isphin) go to 190
          call sphcg1(kno,x_np,y_np,z_np,u_np,v_np,w_np)
c     rcc check
        elseif(jty.eq.ityknd(3)) then
          if(kno.le.0.or.kno.gt.irccin) go to 190
          call rcccg1(kno,x_np,y_np,z_np,u_np,v_np,w_np)
c     trc check
        elseif(jty.eq.ityknd(4)) then
          if(kno.le.0.or.kno.gt.itrcin) go to 190
          call trccg1(kno,x_np,y_np,z_np,u_np,v_np,w_np)
c     tor check
        elseif(jty.eq.ityknd(5)) then
          if(kno.le.0.or.kno.gt.itorin) go to 190
          call torcg1(kno,x_np,y_np,z_np,u_np,v_np,w_np)
c     rec check
        elseif(jty.eq.ityknd(6)) then
          if(kno.le.0.or.kno.gt.irecin) go to 190
          call reccg1(kno,x_np,y_np,z_np,u_np,v_np,w_np)
c     ell check
        elseif(jty.eq.ityknd(7)) then
          if(kno.le.0.or.kno.gt.iellin) go to 190
          call ellcg1(kno,x_np,y_np,z_np,u_np,v_np,w_np)
c     wed check
        elseif(jty.eq.ityknd(8)) then
          if(kno.le.0.or.kno.gt.iwedin) go to 190
          call wedcg1(kno,x_np,y_np,z_np,u_np,v_np,w_np)
c     box check
        elseif(jty.eq.ityknd(9)) then
          if(kno.le.0.or.kno.gt.iboxin) go to 190
          call boxcg1(kno,x_np,y_np,z_np,u_np,v_np,w_np)
c     arb check
        elseif(jty.eq.ityknd(10)) then
          if(kno.le.0.or.kno.gt.iarbin) go to 190
          call arbcg1(kno,x_np,y_np,z_np,u_np,v_np,w_np)
c     hex check
        elseif(jty.eq.ityknd(11)) then
          if(kno.le.0.or.kno.gt.ihexin) go to 190
          call hexcg1(kno,x_np,y_np,z_np,u_np,v_np,w_np)
c     haf check
        elseif(jty.eq.ityknd(12)) then
          if(kno.le.0.or.kno.gt.ihafin) go to 190
          call hafcg1(kno,x_np,y_np,z_np,u_np,v_np,w_np)
c     tec check
        elseif(jty.eq.ityknd(13)) then
          if(kno.le.0.or.kno.gt.itecin) go to 190
          call teccg1(kno,x_np,y_np,z_np,u_np,v_np,w_np)
c     gel check
        elseif(jty.eq.ityknd(14)) then
          if(kno.le.0.or.kno.gt.igelin) go to 190
          call gelcg1(kno,x_np,y_np,z_np,u_np,v_np,w_np)
c
c**** add new geometry in here
c
       end if
  190  continue
      end do
c
      irnear=ir_np
      if(itvalm.eq.0) then
        tval0=cgeps1
        xidd=x_np+tval0*u_np
        yidd=y_np+tval0*v_np
        zidd=z_np+tval0*w_np
  310   continue
          if(x_np.ne.xidd.or.y_np.ne.yidd.or.z_np.ne.zidd) goto 320
          tval0=tval0*10.d0
          xidd=x_np+tval0*u_np
          yidd=y_np+tval0*v_np
          zidd=z_np+tval0*w_np
          go to 310
  320   continue
c       write(*,*) 'srzone:1'
        call srzone(xidd,yidd,zidd,iq_np,ir_np,irnext)
c
        if(irnext.ne.ir_np) then
          tval=0.0d0
          irnear=irnext
        else
          tval00=0.0d0
          tval10=10.0d0*tval0
          irlold=ir_np
          irlfg=0
  330     continue
          if(irlfg.eq.1) go to 340
            tval00=tval00+tval10
            if(tval00.gt.1.0d+06) then
              write(6,9000) iq(np),ir(np),x(np),y(np),z(np),
     &                      u(np),v(np),w(np),tval00
 9000 format(' TVAL00 ERROR : iq,ir,x,y,z,u,v,w,tval=',
     &       2I3,1P7E12.5)
              stop
            end if
            xidd=x_np+tval00*u_np
            yidd=y_np+tval00*v_np
            zidd=z_np+tval00*w_np
            call srzold(xidd,yidd,zidd,irlold,irlfg)
            go to 330
  340     continue
c
          tval=tval00
          do j=1,10
            xidd=x_np+tval00*u_np
            yidd=y_np+tval00*v_np
            zidd=z_np+tval00*w_np
c           write(*,*) 'srzone:2'
            call srzone(xidd,yidd,zidd,iq_np,irlold,irnext)
            if(irnext.ne.irlold) then
              tval=tval00
              irnear=irnext
            end if
            tval00=tval00-tval0
          end do
          if(ir_np.eq.irnear) then
            write(0,*) 'ir(np),tval=',ir_np,tval
          end if
        end if
      else
        do j=1,itvalm-1
          do i=j+1,itvalm
            if(atval(i).lt.atval(j)) then
              atvaltmp=atval(i)
              atval(i)=atval(j)
              atval(j)=atvaltmp
            endif
          enddo
        enddo
        itvlfg=0
        tvalmn=tval
        do jjj=1,itvalm
          if(tvalmn.gt.atval(jjj)) then
            tvalmn=atval(jjj)
          end if
          delhow=cgeps2
          tval0=atval(jjj)+delhow
          xidd=x_np+tval0*u_np
          yidd=y_np+tval0*v_np
          zidd=z_np+tval0*w_np
  410     continue
          if(x_np.ne.xidd.or.y_np.ne.yidd.or.z_np.ne.zidd) go to 420
            delhow=delhow*10.d0
            tval0=atval(jjj)+delhow
            xidd=x_np+tval0*u_np
            yidd=y_np+tval0*v_np
            zidd=z_np+tval0*w_np
          go to 410
  420     continue
c         write(*,*) 'srzone:3'
          call srzone(xidd,yidd,zidd,iq_np,ir_np,irnext)
          if((irnext.ne.ir_np.or.atval(jjj).ge.1.).and.
     &        tval.gt.atval(jjj)) THEN
            tval=atval(jjj)
            irnear=irnext
            itvlfg=1
            goto 425
          end if
        end do
  425   continue
        if(itvlfg.eq.0) then
          tval0=cgmnst
          xidd=x_np+tval0*u_np
          yidd=y_np+tval0*v_np
          zidd=z_np+tval0*w_np
  430     continue
          if(x_np.ne.xidd.or.y_np.ne.yidd.or.z_np.ne.zidd) go to 440
            tval0=tval0*10.d0
            xidd=x_np+tval0*u_np
            yidd=y_np+tval0*v_np
            zidd=z_np+tval0*w_np
            go to 430
  440     continue
          if(tvalmn.gt.tval0) then
            tval=tvalmn
          else
            tval=tval0
          end if
        end if
      end if
      ihitcg=0
      if(tval.le.ustep) then
        ustep=tval
        ihitcg=1
      end if
      if(ihitcg.eq.1) THEN
        if(irnear.eq.0) THEN
          write(6,9200) iq(np),ir(np),x(np),y(np),z(np),
     &                  u(np),v(np),w(np),tval
 9200 format(' TVAL ERROR : iq,ir,x,y,z,u,v,w,tval=',2I3,1P7E12.5)
          idisc=1
          itverr=itverr+1
          if(itverr.ge.100) then
            stop
          end if
          return
        end if
        irnew=irnear
        if(irnew.ne.ir_np) then
          call rstnxt(iq_np,ir_np,irnew)
        endif
      end if
      return
      end
!--------------------last line of subroutine howfar---------------------
