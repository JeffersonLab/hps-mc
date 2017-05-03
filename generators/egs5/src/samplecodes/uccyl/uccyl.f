!***********************************************************************
!                                                                       
!                     **************                                    
!                     *            *                                    
!                     *  uccyl.f   *                                    
!                     *            *                                    
!                     **************                                    
!                                                                       
! An EGS5 user code with generalized multi-slab geometry.
! Demonstrate how to incorporate particle splitting and leading 
! particle biasing.
!
!  For SLAC-R-730/KEK Report 2005-8:  Example of generalized multi-slab
!  geometry with or without particle splitting and leading particle 
!  biasing.
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

      include 'include/egs5_bounds.f'  ! bounds contains ecut and pcut
      include 'include/egs5_edge.f'    ! edge contains iedgfl
      include 'include/egs5_epcont.f'  ! epcont contains iausfl
      include 'include/egs5_media.f'   ! media contains the array media
      include 'include/egs5_misc.f'    ! misc contains med
      include 'include/egs5_thresh.f'  ! thresh contains ae and ap
      include 'include/egs5_uphiot.f'  ! uphiot contains PI
      include 'include/egs5_useful.f'  ! useful contains RM
      include 'include/egs5_usersc.f'  ! usersc contains emaxe
      include 'include/randomm.f'

!     ----------------------
!     Auxiliary-code COMMONs
!     ----------------------
      include 'auxcommons/aux_h.f'   ! Auxiliary-code "header" file

      include 'auxcommons/cyldta.f'
      include 'auxcommons/pladta.f'
      include 'auxcommons/etaly1.f'
      include 'auxcommons/georz.f'
      include 'auxcommons/lines.f'
      include 'auxcommons/ntaly1.f'

      common/passit/irsplt(66),lsplt,nsplt
      integer irsplt,lsplt,nsplt

      real*8 ein,ekin,xin,yin,zin,        ! Arguments
     *       uin,vin,win,wtin
      integer iqin,irin
      
      real*8 gaussd                               ! External function

      real*8 bigh,cyrlo,deltaz,fde,fdedv,gsfde,ran1, ! Local variables
     *       sfde,sigma,sigx,sigy,totke,xi0,yi0,zi0
      real*8 ereg(66),vol(66)
      real tarray(2),tt,tt0,tt1,cputime
      integer i,idinc,irl,ispot,j,jr,jz,k,medsol,medtar,ncases,nz1
      character*24 medarr(2)
      real etime

!     ----------
!     Open files
!     ----------
      open(UNIT= 6,FILE='egs5job.out',STATUS='unknown')

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
      nmed=2
      medarr(1)='IRON-100                '
!      medarr(2)='TA90W10                 '
      medarr(2)='TA                      '

      do j=1,nmed
        do i=1,24
          media(i,j)=medarr(j)(i:i)
        end do
      end do  

      chard(1) = .64d0       !  optional, but recommended to invoke
      chard(2) = .64d0       !  automatic step-size control

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
      nplan=8          ! Number of planes
      ncyl=9           ! Number of cylinder
      nreg=(nplan-1)*ncyl+3 ! Number of regions 
                            ! (Including outside vacuumregion)
      irz=nreg-3
      medtar=2
      medsol=1

!     Set material to each region
!     At first all region to vacuum
      do i=1,nreg
        med(i)=0
      end do
      do i=3,10
        med(i)=medsol
      end do
      do i=12,19
        med(i)=medsol
      end do
      do i=21,28
        med(i)=medsol
      end do
      do i=30,31
        med(i)=medsol
      end do
      do i=35,37
        med(i)=medsol
      end do
      do i=44,46
        med(i)=medsol
      end do
      do i=53,55
        med(i)=medsol
      end do
      do i=58,64
        med(i)=medsol
      end do
      med(39)=medsol
      med(47)=medtar

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
      iqin=-1            ! Incident charge - electrons
      ein=1000.D0        ! 1 GeV total energy
      ekin=ein+iqin*RM
      ispot=1
      sigma=0.005
      sigy=sigma
      sigx=sigma
      xi0=0.0            ! Incident at origin
      yi0=0.0
      zi0=0.0
      uin=0.0            ! Moving along z axis
      vin=0.0
      win=1.0
      irin=2             ! Starts in region 2, could be 1
      wtin=1.0           ! Weight = 1 since no variance reduction used

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
130   format(/' Start uccyl'/' Call hatch to get cross-section data')

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
150   format(/' Quantities associated with each media:',/)
      do j=1,nmed
      write(6,160)(media(i,j),i=1,24)
160   format(1X,24A1)
      write(6,170) rhom(j),rlcm(j)
170   format(5X,' rho=',G15.7,' g/cm**3     rlc=', G15.7,' cm')
      write(6,180) ae(j),ue(j)
180   format(5X,' AE=',G15.7,' MeV    UE=',G15.7,' MeV')
      write(6,190) ap(j),up(j)
190   format(5X,' AP=',G15.7,' MeV    UP=',G15.7,' MeV')
      end do

      lsplt=0   ! Number of split. 0 means no splitting
      nsplt=0
      if (lsplt.gt.0) then
        nsplt=lsplt-1
        do irl=1,nreg
          irsplt(irl)=0
        end do
        irsplt(50)=1
      end if
      
!-----------------------------------------------------------------------
! Step 6:  Initialization-for-howfar
!-----------------------------------------------------------------------
      do j=1,nplan
        pcoord(1,j)=0.0
        pcoord(2,j)=0.0
        pnorm(1,j)=0.0
        pnorm(2,j)=0.0
        pnorm(3,j)=1.0
      end do
      pcoord(3,1)=0.0
      pcoord(3,2)=0.64
      pcoord(3,3)=2.48
      pcoord(3,4)=4.32
      pcoord(3,5)=7.11
      pcoord(3,6)=10.7
      pcoord(3,7)=12.7
      pcoord(3,8)=22.86

      cyrad(1)=0.762
      cyrad(2)=5.84
      cyrad(3)=7.62
      cyrad(4)=10.16
      cyrad(5)=12.7
      cyrad(6)=15.24
      cyrad(7)=15.88
      cyrad(8)=17.40
      cyrad(9)=25.40

      write(6,200)
200   format(/' Pcoord and pnorm values for each j-plane (i=1,3):',/)
      do j=1,nplan
        write(6,210) j,(pcoord(i,j),i=1,3),(pnorm(i,j),i=1,3)
210     format(I5,6G15.7)
      end do

      write(6,220)
220   format(/' Cylinder radii:',/)
      do i=1,ncyl
        cyrad2(i)=cyrad(i)*cyrad(i)
        write(6,230) i,cyrad(i)
230     format(' CYRAD(',I2,')=',G15.7)
      end do

      write(6,240)
240   format(/' Volume (cm**3) of each region:',/)
      do i=1,nplan-1
        do j=1,ncyl
          irl=j+1+ncyl*(i-1)
          bigh=pcoord(3,i+1)-pcoord(3,i)
          if (j.eq.1) then
            vol(irl)=PI*cyrad2(j)*bigh
          else
            vol(irl)=PI*(cyrad2(j)-cyrad2(j-1))+bigh
          end if
          write(6,250) irl,med(irl),vol(irl),irsplt(irl)
250       format(' irl=',I3,5X,'med=',I3,5X,'vol=',G15.5,5X,5X,
     *           'irsplt=', I2)
        end do
      end do
      vol(1)=-1
      vol(65)=-1
      vol(66)=-1

!-----------------------------------------------------------------------
! Step 7:  Initialization-for-ausgab
!-----------------------------------------------------------------------
      call ecnsv1(0,nreg,totke)
      call ntally(0,nreg)

!  A Leading Particle Biasing option, which may be applied after every 
!  bremsstrahlung or pair production event, is available in ausgab.
!  To invoke Leading Particle Biasing, set the appropriate iausfl flag 
!  (found in common/epcont/) to 1 in this step. iausfl(8)=1 turns on 
!  Leading Particle Biasing for bremsstrahlung, and iasufl(17)=1 turns on 
!  Leading Particle Biasing for pair production.

      iausfl(8)=0   ! After bremsstrahlung
      iausfl(17)=0  ! After pair production

      ncount=0
      ilines=0
      ncases=10
      nwrite=10
      nlines=20
      write(6,260)
260   format(/' Energy/coordinates/direction cosines/etc.',/
     *       ,'   (First hundred or so lines)   ',/)
      write(6,270)
270   format(6X,'e',16X,'x',14X,'y',14X,'z',14X,'u',14X,'v', 14X,'w',
     *       9X,'iq',4X,'ir',3X,'iarg',/)

!-----------------------------------------------------------------------
! Step 8:  Shower-call
!-----------------------------------------------------------------------
      tt=etime(tarray)
      tt0=tarray(1)

      do i=1,ncases 
        if (ispot.eq.0) then
          xin=xi0 
          yin=yi0
          zin=zi0
        else
          call randomset(ran1)
          yin=yi0 + sigy*gaussd(ran1)
          call randomset(ran1)
          xin=xi0 + sigx*gaussd(ran1)
          zin=zi0
        end if
        if (ncount.le.nwrite.and.ilines.le.nlines) then
          write(6,280) ein,xin,yin,zin,uin,vin,win,iqin,irin,idinc
280       format(7G15.7,3I5)
          ilines=ilines+1
        end if
        call shower(iqin,ein,xin,yin,zin,uin,vin,win,irin,wtin)
        ncount=ncount+1
      end do
      
      totke=ncount*ekin

      tt=etime(tarray)
      tt1=tarray(1)
      cputime=tt1-tt0

!-----------------------------------------------------------------------
! Step 9:  Output-of-results
!-----------------------------------------------------------------------
      if(iausfl(8).eq.1.and.iausfl(17).eq.1) then
        write(6,290) cputime,ncases
290     format(/1X,G15.5,' sec for ',I8,' cases'/
     *  ' With Leading Particle Biasing.')
      else 
        write(6,300) cputime,ncases
300     format(/1X,G15.5,' sec for ',I8,' cases'/
     *  ' Without Leading Particle Biasing.')
      end if

      do j=1,nreg
        ereg(j)=0.d0
        do i=1,3
          do k=1,5
            ereg(j)=ereg(j)+esum(i,j,k)
          end do
        end do
      ereg(j)=ereg(j)/ncount
      end do
 
      nz1=nplan-1
      gsfde=0.d0
      
!     Write-out the backscattered energy
      write(6,310)
310   format(/' Radial histogram output:',//,' Z-bin= -0.0 to -infinity'
     *, ' (Backscatter)',//,6X,'r1(cm)',9X,'r2(cm)',7X,'(de/e)/dv',9X,
     *'de/e', 7X,'sum de/e',/)
      fde=ereg(1)/ekin
      sfde=fde
      gsfde=gsfde+sfde
      write(6,320) fde,sfde
320   format(/,8X,'0.0',9X,'infinity',17X,2G15.7)

      do jz=1,nz1
        deltaz=pcoord(3,jz+1)-pcoord(3,jz)
        write(6,330) pcoord(3,jz),pcoord(3,jz+1)
330     format(' Radial histogram output:',//,' Z-bin=',G15.7,
     *  '(cm)  to',G15.7, '(cm)',//,6X,'r1(cm)',9X,'r2(cm)',7X,
     *  '(de/e)/dv',9X,'de/e',7X,'sum de/e',/)
        sfde=0.d0
        do jr=1,ncyl
          irl=jr+1+ncyl*(jz-1)
          if (jr.eq.1) then
            cyrlo=0.0 
          else 
            cyrlo=cyrad(jr-1) 
          end if 
          fde=ereg(irl)/ekin
          fdedv=fde/vol(irl)
          sfde=sfde + fde
          write(6,340) cyrlo,cyrad(jr),fdedv,fde,sfde
340       format(5G15.7)
        end do
      end do
!     Leakage from side
      fde=ereg(nreg)/ekin
      sfde=sfde + fde
      gsfde=gsfde + sfde
      write(6,350) cyrad(ncyl),fde,sfde
350   format(/,G15.7,4X,'infinity',18X,2G15.7)

      fde=ereg(nreg-1)/ekin
      sfde=sfde + fde
      write(6,360) fde,sfde
360   format('  Transmitted ',2G15.7)

      gsfde=gsfde + sfde
      write(6,370) gsfde
370   format(//,45X,'Total sum de/e ',G15.7,'  (should be close',
     *       ' to unity)')

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
!                                                                       
!  In this AUSGAB routine for uccyl, we score the energy deposited
!  in the various regions. 

      subroutine ausgab(iarg)

      implicit none

      include 'include/egs5_h.f'                ! Main EGS "header" file

      include 'include/egs5_epcont.f'    ! epcont contains edep
      include 'include/egs5_stack.f'     ! stack contains x, y, z, u, v,
                                         ! w, ir and np
      include 'include/egs5_useful.f'    ! useful contains RM
      include 'include/randomm.f'

      include 'auxcommons/aux_h.f'   ! Auxiliary-code "header" file

      include 'auxcommons/etaly1.f'        ! Auxiliary-code COMMONs
      include 'auxcommons/lines.f'
      include 'auxcommons/ntaly1.f'

      integer iarg                                          ! Arguments

      integer irl                                     ! Local variables
      real*8 edepwt,eks,ekenp,rnnolp
      
      irl=ir(np)
      edepwt=edep*wt(np)
      if (ncount.le.nwrite.and.ilines.le.nlines) then
        write(6,100) e(np),x(np),y(np),z(np),u(np),v(np),w(np),iq(np),
     *                ir(np),iarg
100     format(7G15.7,3I5) 
        ilines=ilines+1
      end if
      if (iarg.le.4) then
        esum(iq(np)+2,irl,iarg+1)=esum(iq(np)+2,irl,iarg+1)+edepwt
        nsum(iq(np)+2,irl,iarg+1)=nsum(iq(np)+2,ir(np),iarg+1)+1
      end if
      if (iarg.eq.7) then   ! Apply Leading Particle Biasing for bremss.
        eks=e(np)+e(np-1)-RM   ! Kinetic energy before bremss.
        ekenp=e(np)
        if(iq(np).ne.0) ekenp=e(np)-RM
        call randomset(rnnolp)
        if (rnnolp.lt.ekenp/eks) then  ! Follow np
          e(np-1)=e(np)
          iq(np-1)=iq(np)
          u(np-1)=u(np)
          v(np-1)=v(np)
          w(np-1)=w(np)
        end if
        ekenp=e(np-1)
        if (iq(np-1).ne.0) ekenp=e(np-1)-RM
        wt(np-1)=wt(np-1)*eks/ekenp
        np=np-1
      end if
      
      if (iarg.eq.16) then  ! Apply Leading Particle Biasing for pair.
        eks=e(np)+e(np-1)-2.0*RM
        ekenp=e(np)-RM
        call randomset(rnnolp)
        if (rnnolp.lt.ekenp/eks) then ! Follow np
          e(np-1)=e(np)
          iq(np-1)=iq(np)
          u(np-1)=u(np)
          v(np-1)=v(np)
          w(np-1)=w(np)
        end if
        ekenp=e(np-1)-RM
        wt(np-1)=wt(np-1)*eks/ekenp
        np=np-1
      end if
            
      return
      end

!--------------------------last line of ausgab.f------------------------
!-------------------------------howfar.f--------------------------------
! Version:   040810-1300
! Reference: SLAC-265 (p.19-20, Appendix 2)
!-----------------------------------------------------------------------
!23456789|123456789|123456789|123456789|123456789|123456789|123456789|12
! ----------------------------------------------------------------------
! Required (geometry) subroutine for use with the EGS5 Code System
! ----------------------------------------------------------------------
! This is a general-purpose, R-Z HOWFAR. 
! ----------------------------------------------------------------------

      subroutine howfar

      implicit none

      include 'include/egs5_h.f'                ! Main EGS "header" file

      include 'include/egs5_epcont.f'    ! COMMONs required by EGS5 code
      include 'include/egs5_stack.f'
      include 'include/egs5_misc.f'

      include 'auxcommons/aux_h.f'   ! Auxiliary-code "header" file

      include 'auxcommons/cyldta.f'        ! Auxiliary-code COMMONs
      include 'auxcommons/georz.f'
      include 'auxcommons/instuf.f'
      include 'auxcommons/pladta.f'

      common/passit/irsplt(66),lsplt,nsplt
      integer irsplt,lsplt,nsplt

      real*8 tcyl                                      ! Local variables
      integer ihit,ipl1,ipl2,irl,irnxt1,irnxt2,isplt,
     *        nannu,ncl1,ncl2,nslab

      irl = ir(np)

      if (irl .le. 0) then
        write(6,*) 'Stopped in howfar with irl <= 1'
        stop
      end if

      if (irl .eq. 1. or. irl .ge. irz+2) then
        idisc = 1  ! ---------------------------------------------------
        return     ! Particle outside geometry - return to ELECTR/PHOTON
      end if       ! ---------------------------------------------------

!     ----------------------------------
!     Get slab number and annulus number
!     ----------------------------------
      nslab = (irl -2 ) / ncyl +1                      ! Slab number
      nannu = irl -1 - ncyl * (nslab -1)               ! Annulus number
!     --------------------
!     Check in Z-direction
!     --------------------
      ipl1 = nslab + 1
      ipl2 = nslab

      if (nslab .lt. nplan-1) then
        irnxt1 = irl + ncyl
      else
        irnxt1 = irz + 2
      end if
      
      if (nslab .gt. 1 ) then
        irnxt2 = irl - ncyl
      else 
        irnxt2 = 1
      end if

      call plan2p(ipl1,irnxt1,1,ipl2,irnxt2,-1)

!     --------------------
!     Check in R-direction
!     --------------------

      if (nannu .lt. ncyl) then
        irnxt2 = irl +1
      else 
        irnxt2 = irz + 3
      end if
     
      if (nannu .gt. 1) then
        irnxt1 = irl -1
        ncl2 = nannu
        ncl1 = nannu -1
        call cyl2 (ncl1, irnxt1, ncl2, irnxt2)
      else                             ! Inner-most cylinder---special case
        call cylndr(1,1,ihit,tcyl)
        if (ihit .eq. 1) then
          call chgtr(tcyl,irnxt2)
        end if
      end if

      if (irsplt(irl).eq.1.and.irl.ne.irold.and.iq(np).eq.0) then
          ! Apply particle splitting
        if (lsplt.ne.0) then
          wt(np)=wt(np)/lsplt
          do isplt=1,nsplt
            x(np+isplt)=x(np)
            y(np+isplt)=y(np)
            z(np+isplt)=z(np)
            u(np+isplt)=u(np)
            v(np+isplt)=v(np)
            w(np+isplt)=w(np)
            ir(np+isplt)=ir(np)
            wt(np+isplt)=wt(np)
            dnear(np+isplt)=dnear(np)
            latch(np+isplt)=latch(np)
            e(np+isplt)=e(np)
            iq(np+isplt)=iq(np)
            x(np+isplt)=x(np)
          end do
          np=np+nsplt
        end if
      end if
                                               ! -----------------------
      return                                   ! Return to ELECTR/PHOTON
                                               ! -----------------------
      end

!--------------------------last line of howfar.f------------------------

      double precision function gaussd(x)
      implicit none
      double precision x
      real*8 b,c,d,e,g,p,q,r,t,u,v,x1
      data b,c,d,e/1.256853,1.706340,-0.2898778,-0.05971928/
      data p,q,r/1.451153,-0.3040790,-0.5111851/
      
      u=x
      x1=u-0.5
      t=x1*x1
      if(dabs(x1).le.0.46875) then
        g=(e/(t+d)+t+b)*c*x1
      else
        v=dsqrt(-dlog(0.5-dabs(x1)))
        g=sign((r/v+q+v)*p,x1)
      end if
      gaussd=g
      return
      end
