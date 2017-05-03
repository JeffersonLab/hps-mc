!--------------------------------pegs5.f--------------------------------
! Version: 090116-0700
! Reference: SLAC-R-730/KEK-2005-8
!-----------------------------------------------------------------------
!23456789|123456789|123456789|123456789|123456789|123456789|123456789|12

      double precision function addmol(x)
      implicit none
      double precision x
      include 'pegscommons/dercon.f'
      addmol=1.0/(x-rm)**2
      return
      end

      double precision function adfmol(x)
      implicit none
      double precision x
      include 'pegscommons/dercon.f'
      adfmol=-1.0/(x-RM)
      return
      end

      double precision function adimol(x)
      implicit none
      double precision x
      include 'pegscommons/dercon.f'
      adimol=-1.0/x+RM
      return
      end

      subroutine adscpr(mxrawt,mxshet,elecnt,nshelt,capint,scprot,qcapt,
     *pz)
      implicit none
      integer iiend, mxrawt, i, j, ii, nshelt, mxshet
      double precision qcapt, elecnt, pz, capint, scprot
      include 'pegscommons/cpcom.f'
      dimension elecnt(200),nshelt(200),capint(200),scprot(31,200), 
     *          qcapt(31)
      iiend=MXRAW+mxrawt+1
      if (iiend.gt.200) then
        write(  26,100)
100     format(' Error Compton profile data  MXRAW .GT. 200')
        close(26)
        stop
      end if
      do i=1,31
        if (MXRAW.eq.0) then
          qcap(i)=qcapt(i)
        else
          if (qcap(i).ne.qcapt(i)) then
            write(  26,110)
110         format(' Error Compton profile data  qcap are not agreed')
            close(26)
            stop
          end if
        end if
      end do
      do j=1,31
        if (MXRAW.eq.0) then
          scprof(j,iiend)=0.0
        else
          scprof(j,iiend)=scprof(j,MXRAW+1)
        end if
      end do
      do i=1,mxrawt
        ii=MXRAW+I
        nshell(ii)=nshelt(i)+MXSHEL
        elecni(ii)=elecnt(i)*pz
        capin(ii)=capint(i)
        do j=1,31
          scprof(j,ii)=scprot(j,i)
          scprof(j,iiend)=scprof(j,iiend)+scprot(j,i)*elecnt(i)*pz
        end do
      end do
      MXRAW=MXRAW+mxrawt
      mxshel=mxshel+mxshet
      return
      end

      double precision function affact(x)
      implicit none
      double precision aintp, x
      include 'pegscommons/cohcom.f'
      affact=aintp(x,XVAL(1),100,AFAC2(1),1,.true.,.true.)
      return
      end

      double precision function aintp(x,xa,nx,ya,isk,xlog,ylog)
      implicit none
      integer nx, isk, j, i
      double precision xi, xj, xv, yi, yj
      double precision xa(nx),x
      double precision ya(isk,nx)
      logical xlog,ylog,xlogl
      xlogl=xlog
      do j=2,nx
        if (x.lt.xa(j)) go to 100
      end do
      j=nx
100   i=j-1
      if (xa(i).le.0.0) then
        xlogl=.false.
      end if
      if (.not.xlogl) then
        xi=xa(i)
        xj=xa(j)
        xv=x
      else
        xi=dlog(xa(i))
        xj=dlog(xa(j))
        xv=dlog(x)
      end if
      if (ylog.and.(ya(1,i).eq.0.0.or.ya(1,j).eq.0.0)) then
        aintp=0.0
      else
        if (ylog) then
          yi=dlog(ya(1,i))
          yj=dlog(ya(1,j))
          if (xj.eq.xi) then
            aintp=yi
          else
            aintp=(yi*(xj-xv)+yj*(xv-xi))/(xj-xi)
          end if
          aintp=dexp(aintp)
        else
          yi=ya(1,i)
          yj=ya(1,j)
          if (xj.eq.xi) then
            aintp=yi
          else
            aintp=(yi*(xj-xv)+yj*(xv-xi))/(xj-xi)
          end if
        end if
      end if
      return
      end

      double precision function alin(x)
      implicit none
      double precision x
      alin=x
      return
      end

      double precision function alini(x)
      implicit none
      double precision x
      alini=x
      return
      end

      double precision function alke(e)
      implicit none
      double precision e
      include 'pegscommons/dercon.f'
      alke=dlog(e-RM)
      return
      end

      double precision function alkei(x)
      implicit none
      double precision x, dexp
      include 'pegscommons/dercon.f'
      alkei=dexp(x) + RM
      return
      end

      double precision function amoldm(en0,en)
      implicit none
      double precision en0, tm, em, betasq, amolfm, en
      include 'pegscommons/pmcons.f'
      include 'pegscommons/dercon.f'
      include 'pegscommons/molvar.f'
      include 'pegscommons/lamolm.f'
      T0=en0-RM
      tm=T0/RM
      em=tm+1.
      c1=(tm/em)**2
      c2=(2.*tm+1.)/em**2
      betasq=1.-1./em**2
      cmoll=RLC*EDEN*2.*PI*R0**2/(betasq*T0*tm)
      amoldm=amolfm(en)
      return
      end

      double precision function amolfm(en)
      implicit none
      double precision t, en, eps, epsp, epsi, epspi
      include 'pegscommons/dercon.f'
      include 'pegscommons/lamolm.f'
      t=en-RM
      eps=t/T0
      epsp=1.-eps
      epsi=1./eps
      epspi=1./epsp
      amolfm=CMOLL*(C1+epsi*(epsi-C2)+epspi*(epspi-C2))
      return
      end

      double precision function amolrm(en0,en1,en2)
      implicit none
      double precision t0, en0, t1, en1, t2, en2, tm, em, c1, c2,
     & betasq, cmoll2, eps1, epsp1, eps2, epsp2, dlog
      include 'pegscommons/pmcons.f'
      include 'pegscommons/dercon.f'
      include 'pegscommons/molvar.f'
      t0=en0-RM
      t1=en1-RM
      t2=en2-RM
      tm=t0/RM
      em=tm+1.
      c1=(tm/em)**2
      c2=(2.*tm+1.)/em**2
      betasq=1.-1./em**2
      cmoll2=RLC*EDEN*2.*PI*R0**2/(betasq*tm)
      eps1=t1/t0
      epsp1=1.-eps1
      eps2=t2/t0
      epsp2=1.-eps2
      amolrm=cmoll2*(c1*(eps2-eps1)+1./eps1-1./eps2+1./epsp2-1./epsp1 -
     *       c2*dlog(eps2*epsp1/(eps1*epsp2)))
      return
      end

      double precision function amoltm(e0)
      implicit none
      double precision e0, t0, amolrm
      include 'pegscommons/thres2.f'
      include 'pegscommons/dercon.f'
      if (e0.le.THMOLL) then
        amoltm=0.
      else
        t0=e0-RM
        amoltm=amolrm(e0,ae,t0*0.5+RM)
      end if
      return
      end

      double precision function anihdm(e0,k)
      implicit none
      double precision gam, e0, t0p, anihfm
      include 'pegscommons/pmcons.f'
      include 'pegscommons/dercon.f'
      include 'pegscommons/molvar.f'
      include 'pegscommons/lanihm.f'
      double precision k
      gam=e0/RM
      a=gam+1.
      t0p=gam-1.
      C1=RLC*EDEN*PI*R0**2/(a*t0p*RM)
      C2=A+2.0*gam/a
      anihdm=anihfm(k)
      return
      end

      double precision function anihfm(k)
      implicit none
      double precision s1
      include 'pegscommons/dercon.f'
      include 'pegscommons/lanihm.f'
      double precision k,kp,x
      s1(x)=C1*(-1.+(C2-1.0/x)/x)
      kp=k/RM
      anihfm=s1(kp)+s1(A-kp)
      return
      end

      double precision function anihrm(e0,k1,k2)
      implicit none
      double precision s2, c1, c2, dlog, gam, e0, a, t0p
      include 'pegscommons/pmcons.f'
      include 'pegscommons/dercon.f'
      include 'pegscommons/molvar.f'
      double precision k1,k2,kp1,kp2
      double precision x
      s2(x)=RM*c1*(-x+c2*dlog(x)+1.0/x)
      gam=e0/RM
      kp1=k1/RM
      kp2=k2/RM
      a=gam+1.
      T0P=gam-1.
      c1=RLC*EDEN*PI*R0**2/(A*T0P*RM)
      c2=A+2.*gam/A
      anihrm=s2(kp2)-s2(kp1)+s2(A-kp1)-s2(A-KP2)
      return
      end

      double precision function anihtm(e0)
      implicit none
      double precision gam, e0, p0p2, p0p, dsqrt, canih, dlog
      include 'pegscommons/pmcons.f'
      include 'pegscommons/dercon.f'
      include 'pegscommons/molvar.f'
      gam=e0/RM
      p0p2=gam*gam-1.0
      p0p=dsqrt(p0p2)
      canih=RLC*EDEN*PI*R0**2/(gam+1.)
      anihtm=canih*((gam*gam+4.*gam+1.)/p0p2*dlog(gam+p0p)-(gam+3.)/p0p)
      return
      end

      double precision function aprim(z,e)
      implicit none
      integer ie, naprz, napre, iz
      double precision e, em, aintp, z
      include 'pegscommons/dercon.f'
      include 'pegscommons/epstar.f'
      double precision aprimd(115,14),eprim(115),zprim(14),aprimz(115)
      data aprimd/1.32,1.26,1.18,1.13,1.09,1.07,1.05,1.04,1.03, 1.02,8*1
     *.0, 97*0.0, 1.34,1.27,1.19,1.13,1.09,1.07,1.05,1.04,1.03,1.02, 8*1
     *.0, 97*0.0, 1.39,1.30,1.21,1.14,1.10,1.07,1.05,1.04,1.03,1.02,0.99
     *4, 2*0.991,0.990,2*0.989,2*0.988, 97*0.0, 1.46,1.34,1.23,1.15,1.11
     *,1.08, 1.06,1.05,1.03,1.02,0.989, 0.973,0.971,0.969,0.967,0.965,2*
     *0.963, 97*0.0, 1.55,1.40,1.26,1.17,1.12,1.09,1.07,1.05,1.03,1.02,0
     *.955,0.935, 0.930,0.925,0.920,0.915,2*0.911, 97*0.0,  1035*0.0/, E
     *PRIM/2.,3.,4.,5.,6.,7.,8.,9.,10.,11.,21.,31.,41.,51.,61.,71.,81.,9
     *1.,  97*0.0/, ZPRIM/6.,13.,29.,50.d0,79., 9*0.0/
      if (IAPRIM.eq.0) then
        if (IAPRFL .eq. 0) then
          IAPRFL=1
          write(  26,100)
100       format(' IAPRIM=0, i.e. uses KOCH AND MOTZ empirical correctio
     *ns to', ' brem cross section'/)
        end if
        if (e.ge.50) then
          APRIM=1.
        else
          em=e/RM
          do ie=1,18
            aprimz(ie)= aintp(z,zprim,5,aprimd(ie,1),115,.false.,.false.
     *      )
          end do
          aprim=aintp(em,eprim,18,aprimz,1,.false.,.false.)
        end if
      else if(IAPRIM.eq.1) then
        if (IAPRFL.eq.0) then
          write(  26,110)
110       format(' IAPRIM=1, i.e. uses NRC(based on NIST/ICRU)', ' corre
     *ctions to brem cross section'/)
          read(22,*) naprz, napre
          read(22,*) (eprim(ie),ie=1,napre)
          do ie=1,napre
            eprim(ie)=1.+eprim(ie)/RM
          end do
          do iz=1,naprz
            read(22,*) zprim(iz),(aprimd(ie,iz),ie=1,napre)
          end do
          iaprfl=1
          rewind(22)
        end if
        em=e/RM
        do ie=1,napre
          aprimz(ie)= aintp(z,zprim,naprz,aprimd(ie,1),115,.true.,.false
     *    .)
        end do
        aprim=aintp(em,eprim,napre,aprimz,1,.false.,.false.)
      else if (iaprim.eq.2) then
        if (iaprfl .eq. 0) then
          iaprfl=1
          write(  26,140)
140       format(' IAPRIM = 2, i.e. uses NO corrections to brem', ' cros
     *s section'/)
        end if
        aprim=1.0
      else
        write(  26,150) iaprim
150     format(//,' Illegal value for iaprim: ',I4)
        close(26)
        stop
      end if
      return
      end

      double precision function arec(x)
      implicit none
      double precision x
      arec=1.0/x
      return
      end

      double precision function bhabdm(en0,en)
      implicit none
      double precision en0, tm, em, y, bhabfm, en
      include 'pegscommons/pmcons.f'
      include 'pegscommons/dercon.f'
      include 'pegscommons/molvar.f'
      include 'pegscommons/lbhabm.f'
      T0=en0-RM
      tm=T0/RM
      em=tm+1.
      y=1./(tm+2.)
      betasi=1./(1.-1./em**2)
      CBHAB=RLC*EDEN*2.*PI*R0**2/(T0*tm)
      B1=2.-y**2
      B2=3.-y*(6.-y*(1.-y*2.))
      B3=2.-y*(10.-y*(16.-y*8.))
      B4=1.-y*(6.-y*(12.-y*8.))
      bhabdm=bhabfm(en)
      return
      end

      double precision function bhabfm(en)
      implicit none
      double precision t, en, eps, epsi
      include 'pegscommons/dercon.f'
      include 'pegscommons/lbhabm.f'
      t=en-RM
      eps=t/T0
      epsi=1./eps
      bhabfm=CBHAB*(epsi*(epsi*BETASI-B1)+B2+EPS*(eps*B4-B3))
      return
      end

      double precision function bhabrm(en0,en1,en2)
      implicit none
      double precision t0, en0, t1, en1, t2, en2, tm, em, y, betasi,
     & cbhab2, b1, b2, b3, b4, eps1, eps2, dlog
      include 'pegscommons/pmcons.f'
      include 'pegscommons/dercon.f'
      include 'pegscommons/molvar.f'
      t0=en0-RM
      t1=en1-RM
      t2=en2-RM
      tm=t0/RM
      em=tm+1.
      y=1./(tm+2.)
      betasi=1./(1.-1./em**2)
      cbhab2=RLC*EDEN*2.*PI*R0**2/tm
      b1=2.-y**2
      b2=3.-y*(6.-y*(1.-y*2.))
      b3=2.-y*(10.-y*(16.-y*8.))
      b4=1.-y*(6.-y*(12.-y*8.))
      eps1=t1/t0
      eps2=t2/t0
      bhabrm=cbhab2*(betasi*(1./eps1-1./eps2)-b1*dlog(eps2/eps1) +b2*(ep
     *s2-eps1)+eps2*eps2*(eps2*b4/3.-0.5*b3) - eps1*eps1*(eps1*b4/3.-0.5
     **b3))
      return
      end

      double precision function bhabtm(e0)
      implicit none
      double precision e0, bhabrm
      include 'pegscommons/thres2.f'
      include 'pegscommons/dercon.f'
      if (e0.le.ae) then
        bhabtm=0.
      else
        bhabtm=bhabrm(e0,ae,e0)
      end if
      return
      end

      double precision function bremdr(ea,k)
      implicit none
      integer ls
      double precision ea, bremfr
      double precision k
      include 'pegscommons/lbremr.f'
      e=ea
      if (e.ge.50.) then
        LD=2
        ls=3
      else
        LD=1
        ls=0
      end if
      LA=ls+1
      LB=ls+2
      bremdr=bremfr(k)
      return
      end

      double precision function bremdz(z,e,k)
      implicit none
      double precision brmsdz, z, e
      double precision k
      bremdz=brmsdz(z,e,k)/k
      return
      end

      double precision function bremfr(k)
      implicit none
      double precision eps, del, delta, a, b
      double precision k
      include 'pegscommons/bremp2.f'
      include 'pegscommons/dbrpr.f'
      include 'pegscommons/lbremr.f'
      eps=k/e
      del=eps/(e*(1-eps))
      if (del.gt.delpos(ld)) then
        bremfr=0.0
        return
      end if
      delta=DELCM*del
      if (delta.le.1.) then
        a=DL1(LA)+delta*(DL2(LA)+delta*DL3(LA))
        b =DL1(LB)+delta*(DL2(LB)+delta*DL3(LB))
      else
        a=DL4(LA)+DL5(LA)*dlog(delta+DL6(LA))
        b =DL4(LB)+DL5(LB)*dlog(delta+DL6(LB))
      end if
      bremfr=(ALPHI(LD)*(1.-eps)/eps/AL2*A+0.5*(2.*eps)*b)/e
      return
      end

      double precision function bremfz(k)
      implicit none
      double precision brmsfz
      double precision k
      bremfz=brmsfz(k)/k
      return
      end

      double precision function bremrm(e,k1,k2)
      implicit none
      integer i
      double precision bremrz, e
      double precision k1,k2
      include 'pegscommons/mixdat.f'
      include 'pegscommons/mxdatc.f'
      bremrm=0.
      do i=1,ne
        bremrm=bremrm+pz(i)*bremrz(z(i),e,k1,k2)
      end do
      return
      end

      double precision function bremrr(e,k1,k2)
      implicit none
      double precision dummy, bremdr, e, qd
      double precision k1,k2
      external bremfr
      dummy=bremdr(e,k1)
      bremrr=qd(bremfr,k1,k2,'bremfr')
      return
      end

      double precision function bremrz(z,e,k1,k2)
      implicit none
      double precision dummy, bremdz, z, e, qd
      double precision k1,k2
      external bremfz
      dummy=bremdz(z,e,k1)
      bremrz=qd(bremfz,k1,k2,'bremfz')
      return
      end

      double precision function bremtm(e0)
      implicit none
      double precision e0, bremrm
      include 'pegscommons/thres2.f'
      include 'pegscommons/dercon.f'
      if (e0.le.AP+RM) then
        bremtm=0.
      else
        bremtm=bremrm(e0,AP,e0-RM)
      end if
      return
      end

      double precision function bremtr(e0)
      implicit none
      double precision e0, bremrr
      include 'pegscommons/thres2.f'
      include 'pegscommons/dercon.f'
      if (e0.le.AP+RM) then
        bremtr=0.
      else
        bremtr=bremrr(e0,AP,e0-RM)
      end if
      return
      end

      double precision function brmsdz(z,ea,k)
      implicit none
      double precision ea, z, aprim, xsif, dlog, fcoulc, brmsfz
      double precision k
      include 'pegscommons/pmcons.f'
      include 'pegscommons/dercon.f'
      include 'pegscommons/molvar.f'
      include 'pegscommons/lbremz.f'
      e=ea
      delc=136.*z**(-1./3.)*RM/e
      const=aprim(z,e)*(AN*RHO/WM)*R0**2*FSC*z*(Z+XSIF(Z))*RLC
      XLNZ=4./3.*dlog(z)
      if (e.ge.50) XLNZ=XLNZ+4.*fcoulc(z)
      DELTAM=dexP((21.12-XLNZ)/4.184)-0.952
      brmsdz=brmsfz(k)
      return
      end

      double precision function brmsfz(k)
      implicit none
      double precision emkloc, delta, sb1, sb2, dlog, ee
      double precision k
      include 'pegscommons/lbremz.f'
      emkloc=E-k
      if (emkloc.eq.0.0) then
        emkloc=1.D-25
      end if
      delta=DELC*k/emkloc
      if (delta.ge.DELTAM) then
        brmsfz=0.0
      else
        if (delta.le.1.) then
          sb1=20.867+delta*(-3.242+delta*0.625)-XLNZ
          sb2=20.209+delta*(-1.930+delta*(-0.086))-XLNZ
        else
          sb1=21.12-4.184*dlog(delta+0.952)-XLNZ
          sb2=sb1
        end if
        ee=emkloc/E
        brmsfz=CONST*((1.+ee*ee)*sb1-0.666667*ee*sb2)
      end if
      return
      end

      double precision function brmsrm(e,k1,k2)
      implicit none
      integer i
      double precision brmsrz, e
      double precision k1,k2
      include 'pegscommons/mixdat.f'
      include 'pegscommons/mxdatc.f'
      brmsrm=0.
      do i=1,ne
        brmsrm=brmsrm+PZ(I)*brmsrz(z(i),e,k1,k2)
      end do
      return
      end

      double precision function brmsrz(z,e,k1,k2)
      implicit none
      double precision dummy, brmsdz, z, e, qd
      double precision k1,k2
      external brmsfz
      dummy=brmsdz(z,e,k1)
      brmsrz=qd(brmsfz,k1,k2,'brmsfz')
      return
      end

      double precision function brmstm(e0,eg)
      implicit none
      double precision e0, au, eg, brmsrm
      include 'pegscommons/dercon.f'
      if (e0.le.RM) then
        brmstm=0.
      else
        au=dmin1(eg,e0-RM)
        brmstm=brmsrm(e0,0.D0,AU)
      end if
      return
      end

      subroutine cfuns(e,v)
      implicit none
      double precision aintp, e
      include 'pegscommons/bcom.f'
      include 'pegscommons/cpcom.f'
      double precision v(1)
      v(1)=aintp(e,QCAP(1),31,AVCPRF(1),1,.true.,.true.)
      return
      end

      subroutine cfuns2(e,v)
      implicit none
      double precision aintp, e
      include 'pegscommons/bcom.f'
      include 'pegscommons/cpcom.f'
      double precision v(1)
      v(1)=aintp(e,CPROFI(1),301,QCAP10(1),1,.true.,.false.)
      return
      end

      subroutine cfuns3(e,v)
      implicit none
      integer ishell
      double precision aintp, e
      include 'pegscommons/bcom.f'
      include 'pegscommons/cpcom.f'
      double precision v(200)
      do ishell=1,MXSHEL
        v(ishell)=aintp(e,SCPROI(1,ISHELL),301,QCAP10(1),1,.true.,.false
     *  .)
      end do
      return
      end

      subroutine cfuns4(e,v)
      implicit none
      integer ishell
      double precision aintp, e
      include 'pegscommons/bcom.f'
      include 'pegscommons/cpcom.f'
      double precision v(200)
      do ishell=1,mxshel
        v(ishell)=aintp(e,QCAP(1),31,SCPSUM(1,ISHELL),1,.true.,.true.)
      end do
      return
      end

      double precision function cohetm(k)
      implicit none
      integer i
      double precision cohetz, cohetzint
      double precision k
      include 'pegscommons/mixdat.f'
      include 'pegscommons/mxdatc.f'
      include 'pegscommons/cohcom.f'
      cohetm=0.d0
      if (irayl.eq.1) then
        do i=1,NE
          cohetm=cohetm+PZ(I)*cohetz(z(i),k)
        end do
        return
      else if(irayl.eq.2) then
        cohetm=cohetzint(1.d0,k)
        return
      end if
      end

      double precision function cohetz(z,k)
      implicit none
      integer iz
      double precision pcon, z, aintp
      double precision k
      include 'pegscommons/molvar.f'
      include 'pegscommons/phpair.f'
      include 'pegscommons/pmcons.f'
      include 'pegscommons/cohcom.f'
      pcon= 1.D-24*(AN*RHO/WM)*RLC
      iz=z
      cohetz=pcon*aintp(k,PHE(1,iz),NPHE(iz),COHE(1,iz),1,.true.,.true.)
      return
      end

      double precision function cohetzint(z,k)
      implicit none
      integer iz
      double precision pcon, z, aintp
      double precision k
      include 'pegscommons/molvar.f'
      include 'pegscommons/phpair.f'
      include 'pegscommons/pmcons.f'
      include 'pegscommons/cohcom.f'
      pcon= 1.D-24*(AN*RHO/WM)*RLC
      iz=z
      cohetzint= pcon*aintp(k,PHE(1,iz),NPHE(iz),COHEINT(1,iz),1,.true.,
     *.true.)
      return
      end

      double precision function compdm(k0a,k)
      implicit none
      include 'pegscommons/pmcons.f'
      include 'pegscommons/dercon.f'
      include 'pegscommons/molvar.f'
      include 'pegscommons/lcompm.f'
      double precision k0a,k0p,compfm, k
      k0=k0a
      k0p=k0/RM
      CCOMP=RLC*EDEN*PI*R0**2/(k0*k0p)
      C1=1./k0p**2
      C2=1.-(2.+2.*k0p)/k0p**2
      C3=(1.+2.*k0p)/k0p**2
      compdm=compfm(k)
      return
      end

      double precision function compfm(k)
      implicit none
      double precision eps, epsi
      double precision k
      include 'pegscommons/lcompm.f'
      eps=k/K0
      epsi=1./eps
      compfm=CCOMP*( (C1*epsi+C2)*epsi+C3+eps )
      return
      end

      double precision function comprm(k0,k1,k2)
      implicit none
      double precision ccomp2
      include 'pegscommons/pmcons.f'
      include 'pegscommons/dercon.f'
      include 'pegscommons/molvar.f'
      double precision k0,k1,k2
      real*8 c1,c2,c3,eps1,eps2,k0p
      k0p=k0/RM
      ccomp2=RLC*EDEN*PI*R0**2/k0p
      c1=1./k0p**2
      c2=1.-(2.+2.*k0p)/k0p**2
      c3=(1.+2.*k0p)/k0p**2
      eps1=k1/k0
      eps2=k2/k0
      comprm=ccomp2*(c1*(1./eps1-1./eps2)+c2*dlog(eps2/eps1)+eps2* (c3+0
     *.5*eps2) - eps1*(c3+0.5*eps1) )
      return
      end

      double precision function comptm(k0)
      implicit none
      integer i, iz
      double precision pcon, comsum, aintp, comprm
      include 'pegscommons/bcom.f'
      include 'pegscommons/dercon.f'
      include 'pegscommons/mixdat.f'
      include 'pegscommons/mxdatc.f'
      include 'pegscommons/phpair.f'
      include 'pegscommons/pmcons.f'
      include 'pegscommons/molvar.f'
      double precision k0,k1
      if (IBOUND.eq.1) then
        pcon=1.D-24*(AN*RHO/WM)*RLC
        comsum=0.0
        do i=1,NE
          iz=Z(i)
          comsum=comsum+PZ(i)*aintp(K0,PBC(1),NPBC,BCOMP(1,iz),1,.true.,
     *    .true.)
        end do
        comptm=pcon*comsum
      else
        K1=K0*RM/(RM+2.*K0)
        comptm=comprm(K0,K1,K0)
      end if
      return
      end

      double precision function cprfil(x)
      implicit none
      double precision aintp, x
      include 'pegscommons/cpcom.f'
      cprfil=aintp(x,QCAP(1),31,AVCPRF(1),1,.true.,.true.)
      return
      end

      double precision function cratio(e)
      implicit none
      double precision tot, pairtu, e, comptm, photte, cohetm
      tot=pairtu(e)+comptm(e)+photte(e)
      cratio=tot/(tot+cohetm(e))
      return
      end

      double precision function dcadre(f,a,b,aerr,rerr,error,ier)
      implicit none
      integer maxts, maxtbl, mxstge, ier, istage, ibeg, iend, l, n,
     & lm1, n2, istep, ii, iii, i, istep2, it, ibegs, nnleft
      external f
      dimension t(10,10),r(10),ait(10),dif(10),rn(4),ts(2049)
      dimension ibegs(30),begin(30),finis(30),est(30)
      dimension reglsv(30)
      logical h2conv,aitken,right,reglar,reglsv
      double precision t,r,ait,dif,rn,ts,begin,finis,est,aitlow
      double precision h2tol,aittol,length,jumptl,zero,p1,half,one
      double precision two,four,fourp5,ten,hun,cadre,error,a,b
      double precision aerr,rerr,stepmn,stepnm,stage,curest,fnsize
      double precision prever,beg,fbeg,end,fend,step,astep,tabs,hovn
      double precision fn,sum,sumabs,absi,vint,tabtlm,ergl,ergoal
      double precision erra,errr,fextrp,errer,diff,sing,fextm1,alg4o2
      double precision h2nxt,singnx,slope,fbeg2,alpha
      double precision erret,h2tfex,fi
      double precision rval,f
      data aitlow,h2tol,aittol,jumptl,maxts,maxtbl,mxstge/1.1D0,.15D0, .
     *1D0,.01D0,2049,10,30/
      data rn(1),rn(2),rn(3),rn(4)/.7142005D0,.3466282D0,.843751D0, .126
     *3305D0/
      data zero,p1,half,one,two,four,fourp5,ten,hun/0.0D0,0.1D0,0.5D0, 1
     *.0D0,2.0D0,4.0D0,4.5D0,10.0D0,100.0D0/
      alg4o2=dlog10(TWO)
      cadre=zero
      error=zero
      curest=zero
      vint=zero
      ier=0
      length=dabs(B-A)
      if (length.eq.zero) go to 215
      if (rerr.gt.p1.or.rerr.lt.zero) go to 210
      if (aerr.eq.zero.and.(rerr+hun).le.hun) go to 210
      errr=rerr
      erra=dabs(AERR)
      stepmn=(length/float(2**mxstge))
      stepnm=dmax1(length,dabs(A),abs(B))*TEN
      stage=half
      istage=1
      fnsize=zero
      prever=zero
      reglar=.false.
      beg=A
      rval=beg
      fbeg=f(rval)*half
      ts(1)=fbeg
      ibeg=1
      end=B
      rval=end
      fend=f(rval)*half
      ts(2)=fend
      iend=2
5     right=.false.
10    step=end - beg
      astep=dabs(step)
      if (astep.lt.stepmn) go to 205
      if (stepnm+astep.eq.stepnm) go to 205
      t(1,1)=fbeg + fend
      tabs=dabs(fbeg) + dabs(fend)
      l=1
      n=1
      h2conv=.false.
      aitken=.false.
15    lm1=l
      l=l + 1
      n2=n + n
      fn=n2
      istep=(iend - ibeg)/n
      if (istep.gt.1) go to 25
      ii=iend
      iend=iend + n
      if (iend.gt.maxts) go to 200
      hovn=step/fn
      iii=iend
      fi=one
      do i=1,n2,2
        ts(iii)=ts(ii)
        rval=end-fi*hovn
        ts(iii-1)=f(rval)
        fi=fi+two
        iii=iii-2
        ii=ii-1
      end do
      istep=2
25    istep2=ibeg + istep/2
      sum=zero
      sumabs=zero
      do i=istep2,iend,istep
        sum=sum + ts(i)
        sumabs=sumabs + dabs(ts(i))
      end do
      t(l,1)=t(l-1,1)*half+sum/fn
      tabs=tabs*half+sumabs/fn
      absi=astep*tabs
      n=n2
      it=1
      vint=step*t(l,1)
      tabtlm=tabs*ten
      fnsize=dmax1(fnsize,dabs(t(l,1)))
      ergl=astep*fnsize*ten
      ergoal=stage*dmax1(erra,errr*dabs(curest+vint))
      fextrp=one
      do i=1,lm1
        fextrp=fextrp*four
        t(i,l)=t(l,i) - t(l-1,i)
        t(l,i+1)=t(l,i) + t(i,l)/(fextrp-one)
      end do
      errer=astep*dabs(t(1,l))
      if (l.gt.2) go to 40
      if (tabs+p1*dabs(t(1,2)).eq.tabs) go to 135
      go to 15
40    do i=2,lm1
      diff=zero
      if (tabtlm+dabs(t(i-1,l)).ne.tabtlm) diff=t(i-1,lm1)/t(i-1,l)
      t(i-1,lm1)=diff
      end do
      if (dabs(four-t(1,lm1)).le.h2tol) go to 60
      if (t(1,lm1).eq.zero) go to 55
      if (dabs(two-abs(t(1,lm1))).lt.jumptl) go to 130
      if (l.eq.3) go to 15
      h2conv=.false.
      if (dabs((t(1,lm1)-t(1,l-2))/t(1,lm1)).le.aittol) go to 75
50    if (reglar) go to 55
      if (l.eq.4) go to 15
55    if(errer.gt.ergoal.and.(ergl+errer).ne.ergl) go to 175
      go to 145
60    if(h2conv) go to 65
      aitken=.false.
      h2conv=.true.
65    fextrp=four
70    it=it + 1
      vint=step*t(l,it)
      errer=dabs(step/(fextrp-one)*t(it-1,l))
      if (errer.le.ergoal) go to 160
      if (ergl+errer.eq.ergl) go to 160
      if (it.eq.lm1) go to 125
      if (t(it,lm1).eq.zero) go to 70
      if (t(it,lm1).le.fextrp) go to 125
      if (dabs(t(it,lm1)/four-fextrp)/fextrp.lt.aittol) 
     *                                     fextrp=fextrp*four
      go to 70
75    if(t(1,lm1).lt.aitlow) go to 175
      if (aitken) go to 80
      h2conv=.false.
      aitken=.true.
80    fextrp=t(l-2,lm1)
      if (fextrp.gt.fourp5) go to 65
      if (fextrp.lt.aitlow) go to 175
      if (dabs(fextrp-t(l-3,lm1))/t(1,lm1).gt.h2tol) go to 175
      sing=fextrp
      fextm1=one/(fextrp - one)
      ait(1)=zero
      do i=2,l
      ait(i)=t(i,1) + (t(i,1)-t(i-1,1))*fextm1
      r(i)=t(1,i-1)
      dif(i)=ait(i) - ait(i-1)
      end do
      it=2
90    vint=step*ait(l)
      errer=errer*fextm1
      if (errer.gt.ergoal.and.(ergl+errer).ne.ergl) go to 95
      alpha=dlog10(sing)/alg4o2 - one
      ier=max0(ier,65)
      go to 160
95    it=it + 1
      if (it.eq.lm1) go to 125
      if (it.gt.3) go to 100
      h2nxt=four
      singnx=sing+sing
100   if(h2nxt.lt.singnx) go to 105
      fextrp=singnx
      singnx=singnx+singnx
      go to 110
105   fextrp=h2nxt
      h2nxt=four*h2nxt
110   do i=it,lm1
      r(i+1)=zero
      if (tabtlm+dabs(dif(i+1)).ne.tabtlm) r(i+1)=dif(i)/dif(i+1)
      end do
      h2tfex=-h2tol*fextrp
      if (r(l)-fextrp.lt.h2tfex) go to 125
      if (r(l-1)-fextrp.lt.h2tfex) go to 125
      errer=astep*dabs(dif(l))
      fextm1=one/(fextrp - one)
      do i=it,l
      ait(i)=ait(i) + dif(i)*fextm1
      dif(i)=ait(i) - ait(i-1)
      end do
      go to 90
125   fextrp=dmax1(prever/errer,aitlow)
      prever=errer
      if (l.lt.5) go to 15
      if (l-it.gt.2.and.istage.lt.mxstge) go to 170
      erret=errer/(fextrp**(maxtbl-l))
      if (erret.gt.ergoal.and.(ergl+erret).ne.ergl) go to 170
      go to 15
130   if(errer.gt.ergoal.and.(ergl+errer).ne.ergl) go to 170
      diff=dabs(t(1,l))*(fn+fn)
      go to 160
135   slope=(fend-fbeg)*two
      fbeg2=fbeg+fbeg
      do i=1,4
      rval=beg+rn(i)*step
      diff=dabs(f(rval) - fbeg2-rn(i)*slope)
      if (tabtlm+diff.ne.tabtlm) go to 155
      end do
      go to 160
145   slope=(fend-fbeg)*two
      fbeg2=fbeg+fbeg
      i=1
150   rval=beg+rn(i)*step
      diff=dabs(f(rval) - fbeg2-rn(i)*slope)
155   errer=dmax1(errer,astep*diff)
      if (errer.gt.ergoal.and.(ergl+errer).ne.ergl) go to 175
      i=i+1
      if (i.le.4) go to 150
      ier=66
160   cadre=cadre + vint
      error=error + errer
      if (right) go to 165
      istage=istage - 1
      if (istage.eq.0) go to 220
      reglar=reglsv(istage)
      beg=begin(istage)
      end=finis(istage)
      curest=curest - est(istage+1) + vint
      iend=ibeg - 1
      fend=ts(iend)
      ibeg=ibegs(istage)
      go to 180
165   curest=curest + vint
      stage=stage+stage
      iend=ibeg
      ibeg=ibegs(istage)
      end=beg
      beg=begin(istage)
      fend=fbeg
      fbeg=ts(ibeg)
      go to 5
170   reglar=.true.
175   if(istage.eq.mxstge) go to 205
      if (right) go to 185
      reglsv(istage+1)=reglar
      begin(istage)=beg
      ibegs(istage)=ibeg
      stage=stage*half
180   right=.true.
      beg=(beg+end)*half
      ibeg=(ibeg+iend)/2
      ts(ibeg)=ts(ibeg)*half
      fbeg=ts(ibeg)
      go to 10
185   nnleft=ibeg - ibegs(istage)
      if (iend+nnleft.ge.maxts) go to 200
      iii=ibegs(istage)
      ii=iend
      do i=iii,ibeg
      ii=ii + 1
      ts(ii)=ts(i)
      end do
      do i=ibeg,ii
      ts(iii)=ts(i)
      iii=iii + 1
      end do
      iend=iend + 1
      ibeg=iend - nnleft
      fend=fbeg
      fbeg=ts(ibeg)
      finis(istage)=end
      end=beg
      beg=begin(istage)
      begin(istage)=end
      reglsv(istage)=reglar
      istage=istage + 1
      reglar=reglsv(istage)
      est(istage)=vint
      curest=curest + est(istage)
      go to 5
200   ier=131
      go to 215
205   ier=132
      go to 215
210   ier=133
215   cadre=curest + vint
220   dcadre=cadre
      return
      end

      subroutine differ
      implicit none
      double precision al183, f10, f20, a1den, a2den, b1den, b2den,
     & c1den, c2den
      include 'pegscommons/molvar.f'
      include 'pegscommons/bremp2.f'
      include 'pegscommons/dbrpr.f'
      include 'pegscommons/dercon.f'
      include 'pegscommons/radlen.f'
      al2 = dlog(2.D0)
      al183= dlog(a183)
      alphi(1)= al2*(4./3. + 1./(9.*al183*(1.+ZP)))
      alphi(2)= al2*(4./3. + 1./(9.*al183*(1.+ZU)))
      alfp1(1)= 2./3. - 1./(36.*al183*(1.+ZP))
      alfp1(2)= 2./3. - 1./(36.*al183*(1.+ZU))
      alfp2(1)= (1./12.)*(4./3. + 1./(9.*al183*(1+ZP)))
      alfp2(2)= (1./12.)*(4./3. + 1./(9.*al183*(1+ZU)))
      bpar(1)= alfp1(1)/(alfp1(1)+alfp2(1))
      bpar(2)= alfp1(2)/(alfp1(2)+alfp2(2))
      delcm= 136.0*dexp(ZG)*RM
      delpos(1)= (dexp((21.12+4.*ZG)/4.184)-0.952)/DELCM
      delpos(2)= (dexp((21.12+4.*ZV)/4.184)-0.952)/DELCM
      f10=4.*al183
      f20=f10 - 2./3.
      a1den =3.0*f10- f20 + 8.0*ZG
      a2den =3.0*f10- f20 + 8.0*ZV
      b1den = f10 + 4.0*ZG
      b2den = f10 + 4.0*ZV
      c1den = 3.0*f10+ f20 + 16.0*ZG
      c2den = 3.0*f10+ f20 + 16.0*ZV
      dl1(1)= (3.0*20.867-20.209+8.0*ZG)/a1den
      dl2(1)= (3.0*(-3.242)-(-1.930))/a1den
      dl3(1)= (3.0*(0.625)-(0.086))/a1den
      dl4(1)= (2.0*21.12+8.0*ZG)/a1den
      dl5(1)= 2.0*(-4.184)/a1den
      dl6(1)= 0.952
      dl1(4)= (3.0*20.867-20.209+8.0*ZV)/a2den
      dl2(4)= (3.0*(-3.242)-(-1.930))/a2den
      dl3(4)= (3.0*(0.625)-(0.086))/a2den
      dl4(4)= (2.0*21.12+8.0*ZV)/a2den
      dl5(4)= 2.0*(-4.184)/a2den
      dl6(4)= 0.952
      dl1(2)= (20.867+4.0*ZG)/b1den
      dl2(2)= -3.242/b1den
      dl3(2)= 0.625/b1den
      dl4(2)= (21.12+4.0*ZG)/b1den
      dl5(2)= -4.184/b1den
      dl6(2)= 0.952
      dl1(5)= (20.867+4.0*ZV)/b2den
      dl2(5)= -3.242/b2den
      dl3(5)= 0.625/b2den
      dl4(5)= (21.12+4.0*ZV)/b2den
      dl5(5)= -4.184/b2den
      dl6(5)= 0.952
      dl1(3)= (3.0*20.867+20.209+16.0*ZG)/c1den
      dl2(3)= (3.0*(-3.242)+(-1.930))/c1den
      dl3(3)= (3.0*0.625+(-0.086))/c1den
      dl4(3)= (4.0*21.12+16.0*ZG)/c1den
      dl5(3)= 4.0*(-4.184)/c1den
      dl6(3)= 0.952
      dl1(6)= (3.0*20.867+20.209+16.0*ZV)/c2den
      dl2(6)= (3.0*(-3.242)+(-1.930))/c2den
      dl3(6)= (3.0*0.625+(-0.086))/c2den
      dl4(6)= (4.0*21.12+16.0*ZV)/c2den
      dl5(6)= 4.0*(-4.184)/c2den
      dl6(6)= 0.952
      write(  26,100)
100   format(/,' In subroutine differ:'// ' Differential cross-section d
     *ata,common brempr'/ ' dl1(6),dl2(6),dl3(6),dl4(6),dl5(6),dl6(6),al
     *phi(2),bpar(2),', 'delcm,delpos(2)')
      write(  26,110) dl1,dl2,dl3,dl4,dl5,dl6,alphi,bpar,delcm,delpos
110   format(1X,6E14.5)
      return
      end

      double precision function ebind(e)
      implicit none
      integer i, j
      double precision phottz, e, stot, photte
      include 'pegscommons/mixdat.f'
      include 'pegscommons/mxdatc.f'
      include 'pegscommons/phpair.f'
      ebind=0.0
      do i=1,NE
        j=z(i)
        ebind=ebind+PZ(i)*phottz(z(i),e)*EKEDGE(j)*0.001
      end do
      stot=photte(e)
      if (stot.ne.0.0) ebind=ebind/stot
      return
      end

      double precision function ebr1(e)
      implicit none
      double precision brem, bremtm, e, tebr, amoltm
      brem=bremtm(e)
      tebr=brem+amoltm(e)
      if (tebr.gt.0.0) then
        ebr1=brem/tebr
      else
        ebr1=0.0
      end if
      return
      end

      double precision function ededx(e)
      implicit none
      double precision sptote, e
      include 'pegscommons/thres2.f'
      ededx=sptote(e,ae,ap)
      return
      end

      subroutine efuns(e,v)
      implicit none
      double precision brem, bremtm, e, amoll, amoltm, bhab, bhabtm,
     & annih, anihtm, esig, psig, sptote, sptotp, tmxs, g1e, k1e, 
     & csdar, estepmax
      double precision v(15)
      include 'pegscommons/thres2.f'
      include 'pegscommons/legacy.f'
      if (iunrst.eq.0 .or. iunrst.eq.1 .or. iunrst.eq.5) then
        brem=bremtm(e)
        amoll=amoltm(e)
        bhab=bhabtm(e)
        annih=anihtm(e)
        esig=brem+amoll
        v(1)=esig
        psig=brem+bhab+annih
        v(2)=psig
        v(3)=sptote(e,AE,AP)
        v(4)=sptotp(e,AE,AP)
        if (esig.gt.0.0) then
          v(5)=brem/esig
        else
          if (thbrem.le.thmoll) then
            v(5)=1.0
          else
            v(5)=0.0
          end if
        end if
        v(6)=brem/psig
        v(7)=(brem+bhab)/psig
        v(8)=tmxs(e)
        v(9) = g1e(-1,e,0)
        v(10) = g1e(+1,e,0)
        if(oldK1run) then
          v(11) = k1e(-1,e)
          v(12) = k1e(+1,e)
        else
          v(11) = 0.d0
          v(12) = 0.d0
        endif
        v(13) = csdar(-1,e)
        v(14) = csdar(+1,e)
        v(15) = estepmax(e)
      else if (iunrst.eq.2) then
        v(1)=0.0
        v(2)=0.0
        v(5)=0.0
        v(6)=0.0
        v(7)=0.0
        v(3) = sptote(e,e,e)
        v(4) = sptotp(e,e,e)
        v(8) = tmxs(e)
        v(9) = g1e(-1,e,0)
        v(10) = g1e(+1,e,0)
        if(oldK1run) then
          v(11) = k1e(-1,e)
          v(12) = k1e(+1,e)
        else
          v(11) = 0.d0
          v(12) = 0.d0
        endif
        v(13) = csdar(-1,e)
        v(14) = csdar(+1,e)
        v(15) = estepmax(e)
      else if (iunrst.eq.3) then
        brem=bremtm(e)
        annih=anihtm(e)
        v(1)=brem
        v(2)=brem + annih
        v(3)=sptote(e,e,AP)
        v(4)=sptotp(e,e,AP)
        v(5)=1.0
        v(6)=brem/v(2)
        v(7)=v(6)
        v(8)=tmxs(e)
        v(9) = g1e(-1,e,0)
        v(10) = g1e(+1,e,0)
        if(oldK1run) then
          v(11) = k1e(-1,e)
          v(12) = k1e(+1,e)
        else
          v(11) = 0.d0
          v(12) = 0.d0
        endif
        v(13) = csdar(-1,e)
        v(14) = csdar(+1,e)
        v(15) = estepmax(e)
      else if (iunrst.eq.4) then
        v(1)=amoltm(e)
        v(2)=bhabtm(e)
        v(3)=sptote(e,AE,e)
        v(4)=sptotp(e,AE,e)
        v(5)=0.0
        v(6)=0.0
        v(7)=1.0
        v(8)=tmxs(e)
        v(9) = g1e(-1,e,0)
        v(10) = g1e(+1,e,0)
        if(oldK1run) then
          v(11) = k1e(-1,e)
          v(12) = k1e(+1,e)
        else
          v(11) = 0.d0
          v(12) = 0.d0
        endif
        v(13) = csdar(-1,e)
        v(14) = csdar(+1,e)
        v(15) = estepmax(e)
      else
        write(  26,100) iunrst
100     format(//'*********Iunrst=',I4,' not allowed by efuns*****'/ ' I
     *unrst=6 or 7 only allowed with call or pltn options'//)
        close(26)
        stop
      end if
      return
      end

      subroutine eiifuns(e,v)
      implicit none
      integer i
      double precision amoll, amoltm, e, eiisum, zval, eiitm
      double precision v(20)
      include 'pegscommons/mixdat.f'
      include 'pegscommons/mxdatc.f'
      do i=1,20
        v(i)=0.0
      end do
      amoll=amoltm(e)
      if (amoll.lt.1.0d-30) return
      eiisum=0.0
      do i=1,NE
        zval=Z(I)
        eiisum=eiisum+eiitm(e,zval)*PZ(i)
        v(i)=eiisum/amoll
      end do
      return
      end

      double precision function eiitm(e,zval)
      implicit none
      integer j, nismall
      double precision zval, ekbmev, x, e, capi, cape, fr1, fr2, fr3,
     & fr4, rfact, smalla0, capi0, capu, smalld0, smalld1, smalld2,
     & smallb0, smallb1, smallb2, sphi, spsi, dexp, qcap, dlog, cape1,
     & cape2, smalph, eke0, qcapa, qdist2, qclose, qdist, beta2a,
     & beta2, beta02, fcap1, fcap2, fcap3, fcap4, fcap5, sma, smb,
     & smc, qconst, g1, g2
      include 'pegscommons/dercon.f'
      include 'pegscommons/eimpact.f'
      include 'pegscommons/phpair.f'
      include 'pegscommons/pmcons.f'
      include 'pegscommons/mixdat.f'
      include 'pegscommons/mxdatc.f'
      include 'pegscommons/molvar.f'
      J=ZVAL
      ekbmev=ekedge(j)*0.001
      if (ekbmev.eq.0.0) then
        eiitm=0.0
        return
      end if
      x=(e-RM)/ekbmev
      nismall=2
      if (x.gt.1.001) then
        capi=ekedge(j)/RM/1000.0
        cape=(e-RM)/RM
        fr1=(2.0+capi)/(2.0+cape)
        fr2=(1.0+cape)/(1.0+capi)
        fr3=(capi+cape)*(2.0+cape)*(1.0+capi)**2
        fr4=cape*(2.0+cape)*(1.0+capi)**2+capi*(2.0+capi)
        rfact=fr1*fr2**2*(fr3/fr4)**1.5
        if (impact.eq.1) then
          smalla0=5.292E3
          capi0=13.606D-3
          capu=(e-RM)/(EKEDGE(j)*0.001)
          smalld0=-0.0318
          smalld1=0.3160
          smalld2=-0.1135
          smallb0=10.57
          smallb1=-1.736
          smallb2=0.317
          sphi=(EKEDGE(j)/capi0)**(smalld0+smalld1/capu+smalld2/capu**2)
          spsi=smallb0*dexp(smallb1/capu+smallb2/capu**2)
          qcap=nismall*smalla0**2*rfact*(capi0/EKEDGE(j))**2*sphi*spsi *
     *    dlog(capu)/capu
        end if
        if (impact.eq.2) then
          cape=(e-RM)/RM
          cape1=cape+1.0
          cape2=cape+2.0
          capi=ekbmev/RM
          smalph=1.0/137.036
          eke0=0.5*(smalph*ZVAL)**2*RM*1000.0
          qcapa=cape1*cape1/capi/cape/cape2
          qdist2=0.275*(eke0/EKEDGE(j))**3*((1.-16./13.*(1.-EKEDGE(j)/ek
     *    e0))* (dlog(2.*cape*cape2/capi)-cape*cape2/(cape1*cape1))-55./
     *    78.- 32./39.*(1.-EKEDGE(j)/eke0))
          qclose=0.99*(1.0-capi/cape*(1.0-cape*cape/2.0/cape1/cape1+ (2.
     *    0*cape+1.0)/cape1/cape1*dlog(cape/capi)))
          qcap=qcapa*(qdist2+qclose)
        end if
        if (impact.eq.3) then
          cape=(e-RM)/RM
          cape1=cape+1.0
          cape2=cape+2.0
          capi=ekbmev/RM
          qcapa=cape1*cape1/capi/cape/cape2
          qdist=0.275*(dlog(1.19*cape*cape2/capi)-cape*cape2/(cape1*cape
     *    1))
          qclose=0.99*(1.0-capi/cape*(1.0-cape*cape/2.0/cape1/cape1+ (2.
     *    0*cape+1.0)/cape1/cape1*dlog(cape/capi)))
          qcap=qcapa*(qdist+qclose)
        end if
        if (impact.eq.4) then
          beta2a=(1.0+(e-RM)/RM)**(-2)
          beta2=1.0-beta2a
          beta02=1.0-(1.0+EKEDGE(j)/(RM*1000))**(-2)
          fcap1=254.9/(EKEDGE(j)*beta2)
          fcap2=dlog(beta2/beta2a)-beta2
          fcap3=1.0-beta02/beta2
          fcap4=dlog(1.0/beta02)
          fcap5=beta02/beta2
          sma=5.14*ZVAL**(-0.48)
          smb=5.76-0.04*ZVAL
          smc=0.72+0.039*ZVAL-0.0006*ZVAL**2
          qcap=sma*fcap1*(fcap2+smb*fcap3+fcap4*fcap5**smc)
        end if
        if (impact.eq.5.or.impact.eq.6) then
          qconst=0.0656
          g1=1.0/x*((x-1.0)/(x+1.0))**1.5
          g2=1.0+0.6667*(1.0-0.5/x)*dlog(2.7+dsqrt(x-1.d0))
          qcap=qconst*nismall/ekbmev**2*g1*g2
          if (impact.eq.6) then
            qcap=qcap*rfact
          end if
        end if
        if (qcap.lt.0.0) then
          qcap=0.0
        end if
        eiitm=qcap*AN*1.0D-24/WM*RHO*RLC
      else
        eiitm=0.0
      end if
      return
      end

      double precision function esig(e)
      implicit none
      double precision bremtm, e, amoltm
      esig=bremtm(e)+amoltm(e)
      return
      end

      double precision function fcoulc(z)
      implicit none
      double precision asq, z
      include 'pegscommons/dercon.f'
      asq=(FSC*z)**2
      fcoulc = asq*(1.0/(1.0+asq)+0.20206+asq*(-0.0369+ asq*(0.0083+asq*
     *(-0.002))))
      return
      end

      double precision function fi(i,x1,x2,x3,x4)
      implicit none
      integer i
      double precision alin, x1, alini, adfmol, adimol, addmol, dlog,
     & dexp, arec, alke, alkei, amoldm, x2, amolfm, amolrm, x3,
     & amoltm, anihdm, anihfm, anihrm, anihtm, aprim, bhabdm, bhabfm,
     & bhabrm, bhabtm, bremdr, bremfr, bremdz, brmsdz, bremfz, brmsfz,
     & bremrr, bremrm, bremrz, x4, bremtm, bremtr, brmsrm, brmsrz,
     & brmstm, cohetm, cohetz, compdm, compfm, comprm, comptm, cratio,
     & ebind, ebr1, ededx, eiitm, esig, fcoulc, gbr1, gbr2, gmfp,
     & pairdr, pairfr, pairdz, pairfz, pairrm, pairrr, pairrz, pairte,
     & pairtm, pairtr, pairtu, pairtz, pbr1, pbr2, pdedx, phottz,
     & photte, psig, spione, spionp, sptote, sptotp, tmxb, tmxs,
     & tmxde2, xsif
      go to(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,
     *24,25,26,27,28,29,30,31,32,33,34,35,36,37,38,39,40,41,42,43,44,45,
     *46,47,48,49,50,51,52,53,54,55,56,57,58,59,60,61,62,63,64,65,66,67,
     *68,69,70,71,72,73,74,75,76,77,78,79),i
1     fi=alin(x1)
      return
2     fi=alini(x1)
      return
3     fi=adfmol(x1)
      return
4     fi=adimol(x1)
      return
5     fi=addmol(x1)
      return
6     fi=dlog(x1)
      return
7     fi=dexp(x1)
      return
8     fi=arec(x1)
      return
9     fi=alke(x1)
      return
10    fi=alkei(x1)
      return
11    fi=amoldm(x1,x2)
      return
12    fi=amolfm(x1)
      return
13    fi=amolrm(x1,x2,x3)
      return
14    fi=amoltm(x1)
      return
15    fi=anihdm(x1,x2)
      return
16    fi=anihfm(x1)
      return
17    fi=anihrm(x1,x2,x3)
      return
18    fi=anihtm(x1)
      return
19    fi=aprim(x1,x2)
      return
20    fi=bhabdm(x1,x2)
      return
21    fi=bhabfm(x1)
      return
22    fi=bhabrm(x1,x2,x3)
      return
23    fi=bhabtm(x1)
      return
24    fi=bremdr(x1,x2)
      return
25    fi=bremfr(x1)
      return
26    fi=bremdz(x1,x2,x3)
      return
27    fi=brmsdz(x1,x2,x3)
      return
28    fi=bremfz(x1)
      return
29    fi=brmsfz(x1)
      return
30    fi=bremrr(x1,x2,x3)
      return
31    fi=bremrm(x1,x2,x3)
      return
32    fi=bremrz(x1,x2,x3,x4)
      return
33    fi=bremtm(x1)
      return
34    fi=bremtr(x1)
      return
35    fi=brmsrm(x1,x2,x3)
      return
36    fi=brmsrz(x1,x2,x3,x4)
      return
37    fi=brmstm(x1,x2)
      return
38    fi=cohetm(x1)
      return
39    fi=cohetz(x1,x2)
      return
40    fi=compdm(x1,x2)
      return
41    fi=compfm(x1)
      return
42    fi=comprm(x1,x2,x3)
      return
43    fi=comptm(x1)
      return
44    fi=cratio(x1)
      return
45    fi=ebind(x1)
      return
46    fi=ebr1(x1)
      return
47    fi=ededx(x1)
      return
48    fi=eiitm(x1,x2)
      return
49    fi=esig(x1)
      return
50    fi=fcoulc(x1)
      return
51    fi=gbr1(x1)
      return
52    fi=gbr2(x1)
      return
53    fi=gmfp(x1)
      return
54    fi=pairdr(x1,x2)
      return
55    fi=pairfr(x1)
      return
56    fi=pairdz(x1,x2,x3)
      return
57    fi=pairfz(x1)
      return
58    fi=pairrm(x1,x2,x3)
      return
59    fi=pairrr(x1,x2,x3)
      return
60    fi=pairrz(x1,x2,x3,x4)
      return
61    fi=pairte(x1)
      return
62    fi=pairtm(x1)
      return
63    fi=pairtr(x1)
      return
64    fi=pairtu(x1)
      return
65    fi=pairtz(x1,x2)
      return
66    fi=pbr1(x1)
      return
67    fi=pbr2(x1)
      return
68    fi=pdedx(x1)
      return
69    fi=phottz(x1,x2)
      return
70    fi=photte(x1)
      return
71    fi=psig(x1)
      return
72    fi=spione(x1,x2)
      return
73    fi=spionp(x1,x2)
      return
74    fi=sptote(x1,x2,x3)
      return
75    fi=sptotp(x1,x2,x3)
      return
76    fi=tmxb(x1)
      return
77    fi=tmxs(x1)
      return
78    fi=tmxde2(x1)
      return
79    fi=xsif(x1)
      return
      end

      double precision function gbr1(e)
      implicit none
      double precision pair, pairtu, e, comptm, photte
      pair=pairtu(e)
      gbr1=pair/(pair+comptm(e)+photte(e))
      return
      end

      double precision function gbr2(e)
      implicit none
      double precision prco, pairtu, e, comptm, photte
      prco=pairtu(e)+comptm(e)
      gbr2=prco/(prco+photte(e))
      return
      end

      subroutine gfuns(e,v)
      implicit none
      double precision pair, pairtu, e, comp, comptm, phot, photte,
     & cohr, cohetm, tsansc, gmfp
      double precision v(4)
      pair=pairtu(e)
      comp=comptm(e)
      phot=photte(e)
      cohr=cohetm(e)
      tsansc=pair+comp+phot
      gmfp=1.0/tsansc
      v(1)=gmfp
      v(2)=pair*gmfp
      v(3)=(pair+comp)*gmfp
      v(4)=tsansc/(tsansc+cohr)
      return
      end

      double precision function gmfp(e)
      implicit none
      double precision pairtu, e, comptm, photte
      gmfp=1.0/(pairtu(e)+comptm(e)+photte(e))
      return
      end

      subroutine hplt1(ei,el,eh,icap,ntimes,nbins,nh,idf,idsig, irsig,it
     *sig)
      implicit none
      integer ibin, irsig, itsig, idf, nbins, ntimes, i, j, idsig, ic
      double precision amax, rtot, fi, ei, el, eh, ttot, dfh, dfl,
     & deldf, dnorm, eli, ehi, eint, v, y
      integer nh(200)
      character*4 icap(12)
      include 'pegscommons/funcs.f'
      include 'pegscommons/funcsc.f'
      character*4 l(100),cm,cr,cd,cbl
      integer ipnts
      data l/100*' '/,cm/'m'/,cr/'r'/,cd/'d'/,cbl/' '/,ipnts/10/
      ibin(y)=max0(1,min0(100,idint(y/amax*100.)+1))
      rtot=fi(irsig,ei,el,eh,0.d0)
      ttot=fi(itsig,ei,0.d0,0.d0,0.d0)
      dfh=fi(idf,eh,0.d0,0.d0,0.d0)
      dfl=fi(idf,el,0.d0,0.d0,0.d0)
      deldf=(dfh-dfl)/nbins
      dnorm=rtot/(deldf*ntimes)
      amax=0.0
      eli=el
      do i=1,nbins
        ehi=fi(idf+1,dfl+deldf*i,0.d0,0.d0,0.d0)
        amax=dmax1(amax,nh(i)*dnorm,fi(irsig,ei,eli,ehi,0.d0)/deldf)
        do j=1,ipnts
          eint=fi(idf+1,dfl+deldf*(i-1+float(j-1)/(ipnts-1)),
     *                       0.d0,0.d0,0.d0)
          amax=dmax1(amax,fi(idsig,ei,eint,0.d0,0.d0)/
     *                  fi(idf+2,eint,0.d0,0.d0,0.d0))
        end do
        eli=ehi
      end do
      write(  26,100) icap,(fname(i,idsig),i=1,6),(fname(i,irsig),
     *  i=1,6), (fname(i,itsig),i=1,6),((fname(i,idf+j-1),i=1,6),j=1,3),
     *  rtot,ttot
100   format(' HPLT functions:monte,dsig,rsig,tsig,cdf,cdfinverse,pdf=',
     * 12a1,6(',',6a1)/' rtot,ttot=',1p,2e15.5)
      write(  26,110) icap,ei,el,eh,nbins,ntimes,(nh(i),i=1,nbins)
110   formaT(' HPLT:raw egs data for routine ',12a1,',ei,elo,ehi=', 3F12
     *.3,',nbins,ntimes=',2I10,',data='/(1X,10I10))
      write(  26,120)
120   format(' Key to plot,m=montecarlo data,r=theoretical integrals', '
     * over bins,d=differential cross-section'/ '    energy          val
     *ue')
      eli=el
      do i=1,nbins
        ehi=fi(idf+1,dfl+deldf*i,0.d0,0.d0,0.d0)
        v=nh(i)*dnorm
        ic=ibin(v)
        l(ic)=cm
        write(  26,130) eli,v,l
130     format(1X,1P,2E15.5,' I',100A1)
        l(ic)=cbl
        v=fi(irsig,ei,eli,ehi,0.d0)/deldf
        ic=ibin(v)
        l(ic)=cr
        write(  26,140) eli,v,l
140     format(1X,1P,2E15.5,' i',100A1)
        l(ic)=cbl
        do j=1,ipnts
          eint=fi(idf+1,dfl+deldf*(i-1+float(j-1)/(ipnts-1)),
     *               0.d0,0.d0,0.d0)
          v=fi(idsig,ei,eint,0.d0,0.d0)/fi(idf+2,eint,0.d0,0.d0,0.d0)
          ic=ibin(v)
          l(ic)=cd
          write(  26,150) eint,v,l
150       format(1X,1P,2E15.5,' i',100A1)
          l(ic)=cbl
        end do
        eli=ehi
      end do
      return
      end

      integer function ifunt(name)
      implicit none
      integer if, j
      character*4 name(6)
      include 'pegscommons/funcs.f'
      include 'pegscommons/funcsc.f'
      do 100 if=1,nfuns
        do j=1,6
          if (name(j).ne.fname(j,if)) go to 100
        end do
        ifunt=if
        return
100   continue
      ifunt=-1
      write(  26,110) name
110   format(' FUNC=',6A1,' not matched')
      return
      end

      subroutine lay
      implicit none
      integer ip, iuecho, ie, nsge, nseke, nleke, ncmfp, nrange, nge,
     & neke, i, ifun, ishell, is
      include 'pegscommons/bremp2.f'
      include 'pegscommons/mixdat.f'
      include 'pegscommons/mxdatc.f'
      include 'pegscommons/cohcom.f'
      include 'pegscommons/rslts.f'
      include 'pegscommons/thres2.f'
      include 'pegscommons/molvar.f'
      include 'pegscommons/eimpact.f'
      include 'pegscommons/epstar.f'
      include 'pegscommons/phpair.f'
      include 'pegscommons/bcom.f'
      include 'pegscommons/cpcom.f'
      include 'pegscommons/sfcom.f'
100   FORMAT(1X,14I5)
110   FORMAT(1X,1P,5E14.5)
      ip=7
      iuecho=  26
      write(iuecho,120)
120   FORMAT(' $ECHO WRITE:MEDIUM,IDSTRN')
      write(ip,130) medium,idstrn
      write(iuecho,130) medium,idstrn
130   FORMAT(' MEDIUM=',24A1,',STERNCID=',24A1)
      if (gasp.ne.0.0) then
        write(iuecho,140)
140     FORMAT(' $ECHO WRITE:MTYP,RHO,NE,GASP, IUNRST,EPSTFL,IAPRIM')
        write(ip,150) mtyp,rho,ne,gasp, iunrst,epstfl,iaprim
        write(iuecho,150) mtyp,rho,ne,gasp, iunrst,epstfl,iaprim
150     FORMAT(1X,4A1,',RHO=',1P,E11.4,',NE=',I2,',GASP=', 1P,E11.4,', I
     *UNRST=',I1,', EPSTFL=',I1,', IAPRIM=',I1)
      else
        write(iuecho,160)
160     FORMAT(' $ECHO WRITE:MTYP,RHO,NE,IUNRST,EPSTFL,IAPRIM')
        write(ip,170) mtyp,rho,ne,iunrst,epstfl,iaprim
        write(iuecho,170) mtyp,rho,ne,iunrst,epstfl,iaprim
170     FORMAT(1X,4A1,',RHO=',1P,E11.4,',NE=',I2,', IUNRST=',I1, ', EPST
     *FL=',I1,', IAPRIM=',I1)
      end if
      do ie=1,ne
        write(iuecho,180)
180     FORMAT(' $ECHO WRITE:ASYM(IE),Z(IE),WA(IE),PZ(IE),RHOZ(IE)')
        write(ip,190) asym(ie),z(ie),wa(ie),pz(ie),rhoz(ie)
        write(iuecho,190) asym(ie),z(ie),wa(ie),pz(ie),rhoz(ie)
190     FORMAT(' ASYM=',A2,',Z=',F3.0,',A=',F9.3, ',PZ=',1P,E12.5,',RHOZ
     *=',E12.5)
      end do
      write(iuecho,200)
200   FORMAT(' $ECHO WRITE:RLC,AE,AP,UE,UP')
      write(ip,110) rlc,ae,ap,ue,up
      write(iuecho,110) rlc,ae,ap,ue,up
      nsge=0
      nseke=0
      nleke=0
      ncmfp=0
      nrange=0
      nge=ngl
      neke=nel
      write(iuecho,210)
210   FORMAT(' $ECHO WRITE:NSGE,NGE,NSEKE,NEKE,NLEKE,NCMFP,NRANGE,IRAYL,
     * IBOUND,INCOH,ICPROF,IMPACT')
      write(ip,100) nsge,nge,nseke,neke,nleke,ncmfp,nrange,irayl,ibound
     *,incoh,icprof,impact
      write(iuecho,100) nsge,nge,nseke,neke,nleke,ncmfp,nrange,irayl,
     *                  ibound,incoh,icprof,impact
      write(iuecho,220)
220   FORMAT(' $ECHO WRITE:(DL1(I),DL2(I),DL3(I),DL4(I),DL5(I),DL6(I),I=
     *1,6)')
      write(ip,110) (dl1(i),dl2(i),dl3(i),dl4(i),dl5(i),dl6(i),i=1,6)
      write(iuecho,110) (dl1(i),dl2(i),dl3(i),dl4(i),dl5(i),dl6(i),i=1,6
     *)
      write(iuecho,230)
230   FORMAT(' $ECHO WRITE:DELCM,(ALPHI(I),BPAR(I),DELPOS(I),I=1,2)')
      write(ip,110) delcm,(alphi(i),bpar(i),delpos(i),i=1,2)
      write(iuecho,110) delcm,(alphi(i),bpar(i),delpos(i),i=1,2)
      write(iuecho,240)
240   FORMAT(' $ECHO WRITE:XR0,TEFF0,BLCC,XCC')
      write(ip,110) xr0,teff0,blcc,xcc
      write(iuecho,110) xr0,teff0,blcc,xcc
      write(iuecho,250)
250   FORMAT(' $Echo write:BXE,AXE')
      write(ip,110) bxe,axe
      write(iuecho,110) bxe,axe
      write(iuecho,260)
260   FORMAT(' $Echo write:((BFE(i,ifun),AFE(i,ifun),ifun=1,15),
     *i=1,neke)')
      write(ip,110) ((bfe(i,ifun),afe(i,ifun),ifun=1,15),i=1,neke)
      write(iuecho,110) ((bfe(i,ifun),afe(i,ifun),ifun=1,15),i=1,neke)
      write(iuecho,270)
270   FORMAT(' $Echo write:EBINDA,BXG,AXG')
      write(ip,110) ebinda,bxg,axg
      write(iuecho,110) ebinda,bxg,axg
      write(iuecho,280)
280   FORMAT(' $Echo write:((bfg(i,ifun),afg(i,ifun),ifun=1,3),i=1,nge)'
     *)
      write(ip,110) ((bfg(i,ifun),afg(i,ifun),ifun=1,3),i=1,nge)
      write(iuecho,110) ((bfg(i,ifun),afg(i,ifun),ifun=1,3),i=1,nge)
      if (irayl.ne.0) then
        write(iuecho,290)
290     FORMAT(' $Echo write:ngr')
        write(ip,100) ngr
        write(iuecho,100) ngr
        write(iuecho,300)
300     FORMAT(' $Echo write:bxr,axr')
        write(ip,110) bxr,axr
        write(iuecho,110) bxr,axr
        write(iuecho,310)
310     FORMAT(' $Echo write:(bfr(i),afr(i),i=1,ngr)')
        write(ip,110) (bfr(i),afr(i),i=1,ngr)
        write(iuecho,110) (bfr(i),afr(i),i=1,ngr)
        write(iuecho,320)
320     FORMAT(' $Echo write:(bfg(i,4),afg(i,4),i=1,nge)')
        write(ip,110) (bfg(i,4),afg(i,4),i=1,nge)
        write(iuecho,110) (bfg(i,4),afg(i,4),i=1,nge)
      end if
      if (incoh.eq.1) then
        write(iuecho,330)
330     FORMAT(' $Echo write:ngs')
        write(ip,100) ngs
        write(iuecho,100) ngs
        write(iuecho,340)
340     FORMAT(' $Echo write:bxs,axs')
        write(ip,110) bxs,axs
        write(iuecho,110) bxs,axs
        write(iuecho,350)
350     FORMAT(' $Echo write:(bfs(i),afs(i),i=1,ngs)')
        write(ip,110) (bfs(i),afs(i),i=1,ngs)
        write(iuecho,110) (bfs(i),afs(i),i=1,ngs)
      end if
      if (icprof.eq.1.or.icprof.eq.2) then
        write(iuecho,360)
360     FORMAT(' $Echo write:ngc')
        write(ip,100) ngc
        write(iuecho,100) ngc
        write(iuecho,370)
370     FORMAT(' $Echo write:bxc,axc,cpimev')
        write(ip,110) bxc,axc,cpimev
        write(iuecho,110) bxc,axc,cpimev
        write(iuecho,390)
390     FORMAT(' $Echo write:(bfc(i),afc(i),i=1,ngc)')
        write(ip,110) (bfc(i),afc(i),i=1,ngc)
        write(iuecho,110) (bfc(i),afc(i),i=1,ngc)
      end if
      if (icprof.eq.3.or.icprof.eq.4) then
        write(iuecho,400)
400     FORMAT(' $Echo write:mxshel,ngcs')
        write(ip,100) mxshel,ngcs
        write(iuecho,100) mxshel,ngcs
        write(iuecho,410)
410     FORMAT(' $Echo write:(elecno(ishell),ishell=1,mxshel)')
        write(ip,110) (elecno(ishell),ishell=1,mxshel)
        write(iuecho,110) (elecno(ishell),ishell=1,mxshel)
        write(iuecho,420)
420     FORMAT(' $Echo write:(capio(ishell),ishell=1,mxshel)')
        write(ip,110) (capio(ishell),ishell=1,mxshel)
        write(iuecho,110) (capio(ishell),ishell=1,mxshel)
        write(iuecho,430)
430     FORMAT(' $Echo write:bxcs,axcs')
        write(ip,110) bxcs,axcs
        write(iuecho,110) bxcs,axcs
        write(iuecho,440)
440     FORMAT(' $Echo write:((bfcs(i,is),afcs(i,is),is=1,mxshel),i=1,ng
     *cs)')
        write(ip,110) ((bfcs(i,is),afcs(i,is),is=1,mxshel),i=1,ngcs)
        write(iuecho,110) ((bfcs(i,is),afcs(i,is),is=1,mxshel),i=1,ngcs)
      end if
      if (impact.ge.1) then
        write(iuecho,450)
450     FORMAT(' $Echo write:ne')
        write(ip,100) ne
        write(iuecho,100) ne
        write(iuecho,460)
460     FORMAT(' $Echo write:neii')
        write(ip,100) neii
        write(iuecho,100) neii
        write(iuecho,470)
470     FORMAT(' $Echo write:bxeii,axeii')
        write(ip,110) bxeii,axeii
        write(iuecho,110) bxeii,axeii
        write(iuecho,480)
480     FORMAT(' $Echo write:((bfeii(i,ifun),afeii(i,ifun),ifun=1,ne),i=
     *1,neii)')
        write(ip,110) ((bfeii(i,ifun),afeii(i,ifun),ifun=1,ne),i=1,neii)
        write(iuecho,110) ((bfeii(i,ifun),afeii(i,ifun),ifun=1,ne),i=1,
     *  neii)
      end if
      return
      end

      subroutine MIX
      implicit none
      integer i, IZZ
      double precision AL183, ZAB, FZC, FCOUL, FCOULC, XSI, XSIF, ZZX,
     & ZZ, V3120
      include 'pegscommons/mimsd.f'
      include 'pegscommons/mixdat.f'
      include 'pegscommons/mxdatc.f'
      include 'pegscommons/molvar.f'
      include 'pegscommons/pmcons.f'
      include 'pegscommons/dercon.f'
      include 'pegscommons/radlen.f'
      include 'pegscommons/mscom.f'
      dimension XSI(20),ZZX(20),FZC(20),FCOUL(20),ZZ(20)
      write(  26,100)
100   format(/' In subroutine mix: '/)
      if (GASP.eq.0.0) then
        write(  26,110) NE,RHO
110     format(' Number of elements = ',I3,',  density=',1P,G15.6,
     *         ' (g/cm**3)')
      else
        write(  26,120) NE,RHO,GASP
120     format(' Number of elements = ',I3,',  density=',1P,G15.6,
     *    ' (g/cm**3) at ntp', '  gas pressure=',1P,G15.6,' atm.')
      end if
      write(  26,130) (i,Z(i),WA(i),PZ(i),RHOZ(i),i=1,NE)
130   format('   i       Z(i)           WA(i)          PZ(i)         RHO
     *Z(i) '/ ' Index   Periodic        Atomic       Proportion     Prop
     *ortion '/ '          number         weight        by number      b
     *y weight '// (I5,1P,4G15.6))
      if (GASP.ne.0.0) then
        RHO=GASP*RHO
      end if
      AL183 = DLOG(A183)
      TPZ=0.0
      WM=0.0
      ZC=0.0
      ZT=0.0
      ZB=0.0
      ZF=0.0
      ZS=0.0
      ZE=0.0
      ZX=0.0
      ZAB=0.0
      do i=1,NE
        TPZ = TPZ + PZ(i)
        WM = WM + PZ(i)*WA(i)
        ZC = ZC + PZ(i)*Z(i)
        FZC(i) =(FSC*Z(i))**2
        FCOUL(i) = FCOULC(Z(i))
        XSI(i) = XSIF (Z(i))
        ZZX(i) = PZ(i)*Z(i)*(Z(i)+XSI(i))
        if (Z(i).le.4.0) then
          IZZ=Z(i)
          ZAB=ZAB+ZZX(i)*ALRAD(IZZ)
        else
          ZAB=ZAB+ZZX(i)*(AL183+DLOG(Z(i)**(-1./3.)))
        end if
        ZT = ZT + ZZX(i)
        ZB = ZB + ZZX(i)*dlog(Z(i)**(-1./3.))
        ZF = ZF + ZZX(i)*FCOUL(i)
        ZZ(i) = PZ(i)*Z(i)*(Z(i)+fudgeMS)
        ZS = ZS + ZZ(i)
        ZE = ZE + ZZ(i)*((-2./3.)*DLOG(Z(i)))
        ZX = ZX + ZZ(i)*DLOG(1.d0+3.34*FZC(i))
      end do
      EZ = ZC/TPZ
      ZA = AL183*ZT
      ZG = ZB/ZT
      ZP = ZB/ZA
      ZV = (ZB-ZF)/ZT
      ZU = (ZB-ZF)/ZA
      EDEN=AN*RHO/WM*ZC
      RLC = 1./( (AN*RHO/WM)*4.0*FSC*R0**2*(ZAB-ZF) )
      write(  26,140) WM,ZC,ZT,ZA,ZB,ZAB,ZF,ZG,ZP,ZV,ZU,ZS,ZE,ZX,RLC,
     *(I,XSI(I),ZZX(I),FZC(I),FCOUL(I),ZZ(I),I=1,NE)
140   format(' Z variables--WM,ZC,ZT,ZA,ZB,ZAB'/1P,6E14.6/ ' ZF,ZG,ZP,ZV
     *,ZU,ZS'/1P,6E14.6/' ZE,ZX,RLC'/1P,3E14.6/ '0(I,XSI,ZZX,FZC,FCOUL,Z
     *Z,I=1,NE)'/ (I5,1P,5E14.6))
      V3120=EDEN
      write(  26,150) V3120
150   FORMAT(' Eden=',1P,G15.7)
      BLCC= A6680*RHO*ZS*DEXP(ZE/ZS)*RLC / (WM*DEXP(ZX/ZS))
      TEFF0 = ( DEXP(BMIN)/BMIN )/BLCC
      XCC= (A22P9/RADDEG) * DSQRT( ZS*RHO*RLC/WM )
      XR0 = XCC*DSQRT(TEFF0*BMIN)
      write(  26,160) BLCC,XCC,TEFF0,XR0
160   format(' BLCC,XCC,TEFF0,XR0=',1P,4E14.5)
      return
      end

      subroutine molier
      implicit none
      integer I, IS, J, L, JLR, ITOT, IP1, IDIF, IP2, N, IFLG, I01,
     & I02, ISWP, IDA, INC, IALL, II, IXTR, ISU, ISL, IUECHO, IPUN,
     & MST
      double precision BLCMIN, B, BLCA, BA, B1, PTOT, P, Q, PPP, PP
      include 'pegscommons/mimsd.f'
      dimension P(29,16),Q(29,16),IP1(29,16),IP2(29), IXTR(29,16),IALL(2
     *9),BLCA(16),BA(16)
      double precision TH(29),DTH(29),F0(29),F1(29),F2(29),BOLD,BLC
      data TH/.05,.2,.4,.6,.8,1.,1.2,1.4,1.6,1.8,2.,2.2,2.4,2.6,2.8, 3.,
     *3.2,3.4,3.6,3.8,4.07,4.5,5.,5.5,6.13,7.,8.,9.,9.75/
      data DTH/.1,19*0.2,0.35,3*0.5,0.75,3*1.0,0.5/
      data F0/2.,1.9216,1.7214,1.4094,1.0546,.7338,.4738,.2817 ,.1546,.0
     *783,.0366,.01581,.0063,.00232,7.9D-4,2.5D-4,7.3D-5, 1.9D-5,4.7D-6,
     *1.1D-6,2.3D-7,3.D-9,2.D-11,2.D-13,5.D-16,1.D-21, 3.D-28,1.D-35,1.1
     *8D-38/
      data F1/.8456,.7038,.3437,-0.0777,-0.3981,-0.5285,-0.4770, -.3183,
     *-.1396,-.0006,+0.0782,.1054,.1008,.08262,.06247,.0455, .03288,.024
     *02,.01791,.01366,.010638,.00614,.003831,.002527, .001739,.000908,.
     *0005211,.0003208,.0002084/
      data F2/2.4929,2.0694,1.0488,-.0044,-.6068,-.6359,-.3086,.0525 ,.2
     *423,.2386,.1316,.0196,-.0467,-.0649,-.0546,-.03568,-.01923, -.0084
     *7,-.00264,5.D-5,.0010741,.0012294,.0008326,.0005368, .0003495,.000
     *1584,7.83D-5,4.17D-5,2.37D-5/
      write(  26,100) (TH(i),DTH(i),F0(i),F1(i),F2(i),i=1,29)
100   format(' Bethe table used for input'/(1X,0P,2F10.2,1P,3E18.5))
      IS=1
        go to 120
110     IS=IS+1
120     if(IS-(MSTEPS-1).gt.0) go to 160
        J=FSTEP(IS)
          go to 140
130       J=J+1
140       IF(J-(FSTEP(IS+1)-1).gt.0) go to 150
          MSMAP(J)=IS
        go to 130
150     continue
      go to 110
160   continue
      MSMAP(JRMAX)=MSTEPS
      BLCMIN = BMIN - DLOG(BMIN)
      do 260 IS=1,MSTEPS
        BLC=BLCMIN+DLOG(FSTEP(IS))
        B=BLC+DLOG(BLC)
170     continue
          BOLD=B
          B=BOLD - (BOLD-DLOG(BOLD)-BLC)/(1.0-1.0/BOLD)
          if (dabs((B-BOLD)/BOLD) .lt. 1.D-5) go to 180
        go to 170 
180     continue
        BLCA(IS)=BLC
        BA(IS)=B
        FSQR(IS)=DSQRT(FSTEP(IS)*B/BMIN)
        B1=1.0/B
        PTOT=0.0
        do i=1,29
          P(i,IS)=TH(i)*DTH(i)*(F0(i)+B1*(F1(i)+B1*F2(i)))
          PTOT=PTOT+P(i,IS)
        end do
        do i=1,29
          P(i,IS)=P(i,IS)/PTOT
        end do
        do i=1,29
          Q(i,IS)=P(i,IS)
        end do
        i=29
190     continue
          l=1
200       if(Q(i,IS).ge.0.001.or.i.le.l) go to 210
            Q(i,IS)=Q(i,IS)+Q(i-l,IS)
            Q(i-l,IS)=0.0
            l=l+1
          go to 200
210       continue
          i=i-l
          if (i.le.0) go to 220
        go to 190
220     continue
        PPP=0.5
        PP=0.5
        do JLR=1,10
          ITOT=0
          do i=1,29
            IP1(i,IS)=Q(i,IS)*1000.0+PP
            ITOT=ITOT+IP1(i,IS)
          end do
          IDIF=ITOT-1000
          if (IDIF.eq.0) go to 260
          PPP=PPP*0.5
          if (IDIF.lt.0) then
            PP=PP+PPP
          else
            PP=PP-PPP
          end if
        end do
        do i=1,29
          IP2(i)=1
        end do
        n=29
230     continue
          n=n-1
          IFLG=0
          do j=1,n
            I01=IP2(j)
            I02=IP2(j+1)
            if (IP1(I01,IS).lt.IP1(I02,IS))  then
              ISWP=IP2(j)
              IP2(j)=IP2(j+1)
              IP2(j+1)=ISWP
              IFLG=1
            end if
          end do
          if (IFLG.eq.0) go to 240
        go to 230
240     continue
        write(  26,250) ITOT
250     FORMAT(' Rounding failed, itot has',I6,' entries')
        if (IDIF.lt.0) then
          IDA=-IDIF
          INC=1
        else
          IDA=IDIF
          INC=-1
        end if
        do i=1,IDA
          I01=IP2(i)
          IP1(I01,IS)=IP1(I01,IS)+INC
        end do
260   continue
      MXV1=0
      do i=1,29
        IALL(i)=IP1(i,1)
        do is=2,MSTEPS
          IALL(i)=MIN0(IALL(i),IP1(i,is))
        end do
        MXV1=MXV1+IALL(I)
      end do
      MXV2=1000-MXV1
      ii=0
      do i=1,29
        j=1
          go to 280
270       j=j+1
280       if(j-(IALL(i)).gt.0) go to 290
          ii=ii+1
          VERT1(ii)=TH(i)
        go to 270
290     continue
      end do
      do is=1,MSTEPS
        ii=0
        do i=1,29
          IXTR(i,is)=IP1(i,is)-IALL(i)
          j=1
            go to 310
300         j=j+1
310         if(j-(IXTR(i,is)).gt.0) go to 320
            ii=ii+1
            VERT2(ii,is)=TH(i)
          go to 300
320       continue
        end do
      end do
      write(  26,330) BMIN,MSTEPS,JRMAX,MXV1,MXV2
330   format(' BMIN,MSTEPS,JRMAX,MXV1,MXV2=', F11.5,4I8)
      ISU=0
340   continue
        ISL=ISU+1
        ISU=MIN0(ISL+9,MSTEPS)
        write(  26,350) ISL,ISU
350     format('  Data for steps ',I3,' to ',I3)
        write(  26,360) (IS,IS=ISL,ISU)
360     format(11X,'ISTEP',I6,9I11)
        write(  26,370) (FSTEP(IS),IS=ISL,ISU)
370     format(11X,'FSTEP',10F11.0)
        write(  26,380) (FSQR(IS),IS=ISL,ISU)
380     format(11X,'FSQR ',10F11.5)
        write(  26,390) (BLCA(IS),IS=ISL,ISU)
390     format(11X,'BLC  ',10F11.5)
        write(  26,400) (BA (IS),IS=ISL,ISU)
400     format(11X,'B    ',10F11.5)
        write(  26,410)
410     format('0I  TH IALL')
        do i=1,29
          if ((i.eq.11).or.(i.eq.23)) then
            write(  26,420)
420         format('1I  TH IALL')
          end if
          write(  26,430) i,TH(i),IALL(i),(P(i,IS),IS=ISL,ISU)
430       format(1X,I2,F5.2,I4,' PR ',10F11.8)
          write(  26,440) (Q(I,IS),IS=ISL,ISU)
440       format(11X,'  Q  ',10F11.8)
          write(  26,450) (IP1(I,IS),IS=ISL,ISU)
450       format(11X,' IP1 ',I7,9I11)
          write(  26,460) (IXTR(I,IS),IS=ISL,ISU)
460       format(11X,'EXTRA',I7,9I11)
        end do
        if (ISU.ge.MSTEPS) go to 470
      go to 340
470   continue
480   format(1X,14I5)
490   format(1X,14F5.2)
      iuecho=  26
      ipun=7
      write(iuecho,500)
500   format(' $echo write:')
      write(ipun,510)
      write(iuecho,510)
510   format(' Material independent multiple scattering data')
      write(iuecho,520)
520   format(' $echo write:JRMAX,MSTEPS,MXV1,MXV2')
      write(ipun,480) JRMAX,MSTEPS,MXV1,MXV2
      write(iuecho,480) JRMAX,MSTEPS,MXV1,MXV2
      write(iuecho,530)
530   format(' $echo write:(FSTEP(i),FSQR(i),i=1,MSTEPS)')
      write(ipun,540) (FSTEP(i),FSQR(i),i=1,MSTEPS)
      write(iuecho,540) (FSTEP(i),FSQR(i),i=1,MSTEPS)
540   format((1X,4(F5.0,F11.6)))
      write(iuecho,550)
550   format(' $echo write:(MSMAP(I),I=1,JRMAX)')
      write(ipun,480) (MSMAP(i),i=1,JRMAX)
      write(iuecho,480) (MSMAP(i),i=1,JRMAX)
      write(iuecho,560)
560   format(' $echo write:(VERT1(I),I=1,MXV1)')
      write(ipun,490) (VERT1(i),i=1,MXV1)
      write(iuecho,490) (VERT1(i),i=1,MXV1)
      do MST=1,MSTEPS
        write(iuecho,570) MST
570     format(' MST=',I5)
        write(iuecho,580)
580     format(' $echo write:(VERT2(I,MST),I=1,MXV2)')
        write(ipun,490) (VERT2(i,MST),i=1,MXV2)
        write(iuecho,490) (VERT2(i,MST),i=1,MXV2)
      end do
      return
      end

      double precision function pairdr(ka,e)
      implicit none
      integer LS
      double precision pairfr, e
      include 'pegscommons/lpairr.f'
      double precision ka
      K=ka
      if (K.lt.50.) then
        LE=1
        LS=0
      else
        LE=2
        LS=3
      end if
      LA=LS+1
      LC=LS+3
      PAIRDR=PAIRFR(e)
      return
      end

      double precision function pairdz(Z,KA,E)
      implicit none
      double precision KA, Z, xsif, dlog, fcoulc, pairfz, E
      include 'pegscommons/pmcons.f'
      include 'pegscommons/dercon.f'
      include 'pegscommons/molvar.f'
      include 'pegscommons/lpairz.f'
      k=ka
      DELC=136.*z**(-1./3.)*RM/K
      CONST=(AN*RHO/WM)*R0**2*FSC*z*(Z+XSIF(Z))*RLC/K**3
      XLNZ=4./3.*dlog(Z)
      if (K.ge.50) XLNZ=XLNZ+4.*FCOULC(Z)
      DELTAM=dexp((21.12-XLNZ)/4.184)-0.952
      PAIRDZ=PAIRFZ(E)
      return
      end

      double precision function pairfr(E)
      implicit none
      double precision EPS, E, DEL, DELTA, A, CC
      include 'pegscommons/bremp2.f'
      include 'pegscommons/dbrpr.f'
      include 'pegscommons/lpairr.f'
      EPS=E/K
      DEL=1./(K*EPS*(1.-EPS))
      if (DEL.gt.DELPOS(LE)) then
        pairfr=0.0
      else
        DELTA=DELCM*DEL
        if (DELTA.le.1.) then
          A=DL1(LA)+DELTA*(DL2(LA)+DELTA*DL3(LA))
          CC=DL1(LC)+DELTA*(DL2(LC)+DELTA*DL3(LC))
        else
          A=DL4(LA)+DL5(LA)*DLOG(DELTA+DL6(LA))
          CC=DL4(LC)+DL5(LC)*DLOG(DELTA+DL6(LC))
        end if
        pairfr=(ALFP1(LE)*CC+ALFP2(LE)*12.*(E/K-0.5)**2*A)/K
      end if
      return
      end

      double precision function pairfz(E)
      implicit none
      double precision EPS, E, ONEEPS, DELTA, SB1, SB2, DLOG, EPLUS
      include 'pegscommons/lpairz.f'
      EPS=E/K
      ONEEPS=1.-EPS
      if (ONEEPS.eq.0.0) then
        ONEEPS=1.18D-38
      end if
      DELTA=DELC/(EPS*ONEEPS)
      if (DELTA.ge.DELTAM) then
        pairfz=0.0
      else
        if (DELTA.le.1.) then
          SB1=20.867+DELTA*(-3.242+DELTA*0.625)-XLNZ
          SB2=20.209+DELTA*(-1.930+DELTA*(-0.086))-XLNZ
        else
          SB1=21.12-4.184*DLOG(DELTA+0.952)-XLNZ
          SB2=SB1
        end if
        EPLUS=K-E
        pairfz=CONST*((E**2+EPLUS**2)*SB1+0.666667*E*EPLUS*SB2 )
      end if
      return
      end

      double precision function pairrm(K,E1,E2)
      implicit none
      integer i
      double precision pairrz, E1, E2
      double precision K
      include 'pegscommons/mixdat.f'
      include 'pegscommons/mxdatc.f'
      pairrm=0.
      do i=1,NE
        pairrm=pairrm+PZ(i)*pairrz(Z(i),K,E1,E2)
      end do
      return
      end

      double precision function pairrr(K,E1,E2)
      implicit none
      double precision DUMMY, pairdr, E1, QD, E2
      double precision K
      external pairfr
      DUMMY=pairdr(K,E1)
      PAIRRR=QD(PAIRFR,E1,E2,'PAIRFR')
      return
      end

      double precision function pairrz(Z,K,E1,E2)
      implicit none
      double precision DUMMY, pairdz, Z, E1, QD, E2
      double precision K
      external pairfz
      DUMMY=pairdz(Z,K,E1)
      pairrz=QD(PAIRFZ,E1,E2,'PAIRFZ')
      return
      end

      double precision function pairte(K)
      implicit none
      integer i
      double precision pairtz
      double precision K
      include 'pegscommons/dercon.f'
      include 'pegscommons/mixdat.f'
      include 'pegscommons/mxdatc.f'
      pairte=0.0
      if (K.le.2.0*RM) return
      do i=1,NE
        pairte=pairte+PZ(i)*pairtz(Z(i),K)
      end do
      return
      end

      double precision function pairtm(K0)
      implicit none
      double precision pairrm
      double precision K0
      include 'pegscommons/dercon.f'
      if (K0.le.2.*RM) then
        pairtm=0.0
      else
        pairtm=pairrm(K0,RM,K0-RM)
      end if
      return
      end

      double precision function pairtr(K0)
      implicit none
      double precision pairrr
      double precision K0
      include 'pegscommons/dercon.f'
      if (K0.le.2.*RM) then
        pairtr=0.0
      else
        pairtr=pairrr(K0,RM,K0-RM)
      end if
      return
      end

      double precision function pairtu(K)
      implicit none
      double precision pairte, pairtm
      double precision K
      if (K.lt.50) then
        pairtu=pairte(K)
      else
        pairtu=pairtm(K)
      end if
      return
      end

      double precision function pairtz(Z,K)
      implicit none
      integer IZ
      double precision PCON, Z, AINTP
      double precision K
      include 'pegscommons/dercon.f'
      include 'pegscommons/phpair.f'
      include 'pegscommons/pmcons.f'
      include 'pegscommons/molvar.f'
      if (K.le.RMT2) then
        pairtz=0.0
      else
        PCON=1.D-24*(AN*RHO/WM)*RLC
        IZ=Z
        pairtz=PCON*AINTP(K,PRE,17,PRD(1,IZ),1,.TRUE.,.FALSE.)
      end if
      return
      end

      double precision function pbr1(E)
      implicit none
      double precision BREM, BREMTM, E, BHABTM, ANIHTM
      BREM=BREMTM(E)
      pbr1=BREM/(BREM+BHABTM(E)+ANIHTM(E))
      return
      end

      double precision function pbr2(E)
      implicit none
      double precision BRBH, BREMTM, E, BHABTM, ANIHTM
      BRBH=BREMTM(E)+BHABTM(E)
      pbr2=BRBH/(BRBH+ANIHTM(E))
      return
      end

      double precision function pdedx(E)
      implicit none
      double precision SPTOTP, E
      include 'pegscommons/thres2.f'
      pdedx=SPTOTP(E,AE,AP)
      return
      end

      subroutine pegs5
      implicit none
      integer NOPT, NPTS, IDF, IFUN, IV, ISUB, IN, IZ, I,
     & ISSBS, ICH, IOPT, IMIXT, I01, IZZ, ILOC, ITEMP,
     & J, ISHELL, IFUNT, NA, ID, NTIMES, NBINS, IQI, IRNFLG, IBIN
      integer ibounds,incohs,icprofs,irayls,impacts,iunrsts,
     & nleg0s,epstfls,iepsts,iaprims,iaprfls
      double precision gasps,efracHs,efracLs, fudgeMSs, ievs
      integer ib, medIdx
      
      DOUBLE PRECISION VLO, VHI, EI, AFACTS, CBARS, SKS, X0S, X1S,
     & ZTBL, EBIND, AX, BX, QD, SCSUM, ZSUM, CPSUM,
     & PZSUM, STEP, CAPIL, VALUE, FI, RNLO, RNHI, PINC, AVE
      include 'include/egs5_h.f'
      include 'include/egs5_media.f'
      include 'pegscommons/bremp2.f'
      include 'pegscommons/dbrpr.f'
      include 'pegscommons/dercon.f'
      include 'pegscommons/elemtb.f'
      include 'pegscommons/elmtbc.f'
      include 'pegscommons/funcs.f'
      include 'pegscommons/funcsc.f'
      include 'pegscommons/lspion.f'
      include 'pegscommons/mimsd.f'
      include 'pegscommons/mixdat.f'
      include 'pegscommons/mxdatc.f'
      include 'pegscommons/molvar.f'
      include 'pegscommons/phpair.f'
      include 'pegscommons/pmcons.f'
      include 'pegscommons/pwlfin.f'
      include 'pegscommons/cohcom.f'
      include 'pegscommons/rslts.f'
      include 'pegscommons/thres2.f'
      include 'pegscommons/epstar.f'
      include 'pegscommons/bcom.f'
      include 'pegscommons/cpcom.f'
      include 'pegscommons/eimpact.f'
      include 'pegscommons/sfcom.f'
      include 'pegscommons/mscom.f'
      include 'pegscommons/legacy.f'
      include 'pegscommons/dcsstr.f'
      double precision XP(4),WASAV(20)
      logical MEDSET,ENGSET
      character*4 OPTION(4,14),OPT(4),BLKW,NAME(6)
      character*4 NAMESB(12),IDFNAM(6)
      integer NH(200)
      external ALKE,ALKEI,CFUNS,EFUNS,GFUNS,RFUNS,ALIN,ALINI,AFFACT,SFUN
     *S, CFUNS2,CFUNS3,CFUNS4,CPRFIL,RFUNS2,EIIFUNS
      intrinsic DLOG,DEXP
      data OPTION/'E','L','E','M','M','I','X','T','C','O','M','P','E','N
     *','E','R','M','I','M','S','P','W','L','F', 'D','E','C','K','T','E'
     *,'S','T','D','B','U','G','C','A','L','L','P','L','T','N','S','T','
     *O','P','P','L','T','I', 'H','P','L','T'/
      data NOPT/15/,BLKW/' '/
      data MEDSET/.FALSE./,ENGSET/.FALSE./
      data NPTS/50/,IDF/6/
      namelist/INP/NE,PZ,RHO,RHOZ,WA,AE,UE,AP,UP, IFUN,XP,IV,VLO,VHI,IDF
     *,NPTS, EPE,ZTHRE,ZEPE,NIPE,NALE,EPG,ZTHRG,ZEPG,NIPG,NALG, EI,ISUB,
     *GASP,IUNRST,IRAYL,AFACT,SK,X0,X1,IEV,CBAR,ISSB,EPSTFL, IAPRIM,IBOU
     *ND,INCOH,ICPROF,IMPACT, efracH, efracL, fudgeMS, nleg0,
     * EPR,ZTHRR,ZEPR,NIPR,NALR,EPSF,ZTHRS,ZEPS,NIPS,NALS, EPCP,ZTHRC,ZE
     *PC,NIPC,NALC
      namelist/PWLFNM/EPE,ZTHRE,ZEPE,NIPE,NALE,EPG,ZTHRG,ZEPG,NIPG,NALG,
     * EPR,ZTHRR,ZEPR,NIPR,NALR,EPSF,ZTHRS,ZEPS,NIPS,NALS, EPCP,ZTHRC,ZE
     *PC,NIPC,NALC
      namelist/BCOMDT/BCOMP,PBC,NPBC
      namelist/ISCADT/SCATF,XSVAL
      namelist/SCPRDT/MXRAW,MXSHEL,ELECNI,NSHELL,CAPIN,SCPROF,QCAP
!  PEGS input data files
      open(UNIT=28,FILE='data/pgs5phtx.dat',STATUS='old')
      open(UNIT= 9,FILE='data/pgs5form.dat',STATUS='old')
      open(UNIT=22,FILE='data/aprime.data',STATUS='old')
!  PEGS material specific input data files (not always used)
      open(UNIT=20,FILE='epstar.dat',STATUS='unknown')
!  KEK LScat input data files
      open(UNIT=11,FILE='data/bcomp.dat',STATUS='old')
      open(UNIT=13,FILE='data/incoh.dat',STATUS='old')
!  PEGS problem input file
      open(UNIT=25,FILE='pgs5job.pegs5inp',STATUS='old')
!  PEGS output files
      open(UNIT=26,FILE='pgs5job.pegs5lst',STATUS='unknown')
      open(UNIT= 7,FILE='pgs5job.pegs5dat',STATUS='unknown')
      open(UNIT=10,FILE='pgs5job.pegs5err',STATUS='unknown')
      open(UNIT=21,FILE='pgs5job.pegs5plot',STATUS='unknown')
!  KEK LScat material specific input data files (not always used)
      open(UNIT=15,FILE='scp.dat',STATUS='unknown')
      open(UNIT=18,FILE='iff.dat',STATUS='unknown')
      open(UNIT=19,FILE='ics.dat',STATUS='unknown')
!  New EGS transport mechanics data files
      open(UNIT=17,FILE='pgs5job.msfit',STATUS='unknown')
      write(  26,100)
100   FORMAT('1',20X,'Pegs5 listing file'/ 20X,'(with nrcc modifications
     *, Jan 13,1988)')
      call pmdcon
      write(  26,110)
110   FORMAT(/' This version reads units 8 and 9 in free format'/)
      read(  28,*) NPHE, ((PHE(in,iz),in=1,61),iz=1,100), ((PHD(in,iz),i
     *n=1,61),iz=1,100), EKEDGE, PRE, ((PRD(in,iz),in=1,17),iz=1,100), (
     *(COHE(in,iz),in=1,61),iz=1,100)
      read(9,*) XVAL, ((AFAC(in,iz),in=1,100),iz=1,100)
      do i=1,100
        XVAL(i)=XVAL(i)**2.
      end do
      ISSBS=ISSB
      AFACTS=AFACT
      CBARS=CBAR
      SKS=SK
      X0S=X0
      X1S=X1
      IEVS=IEV
      ibounds=IBOUND
      incohs=INCOH
      icprofs=ICPROF
      gasps=GASP
      irayls=IRAYL 
      impacts=IMPACT
      iunrsts=IUNRST
      fudgeMSs=fudgeMS
      efracHs=efracH
      efracLs=efracL
      nleg0s=nleg0
      epstfls=EPSTFL
      iepsts=IEPST
      iaprims=IAPRIM
      iaprfls=IAPRFL
1040  continue
      do i=1,20
        WASAV(i)=WA(i)
        WA(i)=0.
      end do
      RHOSAV=RHO
      RHO=0.0
      read(  25,130,end=1060) OPT
130   FORMAT(4A1)
      write(  26,140) OPT
140   FORMAT(//,1X,60('*'),/,' *',T61,'*',/,' *  OPT = ',4A1,T61,'*',/,
     *' *',T61,'*',/,1X,60('*'),//)
      read(  25,INP,end=8990)
      if (RHO.eq.0) then
        do ICH=1,4
          if (OPT(ICH).ne.OPTION(ICH,1)) then
            RHO=RHOSAV
            go to 150
          end if
        end do
150     continue
      end if
      do 160 IOPT=1,NOPT
        do ICH=1,4
          if (OPT(ICH).ne.OPTION(ICH,IOPT)) go to 160
        end do
        go to 1120
160   continue
      write(  26,1130)
1130  format(' Option not found, job aborted.')
      close(26)
      stop 16
1120  if (IOPT.gt.3) then
        RHO=RHOSAV
        do i=1,20
          WA(i)=WASAV(i)
        end do
      else
        do i=1,4
          MTYP(i)=OPT(i)
        end do
      end if
      go to(1160,1170,1180,1190,1200,1210,1220,1230, 1240,1250,1260,1070
     *,1270,1280),IOPT
! ***********>  OPT = ELEM  <**************
1160  NE=1
      PZ(1)=1
      IMIXT=0
      go to 1290
! ***********>  OPT = MIXT  <**************
1170  if (NE.le.1) then
        write(  26,1300) NE
1300    format(//,' NE=',I6,' is improperly defined for a mixture.')
        close(26)
        stop
      end if
      IMIXT=1
      go to 1290
! ***********>  OPT = COMP  <**************
1180  if (NE.le.1) then
        write(  26,1310) NE
1310    format(//,' NE=',I6,' is improperly defined for a compound.')
        close(26)
        stop
      end if
      IMIXT=0
1290  read(  25,1320) MEDIUM,IDSTRN
1320  FORMAT(24A1,6X,24A1)
      read(  25,1330) (ASYM(i),i=1,NE)
1330  FORMAT(24(A2,1X))
      if (IDSTRN(1).eq.BLKW) then
        do i=1,LMED
          IDSTRN(i)=MEDIUM(i)
        end do
      end if
      write(  26,1350) MEDIUM,IDSTRN,(ASYM(i),i=1,NE)
1350  format(1X,60('-')/' Medium=',24A1,',Sternheimer ID=',24A1,/1X,60('
     *-')// ,' Atomic symbols are: ',(1X,24(A2,1X) ))

!  see if this medium is being used in the current problem.
!  this needs to be done so we can later affirm the integrity
!  of the pegs data file in case it is reused.

      do 16 medIdx = 1, nmed
        do ib = 1, 24
          if(MEDIUM(ib) .ne. media(ib,medIdx)) go to 16
          if(ib .eq. 24) go to 17
        end do
16    continue
17    oldK1run = .true.
      if(medIDx.le.nmed) then
        if(charD(medIdx).ne.0.d0) then
          oldK1run = .false.
        endif
      endif

      if (IUNRST.eq.1) then
        write(  26,1360)
1360    format(/T10,'***Calculates unrestricted collision', ' stopping p
     *ower***  IUNRST=1'//)
      else if (IUNRST.eq.2) then
        write(  26,1370)
1370    format(/T10,'****Data set for a csda calculation', '******  IUNR
     *ST=2'//)
      else if (IUNRST.eq.3) then
        write(  26,1380)
1380    format(/T10,'****Data set for a csda calculation', ' but with BR
     *EM events******  IUNRST = 3'//)
      else if (IUNRST.eq.4) then
        write(  26,1390)
1390    format(/T10,'****Data set for a calculation', 'with DELTAS DISCR
     *ETE,BREM CSDA******  IUNRST = 4'//)
      else if (IUNRST.eq.5) then
        write(  26,1400)
1400    format(/T10,'****Calculates unrestricted radiative', ' stopping
     *power***** IUNRST = 5')
      else if (IUNRST.eq.6) then
        write(  26,1410)
1410    format(/T10,'****Calculates restricted radiative', ' stopping po
     *wer***** IUNRST = 6')
      else if (IUNRST.eq.7) then
        write(  26,1420)
1420    format(/T10,'****Calculates restricted collision', ' stopping po
     *wer***** IUNRST = 7')
      end if
      do i=1,NE
        Z(i)=ZTBL(ASYM(i))
        if (Z(i).eq.0.0) then
          write(  26,1440)
1440      format(' Bad atomic symbol....job aborted.')
          close(26)
          stop 16
        end if
        if (WA(I).eq.0.) then
          I01=Z(i)
          WA(i)=WATBL(I01)
        end if
        if (IMIXT.ne.0) then
          PZ(i)=RHOZ(i)/WA(I)
        else
          RHOZ(i)=PZ(i)*WA(i)
        end if
      end do
      if (NE.eq.1.and.RHO.eq.0.) then
        I01=Z(1)
        RHO=RHOTBL(I01)
      end if
      call MIX
      call SPINIT
      call DIFFER
      ! Read in the DCS and get the scattering power
      call elinit(z,pz,ne)
      MEDSET=.true.
      read(11,BCOMDT)
      rewind 11
      read(  13,ISCADT)
      rewind   13
      if (ICPROF.eq.3.or.ICPROF.eq.4) then
        write(  26,1450) ICPROF
1450    format(' Reading shellwise Compton profile as ICPROF=',I3)
        read(15,SCPRDT,ERR=8991)
      end if
      if (ICPROF.eq.-3.or.ICPROF.eq.-4) then
        write(  26,1460) ICPROF
1460    format(' Reading shellwise Compton profile as ICPROF=',I3)
        MXRAW=0
        MXSHEL=0
        call RDSCPR
        open(UNIT=31,FILE='pgs5job.ssl',STATUS='old')
        read(31,SCPRDT)
        close(31)
        ICPROF=ABS(ICPROF)
      end if
      if (IRAYL.eq.2) then
        do i=1,100
          read(18,*,ERR=8992) XVALFAC(i),BFAC2(i)
          write(  26,1480) XVALFAC(i),BFAC2(i)
1480      format(2G10.5)
        end do
        write(  26,1490)
1490    format(/'Reading interference form factors for use with IRAYL=2'
     *  ,/)
        do i=1,61
          read(19,*,ERR=8993) XVALINT(i),COHEINT(i,1)
          write(  26,1510) XVALINT(i),COHEINT(i,1)
1510      format(2G10.5)
        end do
        write(  26,1520)
1520    format(/'Reading interference coherent cross sections for use 
     *  with IRAYL=2',/)
      end if
      write(  26,1530)
1530  format(//,' End of elem, mixt, or comp option',///)
      go to 1040
! ***********>  OPT = ENER  <**************
1190  if (AE.lt.0) AE=-AE*RM
      if (UE.lt.0) UE=-UE*RM
      if (AP.lt.0) AP=-AP*RM
      if (UP.lt.0) UP=-UP*RM
      TE=AE-RM
      TET2=TE*2.0
      TEM=TE/RM
      THBREM=RM+AP
      THMOLL=AE+TE
      write(  26,1540) AE,UE,AP,UP,TE,TET2,TEM,THBREM,THMOLL
1540  format(' AE,UE,AP,UP,TE,TET2,TEM,THBREM,THMOLL'/1X,1P,5E15.7/1X,1P
     *,4E15.7)
!     can now initialize energy grids for computing scattering strength
      call inigrd(ae,ue,1)
      ENGSET=.true.
!     get estepe limits
      call esteplim
      go to 1040
! ***********>  OPT = MIMS  <**************
! this is no longer supported
1200  call MOLIER
      go to 1040
! ***********>  OPT = PWLF  <**************
1210  if (MEDSET.and.ENGSET) go to 1550
      write(  26,1560) MEDSET,ENGSET
1560  format(' MEDSET,ENGSET=',2L2,',PWLF req. ignored.')
      close(26)
      stop 16
1550  EBINDA=EBIND(AP)
      write(  26,PWLFNM)
      write(  26,1570) EBINDA
1570  format(' Average K-ionization energy=',F10.6,'(MeV)')

      if(efracH .gt. 0.5d0) then
        efracH = .5d0
        write(6,1541) efracH
        write(26,1541) efracH
      else if(efracH .le. 1.d-6) then
        efracH = 5.d-2
        write(6,1542) efracH
        write(26,1542) efracH
      endif
      if(efracL .gt. 0.5d0) then
        efracL = .5d0
        write(6,1543) efracL
        write(26,1543) efracL
      else if(efracL .le. 1.d-6) then
        efracL = 5.d-2
        write(6,1544) efracL
        write(26,1544) efracL
      endif
1541  format('Warning:  EFRAC_HIGH > MAX.  setting to ',F6.4)
1542  format('Warning:  EFRAC_HIGH < MIN.  setting to ',F6.4)
1543  format('Warning:  EFRAC_LOW > MAX.  setting to ',F6.4)
1544  format('Warning:  EFRAC_LOW < MIN.  setting to ',F6.4)
      if(oldK1run) call makek1(ae,ue)

      write(  26,1580)
1580  format(' Do PWLF to electron data sets.'/)
      call PWLF1(NEL,NALE,AE,UE,THMOLL,EPE,ZTHRE,ZEPE,NIPE,ALKE,ALKEI, A
     *XE,BXE,150,15,AFE,BFE,EFUNS)
      if (IMPACT.ge.1) then
        write(  26,1590) IMPACT
1590    format(' Do pwlf to EII/MOLLER. IMPACT=',I3/)
        call PWLF1(NEII,NALE,AE,UE,THMOLL,EPE,ZTHRE,ZEPE,NIPE,ALKE,ALKEI
     *  , AXEII,BXEII,150,20,AFEII,BFEII,EIIFUNS)
      end if
      write(  26,1600)
1600  format(' Do pwlf to photon data sets.'/)
      if (IBOUND.eq.1) then
        write(  26,1610)
1610    format(/' Total Compton cross section of bound electron in'/ ' S
     *torm-Israel is used'/)
      end if
      call PWLF1(NGL,NALG,AP,UP,RMT2,EPG,ZTHRG,ZEPG,NIPG,DLOG,DEXP,AXG,B
     *XG,1000,4,AFG,BFG,GFUNS)
      if (IRAYL.eq.1) then
        write(  26,1620)
1620    format(//,' ***** IRAYL=1: Rayleigh data included *****',//)
        write(  26,1630)
1630    format(' Do PWLF to Rayleigh distribution.'/)
        do i=1,100
          AFAC2(i)=0.0
          do in=1,NE
            IZZ=Z(in)
            AFAC2(i)=AFAC2(i)+PZ(in)*AFAC(i,IZZ)**2
          end do
        end do
        AFFI(1)=0.0
        do i=2,97
          AX=XVAL(i-1)
          BX=XVAL(i)
          AFFI(i)=QD(AFFACT,AX,BX,'AFFACT')
        end do
        do i=2,97
          AFFI(i)=AFFI(i)+AFFI(i-1)
        end do
        do i=1,97
          AFFI(i)=AFFI(i)/AFFI(97)
        end do
        call PWLF1(NGR,NALR,0.D0,1.D0,0.D0,EPR,ZTHRR,ZEPR,NIPR,ALIN,
     *  ALINI,AXR,BXR,100,1,AFR,BFR,RFUNS)
      end if
      if (IRAYL.eq.2) then
        write(  26,1690)
1690    format(//,' IRAYL=2: Rayleigh data with interference effects inc
     *luded.',//)
        write(  26,1700)
1700    format(' Do PWLF to Rayleigh distribution.'/)
        do i=1,100
          AFAC2(i)=(BFAC2(i)*DSQRT(WM))**2
        end do
        do i=1,100
          XVAL(i)=XVALFAC(i)**2
        end do
        AFFI(1)=0.0
        do i=2,97
          AX=XVAL(i-1)
          BX=XVAL(i)
          AFFI(i)=QD(AFFACT,AX,BX,'AFFACT')
        end do
        do i=2,97
          AFFI(i)=AFFI(i)+AFFI(i-1)
        end do
        do i=1,97
          AFFI(i)=AFFI(i)/AFFI(97)
        end do
        call PWLF1(NGR,NALR,0.D0,1.D0,0.D0,EPR,ZTHRR,ZEPR,NIPR,ALIN,
     *  ALINI,AXR,BXR,100,1,AFR,BFR,RFUNS)
      end if
      if (IRAYL.eq.3) then
        write(  26,1760)
1760    format(//,' ***** IRAYL=3: Rayleigh data included *****',//)
        write(  26,1770)
1770    format(' Do PWLF to Rayleigh distribution. X-F2(X,Z) tabulation
     *'/)
        do i=1,100
          AFAC2(i)=0.0
          do IN=1,NE
            IZZ=Z(IN)
            AFAC2(I)=AFAC2(I)+PZ(IN)*AFAC(I,IZZ)**2
          end do
        end do
        call PWLF1(NGR,NALR,1.0D-3,1.0D2,1.0D-3,EPR,ZTHRR,ZEPR,NIPR,DLOG
     *  ,DEXP, AXR,BXR,100,1,AFR,BFR,RFUNS2)
      end if
      if (INCOH.eq.1) then
        write(  26,1800)
1800    format(//,' ***** INCOH=1: S(X,Z) data included *****',//)
        write(  26,1810)
1810    format(' Do PWLF to S(X,Z)'/)
        do i=1,45
          SCSUM=0.0
          ZSUM=0.0
          do in=1,NE
            IZZ=Z(in)
            SCSUM=SCSUM+PZ(in)*SCATF(i,IZZ)
            ZSUM=ZSUM+PZ(in)*Z(in)
          end do
          SCATZ(i)=SCSUM/ZSUM
        end do
        call PWLF1(NGS,NALS,5.0D-3,80.D0,80.D0,EPSF,ZTHRS,ZEPS,NIPS,
     *  DLOG,DEXP,AXS,BXS,100,1,AFS,BFS,SFUNS)
      end if
!  Trap to stop ICPROF of 1 or 2, since EGS5 does not process this data
!  properly.  The PEGS code for these options is left in place in case 
!  they are to be resurrected later.
      if(ICPROF.eq.1) then
        write(  26,1830) ICPROF
1830    format(//,' ***** ICPROF=',I2,':T-Compton profile data not ',
     *'currently supported.  Setting ICPROF=0',//)
        ICPROF=0
      else if(ICPROF.eq.2) then
        write(  26,1830) ICPROF
        ICPROF=0
      endif
      if (ICPROF.eq.1.or.ICPROF.eq.2) then
        CPIMEV=IEV*1.0D-6
        write(  26,1840) CPIMEV
1840    format(' CPIMEV=',E12.5)
        write(  26,1850) ICPROF
1850    format(//,' ***** ICPROF=',I2,':T-Compton profile data included
     ******',//)
        if (ICPROF.eq.1) then
          write(  26,1860)
1860      format(' Do PWLF to Q vs int J(Q)'/)
        end if
        if (ICPROF.eq.2) then
          write(  26,1870)
1870      format(' Do PWLF to J(Q)'/)
        end if
        do i=1,31
          CPSUM=0.0
          PZSUM=0.0
          do in=1,NE
            IZZ=Z(in)
            CPSUM=CPSUM+PZ(in)*CPROF(i,IZZ)
            PZSUM=PZSUM+PZ(in)
          end do
          AVCPRF(i)=CPSUM/PZSUM
          write(  26,1900) QCAP(I),AVCPRF(I)
1900      format(' QCAP= ',1P,E9.2,' AVCPRF= ',1P,E9.2)
        end do
        if (ICPROF.eq.2) then
          call PWLF1(NGC,NALC,1.D-2,1.D2,1.D2,EPCP,ZTHRC,ZEPC,NIPC,DLOG
     *    ,DEXP, AXC,BXC,2000,1,AFC,BFC,CFUNS)
        end if
        if (ICPROF.eq.1) then
          CPROFI(1)=0.0
          QCAP10(1)=0.0
          do i=2,301
            ILOC=(i-2)/10+1
            STEP=(QCAP(ILOC+1)-QCAP(ILOC))*0.1
            AX=QCAP(ILOC)+STEP*FLOAT(i-2-(ILOC-1)*10)
            BX=AX+STEP
            QCAP10(i)=BX
            CPROFI(i)=QD(CPRFIL,AX,BX,'CPRFIL')
            write(  26,1920) i,CPROFI(i)
1920        format(1X,I5,'-th interval CPROFI=',1P,E12.5)
          end do
          write(  26,1930)
1930      format(' Sum of integration of CPROF')
          do i=2,301
            CPROFI(i)=CPROFI(i)+CPROFI(i-1)
            write(  26,1950)I,CPROFI(i)
1950        format(1X,I5,'-th interval. Sum CPROFI=',1P,E12.5)
          end do
          write(  26,1960)
1960      format(' Normalized sum of integration of CPROF')
          do i=1,301
            CPROFI(i)=CPROFI(i)/CPROFI(301)
            write(  26,1980) i,CPROFI(i)
1980        format(1X,I5,'-th interval. NORM.SUM CPROFI=',1P,E12.5)
          end do
          call PWLF1(NGC,NALC,0.D0,1.D0,0.D0,EPCP,ZTHRC,ZEPC,NIPC,ALIN,
     *    ALINI, AXC,BXC,2000,1,AFC,BFC,CFUNS2)
        end if
        write(  26,1990) NGC
1990    format(' Compton profile was PWLF-ED, NGC= ',I5)
      end if
      if (ICPROF.eq.3.OR.ICPROF.eq.4) then
        write(  26,2000) ICPROF
2000    format(//,' ***** ICPROF=',I2,': S-Compton profile data included
     * *****',//)
        if (ICPROF.eq.3) then
          write(  26,2010)
2010      format(' Do pwlf to Q vs int J(Q)'/)
        end if
        if (ICPROF.eq.4) then
          write(  26,2020)
2020      format(' Do PWLF to J(Q)'/)
        end if
        do i=1,MXSHEL
          ELECNO(i)=0.
          ELECNJ(i)=0.
        end do
        do i=1,MXRAW
          ITEMP=NSHELL(i)
          ELECNO(ITEMP)=ELECNO(ITEMP)+ELECNI(i)
        end do
        do i=1,MXSHEL
          write(  26,2060) i,ELECNO(i)
2060      format(' Shell ,ELECNO=',I5,E12.5)
        end do
        do i=1,MXSHEL
          CAPIO(i)=0.0
          CAPILS(i)=0.0
        end do
        do i=1,MXRAW
          if (CAPIN(i).gt.0.0) then
            CAPIL=DLOG(CAPIN(i))
            J=NSHELL(i)
            CAPILS(J)=CAPILS(J)+CAPIL*ELECNI(i)
            ELECNJ(J)=ELECNJ(J)+ELECNI(i)
          end if
        end do
        do i=1,MXSHEL
          if (ELECNJ(i).gt.0.) then
            CAPIO(i)=DEXP(CAPILS(i)/ELECNJ(i))
          end if
        end do
        do i=1,MXRAW
          write(  26,2110) i,CAPIN(i)
2110      format('i,CAPIN(i)= ',I5,E12.5)
        end do
        do i=1,MXSHEL
          write(  26,2130) i,CAPIO(i)
2130      format(' i,CAPIO(i)= ',I5,E12.5)
        end do
        do i=1,31
          do ishell=1,MXSHEL
            SCPSUM(i,ishell)=0.0
          end do
          do in=1,MXRAW
            ISHELL=NSHELL(in)
            SCPSUM(i,ishell)=SCPSUM(i,ishell)+SCPROF(i,in)*ELECNI(in)
          end do
        end do
        do ishell=1,MXSHEL
          write(  26,2180) ishell
2180      format(' ',I5,'-th shell. SCPSUM')
          write(  26,2190) (SCPSUM(i,ishell),I=1,31)
2190      format(' ',1P,7E9.2)
        end do
        do i=2,MXSHEL
          ELECNO(i)=ELECNO(i)+ELECNO(i-1)
        end do
        do i=1,MXSHEL
          ELECNO(i)=ELECNO(i)/ELECNO(MXSHEL)
        end do
        do i=1,MXSHEL
          write(  26,2230) i,ELECNO(i)
2230      format(' Shell ,ELECNO=',I5,E12.5)
        end do
        if (ICPROF.eq.4) then
          call PWLF1(NGCS,NALC,1.D-2,1.D2,1.D2,EPCP,ZTHRC,ZEPC,NIPC,DLOG
     *    ,DEXP, AXCS,BXCS,2000,MXSHEL,AFCS,BFCS,CFUNS4)
        end if
        if (ICPROF.eq.3) then
          do ishell=1,MXSHEL
            CPROFI(1)=0.0
            QCAP10(1)=0.0
            write(  26,2250) ishell
2250        format(' ICPROF=3,ISHELL=',I5)
            do i=1,31
              AVCPRF(i)=SCPSUM(i,ishell)
            end do
            write(  26,2270)
2270        format(' AVCPROF')
            write(  26,2280) (AVCPRF(i),i=1,31)
2280        FORMAT(' ',7E12.5)
            do i=2,301
              ILOC=(i-2)/10+1
              STEP=(QCAP(ILOC+1)-QCAP(ILOC))*0.1
              AX=QCAP(ILOC)+STEP*FLOAT(I-2-(ILOC-1)*10)
              BX=AX+STEP
              QCAP10(i)=BX
              CPROFI(i)=QD(CPRFIL,AX,BX,'CPRFIL')
            end do
            do i=2,301
              CPROFI(i)=CPROFI(i)+CPROFI(i-1)
            end do
            do i=1,301
              SCPROI(i,ishell)=CPROFI(i)/CPROFI(301)
            end do
          end do
          call PWLF1(NGCS,NALC,0.D0,1.D0,0.D0,EPCP,ZTHRC,ZEPC,NIPC,ALIN,
     *    ALINI, AXCS,BXCS,2000,MXSHEL,AFCS,BFCS,CFUNS3)
        end if
        write(  26,2320) NGCS,NALC
2320    format(' S-Compton profile was PWLF-ED, NGCS= ',I5,' NALC=',I5)
      end if
      go to 1040
! ***********>  OPT = DECK  <**************
1220  call LAY
      ISSB=ISSBS
      AFACT=AFACTS
      CBAR=CBARS
      SK=SKS
      X0=X0S
      X1=X1S
      IEV=IEVS
      write(  26,2330) ISSB,AFACT,CBAR,SK,X0,X1,IEV
2330  format(' After DECK. ISSB,AFACT,CBAR,SK,X0,X1,IEV=',I5,6E11.4)
      IBOUND=ibounds
      INCOH=incohs
      ICPROF=icprofs
      GASP=gasps
      IRAYL=irayls
      IMPACT=impacts
      IUNRST=iunrsts
      fudgeMS=fudgeMSs
      efracH=efracHs
      efracL=efracLs
      nleg0=nleg0s
      EPSTFL=epstfls
      IEPST=iepsts
      IAPRIM=iaprims
      IAPRFL=iaprfls
      go to 1040
! ***********>  OPT = TEST  <**************
1230  continue
      call PLOT(49,XP,1,AE,UE,NPTS,9)
      call PLOT(71,XP,1,AE,UE,NPTS,9)
      call PLOT(47,XP,1,AE,UE,NPTS,9)
      call PLOT(68,XP,1,AE,UE,NPTS,9)
      call PLOT(46,XP,1,AE,UE,NPTS,9)
      call PLOT(66,XP,1,AE,UE,NPTS,9)
      call PLOT(67,XP,1,AE,UE,NPTS,9)
      call PLOT(77,XP,1,AE,UE,NPTS,9)
      call PLOT(78,XP,1,AE,UE,NPTS,9)
      call PLOT(53,XP,1,AP,UP,NPTS,6)
      call PLOT(51,XP,1,AP,UP,NPTS,6)
      call PLOT(52,XP,1,AP,UP,NPTS,6)
      call PLOT(44,XP,1,AP,UP,NPTS,6)
      write(  26,2340)
2340  format('1')
      go to 1040
! ***********>  OPT = DBUG  <**************
1240  continue
      go to 1040
! ***********>  OPT = CALL  <**************
1250  read(  25,2350) NAME
2350  format(6A1)
      IFUN=IFUNT(NAME)
      if (IFUN.le.0) go to 1040
      VALUE=FI(IFUN,XP(1),XP(2),XP(3),XP(4))
      NA=NFARG(IFUN)
      write(  26,2360) VALUE,(FNAME(i,IFUN),i=1,6),(XP(i),i=1,NA)
2360  format(' Function call: ',1P,G15.6,' = ',6A1,' OF ',4G15.6)
      go to 1040
! ***********>  OPT = PLTN  <**************
1260  read(  25,2370) NAME,IDFNAM
2370  format(12A1)
      IFUN=IFUNT(NAME)
      if (IFUN.le.0) go to 1040
      ID=IFUNT(IDFNAM)
      if (ID.lt.0) go to 1040
      if (ID.ne.0) IDF=ID
! ***********>  OPT = PLTI  <**************
1270  call PLOT(IFUN,XP,IV,VLO,VHI,NPTS,IDF)
      go to 1040
! ***********>  OPT = HPLT  <**************
1280  read(  25,2380) NAMESB,NTIMES,NBINS,IQI,RNLO,RNHI,IRNFLG,
     * (NH(IBIN),IBIN=1,NBINS)
2380  format(' Test data for routine=',12A1,',#SAMPLES=',I10,',NBINS=',I
     *5 /' IQI=',I2,',RNLO,RNHI=',2F12.8,',IRNFLG=',I2/(9I8))
      write(  26,2380) NAMESB,NTIMES,NBINS,IQI,RNLO,RNHI,IRNFLG,
     * (NH(IBIN),IBIN=1,NBINS)
      write(  26,2390) EI,ISUB
2390  FORMAT(' EI=',F14.3,',ISUB=',I3)
      go to (2400,2410,2420,2430,2440,2450),ISUB
2400  call HPLT1(EI,RM,EI-RM,NAMESB,NTIMES,NBINS,NH, 1,54,59,63 )
      go to 1040
2410  call HPLT1(EI,EI/(1.0+2.0*EI/RM),EI,NAMESB,NTIMES,NBINS,NH, 6,40,4
     *2,43 )
      go to 1040
2420  call HPLT1(EI,AP,EI-RM,NAMESB,NTIMES,NBINS,NH, 6,24,30,34 )
      go to 1040
2430  call HPLT1(EI,AE,RM+(EI-RM)*0.5,NAMESB,NTIMES,NBINS,NH, 3,11,13,14
     * )
      go to 1040
2440  call HPLT1(EI,AE,EI,NAMESB,NTIMES,NBINS,NH, 3,20,22,23 )
      go to 1040
2450  PINC=DSQRT(EI**2-RM**2)
      AVE=EI+RM
      call HPLT1(EI,AVE*RM/(AVE+PINC),AVE*0.5,NAMESB,NTIMES,NBINS,NH, 6,
     *15,17,18 )
      go to 1040

! ***********>  standard PEGS termination  <**************
! Print the new mscat options parameters.  GS distribution computation 
! moved to egs so that we can use it with chard method.
1060  call prelastino

! ***********>  OPT = STOP or standard PEGS termination  <**************
1070  write(  26,2470)
2470  format(///' End of file read - exit from pegs5'/'1')
      close(UNIT=25)
      close(UNIT=26)
      close(UNIT=7)
      close(UNIT=28)
      close(UNIT=9)
      close(UNIT=10)
      close(UNIT=11)
      close(UNIT=13)
      close(UNIT=14)
      close(UNIT=15)
      close(UNIT=17)
      close(UNIT=18)
      close(UNIT=19)
      close(UNIT=21)
      close(UNIT=22)
      return

! abnormal termination because of end-of-file on an expected
! user-supplied input data file

8990  write(26,9990)
9990  format(' Stopped in pegs5 because namelist/INP/',
     *' data was missing.')
      go to 9995
8991  write(26,9991)
9991  format(//,'EOF on user supplied Compton profile data - stopping.')
      go to 9995
8992  write(26,9992)
9992  format(//,'EOF on user supplied interference cs data - stopping.')
      go to 9995
8993  write(26,9993)
9993  format(//,'EOF on user supplied interference ff data - stopping.')

9995  close(26)
      stop

      end

      block data PEGSDAT
!     Use Revised Sternheimer Density Effects Coeeficients
!     Atomic Data Nuclear Data Tables 30, 261(1984) by 
!     R. M. Sternheimer et al. 
!     Reference KEK Internal 95-17 (1995)
      double precision STDAT1, STDAT2, STDAT3, STDAT4, STDAT5,
     *                 STDAT6, STDAT7, STDAT8, STDAT9, STDA10,
     *                 STDA11, STDA12, STDA13, STDA14
      include 'include/egs5_h.f'
      include 'pegscommons/bremp2.f'
      include 'pegscommons/dbrpr.f'
      include 'pegscommons/dercon.f'
      include 'pegscommons/elemtb.f'
      include 'pegscommons/elmtbc.f'
      include 'pegscommons/funcs.f'
      include 'pegscommons/funcsc.f'
      include 'pegscommons/lspion.f'
      include 'pegscommons/mimsd.f'
      include 'pegscommons/mixdat.f'
      include 'pegscommons/mxdatc.f'
      include 'pegscommons/molvar.f'
      include 'pegscommons/phpair.f'
      include 'pegscommons/pmcons.f'
      include 'pegscommons/pwlfin.f'
      include 'pegscommons/radlen.f'
      include 'pegscommons/cohcom.f'
      include 'pegscommons/spcomm.f'
      include 'pegscommons/spcomc.f'
      include 'pegscommons/rslts.f'
      include 'pegscommons/thres2.f'
      include 'pegscommons/epstar.f'
      include 'pegscommons/eimpact.f'
      include 'pegscommons/bcom.f'
      include 'pegscommons/cpcom.f'
      include 'pegscommons/sfcom.f'
      include 'pegscommons/mscom.f'
      include 'pegscommons/dcsstr.f'
      character*4 MEDTB1(24,20),MEDTB2(24,20),MEDTB3(24,20),
     *            MEDTB4(24,20),MEDTB5(24,20),MEDTB6(24,20),
     *            MEDTB7(24,10),MEDTB8(24,10),MEDTB9(24,10),
     *            MEDT10(24,10),MEDT11(24,10),MEDT12(24,10),
     *            MEDT13(24,10),MEDT14(24,10),MEDT15(24,10),
     *            MEDT16(24,10),MEDT17(24,10),MEDT18(24,10),
     *            MEDT19(24,10),MEDT20(24,10),MEDT21(24,10),
     *            MEDT22(24,8)
      equivalence (MEDTBL(1,1),MEDTB1(1,1))
      equivalence (MEDTBL(1,21),MEDTB2(1,1))
      equivalence (MEDTBL(1,41),MEDTB3(1,1))
      equivalence (MEDTBL(1,61),MEDTB4(1,1))
      equivalence (MEDTBL(1,81),MEDTB5(1,1))
      equivalence (MEDTBL(1,101),MEDTB6(1,1))
      equivalence (MEDTBL(1,121),MEDTB7(1,1))
      equivalence (MEDTBL(1,131),MEDTB8(1,1))
      equivalence (MEDTBL(1,141),MEDTB9(1,1))
      equivalence (MEDTBL(1,151),MEDT10(1,1))
      equivalence (MEDTBL(1,161),MEDT11(1,1))
      equivalence (MEDTBL(1,171),MEDT12(1,1))
      equivalence (MEDTBL(1,181),MEDT13(1,1))
      equivalence (MEDTBL(1,191),MEDT14(1,1))
      equivalence (MEDTBL(1,201),MEDT15(1,1))
      equivalence (MEDTBL(1,211),MEDT16(1,1))
      equivalence (MEDTBL(1,221),MEDT17(1,1))
      equivalence (MEDTBL(1,231),MEDT18(1,1))
      equivalence (MEDTBL(1,241),MEDT19(1,1))
      equivalence (MEDTBL(1,251),MEDT20(1,1))
      equivalence (MEDTBL(1,261),MEDT21(1,1))
      equivalence (MEDTBL(1,271),MEDT22(1,1))
      dimension STDAT1(7,20),STDAT2(7,20),STDAT3(7,20),STDAT4(7,20),
     *          STDAT5(7,20),STDAT6(7,20),STDAT7(7,20),STDAT8(7,20),
     *          STDAT9(7,20),STDA10(7,20),STDA11(7,20),STDA12(7,20),
     *          STDA13(7,20),STDA14(7,18)
      equivalence (STDATA(1,1),STDAT1(1,1))
      equivalence (STDATA(1,21),STDAT2(1,1))
      equivalence (STDATA(1,41),STDAT3(1,1))
      equivalence (STDATA(1,61),STDAT4(1,1))
      equivalence (STDATA(1,81),STDAT5(1,1))
      equivalence (STDATA(1,101),STDAT6(1,1))
      equivalence (STDATA(1,121),STDAT7(1,1))
      equivalence (STDATA(1,141),STDAT8(1,1))
      equivalence (STDATA(1,161),STDAT9(1,1))
      equivalence (STDATA(1,181),STDA10(1,1))
      equivalence (STDATA(1,201),STDA11(1,1))
      equivalence (STDATA(1,221),STDA12(1,1))
      equivalence (STDATA(1,241),STDA13(1,1))
      equivalence (STDATA(1,261),STDA14(1,1))
!     Data for common bloack lspion
      data AFACT/0.0/,SK/0.0/,X0/0.0/,X1/0.0/,CBAR/0.0/,DELTA0/0.0/,
     *IEV/0.0/,ISSB/0/
      data LMED/24/,NMED/278/
      data MEDTB1/'H','2','-','G','A','S',18*' ','H','2','-','L','I','Q'
     *,'U','I','D',15*' ','H','E','-','G','A','S',18*' ','L','I',22*' ',
     * 'B','E',22*' ','B',23*' ','C','-','2','.','2','6','5',' ','G','/'
     *,'C','M','*','*','3',9*' ', 'C','-','2','.','0','0',' ','G','/','C
     *','M','*','*','3',10*' ','C','-','1','.','7','0',' ','G','/','C','
     *M','*','*','3',10*' ','N','2','-','G','A','S',18*' ', 'O','2','-',
     *'G','A','S',18*' ','F',23*' ','N','E','-','G','A','S',18*' ','N','
     *A',22*' ', 'M','G',22*' ','A','L',22*' ','S','I',22*' ','P',23*' '
     *,'S',23*' ', 'C','L',22*' '/
      data MEDTB2/'A','R','-','G','A','S',18*' ','K',23*' ','C','A',22*'
     * ','S','C',22*' ', 'T','I',22*' ','V',23*' ','C','R',22*' ','M','N
     *',22*' ', 'F','E',22*' ','C','O',22*' ','N','I',22*' ','C','U',22*
     *' ', 'Z','N',22*' ','G','A',22*' ','G','E',22*' ','A','S',22*' ', 
     *'S','E',22*' ','B','R',22*' ','K','R','-','G','A','S',18*' ','R','
     *B',22*' '/
      data MEDTB3/'S','R',22*' ','Y',23*' ','Z','R',22*' ','N','I',22*' 
     *','M','O',22*' ', 'T','C',22*' ','R','U',22*' ','R','H',22*' ','P'
     *,'D',22*' ', 'A','G',22*' ','C','D',22*' ','I','N',22*' ','S','N',
     *22*' ', 'S','B',22*' ','T','E',22*' ','I',23*' ','X','E','-','G','
     *A','S',18*' ', 'C','S',22*' ','B','A',22*' ','L','A',22*' '/      
      data MEDTB4/'C','E',22*' ','P','R',22*' ','N','D',22*' ','P','M',2
     *2*' ', 'S','M',22*' ','E','U',22*' ','G','D',22*' ','T','B',22*' '
     *,'D','Y',22*' ', 'H','O',22*' ','E','R',22*' ','T','M',22*' ','Y',
     *'B',22*' ','L','U',22*' ', 'H','F',22*' ','T','A',22*' ','W',23*' 
     *','R','E',22*' ','O','S',22*' ', 'I','R',22*' '/
      data MEDTB5/'P','T',22*' ','A','U',22*' ','H','G',22*' ','T','L',2
     *2*' ', 'P','B',22*' ','B','I',22*' ','P','O',22*' ','R','N','-','G
     *','A','S',18*' ', 'R','A',22*' ','A','C',22*' ','T','H',22*' ','P'
     *,'A',22*' ', 'U',23*' ','N','P',22*' ','P','U',22*' ','A','M',22*'
     * ', 'C','M',22*' ','B','K',22*' ','A',' ','1','5','0','-',
     *'P','L','A','S','T','I','C',11*' ', 'A','C','E','T','O','N','E'
     *,17*' '/
      data MEDTB6/'A','C','E','T','Y','L','E','N','E',15*' ','A','D','E'
     *,'N','I','N','E',17*' ','A','D','I','P','O','S','E',' ','T','I','S
     *','S','U','E',10*' ', 'A','I','R','-','G','A','S',17*' ','A','L','
     *A','N','I','N','E',17*' ','A','L','U','M','I','N','I','U','M',' ',
     *'O','X','I','D','E',9*' ', 'A','M','B','E','R',19*' ','A','M','M',
     *'O','N','I','A',17*' ','A','N','I','L','I','N','E',17*' ', 'A','N'
     *,'T','H','R','A','C','E','N','E',14*' ','B','-','1','0','0',' ','B
     *','O','N','E','-','E','Q','.',' ','P','L','A','S','T','I','C',2*' 
     *', 'B','A','K','E','L','I','T','E',16*' ','B','A','R','I','U','M',
     *' ','F','L','U','O','R','I','D','E',9*' ', 'B','A','R','I','U','M'
     *,' ','S','U','L','F','A','T','E',10*' ','B','E','N','Z','E','N','E
     *',17*' ', 'B','E','R','Y','L','L','I','U','M',' ','O','X','I','D',
     *'E',9*' ','B','G','O',21*' ','B','L','O','O','D',' ','(','I','C',
     *'R','P',')',12*' ','B','O','N','E',',',' ','C','O','M',
     *'P','A','C','T',' ','(','I','C','R','U',')',4*' ', 'B','O','N','E'
     *,' ','C','O','R','T','I','C','A','L',' ','(','I','C','R','P',')',4
     **' '/
      data MEDTB7/'B','O','R','O','N',' ','C','A','R','B','I','D','E',11
     **' ','B','O','R','N',' ','O','X','I','D','E',14*' ', 'B','R','A','
     *I','N',' ','(','I','C','R','P',')',12*' ','B','U','T','A','N','E',
     *18*' ', 'N','-','B','U','T','Y','L',' ','A','L','C','H','O','L',10
     **' ','C','-','5','5','2',' ','A','I','R','-','E','Q','.',' ','P','
     *L','A','S','T','I','C',' ',' ',' ', 'C','A','D','M','I','U','M',' 
     *','T','E','L','L','U','R','I','D','E',7*' ','C','A','D','M','I','U
     *','M',' ','T','U','N','G','S','T','A','T','E',7*' ', 'C','A','L','
     *C','I','U','M',' ','C','A','R','B','O','N','I','T','E',7*' ','C','
     *A','F','2',20*' '/
      data MEDTB8/'C','A','L','C','I','U','M',' ','O','X','I','D','E',11
     **' ','C','A','L','C','I','U','M',' ','S','U','L','F','A','T','E',9
     **' ', 'C','A','L','C','I','U','M',' ','T','U','N','G','S','T','A',
     *'T','E',7*' ','C','A','R','B','O','N',' ','D','I','O','X','I','D',
     *'E',10*' ', 'C','A','R','B','O','N',' ','T','E','T','R','A','C','H
     *','L','O','R','I','D','E',4*' ','C','E','L','L','O','P','H','A','N
     *','E',14*' ', 'C','E','L','L','U','L','O','S','E',' ','A','C','E',
     *'T','A','T','E',' ','B','U','T','Y','R','A','C','E','L','L','U','L
     *','O','S','E',' ','N','I','T','R','A','T','E',7*' ', 'C','E','R','
     *I','C',' ','S','U','R','F','A','R','E',' ','D','O','S','I','M','E'
     *,'T','E','R',1*' ','C','E','S','I','U','M',' ','F','L','U','O','R'
     *,'I','D','E',9*' '/
      data MEDTB9/'C','S','I',21*' ','C','H','L','O','R','O','B','E',
     *'N','Z','E','N','E',11*' ','C',
     *'H','L','O','R','O','F','O','R','M',14*' ','C','O','N','C','R',
     *'E','T','E',',',' ','P','O','R','T','L','A','N','D',6*' ', 'C','Y'
     *,'C','L','O','H','E','X','A','N','E',13*' ','1',',','2','-','D','I
     *','C','H','L','O','R','O','B','E','N','Z','E','N','E',5*' ', 'D','
     *I','C','H','L','O','R','O','D','I','E','T','H','Y','L',' ','E','T'
     *,'H','E','R',3*' ','1',',','2','-','D','I','C','H','L','O','R','O'
     *,'E','T','H','A','N','E',6*' ', 'D','I','E','T','H','Y','L',' ','E
     *','T','H','E','R',11*' ','N',',','N','-','D','I','M','E','T','H','
     *Y','L',' ','F','O','R','M','A','M','I','D','E',' ',' '/
      data MEDT10/'D','I','M','E','T','H','Y','L',' ','S','U','L','F','O
     *','X','I','D','E',6*' ','E','T','H','A','N','E',18*' ', 'E','T','H
     *','Y','L',' ','A','L','C','O','H','O','L',11*' ','E','T','H','Y','
     *L',' ','C','E','L','L','U','L','O','S','E',9*' ', 'E','T','H','Y',
     *'L','E','N','E',16*' ','E','Y','E',' ','L','E','N','S',' ','(','I'
     *,'C','R','P',')',9*' ', 'F','E','R','R','I','C',' ','O','X','I','D
     *','E',12*' ','F','E','R','R','O','B','O','R','I','D','E',13*' ', '
     *F','E','R','R','O','U','S',' ','O','X','I','D','E',11*' ','F','E',
     *'R','R','O','U','S',' ','S','U','L','F','A','T','E',' ','D','O','S
     *','I','M','E','T','E'/
      data MEDT11/'F','R','E','O','N','-','1','2',16*' ','F','R','E','O'
     *,'N','-','1','2','B','2',14*' ','F','R','E','O','N','-','1','3',16
     **' ', 'F','R','E','O','N','-','1','3','B','1',14*' ','F','R','E','
     *O','N','-','1','3','I','1',14*' ', 'G','A','D','O','L','I','N','I'
     *,'U','M',' ','O','X','Y','S','U','L','F','I','D','E',' ',' ',' ','
     *G','A','L','L','I','U','M',' ','A','R','S','E','N','I','D','E',8*'
     * ', 'G','E','L',' ','I','N',' ','P','H','O','T','O','G','R','A','P
     *','H','I','C',' ','E','M','U','L','P','Y','R','E','X','-','G','L',
     *'A','S',14*' ', 'G','L','A','S','S',',',' ','L','E','A','D',
     *13*' '/
      data MEDT12/'G','L','A','S','S',',',' ','P','L','A','T','E',12*' '
     *, 'G','L','U','C','O','S','E',17*' ','G','L','U','T','A','M','I','
     *N','E',15*' ','G','L','Y','C','E','R','O','L',16*' ', 'G','U','A',
     *'N','I','N','E',17*' ','G','Y','P','S','U','M',',',' ','P','L','A'
     *,'S','T','E','R',' ','O','F',' ','P','A','R','I','S', 'N','-','H',
     *'E','P','T','A','N','E',15*' ','N','-','H','E','X','A','N','E',16*
     *' ', 'K','A','P','T','O','N',18*' ',
     *'L','A','N','T','H','A','N','U'
     *,'M',' ','O','X','Y','B','R','O','M','I','D','E',4*' '/
      data MEDT13/'L','A','N','T','H','A','N','U','M',' ','O','X','Y','S
     *','U','L','F','I','D','E',4*' ','L','E','A','D',' ','O','X','I','D
     *','E',14*' ', 'L','I','T','H','I','U','M',' ','A','M','I','D','E',
     *11*' ','L','I','T','H','I','U','M',' ','C','A','R','B','O','N','A'
     *,'T','E',7*' ','L','I','F',21*' ',
     *'L','I','T','H','I','U','M',' ','H','Y','D','R','I','D','E',9*' ',
     *'L','I','I',21*' ','L','I','T','H','I','U','M',' ','O','X','I',
     *'D','E',11*' ', 'L','I','T','H','I','U','M',' ','T','E','T','R',
     *'A','B','O','R','A','T','E',5*' ','L','U','N','G',' ',
     *'(','I','C','R','P',')',13*' '/
      data MEDT14/'M','3',' ','W','A','X',18*' ','M','A','G','N','E','S'
     *,'I','U','M',' ','C','A','R','B','O','N','A','T','E',5*' ', 'M','A
     *','N','E','S','I','U','M',' ','F','L','U','O','R','I','D','E',7*' 
     *','M','A','G','N','E','S','I','U','M',' ','O','X','I','D','E',9*' 
     *', 'M','A','G','N','E','S','I','U','M',' ','T','E','T','R','A','B'
     *,'O','R','A','T','E',3*' ','M','E','R','C','U','R','I','C',' ','I'
     *,'O','D','I','D','E',9*' ', 'M','E','T','H','A','N','E',17*' ','M'
     *,'E','T','H','A','N','O','L',16*' ', 'M','I','X',' ','D',' ','W','
     *A','X',15*' ','M','S','2','0',' ','T','I','S','S','U','E',' ','S',
     *'U','B','S','T','I','T','U','T','E',' ',' '/
      data MEDT15/'M','U','S','C','L','E',',',' ','S','K','E','L','E','T
     *','A','L',' ','(','I','C','R','P',')',' ','M','U','S','C','L','E',
     *',',' ','S','T','R','I','A','T','E','D',' ','(','I','C','R','U',')
     *',' ', 'M','U','S','C','L','E','-','E','Q','.',' ','L','I','Q','.'
     *,' ','W',' ','S','U','C','R','O','S','M','U','S','C','L','E','-','
     *E','Q','.',' ','L','I','Q','.',' ','W','/','O',' ','S','U','C','R'
     *, 'N','A','P','T','H','A','L','E','N','E',14*' ','N','I','T','R','
     *O','B','E','N','Z','E','N','E',12*' ', 'N','I','T','R','O','U','S'
     *,' ','O','X','I','D','E',11*' ','N','Y','L','O','N',',',' ','D','U
     *',' ','P','O','N','T',10*' ', 'N','Y','L','O','N',',',' ','T','Y',
     *'P','E',' ','6',' ','A','N','D',' ','6','/','6',3*' ','N','Y','L',
     *'O','N',',',' ','T','Y','P','E',' ','6','/','1','0',8*' '/
      data MEDT16/'N','Y','L','O','N',',',' ','T','Y','P','E',' ','1','1
     *',10*' ','O','C','T','A','N','E',',',' ','L','I','Q','U','I','D',1
     *0*' ', 'P','A','R','A','F','F','I','N',' ','W','A','X',12*' ','N',
     *'-','P','E','N','T','A','N','E',15*' ', 'P','H','O','T','O',
     *'E','M','U','L','S','I','O','N',11*' ','P','L','A','S','T','I',
     *'C',' ','S','C','I','N','T','.',10*' ', 'P',
     *'L','U','T','O','N','I','U','M',' ','D','I','O','X','I','D','E',7*
     *' ','P','O','L','Y','C','R','Y','L','O','N','I','T','R','I','L','E
     *',8*' ', 'P','O','L','Y','C','A','R','B','O','N','A','T','E',11*' 
     *','P','O','L','Y','C','H','L','O','R','O','S','T','Y','R','W','N',
     *'E',7*' '/
      data MEDT17/'P','O','L','Y','E','T','H','Y','L','E','N','E',12*' '
     *,'M','Y','L','A','R',19*' ','L','U','C','I','T','E',18*' ', 'P','O
     *','L','Y','O','X','Y','M','E','T','H','Y','L','E','N','E',8*' ','P
     *','O','L','Y','P','R','O','P','Y','L','E','N','E',11*' ', 'P','O',
     *'L','Y','S','T','Y','R','E','N','E',13*' ','T','E','F','L','O','N'
     *,18*' ', 'P','O','L','Y','T','R','I','F','L','U','O','R','O','C','
     *H','L','O','R','O','E','T','H','Y','.','P','O','L','Y','V','I','N'
     *,'Y','L',' ','A','C','E','T','A','T','E',7*' ', 'P','O','L','Y','V
     *','I','N','Y','L',' ','A','L','C','O','H','O','L',7*' '/
      data MEDT18/'P','O','L','Y','V','I','N','Y','L',' ','B','U','T','Y
     *','R','A','L',7*' ', 'P','O','L','Y','V','I','N','Y','L',' ','C','
     *H','L','O','R','I','D','E',6*' ','S','A','R','A','N',19*' ', 'P','
     *L','O','Y','V','I','N','Y','L','I','D','E','N','E',' ','F','L','U'
     *,'O','R','I','D','E',' ','P','O','L','Y','V','I','N','Y','L',' ','
     *P','Y','R','R','O','L','I','D','O','N','E',' ',' ',' ', 'P','O','T
     *','A','S','S','I','U','M',' ','I','O','D','I','N','E',8*' ','P','O
     *','T','A','S','S','I','U','M',' ','O','X','I','D','E',9*' ', 'P','
     *R','O','P','A','N','E',17*' ','P','R','O','P','A','N','E',',',' ',
     *'L','I','Q','U','I','D',9*' ', 'N','-','P','R','O','P','Y','L',' '
     *,'A','L','C','O','H','O','L',8*' '/
      data MEDT19/'P','Y','R','I','D','I','N','E',16*' ','R','U','B','B'
     *,'E','R',',',' ','B','U','T','Y','L',11*' ', 'R','U','B','B','E','
     *R',',',' ','N','A','T','U','R','A','L',9*' ','R','U','B','B','E','
     *R',',',' ','N','E','O','P','R','E','N','E',8*' ', 'S','I','O','2',
     *20*' ','A','G','B','R',20*' ', 'A','G','C','L',20*' ',
     *'S','I','L','V','E','R',' ','H',
     *'A','L','I','D','E','S',' ','I','N',' ','E','M','U',
     *'L','.',' ', 'S','I','L','V','E','R',' ','I','O','D','I','D','E',1
     *1*' ','S','K','I','N',' ','(','I','C','R','P',')',13*' '/
      data MEDT20/'S','O','D','I','U','M',' ','C','A','R','B','O','N','A
     *','T','E',8*' ','N','A','I',21*' ', 'S','O','D','I','U','M',' ',
     *'M','O','N','O','X','I','D','E',9*' ',
     *'S','O','D','I','U','M',' ','N','I','T','R','A','T','E',
     *10*' ', 'S','T','I','L','B','E','N','E',16*' ','S','U','C','R','O'
     *,'S','E',17*' ', 'T','R','R','P','H','E','N','Y','L',15*' ','T','E
     *','S','T','E','S',' ','(','I','C','R','P',')',11*' ', 'T','E','T',
     *'R','A','C','H','L','O','R','O','E','T','H','T','L','E','N','E',5*
     *' ','T','H','A','L','L','I','U','M',' ','C','H','L','O','R','I','D
     *','E',7*' '/
      data MEDT21/'T','I','S','S','U','E',',',' ','S','O','F','T',' ','(
     *','I','C','R','P',')',5*' ','I','C','R','U',' ','F','O','U','R','-
     *','C','O','M','P','.',' ','T','I','S','S','U','E',' ',' ', 'T','I'
     *,'S','S','U','E','-','E','Q','.',' ','G','A','S',' ','(','M','E','
     *T','H','A','N','E',')','T','I','S','S','U','E','-','E','Q','.',' '
     *,'G','A','S',' ','(','P','R','O','P','A','N','E',')', 'T','I','T',
     *'A','N','I','U','M',' ','D','I','O','X','I','D','E',8*' ','T','O',
     *'L','U','E','N',18*' ', 'T','R','I','C','H','L','O','R','O','E','T
     *','H','Y','L','E','N','E',7*' ','T','R','I','E','T','H','Y','L',' 
     *','P','H','O','S','P','H','A','T','E',6*' ', 'T','U','N','G','S','
     *T','E','N',' ','H','E','X','A','F','L','U','O','R','I','D','E',3*'
     * ', 'U','R','A','N','I','U','M',' ','D','I','C','A','R','B','I','D
     *','E',7*' '/
      data MEDT22/'U','R','A','N','I','U','M',' ','M','O','N','O','C','A
     *','R','B','I','D','E',5*' ','U','R','A','N','I','U','M',' ','O','X
     *','I','D','E',11*' ', 'U','R','E','A',20*' ','V','A','L','I','N','
     *E',18*' ','V','I','T','O','N',19*' ', 'H','2','O',21*' ','H','2',
     *'O',' ','V','A','P','O','R',15*' ', 
     *'X','Y','L','E','N','E',18*' '/
      data STDAT1/0.14092,5.7273,1.8639,3.2718,19.2,9.5835,0.0, 0.13483,
     *5.6249,0.4759,1.9215,21.8,3.2632,0.0, 0.13443,5.8347,2.2017,3.6122
     *,41.8,11.1393,0.0, 0.95136,2.4993,0.1304,1.6397,40.0,3.1221,0.14, 
     *0.80392,2.4339,0.0592,1.6922,63.7,2.7847,0.14, 0.56224,2.4512,0.03
     *05,1.9688,76.0,2.8477,0.14, 0.26142,2.8697,-0.0178,2.3415,78.0,2.8
     *680,0.12, 0.20240,3.0036,-0.0351,2.4860,78.0,2.9925,0.10, 0.20762,
     *2.9532,0.0480,2.5387,78.0,3.1550,0.14, 0.15349,3.2125,1.7378,4.132
     *3,82.0,10.5400,0.0, 0.11778,3.2913,1.7541,4.3213,95.0,10.7004,0.0,
     * 0.11083,3.2962,1.8433,4.4096,115.0,10.9653,0.0, 0.08064,3.5771,2.
     *0735,4.6421,137.0,11.9041,0.0, 0.07772,3.6452,0.2880,3.1962,149.0,
     *5.0526,0.08, 0.08163,3.6166,0.1499,3.0668,156.0,4.5297,0.08, 0.080
     *24,3.6345,0.1708,3.0127,166.0,4.2395,0.12, 0.14921,3.2546,0.2014,2
     *.8715,173.0,4.4351,0.14, 0.23610,2.9158,0.1696,2.7815,173.0,4.5214
     *,0.14, 0.33992,2.6456,0.1580,2.7159,180.0,4.6659,0.14, 0.19849,2.9
     *702,1.5555,4.2994,174.0,11.1421,0.0/
      data STDAT2/0.19714,2.9618,1.7635,4.4855,188.0,11.9480,0.0, 0.1982
     *7,2.9233,0.3851,3.1724,190.0,5.6423,0.10, 0.15643,3.0745,0.3228,3.
     *1191,191.0,5.0396,0.14, 0.15754,3.0517,0.1640,3.0593,216.0,4.6949,
     *0.10, 0.15662,3.0302,0.0957,3.0386,233.0,4.4450,0.12, 0.15436,3.01
     *63,0.0691,3.0322,245.0,4.2659,0.14, 0.15419,2.9896,0.0340,3.0451,2
     *57.0,4.1781,0.14, 0.14973,2.9796,0.0447,3.1074,272.0,4.2702,0.14, 
     *0.14680,2.9632,-0.0012,3.1531,286.0,4.2911,0.12, 0.14474,2.9502,-0
     *.0187,3.1790,297.0,4.2601,0.12, 0.16496,2.8430,-0.0566,3.1851,311.
     *0,4.3115,0.10, 0.14339,2.9044,-0.0254,3.2792,322.0,4.4190,0.08, 0.
     *14714,2.8652,0.0049,3.3668,330.0,4.6906,0.08, 0.09440,3.1314,0.226
     *7,3.5434,334.0,4.9353,0.14, 0.07188,3.3306,0.3376,3.6096,350.0,5.1
     *411,0.14, 0.06633,3.4176,0.1767,3.5702,347.0,5.0510,0.08, 0.06568,
     *3.4317,0.2258,3.6264,348.0,5.3210,0.10, 0.06335,3.4670,1.5262,4.98
     *99,343.0,11.7307,0.0, 0.07446,3.4051,1.7158,5.0748,352.0,12.5115,0
     *.0, 0.07261,3.4177,0.5737,3.7995,363.0,6.4776,0.14/
      data STDAT3/0.07165,3.4435,0.4585,3.6778,366.0,5.9867,0.14, 0.0713
     *8,3.4585,0.3608,3.5542,379.0,5.4801,0.14, 0.07177,3.4533,0.2957,3.
     *4890,393.0,5.1774,0.14, 0.13883,3.0930,0.1785,3.2201,417.0,5.0141,
     *0.14, 0.10525,3.2549,0.2267,3.2784,424.0,4.8793,0.14, 0.16572,2.97
     *38,0.0949,3.1253,428.0,4.7769,0.14, 0.19342,2.8707,0.0599,3.0834,4
     *41.0,4.7694,0.14, 0.19205,2.8633,0.0576,3.1069,449.0,4.8008,0.14, 
     *0.24178,2.7239,0.0563,3.0555,470.0,4.9358,0.14, 0.24585,2.6899,0.0
     *657,3.1074,470.0,5.0630,0.14, 0.24609,2.6772,0.1281,3.1667,469.0,5
     *.2727,0.14, 0.23879,2.7144,0.2406,3.2032,488.0,5.5211,0.14, 0.1868
     *9,2.8576,0.2879,3.2959,488.0,5.5340,0.14, 0.16652,2.9519,0.3189,3.
     *3489,487.0,5.6241,0.14, 0.13815,3.0354,0.3296,3.4418,485.0,5.7131,
     *0.14, 0.23766,2.7276,0.0549,3.2596,491.0,5.9488,0.0, 0.23314,2.741
     *4,1.5630,4.7371,482.0,12.7281,0.0, 0.18233,2.8866,0.5473,3.5914,48
     *8.0,6.9135,0.14, 0.18268,2.8906,0.4190,3.4547,491.0,6.3153,0.14, 0
     *.18591,2.8828,0.3161,3.3293,501.0,5.7850,0.14/
      data STDAT4/0.18885,2.8592,0.2713,3.3432,523.0,5.7837,0.14, 0.2326
     *5,2.7331,0.2333,3.2773,535.0,5.8096,0.14, 0.23530,2.7050,0.1984,3.
     *3063,546.0,5.8290,0.14, 0.24280,2.6674,0.1627,3.3199,560.0,5.8224,
     *0.14, 0.24698,2.6403,0.1520,3.3460,574.0,5.8597,0.14, 0.24448,2.62
     *45,0.1888,3.4633,580.0,6.2278,0.14, 0.25109,2.5977,0.1058,3.3932,5
     *91.0,5.8738,0.14, 0.24453,2.6056,0.0947,3.4224,614.0,5.9045,0.14, 
     *0.24665,2.5849,0.0822,3.4474,628.0,5.9183,0.14, 0.24638,2.5726,0.0
     *761,3.4782,650.0,5.9587,0.14, 0.24823,2.5573,0.0648,3.4922,658.0,5
     *.9521,0.14, 0.24889,2.5469,0.0812,3.5085,674.0,5.9677,0.14, 0.2529
     *5,2.5141,0.1199,3.6246,684.0,6.3325,0.14, 0.24033,2.5643,0.1560,3.
     *5218,694.0,5.9785,0.14, 0.22918,2.6155,0.1965,3.4337,705.0,5.7139,
     *0.14, 0.17798,2.7623,0.2117,3.4805,718.0,5.5262,0.14, 0.15509,2.84
     *47,0.2167,3.4960,727.0,5.4059,0.14, 0.15184,2.8627,0.0559,3.4845,7
     *36.0,5.3445,0.08, 0.12751,2.9608,0.0891,3.5414,746.0,5.3083,0.10, 
     *0.12690,2.9658,0.0819,3.5480,757.0,5.3418,0.10/
      data STDAT5/0.11128,3.0417,0.1484,3.6212,790.0, 5.4732,0.12, 0.097
     *56,3.1101,0.2021,3.6979,790.0, 5.5747,0.14, 0.11014,3.0519,0.2756,
     *3.7275,800.0, 5.9605,0.14, 0.09455,3.1450,0.3491,3.8044,810.0, 6.1
     *365,0.14, 0.09359,3.1608,0.3776,3.8073,823.0, 6.2018,0.14, 0.09410
     *,3.1671,0.4152,3.8248,823.0, 6.3505,0.14, 0.09282,3.1830,0.4267,3.
     *8293,830.0, 6.4003,0.14, 0.20798,2.7409,1.5368,4.9889,794.0,13.283
     *9,0.0, 0.08804,3.2454,0.5991,3.9428,826.0, 7.0452,0.14, 0.08567,3.
     *2683,0.4559,3.7966,841.0, 6.3742,0.14, 0.08655,3.2610,0.4202,3.768
     *1,847.0, 6.2473,0.14, 0.14770,2.9845,0.3144,3.5079,878.0, 6.0327,0
     *.14, 0.19677,2.8171,0.2260,3.3721,890.0, 5.8694,0.14, 0.19741,2.80
     *82,0.1869,3.3690,902.0, 5.8149,0.14, 0.20419,2.7679,0.1557,3.3981,
     *921.0, 5.8748,0.14, 0.20308,2.7615,0.2274,3.5021,934.0, 6.2813,0.1
     *4, 0.20257,2.7579,0.2484,3.5160,939.0, 6.3097,0.14, 0.20192,2.7560
     *,0.2378,3.5186,952.0, 6.2912,0.14, 0.10783,3.4442,0.1329,2.6234, 6
     *5.1, 3.1100,0.0, 0.11100,3.4047,0.2197,2.6028, 64.2, 3.4341,0.0/
      data STDAT6/0.12167,3.4277, 1.6017,4.0074, 58.2, 9.8419,0.0, 0.209
     *08,3.0271, 0.1295,2.4219, 71.4, 3.1724,0.0, 0.10278,3.4817, 0.1827
     *,2.6530, 63.2, 3.2367,0.0, 0.10914,3.3994, 1.7418,4.2759, 85.7,10.
     *5961,0.0, 0.11484,3.3526, 0.1354,2.6336, 71.9, 3.0965,0.0, 0.08500
     *,3.5458, 0.0402,2.8665,145.2, 3.5682,0.0, 0.11934,3.4098, 0.1335,2
     *.5610, 63.2, 3.0701,0.0, 0.08315,3.6464, 1.6822,4.1158, 53.7, 9.87
     *63,0.0, 0.13134,3.3434, 0.1618,2.5805, 66.2, 3.2622,0.0, 0.14677,3
     *.2831, 0.1146,2.5213, 69.5, 3.1514,0.0, 0.05268,3.7365, 0.1252,3.0
     *420, 85.9, 3.4528,0.0, 0.12713,3.3470, 0.1471,2.6055, 72.4, 3.2582
     *,0.0, 0.15991,2.8867,-0.0098,3.3871,375.9, 5.4122,0.0, 0.11747,3.0
     *427,-0.0128,3.4069,285.7, 4.8923,0.0, 0.16519,3.2174, 0.1710,2.509
     *1, 63.4, 3.3269,0.0, 0.10755,3.4927, 0.0241,2.5846, 93.2, 2.9801,0
     *.0, 0.09569,3.0781, 0.0456,3.7816,534.1, 5.7409,0.0, 0.08492,3.540
     *6, 0.2239,2.8017, 75.2, 3.4581,0.0, 0.05822,3.6419, 0.0944,3.0201,
     * 91.9, 3.3390,0.0, 0.06198,3.5919, 0.1161,3.0919,106.4, 3.6488,0.0
     */
      data STDAT7/0.37087,2.8076,0.0093,2.1006,84.7,2.9859,0.0, 0.11548,
     *3.3832,0.1843,2.7379,99.6,3.6027,0.0, 0.08255,3.5585,0.2206,2.8021
     *,73.3,3.4279,0.0, 0.10852,3.4884,1.3788,3.7524,48.3,8.5633,0.0, 0.
     *10081,3.5139,0.1937,2.6439,59.9,3.2425,0.0, 0.10492,3.4344,0.1510,
     *2.7083,86.8,3.3338,0.0, 0.24840,2.6665,0.0438,3.2836,539.3,5.9096,
     *0.0, 0.12861,2.9150,0.0123,3.5941,468.3,5.3594,0.0, 0.08301,3.4120
     *,0.0492,3.0549,136.4,3.7738,0.0, 0.06942,3.5263,0.0676,3.1683,166.
     *0,4.0653,0.0, 0.12128,3.1936,-0.0172,3.0171,176.1,4.1209,0.0, 0.07
     *708,3.4495,0.0587,3.1229,152.3,3.9388,0.0, 0.06210,3.2649,0.0323,3
     *.8932,395.0,5.2603,0.0, 0.11768,3.3227,1.6294,4.1825,85.0,10.1537,
     *0.0, 0.19018,3.0116,0.1773,2.9165,166.3,4.7712,0.0, 0.11151,3.3810
     *,0.1580,2.6778,77.6,3.2647,0.0, 0.11444,3.3738,0.1794,2.6809,74.6,
     *3.3497,0.0, 0.11813,3.3237,0.1897,2.7253,87.0,3.4762,0.0, 0.07666,
     *3.5607,0.2363,2.8769,76.7,3.5212,0.0, 0.22052,2.7280,0.0084,3.3374
     *,440.7,5.9046,0.0/
      data STDAT8/0.25381,2.6657,0.0395,3.3353,553.1,6.2807,0.0, 0.09856
     *,3.3797,0.1714,2.9272,89.1,3.8201,0.0, 0.16959,3.0627,0.1786,2.958
     *1,156.0,4.7055,0.0, 0.07515,3.5467,0.1301,3.0466,135.2,3.9464,0.0,
     * 0.12035,3.4278,0.1728,2.5549,56.4,3.1544,0.0, 0.16010,3.0836,0.15
     *87,2.8276,106.5,4.0348,0.0, 0.06799,3.5250,0.1773,3.1586,103.5,4.0
     *135,0.0, 0.13383,3.1675,0.1375,2.9529,111.9,4.1849,0.0, 0.10550,3.
     *4586,0.2231,2.6745,60.0,3.3721,0.0, 0.11470,3.3710,0.1977,2.6686,6
     *6.6,3.3311,0.0, 0.06619,3.5708,0.2021,3.1263,98.6,3.9844,0.0, 0.09
     *627,3.6095,1.5107,3.8743,45.4,9.1043,0.0, 0.09878,3.4834,0.2218,2.
     *7052,62.9,3.3699,0.0, 0.11077,3.4098,0.1683,2.6257,69.3,3.2415,0.0
     *, 0.10636,3.5387,1.5528,3.9327,50.7,9.4380,0.0, 0.09690,3.4550,0.2
     *070,2.7446,73.3,3.3720,0.0, 0.10478,3.1313,-0.0074,3.2573,227.3,4.
     *2245,0.0, 0.12911,3.0240,-0.0988,3.1749,261.0,4.2057,0.0, 0.12959,
     *3.0168,-0.0279,3.2002,248.6,4.3175,0.0, 0.08759,3.4923,0.2378,2.82
     *54,76.4,3.5183,0.0/
      data STDAT9/0.07978,3.4626,0.3035,3.2659,143.0,4.8251,0.0, 0.05144
     *,3.5565,0.3406,3.7956,284.9,5.7976,0.0, 0.07238,3.5551,0.3659,3.23
     *37,126.6,4.7483,0.0, 0.03925,3.7194,0.3522,3.7554,210.5,5.3555,0.0
     *, 0.09112,3.1658,0.2847,3.7280,293.5,5.8774,0.0, 0.22161,2.6300,-0
     *.1774,3.4045,493.3,5.5347,0.0, 0.07152,3.3356,0.1764,3.6420,384.9,
     *5.3299,0.0, 0.10102,3.4418,0.1709,2.7058,74.8,3.2687,0.0, 0.08270,
     *3.5224,0.1479,2.9933,134.0,3.9708,0.0, 0.09544,3.0740,0.0614,3.814
     *6,526.4,5.8476,0.0, 0.07678,3.5381,0.1237,3.0649,145.4,4.0602,0.0,
     * 0.10783,3.3946,0.1411,2.6700,77.2,3.1649,0.0, 0.11931,3.3254,0.13
     *47,2.6301,73.3,3.1167,0.0, 0.10168,3.4481,0.1653,2.6862,72.6,3.226
     *7,0.0, 0.20530,3.0186,0.1163,2.4296,75.0,3.1171,0.0, 0.06949,3.513
     *4,0.0995,3.1206,129.7,3.8382,0.0, 0.11255,3.4885,0.1928,2.5706,54.
     *4,3.1978,0.0, 0.11085,3.5027,0.1984,2.5757,54.0,3.2156,0.0, 0.1597
     *2,3.1921,0.1509,2.5631,79.6,3.3497,0.0, 0.17830,2.8457,-0.0350,3.3
     *288,439.7,5.4666,0.0/
      data STDA10/0.21501,2.7298,-0.0906,3.2664,421.2,5.4470,0.0, 0.1964
     *5,2.7299,0.0356,3.5456,766.7,6.2162,0.0, 0.08740,3.7534,0.0198,2.5
     *152,55.5,2.7961,0.0, 0.09936,3.5417,0.0551,2.6598,87.9,3.2029,0.0,
     * 0.07593,3.7478,0.0171,2.7049,94.0,3.1667,0.0, 0.90567,2.5849,-0.0
     *988,1.4515,36.5,2.3580,0.0, 0.23274,2.7146,0.0892,3.3702,485.1,6.2
     *671,0.0, 0.08035,3.7878,-0.0511,2.5874,73.6,2.9340,0.0, 0.11075,3.
     *4389,0.0737,2.6502,94.6,3.2093,0.0, 0.08588,3.5353,0.2261,2.8001,7
     *5.3,3.4708,0.0, 0.07864,3.6412,0.1523,2.7529,67.9,3.2540,0.0, 0.09
     *219,3.5003,0.0860,2.7997,118.0,3.4319,0.0, 0.07934,3.6485,0.1369,2
     *.8630,134.3,3.7105,0.0, 0.08313,3.5968,0.0575,2.8580,143.8,3.6404,
     *0.0, 0.09703,3.4893,0.1147,2.7635,108.3,3.4328,0.0, 0.21513,2.7264
     *,0.1040,3.4728,684.5,6.3787,0.0, 0.09253,3.6257,1.6263,3.9716,41.7
     *,9.5243,0.0, 0.08970,3.5477,0.2529,2.7639,67.6,3.5160,0.0, 0.07490
     *,3.6823,0.1371,2.7145,60.9,3.0780,0.0, 0.08294,3.6061,0.1997,2.803
     *3,75.1,3.5341,0.0/
      data STDA11/0.08636,3.5330,0.2282,2.7999,75.3,3.4809,0.0, 0.08507,
     *3.5383,0.2249,2.8032,74.7,3.4636,0.0, 0.09481,3.4699,0.2098,2.7550
     *,74.3,3.3910,0.0, 0.09143,3.4982,0.2187,2.7680,74.2,3.4216,0.0, 0.
     *14766,3.2654,0.1374,2.5429,68.4,3.2274,0.0, 0.12727,3.3091,0.1777,
     *2.6630,75.8,3.4073,0.0, 0.11992,3.3318,1.6477,4.1565,84.9,10.1575,
     *0.0, 0.11513,3.4044,0.1503,2.6004,64.3,3.1250,0.0, 0.11818,3.3826,
     *0.1336,2.5834,63.9,3.0634,0.0, 0.11852,3.3912,0.1304,2.5681,63.2,3
     *.0333,0.0, 0.14868,3.2576,0.0678,2.4281,61.6,2.7514,0.0, 0.11387,3
     *.4776,0.1882,2.5664,54.7,3.1834,0.0, 0.12087,3.4288,0.1289,2.5084,
     *55.9,2.9551,0.0, 0.10809,3.5265,0.2086,2.5855,53.6,3.2504,0.0, 0.1
     *2399,3.0094,0.1009,3.4866,331.0,5.3319,0.0, 0.16101,3.2393,0.1464,
     *2.4855,64.7,3.1997,0.0, 0.20594,2.6522,-0.2311,3.5554,746.5,5.9719
     *,0.0, 0.16275,3.1975,0.1504,2.5159,69.6,3.2459,0.0, 0.12860,3.3288
     *,0.1606,2.6225,73.1,3.3201,0.0, 0.07530,3.5441,0.1238,2.9241,81.7,
     *3.4659,0.0/
      data STDA12/0.12108,3.4292,0.1370,2.5177,57.4,3.0016,0.0, 0.12679,
     *3.3076,0.1562,2.6507,78.7,3.3262,0.0, 0.11433,3.3836,0.1824,2.6681
     *,74.0,3.3297,0.0, 0.10808,3.4002,0.1584,2.6838,77.4,3.2514,0.0, 0.
     *15045,3.2855,0.1534,2.4822,59.2,3.1252,0.0, 0.16454,3.2224,0.1647,
     *2.5031,68.7,3.2999,0.0, 0.10606,3.4046,0.1648,2.7404,99.1,3.4161,0
     *.0, 0.07727,3.5085,0.1714,3.0265,120.7,3.8551,0.0, 0.11442,3.3762,
     *0.1769,2.6747,73.7,3.3309,0.0, 0.11178,3.3893,0.1401,2.6315,69.7,3
     *.1115,0.0, 0.11544,3.3983,0.1555,2.6186,67.2,3.1865,0.0, 0.12438,3
     *.2104,0.1559,2.9415,108.2,4.0532,0.0, 0.15466,3.1020,0.1314,2.9009
     *,134.3,4.2506,0.0, 0.10316,3.4200,0.1717,2.7375,88.8,3.3793,0.0, 0
     *.12504,3.3326,0.1324,2.5867,67.7,3.1017,0.0, 0.22053,2.7558,0.1044
     *,3.3442,431.9,6.1088,0.0, 0.16789,3.0121,0.0480,3.0110,189.9,4.646
     *3,0.0, 0.09916,3.5920,1.4326,3.7998,47.1,8.7878,0.0, 0.10329,3.562
     *0,0.2861,2.6568,52.0,3.5529,0.0, 0.09644,3.5415,0.2046,2.6681,61.1
     *,3.2915,0.0/
      data STDA13/0.16399,3.1977,0.1670,2.5245,66.2,3.3148,0.0, 0.12108,
     *3.4296,0.1347,2.5154,56.5,2.9915,0.0, 0.15058,3.2879,0.1512,2.4815
     *,59.8,3.1272,0.0, 0.09763,3.3632,0.1501,2.9461,93.0,3.7911,0.0, 0.
     *08408,3.5064,0.1385,3.0025,139.2,4.0029,0.0, 0.24582,2.6820,0.0352
     *,3.2109,486.6,5.6139,0.0, 0.22968,2.7041,-0.0139,3.2022,398.4,5.34
     *37,0.0, 0.24593,2.6814,0.0353,3.2117,487.1,5.6166,0.0, 0.25059,2.6
     *572,0.0148,3.2908,543.5,5.9342,0.0, 0.09459,3.4643,0.2019,2.7526,7
     *2.7,3.3546,0.0, 0.08715,3.5638,0.1287,2.8591,125.0,3.7178,0.0, 0.1
     *2516,3.0398,0.1203,3.5920,452.0,6.0572,0.0, 0.07501,3.6943,0.1652,
     *2.9793,148.8,4.1892,0.0, 0.09391,3.5097,0.1534,2.8221,114.6,3.6502
     *,0.0, 0.16659,3.2168,0.1734,2.5142,67.7,3.3680,0.0, 0.11301,3.3630
     *,0.1341,2.6558,77.5,3.1526,0.0, 0.14964,3.2685,0.1322,2.5429,71.7,
     *3.2639,0.0, 0.08533,3.5428,0.2274,2.7988,75.0,3.4698,0.0, 0.18595,
     *3.0156,0.1713,2.9083,159.2,4.6619,0.0, 0.18599,2.7690,0.0705,3.571
     *6,690.3,6.3009,0.0/
      data STDA14/0.08926,3.5110,0.2211,2.7799,72.3,3.4354,0.0, 0.09629,
     *3.4371,0.2377,2.7908,74.9,3.5087,0.0, 0.09946,3.4708,1.6442,4.1399
     *,61.2,9.9500,0.0, 0.09802,3.5159,1.5139,3.9916,59.5,9.3529,0.0, 0.
     *08569,3.3267,-0.0119,3.1647,179.5,3.9522,0.0, 0.13284,3.3558,0.172
     *2,2.5728,62.5,3.3026,0.0, 0.18272,3.0137,0.1803,2.9140,148.1,4.614
     *8,0.0, 0.06922,3.6302,0.2054,2.9428,81.2,3.6242,0.0, 0.03658,3.513
     *4,0.3020,4.2602,354.4,5.9881,0.0, 0.21120,2.6577,-0.2191,3.5208,75
     *2.0,6.0247,0.0, 0.22972,2.6169,-0.2524,3.4941,862.0,6.1210,0.0, 0.
     *20463,2.6711,-0.1938,3.5292,720.6,5.9605,0.0, 0.11609,3.3461,0.160
     *3,2.6525,72.8,3.2032,0.0, 0.11386,3.3774,0.1441,2.6227,67.7,3.1059
     *,0.0, 0.09965,3.4556,0.2106,2.7874,98.6,3.5943,0.0, 0.09116,3.4773
     *,0.2400,2.8004,75.0,3.5017,0.0, 0.08101,3.5901,1.7952,4.3437,71.6,
     *10.5962,0.0, 0.13216,3.3564,0.1695,2.5675,61.8,3.2698,0.0/
      data EPE/.01/,ZTHRE,ZEPE/80*0.0/,NIPE/20/,NALE/150/,EPG/.01/, ZTHR
     *G/0.0,.1,38*0.0/,ZEPG/0.0,.01,38*0.0/,NIPG/20/,NALG/1000/, EPR/.
     *01/,ZTHRR,ZEPR/80*0.0/,NIPR/20/,NALR/100/,EPSF/.01/,ZTHRS,ZEPS/80*
     *0.0/,NIPS/20/,NALS/100/, EPCP/.03/,ZTHRC,ZEPC/400*0.0/,NIPC/20/,NA
     *LC/2000/
      data NET/100/
      data ASYMT/'H','HE','LI','BE','B','C','N','O','F','NE', 'NA','MG',
     *'AL','SI','P','S','CL','AR','K','CA','SC','TI', 'V','CR','MN','FE'
     *,'CO','NI','CU','ZN','GA','GE','AS','SE','BR', 'KR','RB','SR','Y',
     *'ZR','NB','MO','TC','RU','RH','PD','AG','CD', 'IN','SN','SB','TE',
     *'I','XE','CS','BA','LA','CE','PR','ND', 'PM','SM','EU','GD','TB','
     *DY','HO','ER','TM','YB','LU','HF','TA', 'W','RE','OS','IR','PT','A
     *U','HG','TL','PB','BI','PO','AT','RN', 'FR','RA','AC','TH','PA','U
     *','NP','PU','AM','CM','BK','CF','ES', 'FM'/
      data WATBL/1.00797,4.0026,6.939,9.0122,10.811,12.01115,14.0067, 15
     *.9994,18.9984,20.183,22.9898,24.312,26.9815,28.088,30.9738, 32.064
     *,35.453,39.948,39.102,40.08,44.956,47.90,50.942,51.998, 54.9380,55
     *.847,58.9332,58.71,63.54,65.37,69.72,72.59,74.9216, 78.96,79.808,8
     *3.80,85.47,87.62,88.905,91.22,92.906,95.94,99.0, 101.07,102.905,10
     *6.4,107.87,112.4,114.82,118.69,121.75,127.60, 126.9044,131.30,132.
     *905,137.34,138.91, 140.12,140.907,144.24,147.,150.35,151.98,157.25
     *,158.924,162.50, 164.930,167.26,168.934,173.04,174.97,178.49,180.9
     *48,183.85, 186.2,190.2,192.2,195.08,196.987,200.59,204.37,207.19,2
     *08.980, 210.,210.,222.,223.,226.,227.,232.036,231.,238.03,237.,242
     *., 243.,247.,247.,248.,254.,253./
      data RHOTBL/0.0808,0.19,0.534,1.85,2.5,2.26,1.14,1.568,1.5,1.0, 0.
     *9712,1.74,2.702,2.4,1.82,2.07,2.2,1.65,0.86,1.55,3.02,4.54, 5.87,7
     *.14,7.3,7.86,8.71,8.90,8.9333,7.140,5.91,5.36,5.73,4.80, 4.2,3.4,1
     *.53,2.6,4.47,6.4,8.57,9.01,11.50,12.20,12.50,12.,10.5, 8.65,7.30,7
     *.31,6.684,6.24,4.93,2.7,1.873,3.5,6.15,6.90,6.769, 7.007, 1. ,7.54
     *,5.17,7.87,8.25,8.56,8.80,9.06,9.32,6.96,9.85, 11.40,16.60,19.30,2
     *0.53,22.48,22.42,21.45,19.30,14.19,11.85, 11.34,9.78,9.30, 1. ,4.,
     * 1. ,5., 1. ,11.0,15.37,18.90, 20.5,19.737,11.7,7.,1. , 1. , 1. ,
     *1./
      data ITBL/19.2,41.8,40.,63.7,76.0,78.0,82.0,95.0,115.,137., 149.,1
     *56.,166.,173.,173.,180.,174.,188.,190.,191.,216.,233.,245., 257.,2
     *72.,286.,297.,311.,322.,330.,334.,350.,347.,348.,357.,352., 363.,3
     *66.,379.,393.,417.,424.,428.,441.,449.,470.,470.,469.,488., 488.,4
     *87.,485.,491.,482.,488.,491.,501.,523.,535.,546.,560.,574., 580.,5
     *91.,614.,628.,650.,658.,674.,684.,694.,705.,718.,727.,736., 746.,7
     *57.,790.,790.,800.,810.,823.,823.,830.,825.,794.,827.,826., 841.,8
     *47.,878.,890.,902.,921.,934.,939.,952.,966.,980.,994./
      data ISTATB/1,1,0,0,0,0,1,1,1,1,0,0,0,0,0,0,1,1,0,0,0,0,0,0,0, 0,0
     *,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0, 0,0,0,
     *0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0, 0,0,0,0,0
     *,0,0,0,0,0,0,0,0/
      data ALRAD/5.31,4.79,4.74,4.71/,ALRADP/6.144,5.621,5.805,5.924/, A
     *1440/1194.0/,A183/184.15/
      data NFUNS/79/
      data NFARG/1,1,1,1,1,1,1,1,1,1,2,1,3,1,2,1,3,1,2,2,1,3,1,2,1,3,3,1
     *,1,3,3,4,1,1,3,4,2,1,2,2,1,3,1,1,1,1,1,2,1,1,1,1,1,2,1,3,1,3,3,4,1
     *,1,1,1,2,1,1,1,2,1,1,2,2,3,3,1,1,1,1/
      data FNAME(1,1),FNAME(2,1), FNAME(3,1), FNAME(4,1),FNAME(5,1),FNAM
     *E(6,1) / 'A','L','I','N', 2*' '/
      data FNAME(1,2),FNAME(2,2), FNAME(3,2), FNAME(4,2),FNAME(5,2),FNAM
     *E(6,2) / 'A','L','I','N','I', 1*' '/
      data FNAME(1,3),FNAME(2,3), FNAME(3,3), FNAME(4,3),FNAME(5,3),FNAM
     *E(6,3) / 'A','D','F','M','O','L'/
      data FNAME(1,4),FNAME(2,4), FNAME(3,4), FNAME(4,4),FNAME(5,4),FNAM
     *E(6,4) / 'A','D','I','M','O','L'/
      data FNAME(1,5),FNAME(2,5), FNAME(3,5), FNAME(4,5),FNAME(5,5),FNAM
     *E(6,5) / 'A','D','D','M','O','L'/
      data FNAME(1,6),FNAME(2,6), FNAME(3,6), FNAME(4,6),FNAME(5,6),FNAM
     *E(6,6) / 'A','L','O','G', 2*' '/
      data FNAME(1,7),FNAME(2,7), FNAME(3,7), FNAME(4,7),FNAME(5,7),FNAM
     *E(6,7) / 'E','X','P', 3*' '/
      data FNAME(1,8),FNAME(2,8), FNAME(3,8), FNAME(4,8),FNAME(5,8),FNAM
     *E(6,8) / 'A','R','E','C', 2*' '/
      data FNAME(1,9),FNAME(2,9), FNAME(3,9), FNAME(4,9),FNAME(5,9),FNAM
     *E(6,9) / 'A','L','K','E', 2*' '/
      data FNAME(1,10),FNAME(2,10), FNAME(3,10), FNAME(4,10),FNAME(5,10)
     *,FNAME(6,10) / 'A','L','K','E','I', 1*' '/
      data FNAME(1,11),FNAME(2,11), FNAME(3,11), FNAME(4,11),FNAME(5,11)
     *,FNAME(6,11) / 'A','M','O','L','D','M'/
      data FNAME(1,12),FNAME(2,12), FNAME(3,12), FNAME(4,12),FNAME(5,12)
     *,FNAME(6,12) / 'A','M','O','L','F','M'/
      data FNAME(1,13),FNAME(2,13), FNAME(3,13), FNAME(4,13),FNAME(5,13)
     *,FNAME(6,13) / 'A','M','O','L','R','M'/
      data FNAME(1,14),FNAME(2,14), FNAME(3,14), FNAME(4,14),FNAME(5,14)
     *,FNAME(6,14) / 'A','M','O','L','T','M'/
      data FNAME(1,15),FNAME(2,15), FNAME(3,15), FNAME(4,15),FNAME(5,15)
     *,FNAME(6,15) / 'A','N','I','H','D','M'/
      data FNAME(1,16),FNAME(2,16), FNAME(3,16), FNAME(4,16),FNAME(5,16)
     *,FNAME(6,16) / 'A','N','I','H','F','M'/
      data FNAME(1,17),FNAME(2,17), FNAME(3,17), FNAME(4,17),FNAME(5,17)
     *,FNAME(6,17) / 'A','N','I','H','R','M'/
      data FNAME(1,18),FNAME(2,18), FNAME(3,18), FNAME(4,18),FNAME(5,18)
     *,FNAME(6,18) / 'A','N','I','H','T','M'/
      data FNAME(1,19),FNAME(2,19), FNAME(3,19), FNAME(4,19),FNAME(5,19)
     *,FNAME(6,19) / 'A','P','R','I','M', 1*' '/
      data FNAME(1,20),FNAME(2,20), FNAME(3,20), FNAME(4,20),FNAME(5,20)
     *,FNAME(6,20) / 'B','H','A','B','D','M'/
      data FNAME(1,21),FNAME(2,21), FNAME(3,21), FNAME(4,21),FNAME(5,21)
     *,FNAME(6,21) / 'B','H','A','B','F','M'/
      data FNAME(1,22),FNAME(2,22), FNAME(3,22), FNAME(4,22),FNAME(5,22)
     *,FNAME(6,22) / 'B','H','A','B','R','M'/
      data FNAME(1,23),FNAME(2,23), FNAME(3,23), FNAME(4,23),FNAME(5,23)
     *,FNAME(6,23) / 'B','H','A','B','T','M'/
      data FNAME(1,24),FNAME(2,24), FNAME(3,24), FNAME(4,24),FNAME(5,24)
     *,FNAME(6,24) / 'B','R','E','M','D','R'/
      data FNAME(1,25),FNAME(2,25), FNAME(3,25), FNAME(4,25),FNAME(5,25)
     *,FNAME(6,25) / 'B','R','E','M','F','R'/
      data FNAME(1,26),FNAME(2,26), FNAME(3,26), FNAME(4,26),FNAME(5,26)
     *,FNAME(6,26) / 'B','R','E','M','D','Z'/
      data FNAME(1,27),FNAME(2,27), FNAME(3,27), FNAME(4,27),FNAME(5,27)
     *,FNAME(6,27) / 'B','R','M','S','D','Z'/
      data FNAME(1,28),FNAME(2,28), FNAME(3,28), FNAME(4,28),FNAME(5,28)
     *,FNAME(6,28) / 'B','R','E','M','F','Z'/
      data FNAME(1,29),FNAME(2,29), FNAME(3,29), FNAME(4,29),FNAME(5,29)
     *,FNAME(6,29) / 'B','R','M','S','F','Z'/
      data FNAME(1,30),FNAME(2,30), FNAME(3,30), FNAME(4,30),FNAME(5,30)
     *,FNAME(6,30) / 'B','R','E','M','R','R'/
      data FNAME(1,31),FNAME(2,31), FNAME(3,31), FNAME(4,31),FNAME(5,31)
     *,FNAME(6,31) / 'B','R','E','M','R','M'/
      data FNAME(1,32),FNAME(2,32), FNAME(3,32), FNAME(4,32),FNAME(5,32)
     *,FNAME(6,32) / 'B','R','E','M','R','Z'/
      data FNAME(1,33),FNAME(2,33), FNAME(3,33), FNAME(4,33),FNAME(5,33)
     *,FNAME(6,33) / 'B','R','E','M','T','M'/
      data FNAME(1,34),FNAME(2,34), FNAME(3,34), FNAME(4,34),FNAME(5,34)
     *,FNAME(6,34) / 'B','R','E','M','T','R'/
      data FNAME(1,35),FNAME(2,35), FNAME(3,35), FNAME(4,35),FNAME(5,35)
     *,FNAME(6,35) / 'B','R','M','S','R','M'/
      data FNAME(1,36),FNAME(2,36), FNAME(3,36), FNAME(4,36),FNAME(5,36)
     *,FNAME(6,36) / 'B','R','M','S','R','Z'/
      data FNAME(1,37),FNAME(2,37), FNAME(3,37), FNAME(4,37),FNAME(5,37)
     *,FNAME(6,37) / 'B','R','M','S','T','M'/
      data FNAME(1,38),FNAME(2,38), FNAME(3,38), FNAME(4,38),FNAME(5,38)
     *,FNAME(6,38) / 'C','O','H','E','T','M'/
      data FNAME(1,39),FNAME(2,39), FNAME(3,39), FNAME(4,39),FNAME(5,39)
     *,FNAME(6,39) / 'C','O','H','E','T','Z'/
      data FNAME(1,40),FNAME(2,40), FNAME(3,40), FNAME(4,40),FNAME(5,40)
     *,FNAME(6,40) / 'C','O','M','P','D','M'/
      data FNAME(1,41),FNAME(2,41), FNAME(3,41), FNAME(4,41),FNAME(5,41)
     *,FNAME(6,41) / 'C','O','M','P','F','M'/
      data FNAME(1,42),FNAME(2,42), FNAME(3,42), FNAME(4,42),FNAME(5,42)
     *,FNAME(6,42) / 'C','O','M','P','R','M'/
      data FNAME(1,43),FNAME(2,43), FNAME(3,43), FNAME(4,43),FNAME(5,43)
     *,FNAME(6,43) / 'C','O','M','P','T','M'/
      data FNAME(1,44),FNAME(2,44), FNAME(3,44), FNAME(4,44),FNAME(5,44)
     *,FNAME(6,44) / 'C','R','A','T','I','O'/
      data FNAME(1,45),FNAME(2,45), FNAME(3,45), FNAME(4,45),FNAME(5,45)
     *,FNAME(6,45) / 'E','B','I','N','D', 1*' '/
      data FNAME(1,46),FNAME(2,46), FNAME(3,46), FNAME(4,46),FNAME(5,46)
     *,FNAME(6,46) / 'E','B','R','1', 2*' '/
      data FNAME(1,47),FNAME(2,47), FNAME(3,47), FNAME(4,47),FNAME(5,47)
     *,FNAME(6,47) / 'E','D','E','D','X', 1*' '/
      data FNAME(1,48),FNAME(2,48), FNAME(3,48), FNAME(4,48),FNAME(5,48)
     *,FNAME(6,48) / 'E','I','I','T','M', 1*' '/
      data FNAME(1,49),FNAME(2,49), FNAME(3,49), FNAME(4,49),FNAME(5,49)
     *,FNAME(6,49) / 'E','S','I','G', 2*' '/
      data FNAME(1,50),FNAME(2,50), FNAME(3,50), FNAME(4,50),FNAME(5,50)
     *,FNAME(6,50) / 'F','C','O','U','L','C'/
      data FNAME(1,51),FNAME(2,51), FNAME(3,51), FNAME(4,51),FNAME(5,51)
     *,FNAME(6,51) / 'G','B','R','1', 2*' '/
      data FNAME(1,52),FNAME(2,52), FNAME(3,52), FNAME(4,52),FNAME(5,52)
     *,FNAME(6,52) / 'G','B','R','2', 2*' '/
      data FNAME(1,53),FNAME(2,53), FNAME(3,53), FNAME(4,53),FNAME(5,53)
     *,FNAME(6,53) / 'G','M','F','P', 2*' '/
      data FNAME(1,54),FNAME(2,54), FNAME(3,54), FNAME(4,54),FNAME(5,54)
     *,FNAME(6,54) / 'P','A','I','R','D','R'/
      data FNAME(1,55),FNAME(2,55), FNAME(3,55), FNAME(4,55),FNAME(5,55)
     *,FNAME(6,55) / 'P','A','I','R','F','R'/
      data FNAME(1,56),FNAME(2,56), FNAME(3,56), FNAME(4,56),FNAME(5,56)
     *,FNAME(6,56) / 'P','A','I','R','D','Z'/
      data FNAME(1,57),FNAME(2,57), FNAME(3,57), FNAME(4,57),FNAME(5,57)
     *,FNAME(6,57) / 'P','A','I','R','F','Z'/
      data FNAME(1,58),FNAME(2,58), FNAME(3,58), FNAME(4,58),FNAME(5,58)
     *,FNAME(6,58) / 'P','A','I','R','R','M'/
      data FNAME(1,59),FNAME(2,59), FNAME(3,59), FNAME(4,59),FNAME(5,59)
     *,FNAME(6,59) / 'P','A','I','R','R','R'/
      data FNAME(1,60),FNAME(2,60), FNAME(3,60), FNAME(4,60),FNAME(5,60)
     *,FNAME(6,60) / 'P','A','I','R','R','Z'/
      data FNAME(1,61),FNAME(2,61), FNAME(3,61), FNAME(4,61),FNAME(5,61)
     *,FNAME(6,61) / 'P','A','I','R','T','E'/
      data FNAME(1,62),FNAME(2,62), FNAME(3,62), FNAME(4,62),FNAME(5,62)
     *,FNAME(6,62) / 'P','A','I','R','T','M'/
      data FNAME(1,63),FNAME(2,63), FNAME(3,63), FNAME(4,63),FNAME(5,63)
     *,FNAME(6,63) / 'P','A','I','R','T','R'/
      data FNAME(1,64),FNAME(2,64), FNAME(3,64), FNAME(4,64),FNAME(5,64)
     *,FNAME(6,64) / 'P','A','I','R','T','U'/
      data FNAME(1,65),FNAME(2,65), FNAME(3,65), FNAME(4,65),FNAME(5,65)
     *,FNAME(6,65) / 'P','A','I','R','T','Z'/
      data FNAME(1,66),FNAME(2,66), FNAME(3,66), FNAME(4,66),FNAME(5,66)
     *,FNAME(6,66) / 'P','B','R','1', 2*' '/
      data FNAME(1,67),FNAME(2,67), FNAME(3,67), FNAME(4,67),FNAME(5,67)
     *,FNAME(6,67) / 'P','B','R','2', 2*' '/
      data FNAME(1,68),FNAME(2,68), FNAME(3,68), FNAME(4,68),FNAME(5,68)
     *,FNAME(6,68) / 'P','D','E','D','X', 1*' '/
      data FNAME(1,69),FNAME(2,69), FNAME(3,69), FNAME(4,69),FNAME(5,69)
     *,FNAME(6,69) / 'P','H','O','T','T','Z'/
      data FNAME(1,70),FNAME(2,70), FNAME(3,70), FNAME(4,70),FNAME(5,70)
     *,FNAME(6,70) / 'P','H','O','T','T','E'/
      data FNAME(1,71),FNAME(2,71), FNAME(3,71), FNAME(4,71),FNAME(5,71)
     *,FNAME(6,71) / 'P','S','I','G', 2*' '/
      data FNAME(1,72),FNAME(2,72), FNAME(3,72), FNAME(4,72),FNAME(5,72)
     *,FNAME(6,72) / 'S','P','I','O','N','E'/
      data FNAME(1,73),FNAME(2,73), FNAME(3,73), FNAME(4,73),FNAME(5,73)
     *,FNAME(6,73) / 'S','P','I','O','N','P'/
      data FNAME(1,74),FNAME(2,74), FNAME(3,74), FNAME(4,74),FNAME(5,74)
     *,FNAME(6,74) / 'S','P','T','O','T','E'/
      data FNAME(1,75),FNAME(2,75), FNAME(3,75), FNAME(4,75),FNAME(5,75)
     *,FNAME(6,75) / 'S','P','T','O','T','P'/
      data FNAME(1,76),FNAME(2,76), FNAME(3,76), FNAME(4,76),FNAME(5,76)
     *,FNAME(6,76) / 'T','M','X','B', 2*' '/
      data FNAME(1,77),FNAME(2,77), FNAME(3,77), FNAME(4,77),FNAME(5,77)
     *,FNAME(6,77) / 'T','M','X','S', 2*' '/
      data FNAME(1,78),FNAME(2,78), FNAME(3,78), FNAME(4,78),FNAME(5,78)
     *,FNAME(6,78) / 'T','M','X','D','E','2'/
      data FNAME(1,79),FNAME(2,79), FNAME(3,79), FNAME(4,79),FNAME(5,79)
     *,FNAME(6,79) / 'X','S','I','F', 2*' '/
      data IBOUND/0/
      data INCOH/0/
      data ICPROF/0/
      data GASP/0.0/
      data IRAYL/0/
      data IMPACT/0/
      data IUNRST/0/
      data efracH/5.d-2/
      data efracL/2.d-1/
      data nleg0/1000/
      data fudgeMS/1/
      data BMIN/4.5/,MSTEPS/16/,JRMAX/200/, FSTEP/1.,2.,3.,4.,6.,8.,10.,
     *15.,20.,30.,40.,60.,80.,100.,150.,200./
      data EPSTFL/0/,IEPST/1/,IAPRIM/1/,IAPRFL/0/
      data                                              ! common/DCSSTR/
     * NSDCS/0/                           ! Number of stored media DCS's
      end

      double precision function photte(K)
      implicit none
      integer i
      double precision phottz
      double precision K
      include 'pegscommons/mixdat.f'
      include 'pegscommons/mxdatc.f'
      photte=0.0
      do i=1,NE
        photte=PHOTTE+PZ(i)*PHOTTZ(Z(i),K)
      end do
      return
      end

      double precision function phottz(Z,K)
      implicit none
      integer IZ
      double precision PCON, Z, AINTP
      double precision K
      include 'pegscommons/phpair.f'
      include 'pegscommons/pmcons.f'
      include 'pegscommons/molvar.f'
      PCON=1.D-24*(AN*RHO/WM)*RLC
      IZ=Z
      phottz=PCON*AINTP(K,PHE(1,IZ),NPHE(IZ),PHD(1,IZ),1,.TRUE.,.TRUE.)
      return
      end

      subroutine plot(IFUN,XP,IV,EL,EH,NPT,IDF)
      implicit none
      integer IXTABF, NMAX, NUPL, itab, ixt1, ixt2, NU, IDFI, IDF, NA,
     & IFUN, ip, IV, ja, ia, i, IBIN, J, NPT
      double precision DFL, FI, EL, DFH, EH, BDF, YMAX, X, DY
      include 'pegscommons/funcs.f'
      include 'pegscommons/funcsc.f'
      include 'pegscommons/molvar.f'
      include 'pegscommons/dercon.f'
      include 'pegscommons/epstar.f'
      character*4 PBUF(101),ID(5),ORDNL(3,4),ICOM,IRPAR,ICOL,IX,IBL
      double precision XTAB(200),XTABA(18),YSAV(200),XP(4),XQ(5)
      data XTABA/1. ,1.25 ,1.5 ,1.75 ,2. ,2.5 ,3. ,3.5 ,4. ,4.5,4.9488 ,
     *5. ,5.5 ,6. ,7. ,8. ,9. ,10./
      data IXTABF/0/
      data ICOM/','/,IRPAR/')'/,ICOL/':'/,ORDNL/'1','S','T','2','N','D',
     *'3','R','D','4','T','H'/
      data PBUF/'I',100*' '/,NMAX/200/,NUPL/  26/,IX/'X'/,IBL/' '/
      if (IXTABF.eq.0) then
        itab= 0
        do ixt1=0,6
          do ixt2=1,17
            if (ixt2.ne.11) then
              itab=itab+ 1
              XTAB(itab)=XTABA(ixt2)*10.**(ixt1-3)
            else if (ixt2.eq.11 .and. ixt1.eq.4) then
              itab=itab+ 1
              XTAB(itab)=XTABA(ixt2)*10.**(ixt1-3)
            end if
          end do
        end do
        itab=itab+1
        XTAB(itab)=XTABA(18)*10.**(6-3)
        IXTABF=1
        NU=ITAB
      end if
      IDFI=IDF+1
      NA=NFARG(IFUN)
      DFL=FI(IDF,EL,0.d0,0.d0,0.d0)
      DFH=FI(IDF,EH,0.d0,0.d0,0.d0)
      BDF=(DFH-DFL)/FLOAT(NU-1)
      YMAX=0.0
      do ip=1,NU
        X=XTAB(ip)+RM
        XP(IV)=X
        YSAV(ip)=FI(IFUN,XP(1),XP(2),XP(3),XP(4))/(RLC*RHO)
        YMAX=DMAX1(YSAV(ip),YMAX)
      end do
      DY=YMAX/100.
      ja=0
      do ia=1,NA
        ja=ja+1
        if (ia.ne.IV) then
          XQ(ja)=XP(ia)
          ID(ja)=ICOM
        else
          XQ(ja)=EL
          ID(ja)=ICOL
          ja=ja+1
          XQ(ja)=EH
          ID(ja)=ICOM
        end if
      end do
      ID(ja)=IRPAR
      write(NUPL,100) (FNAME(i,IFUN),i=1,6),(XQ(i),ID(i),i=1,JA)
100   format (' Plot of function ',6A1,'(',5(1P,G15.6,1X,A1) )
      write(NUPL,110) (ORDNL(i,IV),i=1,3),NU,EL,EH,
     *(FNAME(i,IDF),i=1,6),(FNAME(i,IDFI),i=1,6),DY
110   format (' The ',3A1,' argument is chosen at ',I4, ' points from ',
     *1P,G15.6, ' to ', 1P,G15.6/' using distribution function ',6A1,' a
     *nd inverse ', 'distribution function ',6A1,'.  EACH X=',1P,G15.6/
     *'0    X(OR E)    Y1')
      write(NUPL,120) RLC,RHO
120   format (/' ***Changed version of pegs which has divided the values
     * by', ' RLC*RHO to get to MeV/g/cm**2'/'  RLC=',E12.4,'  RHO=',E12
     *.4)
      do ip=1,NU
        X=XTAB(ip)
        if (DY.ne.0.0) then
          IBIN=YSAV(ip)/DY+1.0
        else
          IBIN=1
        end if
        if (IBIN.ge.2) PBUF(IBIN)=IX
        write(NUPL,130) IP,X,YSAV(ip),(PBUF(j),j=1,IBIN)
130     format (1X,I3,1P,2G13.6,1X,101A1)
        if (IBIN.ge.2) PBUF(IBIN)=IBL
      end do
      write(21,*) NU
      write(21,140) (XTAB(ip),ip=1,NU)
      write(21,*) NU
      write(21,140) (YSAV(ip),ip=1,NU)
140   FORMAT(5(1P,E15.7))
      return
      end

      subroutine PLOT1(IFUN,XP,IV,EL,EH,NPT,IDF)
      implicit none
      integer NMAX, NUPL, NU, NPT, IDFI, IDF, NA, IFUN, ip, i, IV, ja,
     & ia, IBIN, J
      double precision DFL, FI, EL, DFH, EH, BDF, YMAX, DF, X, DY
      include 'pegscommons/funcs.f'
      include 'pegscommons/funcsc.f'
      character*4 PBUF(101),ID(5),ORDNL(3,4),ICOM,IRPAR,ICOL,IX,IBL
      double precision YSAV(200),XP(4),XQ(5)
      data ICOM/','/,IRPAR/')'/,ICOL/':'/,ORDNL/'1','S','T','2','N','D',
     *'3','R','D','4','T','H'/
      data PBUF/'I',100*' '/,NMAX/200/,NUPL/6/,IX/'X'/,IBL/' '/
      NU=MIN0(NPT,NMAX)
      IDFI=IDF+1
      NA=NFARG(IFUN)
      DFL=FI(IDF,EL,0.d0,0.d0,0.d0)
      DFH=FI(IDF,EH,0.d0,0.d0,0.d0)
      BDF=(DFH-DFL)/FLOAT(NU-1)
      YMAX=0.0
      do ip=1,NU
        i=ip-1
        DF=DFL+BDF*FLOAT(i)
        X=FI(IDFI,DF,0.d0,0.d0,0.d0)
        XP(IV)=X
        YSAV(ip)=FI(IFUN,XP(1),XP(2),XP(3),XP(4))
        YMAX=DMAX1(YSAV(ip),YMAX)
      end do
      DY=YMAX/100.
      ja=0
      do ia=1,NA
        ja=ja+1
        if (ia.ne.IV) then
          XQ(ja)=XP(ia)
          ID(ja)=ICOM
        else
          XQ(ja)=EL
          ID(ja)=ICOL
          ja=ja+1
          XQ(ja)=EH
          ID(ja)=ICOM
        end if
      end do
      ID(ja)=IRPAR
      write(NUPL,100) (FNAME(i,IFUN),i=1,6),(XQ(i),ID(i),i=1,ja)
100   format (' Plot of function ',6A1,'(',5(1P,G15.6,1X,A1) )
      write(NUPL,110) (ORDNL(i,IV),i=1,3),NU,EL,EH, (FNAME(i,IDF),
     *i=1,6),(FNAME(i,IDFI),i=1,6),DY
110   format (' The ',3A1,' argument is chosen at ',I4, ' points from ',
     *1P,G15.6, ' to ', 1P,G15.6/' using distribution function ',6A1,' a
     *nd inverse ', 'distribution function ',6A1,'.  Each X=',1P,G15.6/
     *'0    X(or E)    Y1')
      do ip=1,NU
        i=ip-1
        X=FI(IDFI,DFL+BDF*FLOAT(i),0.d0,0.d0,0.d0)
        if (DY.ne.0.0) then
          IBIN=YSAV(ip)/DY+1.0
        else
          IBIN=1
        end if
        if (IBIN.ge.2) PBUF(IBIN)=IX
        write(NUPL,120) ip,X,YSAV(ip),(PBUF(j),j=1,IBIN)
120     format (1X,I3,1P,2G13.6,1X,101A1)
        if (IBIN.ge.2) PBUF(IBIN)=IBL
      end do
      return
      end

      subroutine PMDCON
      implicit none
      double precision FSCI
      include 'pegscommons/pmcons.f'
      include 'pegscommons/dercon.f'
      PI=3.1415926535897932D+0
      C=2.99792458D+10
      RME=9.10938188D-28
      HBAR=1.054571596E-27
      ECGS=4.8032068D-10
      EMKS=1.602176462D-19
      AN=6.02214199D+23
      RADDEG=180./PI
      FSC = ECGS**2/(HBAR*C)
      FSCI=1./FSC
      ERGMEV = (1.D+6)*(EMKS*1.D+7)
      R0 = (ECGS**2)/(RME*C**2)
      RM = RME*C**2/ERGMEV
      RMT2 = RM*2.0
      RMSQ = RM*RM
      A22P9 = RADDEG*DSQRT(4.*PI*AN)*ECGS**2/ERGMEV
      A6680 = 4.0*PI*AN*(HBAR/(RME*C))**2*(0.885**2/(1.167*1.13))
      return
      end

      double precision function PSIG(E)
      implicit none
      double precision BREMTM, E, BHABTM, ANIHTM
      PSIG=BREMTM(E)+BHABTM(E)+ANIHTM(E)
      return
      end

      subroutine PWLF1(NI,NIMX,XL,XU,XR,EP,ZTHR,ZEP,NIP,XFUN,XFI, AX,BX,
     *NALM,NFUN,AF,BF,VFUNS)
      implicit none
      integer NALM, NFUN, NL, NU, IPRN, NJ, NIMX, NIP, NI, NK
      double precision XL, XU, XR, EP, ZTHR, ZEP, REM, AX, BX, AF, BF
      external XFUN,XFI,VFUNS
      dimension AF(NALM,NFUN),BF(NALM,NFUN),ZTHR(NFUN),ZEP(NFUN)
      logical QFIT
      NL=0
      NU=1
      IPRN=0
100   continue
        NJ=MIN0(NU,NIMX)
        if (QFIT(NJ,XL,XU,XR,EP,ZTHR,ZEP,REM,NIP,XFUN,XFI, AX,BX,NALM,NF
     *  UN,AF,BF,VFUNS,0)) go to 120
        if (NU.ge.NIMX) then
          write(  26,110) NIMX,EP
110       format(' Number of allocated intervals(=',I5,') was insufficie
     *nt' ,/ ,' to get maximum relative error less than ',1P,G14.6)
          NI=NJ
          return
        end if
        NL=NU
        NU=NU*2
      go to 100
120   continue
      NU=NJ
130   if (NU.le.NL+1) go to 140
        NJ=(NL+NU)/2
        NK=NJ
        if (QFIT(NJ,XL,XU,XR,EP,ZTHR,ZEP,REM,NIP,XFUN,XFI, AX,BX,NALM,
     *  NFUN,AF,BF,VFUNS,0)) then
          NU=NJ
        else
          NL=NK
        end if
      go to 130
140   continue
      NI=NU
      if (NI.eq.NJ) return
      if (.not.QFIT(NI,XL,XU,XR,EP,ZTHR,ZEP,REM,NIP,XFUN,XFI, AX,BX,NALM
     *,NFUN,AF,BF,VFUNS,0)) write(  26,150) NI
150   format(' Catastrophe---does not fit when it should,NI=',I5)
      return
      end

      double precision function QD(F,A,B,MSG)
      implicit none
      integer IER
      double precision A, B
      external F
      double precision DCADRE,ADUM,BDUM,ERRDUM
      character*6 MSG
      ADUM=A
      BDUM=B
      QD=DCADRE(F,ADUM,BDUM,1.D-16,1.D-5,ERRDUM,IER)
      if (IER.gt.66) then
        write(10,100) IER,MSG,A,B,QD,ERRDUM
100     formaT (' DCADRE code=',I4,' for integral ',A6,' from ',1P,G14.6
     *  ,' to ',G14.6, ',QD=',G14.6,'+-',G14.6)
      end if
      return
      end

      logical function QFIT(NJ,XL,XH,XR,EP,ZTHR,ZEP,REM,NJP,XFUN,XFI, AX
     *,BX,NALM,NFUN,AF,BF,VFUNS,IPRN)
      implicit none
      integer NALM, NFUN, NKP, NI, NJ, NIP, NJP, isub, IPRN, ifun,
     & JSUB, ip
      double precision XH, XL, XS, XR, XFL, XFUN, XFH, XFS, XM, DX, W,
     & XLL, AX, BX, REM, SXFL, XSXF, XFI, FSXL, SXFH, FSXH, DSXF, AF, BF
     & , WIP, SXFIP, XIP, FIP, FFIP, AFIP, AER, RE, ZTHR, ZEP, EP, EPS1
      external XFUN,XFI,VFUNS
      dimension FSXL(79),FSXH(79),FIP(79),FFIP(79),AFIP(79)
      dimension RE(79),AER(79)
      dimension AF(NALM,NFUN),BF(NALM,NFUN),ZTHR(NFUN),ZEP(NFUN)
      data EPS1/1.d-15/
      data NKP/3/
      if (XH.le.XL) then
        write(  26,100) XL,XH
100     format(' QFIT error:XL should be < XH. XL,XH=',2G14.6)
        QFIT=.false.
        return
      end if
      XS=DMAX1(XL,DMIN1(XH,XR))
      NI=NJ-2
      if (((XS.eq.XL.or.XS.eq.XH).and.NI.ge.1).or.NI.ge.2) then
        XFL=XFUN(XL)
      else
        QFIT=.false.
        return
      end if
      XFH=XFUN(XH)
      XFS=XFUN(XS)
      XM=DMAX1(XFH-XFS,XFS-XFL)
      DX=XFH-XFL
! patch to eliminate g77 optimization level dependence -- YN
!      W=XM/DMAX1(1.d0,AINT(NI*XM/DX))
      IF(DABS(XM-DX).LE.EPS1*DX) THEN
        W=XM/DMAX1(1.d0,AINT(NI*1.d0))
      else
        W=XM/DMAX1(1.d0,AINT(NI*XM/DX))
      end if

      NI=NI-AINT(NI-DX/W)
      NIP=MAX0(NKP,(NJP+NI-1)/NI)
      NIP=(NIP/2)*2+1
      if (XFH-XFS.le.XFS-XFL) then
        XLL=XFL
      else
        XLL=XFH-NI*W
      end if
      AX=1./W
      BX=2.-XLL*AX
      REM=0.0
      QFIT=.true.
      SXFL=DMAX1(XLL,XFL)
      ISUB=0
      XSXF=XFI(SXFL)
      call VFUNS(XSXF,FSXL)
      if (IPRN.ne.0) WRITE(  26,110) isub,SXFL,XSXF, (FSXL(IFUN),IFUN=1
     *,NFUN)
110   format(' QFIT:ISUB,SXF,XSXF,FSX()=',I4,1P,9G11.4/(1X,12G11.4))
      do isub=1,NI
        JSUB=isub+1
        SXFH=DMIN1(XLL+W*isub,XH)
        XSXF=XFI(SXFH)
        call VFUNS(XSXF,FSXH)
        if (IPRN.ne.0) write(  26,110) isub,SXFH,XSXF, (FSXH(ifun),
     *   ifun=1,NFUN)
        DSXF=SXFH-SXFL
        do ifun=1,NFUN
          AF(JSUB,ifun)=(FSXH(ifun)-FSXL(ifun))/DSXF
          BF(JSUB,ifun)=(FSXL(ifun)*SXFH-FSXH(ifun)*SXFL)/DSXF
        end do
        WIP=DSXF/(NIP+1)
        do ip=1,NIP
          SXFIP=SXFL+ip*WIP
          XIP=XFI(SXFIP)
          call VFUNS(XIP,FIP)
          do ifun=1,NFUN
            FFIP(ifun)=AF(JSUB,ifun)*SXFIP+BF(JSUB,ifun)
            AFIP(ifun)=dabs(FIP(ifun))
            AER(ifun)=dabs(FFIP(ifun)-FIP(ifun))
            RE(ifun)=0.0
            if (FIP(ifun).ne.0.0) then
              RE(ifun)=AER(ifun)/AFIP(ifun)
            end if
            if (AFIP(ifun).ge.ZTHR(ifun)) then
              REM=dmax1(REM,RE(ifun))
            else if (AER(ifun).gt.ZEP(ifun)) then
              QFIT=.false.
            end if
          end do
          if (IPRN.ne.0) then
            write(  26,120) ISUB,ip,SXFIP,XIP,REM,QFIT,(FIP(ifun),
     *      FFIP(ifun), RE(ifun),AER(ifun),ifun=1,NFUN)
120         format(1X,2I4,1P,2G12.5,6P,F12.0,L2,1P,2G11.4,6P,F11.0,1P,G1
     *      1.4/ (1X,3(1P,2G11.4,6P,F11.0,1P,G11.4)))
          end if
        end do
        SXFL=SXFH
        do ifun=1,NFUN
          FSXL(ifun)=FSXH(ifun)
        end do
      end do
      do ifun=1,NFUN
        AF(1,ifun)=AF(2,ifun)
        BF(1,ifun)=BF(2,ifun)
        AF(NI+2,ifun)=AF(NI+1,ifun)
        BF(NI+2,ifun)=BF(NI+1,ifun)
      end do
      QFIT=QFIT.AND.REM.LE.EP
      NJ=NI+2
      return
      end

      subroutine RDSCPR
      implicit none
      integer MXRAW, MXSHEL, NSHELL, i
      double precision ELECNI, CAPIN, SCPROF, QCAP, PZT
      include 'pegscommons/mixdat.f'
      include 'pegscommons/mxdatc.f'
      character FILENM*60,NOZ*3
      dimension ELECNI(200),NSHELL(200),CAPIN(200),SCPROF(31,200), QCAP(
     *31)
      NAMELIST/SCPRDT/MXRAW,MXSHEL,ELECNI,NSHELL,CAPIN,SCPROF,QCAP
      do i=1,NE
        WRITE(NOZ,'(I3.3)') NINT(Z(i))
        FILENM='data/shellwise_Compton_profile/z'//NOZ//'.dat'
        write(  26,100) FILENM
100     format(' Reading ',A50)
        open(UNIT=30,FILE=FILENM,STATUS='old')
        PZT=PZ(i)
        read(30,SCPRDT)
        call ADSCPR(MXRAW,MXSHEL,ELECNI,NSHELL,CAPIN,SCPROF,QCAP,PZT)
        close(30)
      end do
      call WTSCPR
      return
      end

      subroutine RFUNS(E,V)
      implicit none
      double precision AINTP, E
      include 'pegscommons/cohcom.f'
      double PRECISION V(1)
      V(1)=AINTP(E,AFFI(1),97,XVAL(1),1,.TRUE.,.TRUE.)
      return
      end

      subroutine RFUNS2(E,V)
      implicit none
      double precision AINTP, E
      include 'pegscommons/cohcom.f'
      double precision V(1)
      V(1)=AINTP(E,XVAL(1),97,AFAC2(1),1,.TRUE.,.TRUE.)
      return
      end

      subroutine SFUNS(E,V)
      implicit none
      double precision AINTP, E
      include 'pegscommons/bcom.f'
      include 'pegscommons/sfcom.f'
      double precision V(1)
      V(1)=AINTP(E,XSVAL(1),41,SCATZ(1),1,.TRUE.,.TRUE.)
      return
      end

      subroutine SPINIT
      implicit none
!     Use Revised Sternheimer Density Effects Coeeficients
!     Atomic Data Nuclear Data Tables 30, 261(1984) by 
!     R. M. Sternheimer et al.
      integer im, j, IZ, ie, i, ICHECK, IESPEL, IPEGEL
      double precision VPLASM, ALIADG, DEXP, EDENL, ALGASP, 
     & EPSTRH, TLRNCE, EPSTWT, V4110, V4130, V4150, V4170, V4190, 
     & V4210, V4230, V4240, V4250, V4270
      include 'pegscommons/pmcons.f'
      include 'pegscommons/spcomm.f'
      include 'pegscommons/spcomc.f'
      include 'pegscommons/dercon.f'
      include 'pegscommons/molvar.f'
      include 'pegscommons/mixdat.f'
      include 'pegscommons/mxdatc.f'
      include 'pegscommons/elemtb.f'
      include 'pegscommons/elmtbc.f'
      include 'pegscommons/lspion.f'
      include 'pegscommons/epstar.f'
      include 'pegscommons/thres2.f'
      double precision IMEV
      TOLN10=2.0*DLOG(10.d0)
      IM=-100
      if (EPSTFL .lt. 0 .or. EPSTFL .gt. 1) then
        EPSTFL = 0
      end if
      write(26,9) EPSTFL
9     format(' EPSTFL=',I15)
      if (EPSTFL.eq.0) then
        if (ISSB.ne.0) then
          if ( AFACT.eq.0.0 .or. CBAR.le.0.0 .or. SK.eq.0.0 .or. X0.eq.
     *    0.0 .or. X1.eq.0.0 .or. IEV.eq.0.0 ) then
            write(  26,100)
100         format(//' *****User error -not all density effect paramters
     * input', '   code stopped in SPINIT****'//)
            close(26)
            stop
          end if
          IMEV=IEV*1.D-6
          VPLASM=DSQRT(EDEN*R0*C**2/PI)
          IM=-1
        else
          if (ISSB.eq.0.and.(AFACT.ne.0.0.or.CBAR.ne.0.0.or.SK.ne.0.0.
     *    or.X0.ne.0.0.or.X1.ne.0.0.or.IEV.ne.0.0)) then
            write(  26,110)
110         format(//,' Stopped in SPINIT: incorrect user-override of SS
     *B-DATA')
            close(26)
            stop
          end if
          DO 120 im=1,NMED
            do j=1,LMED
              if (IDSTRN(j).ne.MEDTBL(j,im)) go to 120
            end do
!           Calculation follows if a match is found
            AFACT=STDATA(1,im)
            SK=STDATA(2,im)
            X0=STDATA(3,im)
            X1=STDATA(4,im)
            IEV=STDATA(5,im)
            CBAR=STDATA(6,im)
!           Define DELATA0
            DELTA0=STDATA(7,im)
            IMEV=IEV*1.0D-6
            VPLASM=DSQRT(EDEN*R0*C**2/PI)
            go to 150
120       continue
!      Sternheimer-Peierls (S-P) general formula section
          DELTA0=0.0
          IM=0
          if (NE.eq.1) then
            IZ=Z(1)
            if (IZ.eq.1.or.IZ.eq.7.or.IZ.eq.8) then
              write(  26,130)
130           format(' Stopped in subroutine SPINIT because this',/, ' e
     *lement (H, N, OR O) can only exist as a diatomic molecule.',/, ' R
     *EMEDY:  use comp option for H2, N2, OR O2 with NE=2,PZ=1,1'/, '
     *       and, in the case of a gas, define Sternheimer ID',/, '
     *     (I.E., IDSTRN) like H2-GAS')
              close(26)
              stop
            end if
            IEV=ITBL(IZ)
          else
            ALIADG=0.0
            do ie=1,NE
              IZ=Z(ie)
              if (IZ.eq.1) then
                IEV=19.2
              else if (IZ.eq.6) then
                if (GASP.eq.0.0) then
                  IEV=81.0
                else
                  IEV=70.0
                end if
              else if (IZ.eq.7) then
                IEV=82.0
              else if (IZ.eq.8) then
                if (GASP.eq.0.0) then
                  IEV=106.0
                else
                  IEV=97.0
                end if
              else if (IZ.eq.9) then
                IEV=112.0
              else if (IZ.eq.17) then
                IEV=180.0
              else
                IEV=1.13*ITBL(IZ)
              end if
              ALIADG=ALIADG + PZ(IE)*Z(IE)*DLOG(IEV)
            end do
            ALIADG=ALIADG/ZC
            IEV=DEXP(ALIADG)
          end if
          IMEV=IEV*1.0D-6
          if (GASP.eq.0.0) then
            EDENL=EDEN
          else
            EDENL=EDEN/GASP
          end if
          VPLASM = DSQRT(EDENL*R0*C**2/PI)
          CBAR=1. + 2.*DLOG(IMEV/(HBAR*2*PI*VPLASM/ERGMEV))
          if (NE.eq.1.and.IDINT(Z(1)).eq.2.and.GASP.ne.0.0) then
            X0=2.191
            X1=3.0
            SK=3.297
          else if
     *      (NE.eq.2.and.IDINT(Z(1)).eq.1.and.IDINT(Z(2)).eq.1) then
            if (GASP.eq.0.0) then
              X0=0.425
              X1=2.0
              SK=5.949
            else
              X0=1.837
              X1=3.0
              SK=4.754
            end if
          else
            SK=3.0
            if (GASP.eq.0.0) then
              if (IEV.lt.100.0) then
                if (CBAR.lt.3.681) then
                  X0=0.2
                  X1=2.0
                else
                  X0=0.326*CBAR - 1.0
                  X1=2.0
                end if
              else
                if (CBAR.lt.5.215) then
                  X0=0.2
                  X1=3.0
                else
                  X0=0.326*CBAR - 1.5
                  X1=3.0
                end if
              end if
              if (X0.ge.X1) then
                write(  26,140) X0,X1,CBAR
140             format(' Stopped in SPINIT due to X0.ge.X1 , X0,X1,CBAR=
     *',3G15.5,/ ,' If this is gas, you must define GASP(ATM)')
                close(26)
                stop
              end if
            else
              if (CBAR.lt.10.0) then
                X0=1.6
                X1=4.0
              else if (CBAR.lt.10.5) then
                X0=1.7
                X1=4.0
              else if (CBAR.lt.11.0) then
                X0=1.8
                X1=4.0
              else if (CBAR.lt.11.5) then
                X0=1.9
                X1=4.0
              else if (CBAR.lt.12.25) then
                X0=2.0
                X1=4.0
              else if (CBAR.lt.13.804) then
                X0=2.0
                X1=5.0
              else
                X0=0.326*CBAR - 2.5
                X1=5.0
              end if
            end if
          end if
        end if
150     if (GASP.ne.0.0) then
          ALGASP=DLOG(GASP)
          CBAR=CBAR - ALGASP
          X0=X0 - ALGASP/TOLN10
          X1=X1 - ALGASP/TOLN10
        end if
        if (IM.eq.0) then
          AFACT=(CBAR - TOLN10*X0)/(X1 - X0)**SK
        end if
      else
        read(20,160,ERR=9991) EPSTTL
160     format(80A1)
        read(20,*,ERR=9991) NEPST,IEV,EPSTRH,NELEPS,(ZEPST(i),
     *          WEPST(i),i=1,NELEPS)
        read(20,*,ERR=9991) (EPSTEN(i),EPSTD(i),i=1,NEPST)
        go to 9993
9991    write(26,9992)
9992    format(/,/,' *****END-OF-FILE on epstar.dat ')
        close(26)
        stop
9993    if (NEPST.gt.150) then
          write(  26,170) NEPST
170       format(//' *****NEPST=',I4,' is greater than the 150 allowed')
          close(26)
          stop
        end if
        do i=1,NEPST
          EPSTEN(i) = EPSTEN(i) + RM
        end do
        IMEV = IEV*1.D-06
        if ( AE .lt. EPSTEN(1)) then
          write(  26,180) EPSTEN(1),AE
180       format(//' ****Lowest energy input for density effect is',1P,E
     *    10.3/ T20,'which is higher than the value of AE=',1P,E10.3,' M
     *eV'/ ' ***It has been set to AE***'//)
          EPSTEN(1) = AE
        end if
        if ( UE .gt. EPSTEN(NEPST)) then
          write(  26,190) EPSTEN(NEPST),UE
190       format(//' ****Highest energy input for density effect is',1P,
     *    E10.3/ T20,'which is lower than the value of UE=',1P,E10.3,' M
     *eV'/ ' ***It has been set to UE***'//)
          EPSTEN(NEPST) = UE
        end if
        ICHECK=0
        TLRNCE=0.01
        if (NELEPS.ne.NE) ICHECK=1
        if ((ICHECK.eq.0) .and. ( (EPSTRH.lt.((1.0-TLRNCE)*RHO)) .or. (E
     *  PSTRH.GT.((1.0+TLRNCE)*RHO)) )) ICHECK=1
        EPSTWT = 0.0
        do i=1,NE
          EPSTWT = EPSTWT + RHOZ(i)
        end do
        if (EPSTWT.eq.0.0) then
          write(  26,200)
200       format(//' *****In SPINIT***something wrong, molecular weight
     *of', 'molecule is zero (I.E. sum of RHOZ)***'//)
        end if
        if (ICHECK.eq.0) then
          IESPEL=0
          ICHECK=1
210       continue
            IESPEL=IESPEL+1
            IPEGEL=0
220         continue
              IPEGEL=IPEGEL+1
              if (DINT(Z(IPEGEL)).eq.ZEPST(IESPEL)) then
                ICHECK=0
                go to 230
              end if
              if (IPEGEL.ge.NE) go to 230
            go to 220
230         continue
            if ((ICHECK.eq.0)  .and. ( (WEPST(IESPEL).lt.((1.0-TLRNCE)*R
     *      HOZ(IPEGEL)/EPSTWT)) .or. (WEPST(IESPEL).gt.((1.0+TLRNCE)*RH
     *      OZ(IPEGEL)/EPSTWT)) )) ICHECK=1
            if (IESPEL.GE.NELEPS) go to 240
          go to 210
240       continue
        end if
        if (ICHECK.eq.1) then
          write(  26,250)
250       format(////' *** Composition in input density file does not ma
     *tch ', ' that being used by pegs'//' ***** Quitting early***'////)
          close(26)
          stop
        end if
      end if
      SPC1=2.*PI*R0**2*RM*EDEN*RLC
      SPC2=DLOG((IMEV/RM)**2/2.0)
      write(  26,260)
260   format(//' Parameters computed in SPINIT.'//1X,64('-'))
      if (IM.eq.0) then
        write(  26,270)
270     format(' Sternheimer-Peierls general formula used for the densit
     *y effect,')
      else if (IM.gt.0) then
        write(  26,280)
280     format(' Sternheimer-Seltzer-Berger table used for density effec
     *t')
      else if (IM .eq. -1) then
        write(  26,290)
290     format(' Sternheimer-Seltzer-Berger density effect data supplied
     * by user')
      else
        write(  26,300) EPSTTL
300     format(' Density effect read in directly:'/T10,80A1)
      end if
      write(  26,310)
310   format(1X,64('-')/)
      write(  26,320) IEV
320   format(/' Adjusted mean ionization = ',F8.2,' eV'/1X,38('-')//)
      if (EPSTFL .eq. 0) then
        V4110=IEV
        write(  26,330) V4110
330     format(' IEV=',1P,G15.7)
        V4130=VPLASM
        write(  26,340) V4130
340     format(' VPLASM=',1P,G15.7)
        V4150=CBAR
        write(  26,350) V4150
350     format(' CBAR=',1P,G15.7)
        V4170=X0
        write(  26,360) V4170
360     format(' X0=',1P,G15.7)
        V4190=X1
        write(  26,370) V4190
370     format(' X1=',1P,G15.7)
        V4210=SK
        write(  26,380) V4210
380     format(' SK=',1P,G15.7)
        V4230=AFACT
        write(  26,390) V4230
390     format(' AFACT=',1P,G15.7)
        V4240=DELTA0
        WRITE(  26,400)V4240
400     FORMAT(' DELTA0=',1P,G15.7)
      end if
      V4250=SPC1
      write(  26,410) V4250
410   format(' SPC1=',1P,G15.7)
      V4270=SPC2
      write(  26,420) V4270
420   format(' SPC2=',1P,G15.7)
      return
      end

      double precision function SPIONB(E0,EE,POSITR)
!     Use Revised Sternheimer Density Effects Coeeficients
!     Atomic Data Nuclear Data Tables 30, 261(1984) by 
!     R. M. Sternheimer et al.
!     Use DELTA0 
      implicit none
      integer i
      double precision G, E0, EEM, EE, T, ETA2, BETA2, ALETA2, DLOG,
     & X, D, FTERM, TP2, D2, D3, D4, DELTA
      logical POSITR
      include 'pegscommons/dercon.f'
      include 'pegscommons/lspion.f'
      include 'pegscommons/epstar.f'
      G=E0/RM
      EEM=EE/RM-1.
      T=G-1
      ETA2=T*(G+1.)
      BETA2=ETA2/G**2
      ALETA2=DLOG(ETA2)
      X=0.21715*ALETA2
      if (.NOT.POSITR) then
        D=DMIN1(EEM,0.5*T)
        FTERM=-1.-BETA2+DLOG((T-D)*D)+T/(T-D) +(D*D/2.+(2.*T+1.)*DLOG(1.
     *  -D/T))/(G*G)
      else
        D=DMIN1(EEM,T)
        TP2=T+2.
        D2=D*D
        D3=D*D2
        D4=D*D3
        FTERM=DLOG(T*D)-(BETA2/T)*( T + 2.*D - (3.*D2/2.)/TP2 -(D-D3/3.)
     *  /(TP2*TP2)-(D2/2.-T*D3/3.+D4/4.)/TP2**3)
      end if
      if (EPSTFL .eq. 0) then
        if (X.le.X0) then
          DELTA=DELTA0*10**(2.0*(X-X0))
        else if (X.lt.X1) then
          DELTA=TOLN10*X - CBAR + AFACT*(X1 - X)**SK
        else
          DELTA=TOLN10*X - CBAR
        end if
      else
        if (E0 .ge. EPSTEN(IEPST)) then
          if (E0 .eq. EPSTEN(IEPST)) then
            go to 100
          end if
          do i=IEPST,NEPST-1
            if (E0.lt.EPSTEN(i+1)) then
              IEPST = I
              go to 100
            end if
          end do
          IEPST = NEPST
          go to 100
        else
          do i=IEPST,2,-1
            if (E0 .ge. EPSTEN(i-1)) then
              IEPST = I-1
              go to 100
            end if
          end do
          IEPST = 1
        END IF
100    if (IEPST .lt. NEPST) then
          DELTA = EPSTD(IEPST) + (E0 - EPSTEN(IEPST))/ (EPSTEN(IEPST+1)
     *    - EPSTEN(IEPST)) * (EPSTD(IEPST+1) - EPSTD(IEPST))
        else
          DELTA = EPSTD(NEPST)
        end if
      end if
      SPIONB=(SPC1/BETA2)*(DLOG(T + 2.) - SPC2 + FTERM - DELTA)
      return
      end

      double precision function spione(E0,EE)
      implicit none
      double precision spionb, E0, EE
      spione=spionb(E0,EE,.FALSE.)
      return
      end

      double precision function spionp(E0,EE)
      implicit none
      double precision spionb, E0, EE
      spionp=spionb(E0,EE,.TRUE.)
      return
      end

      double precision function sptote(E0,EE,EG)
      implicit none
      double precision spione, E0, EE, brmstm, EG
      include 'pegscommons/thres2.f'
      if (IUNRST.eq.0) then
        sptote=spione(E0,EE)+brmstm(E0,EG)
      else if (IUNRST.eq.1) then
        sptote=spione(E0,E0)
      else if (IUNRST.eq.2) then
        sptote=spione(E0,E0)+brmstm(E0,E0)
      else if (IUNRST.eq.3) then
        sptote=spione(E0,E0)+brmstm(E0,EG)
      else if (IUNRST.eq.4) then
        sptote=spione(E0,EE)+brmstm(E0,E0)
      else if (IUNRST.eq.5)  then
        sptote=brmstm(E0,E0)
      else if (IUNRST.eq.6) then
        sptote=brmstm(E0,EG)
      else if (IUNRST.eq.7) then
        sptote=spione(E0,EE)
      end if
      return
      end

      double precision function sptotp(E0,EE,EG)
      implicit none
      double precision spionp, E0, EE, brmstm, EG
      include 'pegscommons/thres2.f'
      if (IUNRST.eq.0) then
        sptotp=spionp(E0,EE)+brmstm(E0,EG)
      else if (IUNRST.eq.1) then
        sptotp=spionp(E0,E0)
      else if (IUNRST.eq.2) then
        sptotp=spionp(E0,E0)+brmstm(E0,E0)
      else if (IUNRST.eq.3) then
        sptotp=spionp(E0,E0)+brmstm(E0,EG)
      else if (IUNRST.eq.4) then
        sptotp=spionp(E0,EE)+brmstm(E0,E0)
      else if (IUNRST.eq.5) then
        sptotp=brmstm(E0,E0)
      else if (IUNRST.eq.6) then
        sptotp=brmstm(E0,EG)
      else if (IUNRST.eq.7) then
        sptotp=spionp(E0,EE)
      end if
      return
      end

      double precision function tmxb(E)
      implicit none
      double precision ESQ, E, BETA2, PX2
      include 'pegscommons/dercon.f'
      include 'pegscommons/molvar.f'
      ESQ=E**2
      BETA2=1.0-RMSQ/ESQ
      PX2=ESQ*BETA2/XCC**2
      tmxb=PX2*BETA2/DLOG(BLCC*PX2)
      return
      end

      double precision function tmxde2(E)
      implicit none
      double precision ESQ, E, BETASQ, TMXB
      include 'pegscommons/dercon.f'
      ESQ=E**2
      BETASQ=1.0-RMSQ/ESQ
      tmxde2=TMXB(E)/(ESQ*BETASQ**2)
      return
      end

      double precision function tmxs(E)
      implicit none
      double precision SAFETY, TABSMX, TMXB, E
      data SAFETY/0.8/,TABSMX/10.0/
      tmxs=DMIN1(TMXB(E)*SAFETY,TABSMX)
      return
      end

      subroutine wtscpr
      implicit none
      integer i, j
      include 'pegscommons/cpcom.f'
      open(UNIT=31,FILE='pgs5job.ssl',STATUS='unknown')
      write(31,'(1H ,A)') '&SCPRDT'
      write(31,'(1H ,A)') 'QCAP='
      write(31,'((1H ,7(F9.2,A)))') (QCAP(I),',',I=1,31)
      write(31,'(1H ,A,I5)') 'MXRAW=',MXRAW
      Write(31,'(1H ,A)') 'ELECNI='
      write(31,'((1H ,7(1PE9.3,A)))') (ELECNI(I),',',I=1,MXRAW)
      write(31,'(1H ,A,I5)') 'MXSHEL=',MXSHEL
      write(31,'(1H ,A)') 'NSHELL='
      write(31,'((1H ,14(I4,A)))') (NSHELL(I),',',I=1,MXRAW)
      write(31,'(1H ,A)') 'CAPIN='
      write(31,'((1H ,5(1PE11.5,A)))') (CAPIN(I),',',I=1,MXRAW)
      write(31,'(1H ,A)') 'SCPROF='
      do i=1,MXRAW+1
        write(31,'((1H ,7(1PE9.3,A)))') (SCPROF(j,i),',',j=1,31)
      end do
      write(31,'(1H ,A)') '/END'
      endfile 31
      close(31)
      return
      end

      double precision function xsif (Z)
      implicit none
      integer IZ
      double precision Z, FCOULC
      include 'pegscommons/radlen.f'
      if (Z.le.4.0) then
        IZ=Z
        xsif=ALRADP(IZ)/(ALRAD(IZ)-FCOULC(Z))
      else
        xsif=dlog(A1440*Z**(-2./3.))/(dlog(A183*Z**(-1./3.))-FCOULC(Z))
      end if
      return
      end

      double precision function ztbl(IASYM)
      implicit none
      integer ie
      include 'pegscommons/elemtb.f'
      include 'pegscommons/elmtbc.f'
      character*4 IASYM,IA
      data IA/'A'/
      if (IASYM.eq.IA) then
        ztbl=18.0
        return
      end if
      do ie=1,NET
        if (IASYM.eq.ASYMT(ie)) then
          ztbl=ie
          return
        end if
      end do
      write(  26,100) IASYM,NET
100   format(1X,A2,' Not an atomic symbol for an element with Z LE ',I3)
      ZTBL=0.0
      return
      end

!-------------------------last line of pegs5.f--------------------------
