!-------------------------------cg_related.f--------------------------------
! Version: 090114-0925
! Provided by T. Torii and T. Sugita
! Reference: T. Torii and T. Sugita, "Development of PRESTA-CG 
! Incorporating Combinatorial Geometry in EGS4/PRESTA", JNC TN1410 2002-201,
! Japan Nuclear Cycle Development Institute (2002).
!
!-----------------------------------------------------------------------
!23456789|123456789|123456789|123456789|123456789|123456789|123456789|12

! ----------------------------------------------------------------------
! Required (geometry) subroutine for use with the EGS5 Code System
! ----------------------------------------------------------------------
! This is a cg-related subroutine. 
! ----------------------------------------------------------------------

!-------------------------------qadrti.f--------------------------------
! Version: 051219-1435
! Reference: JNC TN1410 2002-201
!-----------------------------------------------------------------------
!23456789|123456789|123456789|123456789|123456789|123456789|123456789|12

! ----------------------------------------------------------------------
! Required (geometry) subroutine for use with the EGS5 Code System
! ----------------------------------------------------------------------
! This is a subroutine qadrti. 
! ----------------------------------------------------------------------
      subroutine qadrti(a,x1,x2,i)
      implicit none
      double precision a(3),x1,x2
      integer i
      double precision p,q,d,rd
c
c'    a(1)*x*x+a(2)*x+a(3)=0. ==>> (x-x1)(x-x2)=0.
c
      if(a(1).ne.0.) then
        p=a(2)/a(1)
        q=a(3)/a(1)
        p=0.5d0*p
        d=p*p-q
        if(d.lt.0.0d0) then
          rd=sqrt(-d)
          x1=-p
          x2=rd
          i=0
        elseif(d.eq.0.) then
          x1=-p
          x2=-p
          i=2
        else
          i=2
          rd=sqrt(d)
          if(p.lt.0.) then
            x1=-p+rd
            x2=q/x1
          elseif(p.eq.0.) then
            x1=rd
            x2=-rd
          else
            x1=-p-rd
            x2=q/x1
          endif
        endif
      else
        if(a(2).ne.0.) then
          x1=-a(3)/a(2)
          x2=-a(3)/a(2)
          i=2
        else
          write(6,'(1h ,a)')
     &    '<< qadrti >> parameter error : a(1) = 0. & a(2) =0.)'
          i=0
          x1=0.0d0
          x2=0.0d0
        endif
      endif
      return
      end
!-----------------last line of subroutine qadrti-----------------------
                                                                        
!-------------------------------carda.f--------------------------------
! Version: 051219-1435
! Reference: JNC TN1410 2002-201
!-----------------------------------------------------------------------
!23456789|123456789|123456789|123456789|123456789|123456789|123456789|12

! ----------------------------------------------------------------------
! Required (geometry) subroutine for use with the EGS5 Code System
! ----------------------------------------------------------------------
! This is a subroutine carda. 
! ----------------------------------------------------------------------
      subroutine carda(aa,x1,x2,x3,i)
      implicit none
      double precision aa(4),a(4),b(4),x1,x2,x3
      double precision eps1,eps2,eps3,e,con,d,p,q,r,s,t,r1,r1c,r2c,rd,
     &                 u,v,xt1,xt2,xt3,dd,r2
      double precision theata,sr,ct,st,xmax
      integer i,ind1,ind2
c
c'    aa(1)*x*x*x+aa(2)*x*x+aa(3)*x+aa(4)=0. ==>> (x-x1)(x-x2)(x-x3)=0.
c
      ind1=0
c      eps1=1.0d-75
c      eps2=1.0d-25
      eps1=1.0d-12
      eps2=1.0d-12
      eps3=1.0d-8
      do 100 i=1,4
        a(i)=aa(i)
        b(i)=aa(i)
  100 continue
c                                                                               
      e=1.0d0/3.0d0
      con=sqrt(3.0d0)/2.0d0
      a(2)=a(2)/a(1)
      a(3)=a(3)/a(1)
      a(4)=a(4)/a(1)
c
  200 continue
        r=a(2)/3.0d0
        s=a(3)/3.0d0
        p=-(r*r)+s
        q=2.0d0*r*r*r-a(2)*s+a(4)
        if((abs(q).le.eps1).and.(abs(p).le.eps2)) go to 300
          t=p*p*p
          d=q*q+4.0d0*t
          if(abs(d).lt.eps3*(abs(q*q)+abs(4.0d0*t))) d=0.0d0
          if (d.eq.0.0d0) then
            r1=-q*0.5d0
            r1c=(abs(r1))**e
            if(r1.lt.0.) then
              r1c=-r1c
            endif
            x1=2.0d0*r1c-r
            if(abs(x1).lt.eps3*(abs(2.0d0*r1c)+abs(r))) x1=0.0d0
            x2=-r1c-r
            x3=-r1c-r
            if(abs(x2).lt.eps3*(abs(r1c)+abs(r))) then
              x2=0.0d0
              x3=0.0d0
            endif
            i=3
            return
          elseif (d.gt.0.0d0) then
            rd=sqrt(d)
            if (q.gt.0.0d0) then
              r1=(-q-rd)*0.5d0
            else
              r1=(-q+rd)*0.5d0
            endif
            r2=-t/r1
            r1c=(abs(r1))**e
            if(r1.lt.0.0d0) then
              r1c=-r1c
            endif
            r2c=(abs(r2))**e
            if(r2.lt.0.0d0) then
              r2c=-r2c
            endif
            u=r1c+r2c
            v=r1c-r2c
            xt1=u-r
            xt2=-0.5d0*u-r
            xt3=con*v
            if(ind1.eq.1) then
              if(ind2.eq.1) then
                x1=1.0d0/xt1
              else
                dd=xt2*xt2+xt3*xt3
                x2=xt2/dd
                x3=xt3/dd
              endif
              i=1
              return
            else
              if((xt1*xt1).ge.(xt2*xt2+xt3*xt3)) then
                x1=xt1
                ind2=0
              else
                x2=xt2
                x3=xt3
                ind2=1
              endif
            endif
          else
            rd=sqrt(-d)
            theata=(atan2(rd,(-q)))/3.0d0
            sr=2.0d0*sqrt(-p)
            ct=cos(theata)
            st=sin(theata)
            xt1=sr*ct-r
            r1c=-0.5d0*ct*sr
            r2c=con*st*sr
            xt2=r1c-r2c-r
            xt3=r1c+r2c-r
            if(abs(xt1).ge.max(abs(xt2),abs(xt3))) then
              xmax=xt1
            elseif(abs(xt2).ge.abs(xt3)) then
              xmax=xt2
            else
              xmax=xt3
            endif
            if(ind1.eq.1) then
              x2=1.0d0/xmax
              x3=-(b(4)/b(1))/(x1*x2)
              i=3
              return
            else
              x1=xmax
            endif
          endif
          ind1=1
          a(2)=b(3)/b(4)
          a(3)=b(2)/b(4)
          a(4)=b(1)/b(4)
          goto 200
  300 continue
      x1=-r
      x2=-r
      x3=-r
      i=3
      return
      end
C                                                                       
!-----------------------last line of subroutine carda------------------
                                                                        
!-------------------------------ferra.f--------------------------------
! Version: 051219-1435
! Reference: JNC TN1410 2002-201
!-----------------------------------------------------------------------
!23456789|123456789|123456789|123456789|123456789|123456789|123456789|12

! ----------------------------------------------------------------------
! Required (geometry) subroutine for use with the EGS5 Code System
! ----------------------------------------------------------------------
! This is a subroutine ferra. 
! ----------------------------------------------------------------------
      subroutine ferra(a,x1,x2,x3,x4,i)
      implicit none
      double precision a(5),ca(4),aa(3),x1,x2,x3,x4
      double precision eps,b1,b2,b3,b4,bb,p,q,r,s,t,dd,u,
     &                 x1r,x1i,theata,rou,e,x2r,x2i,as,d
      double precision y1,y2,y3,z1,z2
      integer i,j,k,l
c
c'    a(1)*x*x*x*x+a(2)*x*x*x+a(3)*x*x+a(4)*x+a(5)=0.
c'     ==>> (x-x1)(x-x2)(x-x3)(x-x4)=0.
c
c      eps=1.0d-75
      eps=1.0d-12
      b1=a(2)/a(1)
      b2=a(3)/a(1)
      b3=a(4)/a(1)
      b4=a(5)/a(1)
      bb=b1/4.0d0
      p=-(3.0d0*b1*b1)/8.d0+b2
      q=-b1*p/2.d0-(b1*b1*b1)/16.0d0+b3
      r=-b1*q/8.0d0+(b1*b1*b1*b1)/256.d0-b1*b3/8.0d0+b4
      if(abs(q).le.eps) goto 200
      if(q*q/(abs(4.d0*r*p)+q*q).le.eps) goto 200
      dd=4.d0*r*p-q*q
      if(abs(dd).le.eps) goto 210
      ca(1)=1.0d0
      ca(2)=-p
      ca(3)=-4.0d0*r
      ca(4)=dd
      call carda(ca,y1,y2,y3,l)
      if(l.ne.1) y1=max(y1,y2,y3)
  220 continue
      u=y1/2.0d0
      aa(1)=1.0d0
      if(y1-p.le.0.0) then
c        write(*,*) 'y1-p,y1,p=',y1-p,y1,p
        s=0.
        t=0.
        if(u.le.0) then
          j=2
          x1=sqrt(-u)
          x2=-sqrt(-u)
        else
          j=0
          x1r=0.0d0
          x1i=sqrt(u)
        endif
      else
        s=sqrt(y1-p)
        t=q/(2.0d0*s)
        aa(2)=s
        aa(3)=-t+u
        call qadrti(aa,z1,z2,j)
        if(j.eq.2) then
          x1=z1
          x2=z2
        else
          x1r=z1
          x1i=z2
        endif
      endif
      aa(2)=-s
      aa(3)=t+u
  290 continue
      call qadrti(aa,z1,z2,k)
      if(k.eq.2) then
        x3=z1
        x4=z2
      else
        x2r=z1
        x2i=z2
      endif
      i=j+k
  260 continue
      if(i.eq.0) then
        x1=x1r-bb
        x2=x1i
        x3=x2r-bb
        x4=x2i
      elseif(i.eq.2) then
        if(j.eq.2) then
          x1=x1-bb
          x2=x2-bb
          x3=x2r-bb
          x4=x2i
        else
          x1=x3-bb
          x2=x4-bb
          x3=x1r-bb
          x4=x1i
        endif
      else
        x1=x1-bb
        x2=x2-bb
        x3=x3-bb
        x4=x4-bb
      endif
      return
  210 continue
      y1=0.0d0
      aa(1)=1.d0
      aa(2)=-p
      aa(3)=-4.0d0*r
      call qadrti(aa,y2,y3,l)
      if(l.eq.2) y1=max(y1,y2,y3)
      goto 220
  200 continue
      if(abs(r).le.eps) goto 230
      if(abs(4.d0*r)/(p*p+abs(4.d0*r)).le.eps) goto 230
      d=p*p-4.d0*r
      if(d.lt.0.) then
        e=1.d0/4.d0
        rou=r**e
        as=sqrt(-d)
        theata=(atan2(as,(-p)))*0.5d0
        x1r=rou*cos(theata)
        x1i=rou*sin(theata)
        x2r=-x1r
        x2i=x1i
        i=0 
        goto 260
      else
        aa(1)=1.d0
        aa(2)=0.0d0
        aa(3)=0.5d0*(p+sqrt(d))
        call qadrti(aa,z1,z2,j)
        if(j.eq.2) then
          x1=z1
          x2=z2
        else
          x1r=z1
          x1i=z2
        endif
        aa(3)=0.5d0*(p-sqrt(d))
        goto 290
       endif
 230   continue
        if(abs(p).le.eps) goto 300
          x1=0.0d0
          x2=0.0d0
          j=2
          aa(1)=1.d0
          aa(2)=0.d0
          aa(3)=p
          goto 290
  300 continue
      x1=-bb
      x2=x1
      x3=x1
      x4=x1
      i=4
      return
      end
!-----------------------last line of subroutine ferra.f----------------

!-------------------------------rppset.f--------------------------------
! Version: 051219-1435
! Reference: JNC TN1410 2002-201
!-----------------------------------------------------------------------
!23456789|123456789|123456789|123456789|123456789|123456789|123456789|12

! ----------------------------------------------------------------------
! Required (geometry) subroutine for use with the EGS5 Code System
! ----------------------------------------------------------------------
! This is a subroutine rppset.
      subroutine rppset(izon)
      implicit none
c
      include 'auxcommons/geom_common.f' ! geom-common file
c
      integer izon

      if(rpppnt(1,izon).ge.rpppnt(2,izon).or.
     &   rpppnt(3,izon).ge.rpppnt(4,izon).or.
     &   rpppnt(5,izon).ge.rpppnt(6,izon)) then
        write(*,*) 'Error of RPP ',nbrpp(izon),' : MAX <= MIX'
        stop
      end if
      return
      end
!--------------------last line of subroutine rppset.f------------------

!-------------------------------rppcg1.f--------------------------------
! Version: 051219-1435
! Reference: JNC TN1410 2002-201
!-----------------------------------------------------------------------
!23456789|123456789|123456789|123456789|123456789|123456789|123456789|12

! ----------------------------------------------------------------------
! Required (geometry) subroutine for use with the EGS5 Code System
! ----------------------------------------------------------------------
! This is a subroutine rppcg1. 
! ----------------------------------------------------------------------
      subroutine rppcg1(izon,xl,yl,zl,unp,vnp,wnp)
      implicit none
c
      include 'auxcommons/geom_common.f' ! geom-common file
c
      double precision xl,yl,zl
      double precision unp,vnp,wnp
      integer izon
      double precision udotal
c
      if(unp.ne.0.0d0) then
        udotal=(rpppnt(1,izon)-xl)/unp
        if ((udotal.ge.-rppeps)) then
          if(udotal.ge.0.0d0) then
            itvalm=itvalm+1
            atval(itvalm)=udotal
          else
            itvalm=itvalm+1
            atval(itvalm)=0.0d0
          endif
        end if
        udotal=(rpppnt(2,izon)-xl)/unp
        if ((udotal.ge.-rppeps)) then
          if(udotal.ge.0.0d0) then
            itvalm=itvalm+1
            atval(itvalm)=udotal
          else
            itvalm=itvalm+1
            atval(itvalm)=0.0d0
          endif
        end if
      end if
      if(vnp.ne.0.0d0) then
        udotal=(rpppnt(3,izon)-yl)/vnp
        if ((udotal.ge.-rppeps)) then
          if(udotal.ge.0.0d0) then
            itvalm=itvalm+1
            atval(itvalm)=udotal
          else
            itvalm=itvalm+1
            atval(itvalm)=0.0d0
          endif
        end if
        udotal=(rpppnt(4,izon)-yl)/vnp
        if ((udotal.ge.-rppeps)) then
          if(udotal.ge.0.0d0) then
            itvalm=itvalm+1
            atval(itvalm)=udotal
          else
            itvalm=itvalm+1
            atval(itvalm)=0.0d0
          endif
        end if
      end if
      if(wnp.ne.0.0d0) then
        udotal=(rpppnt(5,izon)-zl)/wnp
        if ((udotal.ge.-rppeps)) then
          if(udotal.ge.0.0d0) then
            itvalm=itvalm+1
            atval(itvalm)=udotal
          else
            itvalm=itvalm+1
            atval(itvalm)=0.0d0
          endif
        end if
        udotal=(rpppnt(6,izon)-zl)/wnp
        if(udotal.ge.-rppeps) then
          if(udotal.ge.0.0d0) then
            itvalm=itvalm+1
            atval(itvalm)=udotal
          else
            itvalm=itvalm+1
            atval(itvalm)=0.0d0
          endif
        end if
      end if
      return
      end 
!-----------------------last line of subroutine rppcg1------------------
                                                                        
!-------------------------------rcccg1.f--------------------------------
! Version: 051219-1435
! Reference: JNC TN1410 2002-201
!-----------------------------------------------------------------------
!23456789|123456789|123456789|123456789|123456789|123456789|123456789|12

! ----------------------------------------------------------------------
! Required (geometry) subroutine for use with the EGS5 Code System
! ----------------------------------------------------------------------
! This is a subroutine rcccg1. 
! ----------------------------------------------------------------------
      subroutine rcccg1(izon,xl,yl,zl,unp,vnp,wnp)
      implicit none
c
      include 'auxcommons/geom_common.f' ! geom-common file
c
      double precision xl,yl,zl
      double precision unp,vnp,wnp
      integer izon
      double precision udotr
      double precision acyl,acylmv,bcyl,bcylmv,bcyln,ccyl,ccyln,dcyl,
     &                 ecylm2,fcyl,argcy,clong2,rootcy
c
      acylmv=1.0d0
      acyl= dsqrt(rccpnt(4,izon)*rccpnt(4,izon)
     &           +rccpnt(5,izon)*rccpnt(5,izon)
     &           +rccpnt(6,izon)*rccpnt(6,izon))
      bcyl=((xl-rccpnt(1,izon))*rccpnt(4,izon)
     &    + (yl-rccpnt(2,izon))*rccpnt(5,izon)
     &    + (zl-rccpnt(3,izon))*rccpnt(6,izon))/acyl
      bcylmv=(rccpnt(4,izon)*unp+rccpnt(5,izon)*vnp
     &      + rccpnt(6,izon)*wnp)/acyl
      ccyl=(xl-rccpnt(1,izon))*(xl-rccpnt(1,izon))
     &    +(yl-rccpnt(2,izon))*(yl-rccpnt(2,izon))
     &    +(zl-rccpnt(3,izon))*(zl-rccpnt(3,izon))
      dcyl=(xl-rccpnt(1,izon))*unp+(yl-rccpnt(2,izon))*vnp
     &    +(zl-rccpnt(3,izon))*wnp
      ecylm2=dabs(acylmv*acylmv-bcylmv*bcylmv)
      if(bcylmv.ne.0.d0) then
        udotr=(acyl-bcyl)/bcylmv*acylmv
        if(udotr.ge.0.d0) then
          bcyln=((xl+udotr*unp-rccpnt(1,izon))*rccpnt(4,izon)
     &          +(yl+udotr*vnp-rccpnt(2,izon))*rccpnt(5,izon)
     &          +(zl+udotr*wnp-rccpnt(3,izon))*rccpnt(6,izon))/acyl
          ccyln=(xl+udotr*unp-rccpnt(1,izon))
     &         *(xl+udotr*unp-rccpnt(1,izon))
     &         +(yl+udotr*vnp-rccpnt(2,izon))
     &         *(yl+udotr*vnp-rccpnt(2,izon))
     &         +(zl+udotr*wnp-rccpnt(3,izon))
     &         *(zl+udotr*wnp-rccpnt(3,izon))
          clong2=dabs(ccyln-bcyln*bcyln)
          if(clong2.le.
     &      (rccpnt(7,izon)+rcceps)*(1.0d0+rcceps)
     &     *(rccpnt(7,izon)+rcceps)*(1.0d0+rcceps) ) then
            itvalm=itvalm+1
            atval(itvalm)=udotr
          end if
        end if
        udotr=-bcyl/bcylmv*acylmv
        if(udotr.ge.0.0d0) then
          bcyln=((xl+udotr*unp-rccpnt(1,izon))*rccpnt(4,izon)
     &          +(yl+udotr*vnp-rccpnt(2,izon))*rccpnt(5,izon)
     &          +(zl+udotr*wnp-rccpnt(3,izon))*rccpnt(6,izon))/acyl
          ccyln=(xl+udotr*unp-rccpnt(1,izon))
     &         *(xl+udotr*unp-rccpnt(1,izon))
     &         +(yl+udotr*vnp-rccpnt(2,izon))
     &         *(yl+udotr*vnp-rccpnt(2,izon))
     &         +(zl+udotr*wnp-rccpnt(3,izon))
     &         *(zl+udotr*wnp-rccpnt(3,izon))
          clong2=dabs(ccyln-bcyln*bcyln)
          if(clong2.le.
     &      (rccpnt(7,izon)+rcceps)*(1.0d0+rcceps)
     &     *(rccpnt(7,izon)+rcceps)*(1.0d0+rcceps)) then
            itvalm=itvalm+1
            atval(itvalm)=udotr
          end if
        end if
      end if
      if(ecylm2.ne.0.0d0) then
        fcyl=(-dcyl+bcylmv*bcyl)/ecylm2
        argcy=fcyl*fcyl-(ccyl-bcyl*bcyl-rccpnt(7,izon)*rccpnt(7,izon))
     &       /ecylm2
        if(argcy.ge.0.0d0) then
          rootcy=dsqrt(argcy)
          udotr=(fcyl-rootcy)
          if(udotr.ge.0.0d0) then
            bcyln=((xl+udotr*unp-rccpnt(1,izon))*rccpnt(4,izon)
     &            +(yl+udotr*vnp-rccpnt(2,izon))*rccpnt(5,izon)
     &            +(zl+udotr*wnp-rccpnt(3,izon))*rccpnt(6,izon))/acyl
            if((bcyln+rcceps)*(1.0d0+rcceps).ge.0.0d0.and.
     &         (bcyln-rcceps)*(1.0d0-rcceps).le.acyl) then
              itvalm=itvalm+1
              atval(itvalm)=udotr
            end if
          end if
          udotr=(fcyl+rootcy)
          if(udotr.ge.0.0d0) then
            bcyln=((xl+udotr*unp-rccpnt(1,izon))*rccpnt(4,izon)
     &            +(yl+udotr*vnp-rccpnt(2,izon))*rccpnt(5,izon)
     &            +(zl+udotr*wnp-rccpnt(3,izon))*rccpnt(6,izon))/acyl
            if (((bcyln+rcceps)*(1.0d0+rcceps).ge.0.0d0.and.
     &           (bcyln-rcceps)*(1.0d0-rcceps).le.acyl)) then
              itvalm=itvalm+1
              atval(itvalm)=udotr
            end if
          end if
        end if
      end if
      return
      end 
!-----------------------last line of subroutine rcccg1------------------
                                                                        
!-------------------------------sphcg1.f--------------------------------
! Version: 051219-1435
! Reference: JNC TN1410 2002-201
!-----------------------------------------------------------------------
!23456789|123456789|123456789|123456789|123456789|123456789|123456789|12

! ----------------------------------------------------------------------
! Required (geometry) subroutine for use with the EGS5 Code System
! ----------------------------------------------------------------------
! This is a subroutine sphcg1. 
! ----------------------------------------------------------------------
      subroutine sphcg1(izon,xl,yl,zl,unp,vnp,wnp)
      implicit none
c
      include 'auxcommons/geom_common.f' ! geom-common file
c
      double precision xl,yl,zl
      double precision unp,vnp,wnp
      integer izon
      double precision udotr
      double precision asph,bsph,csph,argsp,rootsp
      asph=1.0d0
      bsph=((xl-sphpnt(1,izon))*unp+(yl-sphpnt(2,izon))*vnp
     &    + (zl-sphpnt(3,izon))*wnp)/asph
      csph=(xl-sphpnt(1,izon))*(xl-sphpnt(1,izon))
     &    +(yl-sphpnt(2,izon))*(yl-sphpnt(2,izon))
     &    +(zl-sphpnt(3,izon))*(zl-sphpnt(3,izon))
     &    -sphpnt(4,izon)*sphpnt(4,izon)
      argsp=bsph*bsph-csph
      if(argsp.ge.0.0d0) then
        rootsp=dsqrt(argsp)
        if(csph.le.0.0d0) then
          udotr=(-bsph+rootsp)/asph
          if(udotr.ge.-spheps) then
            if(udotr.ge.0.0d0) then
              itvalm=itvalm+1
              atval(itvalm)=udotr
            else
              itvalm=itvalm+1
              atval(itvalm)=0.0d0
            end if
          end if
        else
          udotr=(-bsph-rootsp)/asph
          if(udotr.ge.-spheps) then
            if(udotr.ge.0.0d0) then
              itvalm=itvalm+1
              atval(itvalm)=udotr
            else
              itvalm=itvalm+1
              atval(itvalm)=0.0d0
            end if
          end if
          udotr=(-bsph+rootsp)/asph
          if(udotr.ge.-spheps) then
            if(udotr.ge.0.0d0) then
              itvalm=itvalm+1
              atval(itvalm)=udotr
            else
              itvalm=itvalm+1
              atval(itvalm)=0.0d0
            end if
          end if
        end if
      end if
      return
      end 
!-----------------------last line of subroutine sphcg1------------------
                                                                        
!-------------------------------srzold.f--------------------------------
! Version: 051219-1435
! Reference: JNC TN1410 2002-201
!-----------------------------------------------------------------------
!23456789|123456789|123456789|123456789|123456789|123456789|123456789|12

! ----------------------------------------------------------------------
! Required (geometry) subroutine for use with the EGS5 Code System
! ----------------------------------------------------------------------
! This is a subroutine srzold. 
! ----------------------------------------------------------------------
      subroutine srzold(xl,yl,zl,irlold,irlfg)
      implicit none
c
      include 'auxcommons/dataconst_common.f' ! dataconst-common file
      include 'auxcommons/geom_common.f' ! geom-common file
c
      double precision xl,yl,zl
      integer irlold,irlfg
      double precision acyl,bcyl,ccyl,clong1
      double precision thator,thstor,thetor
      integer nozone,jty,kno
      integer m,n,k,imach,imachf,ithafg
c rec & tec
      double precision hh,rs2,rl2,rs4,rl4,plx,ply,plz
      double precision vph,vrs,vrl,um
c tec
      double precision rrf,rrf2
c ell & gel
      double precision b1,b2
c wed & box & hex & arb
      integer i,ii,ifgin
c wed & box & hex
      double precision ap(4)
c wed
      double precision ap4,top
c arb
      integer nside
      double precision dx
c gel
      double precision b3
c
      integer iorchk,iio
      integer iinout(MAX_GEOM)
c
      do n=1,MAX_GEOM
        iinout(n)=0
      enddo
c
      irlfg=1
      do n=irlold,irlold
        imach=1
        imachf=0
        iorchk=iorcnt(n)
        do k=1,nbbody(n)
          if(zoneor(k,n).eq.'OR'.or.zoneor(k,n).eq.'or') then
            iorchk=iorchk-1
            if(imachf.eq.1.and.imach.eq.1) then
              irlfg=0
              goto 900
            else
              imachf=0
              imach=1
            end if
          end if
          m=nbzone(k,n)
          nozone=abs(nbzone(k,n))
          jty=itblty(nozone)
          kno=itblno(nozone)
          iio=iinout(nozone)
          if(iio.lt.0.and.m.gt.0) then
            imach=0
            imachf=1
          elseif(iio.gt.0.and.m.lt.0) then
            imach=0
            imachf=1
          else
c     rpp check
            if(jty.eq.ityknd(1)) then
              if(kno.ge.1.and.kno.le.irppin) then
                if(xl.lt.rpppnt(1,kno).or.xl.gt.rpppnt(2,kno).or.
     &             yl.lt.rpppnt(3,kno).or.yl.gt.rpppnt(4,kno).or.
     &             zl.lt.rpppnt(5,kno).or.zl.gt.rpppnt(6,kno)) then
                  iinout(nozone)=-1
                  if ((m.gt.0)) then
                    imach=0
                  end if
                else
                  iinout(nozone)=1
                  if ((m.lt.0)) then
                    imach=0
                  end if
                end if
                imachf=1
              endif
c     sph check
            elseif(jty.eq.ityknd(2)) then
              if(kno.ge.1.and.kno.le.isphin) then
                if((xl-sphpnt(1,kno))*(xl-sphpnt(1,kno))
     &            +(yl-sphpnt(2,kno))*(yl-sphpnt(2,kno))
     &            +(zl-sphpnt(3,kno))*(zl-sphpnt(3,kno)) .gt.
     &            sphpnt(4,kno)*sphpnt(4,kno)) then
                  iinout(nozone)=-1
                  if(m.gt.0) then
                    imach=0
                  end if
                else
                  iinout(nozone)=1
                  if(m.lt.0) then
                    imach=0
                  end if
                end if
                imachf=1
              end if
c     rcc check
            elseif(jty.eq.ityknd(3)) then
              if(kno.ge.1.and.kno.le.irccin) then
                acyl=dsqrt(rccpnt(4,kno)*rccpnt(4,kno)
     &                    +rccpnt(5,kno)*rccpnt(5,kno)
     &                    +rccpnt(6,kno)*rccpnt(6,kno))
                bcyl=((xl-rccpnt(1,kno))*rccpnt(4,kno)
     &              + (yl-rccpnt(2,kno))*rccpnt(5,kno)
     &              + (zl-rccpnt(3,kno))*rccpnt(6,kno))/acyl
                ccyl=(xl-rccpnt(1,kno))*(xl-rccpnt(1,kno))
     &              +(yl-rccpnt(2,kno))*(yl-rccpnt(2,kno))
     &              +(zl-rccpnt(3,kno))*(zl-rccpnt(3,kno))
                clong1=dsqrt(dabs(ccyl-bcyl*bcyl))
                if(clong1.gt.rccpnt(7,kno).or.
     &             bcyl.lt.0.0d0.or.bcyl.gt.acyl) then
                  iinout(nozone)=-1
                  if(m.gt.0) then
                    imach=0
                 end if
                else
                  iinout(nozone)=1
                  if(m.lt.0) then
                    imach=0
                  end if
                end if
                imachf=1
              end if
c     trc check
            elseif(jty.eq.ityknd(4)) then
              if(kno.ge.1.and.kno.le.itrcin) then
                acyl=dsqrt(trcpnt(4,kno)*trcpnt(4,kno)
     &                    +trcpnt(5,kno)*trcpnt(5,kno)
     &                    +trcpnt(6,kno)*trcpnt(6,kno))
                bcyl=((xl-trcpnt(1,kno))*trcpnt(4,kno)
     &              + (yl-trcpnt(2,kno))*trcpnt(5,kno)
     &              + (zl-trcpnt(3,kno))*trcpnt(6,kno))/acyl
                ccyl=(xl-trcpnt(1,kno))*(xl-trcpnt(1,kno))
     &              +(yl-trcpnt(2,kno))*(yl-trcpnt(2,kno))
     &              +(zl-trcpnt(3,kno))*(zl-trcpnt(3,kno))
                clong1=dsqrt(dabs(ccyl-bcyl*bcyl))
                if(clong1.gt.(trcpnt(8,kno)-trcpnt(7,kno))/acyl*bcyl
     &            +trcpnt(7,kno) .or.bcyl.lt.0.0.or.bcyl.gt.acyl) then
                  iinout(nozone)=-1
                  if(m.gt.0) then
                    imach=0
                  end if
                else
                  iinout(nozone)=1
                  if(m.lt.0) then
                    imach=0
                  end if
                end if
                imachf=1
              end if
c     tor check
            elseif(jty.eq.ityknd(5)) then
              if(kno.ge.1.and.kno.le.itorin) then
                if(nint(torpnt(8,kno)).eq.1) then
                  acyl=(yl-torpnt(2,kno))*(yl-torpnt(2,kno))
     &                +(zl-torpnt(3,kno))*(zl-torpnt(3,kno))
                  bcyl=xl-torpnt(1,kno)
                  if(yl-torpnt(2,kno).eq.0.0d0.and.
     &               zl-torpnt(3,kno).eq.0.0d0) then
                    thator=0.0d0
                  else
                    thator=atan2(zl-torpnt(3,kno),yl-torpnt(2,kno))
                  end if
                end if
                if(nint(torpnt(8,kno)).eq.2) then
                  acyl=(zl-torpnt(3,kno))*(zl-torpnt(3,kno))
     &                +(xl-torpnt(1,kno))*(xl-torpnt(1,kno))
                  bcyl=yl-torpnt(2,kno)
                  if(zl-torpnt(3,kno).eq.0.0d0.and.
     &               xl-torpnt(1,kno).eq.0.0d0) then
                    thator=0.0d0
                  else
                    thator=atan2(xl-torpnt(1,kno),zl-torpnt(3,kno))
                  end if
                end if
                if(nint(torpnt(8,kno)).eq.3) then
                  acyl=(xl-torpnt(1,kno))*(xl-torpnt(1,kno))
     &                +(yl-torpnt(2,kno))*(yl-torpnt(2,kno))
                  bcyl=zl-torpnt(3,kno)
                  if(yl-torpnt(2,kno).eq.0.0d0.and.
     &               xl-torpnt(1,kno).eq.0.0d0) then
                    thator=0.0d0
                  else
                    thator=atan2(yl-torpnt(2,kno),xl-torpnt(1,kno))
                  end if
                end if
                thator=thator*180.0d0/PI
                thstor=torpnt(6,kno)
                thetor=torpnt(7,kno)
                if(thator.lt.0.0d0) then
                  thator=thator+360.0d0
                end if
                if(thstor.lt.0.0d0) then
                  thstor=thstor+360.0d0
                end if
                if(thetor.lt.0.0d0) then
                  thetor=thetor+360.0d0
                end if
                ithafg=0
                if(thstor.eq.thetor) then
                  ithafg=1
                else
                  if(thetor.gt.thstor) then
                    if(thator.ge.thstor.and.thator.le.thetor) then
                      ithafg=1
                    end if
                  else
                    if(thator.ge.thstor.or.thator.le.thetor) then
                      ithafg=1
                    end if
                  end if
                end if
                ccyl=dsqrt(acyl)-torpnt(4,kno)
                clong1=dsqrt(ccyl*ccyl+bcyl*bcyl)
                if(clong1.gt.torpnt(5,kno).or.ithafg.eq.0) then
                  iinout(nozone)=-1
                  if(m.gt.0) then
                    imach=0
                  end if
                else
                  iinout(nozone)=1
                  if(m.lt.0) then
                    imach=0
                  end if
                end if
                imachf=1
              end if
c     rec check
            elseif(jty.eq.ityknd(6)) then
              if(kno.ge.1.and.kno.le.irecin) then
                hh =recpnt(13,kno)
                rs2=recpnt(14,kno)
                rl2=recpnt(15,kno)
                rs4=rs2*rs2
                rl4=rl2*rl2
c
c5    COMPUTE (V-XB) FOR X,Y,Z COORDINATES
c
                plx=recpnt(1,kno)-xl
                ply=recpnt(2,kno)-yl
                plz=recpnt(3,kno)-zl
c
c6    TRANSFORM XL,YL,ZL TO THE COORDINATES OF THE REC
c
                vph=plx*recpnt( 4,kno)+ply*recpnt( 5,kno)
     &             +plz*recpnt( 6,kno)
                vrs=plx*recpnt( 7,kno)+ply*recpnt( 8,kno)
     &             +plz*recpnt( 9,kno)
                vrl=plx*recpnt(10,kno)+ply*recpnt(11,kno)
     &             +plz*recpnt(12,kno)
                um=rl4*vrs*vrs+rs4*vrl*vrl-rs4*rl4
                if(-vph.lt.0.0d0.OR.-vph.gt.hh.or.um.gt.0.0d0) then
                  iinout(nozone)=-1
                  if(m.gt.0) then
                    imach=0
                  end if
                else
                  iinout(nozone)=1
                  if(m.lt.0) then
                    imach=0
                  end if
                end if
                imachf=1
              end if
c     ell check
            elseif(jty.eq.ityknd(7)) then
              if(kno.ge.1.and.kno.le.iellin) then
                b1=(xl-ellpnt(1,kno))*(xl-ellpnt(1,kno))
     &            +(yl-ellpnt(2,kno))*(yl-ellpnt(2,kno))
     &            +(zl-ellpnt(3,kno))*(zl-ellpnt(3,kno))
                b2=(xl-ellpnt(4,kno))*(xl-ellpnt(4,kno))
     &            +(yl-ellpnt(5,kno))*(yl-ellpnt(5,kno))
     &            +(zl-ellpnt(6,kno))*(zl-ellpnt(6,kno))
                if(dsqrt(b1)+dsqrt(b2).gt.ellpnt(7,kno)) then
                  iinout(nozone)=-1
                  if(m.gt.0) then
                    imach=0
                  end if
                else
                  iinout(nozone)=1
                  if(m.lt.0) then
                    imach=0
                  end if
                end if
                imachf=1
              end if
c     wed check
            elseif(jty.eq.ityknd(8)) then
              if(kno.ge.1.and.kno.le.iwedin) then
                ifgin=1
                do i=1,3
                  ii=3*i+1
                  ap(i)=(xl-wedpnt(1,kno))*wedpnt(ii,kno)
     &                 +(yl-wedpnt(2,kno))*wedpnt(ii+1,kno)
     &                 +(zl-wedpnt(3,kno))*wedpnt(ii+2,kno)
                  if(ap(i).lt.0.0.or.ap(i).gt.wedpnt(i+12,kno)) then
                    ifgin=0
                    go to 810
                  endif
                end do
                ap4=ap(1)*wedpnt(14,kno)+ap(2)*wedpnt(13,kno)
                top=wedpnt(13,kno)*wedpnt(14,kno)-ap4
                if(top.lt.0.0) then
                  ifgin=0
                endif
c
  810           continue
                if(ifgin.eq.0) then
                  iinout(nozone)=-1
                  if(m.gt.0) then
                    imach=0
                  end if
                else
                  iinout(nozone)=1
                  if(m.lt.0) then
                    imach=0
                  end if
                end if
               imachf=1
              end if
c     box check
            elseif(jty.eq.ityknd(9)) then
              if(kno.ge.1.and.kno.le.iboxin) then
                ifgin=1
                do i=1,3
                  ii=3*i+1
                  ap(i)=(xl-boxpnt(1,kno))*boxpnt(ii,kno)
     &                 +(yl-boxpnt(2,kno))*boxpnt(ii+1,kno)
     &                 +(zl-boxpnt(3,kno))*boxpnt(ii+2,kno)
c
                  if(ap(i).lt.0.0d0.or.ap(i).gt.boxpnt(i+12,kno)) then
                    ifgin=0
                    go to 910
                  endif
                end do
c
  910           continue
                if(ifgin.eq.0) then
                  iinout(nozone)=-1
                  if(m.gt.0) then
                    imach=0
                  end if
                else
                  iinout(nozone)=1
                  if(m.lt.0) then
                    imach=0
                  end if
                end if
                imachf=1
              end if
c     arb check
            elseif(jty.eq.ityknd(10)) then
              if(kno.ge.1.and.kno.le.iarbin) then
                ifgin=1
                nside=arbtbl(26,kno)
                do i=1,nside
                  ii=(i-1)*4+1
                  dx=arbtbl(ii  ,kno)*xl+arbtbl(ii+1,kno)*yl
     &              +arbtbl(ii+2,kno)*zl+arbtbl(ii+3,kno)
                  if( dx.lt.0.0d0) then
                    ifgin=0
                    goto 1010
                  endif
                end do
 1010           continue
                if(ifgin.eq.0) then
                  iinout(nozone)=-1
                  if(m.gt.0) then
                    imach=0
                  end if
                else
                  iinout(nozone)=1
                  if(m.lt.0) then
                    imach=0
                  end if
                end if
                imachf=1
              end if
c     hex check
            elseif(jty.eq.ityknd(11)) then
              if(kno.ge.1.and.kno.le.ihexin) then
                ifgin=1
                do i=1,4
                  ii=3*i
                  ap(i)=(xl-hexpnt(ii+12,kno))*hexpnt(ii+24,kno)
     &                 +(yl-hexpnt(ii+13,kno))*hexpnt(ii+25,kno)
     &                 +(zl-hexpnt(ii+14,kno))*hexpnt(ii+26,kno)
c
                  if(ap(i).lt.0.0d0.or.ap(i).gt.hexpnt(i+10,kno)) then
                    ifgin=0
                    go to 1110
                  endif
                end do
 1110           continue
                if(ifgin.eq.0) then
                  iinout(nozone)=-1
                  if(m.gt.0) then
                    imach=0
                  end if
                else
                  iinout(nozone)=1
                  if(m.lt.0) then
                    imach=0
                  end if
                end if
                imachf=1
              end if
c     haf check
            elseif(jty.eq.ityknd(12)) then
              if(kno.ge.1.and.kno.le.ihafin) then
                if((xl*hafpnt(1,kno)
     &              +yl*hafpnt(2,kno)
     &              +zl*hafpnt(3,kno)).lt.
     &              hafpnt(4,kno)*hafpnt(5,kno)) then
                  iinout(nozone)=-1
                  if(m.gt.0) then
                    imach=0
                  end if
                else
                  iinout(nozone)=1
                  if(m.lt.0) then
                    imach=0
                  end if
                end if
                imachf=1
              end if
c     tec check
            elseif(jty.eq.ityknd(13)) then
              if(kno.ge.1.and.kno.le.itecin) then
                hh =tecpnt(14,kno)
                rs2=tecpnt(15,kno)
                rl2=tecpnt(16,kno)
                rs4=rs2*rs2
                rl4=rl2*rl2
                plx=(xl-tecpnt(1,kno))
                ply=(yl-tecpnt(2,kno))
                plz=(zl-tecpnt(3,kno))
                vph=plx*tecpnt( 4,kno)+ply*tecpnt( 5,kno)
     &             +plz*tecpnt( 6,kno)
                vrs=plx*tecpnt( 7,kno)+ply*tecpnt( 8,kno)
     &             +plz*tecpnt( 9,kno)
                vrl=plx*tecpnt(10,kno)+ply*tecpnt(11,kno)
     &             +plz*tecpnt(12,kno)
                rrf =1.0d0-(1.0d0-tecpnt(13,kno))*vph/hh
                rrf2=rrf*rrf
                um=rl4*vrs*vrs+rs4*vrl*vrl-rs4*rl4*rrf2
                IF(vph.lt.0.0d0.or.vph.gt.hh.or.um.gt.0.0d0) then
                  iinout(nozone)=-1
                  if(m.gt.0) then
                    imach=0
                  end if
                else
                  iinout(nozone)=1
                  if(m.lt.0) then
                    imach=0
                  end if
                end if
               imachf=1
              end if
c     gel check
            elseif(jty.eq.ityknd(14)) then
              if(kno.ge.1.and.kno.le.igelin) then
                b1=((xl-gelpnt(1,kno))*gelpnt(4,kno)
     &             +(yl-gelpnt(2,kno))*gelpnt(5,kno)
     &             +(zl-gelpnt(3,kno))*gelpnt(6,kno))
     &             /gelpnt(13,kno)/gelpnt(13,kno)
                b2=((xl-gelpnt(1,kno))*gelpnt(7,kno)
     &             +(yl-gelpnt(2,kno))*gelpnt(8,kno)
     &             +(zl-gelpnt(3,kno))*gelpnt(9,kno))
     &             /gelpnt(14,kno)/gelpnt(14,kno)
                b3=((xl-gelpnt(1,kno))*gelpnt(10,kno)
     &             +(yl-gelpnt(2,kno))*gelpnt(11,kno)
     &             +(zl-gelpnt(3,kno))*gelpnt(12,kno))
     &             /gelpnt(15,kno)/gelpnt(15,kno)
                if((b1*b1+b2*b2+b3*b3).gt.1.0d0) then
                  iinout(nozone)=-1
                  if(m.gt.0) then
                    imach=0
                  end if
                else
                  iinout(nozone)=1
                  if(m.lt.0) then
                    imach=0
                  end if
                end if
                imachf=1
              end if
c
c**** added new geometry
c
            end if
          end if
          if(imachf.eq.1.and.imach.eq.0.and.iorchk.eq.0) then
            goto 800
          endif
        end do

        if(imachf.eq.1.and.imach.eq.1) then
          irlfg=0
          goto 900
        end if
  800   continue
      end do
c
  900 continue
      return
      end 
!--------------------last line of subroutine srzold.f-------------------
                                                                        
!-------------------------------srzone.f--------------------------------
! Version: 051219-1435
! Reference: JNC TN1410 2002-201
! Provided as new one from T. Sugita   07/28/2004
!-----------------------------------------------------------------------
!23456789|123456789|123456789|123456789|123456789|123456789|123456789|12
! ----------------------------------------------------------------------
! Required (geometry) subroutine for use with the EGS5 Code System
! ----------------------------------------------------------------------
! This is a subroutine srzone. 
! ----------------------------------------------------------------------
      subroutine srzone(xl,yl,zl,iray,irnow,irl)
      implicit none
c
      include 'auxcommons/dataconst_common.f' ! dataconst-common file
      include 'auxcommons/geom_common.f' ! geom-common file
c
      double precision xl,yl,zl
      integer iray,irnow,irl
      double precision acyl,bcyl,ccyl,clong1
      double precision thator,thstor,thetor
      integer n,k,imach,imachf,m,ithafg
      integer nn,iorchk,iio,nnn
      integer nozone,jty,kno
c rec & tec
      double precision hh,rs2,rl2,rs4,rl4,plx,ply,plz
      double precision vph,vrs,vrl,um
c tec
      double precision rrf,rrf2
c ell & gel
      double precision b1,b2
c wed & box & hex & arb
      integer i,ii,ifgin
c wed & box & hex
      double precision ap(4)
c wed
      double precision ap4,top
c arb
      integer nside
      double precision dx
c gel
      double precision b3
c
      integer iinout(MAX_GEOM)
c
      do nn=1,MAX_GEOM
        iinout(nn)=0
      enddo
c
      irl=0
      do nn=1,izonin
        nnn = nn
        if((izonin-1)*irnow.ne.0) then
          if((nn-1)/2.eq.0) then
            nnn=3-nn
          endif   
        endif   
        n=iznnxp(iray,nnn,irnow+1)
c       n=iznnxp(iray,nn,irnow)
        imach=1
        imachf=0
        iorchk=iorcnt(n)
        do k=1,nbbody(n)
          if(zoneor(k,n).eq.'OR'.or.zoneor(k,n).eq.'or') then
            iorchk=iorchk-1
            if(imachf.eq.1.and.imach.eq.1) then
              irl=n
              goto 900
            else
              imachf=0
              imach=1
            end if
          end if
c
          m=nbzone(k,n)
          nozone=abs(nbzone(k,n))
          jty=itblty(nozone)
          kno=itblno(nozone)
          iio=iinout(nozone)
          if(iio.lt.0.and.m.gt.0) then
            imach=0
            imachf=1
          elseif(iio.gt.0.and.m.lt.0) then
            imach=0
            imachf=1
          else
c     rpp check
            if(jty.eq.ityknd(1)) then
              if(kno.ge.1.and.kno.le.irppin) then
                if(xl.lt.rpppnt(1,kno).or.xl.gt.rpppnt(2,kno).or.
     &             yl.lt.rpppnt(3,kno).or.yl.gt.rpppnt(4,kno).or.
     &             zl.lt.rpppnt(5,kno).or.zl.gt.rpppnt(6,kno)) then
                  iinout(nozone)=-1
                  if(m.gt.0) then
                    imach=0
                  end if
                else
                  iinout(nozone)=1
                  if(m.lt.0) then
                    imach=0
                  end if
                end if
                imachf=1
              end if
c     sph check
            elseif(jty.eq.ityknd(2)) then
              if(kno.ge.1.and.kno.le.isphin) then
                if((xl-sphpnt(1,kno))*(xl-sphpnt(1,kno))
     &            +(yl-sphpnt(2,kno))*(yl-sphpnt(2,kno))
     &            +(zl-sphpnt(3,kno))*(zl-sphpnt(3,kno)) .gt.
     &              sphpnt(4,kno)*sphpnt(4,kno)) then
                  iinout(nozone)=-1
                  if(m.gt.0) then
                    imach=0
                  end if
                else
                  iinout(nozone)=1
                  if(m.lt.0) then
                    imach=0
                  end if
                end if
                imachf=1
              end if
c     rcc check
            elseif(jty.eq.ityknd(3)) then
              if(kno.ge.1.and.kno.le.irccin) then
                acyl=dsqrt(rccpnt(4,kno)*rccpnt(4,kno)
     &                    +rccpnt(5,kno)*rccpnt(5,kno)
     &                    +rccpnt(6,kno)*rccpnt(6,kno))
                bcyl=((xl-rccpnt(1,kno))*rccpnt(4,kno)
     &              + (yl-rccpnt(2,kno))*rccpnt(5,kno)
     &              + (zl-rccpnt(3,kno))*rccpnt(6,kno))/acyl
                ccyl=(xl-rccpnt(1,kno))*(xl-rccpnt(1,kno))
     &              +(yl-rccpnt(2,kno))*(yl-rccpnt(2,kno))
     &              +(zl-rccpnt(3,kno))*(zl-rccpnt(3,kno))
                clong1=dsqrt(dabs(ccyl-bcyl*bcyl))
                if(clong1.gt.rccpnt(7,kno).or.
     &               bcyl.lt.0.0d0.or.bcyl.gt.acyl) then
                  iinout(nozone)=-1
                  if(m.gt.0) then
                    imach=0
                  end if
                else
                  iinout(nozone)=1
                  if(m.lt.0) then
                    imach=0
                  end if
               end if
               imachf=1
              end if
c     trc check
            elseif(jty.eq.ityknd(4)) then
              if(kno.ge.1.and.kno.le.itrcin) then
                acyl=dsqrt(trcpnt(4,kno)*trcpnt(4,kno)
     &                    +trcpnt(5,kno)*trcpnt(5,kno)
     &                    +trcpnt(6,kno)*trcpnt(6,kno))
                bcyl=((xl-trcpnt(1,kno))*trcpnt(4,kno)
     &              + (yl-trcpnt(2,kno))*trcpnt(5,kno)
     &              + (zl-trcpnt(3,kno))*trcpnt(6,kno))/acyl
                ccyl=(xl-trcpnt(1,kno))*(xl-trcpnt(1,kno))
     &              +(yl-trcpnt(2,kno))*(yl-trcpnt(2,kno))
     &              +(zl-trcpnt(3,kno))*(zl-trcpnt(3,kno))
                clong1=dsqrt(dabs(ccyl-bcyl*bcyl))
                if(clong1.gt.(trcpnt(8,kno)-trcpnt(7,kno))/acyl*bcyl
     &          +trcpnt(7,kno).or.bcyl.lt.0.0d0.or.bcyl.gt.acyl) then
                  iinout(nozone)=-1
                  if(m.gt.0) then
                    imach=0
                  end if
                else
                  iinout(nozone)=1
                  if(m.lt.0) then
                    imach=0
                  end if
                end if
                imachf=1
              end if
c     tor check
            elseif(jty.eq.ityknd(5)) then
              if(kno.ge.1.and.kno.le.itorin) then
                if(nint(torpnt(8,kno)).eq.1) then
                  acyl=(yl-torpnt(2,kno))*(yl-torpnt(2,kno))
     &                +(zl-torpnt(3,kno))*(zl-torpnt(3,kno))
                  bcyl=xl-torpnt(1,kno)
                  if(yl-torpnt(2,kno).eq.0.0d0.and.
     &                 zl-torpnt(3,kno).eq.0.0d0) then
                    thator=0.0d0
                  else
                    thator=atan2(zl-torpnt(3,kno),yl-torpnt(2,kno))
                  end if
                end if
                if(nint(torpnt(8,kno)).eq.2) then
                  acyl=(zl-torpnt(3,kno))*(zl-torpnt(3,kno))
     &                +(xl-torpnt(1,kno))*(xl-torpnt(1,kno))
                  bcyl=yl-torpnt(2,kno)
                  if(zl-torpnt(3,kno).eq.0.0d0.and.
     &                 xl-torpnt(1,kno).eq.0.0d0) then
                    thator=0.0d0
                  else
                    thator=atan2(xl-torpnt(1,kno),zl-torpnt(3,kno))
                  end if
                end if
                if(nint(torpnt(8,kno)).eq.3) then
                  acyl=(xl-torpnt(1,kno))*(xl-torpnt(1,kno))
     &                +(yl-torpnt(2,kno))*(yl-torpnt(2,kno))
                  bcyl=zl-torpnt(3,kno)
                  if(yl-torpnt(2,kno).eq.0.0d0.and.
     &                 xl-torpnt(1,kno).eq.0.0d0) then
                    thator=0.0d0
                  else
                    thator=atan2(yl-torpnt(2,kno),xl-torpnt(1,kno))
                  end if
                end if
                thator=thator*180.0d0/PI
                thstor=torpnt(6,kno)
                thetor=torpnt(7,kno)
                if(thator.lt.0.0d0) then
                  thator=thator+360.0d0
                end if
                if(thstor.lt.0.0d0) then
                  thstor=thstor+360.0d0
                end if
                if(thetor.lt.0.0d0) then
                  thetor=thetor+360.0d0
                end if
                ithafg=0
                if(thstor.eq.thetor) then
                  ithafg=1
                else
                  if(thetor.gt.thstor) then
                    if(thator.ge.thstor.and.thator.le.thetor) then
                      ithafg=1
                    end if
                  else
                    if(thator.ge.thstor.or.thator.le.thetor) then
                      ithafg=1
                    end if
                  end if
                end if
                ccyl=dsqrt(acyl)-torpnt(4,kno)
                clong1=dsqrt(ccyl*ccyl+bcyl*bcyl)
                if(clong1.gt.torpnt(5,kno).or.ithafg.eq.0) then
                  iinout(nozone)=-1
                  if(m.gt.0) then
                    imach=0
                  end if
                else
                  iinout(nozone)=1
                  if(m.lt.0) then
                    imach=0
                  end if
                end if
                imachf=1
              end if
c     rec check
            elseif(jty.eq.ityknd(6)) then
              if(kno.ge.1.and.kno.le.irecin) then
                hh =recpnt(13,kno)
                rs2=recpnt(14,kno)
                rl2=recpnt(15,kno)
                rs4=rs2*rs2
                rl4=rl2*rl2
c
c5    COMPUTE (V-XB) FOR X,Y,Z COORDINATES
c
                plx=recpnt(1,kno)-xl
                ply=recpnt(2,kno)-yl
                plz=recpnt(3,kno)-zl
c
c6    TRANSFORM XL,YL,ZL TO THE COORDINATES OF THE REC
c
                vph=plx*recpnt( 4,kno)+ply*recpnt( 5,kno)
     &             +plz*recpnt( 6,kno)
                vrs=plx*recpnt( 7,kno)+ply*recpnt( 8,kno)
     &             +plz*recpnt( 9,kno)
                vrl=plx*recpnt(10,kno)+ply*recpnt(11,kno)
     &             +plz*recpnt(12,kno)
                um=rl4*vrs*vrs+rs4*vrl*vrl-rs4*rl4
                if(-vph.lt.0.0d0.OR.-vph.gt.hh.or.um.gt.0.0d0) then
                  iinout(nozone)=-1
                  if(m.gt.0) then
                    imach=0
                  end if
                else
                  iinout(nozone)=1
                  if(m.lt.0) then
                    imach=0
                  end if
                end if
                imachf=1
              end if
c     ell check
            elseif(jty.eq.ityknd(7)) then
              if(kno.ge.1.and.kno.le.iellin) then
                b1=(xl-ellpnt(1,kno))*(xl-ellpnt(1,kno))
     &            +(yl-ellpnt(2,kno))*(yl-ellpnt(2,kno))
     &            +(zl-ellpnt(3,kno))*(zl-ellpnt(3,kno))
                b2=(xl-ellpnt(4,kno))*(xl-ellpnt(4,kno))
     &            +(yl-ellpnt(5,kno))*(yl-ellpnt(5,kno))
     &            +(zl-ellpnt(6,kno))*(zl-ellpnt(6,kno))
                if(dsqrt(b1)+dsqrt(b2).gt.ellpnt(7,kno)) then
                  iinout(nozone)=-1
                  if(m.gt.0) then
                    imach=0
                  end if
                else
                  iinout(nozone)=1
                  if(m.lt.0) then
                    imach=0
                  end if
                end if
                imachf=1
              end if
c     wed check
            elseif(jty.eq.ityknd(8)) then
              if(kno.ge.1.and.kno.le.iwedin) then
                ifgin=1
                do i=1,3
                  ii=3*i+1
                  ap(i)=(xl-wedpnt(1,kno))*wedpnt(ii,kno)
     &                 +(yl-wedpnt(2,kno))*wedpnt(ii+1,kno)
     &                 +(zl-wedpnt(3,kno))*wedpnt(ii+2,kno)
                  if(ap(i).lt.0.0.or.ap(i).gt.wedpnt(i+12,kno)) then
                    ifgin=0
                    go to 810
                  endif
                end do
                ap4=ap(1)*wedpnt(14,kno)+ap(2)*wedpnt(13,kno)
                top=wedpnt(13,kno)*wedpnt(14,kno)-ap4
                if(top.lt.0.0) then
                  ifgin=0
                endif
c
  810           continue
                if(ifgin.eq.0) then
                  iinout(nozone)=-1
                  if(m.gt.0) then
                    imach=0
                  end if
                else
                  iinout(nozone)=1
                  if(m.lt.0) then
                    imach=0
                  end if
                end if
                imachf=1
              end if
c     box check
            elseif(jty.eq.ityknd(9)) then
              if(kno.ge.1.and.kno.le.iboxin) then
                ifgin=1
                do i=1,3
                  ii=3*i+1
                  ap(i)=(xl-boxpnt(1,kno))*boxpnt(ii,kno)
     &                 +(yl-boxpnt(2,kno))*boxpnt(ii+1,kno)
     &                 +(zl-boxpnt(3,kno))*boxpnt(ii+2,kno)
c
                  if(ap(i).lt.0.0d0.or.ap(i).gt.boxpnt(i+12,kno)) then
                    ifgin=0
                    go to 910
                  endif
                end do
c
  910           continue
                if(ifgin.eq.0) then
                  iinout(nozone)=-1
                  if(m.gt.0) then
                    imach=0
                  end if
                else
                  iinout(nozone)=1
                  if(m.lt.0) then
                    imach=0
                  end if
                end if
                imachf=1
              end if
c     arb check
            elseif(jty.eq.ityknd(10)) then
              if(kno.ge.1.and.kno.le.iarbin) then
                ifgin=1
                nside=arbtbl(26,kno)
                do i=1,nside
                  ii=(i-1)*4+1
                  dx=arbtbl(ii  ,kno)*xl+arbtbl(ii+1,kno)*yl
     &              +arbtbl(ii+2,kno)*zl+arbtbl(ii+3,kno)
                  if( dx.lt.0.0d0) then
                    ifgin=0
                    goto 1010
                  endif
                end do
 1010           continue
                if(ifgin.eq.0) then
                  iinout(nozone)=-1
                  if(m.gt.0) then
                    imach=0
                  end if
                else
                  iinout(nozone)=1
                  if(m.lt.0) then
                    imach=0
                  end if
                end if
                imachf=1
              end if
c     hex check
            elseif(jty.eq.ityknd(11)) then
              if(kno.ge.1.and.kno.le.ihexin) then
                ifgin=1
                do i=1,4
                  ii=3*i
                  ap(i)=(xl-hexpnt(ii+12,kno))*hexpnt(ii+24,kno)
     &                 +(yl-hexpnt(ii+13,kno))*hexpnt(ii+25,kno)
     &                 +(zl-hexpnt(ii+14,kno))*hexpnt(ii+26,kno)
c
                  if(ap(i).lt.0.0d0.or.ap(i).gt.hexpnt(i+10,kno)) then
                    ifgin=0
                    go to 1110
                  endif
                end do
 1110           continue
                if(ifgin.eq.0) then
                  iinout(nozone)=-1
                  if(m.gt.0) then
                    imach=0
                  end if
                else
                  iinout(nozone)=1
                  if(m.lt.0) then
                    imach=0
                  end if
                end if
                imachf=1
              end if
c     haf check
            elseif(jty.eq.ityknd(12)) then
              if(kno.ge.1.and.kno.le.ihafin) then
                if((xl*hafpnt(1,kno)
     &              +yl*hafpnt(2,kno)
     &              +zl*hafpnt(3,kno)).lt.
     &              hafpnt(4,kno)*hafpnt(5,kno)) then
                  iinout(nozone)=-1
                  if(m.gt.0) then
                    imach=0
                  end if
                else
                  iinout(nozone)=1
                  if(m.lt.0) then
                    imach=0
                  end if
                end if
                imachf=1
              end if
c     tec check
            elseif(jty.eq.ityknd(13)) then
              if(kno.ge.1.and.kno.le.itecin) then
                hh =tecpnt(14,kno)
                rs2=tecpnt(15,kno)
                rl2=tecpnt(16,kno)
                rs4=rs2*rs2
                rl4=rl2*rl2
                plx=(xl-tecpnt(1,kno))
                ply=(yl-tecpnt(2,kno))
                plz=(zl-tecpnt(3,kno))
                vph=plx*tecpnt( 4,kno)+ply*tecpnt( 5,kno)
     &             +plz*tecpnt( 6,kno)
                vrs=plx*tecpnt( 7,kno)+ply*tecpnt( 8,kno)
     &             +plz*tecpnt( 9,kno)
                vrl=plx*tecpnt(10,kno)+ply*tecpnt(11,kno)
     &             +plz*tecpnt(12,kno)
                rrf =1.0d0-(1.0d0-tecpnt(13,kno))*vph/hh
                rrf2=rrf*rrf
                um=rl4*vrs*vrs+rs4*vrl*vrl-rs4*rl4*rrf2
                IF(vph.lt.0.0d0.or.vph.gt.hh.or.um.gt.0.0d0) then
                  iinout(nozone)=-1
                  if(m.gt.0) then
                    imach=0
                  end if
                else
                  iinout(nozone)=1
                  if(m.lt.0) then
                    imach=0
                  end if
                end if
                imachf=1
              end if
c     gel check
            elseif(jty.eq.ityknd(14)) then
              if(kno.ge.1.and.kno.le.igelin) then
                b1=((xl-gelpnt(1,kno))*gelpnt(4,kno)
     &             +(yl-gelpnt(2,kno))*gelpnt(5,kno)
     &             +(zl-gelpnt(3,kno))*gelpnt(6,kno))
     &             /gelpnt(13,kno)/gelpnt(13,kno)
                b2=((xl-gelpnt(1,kno))*gelpnt(7,kno)
     &             +(yl-gelpnt(2,kno))*gelpnt(8,kno)
     &             +(zl-gelpnt(3,kno))*gelpnt(9,kno))
     &             /gelpnt(14,kno)/gelpnt(14,kno)
                b3=((xl-gelpnt(1,kno))*gelpnt(10,kno)
     &             +(yl-gelpnt(2,kno))*gelpnt(11,kno)
     &             +(zl-gelpnt(3,kno))*gelpnt(12,kno))
     &             /gelpnt(15,kno)/gelpnt(15,kno)
                if((b1*b1+b2*b2+b3*b3).gt.1.0d0) then
                  iinout(nozone)=-1
                  if(m.gt.0) then
                    imach=0
                  end if
                else
                  iinout(nozone)=1
                  if(m.lt.0) then
                    imach=0
                  end if
                end if
                imachf=1
              end if
c
c**** added new geometry
c
            end if
          end if
          if(imachf.eq.1.and.imach.eq.0.and.iorchk.eq.0) then
            goto 800
          endif
        end do
        if(imachf.eq.1.and.imach.eq.1) then
          irl=n
          goto 900
        end if
  800   continue
      end do
c
  900 continue
      return
      end
!--------------------last line of subroutine srzone.f-------------------
                                                                        
!-------------------------------trccg1.f--------------------------------
! Version: 051219-1435
! Reference: JNC TN1410 2002-201
!-----------------------------------------------------------------------
!23456789|123456789|123456789|123456789|123456789|123456789|123456789|12

! ----------------------------------------------------------------------
! Required (geometry) subroutine for use with the EGS5 Code System
! ----------------------------------------------------------------------
! This is a subroutine trccg1. 
! ----------------------------------------------------------------------
      subroutine trccg1(izon,xl,yl,zl,unp,vnp,wnp)
      implicit none
c
      include 'auxcommons/geom_common.f' ! geom-common file
c
      double precision xl,yl,zl
      double precision unp,vnp,wnp
      integer izon
      double precision udotr
      double precision hhtrc,xyztrc,uvwtrc,btrcn,ctrcn
      double precision clong2,rootcy
      double precision ddtrc,dwtrc,tbtrc,rrtrc,c2trc,bmtrc,a2trc,batrc
c
      hhtrc=dsqrt(trcpnt(4,izon)*trcpnt(4,izon)
     &           +trcpnt(5,izon)*trcpnt(5,izon)
     &           +trcpnt(6,izon)*trcpnt(6,izon))
      xyztrc=((xl-trcpnt(1,izon))*trcpnt(4,izon)
     &      + (yl-trcpnt(2,izon))*trcpnt(5,izon)
     &      + (zl-trcpnt(3,izon))*trcpnt(6,izon))/hhtrc
      uvwtrc=(trcpnt(4,izon)*unp+trcpnt(5,izon)*vnp
     &      + trcpnt(6,izon)*wnp)/hhtrc
      if(uvwtrc.lt.0.0d0) then
        if(xyztrc.ge.0.0d0) then
          udotr=-xyztrc/uvwtrc
          btrcn=((xl+udotr*unp-trcpnt(1,izon))*trcpnt(4,izon)
     &          +(yl+udotr*vnp-trcpnt(2,izon))*trcpnt(5,izon)
     &          +(zl+udotr*wnp-trcpnt(3,izon))*trcpnt(6,izon))/hhtrc
          ctrcn=(xl+udotr*unp-trcpnt(1,izon))
     &         *(xl+udotr*unp-trcpnt(1,izon))
     &         +(yl+udotr*vnp-trcpnt(2,izon))
     &         *(yl+udotr*vnp-trcpnt(2,izon))
     &         +(zl+udotr*wnp-trcpnt(3,izon))
     &         *(zl+udotr*wnp-trcpnt(3,izon))
          clong2=dabs(ctrcn-btrcn*btrcn)
          if(clong2.le.(trcpnt(7,izon)+trceps)*(1.0d0+trceps)
     &                *(trcpnt(7,izon)+trceps)*(1.0d0+trceps) ) then
            itvalm=itvalm+1
            atval(itvalm)=udotr
          end if
        end if
        if ((xyztrc.ge.hhtrc)) then
          udotr=(hhtrc-xyztrc)/uvwtrc
          btrcn=((xl+udotr*unp-trcpnt(1,izon))*trcpnt(4,izon)
     &          +(yl+udotr*vnp-trcpnt(2,izon))*trcpnt(5,izon)
     &          +(zl+udotr*wnp-trcpnt(3,izon))*trcpnt(6,izon))/hhtrc
          ctrcn=(xl+udotr*unp-trcpnt(1,izon))
     &         *(xl+udotr*unp-trcpnt(1,izon))
     &         +(yl+udotr*vnp-trcpnt(2,izon))
     &         *(yl+udotr*vnp-trcpnt(2,izon))
     &         +(zl+udotr*wnp-trcpnt(3,izon))
     &         *(zl+udotr*wnp-trcpnt(3,izon))
          clong2=dabs(ctrcn-btrcn*btrcn)
          if(clong2.le.(trcpnt(8,izon)+trceps)*(1.0d0+trceps)
     &                *(trcpnt(8,izon)+trceps)*(1.0d0+trceps) ) then
            itvalm=itvalm+1
            atval(itvalm)=udotr
          end if
        end if
      end if
      if(uvwtrc.gt.0.0d0) then
        if(xyztrc.le.0.0d0) then
          udotr=-xyztrc/uvwtrc
          btrcn=((xl+udotr*unp-trcpnt(1,izon))*trcpnt(4,izon)
     &          +(yl+udotr*vnp-trcpnt(2,izon))*trcpnt(5,izon)
     &          +(zl+udotr*wnp-trcpnt(3,izon))*trcpnt(6,izon))/hhtrc
          ctrcn=(xl+udotr*unp-trcpnt(1,izon))
     &         *(xl+udotr*unp-trcpnt(1,izon))
     &         +(yl+udotr*vnp-trcpnt(2,izon))
     &         *(yl+udotr*vnp-trcpnt(2,izon))
     &         +(zl+udotr*wnp-trcpnt(3,izon))
     &         *(zl+udotr*wnp-trcpnt(3,izon))
          clong2=dabs(ctrcn-btrcn*btrcn)
          if(clong2.le.(trcpnt(7,izon)+trceps)*(1.0d0+trceps)
     &                *(trcpnt(7,izon)+trceps)*(1.0d0+trceps) ) then
            itvalm=itvalm+1
            atval(itvalm)=udotr
          end if
        end if
        if(xyztrc.le.hhtrc) then
          udotr=(hhtrc-xyztrc)/uvwtrc
          btrcn=((xl+udotr*unp-trcpnt(1,izon))*trcpnt(4,izon)
     &          +(yl+udotr*vnp-trcpnt(2,izon))*trcpnt(5,izon)
     &          +(zl+udotr*wnp-trcpnt(3,izon))*trcpnt(6,izon))/hhtrc
          ctrcn=(xl+udotr*unp-trcpnt(1,izon))
     &         *(xl+udotr*unp-trcpnt(1,izon))
     &         +(yl+udotr*vnp-trcpnt(2,izon))
     &         *(yl+udotr*vnp-trcpnt(2,izon))
     &         +(zl+udotr*wnp-trcpnt(3,izon))
     &         *(zl+udotr*wnp-trcpnt(3,izon))
          clong2=dabs(ctrcn-btrcn*btrcn)
          if(clong2.le.(trcpnt(8,izon)+trceps)*(1.0d0+trceps)
     &                *(trcpnt(8,izon)+trceps)*(1.0d0+trceps) ) then
            itvalm=itvalm+1
            atval(itvalm)=udotr
          end if
        end if
      end if
      ddtrc=(xl-trcpnt(1,izon))*(xl-trcpnt(1,izon))
     &     +(yl-trcpnt(2,izon))*(yl-trcpnt(2,izon))
     &     +(zl-trcpnt(3,izon))*(zl-trcpnt(3,izon))
      dwtrc=-((xl-trcpnt(1,izon))*unp+(yl-trcpnt(2,izon))*vnp
     &       +(zl-trcpnt(3,izon))*wnp)
      tbtrc=(trcpnt(8,izon)-trcpnt(7,izon))/hhtrc
      rrtrc=trcpnt(7,izon)+xyztrc*tbtrc
      c2trc=ddtrc-rrtrc*rrtrc-xyztrc*xyztrc
      bmtrc=dwtrc+uvwtrc*(xyztrc+tbtrc*rrtrc)
      a2trc=1.0d0-uvwtrc*uvwtrc*(1.0d0+tbtrc*tbtrc)
      if(a2trc.eq.0.0d0) then
        udotr=c2trc/(2.0d0*bmtrc)
        if(udotr.ge.0.0d0) then
          btrcn=((xl+udotr*unp-trcpnt(1,izon))*trcpnt(4,izon)
     &          +(yl+udotr*vnp-trcpnt(2,izon))*trcpnt(5,izon)
     &          +(zl+udotr*wnp-trcpnt(3,izon))*trcpnt(6,izon))/hhtrc
          if(btrcn.ge.-trceps.and.
     &       btrcn.le.(hhtrc+trceps)*(1.0d0+trceps)) then
            itvalm=itvalm+1
            atval(itvalm)=udotr
          end if
        end if
      end if
      if(a2trc.ne.0.0d0) then
        batrc=bmtrc/a2trc
        rootcy=batrc*batrc-c2trc/a2trc
        if(rootcy.ge.0.0d0) then
          udotr=batrc-dsqrt(rootcy)
          if(udotr.ge.0.0d0) then
            btrcn=((xl+udotr*unp-trcpnt(1,izon))*trcpnt(4,izon)
     &            +(yl+udotr*vnp-trcpnt(2,izon))*trcpnt(5,izon)
     &            +(zl+udotr*wnp-trcpnt(3,izon))*trcpnt(6,izon))/hhtrc
            if(btrcn.ge.-trceps.and.
     &         btrcn.le.(hhtrc+trceps)*(1.0d0+trceps)) then
              itvalm=itvalm+1
              atval(itvalm)=udotr
            end if
          end if
          udotr=batrc+dsqrt(rootcy)
          if(udotr.ge.0.0d0) then
            btrcn=((xl+udotr*unp-trcpnt(1,izon))*trcpnt(4,izon)
     &            +(yl+udotr*vnp-trcpnt(2,izon))*trcpnt(5,izon)
     &            +(zl+udotr*wnp-trcpnt(3,izon))*trcpnt(6,izon))/hhtrc
            if(btrcn.ge.-trceps.and.
     &         btrcn.le.(hhtrc+trceps)*(1.0d0+trceps)) then
              itvalm=itvalm+1
              atval(itvalm)=udotr
            end if
          end if
        end if
      end if
      return
      end 
!--------------------last line of subroutine trccg1.f-------------------
                                                                        
!-------------------------------torcg1.f--------------------------------
! Version: 051219-1435
! Reference: JNC TN1410 2002-201
!-----------------------------------------------------------------------
!23456789|123456789|123456789|123456789|123456789|123456789|123456789|12

! ----------------------------------------------------------------------
! Required (geometry) subroutine for use with the EGS5 Code System
! ----------------------------------------------------------------------
! This is a subroutine torcg1. 
! ----------------------------------------------------------------------
      subroutine torcg1(izon,xl,yl,zl,unp,vnp,wnp)
      implicit none
c
      include 'auxcommons/dataconst_common.f' ! dataconst-common file
      include 'auxcommons/geom_common.f' ! geom-common file
c
      double precision xl,yl,zl
      double precision unp,vnp,wnp
      double precision xyztor,xxtor,yytor,zztor,uvwtor,a(5)
      integer izon
      double precision uxvywz,sstor
      double precision x1,x2,x3,x4,clonga(4)
      double precision thator,thstor,thetor
      double precision xlnew,ylnew,zlnew,clong1
      double precision torll1,torll2,torll3,ator,btor,
     &                 utora1,utora2,utora3
      double precision etor1,ftor1,gtor1,etor2,ftor2,gtor2,rtor1
      integer i,ifg
c
      xxtor=xl-torpnt(1,izon)
      yytor=yl-torpnt(2,izon)
      zztor=zl-torpnt(3,izon)
      if(nint(torpnt(8,izon)).eq.1) then
        uvwtor=unp
        xyztor=xxtor
      end if 
      if(nint(torpnt(8,izon)).eq.2) then
        uvwtor=vnp
        xyztor=yytor
      end if
      if(nint(torpnt(8,izon)).eq.3) then
        uvwtor=wnp
        xyztor=zztor
      end if
      uxvywz=unp*xxtor+vnp*yytor+wnp*zztor
      sstor=xxtor*xxtor+yytor*yytor+zztor*zztor
     &     -torpnt(5,izon)*torpnt(5,izon)-torpnt(4,izon)*torpnt(4,izon)
      a(1)=1.0d0
      a(2)=4.0d0*uxvywz
      a(3)=4.0d0*uxvywz*uxvywz+2.0d0*sstor
     &    +4.0d0*torpnt(4,izon)*torpnt(4,izon)*uvwtor*uvwtor
      a(4)=4.0d0*uxvywz*sstor
     &    +8.0d0*torpnt(4,izon)*torpnt(4,izon)*uvwtor*xyztor
      a(5)=sstor*sstor+4.0d0*torpnt(4,izon)*torpnt(4,izon)
     &    *(xyztor*xyztor-torpnt(5,izon)*torpnt(5,izon))
      call ferra(a,x1,x2,x3,x4,ifg)
      clonga(1)=x1
      clonga(2)=x2
      clonga(3)=x3
      clonga(4)=x4
      thstor=torpnt(6,izon)
      thetor=torpnt(7,izon)
      if(thstor.gt.180.d0) then
        thstor=thstor-360.0d0
      end if
      if(thetor.gt.180.0d0) then
        thetor=thetor-360.0d0
      end if
      thstor=thstor/180.0d0*PI
      thetor=thetor/180.0d0*PI
      do i=1,ifg
        if(clonga(i).ge.0.0d0) then
          xlnew=xl+clonga(i)*unp-torpnt(1,izon)
          ylnew=yl+clonga(i)*vnp-torpnt(2,izon)
          zlnew=zl+clonga(i)*wnp-torpnt(3,izon)
          if(nint(torpnt(8,izon)).eq.1) then
            if(zlnew.eq.0.0d0.and.ylnew.eq.0.0d0) then
              thator=0.0d0
            else
              thator=atan2(zlnew,ylnew)
            end if
          end if
          if(nint(torpnt(8,izon)).eq.2) then
            if(xlnew.eq.0.0d0.and.zlnew.eq.0.0d0) then
              thator=0.0d0
            else
              thator=atan2(xlnew,zlnew)
            end if
          end if
          if(nint(torpnt(8,izon)).eq.3) then
            if(ylnew.eq.0.0d0.and.xlnew.eq.0.0d0) then
              thator=0.0d0
            else
              thator=atan2(ylnew,xlnew)
            end if
          end if
          if(thetor.eq.thstor) then
            itvalm=itvalm+1
            atval(itvalm)=clonga(i)
          end if
          if(thetor.gt.thstor) then
            if ((thator.ge.thstor.and.thator.le.thetor)) then
              itvalm=itvalm+1
              atval(itvalm)=clonga(i)
            end if
          end if
          if(thetor.lt.thstor) then
            if ((thator.ge.thstor.or.thator.le.thetor)) then
              itvalm=itvalm+1
              atval(itvalm)=clonga(i)
            end if
          end if
        end if
      end do
      if(thetor.ne.thstor) then
        if(nint(torpnt(8,izon)).eq.1) then
          torll1=xl-torpnt(1,izon)
          torll2=yl-torpnt(2,izon)
          torll3=zl-torpnt(3,izon)
          utora1=unp
          utora2=vnp
          utora3=wnp
        end if
        if(nint(torpnt(8,izon)).eq.2) then
          torll1=yl-torpnt(2,izon)
          torll2=zl-torpnt(3,izon)
          torll3=xl-torpnt(1,izon)
          utora1=vnp
          utora2=wnp
          utora3=unp
        end if
        if(nint(torpnt(8,izon)).eq.3) then
          torll1=zl-torpnt(3,izon)
          torll2=xl-torpnt(1,izon)
          torll3=yl-torpnt(2,izon)
          utora1=wnp
          utora2=unp
          utora3=vnp
        end if
        ator=torll2*torll2+torll3*torll3
        btor=torll1*torll1
        if(torll2.eq.0.0d0.and.torll3.eq.0.0d0) then
          thator=0.0d0
        else
          thator=atan2(torll3,torll2)
        end if
        etor1=(torll2-torpnt(4,izon)*dcos(thstor))**2
     &   +(torll3-torpnt(4,izon)*dsin(thstor))**2 +torll1*torll1
        ftor1=(torll2-torpnt(4,izon)*dcos(thstor))*(-dsin(thstor))
     &       +(torll3-torpnt(4,izon)*dsin(thstor))*dcos(thstor)
        gtor1=utora2*(-dsin(thstor))+utora3*dcos(thstor)
        if((ftor1.gt.0.0d0.and.gtor1.lt.0.0d0).or.
     &     (ftor1.lt.0.0d0.and.gtor1.gt.0.0d0)) then
          clong1=-ftor1/gtor1
          rtor1=(torll2+utora2*clong1-torpnt(4,izon)*dcos(thstor))**2
     &         +(torll3+utora3*clong1-torpnt(4,izon)*dsin(thstor))**2
     &         +(torll1+utora1*clong1)*(torll1+utora1*clong1)
          if(rtor1.le.
     &      (torpnt(5,izon)*torpnt(5,izon)+toreps)*(1.0d0+toreps)) then
            itvalm=itvalm+1
            atval(itvalm)=clong1
          end if
        end if
        etor2=(torll2-torpnt(4,izon)*dcos(thetor))**2
     &       +(torll3-torpnt(4,izon)*dsin(thetor))**2 +torll1*torll1
        ftor2=(torll2-torpnt(4,izon)*dcos(thetor))*(-dsin(thetor))
     &       +(torll3-torpnt(4,izon)*dsin(thetor))*dcos(thetor)
        gtor2=utora2*(-dsin(thetor))+utora3*dcos(thetor)
        if((ftor2.gt.0.0d0.and.gtor2.lt.0.0d0).or.
     &     (ftor2.lt.0.0d0.and.gtor2.gt.0.0d0)) then
          clong1=-ftor2/gtor2
          rtor1=(torll2+utora2*clong1-torpnt(4,izon)*dcos(thetor))**2
     &         +(torll3+utora3*clong1-torpnt(4,izon)*dsin(thetor))**2
     &         +(torll1+utora1*clong1)*(torll1+utora1*clong1)
          if(rtor1.le.
     &      (torpnt(5,izon)*torpnt(5,izon)+toreps)*(1.0d0+toreps)) then
            itvalm=itvalm+1
            atval(itvalm)=clong1
          end if
        end if
      end if
      return
      end
!--------------------last line of subroutine torcg1.f-------------------
                                                                        
!-------------------------------block data cgtype-----------------------
! Version: 060117-0900
! Reference: JNC TN1410 2002-201
!-----------------------------------------------------------------------
!23456789|123456789|123456789|123456789|123456789|123456789|123456789|12

! ----------------------------------------------------------------------
! Required (geometry) subroutine for use with the EGS5 Code System
! ----------------------------------------------------------------------
! This is a block data cgtype.
! ----------------------------------------------------------------------
      block data cgtype
      include 'auxcommons/geom_common.f' ! geom-common file
      data ityknd/ 1, 2, 3, 4, 5, 6, 7, 8, 9,10,
     &            11,12,13,14/
      data cgmnst/1.0d-4/
      data cgeps1/1.0d-4/
      data cgeps2/1.0d-4/
      data rcceps/1.0d-4/
      data trceps/1.0d-4/
      data rppeps/1.0d-4/
      data spheps/1.0d-4/
      data toreps/1.0d-4/
      data elleps/1.0d-4/
      data arbeps/1.0d-4/
      data receps/1.0d-4/
      data wedeps/1.0d-4/
      data boxeps/1.0d-4/
      data hafeps/1.0d-4/
      data hexeps/1.0d-4/
      data teceps/1.0d-4/
      data geleps/1.0d-4/
      data itbody/0/
      data irppin/0/
      data isphin/0/
      data irccin/0/
      data itrcin/0/
      data itorin/0/
      data irecin/0/
      data iellin/0/
      data iwedin/0/
      data iboxin/0/
      data iarbin/0/
      data ihexin/0/
      data ihafin/0/
      data itecin/0/
      data igelin/0/
      data izonin/0/
      data itverr/0/
      data igmmax/0/
      end
!--------------------last line of block data cgtype---------------------
                                                                        
!-------------------------------stgeom.f--------------------------------
! Version: 051219-1435
! Reference: JNC TN1410 2002-201
!-----------------------------------------------------------------------
!23456789|123456789|123456789|123456789|123456789|123456789|123456789|12

! ----------------------------------------------------------------------
! Required (geometry) subroutine for use with the EGS5 Code System
! ----------------------------------------------------------------------
! This is a subroutine stgeom. 
! ----------------------------------------------------------------------
      subroutine stgeom(chkey,igmid,gmdata)
      implicit none
c
      include 'auxcommons/geom_common.f' ! geom-common file
c
      integer igmid
cssl  character chkey*3
      character chkey*(*)
      double precision gmdata(*)
      integer i
      if(chkey.eq.'RPP'.or.chkey.eq.'rpp')then
        itbody=itbody+1
        irppin=irppin+1
        if(irppin.gt.MAX_RPP) then
          write(*,*) 'Dimension over of RPP=',MAX_RPP
          stop
        endif
        nbrpp(irppin)=igmid
        irppuse(irppin)=0
        igmmax = max(igmmax,igmid)
        if(igmmax.gt.MAX_GEOM) then
          write(*,*) 'Dimension over of GEOM=',MAX_GEOM
          stop
        endif
        do i=1,6
          rpppnt(i,irppin)=gmdata(i)
        end do
        itblty(igmid)=ityknd(1)
        itblno(igmid)=irppin
        call rppset(irppin)
      elseif(chkey.eq.'SPH'.or.chkey.eq.'sph') then
        itbody=itbody+1
        isphin=isphin+1
        if(isphin.gt.MAX_SPH) then
          write(*,*) 'Dimension over of SPH=',MAX_SPH
          stop
        endif
        nbsph(isphin)=igmid
        isphuse(isphin)=0
        igmmax = max(igmmax,igmid)
        if(igmmax.gt.MAX_GEOM) then
          write(*,*) 'Dimension over of GEOM=',MAX_GEOM
          stop
        endif
        do i=1,4
          sphpnt(i,isphin)=gmdata(i)
        end do
        itblty(igmid)=ityknd(2)
        itblno(igmid)=isphin
      elseif(chkey.eq.'RCC'.or.chkey.eq.'rcc') then
        itbody=itbody+1
        irccin=irccin+1
        if(irccin.gt.MAX_RCC) then
          write(*,*) 'Dimension over of RCC=',MAX_RCC
          stop
        endif
        nbrcc(irccin)=igmid
        irccuse(irccin)=0
        igmmax = max(igmmax,igmid)
        if(igmmax.gt.MAX_GEOM) then
          write(*,*) 'Dimension over of GEOM=',MAX_GEOM
          stop
        endif
        do i=1,7
          rccpnt(i,irccin)=gmdata(i)
        end do
        itblty(igmid)=ityknd(3)
        itblno(igmid)=irccin
      elseif(chkey.eq.'TRC'.or.chkey.eq.'trc') then
        itbody=itbody+1
        itrcin=itrcin+1
        if(itrcin.gt.MAX_TRC) then
          write(*,*) 'Dimension over of TRC=',MAX_TRC
          stop
        endif
        nbtrc(itrcin)=igmid
        itrcuse(itrcin)=0
        igmmax = max(igmmax,igmid)
        if(igmmax.gt.MAX_GEOM) then
          write(*,*) 'Dimension over of GEOM=',MAX_GEOM
          stop
        endif
        do i=1,8
          trcpnt(i,itrcin)=gmdata(i)
        end do
        itblty(igmid)=ityknd(4)
        itblno(igmid)=itrcin
      elseif(chkey.eq.'TOR'.or.chkey.eq.'tor') then
        itbody=itbody+1
        itorin=itorin+1
        if(itorin.gt.MAX_TOR) then
          write(*,*) 'Dimension over of TOR=',MAX_TOR
          stop
        endif
        nbtor(itorin)=igmid
        itoruse(itorin)=0
        igmmax = max(igmmax,igmid)
        if(igmmax.gt.MAX_GEOM) then
          write(*,*) 'Dimension over of GEOM=',MAX_GEOM
          stop
        endif
        do i=1,8
          torpnt(i,itorin)=gmdata(i)
        end do
        itblty(igmid)=ityknd(5)
        itblno(igmid)=itorin
      elseif(chkey.eq.'REC'.or.chkey.eq.'rec') then
        itbody=itbody+1
        irecin=irecin+1
        if(irecin.gt.MAX_REC) then
          write(*,*) 'Dimension over of REC=',MAX_REC
          stop
        endif
        nbrec(irecin)=igmid
        irecuse(irecin)=0
        igmmax = max(igmmax,igmid)
        if(igmmax.gt.MAX_GEOM) then
          write(*,*) 'Dimension over of GEOM=',MAX_GEOM
          stop
        endif
        do i=1,12
          recpnt(i,irecin)=gmdata(i)
        end do
        itblty(igmid)=ityknd(6)
        itblno(igmid)=irecin
        call recset(irecin)
      elseif(chkey.eq.'ELL'.or.chkey.eq.'ell') then
        itbody=itbody+1
        iellin=iellin+1
        if(iellin.gt.MAX_ELL) then
          write(*,*) 'Dimension over of ELL=',MAX_ELL
          stop
        endif
        nbell(iellin)=igmid
        ielluse(iellin)=0
        igmmax = max(igmmax,igmid)
        if(igmmax.gt.MAX_GEOM) then
          write(*,*) 'Dimension over of GEOM=',MAX_GEOM
          stop
        endif
        do i=1,7
          ellpnt(i,iellin)=gmdata(i)
        end do
        itblty(igmid)=ityknd(7)
        itblno(igmid)=iellin
        call ellset(iellin)
      elseif(chkey.eq.'WED'.or.chkey.eq.'wed') then
        itbody=itbody+1
        iwedin=iwedin+1
        if(iwedin.gt.MAX_WED) then
          write(*,*) 'Dimension over of WED=',MAX_WED
          stop
        endif
        nbwed(iwedin)=igmid
        iweduse(iwedin)=0
        igmmax = max(igmmax,igmid)
        if(igmmax.gt.MAX_GEOM) then
          write(*,*) 'Dimension over of GEOM=',MAX_GEOM
          stop
        endif
        do i=1,12
          wedpnt(i,iwedin)=gmdata(i)
        end do
        itblty(igmid)=ityknd(8)
        itblno(igmid)=iwedin
        call wedset(iwedin)
      elseif(chkey.eq.'BOX'.or.chkey.eq.'box') then
        itbody=itbody+1
        iboxin=iboxin+1
        if(iboxin.gt.MAX_BOX) then
          write(*,*) 'Dimension over of BOX=',MAX_BOX
          stop
        endif
        nbbox(iboxin)=igmid
        iboxuse(iboxin)=0
        igmmax = max(igmmax,igmid)
        if(igmmax.gt.MAX_GEOM) then
          write(*,*) 'Dimension over of GEOM=',MAX_GEOM
          stop
        endif
        do i=1,12
          boxpnt(i,iboxin)=gmdata(i)
        end do
        itblty(igmid)=ityknd(9)
        itblno(igmid)=iboxin
        call boxset(iboxin)
      elseif(chkey.eq.'ARB'.or.chkey.eq.'arb') then
        itbody=itbody+1
        iarbin=iarbin+1
        if(iarbin.gt.MAX_ARB) then
          write(*,*) 'Dimension over of ARB=',MAX_ARB
          stop
        endif
        nbarb(iarbin)=igmid
        iarbuse(iarbin)=0
        igmmax = max(igmmax,igmid)
        if(igmmax.gt.MAX_GEOM) then
          write(*,*) 'Dimension over of GEOM=',MAX_GEOM
          stop
        endif
        do i=1,30
          arbpnt(i,iarbin)=gmdata(i)
        end do
        itblty(igmid)=ityknd(10)
        itblno(igmid)=iarbin
        call arbset(iarbin)
      elseif(chkey.eq.'HEX'.or.chkey.eq.'hex') then
        itbody=itbody+1
        ihexin=ihexin+1
        if(ihexin.gt.MAX_HEX) then
          write(*,*) 'Dimension over of HEX=',MAX_HEX
          stop
        endif
        nbhex(ihexin)=igmid
        ihexuse(ihexin)=0
        igmmax = max(igmmax,igmid)
        if(igmmax.gt.MAX_GEOM) then
          write(*,*) 'Dimension over of GEOM=',MAX_GEOM
          stop
        endif
        do i=1,10
          hexpnt(i,ihexin)=gmdata(i)
        end do
        itblty(igmid)=ityknd(11)
        itblno(igmid)=ihexin
        call hexset(ihexin)
      elseif(chkey.eq.'HAF'.or.chkey.eq.'haf') then
        itbody=itbody+1
        ihafin=ihafin+1
        if(ihafin.gt.MAX_HAF) then
          write(*,*) 'Dimension over of HAF=',MAX_HAF
          stop
        endif
        nbhaf(ihafin)=igmid
        ihafuse(ihafin)=0
        igmmax = max(igmmax,igmid)
        if(igmmax.gt.MAX_GEOM) then
          write(*,*) 'Dimension over of GEOM=',MAX_GEOM
          stop
        endif
        do i=1,4
          hafpnt(i,ihafin)=gmdata(i)
        end do
        itblty(igmid)=ityknd(12)
        itblno(igmid)=ihafin
        call hafset(ihafin)
      elseif(chkey.eq.'TEC'.or.chkey.eq.'tec') then
        itbody=itbody+1
        itecin=itecin+1
        if(itecin.gt.MAX_TEC) then
          write(*,*) 'Dimension over of TEC=',MAX_TEC
          stop
        endif
        nbtec(itecin)=igmid
        itecuse(itecin)=0
        igmmax = max(igmmax,igmid)
        if(igmmax.gt.MAX_GEOM) then
          write(*,*) 'Dimension over of GEOM=',MAX_GEOM
          stop
        endif
        do i=1,13
          tecpnt(i,itecin)=gmdata(i)
        end do
        itblty(igmid)=ityknd(13)
        itblno(igmid)=itecin
        call tecset(itecin)
      elseif(chkey.eq.'GEL'.or.chkey.eq.'gel') then
        itbody=itbody+1
        igelin=igelin+1
        if(igelin.gt.MAX_GEL) then
          write(*,*) 'Dimension over of GEL=',MAX_GEL
          stop
        endif
        nbgel(igelin)=igmid
        igeluse(igelin)=0
        igmmax = max(igmmax,igmid)
        if(igmmax.gt.MAX_GEOM) then
          write(*,*) 'Dimension over of GEOM=',MAX_GEOM
          stop
        endif
        do i=1,12
          gelpnt(i,igelin)=gmdata(i)
        end do
        itblty(igmid)=ityknd(14)
        itblno(igmid)=igelin
        call gelset(igelin)
      endif
      return
      end
!--------------------last line of subroutine stgeom.f------------------
                                                                        
!-------------------------------stzone.f--------------------------------
! Version: 051219-1435
! Reference: JNC TN1410 2002-201
!-----------------------------------------------------------------------
!23456789|123456789|123456789|123456789|123456789|123456789|123456789|12

! ----------------------------------------------------------------------
! Required (geometry) subroutine for use with the EGS5 Code System
! ----------------------------------------------------------------------
! This is a subroutine stzone. 
! ----------------------------------------------------------------------
      subroutine stzone(chkey,iznid,iznor,izndt,iprm)
      implicit none
c
      include 'auxcommons/geom_common.f' ! geom-common file
c
      integer iznid,iprm
cssl  character chkey*3
      character chkey*(*)
      character iznor(MAX_IZN)*2
      integer izndt(MAX_IZN)
      integer i,j
      izonin = izonin + 1
      if(izonin.gt.MAX_ZONE) then
        write(*,*) 'Dimension over of ZONE=',MAX_ZONE
        stop
      endif
      zoneid(izonin) = chkey
      nbbody(izonin) = iprm
      if(iprm.gt.MAX_BODY) then
        write(*,*) 'Dimension over of BODY=',MAX_BODY
        stop
      endif
      do i=1,iprm
        zoneor(i,izonin) = iznor(i)
        if(iznor(i).eq.'or'.or.iznor(i).eq.'OR') then
          j=j+1
        endif
        nbzone(i,izonin) = izndt(i)
        call zonegeom(abs(izndt(i)))
      end do
      iorcnt(izonin)=j
      return
      end
!--------------------last line of subroutine stzone.f-------------------

!-------------------------------zonegeom.f------------------------------
! Version: 051219-1435
! Reference: JNC TN1410 2002-201
!-----------------------------------------------------------------------
!23456789|123456789|123456789|123456789|123456789|123456789|123456789|12

! ----------------------------------------------------------------------
! Required (geometry) subroutine for use with the EGS5 Code System
! ----------------------------------------------------------------------
! This is a subroutine zonegeom
! ----------------------------------------------------------------------
      subroutine zonegeom(izndt)
      implicit none
c
      include 'auxcommons/geom_common.f' ! geom-common file
      integer izndt,i
c
      do i=1,irppin
        if(nbrpp(i).eq.izndt) then
          irppuse(i)=1
          goto 190
        endif
      end do
      do i=1,isphin
        if(nbsph(i).eq.izndt) then
          isphuse(i)=1
          goto 190
        endif
      end do
      do i=1,irccin
        if(nbrcc(i).eq.izndt) then
          irccuse(i)=1
          goto 190
        endif
      end do
      do i=1,itrcin
        if(nbtrc(i).eq.izndt) then
          itrcuse(i)=1
          goto 190
        endif
      end do
      do i=1,itorin
        if(nbtor(i).eq.izndt) then
          itoruse(i)=1
          goto 190
        endif
      end do
      do i=1,irecin
        if(nbrec(i).eq.izndt) then
          irecuse(i)=1
          goto 190
        endif
      end do
      do i=1,iellin
        if(nbell(i).eq.izndt) then
          ielluse(i)=1
          goto 190
        endif
      end do
      do i=1,iwedin
        if(nbwed(i).eq.izndt) then
          iweduse(i)=1
          goto 190
        endif
      end do
      do i=1,iboxin
        if(nbbox(i).eq.izndt) then
          iboxuse(i)=1
          goto 190
        endif
      end do
      do i=1,iarbin
        if(nbarb(i).eq.izndt) then
          iarbuse(i)=1
          goto 190
        endif
      end do
      do i=1,ihexin
        if(nbhex(i).eq.izndt) then
          ihexuse(i)=1
          goto 190
        endif
      end do
      do i=1,ihafin
        if(nbhaf(i).eq.izndt) then
          ihafuse(i)=1
          goto 190
        endif
      end do
      do i=1,itecin
        if(nbtec(i).eq.izndt) then
          itecuse(i)=1
          goto 190
        endif
      end do
      do i=1,igelin
        if(nbgel(i).eq.izndt) then
          igeluse(i)=1
          goto 190
        endif
      end do
  190 continue
      return
      end
!--------------------last line of subroutine zonegeom.f-----------------
                                                                        
!-------------------------------geomgt.f--------------------------------
! Version: 051219-1435
! Reference: JNC TN1410 2002-201
!-----------------------------------------------------------------------
!23456789|123456789|123456789|123456789|123456789|123456789|123456789|12

! ----------------------------------------------------------------------
! Required (geometry) subroutine for use with the EGS5 Code System
! ----------------------------------------------------------------------
! This is a subroutine geomgt. 
! ----------------------------------------------------------------------
      subroutine geomgt(ifti,ifto)
      implicit none
      include 'auxcommons/geom_common.f' ! geom-common file
      include 'include/egs5_h.f'
      integer ifti,ifto
cssl  character chkey*3
      character chkey*10
      double precision gmdata(50)
      character iznor(MAX_IZN)*2
      integer izndt(MAX_IZN)
      integer ioptid
      data ioptid/0/
      integer i,j,jps,jpe,iznid,igmid,iprm
      itbody=0
      irppin=0
      isphin=0
      irccin=0
      itorin=0
      itrcin=0
      irecin=0
      iellin=0
      iwedin=0
      iboxin=0
      iarbin=0
      ihexin=0
      ihafin=0
      itecin=0
      igelin=0
      izonin=0
      itverr=0
      igmmax=0
100   continue
      call freegm(ifti,ifto,chkey,igmid,gmdata,iprm)
      if((chkey.eq.'   '))then
        write(ifto,'(a)') ' geom data error : not found [ end ]'
        goto 900
      elseif(chkey.eq.'END'.or.chkey.eq.'end') then
        write(ifto,6000) chkey
6000  format(1h ,2x,a10,i10,1p8e12.4)
        goto 200
      else
        do i=1,iprm,8
          jps = i
          jpe = min(iprm,i+7)
          if(i.eq.1) then
            write(ifto,6000) chkey,igmid,(gmdata(j),j=jps,jpe)
          else
            write(ifto,6050) (gmdata(j),j=jps,jpe)
6050  format(1h ,22x,1p8e12.4)
          endif
        end do
        call stgeom(chkey,igmid,gmdata)
      endif
      goto 100
200   continue
      call freezn(ifti,ifto,chkey,iznid,iznor,izndt,iprm,ioptid,
     &            MAX_IZN)
      if((chkey.eq.'   '))then
        write(ifto,'(a)') ' zone data error : not found [ end ]'
        goto 900
      elseif(chkey.eq.'END'.or.chkey.eq.'end') then
        write(ifto,6000) chkey
        goto 300
      else
        do i=1,iprm,10
          jps = i
          jpe = min(iprm,i+9)
          if((i.eq.1))then
            if((ioptid.eq.0))then
              write(ifto,6200) chkey,(iznor(j),izndt(j),j=jps,jpe)
6200  format(1h ,2x,a10,5x,1x,10(a2,i8,1x))
            else
              write(ifto,6250) chkey,iznid,(iznor(j),izndt(j),
     &                                                     j=jps,jpe)
6250  format(1h ,2x,a10,i5,1x,10(a2,i8,1x))
            endif
          else
            write(ifto,6300) (iznor(j),izndt(j),j=jps,jpe)
6300  format(1h ,17x ,1x,10(a2,i8,1x))
          endif
        end do
        call stzone(chkey,iznid,iznor,izndt,iprm)
      endif
      goto 200
900   continue
      stop
300   continue
c
      call setnxt                      
c
      !  Check nreg=izonin value
      if (izonin.gt.mxreg) then
        write(1,140) izonin,mxreg 
140     FORMAT(' nreg(=',I12,') must be less than MXREG(=',I12,')' /
     *  ' You must change MXREG in include/egs5_h.f.')
        stop
      end if

      return
      end
!--------------------last line of subroutine geomgt.f------------------
                                                                        
!-------------------------------setnxt.f--------------------------------
! Version: 051219-1435
! Reference: JNC TN1410 2002-201
!-----------------------------------------------------------------------
!23456789|123456789|123456789|123456789|123456789|123456789|123456789|12

! ----------------------------------------------------------------------
! Required (geometry) subroutine for use with the EGS5 Code System
! ----------------------------------------------------------------------
! This is a subroutine setnxt.
! ----------------------------------------------------------------------
      subroutine setnxt
      implicit none
c
      include 'auxcommons/geom_common.f' ! geom-common file
      integer i,j,l,jj,l1,l2,ll
c
      l=0
      do i=1,3
        iznnxs(i,1)=0
        iznnxs(i,2)=0
        do j=1,izonin
          iznnxt(i,j,l)=j
          iznnxc(i,j,l)=0
        enddo
      enddo
      do i=1,3
        do j=1,izonin
          iznnxp(i,iznnxt(i,j,l),l)=j
        enddo
c       write(*,*) 'i,izonnxp=',i,(iznnxp(i,j,l),j=1,izonin)
      enddo
      do l=1,izonin
        l1=l-1
        l2=izonin-l
        ll=min(l1,l2)
        do j=1,izonin
          if(j.eq.l) then
c           jj=izonin
            jj=0
          elseif(iabs(j-l).gt.ll) then
            jj=iabs(j-l)+ll
          elseif(j.lt.l) then
           jj=2*(l-j)
          else
           jj=2*(j-l)-1
          endif
          do i=1,3
c           iznnxt(i,j,l)=jj
            iznnxt(i,j,l)=jj+1
            iznnxc(i,j,l)=0
          enddo
        enddo
      enddo
      do l=1,izonin
        do j=1,izonin
          do i=1,3
            iznnxp(i,iznnxt(i,j,l),l)=j
          enddo
        enddo
        do i=1,3
c         write(*,*) 'i,izonnxp=',i,(iznnxp(i,j,l),j=1,izonin)
        enddo
      enddo
      return
      end
!--------------------last line of subroutine setnxt.f------------------

!-------------------------------rstnxt.f--------------------------------
! Version: 051219-1435
! Reference: JNC TN1410 2002-201
!-----------------------------------------------------------------------
!23456789|123456789|123456789|123456789|123456789|123456789|123456789|12

! ----------------------------------------------------------------------
! Required (geometry) subroutine for use with the EGS5 Code System
! ----------------------------------------------------------------------
! This is a subroutine rstnxt.
! ----------------------------------------------------------------------
      subroutine rstnxt(iray,irold,irnew)                     
      implicit none
c
      include 'auxcommons/geom_common.f' ! geom-common file
      integer iray,irold,irnew
      integer i,j,ll,n,ns,ne,jj
      integer icknxt,ickmin,id,ik,k
      data icknxt/100000000/
      data ickmin/1000/
      integer itbl(max_zone),itbl1
c
   
      if(irold.eq.0) then
        ll=1
        ns=0
        ne=0
      else
        ll=2
        ns=1
        ne=izonin
      endif   
      if(iznnxs(iray,ll).lt.icknxt) then
        iznnxs(iray,ll)=iznnxs(iray,ll)+1
        iznnxc(iray,irnew,irold)=iznnxc(iray,irnew,irold)+1
        if(mod(iznnxs(iray,ll),ickmin).eq.0) then
          ik=0
          k=1
  100     continue
          id=k*ickmin
          if(iznnxs(iray,ll).eq.id) then
            ik=1
          elseif(id.lt.icknxt) then
            k=k*10
            goto 100
          endif
c
          if(ik.eq.1) then
            do n=ns,ne
              if(ll.eq.1) then
                do j=1,izonin
                  itbl(j)=j
                enddo
              else
                itbl(1)=n
                jj=0
                do j=2,izonin
                   jj=jj+1
                  if(jj.eq.n) then
                    jj=jj+1
                   endif
                   itbl(j)=jj
                enddo
              endif
              do i=1,izonin-1
                do j=i+1,izonin
                  if(ll.eq.1.or.i.ne.1) then
                    if(iznnxc(iray,itbl(j),n).gt.
     &                 iznnxc(iray,itbl(i),n)) then
                       itbl1=itbl(j)
                       itbl(j)=itbl(i)
                       itbl(i)=itbl1
                    elseif(iznnxc(iray,itbl(j),n).eq.
     &                     iznnxc(iray,itbl(i),n)) then
                      if(iznnxt(iray,itbl(j),n).lt.
     &                   iznnxt(iray,itbl(i),n)) then
                        itbl1=itbl(j)
                        itbl(j)=itbl(i)
                        itbl(i)=itbl1
                      endif
                    endif
                  endif
                enddo
              enddo
              do j=1,izonin
                iznnxt(iray,itbl(j),n)=j
              enddo
              do j=1,izonin
                iznnxp(iray,iznnxt(iray,j,n),n)=j
              enddo
c             write(*,*) 'iray,ir,iznnxt=',
c    &                    iray,n,(iznnxt(iray,j,n),j=1,izonin)
c             write(*,*) 'iray,ir,iznnxp=',
c    &                    iray,n,(iznnxp(iray,j,n),j=1,izonin)
c             write(*,*) 'iray,ir,iznnxc=',
c    &                    iray,n,(iznnxc(iray,j,n),j=1,izonin)
            enddo
          endif
        endif
      endif
      return
      end
!--------------------last line of subroutine rstnxt.f------------------
                                                                        
!-------------------------------getnxt.f--------------------------------
! Version: 051219-1435
! Reference: JNC TN1410 2002-201
!-----------------------------------------------------------------------
!23456789|123456789|123456789|123456789|123456789|123456789|123456789|12

! ----------------------------------------------------------------------
! Required (geometry) subroutine for use with the EGS5 Code System
! ----------------------------------------------------------------------
! This is a subroutine getnxt.
! ----------------------------------------------------------------------
      subroutine getnxt(iray,jj,irnow,inext)                     
      implicit none
      include 'auxcommons/geom_common.f' ! geom-common file
      integer iray,jj,irnow,inext
      inext=iznnxp(iray,jj,irnow)
      return
      end
!--------------------last line of subroutine gettxt.f------------------
                                                                        
!-------------------------------freegm.f--------------------------------
! Version: 051219-1435
! Reference: JNC TN1410 2002-201
!-----------------------------------------------------------------------
!23456789|123456789|123456789|123456789|123456789|123456789|123456789|12

! ----------------------------------------------------------------------
! Required (geometry) subroutine for use with the EGS5 Code System
! ----------------------------------------------------------------------
! This is a subroutine freegm. 
! ----------------------------------------------------------------------
      subroutine freegm(ifti,ifto,chkey,igmid,gmdata,iprm)
      implicit none
      integer ifti,ifto,igmid,iprm
cssl  character chkey*3
      character chkey*(*)
      double precision gmdata(*)
      character chin*250
      integer istbl(200),ietbl(200)
      character gmtyps(15)*3,gmtypl(15)*3
      integer ityppm(15)
      integer igmtyp
      data igmtyp/15/
      data gmtypl/'END','RPP','SPH','RCC','TRC','TOR',
     &            'REC','ELL','WED','BOX','ARB','HEX',
     &            'HAF','TEC','GEL'                 /
      data gmtyps/'end','rpp','sph','rcc','trc','tor',
     &            'rec','ell','wed','box','arb','hex',
     &            'haf','tec','gel'                  /
      data ityppm/ 0, 6, 4, 7, 8, 8,
     &            12, 7,12,12,30,10,
     &             4,13,12         /
      integer i,j,ino,ich,inonw,ichs,iche,inost,ikey,ichkfg
      inonw = 0
      chkey = '   '
      iprm = 0
      ichs = 1
      iche = 250
100   continue
      read(ifti,'(a)',end=990) chin
      call splitc(chin,ich,ichs,iche,ino,istbl,ietbl)
      if((ino.eq.0))goto 100
      if(((ietbl(1)-istbl(1)).ne.2))goto 990
      chkey = chin(istbl(1):ietbl(1))
      inost = 2
      do i=1,igmtyp
        if(chkey.eq.gmtypl(i).or.chkey.eq.gmtyps(i))then
          ikey = i
          iprm = ityppm(i)
          goto 220
        endif
      end do
      goto 990
220   continue
      if((ikey.eq.1))goto 900
      if((ino.ne.1))goto 400
300   continue
      read(ifti,'(a)',end=990) chin
      call splitc(chin,ich,ichs,iche,ino,istbl,ietbl)
      if((ino.eq.0))goto 300
      inost = 1
400   continue
      if((inonw.eq.0))then
      j = ietbl(inost)-istbl(inost)+1
      call chkint(chin(istbl(inost):ietbl(inost)),j,ichkfg)
      if((ichkfg.ne.0))goto 990
      read(chin(istbl(inost):ietbl(inost)),*) igmid
      inost = inost + 1
      endif
      do i=inost,ino
        inonw = inonw + 1
        if((inonw.gt.ityppm(ikey)))goto 990
        j = ietbl(i)-istbl(i)+1
        call chkflt(chin(istbl(i):ietbl(i)),j,ichkfg)
        if((ichkfg.ne.0))goto 990
        read(chin(istbl(i):ietbl(i)),*) gmdata(inonw)
      end do
      if((inonw.lt.ityppm(ikey)))goto 300
900   continue
      return
990   continue
      write(ifto,'(a,a)') ' geom data error : ',chkey
      write(ifto,'(a)') chin
      stop
      end
!--------------------last line of subroutine freegm.f------------------
                                                                        
!-------------------------------freezn.f--------------------------------
! Version: 051219-1435
! Reference: JNC TN1410 2002-201
!-----------------------------------------------------------------------
!23456789|123456789|123456789|123456789|123456789|123456789|123456789|12

! ----------------------------------------------------------------------
! Required (geometry) subroutine for use with the EGS5 Code System
! ----------------------------------------------------------------------
! This is a subroutine freezn. 
! ----------------------------------------------------------------------
      subroutine freezn(ifti,ifto,chkey,iznid,iznor,izndt,iprm,ioptid,
     &                  MAX_IZN)
      implicit none
      integer ifti,ifto,iznid,iprm,ioptid
      integer  MAX_IZN
cssl  character chkey*3,iznor(*)*2
      character chkey*(*),iznor(*)*2
      integer izndt(*)
      character chin*250
      integer istbl(200),ietbl(200)
      character zntypl(2)*3,zntyps(2)*3
      integer izntyp
      data izntyp/2/
      data zntypl/'END','ZON'/
      data zntyps/'end','zon'/
      integer i,j,inonw,ichs,iche,inost,ikey,
     &        ino,ich,ichkfg
      inonw = 0
      chkey = '   '
      iprm = 0
      ichs = 1
      iche = 250
      iznor(1) = '  '
100   continue
      read(ifti,'(a)',end=990) chin
      call splitc(chin,ich,ichs,iche,ino,istbl,ietbl)
      if((ino.eq.0))goto 100
      j = ietbl(1)-istbl(1)+1
      call chkint(chin(istbl(1):ietbl(1)),j,ichkfg)
      if((ichkfg.eq.0))goto 990
      inost = 2
      ikey = 0
cssl  if(((ietbl(1)-istbl(1)).gt.3))goto 990
      if(((ietbl(1)-istbl(1)).gt.10))goto 990
      chkey = chin(istbl(1):ietbl(1))
      if(((ietbl(1)-istbl(1)).ne.2))goto 220
      do i=1,izntyp
        if(chkey.eq.zntypl(i).or.chkey.eq.zntyps(i))then
          ikey = i
          goto 220
        endif
      end do
220   continue
      if((ikey.eq.1))goto 900
      if((ino.ne.1))goto 400
300   continue
      read(ifti,'(a)',end=990) chin
      call splitc(chin,ich,ichs,iche,ino,istbl,ietbl)
      if((ino.eq.0))goto 300
      j = ietbl(1)-istbl(1)+1
      call chkint(chin(istbl(1):ietbl(1)),j,ichkfg)
      if((ichkfg.ne.0))then
      if((j.eq.2 .and.((chin(istbl(1):ietbl(1)).eq.'OR') .or.
     &   (chin(istbl(1):ietbl(1)).eq.'or'))))then
      else
      backspace (ifti)
      goto 900
      endif
      endif
      inost = 1
400   continue
      if((inonw.eq.0.and.ioptid.eq.1))then
      j = ietbl(inost)-istbl(inost)+1
      call chkint(chin(istbl(inost):ietbl(inost)),j,ichkfg)
      if((ichkfg.ne.0))goto 990
      read(chin(istbl(inost):ietbl(inost)),*) iznid
      inost = inost + 1
      endif
      do i=inost,ino
        j = ietbl(i)-istbl(i)+1
        call chkint(chin(istbl(i):ietbl(i)),j,ichkfg)
        if((ichkfg.eq.0))then
          inonw = inonw + 1
          if(inonw.ge.MAX_IZN) then
            write(*,*) 'Dimension over of izndt=',MAX_IZN
            stop
          endif
          read(chin(istbl(i):ietbl(i)),*) izndt(inonw)
          iznor(inonw+1) = '  '
        elseif(j.eq.2 .and.((chin(istbl(i):ietbl(i)).eq.'OR') .or.
     &       (chin(istbl(i):ietbl(i)).eq.'or'))) then
          inonw = inonw + 1
          if((inonw.eq.1))goto 990
          if((iznor(inonw).eq.'OR'.or.iznor(inonw).eq.'or'))goto 990
          iznor(inonw) = chin(istbl(i):ietbl(i))
          inonw = inonw - 1
        else
          goto 990
        endif
      end do
      goto 300
900   continue
      iprm = inonw
      return
990   continue
      write(ifto,'(a,a)') ' zone data error : ',chkey
      write(ifto,'(a)') chin
      stop
      end
!--------------------last line of subroutine freezn.f------------------
                                                                        
!-------------------------------splitc.f--------------------------------
! Version: 051219-1435
! Reference: JNC TN1410 2002-201
!-----------------------------------------------------------------------
!23456789|123456789|123456789|123456789|123456789|123456789|123456789|12

! ----------------------------------------------------------------------
! Required (geometry) subroutine for use with the EGS5 Code System
! ----------------------------------------------------------------------
! This is a subroutine splitc. 
! ----------------------------------------------------------------------
      subroutine splitc(chin,ich,ichs,iche,ino,istbl,ietbl)
      implicit none
      character chin*(*)
      integer i,ich,ichs,iche,ino
      integer istbl(*),ietbl(*)
      integer ichss,ichee,is,ie
      do i=ichs,iche
        if (chin(i:i).lt.' ') then
          chin(i:i)=' '
        end if
      end do
      ichss = ichs
      if((ichss.eq.0))then
        ichss = 1
      endif
      ichee = iche
      if((ichee.eq.0))then
        ichee = ich
      endif
      ino = 0
      is = 0
      ie = 0
      do i=ichss,ichee
cssl    if((is.eq.0.and.chin(i:i).ne.' '))then
        if((is.eq.0.and.
     &   (chin(i:i).ne.' '.and.chin(i:i).ne.',')))then
          is = i
          ie = i
cssl    elseif(chin(i:i).ne.' ') then
        elseif(chin(i:i).ne.' '.and.chin(i:i).ne.',') then
          ie = i
        elseif(is.ne.0) then
          ino = ino + 1
          istbl(ino) = is
          ietbl(ino) = ie
          is = 0
          ie = 0
        endif
        if((i.eq.ichee.and.is.ne.0))then
         ino = ino + 1
         istbl(ino) = is
         ietbl(ino) = ie
         is = 0
         ie = 0
        endif
      end do
      return
      end
!--------------------last line of subroutine splitc.f------------------
                                                                        
!-------------------------------chkint.f--------------------------------
! Version: 051219-1435
! Reference: JNC TN1410 2002-201
!-----------------------------------------------------------------------
!23456789|123456789|123456789|123456789|123456789|123456789|123456789|12

! ----------------------------------------------------------------------
! Required (geometry) subroutine for use with the EGS5 Code System
! ----------------------------------------------------------------------
! This is a subroutine chkint. 
! ----------------------------------------------------------------------
      subroutine chkint(chin,ich,ichkfg)
      implicit none
      character chin*(*)
      integer ich,ichkfg
      integer i
      ichkfg = 0
      do i=1,ich
        if((i.eq.1.and.(chin(i:i).eq.'+'.or.chin(i:i).eq.'-')))then
        elseif(chin(i:i).ge.'0'.and.chin(i:i).le.'9') then
        else
          ichkfg = 1
        endif
      end do
      return
      end
!--------------------last line of subroutine chkint.f------------------
                                                                        
!-------------------------------chkflt.f--------------------------------
! Version: 051219-1435
! Reference: JNC TN1410 2002-201
!-----------------------------------------------------------------------
!23456789|123456789|123456789|123456789|123456789|123456789|123456789|12

! ----------------------------------------------------------------------
! Required (geometry) subroutine for use with the EGS5 Code System
! ----------------------------------------------------------------------
! This is a subroutine chkflt. 
! ----------------------------------------------------------------------
      subroutine chkflt(chin,ich,ichkfg)
      implicit none
      character chin*(*)
      integer ich,ichkfg
      integer i,ie,idot
      ichkfg = 0
      idot = 0
      ie = 0
      do i=1,ich
        if((i.eq.1.and.(chin(i:i).eq.'+'.or.chin(i:i).eq.'-')))then
        elseif(ie.eq.0.and.(chin(i:i).eq.'E'.or.chin(i:i).eq.'e')) then
          ie = 1
        elseif(ie.eq.0.and.(chin(i:i).eq.'D'.or.chin(i:i).eq.'d')) then
          ie = 1
        elseif(ie.eq.1.and.(chin(i:i).eq.'+'.or.chin(i:i).eq.'-')) then
          ie =2
        elseif(idot.eq.0.and.chin(i:i).eq.'.') then
          idot = 1
        elseif(chin(i:i).ge.'0'.and.chin(i:i).le.'9') then
          if((ie.eq.1))ie = 2
        else
         ichkfg = 1
        endif
      end do
      return
      end
!--------------------last line of subroutine chkflt.f------------------

!-------------------------------ellset.f--------------------------------
! Version: 051219-1435
! Reference: JNC TN1410 2002-201
!-----------------------------------------------------------------------
!23456789|123456789|123456789|123456789|123456789|123456789|123456789|12

! ----------------------------------------------------------------------
! Required (geometry) subroutine for use with the EGS5 Code System
! ----------------------------------------------------------------------
! This is a subroutine ellset.
      subroutine ellset(izon)
      implicit none
c
      include 'auxcommons/geom_common.f' ! geom-common file
c
      integer izon

      double precision chk1

      chk1 = sqrt(
     &       (ellpnt(4,izon)-ellpnt(1,izon))
     &     * (ellpnt(4,izon)-ellpnt(1,izon))
     &     + (ellpnt(5,izon)-ellpnt(2,izon))
     &     * (ellpnt(5,izon)-ellpnt(2,izon))
     &     + (ellpnt(6,izon)-ellpnt(3,izon))
     &     * (ellpnt(6,izon)-ellpnt(3,izon)))
      if(ellpnt(7,izon).le.chk1) then
        write(*,*) 'Error of ELL ',nbell(izon),' : Radius is short'
        stop
      end if
      return
      end
!--------------------last line of subroutine ellset.f------------------

!-------------------------------ellcg1.f--------------------------------
! Version: 051219-1435
! Reference: JNC TN1410 2002-201
!-----------------------------------------------------------------------
!23456789|123456789|123456789|123456789|123456789|123456789|123456789|12

! ----------------------------------------------------------------------
! Required (geometry) subroutine for use with the EGS5 Code System
! ----------------------------------------------------------------------
! This is a subroutine ellcg1.
! ----------------------------------------------------------------------
      subroutine ellcg1(izon,xl,yl,zl,unp,vnp,wnp)
      implicit none
c
      include 'auxcommons/geom_common.f' ! geom-common file
c
      double precision xl,yl,zl
      double precision unp,vnp,wnp
      integer izon
      double precision a,a1,a2,b,b1,b2,c,u
      double precision ald,alu,rin,rout
      rin = dlnmax
      rout=-dlnmax
      a1=(xl-ellpnt(1,izon))*unp
     &  +(yl-ellpnt(2,izon))*vnp
     &  +(zl-ellpnt(3,izon))*wnp
      b1=(xl-ellpnt(1,izon))*(xl-ellpnt(1,izon))
     &  +(yl-ellpnt(2,izon))*(yl-ellpnt(2,izon))
     &  +(zl-ellpnt(3,izon))*(zl-ellpnt(3,izon))
      a2=(xl-ellpnt(4,izon))*unp
     &  +(yl-ellpnt(5,izon))*vnp
     &  +(zl-ellpnt(6,izon))*wnp
      b2=(xl-ellpnt(4,izon))*(xl-ellpnt(4,izon))
     &  +(yl-ellpnt(5,izon))*(yl-ellpnt(5,izon))
     &  +(zl-ellpnt(6,izon))*(zl-ellpnt(6,izon))
c
      c=ellpnt(7,izon)
      a=(a2-a1)/c
      b=(c*c+b2-b1)/c*0.5d0
      ald=a*a-1.0d0
      alu=(a*b-a2)/ald
      u=(b*b-b2)/ald
      c=alu*alu-u
      IF(c.ge.0.0d0) then
        c=dsqrt(c)
        rin= -alu-c
        rout=-alu+c
        IF(rin.ge.0.0d0) then
          itvalm=itvalm+1
          atval(itvalm)=rin
        elseif(rin.ge.-elleps.and.rin.lt.0.0d0) then
          itvalm=itvalm+1
          atval(itvalm)=0.0d0
        endif
        IF(rout.ge.0.0d0) then
          itvalm=itvalm+1
          atval(itvalm)=rout
        elseif(rout.ge.-elleps.and.rout.lt.0.0d0) then
          itvalm=itvalm+1
          atval(itvalm)=0.0d0
        endif
      endif
      RETURN
      end
!--------------------last line of subroutine ellcg1.f------------------

!-------------------------------arbset.f--------------------------------
! Version: 051219-1435
! Reference: JNC TN1410 2002-201
!-----------------------------------------------------------------------
!23456789|123456789|123456789|123456789|123456789|123456789|123456789|12

! ----------------------------------------------------------------------
! Required (geometry) subroutine for use with the EGS5 Code System
! ----------------------------------------------------------------------
! This is a subroutine arbset.
! ----------------------------------------------------------------------
      SUBROUTINE arbset(izon)
      implicit none
c
      include 'auxcommons/geom_common.f' ! geom-common file
c
      integer izon
      double precision X(3,8),V(3,4)
      integer IX(4,6)
      integer i,j,k,m,ipmax,nside,j1,j2,npl,nmi
      double precision a,b,c,d,dmin,ds,eps
      double precision dltarb2
      dltarb2=1.0d-6
c
      DO 100 I=1,8
        DO 100 J=1,3
          k=3*(I-1)+J
          X(J,I)=arbpnt(k,izon)
  100 continue
C
      IPMAX=0
      NSIDE=0
      DO  140 I=1,6
        K=0
        M=arbpnt(I+24,izon)
        DO  120  J=1,4
          IX(J,I)=M-(M/10)*10
          IF( IX(J,I).NE.0) K=K+1
          IF( IX(J,I).GT.IPMAX) IPMAX=IX(J,I)
          M=M/10
  120   continue
        IF(K.EQ.0) then
          GO TO 150
        elseif(K.lt.3) then
          WRITE(*,6000) 'ERROR IN SIDE DESCRIPTION',
     &                     I,J,arbpnt(I+24,izon)
 6000 FORMAT(1x,a,2I10,F10.0)
          stop
        endif
        NSIDE=I
  140 CONTINUE
C
C  FIND  MINIMUM DISTANCE BETWEEN POINTS
C
  150 continue
      DMIN=dlnmax
      DO 160 I=1,IPMAX-1
        DO 160 J=I+1,IPMAX
          D = (X(1,I)-X(1,J))**2 + (X(2,I)-X(2,J))**2
     &      + (X(3,I)-X(3,J))**2
          IF((D.GT.0).AND.(D.LT.DMIN)) then
            DMIN=D
          endif
  160 CONTINUE
C
      DMIN=DSQRT(DMIN)
C
      DO 300 I=1,NSIDE
        J1= IX( 3,I)
        DO 200  J=2,4,2
          J2= IX( J,I)
          DO 200  K=1,3
            V(K,J) = X(K,J1)- X(K,J2)
  200   continue
        A= V(2,2)*V(3,4) -  V(3,2)*V(2,4)
        B= V(3,2)*V(1,4) -  V(1,2)*V(3,4)
        C= V(1,2)*V(2,4) -  V(2,2)*V(1,4)
        D=-(A*X(1,J1)+ B*X(2,J1)+ C*X(3,J1) )
        EPS=DSQRT(A*A + B*B + C*C)
        NPL=0
        NMI=0
        DO 280 J=1,IPMAX
          DS=(A*X(1,J) + B*X(2,J)+ C*X(3,J) + D)/EPS
          IF(DABS(DS).ge.DMIN*dltarb2) then
            IF(ds.lt.0.0d0) then
              NMI=NMI+1
            ELSEIF(ds.gt.0.0d0) then
              NPL=NPL+1
            endif
          endif
  280   CONTINUE
        if((NMI.GT.0).or.(NPL.EQ.0)) then
          EPS=-EPS
          if((NPL.gt.0).or.(NMI.eq.0)) then
            WRITE(*,6200) 'ERROR IN FACE DESCRIPTION',
     &                  I,NMI,NPL,arbpnt(i+24,izon),A,B,C,D
 6200 FORMAT(1x,a,3I10/5D15.7)
            stop
          ENDIF
        endif
        arbtbl(4*i-3,izon) = A/EPS
        arbtbl(4*i-2,izon) = B/EPS
        arbtbl(4*i-1,izon) = C/EPS
        arbtbl(4*i  ,izon) = D/EPS
  300 continue
c
      arbtbl(25,izon)=DMIN
      arbtbl(26,izon)=NSIDE
cc    do j=1,26
cc    WRITE(*,*) 'j,arbtbl=',j,arbtbl(j,izon)
cc    end do
      RETURN
      END
!--------------------last line of subroutine arbset.f------------------

!-------------------------------arbcg1.f--------------------------------
! Version: 060719-1615
! Reference: JNC TN1410 2002-201
!-----------------------------------------------------------------------
!23456789|123456789|123456789|123456789|123456789|123456789|123456789|12

! ----------------------------------------------------------------------
! Required (geometry) subroutine for use with the EGS5 Code System
! ----------------------------------------------------------------------
! This is a subroutine arbcg1.
! ----------------------------------------------------------------------
      subroutine arbcg1(izon,xl,yl,zl,unp,vnp,wnp)
      implicit none
c
      include 'auxcommons/geom_common.f' ! geom-common file
c
      double precision xl,yl,zl
      double precision unp,vnp,wnp
      integer izon
      integer i,ii,j,jj,nside
      double precision dmin
      double precision rin,rout,dx,dy,dz
      double precision pv(3)
      double precision dltarb2
      dltarb2=1.0d-6
      dmin =arbtbl(25,izon)*dltarb2
      nside=arbtbl(26,izon)
      rin = dlnmax
      rout=-dlnmax
      do 400 ii=1,nside
        i=(ii-1)*4+1
        dx=arbtbl(i  ,izon)*xl+arbtbl(i+1,izon)*yl
     &    +arbtbl(i+2,izon)*zl+arbtbl(i+3,izon)
        dy= -arbtbl(i  ,izon)*unp-arbtbl(i+1,izon)*vnp
     &      -arbtbl(i+2,izon)*wnp
        if( dabs(dy).gt.dltarb2) then
           dz=dx/dy
           pv(1)=xl+dz*unp
           pv(2)=yl+dz*vnp
           pv(3)=zl+dz*wnp
           do 200 jj=1,nside
             j=(jj-1)*4+1
             if(j.ne.i) then
               dx=arbtbl(j  ,izon)*pv(1)+arbtbl(j+1,izon)*pv(2)
     &           +arbtbl(j+2,izon)*pv(3)+arbtbl(j+3,izon)
               if( dx.lt.-arbeps .and. -dx.ge.dmin ) then
                 goto 400
               endif
             end if
  200      continue
c           WRITE(*,*) 'ii,dz,rin,rout=',ii,dz,rin,rout
           if(dz.le.rout) then
             rin=dz
             goto 600
           else
             rin=rout
             rout=dz
             IF(rin.gt.-dlnmax) then
               GOTO 600
             endif
           endif
        endif
c        WRITE(*,*) 'ii,rin,rout=',ii,rin,rout
  400 continue
  600 continue
      IF(rin.ge.0.0d0.and.rin.lt.dlnmax) then
          itvalm=itvalm+1
          atval(itvalm)=rin
      elseif(rin.ge.-arbeps.and.rin.lt.0.0d0) then
          itvalm=itvalm+1
          atval(itvalm)=0.0d0
      endif
      IF(rout.ge.0.0d0.and.rout.lt.dlnmax) then
        itvalm=itvalm+1
        atval(itvalm)=rout
      elseif(rout.ge.-arbeps.and.rout.lt.0.0d0) then
          itvalm=itvalm+1
          atval(itvalm)=0.0d0
      endif
      return
      end
!--------------------last line of subroutine arbcg1.f------------------

!-------------------------------recset.f--------------------------------
! Version: 051219-1435
! Reference: JNC TN1410 2002-201
!-----------------------------------------------------------------------
!23456789|123456789|123456789|123456789|123456789|123456789|123456789|12

! ----------------------------------------------------------------------
! Required (geometry) subroutine for use with the EGS5 Code System
! ----------------------------------------------------------------------
! This is a subroutine recset.
      subroutine recset(izon)
      implicit none
c
      include 'auxcommons/geom_common.f' ! geom-common file
c
      integer izon

      double precision chk1,chk2,chk3,chksqr
      data chksqr/1.0d-4/

      recpnt(13,izon)=recpnt(4,izon)*recpnt(4,izon)
     &               +recpnt(5,izon)*recpnt(5,izon)
     &               +recpnt(6,izon)*recpnt(6,izon)
      recpnt(14,izon)=recpnt(7,izon)*recpnt(7,izon)
     &               +recpnt(8,izon)*recpnt(8,izon)
     &               +recpnt(9,izon)*recpnt(9,izon)
      recpnt(15,izon)=recpnt(10,izon)*recpnt(10,izon)
     &               +recpnt(11,izon)*recpnt(11,izon)
     &               +recpnt(12,izon)*recpnt(12,izon)
      if(recpnt(13,izon).eq.0.0d0.or.
     &   recpnt(14,izon).eq.0.0d0.or.
     &   recpnt(15,izon).eq.0.0d0) then
        write(*,*) 'Error of REC ',nbrec(izon),' : Length is 0.0'
        stop
      end if
c
      chk1 = ( recpnt( 7,izon)*recpnt(4,izon)
     &     +   recpnt( 8,izon)*recpnt(5,izon)
     &     +   recpnt( 9,izon)*recpnt(6,izon) )
     &     / sqrt(recpnt(13,izon)*recpnt(14,izon))
      chk2 = ( recpnt(10,izon)*recpnt(4,izon)
     &     +   recpnt(11,izon)*recpnt(5,izon)
     &     +   recpnt(12,izon)*recpnt(6,izon) )
     &     / sqrt(recpnt(13,izon)*recpnt(15,izon))
      chk3 = ( recpnt(10,izon)*recpnt(7,izon)
     &     +   recpnt(11,izon)*recpnt(8,izon)
     &     +   recpnt(12,izon)*recpnt(9,izon) )
     &     / sqrt(recpnt(14,izon)*recpnt(15,izon))
      if(abs(chk1).gt.chksqr.or.
     &   abs(chk2).gt.chksqr.or.
     &   abs(chk3).gt.chksqr) then
        write(*,*) 'Error of REC ',nbrec(izon),' : Vector is not square'
     &             ,chk1,chk2,chk3,chksqr
        stop
      end if
      return
      end
!--------------------last line of subroutine recset.f------------------

!-------------------------------reccg1.f--------------------------------
! Version: 051219-1435
! Reference: JNC TN1410 2002-201
!-----------------------------------------------------------------------
!23456789|123456789|123456789|123456789|123456789|123456789|123456789|12

! ----------------------------------------------------------------------
! Required (geometry) subroutine for use with the EGS5 Code System
! ----------------------------------------------------------------------
! This is a subroutine reccg1.
      subroutine reccg1(izon,xl,yl,zl,unp,vnp,wnp)
      implicit none
c
      include 'auxcommons/geom_common.f' ! geom-common file
c
      double precision xl,yl,zl
      double precision unp,vnp,wnp
      integer izon
      integer lro,lri,lcp,lcm
      double precision rin,rout
      double precision vph,vphhh
      double precision ambd,ambda,um,umu,den,disc
      double precision sd,f1,r1,r2,hh,wh,cp,cm
c
      double precision rs2,rl2,plx,ply,plz,vrs,vrl,wrs,wrl,wrs2,wrl2
      double precision rs4,rl4,dltrec
c
      double precision plxn,plyn,plzn,vphn
      double precision wph,wph2
      double precision vrsn,vrln

      DATA dltrec/1.0d-6/
c
      rin =-dlnmax
      rout= dlnmax
      lro=0
      lri=0
c
c4    COMPUTE DOT PRODUCTS OF P.P AND Q.Q
c
      hh =recpnt(13,izon)
      rs2=recpnt(14,izon)
      rl2=recpnt(15,izon)
      rs4=rs2*rs2
      rl4=rl2*rl2
c
c5    COMPUTE (V-XB) FOR X,Y,Z COORDINATES
c
      plx=(xl-recpnt(1,izon))
      ply=(yl-recpnt(2,izon))
      plz=(zl-recpnt(3,izon))
c
c6    TRANSFORM XL,YL,ZL TO THE COORDINATES OF THE REC
c
      vph=plx*recpnt( 4,izon)+ply*recpnt( 5,izon)+plz*recpnt( 6,izon)
      vrs=plx*recpnt( 7,izon)+ply*recpnt( 8,izon)+plz*recpnt( 9,izon)
      vrl=plx*recpnt(10,izon)+ply*recpnt(11,izon)+plz*recpnt(12,izon)
c
      um=rl4*vrs*vrs+rs4*vrl*vrl-rs4*rl4
      wh =unp*recpnt(4,izon)+vnp*recpnt(5,izon)+wnp*recpnt(6,izon)
c
      wph=unp*recpnt( 4,izon)+vnp*recpnt( 5,izon)+wnp*recpnt( 6,izon)
      wrs=unp*recpnt( 7,izon)+vnp*recpnt( 8,izon)+wnp*recpnt( 9,izon)
      wrl=unp*recpnt(10,izon)+vnp*recpnt(11,izon)+wnp*recpnt(12,izon)
      wph2=wph*wph
      wrs2=wrs*wrs
      wrl2=wrl*wrl
      ambd=wrs*vrs*rl4+wrl*vrl*rs4
      den=wrs2*rl4+wrl2*rs4
      if(dabs(den).le.dltrec) then
        IF(dabs(ambd).le.dltrec) then
          r1=-dlnmax
          r2= dlnmax
        else
          r1 = -um/ambd/2.0d0
          r2 = -um/ambd/2.0d0
        endif
      else
        ambda=ambd/den
        umu=um/den
        disc=ambda**2-umu
        if(disc.lt.0.0d0) goto 250
c
c8    COMPUTE THE INTERSECT POINTS ON THE QUADRATIC SURFACE
c
        if(disc.eq.0.0d0) then
          r1=-ambda
          r2=-ambda
        else
          sd=dsqrt(disc)
          r1=-ambda-sd
          r2=-ambda+sd
        endif
      endif
      IF(r1.ge.-receps.and.r1.lt.dlnmax) then
        plxn=(r1*unp+xl-recpnt(1,izon))
        plyn=(r1*vnp+yl-recpnt(2,izon))
        plzn=(r1*wnp+zl-recpnt(3,izon))
        vphn=plxn*recpnt( 4,izon)+plyn*recpnt( 5,izon)
     &      +plzn*recpnt( 6,izon)
        IF(vphn.ge.-receps.and.vphn.le.(hh+receps)*(1.0d0+receps)) then
          if(r1.ge.0.0d0) then
            itvalm=itvalm+1
            atval(itvalm)=r1
          else
            itvalm=itvalm+1
            atval(itvalm)=0.0d0
          endif
        endif
      endif
      IF(r2.ge.-receps.and.r2.lt.dlnmax.and.r2.ne.r1) then
        plxn=(r2*unp+xl-recpnt(1,izon))
        plyn=(r2*vnp+yl-recpnt(2,izon))
        plzn=(r2*wnp+zl-recpnt(3,izon))
        vphn=plxn*recpnt( 4,izon)+plyn*recpnt( 5,izon)
     &      +plzn*recpnt( 6,izon)
        IF(vphn.ge.-receps.and.vphn.le.(hh+receps)*(1.0d0+receps)) then
          itvalm=itvalm+1
          if(r2.ge.0.0d0) then
            itvalm=itvalm+1
            atval(itvalm)=r2
          else
            itvalm=itvalm+1
            atval(itvalm)=0.0d0
          endif
        endif
      endif
  250 continue
c
c9    DETERMINE IF RAY PARALLEL TO PLANAR SURFACES
c
      if(wh.lt.0.0d0) then
        if(vph.lt.-receps) goto 300
c
c10   COMPUTE THE INTERSECT POINTS ON THE PLANAR SURFACES
c
        cp=-vph/wh
        cm=(-vph+hh)/wh
        lcp=1
        lcm=2
      ELSEIF(wh.gt.0.0d0) then
        vphhh=-vph+hh
        if(vphhh.lt.-receps) goto 300
        cp=vphhh/wh
        cm=-vph/wh
        lcm=1
        lcp=2
      else
        cp=dlnmax
        cm=-cp
      endif
c
      IF(lcm.le.2) then
        IF(cm.ge.-receps.and.cm.lt.dlnmax) then
          plxn=(cm*unp+xl-recpnt(1,izon))
          plyn=(cm*vnp+yl-recpnt(2,izon))
          plzn=(cm*wnp+zl-recpnt(3,izon))
          vrsn=plxn*recpnt( 7,izon)+plyn*recpnt( 8,izon)
     &        +plzn*recpnt( 9,izon)
          vrln=plxn*recpnt(10,izon)+plyn*recpnt(11,izon)
     &        +plzn*recpnt(12,izon)
          f1=rl4*vrsn*vrsn+rs4*vrln*vrln-rs4*rl4
          IF(f1.le.receps) then
            if(cm.ge.0.0d0) then
              itvalm=itvalm+1
              atval(itvalm)=cm
            else
              itvalm=itvalm+1
              atval(itvalm)=0.0d0
            endif
          endif
        endif
      endif
c
      IF(lcp.le.2) then
        IF(cp.ge.-receps.and.cp.lt.dlnmax) then
          plxn=(cp*unp+xl-recpnt(1,izon))
          plyn=(cp*vnp+yl-recpnt(2,izon))
          plzn=(cp*wnp+zl-recpnt(3,izon))
          vrsn=plxn*recpnt( 7,izon)+plyn*recpnt( 8,izon)
     &        +plzn*recpnt( 9,izon)
          vrln=plxn*recpnt(10,izon)+plyn*recpnt(11,izon)
     &        +plzn*recpnt(12,izon)
          f1=rl4*vrsn*vrsn+rs4*vrln*vrln-rs4*rl4
          IF(f1.le.receps) then
            if(cp.ge.0.0d0) then
              itvalm=itvalm+1
              atval(itvalm)=cp
            else
              itvalm=itvalm+1
              atval(itvalm)=0.0d0
            endif
          endif
        endif
      endif
c
  300 continue
      return
      end
!--------------------last line of subroutine reccg1.f------------------

!-------------------------------wedset.f--------------------------------
! Version: 051219-1435
! Reference: JNC TN1410 2002-201
!-----------------------------------------------------------------------
!23456789|123456789|123456789|123456789|123456789|123456789|123456789|12

! ----------------------------------------------------------------------
! Required (geometry) subroutine for use with the EGS5 Code System
! ----------------------------------------------------------------------
! This is a subroutine wedset.
      subroutine wedset(izon)
      implicit none
c
      include 'auxcommons/geom_common.f' ! geom-common file
c
      integer izon
      integer i,ja

      double precision chk1,chk2,chk3,chksqr
      data chksqr/1.0d-4/
c
      DO 100 i=1,3
        ja=3*i
        wedpnt(i+12,izon)=wedpnt(ja+1,izon)*wedpnt(ja+1,izon)
     &                   +wedpnt(ja+2,izon)*wedpnt(ja+2,izon)
     &                   +wedpnt(ja+3,izon)*wedpnt(ja+3,izon)
  100 continue
c
      if(wedpnt(13,izon).eq.0.0d0.or.
     &   wedpnt(14,izon).eq.0.0d0.or.
     &   wedpnt(15,izon).eq.0.0d0) then
        write(*,*) 'Error of WED ',nbwed(izon),' : Length is 0.0'
        stop
      end if
c
      chk1 = ( wedpnt( 7,izon)*wedpnt(4,izon)
     &     +   wedpnt( 8,izon)*wedpnt(5,izon)
     &     +   wedpnt( 9,izon)*wedpnt(6,izon) )
     &     / sqrt(wedpnt(13,izon)*wedpnt(14,izon))
      chk2 = ( wedpnt(10,izon)*wedpnt(4,izon)
     &     +   wedpnt(11,izon)*wedpnt(5,izon)
     &     +   wedpnt(12,izon)*wedpnt(6,izon) )
     &     / sqrt(wedpnt(13,izon)*wedpnt(15,izon))
      chk3 = ( wedpnt(10,izon)*wedpnt(7,izon)
     &     +   wedpnt(11,izon)*wedpnt(8,izon)
     &     +   wedpnt(12,izon)*wedpnt(9,izon) )
     &     / sqrt(wedpnt(14,izon)*wedpnt(15,izon))
      if(abs(chk1).gt.chksqr.or.
     &   abs(chk2).gt.chksqr.or.
     &   abs(chk3).gt.chksqr) then
        write(*,*) 'Error of WED ',nbwed(izon),' : Vector is not square'
     &             ,chk1,chk2,chk3,chksqr
        stop
      end if
      return
      end      
!--------------------last line of subroutine wedset.f------------------

!-------------------------------wedcg1.f--------------------------------
! Version: 051219-1435
! Reference: JNC TN1410 2002-201
!-----------------------------------------------------------------------
!23456789|123456789|123456789|123456789|123456789|123456789|123456789|12

! ----------------------------------------------------------------------
! Required (geometry) subroutine for use with the EGS5 Code System
! ----------------------------------------------------------------------
! This is a subroutine wedcg1.
      subroutine wedcg1(izon,xl,yl,zl,unp,vnp,wnp)
      implicit none
c
      include 'auxcommons/geom_common.f' ! geom-common file
c
      double precision xl,yl,zl
      double precision unp,vnp,wnp
      integer izon
      integer i,ii,ll,kk
      double precision rin,rout,cm,cp
      double precision al(3),ap(3),aw(3)
      double precision dx,dy,dz
      double precision ag,temp,top,ap4
c
      rin =-dlnmax
      rout= dlnmax
      cm  =-dlnmax
      cp  = dlnmax
      ll=0
      kk=0
      dx=xl-wedpnt(1,izon)
      dy=yl-wedpnt(2,izon)
      dz=zl-wedpnt(3,izon)
      do 100 i=1,3
        ii=3*i+1
        al(i)=wedpnt(i+12,izon)
        ap(i)=dx*wedpnt(ii,izon)+dy*wedpnt(ii+1,izon)
     &       +dz*wedpnt(ii+2,izon)
        aw(i)=unp*wedpnt(ii,izon)+vnp*wedpnt(ii+1,izon)
     &       +wnp*wedpnt(ii+2,izon)
  100 continue
      ap4=ap(1)*al(2)+ap(2)*al(1)
      top=al(1)*al(2)-ap4
c
      do 200 i=1,2
        IF(aw(i).lt.0.0d0) then
          if(ap(i).lt.-wedeps) goto 300
          temp=-ap(i)/aw(i)
          if(temp.lt.cp) then
            cp=temp
            ll=i
          endif
        ELSEIF(aw(i).gt.0.0d0) then
          if((ap(i)+wedeps)*(1.0d0+wedeps).gt.al(i)) goto 300
          if(ap(i).le.0.0d0) then
            temp=-ap(i)/aw(i)
            if(temp.gt.cm) then
              cm=temp
              kk=i
            endif
          endif
        else
          if( ap(i).lt.-wedeps) goto 900
          if( (ap(i)+wedeps)*(1.0d0+wedeps).gt.al(i)) goto 900
        endif
  200 continue
c     WRITE(*,*) 'aw(3),ap(3),al(3)=',aw(3),ap(3),al(3)
      IF(aw(3).lt.0.0d0) then
        temp=-ap(3)+al(3)
        if(temp.le.wedeps) then
          temp=temp/aw(3)
          if(temp.gt.cm) then
            cm=temp
            kk=3
          endif
        endif
        if(ap(3).lt.-wedeps) goto 300
        temp=-ap(3)/aw(3)
        if(temp.lt.cp) then
          cp=temp
          ll=3
        endif
      elseif(aw(3).eq.0.0d0) then
        if(ap(3).lt.-wedeps) goto 300
        if((ap(3)+wedeps)*(1.0d0+wedeps).gt.al(3)) goto 300
      else
        if(ap(3).le.wedeps) then
          temp=-ap(3)/aw(3)
          if(temp.gt.cm) then
            cm=temp
            kk=3
          endif
        endif
        temp=-ap(3)+al(3)
        if(temp.lt.-wedeps) goto 300
        temp=temp/aw(3)
        if(temp.lt.cp) then
          cp=temp
          ll=3
        endif
      endif
      ag=al(2)*aw(1)+al(1)*aw(2)
c
      IF(ag.lt.0.0d0) then
        temp=top/ag
        if(temp.gt.cm) then
          cm=temp
          kk=4
        endif
      elseif(ag.gt.0.0d0) then
        if(top.lt.-wedeps) goto 300
        temp=top/ag
        if(temp.lt.cp) then
          cp=temp
          ll=4
         endif
      else
        if(ap4.lt.-wedeps) goto 300
        if(top.lt.-wedeps) goto 300
      endif
c     WRITE(*,*) '   ll,kk,cp,cm=',ll,kk,cp,cm
      if(ll+kk.ge.1) then
        rout=cp
        rin=cm
      endif
c
  300 continue
c     WRITE(*,*) '300 ll,kk,cp,cm=',ll,kk,cp,cm
      
      IF(rin.ge.0.0d0.and.rin.lt.dlnmax) then
        itvalm=itvalm+1
        atval(itvalm)=rin
      elseif(rin.ge.-wedeps.and.rin.lt.0.0d0) then
        itvalm=itvalm+1
        atval(itvalm)=0.0d0
      endif
      IF(rout.ge.0.0d0.and.rout.lt.dlnmax) then
        itvalm=itvalm+1
        atval(itvalm)=rout
      elseif(rout.ge.-wedeps.and.rout.lt.0.0d0) then
        itvalm=itvalm+1
        atval(itvalm)=0.0d0
      endif
  900 continue
c     WRITE(*,*) '900 ll,kk,cp,cm=',ll,kk,cp,cm
      return
      end
!--------------------last line of subroutine wedcg1.f------------------

!-------------------------------boxset.f--------------------------------
! Version: 051219-1435
! Reference: JNC TN1410 2002-201
!-----------------------------------------------------------------------
!23456789|123456789|123456789|123456789|123456789|123456789|123456789|12

! ----------------------------------------------------------------------
! Required (geometry) subroutine for use with the EGS5 Code System
! ----------------------------------------------------------------------
! This is a subroutine boxset.
      subroutine boxset(izon)
      implicit none
c
      include 'auxcommons/geom_common.f' ! geom-common file
c
      integer izon
      integer i,ja
c
      double precision chk1,chk2,chk3,chksqr
      data chksqr/1.0d-4/
c
      DO 100 i=1,3
        ja=3*i
        boxpnt(i+12,izon)=boxpnt(ja+1,izon)*boxpnt(ja+1,izon)
     &                   +boxpnt(ja+2,izon)*boxpnt(ja+2,izon)
     &                   +boxpnt(ja+3,izon)*boxpnt(ja+3,izon)
  100 continue
c
      if(boxpnt(13,izon).eq.0.0d0.or.
     &   boxpnt(14,izon).eq.0.0d0.or.
     &   boxpnt(15,izon).eq.0.0d0) then
        write(*,*) 'Error of BOX ',nbbox(izon),' : Length is 0.0'
        stop
      end if
c
      chk1 = ( boxpnt( 7,izon)*boxpnt(4,izon)
     &     +   boxpnt( 8,izon)*boxpnt(5,izon)
     &     +   boxpnt( 9,izon)*boxpnt(6,izon) )
     &     / sqrt(boxpnt(13,izon)*boxpnt(14,izon))
      chk2 = ( boxpnt(10,izon)*boxpnt(4,izon)
     &     +   boxpnt(11,izon)*boxpnt(5,izon)
     &     +   boxpnt(12,izon)*boxpnt(6,izon) )
     &     / sqrt(boxpnt(13,izon)*boxpnt(15,izon))
      chk3 = ( boxpnt(10,izon)*boxpnt(7,izon)
     &     +   boxpnt(11,izon)*boxpnt(8,izon)
     &     +   boxpnt(12,izon)*boxpnt(9,izon) )
     &     / sqrt(boxpnt(14,izon)*boxpnt(15,izon))
      if(abs(chk1).gt.chksqr.or.
     &   abs(chk2).gt.chksqr.or.
     &   abs(chk3).gt.chksqr) then
        write(*,*) 'Error of BOX ',nbbox(izon),' : Vector is not square'
     &             ,chk1,chk2,chk3,chksqr
        stop
      end if
      return
      end      
!--------------------last line of subroutine boxset.f------------------

!-------------------------------boxcg1.f--------------------------------
! Version: 051219-1435
! Reference: JNC TN1410 2002-201
!-----------------------------------------------------------------------
!23456789|123456789|123456789|123456789|123456789|123456789|123456789|12

! ----------------------------------------------------------------------
! Required (geometry) subroutine for use with the EGS5 Code System
! ----------------------------------------------------------------------
! This is a subroutine boxcg1.
      subroutine boxcg1(izon,xl,yl,zl,unp,vnp,wnp)
      implicit none
c
      include 'auxcommons/geom_common.f' ! geom-common file
c
      double precision xl,yl,zl
      double precision unp,vnp,wnp
      integer izon
      integer i,ja
      double precision rin,rout
      double precision al,ap,av,dl,ds
c
      rin =-dlnmax
      rout= dlnmax
      DO 300 i=1,3
        ja=3*i
        ap=(boxpnt(1,izon)-xl)*boxpnt(ja+1,izon)
     &    +(boxpnt(2,izon)-yl)*boxpnt(ja+2,izon)
     &    +(boxpnt(3,izon)-zl)*boxpnt(ja+3,izon)
        av=unp*boxpnt(ja+1,izon)
     &    +vnp*boxpnt(ja+2,izon)
     &    +wnp*boxpnt(ja+3,izon)
        al=boxpnt(i+12,izon)
c
        IF(av.eq.0.0d0) then
          if(-ap.lt.-boxeps) goto 900
          if(ap+al.lt.-boxeps) goto 900
        ELSE
          IF(av.lt.0.0d0) then
            dl=ap/av
            if(dl.lt.-boxeps) goto 900
            ds=(ap+al)/av
          else
            dl=(ap+al)/av
            if(dl.lt.-boxeps) goto 900
            ds=ap/av
          endif
          if(rout.ge.dl) then
            rout=dl
          endif
          if(rin.le.ds) then
            rin=ds
          endif
        endif
  300 continue
      IF(rin.ge.0.0d0.and.rin.lt.dlnmax) then
        itvalm=itvalm+1
        atval(itvalm)=rin
      elseif(rin.ge.-boxeps.and.rin.lt.0.0d0) then
        itvalm=itvalm+1
        atval(itvalm)=0.0d0
      endif
      IF(rout.ge.0.0d0.and.rout.lt.dlnmax) then
        itvalm=itvalm+1
        atval(itvalm)=rout
      elseif(rout.ge.-boxeps.and.rout.lt.0.0d0) then
        itvalm=itvalm+1
        atval(itvalm)=0.0d0
      endif
      return
  900 continue
      return
      end
!--------------------last line of subroutine boxcg1.f------------------

!-------------------------------hafset.f--------------------------------
! Version: 051219-1435
! Reference: JNC TN1410 2002-201
!-----------------------------------------------------------------------
!23456789|123456789|123456789|123456789|123456789|123456789|123456789|12

! ----------------------------------------------------------------------
! Required (geometry) subroutine for use with the EGS5 Code System
! ----------------------------------------------------------------------
! This is a subroutine hafset.
      subroutine hafset(izon)
      implicit none
c
      include 'auxcommons/geom_common.f' ! geom-common file
c
      integer izon
c
      hafpnt(5,izon)=dsqrt(hafpnt(1,izon)*hafpnt(1,izon)
     &                    +hafpnt(2,izon)*hafpnt(2,izon)
     &                    +hafpnt(3,izon)*hafpnt(3,izon))
c
      if(hafpnt(5,izon).eq.0.0d0) then
        write(*,*) 'Error of HAF ',nbhaf(izon),' : Vector is 0.0'
        stop
      end if
c
      return
      end
!--------------------last line of subroutine hafset.f------------------

!-------------------------------hafcg1.f--------------------------------
! Version: 051219-1435
! Reference: JNC TN1410 2002-201
!-----------------------------------------------------------------------
!23456789|123456789|123456789|123456789|123456789|123456789|123456789|12

! ----------------------------------------------------------------------
! Required (geometry) subroutine for use with the EGS5 Code System
! ----------------------------------------------------------------------
! This is a subroutine hafcg1.
      subroutine hafcg1(izon,xl,yl,zl,unp,vnp,wnp)
      implicit none
c
      include 'auxcommons/geom_common.f' ! geom-common file
c
      double precision xl,yl,zl
      double precision unp,vnp,wnp
      integer izon
      double precision rin
      double precision als,ap,av,ar
c
      rin =-dlnmax
c
      ap=xl*hafpnt(1,izon)
     &  +yl*hafpnt(2,izon)
     &  +zl*hafpnt(3,izon)
      ar =hafpnt(4,izon)
      als=hafpnt(5,izon)
c
      av=unp*hafpnt(1,izon)
     &  +vnp*hafpnt(2,izon)
     &  +wnp*hafpnt(3,izon)
c
c     WRITE(*,*) 'ap,av,als,ar=',ap,av,als,ar
c
      IF(av.ne.0.0d0) then
        rin=(ar*als-ap)/av
ccc   ELSE
c       IF(ar*als.eq.ap) then
c         rin=0.0d0
ccc     endif
      endif
c
      IF(rin.ge.0.0d0) then
        itvalm=itvalm+1
        atval(itvalm)=rin
      elseif(rin.ge.-hafeps.and.rin.lt.0.0d0) then
        itvalm=itvalm+1
        atval(itvalm)=0.0d0
      endif
      return
      end
!--------------------last line of subroutine hafcg1.f------------------

!-------------------------------hexset.f--------------------------------
! Version: 051219-1435
! Reference: JNC TN1410 2002-201
!-----------------------------------------------------------------------
!23456789|123456789|123456789|123456789|123456789|123456789|123456789|12

! ----------------------------------------------------------------------
! Required (geometry) subroutine for use with the EGS5 Code System
! ----------------------------------------------------------------------
! This is a subroutine hafset.
      subroutine hexset(izon)
      implicit none
c
      include 'auxcommons/geom_common.f' ! geom-common file
c
      integer izon
      integer i
      double precision axsq,angdeg
c
      double precision chk1,chksqr
      data chksqr/1.0d-4/
c
      hexpnt(11,izon)=hexpnt( 4,izon)*hexpnt( 4,izon)
     &               +hexpnt( 5,izon)*hexpnt( 5,izon)
     &               +hexpnt( 6,izon)*hexpnt( 6,izon)
c
      hexpnt(12,izon)=hexpnt( 7,izon)*hexpnt( 7,izon)
      hexpnt(13,izon)=hexpnt(12,izon)
      hexpnt(14,izon)=hexpnt(12,izon)
      axsq=dsqrt(hexpnt( 8,izon)*hexpnt( 8,izon)
     &          +hexpnt( 9,izon)*hexpnt( 9,izon)
     &          +hexpnt(10,izon)*hexpnt(10,izon))

      do i=1,3
        hexpnt(i+26,izon)=hexpnt(i+3,izon)
        hexpnt(i+29,izon)=hexpnt(7,izon)*hexpnt(i+7,izon)
     &                   /axsq
        hexpnt(i+14,izon)=hexpnt(i,izon)
        hexpnt(i+17,izon)=hexpnt(i,izon)-hexpnt(i+29,izon)/2.0d0
      enddo
      angdeg= 60.0d0
      call hexrot(hexpnt(4,izon),hexpnt(30,izon),hexpnt(33,izon),angdeg)
      angdeg=-60.0d0
      call hexrot(hexpnt(4,izon),hexpnt(30,izon),hexpnt(36,izon),angdeg)
      do i=1,3
        hexpnt(i+20,izon)=hexpnt(i,izon)-hexpnt(i+32,izon)/2.0d0
        hexpnt(i+23,izon)=hexpnt(i,izon)-hexpnt(i+35,izon)/2.0d0
      enddo
c     WRITE(*,*) 'hexpnt(11,izon)=',(hexpnt(i,izon),i=11,14)
c     WRITE(*,*) 'hexpnt(15,izon)=',(hexpnt(i,izon),i=15,17)
c     WRITE(*,*) 'hexpnt(18,izon)=',(hexpnt(i,izon),i=18,20)
c     WRITE(*,*) 'hexpnt(21,izon)=',(hexpnt(i,izon),i=21,23)
c     WRITE(*,*) 'hexpnt(24,izon)=',(hexpnt(i,izon),i=24,26)
c     WRITE(*,*) 'hexpnt(27,izon)=',(hexpnt(i,izon),i=27,29)
c     WRITE(*,*) 'hexpnt(30,izon)=',(hexpnt(i,izon),i=30,32)
c     WRITE(*,*) 'hexpnt(33,izon)=',(hexpnt(i,izon),i=33,35)
c     WRITE(*,*) 'hexpnt(36,izon)=',(hexpnt(i,izon),i=36,38)
c
      if(hexpnt(11,izon).eq.0.0d0.or.
     &   axsq.eq.0.0d0.or.
     &   hexpnt(12,izon).eq.0.0d0) then
        write(*,*) 'Error of HEX ',nbhex(izon),' : Length is 0.0'
        stop
      end if
c
      chk1 = ( hexpnt( 8,izon)*hexpnt(4,izon)
     &     +   hexpnt( 9,izon)*hexpnt(5,izon)
     &     +   hexpnt(10,izon)*hexpnt(6,izon) )
     &     / (sqrt(hexpnt(11,izon))*axsq)
      if(abs(chk1).gt.chksqr) then
        write(*,*) 'Error of HEX ',nbhex(izon),' : Vector is not square'
     &             ,chk1,chksqr
        stop
      end if
      return
      end
!--------------------last line of subroutine hexset.f------------------

!-------------------------------hexcg1.f--------------------------------
! Version: 051219-1435
! Reference: JNC TN1410 2002-201
!-----------------------------------------------------------------------
!23456789|123456789|123456789|123456789|123456789|123456789|123456789|12

! ----------------------------------------------------------------------
! Required (geometry) subroutine for use with the EGS5 Code System
! ----------------------------------------------------------------------
! This is a subroutine hexcg1.
      subroutine hexcg1(izon,xl,yl,zl,unp,vnp,wnp)
      implicit none
c
      include 'auxcommons/geom_common.f' ! geom-common file
c
      double precision xl,yl,zl
      double precision unp,vnp,wnp
      integer izon
      integer i,jp,ja
      double precision rin,rout
      double precision al,ap,av,dl,ds
c
      rin =-dlnmax
      rout= dlnmax
      DO 300 i=1,4
        jp=3*i+11
        ja=3*i+23
        ap=(hexpnt(jp+1,izon)-xl)*hexpnt(ja+1,izon)
     &    +(hexpnt(jp+2,izon)-yl)*hexpnt(ja+2,izon)
     &    +(hexpnt(jp+3,izon)-zl)*hexpnt(ja+3,izon)
        av=unp*hexpnt(ja+1,izon)
     &    +vnp*hexpnt(ja+2,izon)
     &    +wnp*hexpnt(ja+3,izon)
        al=hexpnt(i+10,izon)
c
        IF(av.eq.0.0d0) then
          if(-ap.lt.-hexeps) goto 900
          if(ap+al.lt.-hexeps) goto 900
        ELSE
          IF(av.lt.0.0d0) then
            dl=ap/av
            if(dl.lt.-hexeps) goto 900
            ds=(ap+al)/av
          else
            dl=(ap+al)/av
            if(dl.lt.-hexeps) goto 900
            ds=ap/av
          endif
          if(rout.ge.dl) then
            rout=dl
          endif
          if(rin.le.ds) then
            rin=ds
          endif
        endif
  300 continue
      IF(rin.ge.0.0d0.and.rin.lt.dlnmax) then
        itvalm=itvalm+1
        atval(itvalm)=rin
      elseif(rin.ge.-hexeps.and.rin.lt.0.0d0) then
        itvalm=itvalm+1
        atval(itvalm)=0.0
      endif
      IF(rout.ge.0.0d0.and.rout.lt.dlnmax) then
        itvalm=itvalm+1
        atval(itvalm)=rout
      elseif(rout.ge.-hexeps.and.rout.lt.0.0d0) then
        itvalm=itvalm+1
        atval(itvalm)=0.0
      endif
      return
  900 continue
      return
      end
!--------------------last line of subroutine hexcg1.f------------------

!-------------------------------hexrot.f--------------------------------
! Version: 051219-1435
! Reference: JNC TN1410 2002-201
!-----------------------------------------------------------------------
!23456789|123456789|123456789|123456789|123456789|123456789|123456789|12

! ----------------------------------------------------------------------
! Required (geometry) subroutine for use with the EGS5 Code System
! ----------------------------------------------------------------------
! This is a subroutine hexrot.
      subroutine hexrot(axv,vin,vout,ang)
      implicit none
c
      double precision axv(3),vin(3),vout(3),ang
      double precision pi,axyz
      double precision c,s,ax,ay,az,xo,yo,zo,x,y,z
      DATA pi/3.14159265d0/
      c=dcos(ang*pi/180.d0)
      s=dsin(ang*pi/180.d0)
      axyz=dsqrt(axv(1)*axv(1)+axv(2)*axv(2)+axv(3)*axv(3))
      ax=axv(1)/axyz
      ay=axv(2)/axyz
      az=axv(3)/axyz
      xo=vin(1)
      yo=vin(2)
      zo=vin(3)
      call rotrot(c,s,ax,ay,az,xo,yo,zo,x,y,z)
      vout(1)=x
      vout(2)=y
      vout(3)=z
      return
      end
!--------------------last line of subroutine hexrot.f------------------

!-------------------------------rotrot.f--------------------------------
! Version: 051219-1435
! Reference: JNC TN1410 2002-201
!-----------------------------------------------------------------------
!23456789|123456789|123456789|123456789|123456789|123456789|123456789|12

! ----------------------------------------------------------------------
! Required (geometry) subroutine for use with the EGS5 Code System
! ----------------------------------------------------------------------
! This is a subroutine rotrot.

      subroutine rotrot(c,s,ax,ay,az,xo,yo,zo,x,y,z)
      implicit none
      double precision c,s,ax,ay,az,xo,yo,zo,x,y,z
      double precision cvf11,cvf21,cvf31,cvf12,cvf22,
     &                 cvf32,cvf13,cvf23,cvf33
c                                                                               
c     c=cos(ang)                                                                
c     s=sin(ang)                                                                
      cvf11=ax*ax*(1.0d0-c)+c
      cvf21=ay*ax*(1.0d0-c)-az*s
      cvf31=az*ax*(1.0d0-c)+ay*s
      cvf12=ax*ay*(1.0d0-c)+az*s
      cvf22=ay*ay*(1.0d0-c)+c
      cvf32=az*ay*(1.0d0-c)-ax*s
      cvf13=ax*az*(1.0d0-c)-ay*s
      cvf23=ay*az*(1.0d0-c)+ax*s
      cvf33=az*az*(1.0d0-c)+c
c                                                                               
      x=xo*cvf11+yo*cvf21+zo*cvf31
      y=xo*cvf12+yo*cvf22+zo*cvf32
      z=xo*cvf13+yo*cvf23+zo*cvf33
c
      return
      end
!--------------------last line of subroutine rotrot.f------------------

!-------------------------------tecset.f--------------------------------
! Version: 051219-1435
! Reference: JNC TN1410 2002-201
!-----------------------------------------------------------------------
!23456789|123456789|123456789|123456789|123456789|123456789|123456789|12

! ----------------------------------------------------------------------
! Required (geometry) subroutine for use with the EGS5 Code System
! ----------------------------------------------------------------------
! This is a subroutine tecset.
      subroutine tecset(izon)
      implicit none
c
      include 'auxcommons/geom_common.f' ! geom-common file
c
      integer izon
      double precision chk1,chk2,chk3,chksqr
      data chksqr/1.0d-4/

      tecpnt(14,izon)=tecpnt(4,izon)*tecpnt(4,izon)
     &               +tecpnt(5,izon)*tecpnt(5,izon)
     &               +tecpnt(6,izon)*tecpnt(6,izon)
      tecpnt(15,izon)=tecpnt(7,izon)*tecpnt(7,izon)
     &               +tecpnt(8,izon)*tecpnt(8,izon)
     &               +tecpnt(9,izon)*tecpnt(9,izon)
      tecpnt(16,izon)=tecpnt(10,izon)*tecpnt(10,izon)
     &               +tecpnt(11,izon)*tecpnt(11,izon)
     &               +tecpnt(12,izon)*tecpnt(12,izon)
      if(tecpnt(14,izon).eq.0.0d0.or.
     &   tecpnt(15,izon).eq.0.0d0.or.
     &   tecpnt(16,izon).eq.0.0d0) then
        write(*,*) 'Error of TEC ',nbtec(izon),': Length is 0.0'
        stop
      end if
c
      chk1 = ( tecpnt( 7,izon)*tecpnt(4,izon)
     &     +   tecpnt( 8,izon)*tecpnt(5,izon)
     &     +   tecpnt( 9,izon)*tecpnt(6,izon) )
     &     / sqrt(tecpnt(14,izon)*tecpnt(15,izon))
      chk2 = ( tecpnt(10,izon)*tecpnt(4,izon)
     &     +   tecpnt(11,izon)*tecpnt(5,izon)
     &     +   tecpnt(12,izon)*tecpnt(6,izon) )
     &     / sqrt(tecpnt(14,izon)*tecpnt(16,izon))
      chk3 = ( tecpnt(10,izon)*tecpnt(7,izon)
     &     +   tecpnt(11,izon)*tecpnt(8,izon)
     &     +   tecpnt(12,izon)*tecpnt(9,izon) )
     &     / sqrt(tecpnt(15,izon)*tecpnt(16,izon))
      if(abs(chk1).gt.chksqr.or.
     &   abs(chk2).gt.chksqr.or.
     &   abs(chk3).gt.chksqr) then
        write(*,*) 'Error of TEC ',nbtec(izon),' : Vector is not square'
     &            ,chk1,chk2,chk3,chksqr
        stop
      end if
      return
      end
!--------------------last line of subroutine tecset.f------------------

!-------------------------------teccg1.f--------------------------------
! Version: 051219-1435
! Reference: JNC TN1410 2002-201
!-----------------------------------------------------------------------
!23456789|123456789|123456789|123456789|123456789|123456789|123456789|12

! ----------------------------------------------------------------------
! Required (geometry) subroutine for use with the EGS5 Code System
! ----------------------------------------------------------------------
! This is a subroutine teccg1.
      subroutine teccg1(izon,xl,yl,zl,unp,vnp,wnp)
      implicit none
c
      include 'auxcommons/geom_common.f' ! geom-common file
c
      double precision xl,yl,zl
      double precision unp,vnp,wnp
      integer izon
      integer lro,lri,lcp,lcm
      double precision rin,rout
      double precision vph,vphhh
      double precision ambd,ambda,um,umu,den,disc
      double precision sd,f1,r1,r2,hh,wh,cp,cm
c
      double precision rs2,rl2,plx,ply,plz,vrs,vrl,wrs,wrl,wrs2,wrl2
      double precision rs4,rl4,dlttec
c
      double precision plxn,plyn,plzn,vphn
      double precision rrw,rrw2
      double precision rrwr,rrwr2
      double precision rrf,rrf2
      double precision wph,wph2,h4
      double precision vrsn,vrln

      DATA dlttec/1.0d-6/
c
      rin =-dlnmax
      rout= dlnmax
      lro=0
      lri=0
c
c4    COMPUTE DOT PRODUCTS OF P.P AND Q.Q
c
      rrw=tecpnt(13,izon)
      hh =tecpnt(14,izon)
      rs2=tecpnt(15,izon)
      rl2=tecpnt(16,izon)
      h4 =hh*hh
      rs4=rs2*rs2
      rl4=rl2*rl2
      rrwr=1.0d0-rrw
      rrwr2=rrwr*rrwr
c
c5    COMPUTE (V-XB) FOR X,Y,Z COORDINATES
c
      plx=(xl-tecpnt(1,izon))
      ply=(yl-tecpnt(2,izon))
      plz=(zl-tecpnt(3,izon))
c
c6    TRANSFORM XL,YL,ZL TO THE COORDINATES OF THE TEC
c
      vph=plx*tecpnt( 4,izon)+ply*tecpnt( 5,izon)+plz*tecpnt( 6,izon)
      vrs=plx*tecpnt( 7,izon)+ply*tecpnt( 8,izon)+plz*tecpnt( 9,izon)
      vrl=plx*tecpnt(10,izon)+ply*tecpnt(11,izon)+plz*tecpnt(12,izon)
      rrf =1.0d0-rrwr*vph/hh
      rrf2=rrf*rrf
      rrw2=rrw*rrw
c
      um =rl4*vrs*vrs*h4+rs4*vrl*vrl*h4-rs4*rl4*vph*vph*rrwr2
     &   +2.0d0*rs4*rl4*hh*rrwr*vph - rs4*rl4*h4
      wh =unp*tecpnt(4,izon)+vnp*tecpnt(5,izon)+wnp*tecpnt(6,izon)
c
      wph=unp*tecpnt( 4,izon)+vnp*tecpnt( 5,izon)+wnp*tecpnt( 6,izon)
      wrs=unp*tecpnt( 7,izon)+vnp*tecpnt( 8,izon)+wnp*tecpnt( 9,izon)
      wrl=unp*tecpnt(10,izon)+vnp*tecpnt(11,izon)+wnp*tecpnt(12,izon)
      wph2=wph*wph
      wrs2=wrs*wrs
      wrl2=wrl*wrl
      ambd=wrs*vrs*rl4*h4+wrl*vrl*rs4*h4-wph*vph*rs4*rl4*rrwr2
     &    +wph*rs4*rl4*hh*rrwr
      den=wrs2*rl4*h4+wrl2*rs4*h4-rs4*rl4*rrwr2*wph2
      if(dabs(den).le.dlttec) then
        IF(dabs(ambd).le.dlttec) then
          r1=-dlnmax
          r2= dlnmax
        else
          r1 = -um/ambd/2.0d0
          r2 = -um/ambd/2.0d0
        endif
      else
        ambda=ambd/den
        umu=um/den
        disc=ambda**2-umu
        if(disc.lt.0.0d0) goto 250
c
c8    COMPUTE THE INTERSECT POINTS ON THE QUADRATIC SURFACE
c
        if(disc.eq.0.0d0) then
          r1=-ambda
          r2=-ambda
        else
          sd=dsqrt(disc)
          r1=-ambda-sd
          r2=-ambda+sd
        endif
      endif
      IF(r1.ge.-teceps.and.r1.lt.dlnmax) then
        plxn=(r1*unp+xl-tecpnt(1,izon))
        plyn=(r1*vnp+yl-tecpnt(2,izon))
        plzn=(r1*wnp+zl-tecpnt(3,izon))
        vphn=plxn*tecpnt( 4,izon)+plyn*tecpnt( 5,izon)
     &      +plzn*tecpnt( 6,izon)
        IF(vphn.ge.-teceps.and.vphn.le.(hh+teceps)*(1.0d0+teceps)) then
          if(r1.gt.0.0) then
            itvalm=itvalm+1
            atval(itvalm)=r1
          else
            itvalm=itvalm+1
            atval(itvalm)=0.0d0
          endif
        endif
      endif
      IF(r2.ge.-teceps.and.r2.lt.dlnmax.and.r2.ne.r1) then
        plxn=(r2*unp+xl-tecpnt(1,izon))
        plyn=(r2*vnp+yl-tecpnt(2,izon))
        plzn=(r2*wnp+zl-tecpnt(3,izon))
        vphn=plxn*tecpnt( 4,izon)+plyn*tecpnt( 5,izon)
     &      +plzn*tecpnt( 6,izon)
        IF(vphn.ge.-teceps.and.vphn.le.(hh+teceps)*(1.0d0+teceps)) then
          if(r2.gt.0.0) then
            itvalm=itvalm+1
            atval(itvalm)=r2
          else
            itvalm=itvalm+1
            atval(itvalm)=0.0d0
          endif
        endif
      endif
  250 continue
c
c9    DETERMINE IF RAY PARALLEL TO PLANAR SURFACES
c
      if(wh.lt.0.0d0) then
        if(vph.lt.-teceps) goto 300
c
c10   COMPUTE THE INTERSECT POINTS ON THE PLANAR SURFACES
c
        cp=-vph/wh
        cm=(-vph+hh)/wh
        lcp=1
        lcm=2
      ELSEIF(wh.gt.0.0d0) then
        vphhh=-vph+hh
        if(vphhh.lt.-teceps) goto 300
        cp=vphhh/wh
        cm=-vph/wh
        lcm=1
        lcp=2
      else
        cp=dlnmax
        cm=-cp
      endif
c
      IF(lcm.le.2) then
        IF(cm.ge.-teceps.and.cm.lt.dlnmax) then
          plxn=(cm*unp+xl-tecpnt(1,izon))
          plyn=(cm*vnp+yl-tecpnt(2,izon))
          plzn=(cm*wnp+zl-tecpnt(3,izon))
          vrsn=plxn*tecpnt( 7,izon)+plyn*tecpnt( 8,izon)
     &        +plzn*tecpnt( 9,izon)
          vrln=plxn*tecpnt(10,izon)+plyn*tecpnt(11,izon)
     &        +plzn*tecpnt(12,izon)
          if(lcm.eq.1) then
            f1=rl4*vrsn*vrsn+rs4*vrln*vrln-rs4*rl4
          else
            f1=rl4*vrsn*vrsn+rs4*vrln*vrln-rs4*rl4*rrw2
          endif
          IF(f1.le.teceps) then
            if(cm.gt.0.0d0) then
              itvalm=itvalm+1
              atval(itvalm)=cm
            else
              itvalm=itvalm+1
              atval(itvalm)=0.0d0
            endif
          endif
        endif
      endif
c
      IF(lcp.le.2) then
        IF(cp.ge.-teceps.and.cp.lt.dlnmax) then
          plxn=(cp*unp+xl-tecpnt(1,izon))
          plyn=(cp*vnp+yl-tecpnt(2,izon))
          plzn=(cp*wnp+zl-tecpnt(3,izon))
          vrsn=plxn*tecpnt( 7,izon)+plyn*tecpnt( 8,izon)
     &        +plzn*tecpnt( 9,izon)
          vrln=plxn*tecpnt(10,izon)+plyn*tecpnt(11,izon)
     &        +plzn*tecpnt(12,izon)
          if(lcp.eq.1) then
            f1=rl4*vrsn*vrsn+rs4*vrln*vrln-rs4*rl4
          else
            f1=rl4*vrsn*vrsn+rs4*vrln*vrln-rs4*rl4*rrw2
          endif
          IF(f1.le.teceps) then
            if(cp.gt.0.0d0) then
              itvalm=itvalm+1
              atval(itvalm)=cp
            else
              itvalm=itvalm+1
              atval(itvalm)=0.0d0
            endif
          endif
        endif
      endif
c
  300 continue
      return
      end
!--------------------last line of subroutine teccg1.f------------------

!-------------------------------gelset.f--------------------------------
! Version: 060919-1435
! Reference: JNC TN1410 2002-201
!-----------------------------------------------------------------------
!23456789|123456789|123456789|123456789|123456789|123456789|123456789|12

! ----------------------------------------------------------------------
! Required (geometry) subroutine for use with the EGS5 Code System
! ----------------------------------------------------------------------
! This is a subroutine gelset.
      subroutine gelset(izon)
      implicit none
c
      include 'auxcommons/geom_common.f' ! geom-common file
c
      integer izon
      double precision chk1,chk2,chk3,chksqr
      data chksqr/1.0d-4/
c
      gelpnt(13,izon) = sqrt(gelpnt(4,izon)*gelpnt(4,izon)
     &                     + gelpnt(5,izon)*gelpnt(5,izon)
     &                     + gelpnt(6,izon)*gelpnt(6,izon))
      gelpnt(14,izon) = sqrt(gelpnt(7,izon)*gelpnt(7,izon)
     &                     + gelpnt(8,izon)*gelpnt(8,izon)
     &                     + gelpnt(9,izon)*gelpnt(9,izon))
      gelpnt(15,izon) = sqrt(gelpnt(10,izon)*gelpnt(10,izon)
     &                     + gelpnt(11,izon)*gelpnt(11,izon)
     &                     + gelpnt(12,izon)*gelpnt(12,izon))
      if(gelpnt(13,izon).eq.0.0d0) then
        write(*,*) 'Error of GEL ',nbgel(izon),' : Vector R1 is 0.0'
        stop
      end if
      if(gelpnt(14,izon).eq.0.0d0) then
        write(*,*) 'Error of GEL ',nbgel(izon),' : Vector R2 is 0.0'
        stop
      end if
      if(gelpnt(15,izon).eq.0.0d0) then
        write(*,*) 'Error of GEL ',nbgel(izon),' : Vector R3 is 0.0'
        stop
      end if
c
      chk1 = ( gelpnt( 7,izon)*gelpnt(4,izon)
     &     +   gelpnt( 8,izon)*gelpnt(5,izon)
     &     +   gelpnt( 9,izon)*gelpnt(6,izon) )
     &     / sqrt(gelpnt(13,izon)*gelpnt(14,izon))
      chk2 = ( gelpnt(10,izon)*gelpnt(4,izon)
     &     +   gelpnt(11,izon)*gelpnt(5,izon)
     &     +   gelpnt(12,izon)*gelpnt(6,izon) )
     &     / sqrt(gelpnt(13,izon)*gelpnt(15,izon))
      chk3 = ( gelpnt(10,izon)*gelpnt(7,izon)
     &     +   gelpnt(11,izon)*gelpnt(8,izon)
     &     +   gelpnt(12,izon)*gelpnt(9,izon) )
     &     / sqrt(gelpnt(14,izon)*gelpnt(15,izon))
      if(abs(chk1).gt.chksqr.or.
     &   abs(chk2).gt.chksqr.or.
     &   abs(chk3).gt.chksqr) then
        write(*,*) 'Error of GEL ',nbgel(izon),' : Vector is not square'
     &             ,chk1,chk2,chk3,chksqr
        stop
      end if
      return
      end
!--------------------last line of subroutine gelset.f------------------

!-------------------------------gelcg1.f--------------------------------
! Version: 080912-0841
! Reference: JNC TN1410 2002-201
!-----------------------------------------------------------------------
!23456789|123456789|123456789|123456789|123456789|123456789|123456789|12

! ----------------------------------------------------------------------
! Required (geometry) subroutine for use with the EGS5 Code System
! ----------------------------------------------------------------------
! This is a subroutine gelcg1.
! ----------------------------------------------------------------------
      subroutine gelcg1(izon,xl,yl,zl,unp,vnp,wnp)
      implicit none
c
      include 'auxcommons/geom_common.f' ! geom-common file
c
      double precision xl,yl,zl
      double precision unp,vnp,wnp
      integer izon
      double precision a,a1,a2,a3,b,b1,b2,b3,c
      double precision s,rin,rout
      rin = dlnmax
      rout=-dlnmax
      a1=(unp*gelpnt( 4,izon)+vnp*gelpnt( 5,izon)+wnp*gelpnt( 6,izon))
     &  /gelpnt(13,izon)/gelpnt(13,izon)
      a2=(unp*gelpnt( 7,izon)+vnp*gelpnt( 8,izon)+wnp*gelpnt( 9,izon))
     &  /gelpnt(14,izon)/gelpnt(14,izon)
      a3=(unp*gelpnt(10,izon)+vnp*gelpnt(11,izon)+wnp*gelpnt(12,izon))
     &  /gelpnt(15,izon)/gelpnt(15,izon)
      b1=((xl-gelpnt(1,izon))*gelpnt( 4,izon)
     &   +(yl-gelpnt(2,izon))*gelpnt( 5,izon)
     &   +(zl-gelpnt(3,izon))*gelpnt( 6,izon))
     &  /gelpnt(13,izon)/gelpnt(13,izon)
      b2=((xl-gelpnt(1,izon))*gelpnt( 7,izon)
     &   +(yl-gelpnt(2,izon))*gelpnt( 8,izon)
     &   +(zl-gelpnt(3,izon))*gelpnt( 9,izon))
     &  /gelpnt(14,izon)/gelpnt(14,izon)
      b3=((xl-gelpnt(1,izon))*gelpnt(10,izon)
     &   +(yl-gelpnt(2,izon))*gelpnt(11,izon)
     &   +(zl-gelpnt(3,izon))*gelpnt(12,izon))
     &  /gelpnt(15,izon)/gelpnt(15,izon)
      a=a1*a1+a2*a2+a3*a3
      b=a1*b1+a2*b2+a3*b3
      c=b1*b1+b2*b2+b3*b3-1.0d0
      if(a.ne.0.0d0) then
        s=(b*b-a*c)
        if(s.ge.0.0d0) then
          s=dsqrt(s)
          rin =(-b-s)/a
          rout=(-b+s)/a
          IF(rin.ge.0.0d0) then
            itvalm=itvalm+1
            atval(itvalm)=rin
          elseif(rin.ge.-geleps.and.rin.lt.0.0d0) then
            itvalm=itvalm+1
            atval(itvalm)=0.0d0
          endif
          IF(rout.ge.0.0d0) then
            itvalm=itvalm+1
            atval(itvalm)=rout
          elseif(rout.ge.-geleps.and.rout.lt.0.0d0) then
            itvalm=itvalm+1
            atval(itvalm)=0.0d0
          endif
        endif
      elseif(b.ne.0.0d0) then
        rin=-c/(b*2.0)
        IF(rin.ge.0.0d0) then
          itvalm=itvalm+1
          atval(itvalm)=rin
        elseif(rin.ge.-geleps.and.rin.lt.0.0d0) then
          itvalm=itvalm+1
          atval(itvalm)=0.0d0
        endif
      endif
      RETURN
      end
!--------------------last line of subroutine gelcg1.f------------------
