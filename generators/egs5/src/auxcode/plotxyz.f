!----------------------------- plotxyz.f--------------------------------
! Version: 060620-1400
!          070817-1500    Include time information
!          080425-1600
! Reference:  by H. Hirayama and Y. Namito
!-----------------------------------------------------------------------
!23456789|123456789|123456789|123456789|123456789|123456789|123456789|12

! ----------------------------------------------------------------------
! Auxiliary (plot) subroutine for use with the EGS5 Code System
! ----------------------------------------------------------------------
! Subroutine plotxyz is called from main program and ausgab to output 
! X,Y,Z,IQ,E,IR,WT,TIME for 3 dimensional graphic display on PC. 
! This subroutine is based on PLOTXZ developed at SLAC for 2 dimensional
! display with UG.
! ----------------------------------------------------------------------
! Input arguments:
! ----------------
!   iarg = Indicates the situation under which ausgab is being called
!          99 means output buffered data
!   np   = Stack pointer
!   iq   = Integer charge of particle
!   x,y,z= Position of particle
!   enp  = Kinetic energy of particle
!   ir   = Index of particle's current region
!   wt   = Statistical weight of current particle used for CGView only
!   time = Time after start in seconds
! ----------------------------------------------------------------------

      subroutine plotxyz(iarg,np,iq,x,y,z,enp,ir,wt,time)

      include 'include/egs5_h.f'               ! Main EGS5 "header" file

      include 'auxcommons/nfac.f'              ! Auxiliary-code COMMON

                                               ! Arguments
      real*8 enp,x,y,z,wt,time
      integer iarg,iq,ir,np

                                               ! Local variables
      integer iept(100,MXSTACK),iqtold(MXSTACK),irpt(100,MXSTACK),
     &        ixpt(100,MXSTACK),
     *        iypt(100,MXSTACK),izpt(100,MXSTACK),npt(MXSTACK)
     
      real*8 ept(100,MXSTACK),wtpt(100,MXSTACK),xpt(100,MXSTACK),
     &       ypt(100,MXSTACK),
     *       zpt(100,MXSTACK),tpt(100,MXSTACK)
 
      real*8 eee
      integer i,iiq,inp,jarg,iff,ief
      
      data npt/MXSTACK*0/
      
! ---------------------
! I/O format statements
! ---------------------
100         FORMAT(I1,4I5)
110         FORMAT(I1,3I8,I5)
120         FORMAT(I1,3(1PE13.6),1PE10.3,I4,1PE10.3,1PE12.5)
130         FORMAT('-1')
140         FORMAT(I1,3(1PE14.6),1PE11.3,I10,1PE11.3,1PE12.5)

      if (npreci.lt.0) then
        return                      ! check display type
      end if
      if (iarg.eq.99) then                        ! Output buffered data
        do i=1, MXSTACK
          if(npt(i).le.0) go to 1
          if (iqtold(i).eq.0) then
             iiq=1
          else if (iqtold(i).eq.-1) then
             iiq=2
          else
             iiq=3
          end if
          
          if (npreci.ne.0) then
             iiq=iiq+3                              ! use different iiq 
          end if
          
          do inp=1,npt(i)
            if (npreci.eq.0)  then
              write(39,100) iiq,ixpt(inp,i),iypt(inp,i),izpt(inp,i),
     *        iept(inp,i)
            else if (npreci.eq.1) then
              write(39,110) iiq,ixpt(inp,i),iypt(inp,i),izpt(inp,i),
     *        iept(inp,i)
            else if (npreci.eq.2) then
              write(39,120) iiq,xpt(inp,i),ypt(inp,i),zpt(inp,i),
     *        ept(np,i),irpt(inp,i),wtpt(inp,i),tpt(inp,i)
            else 
              write(39,140) iiq,xpt(inp,i),ypt(inp,i),zpt(inp,i),
     *        ept(np,i),irpt(inp,i),wtpt(inp,i),tpt(inp,i)
            end if
            
            if (inp.eq.npt(i)) then
              write(39,130)
            end if
            npt(i)=0
          end do
          
1         continue
        end do
      
      else                                      ! iarg ne 99
        jarg=iarg
        npt(np)=npt(np) + 1
        if (npt(np).eq.1) iqtold(np)=iq
        if (npreci.eq.0) then                   ! 16 bitsPICT
          ixpt(npt(np),np)=x/fnorm*10000+50000
          iypt(npt(np),np)=y/fnorm*10000+50000
          izpt(npt(np),np)=z/fnorm*10000+50000
        else if (npreci.eq.1)  then             ! 32 bits PICT 
          ixpt(npt(np),np)=x/fnorm*8388608+33554432
          iypt(npt(np),np)=y/fnorm*8388608+33554432
          izpt(npt(np),np)=z/fnorm*8388608+33554432
        else
          xpt(npt(np),np)=x
          ypt(npt(np),np)=y
          zpt(npt(np),np)=z
        end if
        
        if (npreci.le.1)  then                 ! PICT 
          if (iq.eq.0)  then                   ! photon
            eee=enp*1000 
          else                                 ! charged particle
            eee=(enp-0.511)*1000.
          end if

          if  (eee.lt.10000.0) then 
            iept(npt(np),np)=int(eee)*10
          else 
            iff=log10(eee)-3
            ief=eee/10**iff
            iept(npt(np),np)=ief*10+iff
          end if 
        
        else                                    ! CGVIEW
          if (iq.eq.0) then 
            ept(npt(np),np)=enp 
          else
            ept(npt(np),np)=enp-0.511
          end if

          wtpt(npt(np),np)=wt
          tpt(npt(np),np)=time
          irpt(npt(np),np)=ir
        end if
        if (iq.ne.iqtold(np)) jarg=-1          ! particle type changes
        if (npt(np).ge.100.or.jarg.ne.0) then
          if (iqtold(np).eq.0) then
            iiq=1
          else if (iqtold(np).eq.-1) then
            iiq=2
          else
            iiq=3
          end if
          if (npreci.ne.0) then                ! not 16 bits PICT
            iiq=iiq+3
          end if
          if (npt(np).ge.1) then 
            do inp=1,npt(np)
              if (npreci.eq.0) then
      ! A particle energy is set at that of starting point of each line.
                if (jarg.ne.0.and.(inp.gt.1.and.inp.eq.npt(np))) then
                  write(39,100) iiq,ixpt(inp,np),iypt(inp,np),
     *            izpt(inp,np),iept(inp-1,np)
                else
                  write(39,100) iiq,ixpt(inp,np),iypt(inp,np),
     *            izpt(inp,np),iept(inp,np)
                end if
              else if (npreci.eq.1) then
      ! A particle energy is set at that of starting point of each line.
                if (jarg.ne.0.and.(inp.gt.1.and.inp.eq.npt(np))) then
                  write(39,110) iiq,ixpt(inp,np),iypt(inp,np),
     *            izpt(inp,np),iept(inp-1,np)
                else
                  write(39,110) iiq,ixpt(inp,np),iypt(inp,np),
     *            izpt(inp,np),iept(inp,np)
                end if
              else if (npreci.eq.2) then
      ! A particle energy is set at that of starting point of each line.
                if (jarg.ne.0.and.(inp.gt.1.and.inp.eq.npt(np))) then
                  write(39,120) iiq,xpt(inp,np),ypt(inp,np),zpt(inp,np),
     *             ept(inp-1,np),irpt(inp,np),wtpt(inp-1,np),
!     *             tpt(inp-1,np)
     *             tpt(inp,np)
                else
                  write(39,120) iiq,xpt(inp,np),ypt(inp,np),zpt(inp,np),
     *             ept(inp,np),irpt(inp,np),wtpt(inp,np),tpt(inp,np)
                end if
              else
      ! A particle energy is set at that of starting point of each line.
                if (jarg.ne.0.and.(inp.gt.1.and.inp.eq.npt(np))) then
                  write(39,140) iiq,xpt(inp,np),ypt(inp,np),zpt(inp,np),
     *             ept(inp-1,np),irpt(inp,np),wtpt(inp-1,np),
!     *             tpt(inp-1,np)
     *             tpt(inp,np)
                else
                  write(39,140) iiq,xpt(inp,np),ypt(inp,np),zpt(inp,np),
     *             ept(inp,np),irpt(inp,np),wtpt(inp,np),tpt(inp,np)
                end if
              end if
              if (inp.eq.npt(np)) then
                write(39,130)
              end if
            end do
          end if

          if (jarg.gt.0.or.iarg.gt.0) then  ! 070817-comment
! jarg.gt.0 can be deleted.
            npt(np)=0
          else if (jarg.eq.-1) then
            if (npreci.le.1) then
              ixpt(1,np)=ixpt(npt(np),np)
              iypt(1,np)=iypt(npt(np),np)
              izpt(1,np)=izpt(npt(np),np)
              iept(1,np)=iept(npt(np),np)
            else
              xpt(1,np)=xpt(npt(np),np)
              ypt(1,np)=ypt(npt(np),np)
              zpt(1,np)=zpt(npt(np),np)
              ept(1,np)=ept(npt(np),np)
              wtpt(1,np)=wtpt(npt(np),np)
              tpt(1,np)=tpt(npt(np),np)
              irpt(1,np)=irpt(npt(np),np)
            end if
            npt(np)=1
            iqtold(np)=iq
          else
            npt(np)=1
            if (npreci.le.1) then
              ixpt(1,np)=ixpt(100,np)
              iypt(1,np)=iypt(100,np)
              izpt(1,np)=izpt(100,np)
              iept(1,np)=iept(100,np)
            else
              xpt(1,np)=xpt(100,np)
              ypt(1,np)=ypt(100,np)
              zpt(1,np)=zpt(100,np)
              ept(1,np)=ept(100,np)
              wtpt(1,np)=wtpt(100,np)
              tpt(1,np)=tpt(100,np)
              irpt(1,np)=irpt(100,np)
            end if
          end if
        else
          iqtold(np)=iq
        end if
      end if
      return
      end

!----------------------last line of plotxyz.f------------------------
