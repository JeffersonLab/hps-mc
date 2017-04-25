!----------------------------- geomout.f--------------------------------
! Version: 060118-1515
! Reference: 040628-1700 by H. Hirayama and Y. Namito
!-----------------------------------------------------------------------
!23456789|123456789|123456789|123456789|123456789|123456789|123456789|12

! ----------------------------------------------------------------------
! Auxiliary (for PICT) subroutine for use with the EGS5 Code System
! ----------------------------------------------------------------------
! Subroutine geomout is called from main program and ausgab to output 
! geometry related information for PICT. 
! ----------------------------------------------------------------------
! Input arguments:
! ----------------
!      ncylg  : number of cylinders to draw
!      nplang : number of planes to draw
! ----------------------------------------------------------------------

      subroutine geomout(ncylg,nplang)
      
      include 'auxcommons/aux_h.f'        ! Auxiliary-code "header" file
      include 'auxcommons/cyldta.f'              ! Auxiliary-code COMMON
      include 'auxcommons/pladta.f'              ! Auxiliary-code COMMON
      include 'auxcommons/nfac.f'                ! Auxiliary-code COMMON

      integer ncylg,nplang                       ! Arguments

      real*8 cyl(MXCYLS),zbin(MXPLNS),ybin(MXPLNS),xbin(MXPLNS)
      real*8 pcv,pcv1,pcv2,pcv3,pnv,pnv1,pnv2,pnv3,zwid
      integer i,nxp,nyp,nzp
      
! ---------------------
! I/O format statements
! ---------------------
10     FORMAT('GSTA')
20     FORMAT('CYLS')
30     FORMAT(3I6)
40     FORMAT(4E15.7)
50     FORMAT('GEND')
60     FORMAT('SLAB')

      if (ncylg.ne.0) then                     ! Cylinder slab geometry
        write(39,10)
        write(39,20)
        
        if (nplang.eq.0) then
          nzp=2
          zbin(1)=zmin
          zbin(2)=zmax

        else 
          nzp=0
          do i=1,nplang
            pnv=pnorm(3,i)
            pcv=pcoord(3,i)
            if (pnv.eq.1.and.(pcv.ge.zmin.and.pcv.le.zmax)) then
              nzp=nzp+1
              zbin(nzp)=pcv
            end if
          end do
        end if
        
        write(39,30) ncylg,nzp
        
        do i=1,ncylg
          cyl(i)=cyrad(i)
        end do
        
        write(39,40) (cyl(i), i=1,ncylg)
        write(39,40) (zbin(i),i=1,nzp)
        write(39,50)
        
      else if (nplang.ne.0) then               ! Plane geometry
        write(39,10)
        write(39,60)
        
        nzp=0
        nyp=0
        nxp=0
        
        do i=1,nplang
          pnv1=pnorm(1,i)
          pcv1=pcoord(1,i)
          pnv2=pnorm(2,i)
          pcv2=pcoord(2,i)
          pnv3=pnorm(3,i)
          pcv3=pcoord(3,i)
          if(pnv1.eq.1) then
            if(pcv1.ge.xmin.and.pcv1.le.xmax) then
              nxp=nxp+1
              xbin(nxp)=pcv1
            end if
          else if (pnv2.eq.1) then
            if (pcv2.ge.ymin.and.pcv2.le.ymax) then
              nyp=nyp+1
              ybin(nyp)=pcv2
            end if
          else 
            if(pcv3.ge.zmin.and.pcv3.le.zmax) then
              nzp=nzp+1
              zbin(nzp)=pcv3
            end if
          end if
        end do
        
        zwid=abs(zmax-zmin)
        if (nxp.eq.0) then
          xbin(1)=-zwid/2.0
          xbin(2)= zwid/2.0
        end if
        if (nyp.eq.0) then
          ybin(1)=-zwid/2.0
          ybin(2)= zwid/2.0
        end if
        write(39,30) nxp,nyp,nzp
        write(39,40) (xbin(i), i=1,nxp)
        write(39,40) (ybin(i), i=1,nyp)
        write(39,40) (zbin(i), i=1,nzp)
        write(39,50)
      else                                    ! Not produce geometry
        write(39,10)
        write(39,50)
      end if
      
      return
      end
      
!----------------------last line of geomout.f------------------------
