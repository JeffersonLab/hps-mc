!-----------------------------rslts.f-----------------------------------
!  Version: 060317-1400
!-----------------------------------------------------------------------
!23456789|123456789|123456789|123456789|123456789|123456789|123456789|12

! ----------------------------------------
! PEGS5 common file for RSLTS
! ----------------------------------------

      integer nel,ngl,ngr,ngs,ngc,ngcs,ngx,neii
      double precision ebinda, axe,bxe,afe,bfe, axg,bxg,afg,bfg,  
     &                         axr,bxr,afr,bfr, axs,bxs,afs,bfs,
     &                         axc,bxc,afc,bfc, axcs,bxcs,afcs,bfcs,
     &                         axx,bxx,afx,bfx, axeii,bxeii,afeii,bfeii

      COMMON/RSLTS/EBINDA,  AXE,BXE,AFE(150,15),BFE(150,15), 
     &                      AXG,BXG,AFG(1000,4),BFG(1000,4),
     &                      AXR,BXR,AFR(100),BFR(100),
     &                      AXS,BXS,AFS(100),BFS(100), 
     &                      AXC,BXC,AFC(2000),BFC(2000),
     &                      AXCS,BXCS,AFCS(2000,200),BFCS(2000,200),
     &                      AXX,BXX,AFX(1000,20),BFX(1000,20), 
     &                      AXEII,BXEII,AFEII(150,20),BFEII(150,20),
     &                      NEL,NGL,NGR,NGS,NGC,NGCS,NGX,NEII


!--------------------------last line------------------------------------
