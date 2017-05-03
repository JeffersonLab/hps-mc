!-----------------------
! madgraph - a Feynman Diagram package by Tim Stelzer and Bill Long 
! (c) 1993
!
! Filename: fsums.f
!-----------------------
 
      Subroutine ffsum(color,iterm,ifact1,element1,ifact2,element2)
!**************************************************************************
!     Does the sum over two gluons in T matrices
!**************************************************************************
      implicit none

! Constants

      include 'params.inc'
c      integer   nc
c      parameter(nc=3)
c      integer   maxlines,  maxfactors,  maxterms
c      parameter(maxlines=8,maxfactors=9,maxterms=250)

! Arguments

      integer color(0:2*maxlines,0:maxfactors,0:maxterms)
      integer ifact1,ifact2,element1,element2,iterm

! Local Variables

      integer nterms,nfactors
      integer ielement,nadded

!-----------
! Begin Code
!-----------
!     Add First Term
      call copyterm(color,iterm,ifact1,ifact2)          !first make a copy
      nterms=color(0,0,0)

!     Add First Factor in Term
      color(0,0,nterms)=color(0,0,nterms)+1             !add one factor
      nfactors=color(0,0,nterms)
      nadded=0
      do ielement=element1+1,color(0,ifact1,iterm)      !add remaining glue's
         nadded=nadded+1
         color(nadded,nfactors,nterms)=color(ielement,ifact1,iterm)
      enddo
      do ielement=1,element1-1                          !add remaining glue's
         nadded=nadded+1
         color(nadded,nfactors,nterms)=color(ielement,ifact1,iterm)
      enddo
      do ielement=element2+1,color(0,ifact2,iterm)      !add remaining glue's
         nadded=nadded+1
         color(nadded,nfactors,nterms)=color(ielement,ifact2,iterm)
      enddo
      do ielement=1,element2-1                          !add remaining glue's
         nadded=nadded+1
         color(nadded,nfactors,nterms)=color(ielement,ifact2,iterm)
      enddo
      color(2,1,nterms)= color(2,1,nterms)*2
      color(0,nfactors,nterms) = nadded                 !number of elements


!     Add Second Term
      call copyterm(color,iterm,ifact1,ifact2)          !first make a copy
      nterms=color(0,0,0)

!     Add First Factor in Term
      color(0,0,nterms)=color(0,0,nterms)+1             !add one factor
      nfactors=color(0,0,nterms)
      nadded=0
      do ielement=element1+1,color(0,ifact1,iterm)      !add remaining glue's
         nadded=nadded+1
         color(nadded,nfactors,nterms)=color(ielement,ifact1,iterm)
      enddo
      do ielement=1,element1-1                          !add remaining glue's
         nadded=nadded+1
         color(nadded,nfactors,nterms)=color(ielement,ifact1,iterm)
      enddo
      color(0,nfactors,nterms) = nadded                 !number of elements

!     Add Second Factor
      color(0,0,nterms)=color(0,0,nterms)+1             !add one factor
      nfactors=color(0,0,nterms)
      nadded=0
      do ielement=element2+1,color(0,ifact2,iterm)      !add remaining glue's
         nadded=nadded+1
         color(nadded,nfactors,nterms)=color(ielement,ifact2,iterm)
      enddo
      do ielement=1,element2-1                          !add remaining glue's
         nadded=nadded+1
         color(nadded,nfactors,nterms)=color(ielement,ifact2,iterm)
      enddo
      color(0,nfactors,nterms) = nadded                 !number of elements
      color(2,1,nterms)= -color(2,1,nterms)*Nc*2
c      if (nc .lt. 0) color(1,1,nterms) = 0              !Large NC limit
      color(1,1,iterm) = 0                              !this means clear

      call  ClearZeros(color)
      end

      Subroutine fsum(color,iterm,ifact1,element1,ifact2,element2)
!**************************************************************************
!     Does the sum over two gluons in T matrices
!**************************************************************************
      implicit none

! Constants

      include 'params.inc'
c      integer   nc
c      parameter(nc=3)
c      integer   maxlines,  maxfactors,  maxterms
c      parameter(maxlines=8,maxfactors=9,maxterms=250)

! Arguments

      integer color(0:2*maxlines,0:maxfactors,0:maxterms)
      integer ifact1,ifact2,element1,element2,iterm

! Local Variables

      integer nterms,nfactors
      integer nadded,ielement

!-----------
! Begin Code
!-----------
      if (ifact1 .ne. ifact2) then
         print*,'Fsum called when factors not right',ifact1,ifact2
         stop
      endif
!     Add First Term
      call copyterm(color,iterm,ifact1,ifact2)          !first make a copy
      nterms=color(0,0,0)

!     Add First Factor in Term
      color(0,0,nterms)=color(0,0,nterms)+1             !add one factor
      nfactors=color(0,0,nterms)
      nadded=0
      do ielement=1,element1-1                          !add remaining glue's
         nadded=nadded+1
         color(nadded,nfactors,nterms)=color(ielement,ifact1,iterm)
      enddo
      do ielement=element2+1,color(0,ifact2,iterm)      !add remaining glue's
         nadded=nadded+1
         color(nadded,nfactors,nterms)=color(ielement,ifact2,iterm)
      enddo
      color(0,nfactors,nterms) = nadded                 !number of elements

!     Add Second Factor in Term
      color(0,0,nterms)=color(0,0,nterms)+1             !add one factor
      nfactors=color(0,0,nterms)
      nadded=0
      do ielement=element1+1,element2-1                 !add remaining glue's
         nadded=nadded+1
         color(nadded,nfactors,nterms)=color(ielement,ifact2,iterm)
      enddo
      color(0,nfactors,nterms) = nadded                 !number of elements
      color(2,1,nterms) = color(2,1,nterms)*2


!     Add Second Term
      call copyterm(color,iterm,ifact1,ifact2)          !first make a copy
      nterms=color(0,0,0)

!     Add First Factor in Term
      color(0,0,nterms)=color(0,0,nterms)+1             !add one factor
      nfactors=color(0,0,nterms)
      nadded=0
      do ielement=1,abs(color(0,ifact1,iterm))          !add remaining glue's
         if (ielement .ne. element1 .and.
     &        ielement .ne. element2) then
            nadded=nadded+1
            color(nadded,nfactors,nterms)=color(ielement,ifact1,iterm)
         endif
      enddo
      color(0,nfactors,nterms) = nadded                 !number of elements

      color(2,1,nterms) = -color(2,1,nterms)*2*Nc
c      if (nc .lt. 0) color(1,1,iterm)=0                 !Large NC limit
      color(1,1,iterm) = 0                              !this means clear

      call  ClearZeros(color)
      end

