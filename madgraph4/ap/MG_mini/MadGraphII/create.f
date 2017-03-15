!-----------------------
! madgraph - a Feynman Diagram package by Tim Stelzer and Bill Long 
! (c) 1993
!
! Filename: create.f
!-----------------------
      Subroutine CreateColor(li,info,n,color)
!**************************************************************************
!     Store colors to be added after graph is accepted
!**************************************************************************
      implicit none
! Constants

      include 'params.inc'

c     Arguments

      integer li(4),info(0:4),n,color

c     Local

      integer i

c     Global
      integer sli(4,maxlines),sinfo(0:4,maxlines),sn(maxlines),num
      common /to_fastcolor/sli,sinfo,sn,num

      num=num+1
      if (num .gt. maxlines) print*,'Error too many colors in fast'
      do i=1,4
         sli(i,num)=li(i)
      enddo
      do i=0,4
         sinfo(i,num)=info(i)
      enddo
      sn(num)=n
      end

      subroutine ProjectColorSinglet(i,j,color)
!**************************************************************************
!     Places  delta_ij  factor to force quarks i,j in color singlet
!     state
!     tjs  7-19-2007
!**************************************************************************
      implicit none
! Constants

      include 'params.inc'

c     Arguments

      integer color(0:2*maxlines,0:maxfactors,0:maxterms)
      integer i,j

c     Local

      integer li(4), info(0:4), n
c-----
c  Begin Code
c-----

      
      info(0) = 4    !Delta i,j
      n = 3           !3 line numbers passed (first is dummy
      li(1) = 0          !dummy
      li(3) = i          !quark
      li(2) = j          !antiquark

      call createcolor1(li,info,n,color)
c
c     need to sum over these lines now at the amplitude level
c
      call sumcolors(color,i)
      call sumcolors(color,j)

      end


      subroutine ProjectColorOctet(i,j,color)
!**************************************************************************
!     Places  T_ij^d  factor to force quarks i,j in color octet state
!     results in external 3 indices i,j being replaced by single 8 index
!     i
!     tjs  7-19-2007
!**************************************************************************
      implicit none
! Constants

      include 'params.inc'

c     Arguments

      integer color(0:2*maxlines,0:maxfactors,0:maxterms)
      integer i,j

c     Local

      integer li(4), info(0:4), n
c-----
c  Begin Code
c-----

      
      info(0) = 1    !T_ i,j
      n = 3           !3 line numbers passed (first is dummy
      li(1) = 99         !octet
      li(2) = i          !quark
      li(3) = j          !antiquark

      call createcolor1(li,info,n,color)
c
c     need to sum over these lines now at the amplitude level
c
      call sumcolors(color,i)
      call sumcolors(color,j)
c
c     Now need to replace dummy index 99, w/ index i
c
      call ReplaceIndex(99,i,color)
      end

      Subroutine ReplaceIndex(old,new,color)
!**************************************************************************
!     Replaces every occorence of index old, w/ index new
!**************************************************************************
      implicit none
! Constants

      include 'params.inc'

c     Arguments

      integer old,new, color(0:2*maxlines,0:maxfactors,0:maxterms)

c     Local

      integer iterm,ifactor,ielement

      do iterm = 1, color(0,0,0)
         do ifactor = 2, color(0,0,iterm)
            do ielement=1, abs(color(0,ifactor,iterm))
               if (color(ielement,ifactor,iterm) .eq. old) then
                  color(ielement,ifactor,iterm) = new
               endif
            enddo
         enddo
      enddo
        
      end


      Subroutine performColor(color)
!**************************************************************************
!     Store colors to be added after graph is accepted
!**************************************************************************
      implicit none
! Constants

      include 'params.inc'
cfax
      include 'onia.inc'
c      integer   maxlines,  maxfactors,  maxterms
c      parameter(maxlines=8,maxfactors=9,maxterms=250)

c     Arguments

      integer color(0:2*maxlines,0:maxfactors,0:maxterms)

c     Local

      integer i

c     Global
      integer sli(4,maxlines),sinfo(0:4,maxlines),sn(maxlines),num
      common /to_fastcolor/sli,sinfo,sn,num
c
cfax 19-AUG-2007
c this block has been added because it contains the
c information on the total number of particles in the process
c the information is in iline(0).
c
      integer        iline(-maxlines:maxlines),idir(-maxlines:maxlines)
      integer        this_coup(max_coup) ,goal_coup(max_coup)
      common/to_proc/iline,idir,this_coup,goal_coup

c
c     Set default so there aren't any projections
c
      psinglet(1,0) = 0
      poctet(1,0) = 0
c
cfax
      if(C_qn.eq.1) then  !color singlet state
c
         write (*,*) "singlet",iline(0)
         psinglet(1,0) = 1      !requesting 1 singlet
         psinglet(1,1) = iline(0)-1 !Quark (c) is next-to-last particle
         psinglet(2,1) = iline(0) !AntiQuark (c~) is last particle

      elseif(C_qn.eq.8) then !color octet state
c
         write (*,*) "octet",iline(0)
         poctet(1,0) = 1        !requesting 1 octet
         poctet(1,1) = iline(0)-1 !Quark (c) is line next-to-last particle
         poctet(2,1) = iline(0) !AntiQuark (c~) is line last particle

      endif

      do i=1,num
         call createcolor1(sli(1,i),sinfo(0,i),sn(i),color)
      enddo
      num=0

      do i = 1, psinglet(1,0)
         write(*,*) "Projecting color singlet",psinglet(1,i),
     $        psinglet(2,i)
         call ProjectColorSinglet(psinglet(1,i),psinglet(2,i),color)
      enddo


      do i = 1, poctet(1,0)
         write(*,*) "Projecting color octet",poctet(1,i),
     $        poctet(2,i)
         call ProjectColorOctet(poctet(1,i),poctet(2,i),color)
      enddo


      end

      subroutine clearcolor
! Constants
      include 'params.inc'
c      integer   maxlines,  maxfactors,  maxterms
c      parameter(maxlines=8,maxfactors=9,maxterms=250)

c     Global
      integer sli(4,maxlines),sinfo(0:4,maxlines),sn(maxlines),num
      common /to_fastcolor/sli,sinfo,sn,num
      num = 0
      end


      Subroutine CreateColor1(li,info,n,color)
!**************************************************************************
!     Add appropriate color factor for the vertex
!**************************************************************************

! Constants

      include 'params.inc'
c      integer   maxlines,  maxfactors,  maxterms
c      parameter(maxlines=8,maxfactors=9,maxterms=250)

c     Arguments

      integer li(4),info(0:4),n
c
c     Local
c
      integer ll(4),i
      integer color(0:2*maxlines,0:maxfactors,0:maxterms)
!-----------
! Begin Code
!-----------
      do i=1,n
         ll(i) = li(i)
      enddo
c      write(*,*) 'Info',info(0),ll(1),ll(2),ll(3)
      if (info(0) .eq. 0) then 
         return
      elseif (info(0) .eq. 1) then            !T(a,i,j)
c         print*,'Adding T ll',ll(1),ll(3),ll(2)
c         print*,info(0),info(1),info(2),info(3)
         call addtfactor(color,ll(1),ll(3),ll(2))
      elseif (info(0) .eq. 2) then        !F(a,b,c)
c         print*,'Adding F',ll(1),ll(3),ll(2)
         call addFfactor(color,ll(1),ll(3),ll(2))
      elseif (info(0) .eq. 3) then        !F(a,b,c)F(c,d,e)
c         print*,'Adding color F4',(ll(i),i=1,n)
         call addF4factor(color,ll(1),ll(2),ll(3),ll(4))
      elseif (info(0) .eq. 4) then        !delta(i,j)
c         print*,'Adding d',ll(2),ll(3)
         call addTdel(color,ll(2),ll(3))
      elseif (info(0) .eq. 5) then        !T(a,i,k)T(b,k,j)
         call addttfactor(color,ll(1),ll(2),ll(3),ll(4))
      elseif (info(0) .eq. 6) then        !delta(a,b)
         print*,'Adding d AB',ll(1),ll(2),ll(3)
         call addfdel(color,ll(1),ll(2))
      elseif (info(0) .eq. 7) then        !d(a,b,c)
         call addDfactor(color,ll(1),ll(3),ll(2))
      elseif (info(0) .lt. 1) then
         print*,'Warning bad color ',info(0)
c
c     Now look for charge conjugate color factors
c
c
c     These are not currently used. No need to CC color factors
c
      elseif (info(0) .eq. -1) then            !T(a,i,j)
         print*,'Adding TCC ll',ll(1),ll(3),ll(2)
         call addtfactor(color,ll(1),ll(2),ll(3))
      elseif (info(0) .eq. -2) then        !F(a,b,c)
         print*,'Adding FCC',ll(1),ll(3),ll(2)
         call addFfactor(color,ll(1),ll(2),ll(3))
      elseif (info(0) .eq. -3) then        !F(a,b,c)F(c,d,e)
         write(*,*) 'Help what is CC for F(a,b,c)F(c,d,e)?',info(0)
         print*,'Adding color F4CC',(ll(i),i=1,n)
         call addF4factor(color,ll(1),ll(2),ll(3),ll(4))
      elseif (info(0) .eq. -4) then        !delta(a,b)
         print*,'Adding dCC',ll(3),ll(2)
         call addTdel(color,ll(2),ll(3))
      else
         print*,'Error unknown color structure',info(0)
      endif
      end

      Subroutine AddTdel(color,fo,fi)
!***************************************************************************
!     Add the T factor t__{fo,fi} to all terms (delta function)
!***************************************************************************
      implicit none

! Constants

      include 'params.inc'
c      integer   maxlines,  maxfactors,  maxterms
c      parameter(maxlines=8,maxfactors=9,maxterms=250)

! Arguments

      integer color(0:2*maxlines,0:maxfactors,0:maxterms)
      integer fo,fi

! Local Variables

      integer nterms,nfactors
      integer iterm
!------------
!  Begin code
!------------
      nterms=color(0,0,0)
c      write(*,*) 'Adding del factor',fo,fi
      do iterm=1,nterms
         color(0,0,iterm)=color(0,0,iterm)+1
         nfactors=color(0,0,iterm)
         color(0,nfactors,iterm)=-2         !One gluon, two quark lines T mat
         color(1,nfactors,iterm)=fo         !Outgoing quark line
         color(2,nfactors,iterm)=fi         !Outgoing quark line
      enddo
      end

      Subroutine AddFdel(color,ng1,ng2)
!***************************************************************************
!     Add the F factor F^ab to all terms (delta ab)
!***************************************************************************
      implicit none

! Constants

      include 'params.inc'
c      integer   maxlines,  maxfactors,  maxterms
c      parameter(maxlines=8,maxfactors=9,maxterms=250)

! Arguments

      integer color(0:2*maxlines,0:maxfactors,0:maxterms)
      integer ng1,ng2

! Local Variables

      integer nterms,nfactors
      integer iterm
!------------
!  Begin code
!------------
      nterms=color(0,0,0)
      write(*,*) 'Adding del a,b factor',ng1,ng2
      do iterm=1,nterms
         color(0,0,iterm)=color(0,0,iterm)+1
         nfactors=color(0,0,iterm)
         color(0,nfactors,iterm)=2          !Two gluons
         color(1,nfactors,iterm)=ng1         !gluon1
         color(2,nfactors,iterm)=ng2         !gluon2
         color(1,1,iterm)=color(1,1,iterm)*2  !Factor of 2 in numerator
      enddo
      end

      Subroutine AddTFactor(color,ng,fo,fi)
!***************************************************************************
!     Add the T factor t^ng__{fo,fi} to all terms
!***************************************************************************
      implicit none

! Constants

      include 'params.inc'
c      integer   maxlines,  maxfactors,  maxterms
c      parameter(maxlines=8,maxfactors=9,maxterms=250)

! Arguments

      integer color(0:2*maxlines,0:maxfactors,0:maxterms)
      integer ng,fo,fi

! Local Varibales

      integer nterms,nfactors
      integer iterm

!------------
!  Begin code
!------------
      nterms=color(0,0,0)
c      write(*,*) 'Adding T factor 3 3~ 8',fo,fi,ng
      do iterm=1,nterms
         color(0,0,iterm)=color(0,0,iterm)+1
         nfactors=color(0,0,iterm)
         color(0,nfactors,iterm)=-3         !One gluon, two quark lines T mat
         color(1,nfactors,iterm)=fo         !Outgoing quark line
         color(2,nfactors,iterm)=fi         !Outgoing quark line
         color(3,nfactors,iterm)=ng         !Gluon color line
      enddo
      end


      Subroutine AddTTFactor(color,ng1,ng2,fo,fi)
!***************************************************************************
!     Add the T factor t^ng1,ng2_{fo,fi}+t^ng2,ng1_{fo,fi} to all terms
!***************************************************************************
      implicit none

! Constants

      include 'params.inc'
c      integer   maxlines,  maxfactors,  maxterms
c      parameter(maxlines=8,maxfactors=9,maxterms=250)

! Arguments

      integer color(0:2*maxlines,0:maxfactors,0:maxterms)
      integer ng1,ng2,fo,fi

! Local Varibales

      integer nterms,nfactors
      integer iterm,jterm

!------------
!  Begin code
!------------
      nterms=color(0,0,0)
c      write(*,*) 'Adding T factor',fo,fi
      do iterm=1,nterms

         call copyterm(color,iterm,-999,-999)    !Add term for 2 flows
c
c     Now add factor T[i,j,b,a] to first term
c
         color(0,0,iterm)=color(0,0,iterm)+1
c         color(1,1,iterm)=-color(1,1,iterm)  !This  appears necessary
         nfactors=color(0,0,iterm)
         color(0,nfactors,iterm)=-4         !two octets, two triplets 
         color(1,nfactors,iterm)=fo         !Outgoing triplet
         color(2,nfactors,iterm)=fi         !incoming triplet
         color(3,nfactors,iterm)=ng1         !Octet color line
         color(4,nfactors,iterm)=ng2         !Octet color line
c
c     Now add factor T[i,j,b,a] to new term
c
         jterm = iterm+nterms
         color(0,0,jterm)=color(0,0,jterm)+1
c         color(1,1,jterm)=-color(1,1,jterm)  !This  appears necessary
         nfactors=color(0,0,jterm)
         color(0,nfactors,jterm)=-4         !One gluon, two quark lines T mat
         color(1,nfactors,jterm)=fo         !Outgoing triplet
         color(2,nfactors,jterm)=fi         !incoming triplet
         color(3,nfactors,jterm)=ng2         !Octet color line
         color(4,nfactors,jterm)=ng1         !Octet color line
      enddo
      if(color(0,0,0) .ne. 2*nterms) then
         print*,'Warning wrong number of terms created in add TT',
     &        color(0,0,0),nterms
      endif

      end

      Subroutine AddFFactor(color,ng1,ng2,ng3)
!***************************************************************************
!     Add the F factor f^abc to all terms
!***************************************************************************
      implicit none

! Constants

      include 'params.inc'
c      integer   maxlines,  maxfactors,  maxterms
c      parameter(maxlines=8,maxfactors=9,maxterms=250)

! Arguments

      integer color(0:2*maxlines,0:maxfactors,0:maxterms)
      integer ng1,ng2,ng3

! Local Variables

      integer nterms,nfactors
      integer iterm

!------------
!  Begin code
!------------
      nterms=color(0,0,0)
c      nflows=color(4,1,nterms)                  !How many flows exist
      do iterm=1,nterms
         color(1,1,iterm)=-color(1,1,iterm)
         call copyterm(color,iterm,-999,-999)    !Add term for two flows

         color(0,0,iterm)=color(0,0,iterm)+1
         nfactors=color(0,0,iterm)
         color(0,nfactors,iterm)=3          !3 Gluons F matrix
         color(1,nfactors,iterm)=ng1        !Gluon 1
         color(2,nfactors,iterm)=ng2        !Gluon 2
         color(3,nfactors,iterm)=ng3        !Gluon 3 color line
         color(1,1,iterm)=color(1,1,iterm)*2

         color(0,0,iterm+nterms)=color(0,0,iterm+nterms)+1
         nfactors=color(0,0,iterm+nterms)
         color(0,nfactors,iterm+nterms)=3          !3 Gluons F matrix
         color(1,nfactors,iterm+nterms)=ng3        !Gluon 1
         color(2,nfactors,iterm+nterms)=ng2        !Gluon 2
         color(3,nfactors,iterm+nterms)=ng1        !Gluon 3 color line
         color(1,1,iterm+nterms)=color(1,1,iterm+nterms)*2
         color(2,1,iterm+nterms)=-color(2,1,iterm+nterms) !Subtract second
      enddo
      if(color(0,0,0) .ne. 2*nterms) then
         print*,'Warning wrong number of terms created in addF',
     &        color(0,0,0),nterms
      endif
      end

      Subroutine AddDFactor(color,ng1,ng2,ng3)
!***************************************************************************
!     Add the d factor d^{a,b,c) to all terms
!***************************************************************************
      implicit none

! Constants

      include 'params.inc'
c      integer   maxlines,  maxfactors,  maxterms
c      parameter(maxlines=8,maxfactors=9,maxterms=250)

! Arguments

      integer color(0:2*maxlines,0:maxfactors,0:maxterms)
      integer ng1,ng2,ng3

! Local Variables

      integer nterms,nfactors
      integer iterm

!------------
!  Begin code
!------------
      nterms=color(0,0,0)
c      nflows=color(4,1,nterms)                  !How many flows exist
      do iterm=1,nterms
         color(1,1,iterm)=-color(1,1,iterm)
         call copyterm(color,iterm,-999,-999)    !Add term for two flows

         color(0,0,iterm)=color(0,0,iterm)+1
         nfactors=color(0,0,iterm)
         color(0,nfactors,iterm)=3          !3 Gluons F matrix
         color(1,nfactors,iterm)=ng1        !Gluon 1
         color(2,nfactors,iterm)=ng2        !Gluon 2
         color(3,nfactors,iterm)=ng3        !Gluon 3 color line
         color(1,1,iterm)=color(1,1,iterm)*2

         color(0,0,iterm+nterms)=color(0,0,iterm+nterms)+1
         nfactors=color(0,0,iterm+nterms)
         color(0,nfactors,iterm+nterms)=3          !3 Gluons F matrix
         color(1,nfactors,iterm+nterms)=ng3        !Gluon 1
         color(2,nfactors,iterm+nterms)=ng2        !Gluon 2
         color(3,nfactors,iterm+nterms)=ng1        !Gluon 3 color line
         color(1,1,iterm+nterms)=color(1,1,iterm+nterms)*2
         color(2,1,iterm+nterms)=+color(2,1,iterm+nterms) !Add second
      enddo
      if(color(0,0,0) .ne. 2*nterms) then
         print*,'Warning wrong number of terms created in addD',
     &        color(0,0,0),nterms
      endif
      end

      Subroutine AddF4Factor(color,ng1,ng2,ng3,ng4)
!***************************************************************************
!     Add the T factor t^ng__{fo,fi} to all terms
!***************************************************************************
      implicit none

! Constants

      include 'params.inc'
c      integer   maxlines,  maxfactors,  maxterms
c      parameter(maxlines=8,maxfactors=9,maxterms=250)

! Arguments

      integer color(0:2*maxlines,0:maxfactors,0:maxterms)
      integer ng1,ng2,ng3,ng4

! Local Variables

      integer nterms,nfactors
      integer iterm,fterm,fflow

!------------
!  Begin code
!------------
      nterms=color(0,0,0)
c      nflows=color(4,1,nterms)                   !How many flows exist
      do iterm=1,nterms
         color(1,1,iterm) = -2*color(1,1,iterm)
         call copyterm(color,iterm,-999,-999)    !Add term for flow 2
         call copyterm(color,iterm,-999,-999)    !Add term for flow 3
         call copyterm(color,iterm,-999,-999)    !Add term for flow 4

         color(0,0,iterm)=color(0,0,iterm)+1     !Add Factor  Term 1
         nfactors=color(0,0,iterm)
         color(0,nfactors,iterm)=4               !4 Gluons F matrix
         color(1,nfactors,iterm)=ng1             !Gluon 1
         color(2,nfactors,iterm)=ng2             !Gluon 2
         color(3,nfactors,iterm)=ng3             !Gluon 3 color line
         color(4,nfactors,iterm)=ng4             !Gluon 4 color line
         
         fterm = nterms+3*(iterm-1)+1
         fflow=1
         color(0,0,fterm)=color(0,0,fterm)+1     !Term 2 1 factors
         nfactors=color(0,0,fterm)
         color(0,nfactors,fterm)=4               !4 Gluons F matrix
         color(1,nfactors,fterm)=ng2             !Gluon 1
         color(2,nfactors,fterm)=ng1             !Gluon 2
         color(3,nfactors,fterm)=ng4             !Gluon 3 color line
         color(4,nfactors,fterm)=ng3             !Gluon 4 color line
         color(3,1,fterm)= fflow
         

         fterm = nterms+3*(iterm-1)+2
         color(0,0,fterm)=color(0,0,fterm)+1     !Term 2
         nfactors=color(0,0,fterm)
         color(0,nfactors,fterm)=4               !4 Gluons F matrix
         color(1,nfactors,fterm)=ng2             !Gluon 1
         color(2,nfactors,fterm)=ng1             !Gluon 2
         color(3,nfactors,fterm)=ng3             !Gluon 3 color line
         color(4,nfactors,fterm)=ng4             !Gluon 4 color line
         color(2,1,fterm)=-color(2,1,fterm)
         color(3,1,fterm)= fflow

         fterm = nterms+3*(iterm-1)+3
         color(0,0,fterm)=color(0,0,fterm)+1     !Term 2
         nfactors=color(0,0,fterm)
         color(0,nfactors,fterm)=4               !4 Gluons F matrix
         color(1,nfactors,fterm)=ng1             !Gluon 1
         color(2,nfactors,fterm)=ng2             !Gluon 2
         color(3,nfactors,fterm)=ng4             !Gluon 3 color line
         color(4,nfactors,fterm)=ng3             !Gluon 4 color line
         color(2,1,fterm)=-color(2,1,fterm)
         color(3,1,fterm)= fflow
      enddo
      if(color(0,0,0) .ne. 4*nterms) then
         print*,'Warning wrong number of terms created in addF4',
     &        color(0,0,0),nterms
      endif
      end
