!-----------------------
! madgraph - a Feynman Diagram package by Tim Stelzer and Bill Long 
! (c) 1993
!
! Filename: fermi4.f
!-----------------------

      subroutine GetFermiFact(tops,ifac,i_this,i_max,passed)
!****************************************************************************
!     Driver for finding fermi factors for antisymmetric graphs with
!     at least 4 identical fermions
!****************************************************************************
      implicit none

! Constants

      include 'params.inc'

! Arguments
      integer tops(0:4,0:maxnodes)
      integer order(maxlines),i_this,i_max,ifac
      logical passed

! Local Variables

      integer nline1,nline2,i,j,k
      integer nline(maxlines),nfound
      logical match,good
      integer stored
      integer iorder(0:maxlines,max_fermi)  !good for maxlines=8 (8/2)!=24

! Global

      integer        iline(-maxlines:maxlines),idir(-maxlines:maxlines)
      integer        this_coup(max_coup) ,goal_coup(max_coup)
      common/to_proc/iline,idir,this_coup,goal_coup

      character*(4*max_particles) particle(4)
      integer                               charge_c(max_particles)
      integer iparticle(0:max_particles,0:4),inverse(max_particles)
      common/to_model/iparticle,  particle,  inverse, charge_c

      save iorder

!-----------
! Begin Code
!-----------
      ifac=1
      call findstart(nline,nfound)
      j = 0
      do i=1,nfound
         nline1=nline(i)
         good=.true.
         do k=1,2*j
            if (nline1 .eq. order(k)) good=.false.
         enddo
         if (good) then
            call followfermi(nline1,nline2,tops)
c
c     Need to put a check in here to ensure that only 1 C.C. is allowed
c     on any Fermion line. Otherwise, use 2 C.C. and get right back to
c     the starting place. Perhaps during check3vert we can see if its a CC
c     interactions or not.
c
c            if (idir(nline1) .gt. 0 .or. idir(nline2) .gt. 0) then
               j=j+1                    !Make sure didn't switch direction
               order(2*j-1) = nline1    !of both lines.
               order(2*j)   = nline2
c            endif
         endif
      enddo
c      write(*,*) 'Fermi Factor',(order(j),j=1,nfound)
      stored=i_max+1
      do i=1,i_max
         match = .true.
         do j=1,nfound
            if (order(j) .ne. iorder(j,i)) match=.false.
         enddo
         if (match) stored = i
      enddo
      if (stored .gt. i_max) then
c
c     This is a new set of fermi connections, need to increase i_max
c     and store this as another set to search.
c
         if (stored .lt. max_fermi) then
            do i=1,nfound
               iorder(i,stored) = order(i)
            enddo
            i_max = stored            
            call isort(order,ifac,nfound)
         else
            write(*,*) 'Error out of memory in fermi4.f!'
            write(*,'(a,i3,a)') 'Reset parameter max_fermi > ',
     &           maxlines/2,' factorial.'
            write(*,*) 'Current value is',max_fermi
            stop
         endif
c
c     In case Fermion line is actually followed backward
c     then need to give factor of -1. Need to be careful since
c     charge_c have opposite flow. Done in two steps here
c
         do i=1,nfound,2
            if (inverse(iline(iorder(i,stored)))
     &           .gt. iline(iorder(i,stored))) then
               ifac=-ifac
c               write(*,*) 'Flipped ifac',i,ifac,iline(iorder(i,stored)),
c     $              inverse(iline(iorder(i,stored)))
            endif
cxxtjsxx 4-16-2006     
c     This checks to see if we have a charge_con on first line, if so
c     says we need to flip since flows opposite direction. But for case
c     of Majorana particle woudl get double-flip since inverse(CC(particle))=particle
c     so trap for that here
c
            if (charge_c(iline(iorder(i,stored))) .ne.
     $           inverse(iline(iorder(i,stored)))) then    !not majorana
               if (charge_c(iline(iorder(i,stored)))
     &              .lt. iline(iorder(i,stored))) then !This is charge cong
                  ifac=-ifac
c                  write(*,*) 'Flipped C ifac',i,ifac,
c     $                 charge_c(iline(iorder(i,stored))),
c     $                 iline(iorder(i,stored)),
c     $                 inverse(iline(iorder(i,stored)))
               endif
            endif
         enddo
         iorder(0,stored) = ifac
c         print*,'Added new one',stored,(iorder(i,stored),i=0,nfound)
c     $        , ifac
      endif
      ifac = iorder(0,stored)
      passed = (stored .eq. i_this)   !This is for majarana fermions?
c
c     Here we might want to add something which keeps track of the
c     topologies which need to be searched again. This could speed things
c     up considerably.
c
c
c     4/15/01
c     I'm trying something new were we don't order the diagrams according
c     to the fermion connections. I think it will be OK, but may need some 
c     checking Need to also change things in fields.f.  Look for i_this loop
c
c      passed = .true.
      end

      
      Subroutine FindStart(nline,nfound)
!*****************************************************************************
!     Output:
!           nline(6)   line number of external particle flowing out
!           nfound     number of external outflowing fermions found
!*****************************************************************************
      implicit none      

! Constants
      include 'params.inc'
c      integer    max_particles       
c      parameter (max_particles=2**7-1)
c      integer    max_string,   maxlines
c      parameter (max_string=120,maxlines=8)
c      integer    max_coup
c      parameter (max_coup=5)


! Arguments

      integer nline(maxlines),nfound

! Local Variables

      integer i

! Global Variables

      character*(max_string) iwave(max_particles),owave(max_particles)
      character*(8) str(3,max_particles)
      integer info_p(5,max_particles), iposx(3,max_particles)
      common/to_external/iwave,owave,iposx,info_p,str

      character*(4*max_particles) particle(4)
      integer                               charge_c(max_particles)
      integer iparticle(0:max_particles,0:4),inverse(max_particles)
      common/to_model/iparticle,  particle,  inverse, charge_c

      integer        iline(-maxlines:maxlines),idir(-maxlines:maxlines)
      integer        this_coup(max_coup) ,goal_coup(max_coup)
      common/to_proc/iline,idir,this_coup,goal_coup

!-----------
! Begin Code
!-----------
      nfound = 0
      do i=1,iline(0)
         if (info_p(3,iline(i)) .eq. 1) then !Fermion 
            nfound=nfound+1
            nline(nfound) = i
         endif
      enddo
      end


      Subroutine FollowFermi(nline1,nline2,tops)
!*****************************************************************************
!     Input:
!           nline1   line of external outgoing fermion
!           tops     topology of graph being considered
!     Output:
!           nline2   line number of external particle flowing out
!*****************************************************************************
      implicit none      

! Constants
      include 'params.inc'
c      integer    fermion
c      parameter (fermion = 1)
c      integer    maxnodes
c      parameter (maxnodes=5)
c      integer    max_particles       
c      parameter (max_particles=2**7-1)
c      integer    max_string,   maxlines
c      parameter (max_string=120,maxlines=8)
c      integer    max_coup
c      parameter (max_coup=5)


! Arguments

      integer nline1,nline2
      integer tops(0:4,0:maxnodes)

! Local Variables

      integer i,j,nvert,oldvert,lnum
      logical finished,foundit

! Global Variables

      character*(max_string) iwave(max_particles),owave(max_particles)
      integer info_p(5,max_particles), iposx(3,max_particles)
      character*(8) str(3,max_particles)
      common/to_external/iwave,owave,iposx,info_p,str

      integer        iline(-maxlines:maxlines),idir(-maxlines:maxlines)
      integer        this_coup(max_coup) ,goal_coup(max_coup)
      common/to_proc/iline,idir,this_coup,goal_coup


!-----------
! Begin Code
!-----------
      nline2 = nline1
      finished = .false.
      oldvert= 0
      do while (.not. finished)
         foundit= .false.
         nvert  = 0
         do while (.not. foundit .and. nvert .lt. tops(1,0))
            nvert=nvert+1
            if (nvert .eq. oldvert) nvert=nvert+1
            do j=1,tops(0,nvert)                    !check each line
               if (tops(j,nvert) .eq. nline2) then
                  foundit = .true.
                  lnum = j
               endif
            enddo
         enddo
         if (.not. foundit)
     &        print*,'Error vertex for line not found',nline2
         foundit=.false.
         i = 0
         do while (i .le. tops(0,nvert) .and. .not. foundit)
            i=i+1
            if (i .ne. lnum .and.
     &          info_p(3,iline(tops(i,nvert))) .eq. fermion) then
               nline2=tops(i,nvert)
               oldvert=nvert
               foundit = .true.
            endif
         enddo
         if (.not. foundit) then
            print*,'Error did not find 2nd fermion in tops',lnum
            print*,'line num ',(tops(i,nvert),i=1,3)
            print*,'fermion ',(iline(tops(i,nvert)),i=1,3)
         endif
         if (nline2 .gt. 0) then !External
            finished = .true.
         endif
      enddo
      end
      
      Subroutine isort(order,ifac,nfound)
!****************************************************************************
!     Sorts the line numbers, keeping track of the number of permutations
!     Input:
!           order(maxlines)       12 lines in order found
!     Output:
!           ifac            -1**(number of permutations to put in order
!****************************************************************************
      implicit none

! Constants

      include 'params.inc'
c      integer    maxlines
c      parameter (maxlines = 8)

! Arguments

      integer ifac,order(maxlines),nfound

! Local Variables

      integer i,j,itemp

!-----------
! Begin Code
!-----------
      ifac = 1
      do i=1,nfound-1
         do j=1,nfound-1
            if (order(j) .ge. order(j+1)) then
               itemp=order(j)
               order(j)=order(j+1)
               order(j+1) = itemp
               ifac=-ifac
            endif
         enddo
      enddo
      do i=1,nfound-1
         if (order(i) .ge. order(i+1)) then
            print*,'Error order not correct',i,order(i),order(i+1)
         endif
      enddo
      end
