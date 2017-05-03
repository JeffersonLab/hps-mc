!-----------------------
! madgraph - a Feynman Diagram package by Tim Stelzer and Bill Long 
! (c) 1993
!
! Filename: sort.f
!-----------------------
 
      subroutine add3vert(i,j,k,iplace)
      implicit none

! Constants
      include 'params.inc'

c      integer   maxv3
c      parameter(maxv3=1024)

! Arguments

      integer i,j,k,iplace

! Local Variables

      integer vertex, imax

! External Functions

      integer packv3

! Global Variables

      integer           nv3,v3_table(maxv3)
      common /v3_block/ nv3,v3_table
      data imax/0/
!-----------
! Begin Code
!-----------
      iplace=-1
      if (nv3 .eq. maxv3) then
         print*,' V3 vertex table size exceeded ',nv3,maxv3
         stop
      end if
      vertex = packv3(i,j,k)
      nv3 = nv3 + 1
      v3_table(nv3) = vertex
      iplace = nv3
c      write(*,*) 'Added 3 vertex ',iplace,i,j,k
      end

!---------------------------
      integer function packv3(i,j,k)
      implicit none

! Arguments

      integer i,j,k

! Local Variables

      integer ii,jj,kk,temp

!-----------
!  Begin code
!-----------
      ii = i
      jj = j
      kk = k
      if (ii .gt. jj) then
         temp = ii
         ii = jj
         jj = temp
      end if
      if (jj .gt. kk) then
         temp = jj
         jj = kk
         kk = temp
      end if
      if (ii .gt. jj) then
         temp = ii
         ii = jj
         jj = temp
      end if
      
      packv3 = (ii*256 + jj)*256 + kk
c      if ( (ii*256+jj)*256+kk .ge. 2**30) then
c         write(*,*) (ii*256 + jj)*256 + kk
c         write(*,*) packv3
c      endif

c      packv3 = (ii*1024 + jj)*1024 + kk
      end

!------------------------
      Subroutine resetV
      implicit none

! Constants

      include 'params.inc'
c      integer   maxv3
c      parameter(maxv3=1024)
c      integer   maxv4
c      parameter(maxv4=64)

! Global Variables

      integer           nv3,v3_table(maxv3)
      common /v3_block/ nv3,v3_table
      integer           nv4,v4_table(maxv4)
      common /v4_block/ nv4,v4_table

!-----------
! Begin Code
!-----------
      nv3 = 0
      nv4 = 0
      end

!------------------------------------
      integer function v3(l1,l2,l3,n)
      implicit none

! Constants

      include 'params.inc'
c      integer   maxv3
c      parameter(maxv3=1024)

! Arguments

      integer l1,l2,l3,n

! Local Variables

      integer i,nfound,vertex

! External Functions

      integer packv3

! Global Variables

      integer           nv3,v3_table(maxv3)
      common /v3_block/ nv3,v3_table


!-----------
!  Begin code
!-----------
      vertex = packv3(l1,l2,l3)
      nfound = 0
      do i=1,nv3
         if (vertex .eq. v3_table(i)) then
            nfound = nfound + 1
            if ( nfound .eq. n) then
               v3 = i
               return
            end if
         end if
      end do
      v3 = 0
      end


      subroutine sort2(array,aux1,n)
      implicit none

! Arguments

      integer n
      integer array(n),aux1(n)

!  Local Variables

      integer i,k,temp
      logical done

!-----------
! Begin Code
!-----------
      do i=n-1,1,-1
         done = .true.
         do k=1,i
            if (array(k) .gt. array(k+1)) then
               temp = array(k)
               array(k) = array(k+1)
               array(k+1) = temp
               temp = aux1(k)
               aux1(k) = aux1(k+1)
               aux1(k+1) = temp
               done = .false.
            end if
         end do
         if (done) return
      end do
      end 

      subroutine optcheck(buff,line,num)
!****************************************************************************
!     This routine looks to see if the wave function has been calculated
!     if so sets the wavenumber for the graph
!     Else a new wave number is assigned and the graph
!     is written
!****************************************************************************
      implicit none

! Constants

      include 'params.inc'
c      integer   maxgraphs,    maxlines
c      parameter(maxgraphs=999,maxlines=8)
c      integer    max_string
c      parameter (max_string=120)

! Arguments

      character*(*) buff
      integer line,num

! Local Variables

      integer i
      logical found

! Global Variables

      integer wnum(-maxlines:maxlines),wcount,wlen(maxgraphs*maxlines)
      integer alen(maxgraphs)
      common/toopt/wcount,wnum,wlen,alen
      character*(max_string) wstrings(maxgraphs*maxlines)
      character*(max_string) astrings(maxgraphs)
      common/tocode/wstrings,astrings

!-----------
! Begin Code
!-----------     
      found=.false.
      do i=1,wcount                        !number of wavefunctions calculated
         if (buff(1:num-1) .eq. wstrings(i)(1:num-1)) then
            found=.true.
            wnum(line) = i
         endif
      enddo
      if (.not. found) then
         wcount=wcount+1
         wnum(line) = wcount
         call makestring(line,buff(num:num+3))
         buff(num+5:num+5) = ')'
c         call writebuff(buff(:num+4))
         wstrings(wcount) = buff(:num+5)
         wlen(wcount) = num+5
      endif
      end


!--------------------------
      subroutine writebuff(buff)

      implicit none

! Constants

      integer   lun
      parameter(lun = 91)

! Arguments

      character*(*) buff

! Local Variables

      integer length,i

!-----------
! Begin Code
!-----------
      length = len(buff)
      if (length .le. 60) then
         write (lun,100) buff
      else
         i = 60
         do while (buff(i:i) .ne. ',')
            i = i - 1
         end do
         write (lun,100) buff(:i)
         write (lun,200) buff(i+1:)
      end if
 100  format ('      CALL ',a)
 200  format ('     &     ',a)
      end

      Subroutine makestring(j,str)
!************************************************************************
!     Returns in string form the number i=wnum(j)
!
!************************************************************************
      implicit none
! Constants
      
      include 'params.inc'
c      integer    maxlines,   maxgraphs
c      parameter (maxlines=8, maxgraphs=999)

! Arguments

      integer j
      character*4 str

! Global Variables

      integer alen(maxgraphs)
      integer wnum(-maxlines:maxlines),wcount,wlen(maxgraphs*maxlines)
      common/toopt/wcount,wnum,wlen,alen

!-----------
! Begin Code
!-----------     
      if (abs(j) .gt. maxlines) then
         print*,'Error j outside [1:9999] in makestring',j
         str='????'
      else
         if (wnum(j) .le. 0) then
            print*,'Warning Line does not have wavefunction',j,
     $           wnum(j),wnum(j+1)
            str='????'
         else
            call makestringamp(wnum(j),str)
         endif
      endif
      end
      
!------------------------------------------
      subroutine makestringamp(i,str)
      implicit none

!  Constants

      integer z
c      parameter (z = ichar('0'))

!  Arguments

      integer i
      character*(*) str
!-----------
! Begin Code
!-----------
      z = ichar('0')
      if (i .gt. 9999 .or. log10(real(i)) .gt. len(str)) then
         print*,' i too big'
      else if (i .gt. 999) then
         str=char(i/1000+z)//char(mod(i/100,10)+z)//
     $        char(mod(i/10,10)+z)//char(mod(i,10)+z)
      else if (i .gt. 99) then
         str=char(i/100+z)//char(mod(i/10,10)+z)//char(mod(i,10)+z)
      else if (i .gt. 9) then
         str=char(i/10+z)//char(mod(i,10)+z)
      else if (i .gt. 0) then
         str=char(i+z)
      else
         print*,' i too small in makestringamp',i
         str='????'
      end if
      end

      subroutine flushbuff(wold)
c**********************************************************************
c     Writes out all of the lines not yet written in strings
c**********************************************************************
      implicit none

! Constants

      include 'params.inc'
c      integer   maxgraphs,    maxlines
c      parameter(maxgraphs=999,maxlines=8)
c      integer    max_string
c      parameter (max_string=120)
c      integer    max_coup
c      parameter (max_coup=5)

! Arguments

      integer wold

! Local Variables

      integer i,wpart(0:3)
      character*2 str
      character*40 buff

! Global Variables

      character*(max_string) astrings(maxgraphs)
      character*(max_string) wstrings(maxgraphs*maxlines)
      integer wnum(-maxlines:maxlines),wcount,wlen(maxgraphs*maxlines)
      integer alen(maxgraphs)
      common/toopt/wcount,wnum,wlen,alen
      common/tocode/wstrings,astrings

      logical         lwp,lwm,lz,decay,cross
      common/to_decay/lwp,lwm,lz,decay,cross

      integer        iline(-maxlines:maxlines),idir(-maxlines:maxlines)
      integer        this_coup(max_coup) ,goal_coup(max_coup)
      common/to_proc/iline,idir,this_coup,goal_coup
c
c     Below is common block for comparison with other processes
c
      character*(max_string) astrings1(maxgraphs,maxproc)
      character*(max_string) wstrings1(maxgraphs*maxlines,maxproc)
      integer wcount1(maxproc),acount1(maxproc)
      integer wlen1(maxgraphs*maxlines,maxproc)
      integer alen1(maxgraphs,maxproc),nproc1
      common/toglobal/wcount1,acount1,wlen1,alen1,nproc1
      common/toglobals/wstrings1,astrings1

      integer          gopt              !Speed optimization setting
      logical                 pmatch
      common/to_gmatch/gopt,  pmatch

! Data
      data wpart/0,0,0,0/
!-----------
! Begin Code
!-----------     
c      print*,'Writing lines',wold+1,wcount
      if (wpart(0) .eq. 0) then
         call getparticle('w+w-z',wpart(1),wpart(0))
         if (wpart(0) .ne. 3) then
            print*,'Warning couldnt find WZ boson codes'
         endif
      endif
      do i=wold+1,wcount
         if (decay .and. i .le. iline(0)) then !Check for decay particle
            if (iline(i) .eq. wpart(1) .or. iline(i) .eq. wpart(2)) then 
               call makestringamp(i,str)
               buff = 'ixxxxx(p(0,??),ZERO,NHEL(??),-1,wx1)'
               buff(12:13)=str
               buff(26:27)=str
               call writebuff(buff)
               call makestringamp(i+1,str)
               buff = 'oxxxxx(p(0,??),ZERO,NHEL(??),+1,wx2)'
               buff(12:13)=str
               buff(26:27)=str
               call writebuff(buff)
               buff = 'jioxxx(wx1,wx2,gwf,wmass,wwidth,w(1,??))'
               call makestringamp(i,str)
               buff(37:38)=str
               call writebuff(buff)
            elseif (iline(i) .eq. wpart(3)) then
               call makestringamp(i,str)
               buff = 'ixxxxx(p(0,??),ZERO,NHEL(??),-1,wx1)'
               buff(12:13)=str
               buff(26:27)=str
               call writebuff(buff)
               call makestringamp(i+1,str)
               buff = 'oxxxxx(p(0,??),ZERO,NHEL(??),+1,wx2)'
               buff(12:13)=str
               buff(26:27)=str
               call writebuff(buff)
               buff = 'jioxxx(wx1,wx2,gzl,zmass,zwidth,w(1,??))'
               call makestringamp(i,str)
               buff(37:38)=str
               call writebuff(buff)
            else
               call writebuff(wstrings(i)(1:wlen(i)))
            endif
         else
            call writebuff(wstrings(i)(1:wlen(i)))
         endif
c
c     Here we can check to see if it is a match quickly. If not
c     we could be done with this process early!
c
         if (pmatch .and. nproc1 .ge. 1) then
            if (wlen(i) .ne. wlen1(i,1))    pmatch=.false.
            if (wstrings(I)(1:wlen(i)) .ne.
     $          wstrings1(I,1)(1:wlen(i)))  pmatch=.false.
         endif
      enddo

      wold = wcount
      end

      subroutine flushamp(ngraph)
c**********************************************************************
c     Writes out all amplitudes
c**********************************************************************
      implicit none

! Constants

      include 'params.inc'
c      integer   maxgraphs,    maxlines
c      parameter(maxgraphs=999,maxlines=8)
c      integer    max_string
c      parameter (max_string=120)

! Arguments

      integer ngraph

! Global Variables

      character*(max_string) astrings(maxgraphs)
      character*(max_string) wstrings(maxgraphs*maxlines)
      integer wnum(-maxlines:maxlines),wcount,wlen(maxgraphs*maxlines)
      integer alen(maxgraphs)
      common/toopt/wcount,wnum,wlen,alen
      common/tocode/wstrings,astrings
!-----------
! Begin Code
!-----------     
      call writebuff(astrings(ngraph)(1:alen(ngraph)))
      end

      subroutine saveproc(nproc,ngraphs)
c**********************************************************************
c     Writes out all amplitudes
c**********************************************************************
      implicit none

! Constants

      include 'params.inc'
c      integer   maxgraphs,    maxlines
c      parameter(maxgraphs=999,maxlines=8)
c      integer    max_string
c      parameter (max_string=120)
c      integer    maxproc
c      parameter (maxproc=50)

! Arguments

      integer nproc,ngraphs

! Local
      
      integer i,iproc, idcsymfact
      logical found,match

! External
      logical qmatch_check
      external qmatch_check
      integer identdc
      external identdc

! Global Variables

      character*(max_string) astrings(maxgraphs)
      character*(max_string) wstrings(maxgraphs*maxlines)
      integer wnum(-maxlines:maxlines),wcount,wlen(maxgraphs*maxlines)
      integer alen(maxgraphs)
      common/toopt/wcount,wnum,wlen,alen
      common/tocode/wstrings,astrings

      character*(max_string) astrings1(maxgraphs,maxproc)
      character*(max_string) wstrings1(maxgraphs*maxlines,maxproc)
      integer wcount1(maxproc),acount1(maxproc)
      integer wlen1(maxgraphs*maxlines,maxproc)
      integer alen1(maxgraphs,maxproc),nproc1
      common/toglobal/wcount1,acount1,wlen1,alen1,nproc1
      common/toglobals/wstrings1,astrings1
c      character*1           gprocprefix(maxproc)
      integer               gprocprefix(maxproc)
      common/to_global_pref/gprocprefix
c      character*1         nameprefix
      integer             nameprefix
      common/to_multiname/nameprefix

      integer             gdcsymfact(maxproc)
      common/to_dcsymfact/gdcsymfact

      integer        iline(-maxlines:maxlines),idir(-maxlines:maxlines)
      integer        this_coup(max_coup) ,goal_coup(max_coup)
      common/to_proc/iline,idir,this_coup,goal_coup


!-----------
! Begin Code
!-----------     
      iproc = 0
      found = .false.
      idcsymfact = identdc()
c      write(*,*) 'decay sym fact', i
      do while (.not. found .and. iproc.lt.nproc1.and.iproc.lt.maxproc)
         iproc=iproc+1
         i=0
         match = .true.
         if (idcsymfact .ne. gdcsymfact(iproc)) match=.false.
         if (nameprefix .ne. gprocprefix(iproc)) match=.false.
         if (wcount .ne. wcount1(iproc))        match=.false.
         if (acount1(iproc) .ne. ngraphs)       match=.false.
         do while (i .lt. wcount .and. match)
            i=i+1
            if (wlen(i) .ne. wlen1(i,iproc))    match=.false.
            if (wstrings(I)(1:wlen(i)) .ne.
     $          wstrings1(I,iproc)(1:wlen(i)))  match=.false.
         enddo
         i=0
         do while (i .lt. ngraphs .and. match)
            i=i+1
            if (alen(i) .ne. alen1(i,iproc))    match=.false.
            if (astrings(I)(1:alen(i)) .ne.
     $          astrings1(I,iproc)(1:alen(i)))  match=.false.
         enddo
         found = match
      enddo
c
c     12-18-2006 Add qmatch_check to make sure 
c
      if (.not. found .or. .not. qmatch_check(iline(0)) ) then
         if (nproc1 .lt. maxproc) then
            nproc1=nproc1+1
            nproc =nproc1
            wcount1(nproc) = wcount
            acount1(nproc) = ngraphs
            gprocprefix(nproc)=nameprefix
            gdcsymfact(nproc) = idcsymfact
            do i=1,wcount
               wstrings1(i,nproc)=wstrings(i)
               wlen1(i,nproc)    =wlen(i)
            enddo
            do i=1,ngraphs
               astrings1(i,nproc)=astrings(i)
               alen1(i,nproc)    =alen(i)
            enddo
         elseif (nproc1 .eq. maxproc) then
            write(*,*) 'Exceeded maximum number of subprocess',
     &           ' can no longer check for identical subprocess.'
            write(*,*) 'Increase maxproc ',maxproc
            nproc1 = nproc1+1
            nproc = nproc1
         else
            nproc=nproc1
         endif
      else
         nproc=-iproc
         print*,'Found Match',iproc,nproc1
      endif
      end

