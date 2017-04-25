      subroutine test_tops
c************************************************************************
c     Test if the topology generator works properly
c************************************************************************
      implicit none

!  Constants

      include 'params.inc'

!  Local

      integer tops(0:4,0:maxnodes,0:1)
      integer tid(maxlines),next,icount,i
      logical done
c-----
c  Begin Code
c-----
      done = .false.
      icount=0
c      read(*,*) next
      do next = 4,5
      do i=4,next          !This is the first topology which we get for free
         tid(i)=1
      enddo
      do while (.not. done) 
         call next_top(tid,tops,next,done)
c         write(*,'(10i4)') (tid(i),i=4,next)
         icount=icount+1
      enddo
      write(*,*) next,icount
      done = .false.
      icount=0
      enddo
      end

      subroutine Increment_Tid(tid,next,done)
c************************************************************************
c     increments tid array to generate the next topology, or done
c     if there are no more possible increments
c************************************************************************
      implicit none

!  Constants

      include 'params.inc'
!  Arguments

      integer tid(maxlines),next
      integer i_inc
      logical done
!  Local

      logical FoundOne
c-----
c  Begin Code
c-----
      i_inc=next
      FoundOne=.false.
      done = .false.
      do while (.not. FoundOne .and. .not. Done)
         foundone = .true.
         if (tid(i_inc) .ge. i_inc+i_inc-4) then !Done with external
            if (i_inc .gt. 4) then
               tid(i_inc)=-1
            else
               tid(i_inc) = 1
               i_inc=i_inc-1
               foundone = .false.
               if (i_inc .le. 3) done = .true.
            endif
         elseif (tid(i_inc) .ge. 0) then !Another external or vertex
            tid(i_inc)=tid(i_inc)+1
         elseif(tid(i_inc) .gt. -i_inc+4) then !Another internal
            tid(i_inc)=tid(i_inc)-1
         else                   !This level complete go back one level
            tid(i_inc)=1                     
            i_inc = i_inc-1
            foundone = .false.
            if (i_inc .le. 3) then 
               done=.true.
c               write(*,*) 'ALL DONE'
            endif
         endif
      end do
c      write(*,*) 'Incrementing ', i_inc, tid(i_inc), done
      end

      subroutine next_top(tid,top1,next,done)
c************************************************************************
c     Given tid and the number of external particles, gets the next
c     topology and returns it in tid. Sets done=true if there are no more
c     topologies.
c************************************************************************
      implicit none

!  Constants

      include 'params.inc'
!  Arguments

      integer tid(maxlines),next
      logical done
      integer top1(0:4,0:maxnodes,0:maxgraphs)
!  Local

      integer i,i_inc,icount
      logical failed
      logical new
c-----
c  Begin Code
c-----
      new = .true.
      if (new) then
         call increment_tid(tid,next,done)
c         write(*,*) 'New',next,done
         if (done)  return
      else
         i_inc=next
         if (tid(i_inc) .eq. i_inc-1) then !Done with external
            tid(i_inc)=-1
         elseif (tid(i_inc) .gt. 0) then !Another external or vertex
            tid(i_inc)=tid(i_inc)+1
         else                   !Another internal
            tid(i_inc)=tid(i_inc)-1
         endif
         done = .false.
      endif
      failed = .true.

      do while (failed .and. .not. done)
c         write(*,*) 'Trying  gen1top',new,tid(1),tid(2),tid(3)
         call gen1top(next,top1,tid,failed)
         if (failed) then
c            write(*,*) 'Failed gen1top',new,tid(1),tid(2),tid(3)
            if (new) then
               call increment_tid(tid,next,done)
               if (done) then
                  failed = .true.
               endif
            else
               if (tid(i_inc) .eq. i_inc-1) then !Done with external  
                  tid(i_inc)=-1
               elseif (tid(i_inc) .gt. 0.and.tid(i_inc).lt.i_inc -1)then !In
                  tid(i_inc)=tid(i_inc) + 1
c               write(*,*) 'Got to increment external line',i_inc,
c     $              tid(i_inc)
               elseif (tid(i_inc) .lt. 0)  then !Done with internal
                  tid(i_inc)=i_inc
               else             !Done with vertex/everything
                  tid(i_inc)=1                     
                  i_inc = i_inc-1
                  if (tid(i_inc) .eq. i_inc-1) then !Done with external
                     tid(i_inc)=-1
                  elseif (tid(i_inc) .gt. 0) then !Another external or vertex
                     tid(i_inc)=tid(i_inc)+1
                  else          !Another internal
                     tid(i_inc)=tid(i_inc)-1
                  endif
                  if (i_inc .le. 3) done=.true.
               endif
c            write(*,*) 'tid',tid(1),tid(2),tid(3),tid(4),i_inc,
c     $           failed,done
            endif
         endif
      enddo
      if (.not. done) then
         call sortvertex(top1)
c         write(*,*) 'New Topology'
c         call write_topology(top1,1)
      endif
      end

      subroutine gen1top(next,top1,tid,failed)
c*************************************************************************
c     Given the number of external particles, returns in tops
c     the topologies for all possible graphs assuming 3 and 4 point
c     interactions
c*************************************************************************
      implicit none

!  Constants
      
      include 'params.inc'

!  Arguments

      integer next, top1(0:4,0:maxnodes,0:1),tid(maxlines)
      logical failed	  

!  Local

      integer i,nvert,n3vert,j
	  
! External

      logical  SplitLineOK, SplitVertOK
      external SplitLineOK, SplitVertOK


c-----
c  Begin Code
c-----
      failed=.true.
      call tops3(top1)             !Start with 3 external line state
      call setmaxalloweddev(top1,1)
c      write(*,'(12i5)') (tid(i),i=1,next)
      if (next .eq. 3) then
         if (tid(4) .eq. 1) then
            tid(4) = tid(4)+1
            failed = .false.
            return
         else
            return
         endif
      endif
c      call setlinegroups
      
      do i=4,next
         top1(1,0,0)=i             !Number of external lines after adding 1
         if (tid(i) .lt. i) then   !Try to split a line
            if (tid(i) .eq. 0) return  !There is no line 0
            if (tid(i) .lt. -top1(1,0,1)+1) return !Not this many internal
c
c          Need to check if it is ok to split this line based on decay contstraints
c          this will be external line (i) created by splitting line tid(i)
c			
            call setmaxalloweddev(top1,1)
            if (SplitLineOK(i,tid(i)) .and. .true.) then
               call splitline(top1,tid(i))
            else
c
c             If line i is not allowed to split line tid(i) we should
c                increment all of the tid(i) so we don't keep trying
c                to split the same line again, just changing "later" split
c                choices.
c
               do j=i+1,next
                  tid(j)= -99       !jumps to end
               enddo
               return
            endif
         else
            nvert=1
            n3vert=0
c            write(*,*) 'Looking for 3 vert',n3vert,tid(i)-i
            do while(n3vert .lt. tid(i)-i+1.and.nvert .le. top1(1,0,1))
               if (top1(0,nvert,1) .eq. 3)  n3vert=n3vert+1
               if (n3vert .eq. tid(i)-i+1) then
                  call setmaxalloweddev(top1,1)
                  if (SplitVertOK(top1,nvert)) then
c                     write(*,*) 'Approved split 4 vertex'
                     call splitvertex(top1,nvert)
                  else
                     return
                  endif
c                  write(*,*) 'Split vertex ',nvert
               else
c                  write(*,*) 'Still looking for 3 vert',n3vert,tid(i)-i
               endif
               nvert=nvert+1
            enddo
            if (n3vert .ne. tid(i)-i+1) return !Sorry didn't work
c            write(*,*) 'Passed:',n3vert,tid(i)-i
         endif
      enddo
      failed=.false.
      end

      subroutine write_topology(tops,ntop)
!*************************************************************************
!     Initialize the smallest graph possible, 3 external lines
!     which can only have one node
!*************************************************************************
      implicit none
! Constants

      include 'params.inc'

! Arguments

      integer tops(0:4,0:maxnodes,0:maxtops),ntop

! Local
      
      integer i,j

!-----------
! Begin Code
!-----------
      do i=1,tops(1,0,ntop)
         write(*,'(10i6)') i,(tops(j,i,ntop),j=1,tops(0,i,ntop))
      enddo
      end

      Subroutine tops3(tops)
!*************************************************************************
!     Initialize the smallest graph possible, 3 external lines
!     which can only have one node
!*************************************************************************
      implicit none

! Constants

      include 'params.inc'

! Arguments

      integer tops(0:4,0:maxnodes,0:maxtops)

!-----------
! Begin Code
!-----------

      tops(0,0,0)=1                !number of tops
      tops(1,0,0)=3                !number of external lines
      tops(0,0,1)=0                !number of internal lines graph 1
      tops(1,0,1)=1                !number of vertices for graph 1
      tops(2,0,1)=1                !number of 3 vertices for graph 1
      tops(3,0,1)=0                !number of 4 vertices for graph 1
      tops(0,1,1)=3                !first vertex has 3 lines
      tops(1,1,1)=1                !first lines in vertex1 is line1
      tops(2,1,1)=2                !second lines in vertex1 is line2
      tops(3,1,1)=3                !third lines in vertex1 is line3
      tops(4,1,1)=0                !this is meaningless but set to zero
      end

      Subroutine addExternal(oldtops,newtops)
!*************************************************************************
!     Add one external leg to oldgraphs topologies and store in
!     newgraphs topologies
!*************************************************************************
      implicit none

! Constants

      include 'params.inc'

! Arguments

      integer oldtops(0:4,0:maxnodes,0:maxtops)
      integer newtops(0:4,0:maxnodes,0:maxtops)

! Local Variables

      integer oldg,oldvert,oldline

!-----------
! Begin Code
!-----------
      newtops(0,0,0)=0                    !no new topologies
      newtops(1,0,0)=oldtops(1,0,0)+1     !adding one external line

      do oldg=1,oldtops(0,0,0)            !loop over each of the old topolog
         do oldline = 1,oldtops(1,0,0)
            call copytops(oldtops,newtops,oldg)
            call splitline(newtops,oldline)
         enddo
         do oldline = -1,oldtops(0,0,oldg),-1   !internal lines are negative
            call copytops(oldtops,newtops,oldg)
            call splitline(newtops,oldline)
         enddo
         do oldvert=1,oldtops(1,0,oldg)   !check each old vertex
            if (oldtops(0,oldvert,oldg) .eq. 3) then    !3 vertex can add
               call copytops(oldtops,newtops,oldg)
               call splitvertex(newtops,oldvert)
            endif
         enddo
      enddo
      end

      Subroutine copyTops(oldtops,newtops,oldg)
!*************************************************************************
!     copy topology from oldgraphs to newgraphs
!*************************************************************************
      implicit none

! Constants

      include 'params.inc'
c      integer    maxtops,      maxnodes
c      parameter (maxtops=2500,maxnodes=5)

! Arguments

      integer oldtops(0:4,0:maxnodes,0:maxtops)
      integer newtops(0:4,0:maxnodes,0:maxtops)
      integer oldg

! Local Variables

      integer ivert,iline,ntops,i

!-----------
! Begin Code
!-----------
      newtops(0,0,0) = newtops(0,0,0)+1            !adding one new topology
c      write(*,*) 'Adding topology ',newtops(0,0,0)
      if (newtops(0,0,0) .gt. maxtops) then
         write(*,*) 'Sorry exceeded maximum number of topologies',
     &        maxtops
         stop
      endif
      ntops=newtops(0,0,0)
      do i=0,4
         newtops(i,0,ntops)=oldtops(i,0,oldg)      !Basic topology info
      enddo
      do ivert=1,oldtops(1,0,oldg)                 !loop over all vertices
         do iline=0,4
            newtops(iline,ivert,ntops)=oldtops(iline,ivert,oldg)
         enddo
      enddo
      end

      Subroutine splitvertex(newtops,nvert)
!*************************************************************************
!     copy topology from oldgraphs to newgraphs
!*************************************************************************
      implicit none

! Constants

      include 'params.inc'
c      integer    maxtops,     maxnodes
c      parameter (maxtops=2500,maxnodes=5)

! Arguments

      integer newtops(0:4,0:maxnodes,0:maxtops),nvert

! Local Variables

      integer ntops

!-----------
! Begin Code
!-----------
      ntops=newtops(0,0,0)                !always working on the newest top.
c      write(*,*) 'Splitting 4 vertex',newtops(1,0,0)
      if (newtops(0,nvert,ntops) .ne. 3) then
         print*,'Error splitting vertex of non-3vertex',newtops
         stop
      endif
      newtops(0,nvert,ntops) = 4                  !now its a 4 vertex
      newtops(4,nvert,ntops) = newtops(1,0,0)     !we are adding the newest Ext
      newtops(2,0,ntops) = newtops(2,0,ntops) -1  !one less 3 vertex
      newtops(3,0,ntops) = newtops(3,0,ntops) +1  !one more 4 vertex
      end

      Subroutine splitline(newtops,nline)
!*************************************************************************
!     copy topology from oldgraphs to newgraphs
!
!     GroupID contains information about decay chain, 0 = no affiliation
!     For Example   gg > (t-> W+b) (t~ -> W-b~) g
!             group 0 = g  
!             group 1 = W+, b
!             group 2 = W- , b~      
!             internal lines may also be affiliated w/ group
!
!*************************************************************************
      implicit none

! Constants

      include 'params.inc'

! Arguments

      integer newtops(0:4,0:maxnodes,0:maxtops),nline

! Local

      integer nvert,ninternal,ntops
      integer nfound,convert(2),conline(2),i,j
      integer g1, g2, ivert
      integer tline
      logical lninternal
      integer gid1,gid2,k,minmatch,mymatch(2)

! External
      integer GroupID
      external GroupID
      logical IsMother
      external IsMother
      integer matchlevel
      external matchlevel

!-----------
! Begin Code
!-----------
      ntops=newtops(0,0,0)                !always working on the newest top.
      nfound=0
      do i=1,newtops(1,0,ntops)           !look through all vertices
         do j=1,newtops(0,i,ntops)        !either 3 or 4 lines to check
            if (newtops(j,i,ntops) .eq. nline) then
               nfound=nfound+1
               convert(nfound)=i
               conline(nfound)=j
            endif
         enddo
      enddo
      if (nline .lt. 0 .and. nfound .ne. 2) then
         print*,'Splitting internal line, but not enough nodes',nfound
         print*,'Graph number',ntops,convert(1),convert(2),nline
         stop
      elseif(nline .gt. 0 .and.  nfound .ne. 1) then
         print*,'Splitting external line, but not enough nodes',nfound
         print*,'Graph number',ntops,convert(1),convert(2),nline
      endif
      newtops(0,0,ntops) = newtops(0,0,ntops) -1     !one more internal line (-)
      newtops(1,0,ntops) = newtops(1,0,ntops) +1     !one more vertex
      newtops(2,0,ntops) = newtops(1,0,ntops) +1     !one more 3 vertex
      nvert=newtops(1,0,ntops)                       !number of new vertex
      ninternal=newtops(0,0,ntops)                   !number of new internal line
      newtops(0,nvert,ntops) = 3                     !Created 3 vertex
      newtops(1,nvert,ntops) = newtops(1,0,0)        !New External line
      newtops(2,nvert,ntops) = nline                 !original split line
      newtops(3,nvert,ntops) = ninternal             !new internal line
      newtops(4,nvert,ntops) = 0                     !Not Used set to zero
c
c     tjs Aug-10-2007
c
c     When splitting an internal line (nline<0)
c     need to decide which vertex should get the "new" line.
c     Only matters for keeping the decay chain information
c     Basically want the original to stay with vertex to the right
c     and the new one associated with vertex to the left.
c     Easiest solution is for original line should stay w/ vertex 
c     that has the largest "match" with original line
c
      if (nline .lt. 0) then
      do i = 1,2
         ivert = convert(i)
         gid1 = groupid(nline)
         minmatch = 8
         do j=1,newtops(0,ivert,ntops)
            gid2 = groupid(newtops(j,ivert,ntops))
            k = matchlevel(gid1,gid2)
            if (k .lt. minmatch) minmatch=k
         enddo
         mymatch(i)=minmatch
      enddo
c      write(*,*) "split",nline,gid1,mymatch(1),mymatch(2)
      if (mymatch(1) .lt. mymatch(2) ) then
         newtops(conline(1),convert(1),ntops)=ninternal !change other node of int
      else
         newtops(conline(2),convert(2),ntops)=ninternal !change other node of int
      endif
      else
         newtops(conline(1),convert(1),ntops)=ninternal !change other node of int

      endif


c
c     Now set group membership for lines and vertice
c     group "0" means no group affiliation
c
      g1 = groupid(nline)            !line being split
      g2 = groupid(newtops(1,0,0))   !line being added (external)
      if (g1 .le. g2) then
         call setgroupid(ninternal,g1)
      else
         call setgroupid(ninternal,g2)
      endif
      return
c
c     Don't need any of this mother stuff
c
      if (g2 .eq. 0) return             !Done w/ groups
c
c     First consider case where split an external line
c
      if (nline .gt. 0) then       !Split external line
         write(*,*) 'Splitting',nline,g1,g2
         if (g1 .eq. g2) then             !Both lines in same group
c            write(*,*) "Got group match",g1,nline,newtops(1,0,0)
c     $           ,ninternal
            call setgroupid(ninternal,g1)
c            if (ismother(nline)) call setmother(ninternal)
         elseif (g1 .eq. 0) then !attaching to initial state
c            call setgroupid(ninternal,g2)
c            call setmother(ninternal)
         else
            if (.not. ismother(nline)) then
            write(*,*) 'Error in grouping tops.f split-line()',g1,g2
            endif
         endif
      else                               !Split internal line
         if (g1 .eq. 0) then   !attaching to initial state, only 1st member can do it
c            call setgroupid(ninternal,0)   !Another initial state t-chan
c            call setmother(newtops(1,0,0)) !First one is always mother
         elseif (g1 .eq. g2) then       !Same group, usual case
            call setgroupid(ninternal,g1) !New line gets group ID
            if (ismother(nline) .or.ismother(newtops(1,0,0))
     $           .and. .false.) then !need to assign mom
c
c           Here we need to use other lines in vertex to determine
c           which one becomes mother. Note ninternal is attached to vertex convert(1)
c           And nline is attached to vertex convert(2)
c
               ivert = convert(1)         
               do j=1,3
                  if (groupid( newtops(1,ivert,ntops)) .ne. g1) then
                     call setmother(ninternal)
                  endif
               enddo
               ivert = convert(2)         
               do j=1,3
                  if (groupid(newtops(1,ivert,ntops)) .ne. g1) then
                     call setmother(nline)
                  endif
               enddo               
            endif
c            call setgroupid(ninternal,0) !No group affiliation
         else  !Match because it is mother attaching to mother.
c
c           Here we need to use other lines in vertex to determine
c           which line gets the new group id and which gets 0
c
               ivert = convert(1)         
               lninternal = .false.
               do j=1,3
                  tline = newtops(1,ivert,ntops)
                  if (tline .ne. ninternal ) then
                     if (groupid(tline) .eq. g1) then
                        lninternal = .true.
                     endif
                  endif
               enddo
               if (lninternal) then    !ninternal is line that keeps group 1 info
                  call setgroupid(ninternal,g1)
                  call setmother(ninternal)
                  call setgroupid(nline,0)
               else                    !nline is line that keeps group 1 info
                  call setgroupid(nline,g1)
                  call setmother(nline)
                  call setgroupid(ninternal,0)
               endif
         endif
      endif
      end

      integer function GroupID(j)
c*************************************************************************
c     Returns group id for line j
c************************************************************************
      implicit none

!     Constants

      include 'params.inc'

!     Arguments

      integer j    !Line to determine group for

!     Global

      integer  LineGroupID(-maxlines:maxlines)
      integer  GroupMother(maxlines)
      common /To_decayGroup/ linegroupid, groupmother

c-----
c  Begin Code
c-----
      
      if (j .lt. -maxlines .or. j .gt. maxlines) then
         write(*,*) 'Error too many propagtors in groupid',j
         groupid = 0
         return
      endif
      groupid = LineGroupID(j)
      end

      subroutine SetGroupID(j,igroup)
c*************************************************************************
c     Sets group id for line j
c************************************************************************
      implicit none

!     Constants

      include 'params.inc'

!     Arguments

      integer j    !Line to set group 
      integer igroup

!     Global

      integer  LineGroupID(-maxlines:maxlines)
      integer  GroupMother(maxlines)
      common /To_decayGroup/ linegroupid, groupmother

c-----
c  Begin Code
c-----
      
      if (j .lt. -maxlines .or. j .gt. maxlines) then
         write(*,*) 'Error too many propagtors in setgroupid',j
         return
      endif
      LineGroupID(j) = igroup
      end

      Subroutine SortVertex(tops)
!*************************************************************************
!     Put vertices for each topology in an intelligent order
!     So that after ones before it are evaluated, each has only 1 unknown
!     wave function (line), and put that line at the end
!*************************************************************************
      implicit none
      
! Constants

      include 'params.inc'
c      integer    maxtops,     maxnodes
c      parameter (maxtops=2500,maxnodes=5)
c      integer   maxlines
c      parameter(maxlines=8)

! Arguments

      integer  tops(0:4,0:maxnodes,0:maxtops)

! Local

      integer itemp
      integer ntop,iorder(maxnodes),nvert,iline(-maxlines:maxlines),ngot
      integer i,ndone
      logical done, ishiggs

!-----------
! Begin Code
!-----------
      do ntop=1,tops(0,0,0)       !sum over each topology
         done=.false.
         ndone=0
         do i=1,maxlines
            iline(i)  = 1         !external lines are positive and known
            iline(-i) = 0         !internal lines neg and not known
         enddo
         nvert = 0
         do while (.not. done)
            nvert=nvert+1
            if (nvert .gt. tops(1,0,ntop)) nvert=1
            ngot=0
            ishiggs=.false.
            do i=1,tops(0,nvert,ntop)
               if (iline(tops(i,nvert,ntop)) .ne. 0) ngot=ngot+1
               if (tops(i,nvert,ntop) .eq. 6) ishiggs=.true.
            enddo
            if (tops(0,nvert,ntop) - ngot .eq. 1 .and..not. ishiggs)then
               ndone=ndone+1
               iorder(ndone) = nvert
               do i=1,tops(0,nvert,ntop)
                  if (iline(tops(i,nvert,ntop)) .eq. 0) then !this is uncalced 
                     iline(tops(i,nvert,ntop)) = 1
                     itemp=tops(i,nvert,ntop)                !move to last position
                     tops(i,nvert,ntop) =
     &                    tops(tops(0,nvert,ntop),nvert,ntop)
                     tops(tops(0,nvert,ntop),nvert,ntop) = itemp
                  endif
               enddo
            endif
            if (tops(0,nvert,ntop) - ngot .eq. 0) then   !have them all
               if (ndone .eq. tops(1,0,ntop)-1)   then   !last one
                  done=.true.
                 do i=1,ndone
                     if (iorder(i) .eq. nvert) then
                        done=.false.
                     endif
                  enddo
                  if (done) then
                     ndone=ndone+1
                     iorder(ndone)=nvert
                  endif
               endif
            endif
         enddo
         call SetMaxAllowedMatch(tops,ntop)
         call orderverts(tops,iorder,ntop)
      enddo
      end

      Subroutine orderverts(tops,iorder,ntop)
!************************************************************************
!     Places vertices in the order specified by iorder which
!     should assure all lines except one are known at each node
!************************************************************************
      implicit none

! Constants

      include 'params.inc'
c      integer    maxtops,     maxnodes
c      parameter (maxtops=2500,maxnodes=5)

! Arguments

      integer tops(0:4,0:maxnodes,0:maxtops)
      integer ntop,iorder(maxnodes)

! Local

      integer iline,vtemp(0:4,0:maxnodes)
      integer i

!-----------
! Begin Code
!-----------
      do i=1,tops(1,0,ntop)
         do iline=0,4
            vtemp(iline,i) = tops(iline,i,ntop)
         enddo
      enddo
      do i=1,tops(1,0,ntop)
         do iline=0,4
            tops(iline,i,ntop) = vtemp(iline,iorder(i))
         enddo
      enddo
      end


      logical function SplitVertOK(tops,nvert)
!************************************************************************
!     Determines if it is OK to add line to 3-vertex based on
!     particle decay constraints (groups)
!************************************************************************
      implicit none

!     Constants

      include 'params.inc'

!     Arguments

      integer nvert
      integer tops(0:4,0:maxnodes,0:maxtops)

!     Local

      integer i,j,ntop,mygroup,k,maxmatch
      integer gid1, gid2
      integer newline, minmatch,reqmatch

! Global
      integer                  ReqMatchLevel(-maxlines:maxlines)
      common /to_ReqMatchLevel/ReqMatchLevel

!     External

      integer matchlevel, groupid
      external matchlevel, groupid

c------
c  Begin Code
c-----
      SplitVertOK = .true.
      newline = tops(1,0,0)
      return     !Don't have safe rule here yet.

c
c     just need to check level of matching for each line 
c     in the vertex to make sure we are OK.
c
      mygroup = groupid(newline) !Group of new line
c      if (mygroup .eq. 0) return    !No cut on this line
      ntop = tops(0,0,0)
      ntop = 1
c
c     Determine matching level required for newline
c
      maxmatch = 0    !No matching digits
      gid1 = mygroup
      do i = 1, newline-1
         gid2 = groupid(i)
         k = matchlevel(gid1,gid2)
         if (k .gt. maxmatch) maxmatch = k
      enddo
c
c     Determine matching level required for each line
c
      reqmatch = 0
      minmatch = 8
      do i = 1, 3
         j = tops(i,nvert,ntop)     !Line to test
         if (reqmatch .lt. reqmatchlevel(j)) then
            reqmatch = reqmatchlevel(j)
         endif
         gid1 = groupid(j)
         if (minmatch .gt. matchlevel(mygroup,gid1)) then
            minmatch = matchlevel(mygroup,gid1)
         endif
      enddo
      if (minmatch .lt. reqmatch .or. minmatch .lt. maxmatch) then
         splitvertok = .false.
c         write(*,*) 'Failed splitvert'
      else
c         write(*,*) 'Good split',minmatch,"<=",reqmatch
      endif
      end

      logical function SplitLineOK(NewLine, SplitLine)
!************************************************************************
!     Determines if it is OK to split line SplitLine w/ line NewLine
!     based on contstraints for decay sequence.
!************************************************************************
      implicit none

! Constants

      include 'params.inc'

! Arguments

      integer NewLine    !Line number of new line (always > 0)
      integer SplitLine  !Line number of line to be split (<0 = internal line >0 external)

! Local
      integer maxmatch    !Finds best match possible
      integer diff       !Difference between two groupsIDs
      integer nfail
      integer i,k
      logical lwrite
      integer gid1, gid2

! Global
      integer                  ReqMatchLevel(-maxlines:maxlines)
      common /to_ReqMatchLevel/ReqMatchLevel

!  External

      integer GroupID
      external GroupID
      integer matchlevel
      external matchlevel

!-----------
! Begin Code
!-----------
c
c     First determine maximum possible level of matching
c     Note, only need to consider external lines, never
c     get higher level of matching from internal lines.
c
      lwrite =  splitline .lt. 0 
      lwrite = .false.
       if (lwrite) write(*,*) 'Checking ',newline,splitline,
     $     groupid(newline),groupid(splitline)
      maxmatch = 0    !No matching digits
      gid1 = groupid(newline)
      do i = 1, newline-1
         gid2 = groupid(i)
         k = matchlevel(gid1,gid2)
         if (k .gt. maxmatch) maxmatch = k
      enddo
      if (gid1 .eq. 0) maxmatch = 0  !group 0 doesn't need to match

      gid2 = groupid(splitline)
      k = matchlevel(gid1,gid2)      
      if (k .ge. maxmatch  .and. k .ge. reqmatchlevel(splitline)) then
         splitlineok = .true.
      else
         splitlineok = .false.
c         write(*,*) 'Failed splitline'
      endif
      if (splitlineok) then
c         write(*,*) "Good split",newline,splitline,
c     $        groupid(newline),groupid(splitline)
      endif
      if (lwrite)  write(*,*) 'Require match',nfail,splitlineok
c      splitlineok = .true.
      end


      Subroutine SetMaxAllowedDev(tops,ntop)
!************************************************************************
!     Determines the maximum acceptable deviation from the GroupID that
!     is allowed to split the current line.
!     This is simply the maximum deviation from the other lines it is
!     currently attached to. 
!************************************************************************
      implicit none
! Constants

      include 'params.inc'

! Arguments
      integer tops(0:4,0:maxnodes,0:maxtops),ntop

! Local      
      integer i,j,k
      integer gid1,gid2
      integer MinMatch

! Global
      integer                  ReqMatchLevel(-maxlines:maxlines)
      common /to_ReqMatchLevel/ReqMatchLevel

! External
      integer MatchLevel
      external MatchLevel
      integer GroupID
      external GroupID

!-----------
! Begin Code
!-----------
      do i = -maxlines,maxlines
         ReqMatchlevel(i) = 8
      enddo
      do i=1,tops(1,0,ntop)    !Loop through each vertex
         minmatch = 8
         gid1 = groupid(tops(1,i,ntop))
c
c     Note, don't need to check all pairings to get minlevel
c     Checking 1 line w/ all others is sufficient
c
         do j = 2,tops(0,i,ntop) !Loop through each line
            gid2=groupid(tops(j,i,ntop))
            k = matchlevel(gid1,gid2)
            if (k .lt. minmatch) then 
               minmatch = k
            endif
         enddo
c
c        Now set value for each line
c
         do j=1,tops(0,i,ntop) 
            k = tops(j,i,ntop)  !Current line working on
            if (ReqMatchLevel(k) .gt. minmatch) then
               ReqMatchLevel(k) = minmatch
            endif
         enddo
      enddo
c      write(*,*) (reqmatchlevel(i),i=-6,6)
      end



      Subroutine SetMaxAllowedMatch(tops,ntop)
!************************************************************************
!     Determines what a propagators maximum group level is
!     This is basically its matching to the lines in the
!     "right-most" vertex. Furthest down decay. But we don't know
!     what is "right-most" so just use the "best" match
!************************************************************************
      implicit none
! Constants

      include 'params.inc'

! Arguments
      integer tops(0:4,0:maxnodes,0:maxtops),ntop

! Local      
      integer i,j,k
      integer gid1,gid2
      integer MinMatch

! Global
      integer                  MaxMatchLevel(-maxlines:maxlines)
      common /to_MaxMatchLevel/MaxMatchLevel

! External
      integer MatchLevel
      external MatchLevel
      integer GroupID
      external GroupID

!-----------
! Begin Code
!-----------
      do i = -maxlines,maxlines
         MaxMatchlevel(i) = 0  !Assume not matching
      enddo
c      write(*,*) "MaxMatchlevel = 0"
      do i=1,tops(1,0,ntop)    !Loop through each vertex
         minmatch = 8
         gid1 = groupid(tops(1,i,ntop))
c
c     Note, don't need to check all pairings to get minlevel
c     Checking 1 line w/ all others is sufficient
c
         do j = 2,tops(0,i,ntop) !Loop through each line
            gid2=groupid(tops(j,i,ntop))
            k = matchlevel(gid1,gid2)
c            if (tops(1,i,ntop) .eq. -1 .or. tops(j,i,ntop).eq.-1) then
c               write(*,*) "Matchlevel",tops(1,i,ntop),tops(j,i,ntop)
c     $              ,gid1,gid2,k
c            endif
            if (k .lt. minmatch) then 
               minmatch = k
            endif
         enddo
c
c        Now set value for each line
c
         do j=1,tops(0,i,ntop) 
            k = tops(j,i,ntop)  !Current line working on
            if (MaxMatchLevel(k) .lt. minmatch) then
c               if (k .eq. -1) write(*,*) 'reset maxmatchlevel',k,
c     $              maxmatchlevel(k),minmatch
               MaxMatchLevel(k) = minmatch
            endif
         enddo
      enddo
c      write(*,*) (reqmatchlevel(i),i=-6,6)
      end


      integer function MatchLevel(GID1,GID2)
!************************************************************************
!     Determines the maximum acceptable deviation from the GroupID that
!     is allowed to split the current line.
!     This is simply the maximum deviation from the other lines it is
!     currently attached to. 
!************************************************************************
      implicit none

! Constants


! Arguments

      integer GID1, GID2    !Group ID's that we are checking for match

! Local
      character*9 buff1, buff2 
      integer i
!-----------
! Begin Code
!-----------
      write(buff1,'(i8)') gid1
      write(buff2,'(i8)') gid2
      i=1
      do while (buff1(i:i) .eq. buff2(i:i) .and. i .lt. 9
     $     .and. buff1(i:i) .ne. "0")
         i=i+1
      enddo
      MatchLevel=i-1
c      write(*,*) buff1,buff2,matchlevel
      end

	  
