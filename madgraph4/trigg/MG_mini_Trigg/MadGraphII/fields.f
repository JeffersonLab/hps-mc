!-----------------------
! madgraph - a Feynman Diagram package by Tim Stelzer and Bill Long 
! (c) 1993
!
! Filename: fields.f
!-----------------------
               
      Subroutine InsertFields(tops,graphs)
!****************************************************************************
!     Given a topology list, and the external particles, check which
!     graphs work, and which don't
!****************************************************************************
      implicit none

! Constants

      include 'params.inc'

! Arguments

      integer tops(0:4,0:maxnodes,0:maxtops)
      integer graphs(0:maxgraphs,0:maxlines) !graphs, topology internal lines

! Local Variables

      integer i,ntop, tid(maxlines),top1(0:4,0:maxnodes,0:1)
      integer ngraphs,wold,iswitch,nswitch
      integer i_this,i_max
      logical done

! Global Variables

      integer        iline(-maxlines:maxlines),idir(-maxlines:maxlines)
      integer        this_coup(max_coup) ,goal_coup(max_coup)
      common/to_proc/iline,idir,this_coup,goal_coup

      character*(4*max_particles) particle(4)
      integer                               charge_c(max_particles)
      integer iparticle(0:max_particles,0:4),inverse(max_particles)
      common/to_model/iparticle,  particle,  inverse, charge_c

      integer             switchline(maxlines,2**maxlines)
      common /to_majarana/switchline

      character*25 name
      integer iname
      common/to_name/iname,name

      logical          this_block(0:max_blocks), need_block(max_blocks)
      common/to_blocks/this_block              , need_block

      integer nskip
      logical skipgraph(maxgraphs)
      common /to_skip/nskip,skipgraph

      logical         major_exist
      common/to_major/major_exist

      integer          gopt              !Speed optimization setting
      logical                 pmatch
      common/to_gmatch/gopt,  pmatch

      character*60 proc
      integer iproc
      common/to_write/iproc,proc

      data wold/0/

!-----------
! Begin Code
!-----------
      wold       = 0
      ngraphs    = 0
      graphs(0,0)= 0
      nskip      = 0
      i_max      = 0
      pmatch     = .true.
      do i=1,max_blocks
         need_block(i)=.false.
      enddo
      do i=1,maxgraphs
         skipgraph(i)=.false.
      enddo
      open(unit=4,file='.'//name(1:iname),status='old',err=11)
      write(*,*) 'Skipping graphs in file .',name(1:iname)
      i=0
      do while (.true. .and. i .le. maxgraphs)
         read(4,*,end=11,err=11) i
         if (i .le. maxgraphs) skipgraph(i)=.true.
      enddo
 11   close(4)
c
c     Reset topologies
c
      tid(4) = 1        !This reset for case when nexternal=3
      do i=4,iline(0)
         tid(i) = 1
      enddo
      tid(iline(0)) = 0  !Rest so get first topology
c      write(*,*) 'Calling next_top'
      call next_top(tid,tops,iline(0),done)
      tops(0,0,0) = 0 
     
      call setswitch(switchline,iline(0),nswitch)
      if (.not. major_exist) nswitch = 1   !If no majorana don't need to loop
      Write(*,'(a$)') 'Progress '
      do iswitch = 1,nswitch         !Loop over fermion flows (Denner)
         Write(*,'(i4,a$)') int(100.*dble(iswitch-1)/nswitch),'%'
         do i=1,iline(0)
            if (switchline(i,iswitch) .eq. 1) then
               iline(i)=charge_c(iline(i))
               idir(i) = -1
            else
               idir(i) = 1
            endif
         enddo
c         write(*,*) 'Ngraphs',ngraphs
c         write(*,'(a,10i4)') 'Switched',(iline(i),i=1,iline(0))
c
c     I_THIS loop, checks to see which fermions are connected. It
c     only accepts one set of configurations with each loop through.
c     This was originally needed to avoid double counting essentially
c     charge conjugate graphs. It keeps you from having the same fermion 
c     connections in a new (nswitch) than you had with an old one.
c     obviously it can make things slow!
c
         i_this = i_max
         do while(i_this .le. i_max) !loop over fermion configurations
            i_this=i_this+1         
c            write(*,'(a,8i4)') 'Particles',(iline(i),i=1,iline(0))
c            write(*,*) i_this, i_max, nswitch
c            write(*,*) iswitch,nswitch
c
c     Here's where we loop over all of the topologies
c
c     Reset tid so start w/ first topology
c
            tid(4) = 1
            do i=4,iline(0)
               tid(i) = 1
            enddo
            tid(iline(0)) = 0   !Reset so incrementing gives 1st top
            call next_top(tid,top1,iline(0),done)
c            write(*,*) 'Gen1top'
c            call gen1top(iline(0),top1,tid,done)   'orig....
            do while (.not. done)
c            do ntop=1,tops(0,0,0) !loop over topologies
               call copytops(top1,tops,1)
               ntop = tops(0,0,0)
               call checktop(ntop,wold,ngraphs,i_this,i_max,tops,graphs)
               call next_top(tid,top1,iline(0),done)

c
c     Optimization
c         gopt=2   means only looking for identical matches
c         gopt=1   means looking for good processes (and matches)
c
               if (gopt .eq. 2 .and. .not. pmatch) done=.true.
               if (gopt .eq. 1 .and. ngraphs.gt. 0 .and. .not. pmatch)
     &              done=.true.
c               if (done) then
c                  write(*,*) 'Exiting ',gopt,pmatch,ngraphs
c               endif

            enddo
            write(*,*) 'finished topology', i_this, i_max
         enddo
         do i=1,iline(0)
            if (switchline(i,iswitch) .eq. 1)iline(i)=charge_c(iline(i))
         enddo
         switchline(maxlines,iswitch)=ngraphs
      enddo
c      Write(*,*)
      if (ngraphs .gt. 0 .and. .false.) then
         open(unit=4,file='.'//name(1:iname),status='new',err=99)
         write(*,*) 'Writing good graph numbers in file .',name(1:iname)
         write(4,*) maxgraphs+1
         do i=1,ngraphs
            write(4,*,err=99) i
         enddo
 99      close(4)
      endif
      if (ngraphs+nskip+1 .gt. maxgraphs) then
         open(unit=itnum,file='overflow.html',status='unknown',
     $        access='append')
         write(itnum,*) '<hr>'
         write(itnum,*) '<H2> <font color="red"> '
         write(itnum,*) '<blink> Error </blink>'
         write(itnum,*) 'too many diagrams in subprocess '
         write(itnum,*) proc(1:iproc),'</font> </H2>'
         write(itnum,*) 'Reconfigure parameter maxgraphs from '
         write(itnum,*) maxgraphs,' to ',ngraphs+nskip+1
         write(itnum,*) '<hr>'
         close(itnum)
      endif

      end
      

      subroutine checktop(ntop,wold,ngraphs,i_this,i_max,tops,graphs)
c***********************************************************************
c     Checks 1 topology and gets all the graphs possible for that
c     topology
c**********************************************************************

! Constants

      include 'params.inc'

! Arguments

      integer ntop,wold,ngraphs,i_this,i_max
      integer tops(0:4,0:maxnodes,0:maxtops)
      integer graphs(0:maxgraphs,0:maxlines) !graphs, topology internal lines
      

! Local Variables

      integer color(0:2*maxlines,0:maxfactors,0:maxterms)
      integer i,nvert,n(maxnodes),nextchange
      logical done,fail,oneit,more,passed
      logical good_topology
      integer ifac

! Global Variables

      integer        iline(-maxlines:maxlines),idir(-maxlines:maxlines)
      integer        this_coup(max_coup) ,goal_coup(max_coup)
      common/to_proc/iline,idir,this_coup,goal_coup

      integer alen(maxgraphs)
      integer wnum(-maxlines:maxlines),wcount,wlen(maxgraphs*maxlines)
      common/toopt/wcount,wnum,wlen,alen

      logical          this_block(0:max_blocks), need_block(max_blocks)
      common/to_blocks/this_block              , need_block

      integer nskip
      logical skipgraph(maxgraphs)
      common /to_skip/nskip,skipgraph

!-----------
! Begin Code
!-----------
      good_topology = .false.              !No need to keep this topology
c
c     Set multiplicity of vertices to 1
c
      do i=1,maxnodes
         n(i)=1           
      enddo
c
c     Loop until vertices w/ multiple choices have been tried
c      
      done=.false.
      do while (.not. done)
         call setcolors(color)  !Reset color factor
         call clearcolor        !Reset fast color
         call write_external(iline(0))
         do i=1,maxlines
            iline(-i) = 1       !reset internal lines as unknown
            idir(-i)  = 1       !reset charge conjugation on unknown lines
         enddo
c
c        Reset couplings for this graph to zero
c
         do i=1,max_coup
            this_coup(i) = 0
         enddo
c
c        Reset blocks need
c
         do i=1,max_blocks
            this_block(i)=.false.
         enddo
c
c        set topology to look at first vertex
c
         nvert=0
         oneit=.true.
         nextchange=0
         do while (oneit)                       !loop over vertices
            nvert=nvert+1
            more = .false.                      !Assume no more doubles
            if (tops(0,nvert,ntop) .eq. 3) then !3 particle vertex
               call check3v(
     &              (tops(1,nvert,ntop)),
     &              (tops(2,nvert,ntop)),
     &              (tops(3,nvert,ntop)),
     &              n(nvert),fail, more,oneit,ngraphs,color)
            else
               call check4v(
     &              (tops(1,nvert,ntop)),
     &              (tops(2,nvert,ntop)),
     &              (tops(3,nvert,ntop)),
     &              (tops(4,nvert,ntop)),
     &              n(nvert),fail, more,oneit,ngraphs,color)
            endif
            if (more) nextchange=nvert
            if (.not. fail .and. .not. oneit) then
               call user_filter(tops(0,0,ntop),passed)
               if (passed)
     $              call getfermifact(tops(0,0,ntop),
     $              ifac,i_this,i_max,passed)
               fail = (.not. passed)
            endif
            if (.not. oneit .and. (skipgraph(ngraphs+nskip+1) .or.
     &           ngraphs .eq. maxgraphs-1) .and. .not. fail ) then
               print*,'Skipping',ngraphs+nskip+1,ngraphs
               nskip=nskip+1                !We don't want this graph
               fail=.true.                  !Just pretend it failed
            endif
            if (fail) then
               oneit=.false.                !Vertex didn't work, done
               wcount=wold                  !with this set
            endif
            if (.not. fail .and. .not. oneit) then
               call save_graph(wold,wcount,ngraphs,ntop,ifac,
     &              color,graphs)
               good_topology = .true.       !Need to save this topology
c               write(*,*) '*****   Saved ********',ngraphs,ntop
               call write_config(tops(0,0,ntop),ngraphs)
            endif
         enddo
         if (nextchange .ne. 0) then
            n(nextchange) = n(nextchange)+1
            do i=nextchange+1,maxnodes
               n(i)=1
            enddo
         else
            done=.true.
         endif
         nextchange=0
      enddo
c      good_topology = .true.
      if (.not. good_topology) then
         tops(0,0,0) = tops(0,0,0)-1       !Don't keep this topology
      endif
      ntop=tops(0,0,0)
      end

      subroutine user_filter(tops,passed)
c***********************************************************************
c     Checks to see if diagram satisfies other user constraints
c     Like having or excluding internal lines
c***********************************************************************
      implicit none
c
c     Parameters
c
      include 'params.inc'
c
c     Arguments
c
      integer  tops(0:4,0:maxnodes)
      logical passed
c
c     Local
c
      logical found, t_connect, tc(-maxlines:maxlines)
      integer i,j,ivert,ltype(-maxlines:maxlines),itype
      integer  iplace,ipart
      logical ismaj,notpass
      integer mygroup, reqlevel, matchedline
c
c     Global
c
      integer       req_part(0:max_particles),exc_part(0:max_particles)
      integer       nores_part(0:max_particles)
      common/to_filter/req_part,exc_part,nores_part

      integer        iline(-maxlines:maxlines),idir(-maxlines:maxlines)
      integer        this_coup(max_coup) ,goal_coup(max_coup)
      common/to_proc/iline,idir,this_coup,goal_coup

      character*(4*max_particles) particle(4)
      integer                               charge_c(max_particles)
      integer iparticle(0:max_particles,0:4),inverse(max_particles)
      common/to_model/iparticle,  particle,  inverse, charge_c

      integer ndcmax
      parameter(ndcmax=20)
      integer ndcpart,idcid(ndcmax),idcgroup(ndcmax)
      logical dcfinal(ndcmax)
      common/to_dc/ndcpart,idcid,idcgroup,dcfinal
c
c     This is information about line connections
c
      integer                  MaxMatchLevel(-maxlines:maxlines)
      common /to_MaxMatchLevel/MaxMatchLevel
      integer                  ReqMatchLevel(-maxlines:maxlines)
      common /to_ReqMatchLevel/ReqMatchLevel

      logical            lForceBW(-maxlines:0)
      common/to_localFBW/lForceBW

!
!   External
!
      integer GroupID, matchlevel
      external GroupID,matchlevel
      logical majarana
      external majarana
      logical is_fermion
      external is_fermion

!-----------
! Begin Code
!-----------      
c
c     Start by eliminating  the excluded particles since it should
c     be pretty quick
c

      passed = .true.
      i = -1
      do while (i .ge. -iline(0)+3 .and. passed)     !Loop over internal lines
         j=1
         lForceBW(i)=.false.                         !Reset so don't require on-shell
         do while(j .le. exc_part(0) .and. passed)
            passed = iline(i) .ne. exc_part(j)
            j=j+1
         enddo
         i=i-1
      enddo
      if(.not. passed) return
c
c     Now look for required s-channel particles
c
c
c     First we need to determine if internal lines are s or t channel
c     If count p1 and p2 as t-channel then each vertex can have either 
c     0 or 2 t-channel lines.
c
c     Note this doesn't  make sense if there are NOT 2 incoming partons
c

      do i=1,iline(0)
         ltype(i) = 1                  !External lines are s-channel
         ltype(-i)= 0                  !Internal lines are unknown
         tc(i) = .false.
         tc(-i) = .false.
      enddo
      ltype(1)=-1                      !Incoming lines t-channel
      ltype(2)=-1                      !Incoming lines t-channel
      tc(1) = .true.
      tc(2) = .true.

      do ivert=1,tops(1,0)-1
         itype=1
         iplace=0
         t_connect=.false.
         do i = 1,tops(0,ivert)
            if (ltype(tops(i,ivert)) .ne. 0) then
               itype=itype*ltype(tops(i,ivert))
               t_connect = (t_connect .or. tc(tops(i,ivert)))
            else
               iplace=tops(i,ivert)
            endif
         enddo
         if (iplace .eq. 0) write(*,*) 'Error with iplace'
         ltype(iplace)=itype
         tc(iplace)=t_connect
c         write(*,*) 'iline, t_connect',iplace,t_connect
c     &        ,(tops(i,ivert),i=1,tops(0,ivert))
      enddo
c      write(*,'(4i6)') tops(0,0),(ltype(i),i=-1,-iline(0)+3)

      passed = .true.
      i = 1
      do while (passed .and. i .le. req_part(0))
c         write(*,*) 'Looking for particle',i,
c     $        req_part(i),charge_c(req_part(i)), inverse(req_part(i))
         ismaj  = (charge_c(req_part(i)) .eq. inverse(req_part(i)))
         j=1
         passed = .false.
         do while(.not. passed .and. j .le. iline(0)-3)
            if (tc(-j)) then
               ipart = inverse(iline(-j))
            else
               ipart = iline(-j)
            endif
c
c     tjs  11/1/2007
c     Added to take care of majorana particles
c
            if (ismaj) then
            passed = ((ipart .eq. req_part(i) .or.
     &           charge_c(ipart) .eq. req_part(i)).and. ltype(-j).eq.1)
            else
            passed = ((ipart .eq. req_part(i)).and. ltype(-j).eq.1)
            endif
c            if (passed) write(*,*) 'Found it',j,iline(-j)
            j = j+1
         enddo
         i=i+1
      enddo
c      write(*,'(9i6)') (iline(-j),j=1,iline(0)-3)
c      write(*,'(9i6)') (ltype(-j),j=1,iline(0)-3)


c
c     Now check for particles associated with each decay group
c
      
      do i = 1, ndcpart
c         mygroup = 1
c         do while (idcgroup(i) .ge. mygroup*10000000)
c            mygroup=mygroup+1
c         enddo
c         mygroup=mygroup-1
c         write(*,*) 'Group',i,idcgroup(i),mygroup,idcid(i),dcfinal(i)         
         matchedline=0
         if (.not. dcfinal(i)) then  !This is particle that decays
            passed = .false.         !Only pass if find particle

            reqlevel = matchlevel(idcgroup(i),idcgroup(i))

c            write(*,*) 'Checking for mother',i,idcid(i),reqlevel
            do j=-1,-iline(0)+3,-1      !Check each internal line
c
c     Here we need to make sure this is the mother. This is moved to end 
c     so can use information about line versus inverse of line
c     Mother is defined as MaxMatchLevel > ReqMatchLevel
c
c               write(*,*) 'Checking line',j,idcgroup(i),groupid(j)
c     $              ,maxmatchlevel(j)

c               maxmatchlevel(j) = 1

               if (matchlevel(groupid(j),idcgroup(i)).ge.reqlevel
     $              .and. maxmatchlevel(j).eq. reqlevel ) then      !Could be match!
                  if (tc(j)) then
                     ipart = inverse(iline(j))
                  else
                     ipart = iline(j)
                  endif
c
c     tjs 8-25-2007
c     Special routine for majarana always want particle, not anti code
c     
                  if (majarana(ipart) .and.
     $                 ipart .gt. inverse(ipart)) then
                     ipart = inverse(ipart)
                  elseif (is_fermion(ipart) .and.
     $                    charge_c(ipart) .lt. ipart) then
c
c     tjs 3-12008
c     Added for reverse-flow always want particle, not charge_c
c
                     if(charge_c(ipart) .gt. 0) ipart = charge_c(ipart)                     
                  endif
c                  write(*,*) 'Possible match',ipart,idcid(i)
c     $                 ,inverse(ipart)
                  if (idcid(i) .eq. ipart) then 
c
c                 Here is where we need to flag that this particle 
c                 MUST be on shell for the calculation to be valid.
c
                     matchedline=j
                     passed = .true.
c                     write(*,*) 'Passed', i,lforcebw(i)
                  endif
c                  if (idcid(i) .eq. inverse(iline(j))) passed = .true.
c                  if (idcid(i) .eq. charge_c(iline(j))) passed = .true.
c                  if (idcid(i) .eq. charge_c(inverse(iline(j))))
c     $                 passed = .true.
c                  write(*,*) i,j,idcid(i),iline(j),inverse(iline(j))
               endif
            enddo
            if (passed) lForceBW(matchedline)=.true.
c            if (passed) write(*,*) 'We passed'
            if (.not. passed) then
c               write(*,*) 'Failed'
               return
            endif
         endif
      enddo
       
c
c    now check for excluded s-channel resonances
c
      i = 1
      do while (i .le. nores_part(0))
c         write(*,*) 'Looking for particle',i,
c     $        nores_part(i),charge_c(nores_part(i)), inverse(nores_part(i))
         ismaj  = (charge_c(nores_part(i)) .eq. inverse(nores_part(i)))
         j=1
         do while( j .le. iline(0)-3)
            if (tc(-j)) then
               ipart = inverse(iline(-j))
            else
               ipart = iline(-j)
            endif
c
c     Take care of majorana particles
c
            if (ismaj) then
               notpass = ((ipart .eq. nores_part(i) .or.
     &           charge_c(ipart).eq.nores_part(i)) .and. ltype(-j).eq.1)
            else
              notpass = ((ipart .eq. nores_part(i)).and. ltype(-j).eq.1)
            endif
            if (notpass) then
               write(*,*) 'Found excluded resonance ',j,iline(-j)
               passed = .not.notpass
               return
            endif
            j = j+1
         enddo
         i=i+1
      enddo
      end

c
c   TJS  Added is_fermion 3-14-2008
c           
      logical function is_fermion(ipart)
c***********************************************************************
c     Returns true if particle ipart is a fermion
c***********************************************************************
      implicit none
c
c     Constants
c
      include 'params.inc'
      integer ipart
c
c     Global
c
      character*(max_string) iwave(max_particles),owave(max_particles)
      integer iposx(3,max_particles)
      integer info_p(5,max_particles)
      character*(8) str(3,max_particles)
      common/to_external/iwave,owave,iposx,info_p,str
c-----
c  Begin Code
c-----

      is_fermion = (info_p(3,ipart) .eq. 1)

      end



      subroutine save_graph(wold,wcount,ngraphs,ntop,ifac,color,graphs)
c***********************************************************************
c     Once a good graph has been found, it must be stored
c     along with its color information
c***********************************************************************
      implicit none
c
c     Constants
c
      include 'params.inc'
c      integer    maxgraphs
c      parameter (maxgraphs=999)
c      integer    maxlines,  maxfactors,  maxterms,    maxflows
c      parameter (maxlines=8,maxfactors=9,maxterms=250,maxflows=200)
c      integer    max_particles       , max_coup
c      parameter (max_particles=2**7-1, max_coup=5)
c
c     Arguments
c
      integer color(0:2*maxlines,0:maxfactors,0:maxterms)
      integer ifac,wcount,wold,ngraphs,ntop
      integer graphs(0:maxgraphs,0:maxlines) !graphs, topology internal lines
c
c     Local
c
      integer i
c
c     Global
c
      integer flows(0:2*maxlines,0:maxfactors,0:24,0:maxflows)
      integer         graphcolor(0:2,0:maxflows,0:maxgraphs)
      common/to_color/graphcolor,flows
      integer            symfact(maxgraphs)
      common /tosymmetry/symfact

      integer        iline(-maxlines:maxlines),idir(-maxlines:maxlines)
      integer        this_coup(max_coup) ,goal_coup(max_coup)
      common/to_proc/iline,idir,this_coup,goal_coup

      logical          this_block(0:max_blocks), need_block(max_blocks)
      common/to_blocks/this_block              , need_block

      logical            lForceBW(-maxlines:0)
      common/to_localFBW/lForceBW

      logical             gForceBW(-maxlines:0,maxgraphs)
      common/to_globalFBW/gForceBW

! External
      
      Integer foundflow
c-----
c  Begin Code
c-----
c      write(*,*) 'Keeping',ngraphs+1
      ngraphs=ngraphs+1
      if (ngraphs .eq. maxgraphs) then
         write(*,*) 'Error too many graphs, reconfigure parameter',
     &        'maxgraphs = ',maxgraphs
         write(*,*) 'All future graphs will be skipped.'
      endif
      symfact(ngraphs)=ifac
      call flushbuff(wold)
      wold=wcount
      call flushamp(ngraphs)
      call performColor(color)  !load all color info for graph
c     call printcolors(color)
      call simpcolors(color,-1) !Sum over internal colors
      call orderF(color)
      call addterms(color)
      call OrderColors(color)
c      call printcolors(color)
      graphcolor(0,0,0) = ngraphs
      graphcolor(0,0,ngraphs) = color(0,0,0)
      do i=1,color(0,0,0)          !This is the number of terms
         graphcolor(0,i,ngraphs) = foundflow(color,flows,i)
         graphcolor(1,i,ngraphs) = color(1,1,i) !This is numerator
         graphcolor(2,i,ngraphs) = color(2,1,i) !This is denomenator
      enddo
      if (ngraphs .le. maxgraphs) then
         graphs(0,0)=ngraphs
         graphs(ngraphs,0)=ntop
         do i=1,maxlines
            graphs(ngraphs,i) = iline(-i)
         enddo
         do i=1,maxlines
            gForceBW(-i,ngraphs)=lForceBW(-i)
         enddo
c         do i=1,max_blocks
c            need_block(i)=(need_block(i) .or. this_block(i))
c            if (this_block(i)) write(*,*) 'Used coupling',i
c         enddo
      elseif (ngraphs .eq. maxgraphs+1) then
         print*,'Sorry Maxgraphs is set to ',maxgraphs
      endif
      end


      subroutine setswitch(switchline,iline,nswitch)
c*************************************************************************
c     Subroutine to set up switching of directions of Majarana 
c     fermions.  Switchline contains 1 or -1 which tell it weather
c     to switch or not.  Majarana fermions may be switched all others
c     will never be switched
c*************************************************************************
      implicit none
c
c     Constants
c
      include 'params.inc'
c
c     Arguments
c
      integer switchline(maxlines,2**maxlines),iline(0:maxlines)
      integer nswitch
c
c     Local
c
      integer i,npart,istep(maxlines)
c
c     Global
c
      character*(4*max_particles) particle(4)
      integer                               charge_c(max_particles)
      integer iparticle(0:max_particles,0:4),inverse(max_particles)
      common/to_model/iparticle,  particle,  inverse, charge_c

      character*(max_string) iwave(max_particles),owave(max_particles)
      integer iposx(3,max_particles)
      integer info_p(5,max_particles)
      character*(8) str(3,max_particles)
      common/to_external/iwave,owave,iposx,info_p,str

      logical flip(maxlines)
      common/to_flip/flip
c
c     External
c
      logical majarana
c-----
c  Begin Code
c-----      
      npart = iline(0)
c
c     This should be checking for Majarana Particles, but it
c     it appears to be switching any fermions. This is taken care of
c     by GetFermiFact which makes sure all of the lines are OK
c     (Not quite try uu~ > uu~) Currently don't loop over Dirac fermion
c
      do i=1,npart
         if (info_p(3,iline(i)) .eq. 1 .and. flip(i)) then  !Fermion
            if (majarana(iline(i))) then
               istep(i) = 2
            else
               istep(i) = 2         !Not Majarana, b
            endif
         else
            istep(i) = 3
         endif
      enddo
      call sethel(switchline(1,1),istep,npart,nswitch)
      end

