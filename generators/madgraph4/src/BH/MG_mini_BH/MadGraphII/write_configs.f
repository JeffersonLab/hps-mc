!-----------------------
! madgraph - a Feynman Diagram package by Tim Stelzer and Bill Long 
! (c) 1993
!
! Filename: write_configs.f
!-----------------------

      subroutine write_config(tops,igraph)
c***********************************************************************
c     Writes phase space configuratio for diagram igraph, including
c     masses and widths of s-channel propagators.
c***********************************************************************
      implicit none
c
c     Parameters
c
      include 'params.inc'
c
c     Arguments
c
      integer  tops(0:4,0:maxnodes),igraph

c
c     Local
c
      logical found, t_connect, tc(-maxlines:maxlines),passed
      integer i,j,ivert,ltype(-maxlines:maxlines),itype
      integer  iplace,ipart
      integer s_count(0:3,0:maxnodes),map(-maxlines:maxlines)
      integer t_count(0:3,0:maxnodes)
      integer l1,l2,l3,jprop,t_match,ic,ilasts
      
      integer iconfig
      save    iconfig

c
c     Global
c
      integer        iline(-maxlines:maxlines),idir(-maxlines:maxlines)
      integer        this_coup(max_coup) ,goal_coup(max_coup)
      common/to_proc/iline,idir,this_coup,goal_coup

      character*(4*max_particles) particle(4)
      integer                               charge_c(max_particles)
      integer iparticle(0:max_particles,0:4),inverse(max_particles)
      common/to_model/iparticle,  particle,  inverse, charge_c


      character*(max_string) iwave(max_particles),owave(max_particles)
      integer iposx(3,max_particles)
      integer info_p(5,max_particles)
      character*(8) str(3,max_particles)

      integer         nincoming,nincfirst
      common/to_proc2/nincoming,nincfirst

      common/to_external/iwave,owave,iposx,info_p,str

      logical             gForceBW(-maxlines:0,maxgraphs)
      common/to_globalFBW/gForceBW


!-----------
! Begin Code
!-----------      
c
c     We need to use a separate counter for igraph and iconfig since
c     some graphs will not have configurations
c
      if (igraph .eq. 1) iconfig=0
      if (igraph .eq. -99) then
         write(35,'(6x,a,i4,a,i4,a)')
     $           'data mapconfig(',0,') /',iconfig,'/'
         write(59,'(6x,a)') 'integer    n_max_cg'      
         write(59,'(6x,a,i4,a)')'parameter (n_max_cg= ',iconfig,')'
      endif
      if (igraph .gt. 0) call flip_props(tops) ! This is the new line

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
         map(i)=i
         map(-i)=0
      enddo
      ltype(1)=-1                      !Incoming lines t-channel
      ltype(2)=-1                      !Incoming lines t-channel
      tc(1) = .true.
      tc(2) = .true.
      passed = .true.                  !We throw out graphs w/ 4 vertex

      s_count(0,0)=0
      t_count(0,0)=0
      do ivert=1,tops(1,0)
         itype=1
         iplace=0
         t_connect=.false.
         l1=0
         l2=0
         l3=0
         if (tops(0,ivert) .eq. 3) then
            do i = 1,tops(0,ivert)
               if (ltype(tops(i,ivert)) .ne. 0) then
                  itype=itype*ltype(tops(i,ivert))
                  t_connect = (t_connect .or. tc(tops(i,ivert)))
                  if (l1 .eq. 0) then
                     l1=tops(i,ivert)
                  elseif(l2 .eq. 0) then
                     l2=tops(i,ivert)
                  else
                     l3=tops(i,ivert)
                     iplace=l3
                  endif
               else
                  iplace=tops(i,ivert) !Found unknown line
                  l3 = iplace
               endif
            enddo
         else
            passed=.false.
         endif
         ltype(iplace)=itype
         tc(iplace)=t_connect
c
c     For purposes of writing out configurations, when 2 t-channels meet
c     we write this as a t-channel, even though the resulting propagator
c     is actually s-channel
c
         if (ltype(l1) .eq. -1 .or. ltype(l2) .eq. -1) itype=-1
         if(nincoming.eq.1) itype=+1 ! Only s-channels in decay processes

         if (itype .eq. +1 ) then    !s-channel
            s_count(0,0)=s_count(0,0)+1
            i = s_count(0,0)
            s_count(1,i) = l1
            s_count(2,i) = l2
            s_count(3,i) = l3
            s_count(0,i) = 0
c            map(l3) = -i            
         else                          !t-channel
            t_count(0,0)=t_count(0,0)+1
            i = t_count(0,0)
            t_count(0,i) = 0                      !Got one
            t_count(1,i) = l1
            t_count(2,i) = l2
            t_count(3,i) = l3
         endif
      enddo
c
c     Some configurations we don't use (eg 4 particle vertice)
c


      if (.not. passed) return

      iconfig=iconfig+1
      write(35,'(a,i6)') 'c   Graph ',igraph
      write(37,'(a,4x,i4,6x,i4)') '*',iconfig,igraph
      write(35,'(6x,a,i4,a,i4,a)')
     $           'data mapconfig(',iconfig,') /',igraph,'/'

c
c     Now write stuff out
c
c     s-channel
c     Need to make sure you have vertices in correct order so that
c     there is only 1 unknown line. This is only a problem because
c     the HELAS code may be written using lines 1 and 2 for some of
c     the s-channel stuff.
c
c   For decay processes, need to keep track of s connecting to incoming
      ilasts=0
      do jprop=1,s_count(0,0)
         i=0
         found=.false.
         do while (i .lt. s_count(0,0) .and. .not. found)
            i=i+1
            if (s_count(0,i) .eq. 0) then
               ic = 0
               do j=1,3
                  if (map(s_count(j,i)) .ne. 0) ic = ic+1
               enddo
               if(nincoming.eq.1.and.
     $              s_count(1,i).eq.1.or.s_count(2,i).eq.1) then
                 ilasts=i ! This gives the particle connecting to the 1
               else
                 found= (ic .ge. 2)
               endif
            endif
         enddo
         if (found) then
            s_count(0,i)=1
            l1 = s_count(1,i)
            l2 = s_count(2,i)
            l3 = s_count(3,i)        
            if (map(l2) .eq. 0) then         !Need to swap 2 and 3
               ic=l2
               l2=l3
               l3=ic               
            endif
            if (map(l1) .eq. 0) then         !Need to swap 1 and 3
               ic=l1
               l1=l3
               l3=ic               
            endif
            map(l3) = -jprop
            if(str(2,iline(l3)).eq.'ZERO')then
               write(34,'(6x,a,i3,a,i4,a,a)')
     $           'pmass(',map(l3),',',iconfig,') = ',str(2,iline(l3)) 
            else
               write(34,'(6x,a,i3,a,i4,a,a,a)')
     $           'pmass(',map(l3),',',iconfig,') = abs(',
     $           str(2,iline(l3)),')' 
            endif
            if(str(3,iline(l3)).eq.'ZERO')then
               write(34,'(6x,a,i3,a,i4,a,a)')
     $           'pwidth(',map(l3),',',iconfig,') = ',str(3,iline(l3)) 
            else
               write(34,'(6x,a,i3,a,i4,a,a,a)')
     $           'pwidth(',map(l3),',',iconfig,') = abs(',
     $           str(3,iline(l3)),')'
            endif
         if (info_p(3,iline(l3)) .eq. 1) then          !fermion
            write(34,'(6x,a,i3,a,i4,a,i2)')
     $           'pow(',map(l3),',',iconfig,') = 1'
         else                                          !boson
            write(34,'(6x,a,i3,a,i4,a,i2)')
     $           'pow(',map(l3),',',iconfig,') = 2'
         endif
         write(35,99) map(l3),iconfig,map(l1),map(l2),str(2,iline(l3))
     &        , str(3,iline(l3))
         write(35,92)  map(l3),iconfig,info_p(5,iline(l3))
         write(37,'(2x,i3,2x,i3,2x,i3,2x,a,2x,a,2x,a,2x,i8)')
     $    map(l3),map(l1),map(l2),str(2,iline(l3)),str(3,iline(l3)),'S'
     $    ,info_p(5,iline(l3))

         if (gforcebw(l3,igraph)) then
            write(36,97) map(l3),iconfig,".True."
         else
            write(36,97) map(l3),iconfig,".False."
         endif
         endif
      enddo
c   Write the extra line which connects the last s-channel to the decaying particle
      if(nincoming.eq.1) then
        if (s_count(1,ilasts).eq.1) then
          write(35,99) -jprop+1,iconfig,map(s_count(2,ilasts)),
     &       map(s_count(3,ilasts)), str(2,iline(1)), str(3,iline(1))
        else
          write(35,99) -jprop+1,iconfig,map(s_count(1,ilasts)),
     &       map(s_count(3,ilasts)), str(2,iline(1)), str(3,iline(1))
        endif
      endif
c     

c     t-channel requires more work. Need to start with p1
c     and then follow channel down to p2
c
      t_match = 1                         !t-channel string starts with p1
      do jprop=s_count(0,0)+1,tops(1,0)-1 !Loop through all t-channel props
         i = 1
         found = .false.
         do while (i .le. t_count(0,0) .and. .not. found)
            if (t_count(0,i) .eq. 0) then !Haven't used this one yet
               l1=0
               l2=0
               l3=0
               do j=1,3
                  if (t_count(j,i) .eq. t_match) then !Found correct vertex
                     found=.true.
                     l1 = t_count(j,i)
                     t_count(0,i) = 1
                  elseif(map(t_count(j,i)) .ne. 0 .and. l2 .eq. 0) then
                     l2 = t_count(j,i) !This is line already have
                  else
                     l3 = t_count(j,i) !This is propagator we produced
                  endif
               enddo
            endif
            i = i+1
         enddo
         if (found) then         
c            write(*,*) 'Found',t_match,l1,l3
            map(l3) = -jprop
            t_match = l3

            if(str(2,iline(l3)).eq.'ZERO')then
               write(34,'(6x,a,i3,a,i4,a,a)')
     $           'pmass(',map(l3),',',iconfig,') = ',str(2,iline(l3)) 
            else
               write(34,'(6x,a,i3,a,i4,a,a,a)')
     $           'pmass(',map(l3),',',iconfig,') = abs(',
     $           str(2,iline(l3)),')' 
            endif
            if(str(3,iline(l3)).eq.'ZERO')then
               write(34,'(6x,a,i3,a,i4,a,a)')
     $           'pwidth(',map(l3),',',iconfig,') = ',str(3,iline(l3)) 
            else
               write(34,'(6x,a,i3,a,i4,a,a,a)')
     $           'pwidth(',map(l3),',',iconfig,') = abs(',
     $           str(3,iline(l3)),')'
            endif
            if (info_p(3,iline(l3)) .eq. 1) then !fermion
               write(34,'(6x,a,i3,a,i4,a,i2)')
     $              'pow(',map(l3),',',iconfig,') = 1'
            else                !boson
               write(34,'(6x,a,i3,a,i4,a,i2)')
     $              'pow(',map(l3),',',iconfig,') = 2'
            endif

            write(35,99)map(l3),iconfig,map(l1),map(l2),str(2,iline(l3))
     &           , str(3,iline(l3))
            write(35,93)  map(l3),iconfig,info_p(5,iline(l3))
            write(37,'(2x,i3,2x,i3,2x,i3,2x,a,2x,a,2x,a,2x,i8)')
     $  map(l3),map(l1),map(l2),str(2,iline(l3)),str(3,iline(l3)),'T'
     $  ,info_p(5,iline(l3))
               else
            write(*,*) 'Warning could not find t_match',t_match,i
            do j=1,t_count(0,0)
               write(*,*) j,(t_count(i,j),i=1,3) 
            enddo
            stop
         endif
      enddo

c   If this is a decay process A>BC... we are done
      if(nincoming.eq.1) return

c
c     Now we need to write the last t-channel propagator which 
c     corresponds to the vertex which has external line 2
c
      i=0
      found=.false.
      do while (i .lt. t_count(0,0) .and. .not. found)
         i=i+1
         if (t_count(1,i) .eq. 2) found=.true.
         if (t_count(2,i) .eq. 2) found=.true.
         if (t_count(3,i) .eq. 2) found=.true.
      enddo
      if (.not. found) then
         write(*,*) 'Error never found line 2'
         stop
      else
         jprop=-99
         do j=1,3
            if (t_count(j,i) .ne. 2) jprop=max(jprop,map(t_count(j,i)))
         enddo
      endif
      if (s_count(0,0) .lt. tops(1,0)-1) then   !It's not all s-channel
         write(35,99) -tops(1,0),iconfig,-tops(1,0)+1,jprop
      endif


 92   format(6x,'data sprop(',i4,',',i4,') /',i8,'/')
 93   format(6x,'data tprid(',i4,',',i4,') /',i8,'/')
 97   format(6x,'data gForceBW(',i4,',',i4,') /',a8,'/')
c 98   format(6x,'data(spole(',i4,'),swidth(',i4,')/',i3,',',i3,'/')
 99   format(6x,'data(iforest(i,',i3,',',i4,'),i=1,2) /',i3,',',i3,'/',
     &     10x,'!  ',2a)
      end
               
      subroutine flip_props(tops)
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

      integer i,ivert, flipped(-maxlines:maxlines)
      integer ncheck
      logical flip
c

c     Global

c

      integer        iline(-maxlines:maxlines),idir(-maxlines:maxlines)
      integer        this_coup(max_coup) ,goal_coup(max_coup)
      common/to_proc/iline,idir,this_coup,goal_coup

      character*(4*max_particles) particle(4)
      integer                               charge_c(max_particles)
      integer iparticle(0:max_particles,0:4),inverse(max_particles)
      common/to_model/iparticle,  particle,  inverse, charge_c

!-----------

! Begin Code

!-----------

c

c     Note this doesn't  make sense if there are NOT 2 incoming partons

c

      do i=1,iline(0)
         flipped(i)= -1     !Not flipped (external)
         flipped(-i) = 0    !unknown (internal)
      enddo
      flipped(1)=1          !flipped incoming
      flipped(2) =1

      do ivert=1,tops(1,0)-1    !Loop over all but final vertex
         flip = .false.
         ncheck = 0
         do i = 1,tops(0,ivert)
            if (flipped(tops(i,ivert)) .eq. 0) then
               ncheck = tops(i,ivert)
            elseif (flipped(tops(i,ivert)) .eq. 1) then
               flip = .true.
            endif
         enddo
         if (flip) then
            flipped(ncheck)= 1
c            write(*,*) 'Flipping ',ncheck, iline(ncheck),
c     $           inverse(iline(ncheck))
            iline(ncheck) = inverse(iline(ncheck))
         else
            flipped(ncheck)=-1
         endif
      enddo
      end

