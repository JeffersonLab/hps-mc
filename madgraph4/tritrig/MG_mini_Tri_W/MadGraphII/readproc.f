
!-----------------------
! madgraph - a Feynman Diagram package by Tim Stelzer and Bill Long 
! (c) 1993
!
! Filename: readproc.f
!-----------------------
 
      subroutine readproc(more)
!************************************************************************
!     Super-Routine that checks for duplicates
!************************************************************************
      implicit none
      logical more
      logical DupProcess
      external DupProcess
      integer i
!-----------
! Begin Code
!-----------
      i = 1
      call readproc_1(more)
      do while (dupProcess(i) .and. more)
         i=i+1
         call readproc_1(more)
      enddo
      end

      subroutine readproc_1(more)
!************************************************************************
!     Reads character string and converts to the appropriate process
!************************************************************************
      implicit none

! Constants

      include 'params.inc'
c
c     quarkonium stuff
      include 'onia.inc'
c      integer    maxlines
c      parameter (maxlines=8)
c      integer    max_particles
c      parameter (max_particles=2**7-1)
c      integer    max_coup
c      parameter (max_coup=5)
c      integer    max_string
c      parameter (max_string=120)

! Arguments
      
      logical more

! Local Variables

      character*(max_string) process,process_temp
      character*(max_string) attempt
      character*(max_string) uproc
      character*10 snum
      character*4  str1
      character*(max_string) dirstr
      integer i,nparticles,j,imin,ifinal,jj,jstop
      integer nchar,length
      integer k,l,nexc,nlen,kinverse
cfax
      logical foundmatch,done_reading

! Global Variables

      integer        iline(-maxlines:maxlines),idir(-maxlines:maxlines)
      integer        this_coup(max_coup) ,goal_coup(max_coup)
      common/to_proc/iline,idir,this_coup,goal_coup
      integer         nincoming,nincfirst
      common/to_proc2/nincoming,nincfirst
!      data nincfirst/0/
      character*25 name
      integer iname
      common/to_name/iname,name
      character*60 proc
      integer iproc
      common/to_write/iproc,proc

      character*(4*max_particles) particle(4)
      integer                               charge_c(max_particles)
      integer iparticle(0:max_particles,0:4),inverse(max_particles)
      common/to_model/iparticle,  particle,  inverse, charge_c

      character*(max_string) iwave(max_particles),owave(max_particles)
      character*(8) str2(3,max_particles)
      integer info_p(5,max_particles),iposx(3,max_particles)
      common/to_external/iwave,owave,iposx,info_p,str2

      integer       req_part(0:max_particles),exc_part(0:max_particles)
      integer       nores_part(0:max_particles)
      common/to_filter/req_part,exc_part,nores_part

      logical              lwrite
      common/to_multiproc/ lwrite

      logical           jetloop
      common/to_jetloop/jetloop

      integer          gopt              !Speed optimization setting
      logical                 pmatch
      common/to_gmatch/gopt,  pmatch

      logical              cross_opt
      common /to_crossopt/ cross_opt

      integer ndcmax
      parameter(ndcmax=20)
      integer ndcpart,idcid(ndcmax),idcgroup(ndcmax)
      logical dcfinal(ndcmax)
      common/to_dc/ndcpart,idcid,idcgroup,dcfinal

c      character*1         nameprefix
      integer             nameprefix
      common/to_multiname/nameprefix

      data snum /'0123456789'/
!      data jetloop /.false./
!      data gopt/0/

      data S_qn,L_qn,J_qn,C_qn/4*0/
      data onium/.false./
      data em_decay/.false./
      data onium_ID/"????????"/

      save process_temp
!-----------
! Begin Code
!-----------
 1    iname=0
      if (jetloop) call inc_jet(iline(0),jetloop)
      if (.not. jetloop) then
c      call resetV
         write(*,*)'Standard Model particles include:'
         write(*,*)'    Quarks:   d u s c b t d~ u~ s~ c~ b~ t~'
         write(*,*)
     $     '    Leptons:  e- mu- ta- e+ mu+ ta+ ve vm vt ve~ vm~ vt~'
         write(*,*)'    Bosons:   g a z w+ w- h'
         write(*,*)' '
         write(*,*) 'Enter process you would like calculated ',
     &        'in the form  e+ e- -> a.'
         write(*,*) '("done" to exit MadGraph.)'
         process = ' '
cfax
 10      read (*,'(a)',end=11,err=11), process
         if(process(1:1).eq. "#".or.len_trim(process).le.1)  goto 10
ctjs 5-03-06
         uproc = process
         call upper_case(uproc)
 11      if (index(uproc,'DONE') .gt. 0) then
c 11      if (process(1:1) .eq. ' ') then
            more = .false.
            return
         else
            more = .true.
         end if
!         
! Ignore characters following ! and #
!
         i = index(process,'!')
         if (i .gt. 0) then
            process = process(1:i)
         endif
cfax 
         i = index(process,'#')
         if (i .gt. 0) then
            process = process(1:i-1)
         endif
!
! Extract the information on the quarkonium state if it is there,
! and then take it out from the process string.
!
cfax
c-find the parenthesis

         i = index(process,'[')
         j = index(process,']')
         
         if (i.gt.0 .and. j.gt.i) then
c-reset the values to zero
         
            S_qn=0              !spin
            L_qn=0              !orbital momentum
            J_qn=0              !total momentum
            C_qn=0              !color
            onium_ID(1:8)="????????"
            
            process_temp=uproc(i:j)
            call no_spaces(process_temp,length)
            if (length.ge.6) then 
               read (process_temp(2:2),'(i1)') S_qn
               S_qn=(S_qn-1)/2
               if(process_temp(3:3).eq.'S') then
                  L_qn=0
               elseif(process_temp(3:3).eq.'P') then
                  L_qn=1
               endif
               read (process_temp(4:4),'(i1)') J_qn
               read (process_temp(5:5),'(i1)') C_qn
               onium_ID(17-length:8)=process_temp(8:length-1)
               if(17-length.gt.1) then
                 do k=1,16-length
                  onium_ID(k:k)=" "
                 enddo
               endif
            endif
            onium=.true.
            if(process_temp(6:6).eq.'d') em_decay=.true.
            write (*,*) " Found projection over spin and color"
            write (*,*) process_temp(1:length)," : ",
     &           "S =",S_qn,", L =",L_qn,", J =",J_qn,", C =",C_qn,
     & ", onium ID ",onium_ID
            write(*,*) 'iproc',iproc
              
c     
c--   take the [ ] out from the proc string
            do k=j+1,max_string
               process(k-(j-i+1):k-(j-i+1))=process(k:k)
            enddo
         endif

!         
! Set prefix for directory name
!
cfax
         i = index(process,'@')
         nameprefix = 0
         if (i .gt. 0) then
c            namePrefix=process(i+1:i+1)
            read(process(i+1:),*,err=12) nameprefix
 12         process = process(1:i)
         endif
!         
! Parse string for particles
!
         i = index(process,'>')
         call getparticle(process(1:i),iline(1),nincoming)
         if(nincfirst.eq.0) nincfirst=nincoming

         j = index(process(i+1:),'/')+i       !Break for excluded particles
         jj = index(process(i+1:),'$')+i !Break for excluded resonances
         jstop = len_trim(process)          
c
c     Allow user to specify excluded particles   uu~ > w+w-bb~  / t a
c
         if (jj.gt.j) jstop = jj-1
         exc_part(0) = 0
         if (j.gt. i) then
            call getparticle(process(j+1:jstop),exc_part(1),exc_part(0))
            if (exc_part(0) .gt. max_particles) then
               write(*,*) 'Too many particles to exclude'
            endif
c            write(*,*) "Found excluded particles",exc_part(0),
c     $           exc_part(1), exc_part(2)
c
c     Now make sure inverse of the particle is also excluded
c
            nexc = exc_part(0)
            do k=1,exc_part(0)
               kinverse = inverse(exc_part(k))
               foundmatch = .false.
               do l=1,nexc
                  if (kinverse .eq. exc_part(l)) then
                     foundmatch=.true.
                  endif
               enddo
               if (.not. foundmatch) then
                  if (nexc .lt. max_particles) then
                     nexc = nexc+1
                     exc_part(nexc) = kinverse
                     call part_string(kinverse,str1,nlen)
c                    write(*,*) 'Adding exclusion of ',str1
                  else
                     call part_string(kinverse,str1,nlen)
                     write(*,*) 'Exceeded number of exclusions',
     $                    ' can not exclude inverse of ',str1
                  endif
               endif
            enddo
            exc_part(0)=nexc
c            write(*,*) 'Excluding particles',exc_part(0),exc_part(1),
c     $           exc_part(2)

c
c     Now make sure CC of the particle is also excluded
c
            nexc = exc_part(0)
            do k=1,exc_part(0)
               kinverse = charge_c(exc_part(k))
               foundmatch = .false.
               do l=1,nexc
                  if (kinverse .eq. exc_part(l)) then
                     foundmatch=.true.
                  endif
               enddo
               if (.not. foundmatch) then
                  if (nexc .lt. max_particles) then
                     nexc = nexc+1
                     exc_part(nexc) = kinverse
                     call part_string(exc_part(k),str1,nlen)
c                    write(*,*) 'Adding exclusion of CC ',str1
                  else
                     call part_string(exc_part(k),str1,nlen)
                     write(*,*) 'Exceeded number of exclusions ',
     $                    'can not exclude cc of ',str1
                  endif
               endif
            enddo
            exc_part(0)=nexc
c      write(*,*) 'Excluding particles',exc_part(0),exc_part(1),
c     $           exc_part(2)
            

         endif
c
c     Allow user to specify excluded s-channel particles
c     uu~ > w+w-bb~  $ t a
c
         nores_part(0)=0
         cross_opt = .true.     !If no particles required/excluded, use cross_opt
         jstop = len_trim(process)
         if (j.gt.jj) jstop = j-1
         if (jj .gt. i) then
            call getparticle(process(jj:jstop),
     $           nores_part(1),nores_part(0))
c
c          If excluding s-channel particles, can't use crossing optimization
c          since it changes particles for s to t channel
c
            if (nores_part(0) .gt. 0) then
               cross_opt = .false.
               write(*,*) 'Excluding s-channel particles',nores_part(0)
            endif
            if(j.gt.i) then
               jstop=min(j,jj)
            else
               jstop=jj
            endif
         endif
c
c     Check for presence of decay chain processes (specified using '(' and ')')
c
         if(index(process(i+1:jstop),'(').ne.0) then
             call read_dc(process(i+1:jstop),
     &           iline(nincoming+1),nparticles)
           req_part(0)=0
c         exc_part(0) = 0
           cross_opt = .false.  !s-channels required, so can't use crossing
         else

c
c     Allow user to specify intermediate particles
c     uu~ > tt~ > w+w-bb~
c
         j = index(process(i+1:),'>')+i       !Break for required particles
         req_part(0)=0
         if (j .gt. i) then
            call getparticle(process(i+1:j),req_part(1),req_part(0))
            i = j
c
c          If requiring s-channel particles, can't use crossing optimization
c          since it changes particles for s to t channel
c
            if (req_part(0) .gt. 0) then
               cross_opt = .false.
            endif
c            write(*,*) 'Requiring particles',req_part(0)
         endif

c
c     Now  get final state
c
         call read_dc(process(i+1:jstop),iline(nincoming+1),nparticles)
         
       endif
         nparticles = nparticles+nincoming
         iline(0) = nparticles
         call set_jet(iline(0),jetloop)
         j = 21
         attempt = 'Attempting Process: '
         do i=1,iline(0)
            call part_string(iline(i),attempt(j:j+3),nchar)
            j = j+nchar+1
            if (i .eq. nincoming) then
               attempt(j:j+2) = '-> '
               j = j+3
            endif
         enddo
         write (*,'(1x,a)') attempt(1:j)
         iproc = j-18
         proc = attempt(19:j)

C      Set new particle ids in decay chain info
         ifinal=nincoming
         do i = 1, ndcpart
           if(dcfinal(i)) then
             ifinal = ifinal+1
             if(ifinal.le.nparticles) then
               print *,'Setting particle ',i,idcid(i),
     $            ' to id ',iline(ifinal)
               idcid(i) = iline(ifinal)
             endif
           endif
         enddo

         if (nparticles .le. 0) then
            write(*,*)'No process specified. Try again'
            goto 1
c            more = .false.
c            return
         endif
         if (iline(0) .lt. 3) then
            write(*,'(a)') 'Sorry you must enter at least 3 particles.'
            write(*,'(a,i2,a)') 'Only',iline(0),' particles found.'
            write(*,'(a)') 'Please try again. '
            goto 1
            more = .true.
            return
         elseif(iline(0) .gt. maxlines-1) then
            write(*,'(2a,i2,a)')
     &           'Sorry this version of MadGraph configured',
     &           ' to a maximum of ',maxlines-1, ' external particles.'
            write(*,'(i2,a)') iline(0),' particles found.'
            write(*,'(a)') 'Please try again. '
            goto 1
            more = .false.
            return
         endif

c         write(*,*)
c         write(*,*) 'Flipping now'
c         write(*,*)
         do i=1,iline(0)
            if (i .le. nincoming) then
                iline(i)=inverse(iline(i))
c               if (info_p(2,iline(i)).eq.-2) iline(i)=inverse(iline(i))
            endif
         enddo
!
! Determine Order in QCD, QED, QFD and Ghosts  
!
         call get_order
      else
c         call inc_jet(iline(0),jetloop)  !Already called
         j = 21
         attempt = 'Attempting Process: '
         do i=1,iline(0)
            if (i .le. nincoming) iline(i)=inverse(iline(i))
            call part_string(iline(i),attempt(j:j+3),nchar)
            if (i .le. nincoming) iline(i)=inverse(iline(i))
            j = j+nchar+1
            if (i .eq. nincoming) then
               attempt(j:j+2) = '-> '
               j = j+3
            endif
         enddo
         write (*,'(1x/a)') attempt(1:j)
         iproc = j-18
         proc = attempt(19:j)
      endif
!
! make name for process
!
      iname = 0
      do i=1,iline(0)
         if (iname .gt. 24) then
            write(*,*)'name too large, truncating: ',name
            iname=24
c            stop
         endif
         if (i .eq. nincoming+1 .and. i .gt. 1) then
            if (iname .lt. 24) then
               iname=iname+1
               name(iname:iname)='_'
            endif
         endif
         if (i .le. nincoming) then
            call part_string(inverse(iline(i)),str1,length)
         else
            call part_string(iline(i),str1,length)
         endif
         if (iname+length .gt. 24) then
            write(*,*) 'Truncating name.'
            length=24-iname
         endif
         if (length .gt. 0) name(iname+1:iname+length) = str1(1:length)
         iname = iname+length
      enddo
      do i=1,iname
         if(name(i:i) .eq. '~') name(i:i) = 'x'
c         if(name(i:i) .eq. '+') name(i:i) = 'p'
c         if(name(i:i) .eq. '-') name(i:i) = 'm'
      enddo
c
c     We'll store this for setting up the directory
c
      if (lwrite) then     !Haven't saved a process yet
         open(unit=itnum ,file='dname.mg',status='unknown')
         if (onium) then
           write(dirstr,*) 'DIRNAME=P',nameprefix,'_',name(1:iname),
     & process_temp(2:5) 
         else
           write(dirstr,*) 'DIRNAME=P',nameprefix,'_',name(1:iname)
         endif
           call trimspace(dirstr,max_string)
           write(itnum,*) dirstr(1:len_trim(dirstr))
         close(itnum)
      endif
c
c     Now reset the default name to be matrix
c
      iname=6
      name='matrix'
!
! Query name for process
!
      if (.not. jetloop) then
         write(*,'(a,a,a)') 'Enter a name to identify process (',
     &        name(1:iname),'): '
cfax
c         read(*,'(a)') process
         process='matrix' 	          
         i=1
         do while (i .lt. 75 .and. (ichar(process(i:i)) .lt. 65 .or.
     &        ichar(process(i:i)) .gt. 122 .or.
     &        (ichar(process(i:i)) .gt. 90 .and.
     &        ichar(process(i:i)) .lt. 97)))
            i=i+1
         enddo
         imin = i
         do while (i .lt. 75 .and. process(i:i) .ne.' ')
            i=i+1
         enddo
         if (imin .ne. i) then
            if (i-imin .gt. 14) then
               write(*,*)'Truncating name'
               i=imin+14
            endif
            iname=i-imin
            name(1:iname)=process(imin:i)
         endif
      else
         iname=6
         name='matrix'
         do i=1,iline(0)
            if (i .le. nincoming) then
c                iline(i)=inverse(iline(i))
c               if (info_p(2,iline(i)).eq.-2) iline(i)=inverse(iline(i))
            endif
         enddo
      endif

c      write(*,'(a,10i5)') 'Particle codes',(iline(j),j=1,iline(0))
c      if (.not. lwrite .and. .not. jetloop) then
c         logic = qmatch_check(iline(0))         
c      endif

     

      if (jetloop .or. gopt .eq. 1) then 
         gopt=1
      else
         gopt=2
      endif

      end



      subroutine write_cross(lun)
c*****************************************************************************
c     Write out subprocess and all non identical crossings into
c     file for later input by MadGraph
c
c     Careful this code doesn't work for eP->jjj yet. Only pp->jjj
c
c*****************************************************************************
      implicit none
!Constants
      include 'params.inc'
      integer    maxpcross
      parameter (maxpcross = 100)
! Arguments

      integer lun

! Local
      integer i,j,k,ncross
      integer imatch(2,maxpcross), jline(maxlines)
      integer icount(2), inc_jet
      logical good, moved, filled(2)

! Global
      integer         nincoming,nincfirst
      common/to_proc2/nincoming,nincfirst

      integer        iline(-maxlines:maxlines),idir(-maxlines:maxlines)
      integer        this_coup(max_coup) ,goal_coup(max_coup)
      common/to_proc/iline,idir,this_coup,goal_coup

      integer         icode(npartons),njet,ijet(10),kcount(10)
      common /to_jets/icode,          njet,ijet,    kcount

!-----------
! Begin Code
!-----------
      inc_jet=0
      do i=1,njet
         if (ijet(i) .le. nincoming) inc_jet=inc_jet+1
      enddo
      if (inc_jet .gt. 2) then
         write(*,*) 'Crossing only works for up to 2 incoming partons'
      endif
      if (inc_jet .lt. 1) return
      write(*,*) 'Looking for crossings',nincoming,inc_jet,njet
c
c     Store original parton assignments in jline
c
      do j=1,iline(0)
         jline(j)=iline(j)
      enddo
c
c     Assume that the first ordering has been calculated or
c     already written
c
      ncross = 1
      do i=1,inc_jet
         imatch(i,ncross)= jline(ijet(i))
         icount(i) = 1
      enddo

c*******************************************************************
c     Now start looping through all combinations
c*******************************************************************
      do while (icount(1) .le. njet)
c
c     Usually falls right through this and increments i=nincoming
c
         i = inc_jet
         do while (icount(i) .eq. njet .and. i .gt. 1)
            i=i-1
         enddo
c
c     Increment the appropriate incoming parton
c
         icount(i)=icount(i)+1
c
c     Reset all "lower" partons
c
         do j=i+1,inc_jet
            icount(j)=1
         enddo
c
c     Check that this is an OK choice
c
         good = .true.
         if (icount(1) .gt. njet) good=.false.
         if (inc_jet .ge. 2) then
            if (icount(1) .eq. icount(2)) good=.false.
         endif
         i=0
c         write(*,*) icount(1),icount(2),good
         do while (good .and. i .lt. ncross)
            i = i+1
            good = .false.
            do j=1,inc_jet
               if (jline(ijet(icount(j))).ne.imatch(j,i)) good=.true.
            enddo
         enddo
         if (good) then
c            write(*,*) 'Cross',ncross+1,(ijet(icount(j)),j=1,inc_jet)
c
c     Fill in the initial state
c
            do j=1,inc_jet
               filled(j)=.false.
            enddo
            do j=1,inc_jet
               i = ijet(icount(j))
               iline(ijet(j))=jline(i)
               if (icount(j) .le. inc_jet) then
                  filled(icount(j))=.true.
               endif
            enddo
c
c     Fill up the final state
c
            do j=inc_jet+1,njet
               moved = .false.
               do i=1,inc_jet
                  if (j .eq. icount(i)) moved=.true.
               enddo
               if (moved) then
                  k =1 
                  do while ( filled(k) .and. (k .lt. inc_jet))
                     k=k+1
                  enddo
                  iline(ijet(j)) = jline(ijet(k))
                  filled(k)=.true.
c                  write(*,*) 'Placing',ijet(k),' in ',ijet(j)
               else
                  iline(ijet(j)) = jline(ijet(j))
               endif
            enddo
c            call write_proc(lun)
c            call write_proc(6)
c            write(*,*) '******************  Ordered ************'
            call order_jets(iline(0))
            call write_proc(lun)
            if (ncross .ge. maxpcross) then
               write(*,*) 'Too many crossings in readproc.f',maxpcross
               write(*,*) 'May get duplicate crossings'
            else
               ncross = ncross+1
               do j=1,inc_jet
                  imatch(j,ncross)= iline(ijet(j))
               enddo
            endif
c
c           restore original parton assignments to iline
c
            do j=1,iline(0)
               iline(j)=jline(j)
            enddo
         endif
      enddo

      end

      subroutine write_proc(lun)
c*****************************************************************************
c     Write out subprocess into file which can later be read as input
c*****************************************************************************
      implicit none
!Constants
      include 'params.inc'

! Arguments

      integer lun

! Local
      integer i,j,k, nchar
      character*(max_string) attempt
      character*(max_string) info_proj
      character*1 lstring
      integer idum,length

! External
      logical DupProcess
      external dupprocess

! Global
      integer         nincoming,nincfirst
      common/to_proc2/nincoming,nincfirst

      integer       req_part(0:max_particles),exc_part(0:max_particles)
      integer       nores_part(0:max_particles)
      common/to_filter/req_part,exc_part,nores_part

      integer        iline(-maxlines:maxlines),idir(-maxlines:maxlines)
      integer        this_coup(max_coup) ,goal_coup(max_coup)
      common/to_proc/iline,idir,this_coup,goal_coup

      character*(10)            coup_name(max_coup)
      integer             ncoups
      common/to_couplings/ncoups,coup_name

      character*(4*max_particles) particle(4)
      integer                               charge_c(max_particles)
      integer iparticle(0:max_particles,0:4),inverse(max_particles)
      common/to_model/iparticle,  particle,  inverse, charge_c

c      character*1         nameprefix
      integer             nameprefix
      common/to_multiname/nameprefix

      integer ndcmax
      parameter(ndcmax=20)
      integer ndcpart,idcid(ndcmax),idcgroup(ndcmax)
      logical dcfinal(ndcmax)
      common/to_dc/ndcpart,idcid,idcgroup,dcfinal

      include 'onia.inc'

! Data

!-----------
! Begin Code
!-----------
c      if  (DupProcess(idum)) return   !Check if already did this process
      j=1
      attempt=""
      do i=1,iline(0)
         if (i .le. nincoming) then
            call part_string(inverse(iline(i)),attempt(j:j+3),nchar)
         else if(ndcpart.gt.0)then
c
c        Print out decay chain process using write_dc
c        Here one should of course replace idcid with the actual ids
c
c            call part_string(iline(i),attempt(j:j+3),nchar)
            call write_dc(attempt,j,idcid,ndcpart)
            j=j+1
            goto 5
         else
            call part_string(iline(i),attempt(j:j+3),nchar)
         endif
         j = j+nchar+1
         if (i .eq. nincoming) then
            attempt(j:j+2) = '-> '
            j = j+3
            if (req_part(0) .gt. 0) then
               do k=1,req_part(0)
                  call part_string(req_part(k),attempt(j:j+3),nchar)
                  j=j+nchar+1
               enddo
               attempt(j:j+2) = '-> '
               j = j+3
            endif
         endif
      enddo
 5    continue
      if(nores_part(0) .gt. 0)then
         attempt(j:j+2)='$'
         j=j+3
         do k=1,nores_part(0)
            call part_string(nores_part(k),attempt(j:j+3),nchar)
            j=j+nchar+1
         enddo
      endif
      if (exc_part(0) .gt. 0) then
         attempt(j:j+2) = ' / '
         j = j+3
         do k=1,exc_part(0)
            call part_string(exc_part(k),attempt(j:j+3),nchar)
c
c          Note we have included inverse and cc, if they don't have
c           characters representations, don't include them in output
c
            if (attempt(j:j) .ne. "?") then
               j=j+nchar+1
            endif
         enddo
      endif
cfax 20.08.2007
c
c     add the information on the onia stuff if there
c
      if(C_qn.gt.0) then
         if(L_qn.eq.0) lstring='S'
         if(L_qn.eq.1) lstring='P'
      write (info_proj,'(a1,i1,a1,i1,i1,a2,a8,a1)')
     &  '[',2*S_qn+1,lstring,J_qn,C_qn,'to',onium_ID,']'
       call no_spaces(info_proj,length)
         attempt(j:j+length-1)=info_proj(1:length)
         j=j+length
c         if(em_decay) then
c           attempt(j:j+4)='>l+l-'
c          j=j+5
c         endif
       endif
c
c     add prefix for directory name
c
      attempt(j:j+1) =" @"
      j=j+2
      write(attempt(j:),*) nameprefix,'_'
      j=len_trim(attempt)
      write (lun,'(a)') attempt(1:j)
      write (*,'(a)') 'Wrote process: ',attempt(1:j)
c      write(*,*) 'Number excluded ', attempt(1:j-1)
c
c     Now write out the couplings
c      
      do i=1,ncoups
         write(lun,'(a,a,i4)') coup_name(i),'=',goal_coup(i)
      enddo
cfax 12.05.2006
      write (lun,'(a)') 'end_coup'
c      write(lun,*)
      end

      logical function qmatch_check(iline)
c***********************************************************************
c     Does a quick check to see if this process could match the one
c     in memory, based on boson vs fermion vs anti_fermion
c***********************************************************************
      implicit none
!Constants
      include 'params.inc'

! Arguments

      integer iline(0:10)

! Local
      integer i,j,k

! Global
      integer          matchline(0:maxlines)
      common/to_qmatch/matchline

      character*(max_string) iwave(max_particles),owave(max_particles)
      character*(8) str2(3,max_particles)
      integer info_p(5,max_particles),iposx(3,max_particles)
      common/to_external/iwave,owave,iposx,info_p,str2

      character*(4*max_particles) particle(4)
      integer                               charge_c(max_particles)
      integer iparticle(0:max_particles,0:4),inverse(max_particles)
      common/to_model/iparticle,  particle,  inverse, charge_c

!-----------
! Begin Code
!-----------
      i = 0
      qmatch_check=(iline(i) .eq. matchline(i)) !Same # of particles
      do while (qmatch_check .and. i .lt. iline(0))
         i=i+1
         if (info_p(3,iline(i)) .ne. info_p(3,matchline(i))) then 
            qmatch_check=.false.
         elseif (info_p(3,iline(i)) .eq. 1) then    !This is fermion
            j =inverse(iline(i))-iline(i)           !make sure both are
            k =inverse(matchline(i))-matchline(i)   !fermion or anti
            if (k * j * (k-j) .ne. 0) qmatch_check=.false.
         endif
c
c     New check tjs 2-12-08 check same color structure
c
         if (info_p(1,iline(i)) .ne. info_p(1,matchline(i))) then
            qmatch_check=.false.
         endif
c
c       New check by tjs 12-14-06 for case of external scalars
c
c         print *,'Checking string ',str2(2,iline(i)),' against ',
c     $        str2(2,matchline(i))
        if (str2(2,iline(i)) .ne. str2(2,matchline(i))) then
           qmatch_check=.false.                !Require same mass
c           print *,'False!'
c        else
c           print *,'True!'
        endif
      enddo

c      if (qmatch_check) then
c         write(*,*) 'Matched'
c      else
c         write(*,*) 'No Match'
c      endif
      end


      Subroutine order_jets(iline)
c***********************************************************************
c     Permutes the final state jets so they are in a specific order
c     This should maximize chances for finding "identical" subprocess
c     Orders particles according to association with the initial state,
c     and then according to # in final state.
c
c     Assumes that icount has all of the jets, and the ordering of 
c     partons placed into a jet is
c      call getparticle('d d~ u u~ s s~ c c~ b  b~ g',icode,i)
c***********************************************************************
      implicit none
!Constants
      include 'params.inc'

! Arguments

      integer iline(0:maxlines)

! Local
      integer i,j,nl
      integer ip, inc_jet, iqmax, imax
      integer nqcount(npartons)
      integer iqplace(npartons)
      integer jline(0:maxlines)
      integer icount(10)


! Global
      integer         icode(npartons),njet,ijet(10),jcount(10)
      common /to_jets/icode,          njet,ijet,    jcount

      integer         nincoming,nincfirst
      common/to_proc2/nincoming,nincfirst




! Data

!-----------
! Begin Code
!-----------
c
c     Need to set up icount according to this crossing
c
      do i=1,njet
         ip = iline(ijet(i))
         j = 1
         do while (j .le. npartons .and. icode(j) .ne. ip)
            j=j+1
         enddo
         if (j .gt. npartons) then
            write(*,*) 'Error no match',i,ijet(i),iline(ijet(i))
            j = npartons
         endif
         icount(i)=j
      enddo
c
c     Start by determining the number of each parton type.
c
c      write(*,'(a,8i4)') 'icount',(icount(i),i=1,njet)
c      write(*,'(a,8i4)') 'icode',(icode(icount(i)),i=1,njet)
      do i=1,npartons
         nqcount(i)=0
         iqplace(i)=0
      enddo
      do i=1,njet
         nqcount(icount(i))=nqcount(icount(i))+1
      enddo
c
c     First order according to incoming partons
c
      ip = 1
      i=1
      inc_jet = 0
      do while (i .le. njet .and. ijet(i) .le. nincoming)
         inc_jet=i
c         write(*,*) 'Checking initial ',i,icount(i),iqplace(icount(i))
         if (iqplace(icount(i)) .eq. 0) then
            if (icount(i) .eq. npartons) then !Gluon
c               iqplace(icount(i))=ip         !Gluons always at end
c               ip=ip+1
            else                              !Quarks
c               write(*,*) 'Writing iqplace initial',icount(i),ip

               if (mod(icount(i),2) .eq. 1)  then     !Quark
c                  write(*,*) 'Filling',icount(i),ip
                  iqplace(icount(i)) = ip
                  ip = ip + 1
c                  write(*,*) 'Filling',icount(i)+1,ip
                  iqplace(icount(i)+1)= ip
                  ip = ip + 1
               else                                    !AntiQuark
c                 Below sets antiquark first
c                  iqplace(icount(i)) = ip
c                  ip = ip + 1
c                  iqplace(icount(i)-1)= ip
c                  ip = ip + 1
c     Below is for quark always first
c                  write(*,*) 'Filling',icount(i)-1,ip
                  iqplace(icount(i)-1) = ip
                  ip = ip + 1
c                  write(*,*) 'Filling',icount(i),ip
                  iqplace(icount(i))= ip
                  ip = ip + 1
               endif
            endif
         endif
         i=i+1
      enddo
c      write(*,'(a,11i5)') 'iqplace',(iqplace(i),i=1,npartons)

c
c     Next order by number of qq pairs alway q first, then qbar
c
      iqmax = 1
      do while (iqmax .gt. 0)
         iqmax=0
         imax = 0
         do i=1,(npartons-1)/2  !Just sum over quarks, gluons at end
            if (iqplace((2*i)) .eq. 0) then !Hasn't been set yet
               if (nqcount(2*i -1)+nqcount(2*i) .gt. iqmax) then !New maximum
                  iqmax = nqcount(2*i -1)+nqcount(2*i)
                  imax = i
               endif
            endif
         enddo
         if (iqmax .gt. 0) then
c            write(*,*) 'Writing iqplace final',i,iqmax,2*imax
            iqplace((2*imax - 1)) = ip
            iqplace((2*imax)) = ip+1
            ip = ip+2
         else
            if (iqplace(npartons) .eq. 0) then
c               write(*,*) 'Writing iqplace end',ip,npartons
               iqplace(npartons) = ip
            endif
         endif
      enddo
c
c     Now set the final state jets in the appropriate order
c     This is a bit inefficient, but should be OK
c
c      write(*,*) 'Ordering',iline(0), inc_jet, njet,ip
c      write(*,'(a,11i5)') 'iqplace',(iqplace(i),i=1,npartons)
c      write(*,'(a,11i5)') 'icount',(icount(i),i=1,njet)
      nl = inc_jet+1
      do i=1,ip
         do j=inc_jet+1,njet
c            write(*,*) 'Checking',i,j,icount(j),iqplace(icount(j))
            if (iqplace(icount(j)) .eq. i) then
               jline(ijet(nl))= icode(icount(j))
c               write(*,*)'Ordered',j,ijet(nl),icount(j),icode(icount(j))
               nl = nl+1
            endif
         enddo
      enddo
      do i=inc_jet+1,njet
         iline(ijet(i))=jline(ijet(i))
      enddo
      end


      Subroutine set_jet_old(iline,jetloop)
c***********************************************************************
c     Looks to see if any of the partons are labeled 'jet' or 'proton'
c     in which case it will be necessary to loop over all the different
c     partons.
c***********************************************************************
      implicit none
!Constants
      include 'params.inc'

! Arguments

      integer iline(0:10)
      logical jetloop

! Local
      integer i,pcode,jcode
      logical first_time

! Global
      integer         icode(npartons),njet,ijet(10),icount(10)
      common /to_jets/icode,          njet,ijet,    icount
      logical         lwp,lwm,lz,decay,cross
      common/to_decay/lwp,lwm,lz,decay,cross


      character*(4*max_particles) particle(4)
      integer                               charge_c(max_particles)
      integer iparticle(0:max_particles,0:4),inverse(max_particles)
      common/to_model/iparticle,  particle,  inverse, charge_c

      integer         nincoming,nincfirst
      common/to_proc2/nincoming,nincfirst

! Data
      Data first_time/.true./
      save pcode,jcode

!-----------
! Begin Code
!-----------
      njet = 0
      if (first_time) then
         first_time=.false.
         call getparticle('P ',pcode,i)
         if (i .ne. 1) then
            write(*,*)'Warning no particle code for P, proton',i
            pcode=-999
         endif
         call getparticle('J ',jcode,i)
         if (i .ne. 1) then
            write(*,*)'Warning no particle code for J, jet',i
            jcode=-999
         endif
      endif
      call getparticle('d d~ u u~ s s~ c c~ g',icode,i)
c      call getparticle('d u s c b d~ u~ s~ c~ b~ g',icode,i)
c      call getparticle('dsucbd~s~u~c~b~g',icode,i)
      if (i .ne. npartons) then
         write(*,*)'Warning not enough jet particle codes',i
      endif
      jetloop = .false.
      do i=1,iline(0)
         if (iline(i) .eq. pcode .or. iline(i) .eq. jcode .or.
     $        inverse(iline(i)) .eq. pcode) then
            jetloop      = .true.
            njet         = njet+1
            iline(i)     = icode(1)
            if (i .le. nincoming) iline(i)=inverse(iline(i))
            ijet(njet)   = i
            icount(njet) = 1
         endif
      enddo
      if (ijet(1) .eq. 1 .and. ijet(2) .eq. 2) then
         cross=.true.
         cross=.false.
      else
         cross=.false.
      endif
      end

      Subroutine set_jet(iline,jetloop)
c***********************************************************************
c     Looks to see if any of the partons are labeled 'jet' or 'proton'
c     in which case it will be necessary to loop over all the different
c     partons.
c***********************************************************************
      implicit none
!Constants
      include 'params.inc'

! Arguments

      integer iline(0:10)
      logical jetloop

! Local
      integer i,j
      character*(5) str1

! Global
      integer                 SumParticles(0:maxpartons,0:maxloops)
      integer                 icount(maxpartons,maxloops)
      integer                 njets(0:maxpartons)
      integer                 ijet(maxpartons,maxloops)
      common/to_SumParticles/ SumParticles,icount,ijet,njets

      integer         nincoming,nincfirst
      common/to_proc2/nincoming,nincfirst
      character*(4*max_particles) particle(4)
      integer                               charge_c(max_particles)
      integer iparticle(0:max_particles,0:4),inverse(max_particles)
      common/to_model/iparticle,  particle,  inverse, charge_c

      logical              cross_opt
      common /to_crossopt/ cross_opt


!-----------
! Begin Code
!-----------
      cross_opt = .false.

      do i=1,SumParticles(0,0)
         njets(i) = 0
      enddo
      jetloop = .false.
      do i=1,iline(0)
         do j=1,SumParticles(0,0)
            if (iline(i) .eq. SumParticles(0,j)) then
c               write(*,*) 'Found jet loop',i,j
c               read(*,*) str1
               jetloop      = .true.
               njets(j)     = njets(j)+1
               iline(i)     = SumParticles(2,j)
               if (i .le. nincoming) iline(i)=inverse(iline(i))
               ijet(njets(j),j)   = i
               icount(njets(j),j) = 1
            endif
         enddo
      enddo
      end

      Subroutine inc_jet(iline,jetloop)
c***********************************************************************
c     Loops over the different possible partons in protons and jets
c***********************************************************************
      implicit none
!Constants
      include 'params.inc'
c      integer    npartons
c      parameter (npartons=11)

! Arguments

      integer iline(0:10)
      logical jetloop

! Local
      logical possible, foundone
      integer i,j,kloop,iloop,nf(-1:1)
      integer iexternal 

! Global
      integer                 SumParticles(0:maxpartons,0:maxloops)
      integer                 icount(maxpartons,maxloops)
      integer                 njets(0:maxpartons)
      integer                 ijet(maxpartons,maxloops)
      common/to_SumParticles/ SumParticles,icount,ijet,njets


      character*(max_string) iwave(max_particles),owave(max_particles)
      character*(8) str2(3,max_particles)
      integer info_p(5,max_particles),iposx(3,max_particles)
      common/to_external/iwave,owave,iposx,info_p,str2

      character*(4*max_particles) particle(4)
      integer                               charge_c(max_particles)
      integer iparticle(0:max_particles,0:4),inverse(max_particles)
      common/to_model/iparticle,  particle,  inverse, charge_c

      logical         major_exist
      common/to_major/major_exist


      integer         nincoming,nincfirst
      common/to_proc2/nincoming,nincfirst

      integer ndcmax
      parameter(ndcmax=20)
      integer ndcpart,idcid(ndcmax),idcgroup(ndcmax)
      logical dcfinal(ndcmax)
      common/to_dc/ndcpart,idcid,idcgroup,dcfinal


! Data

!-----------
! Begin Code
!-----------
c
c	Iline(0) = # partons   iline(1)=parton_1 iline(2)=parton2 .....
c	njet(loop) = # protons+jets  things looping over for that type (P,J,L+ etc)
c	icount(1,loop) tells what particle 1 is currently  (u, u~, d....etc)
c	ijet(1,loop) tells you which parton is jet #1. For PP-> w+jj   ijet = {(1,2),(4,5)}
c       
c
      possible = .false.
      do while (jetloop .and. .not. possible ) 

         jetloop=.false.
c
c	Loop to find first jet which isn't at maximum value (eg it can be incremented)
c	Usually falls right through this. i=njets.
c
         iloop = SumParticles(0,0)    !Start at the outer most loop
         foundone=.false.
         do while (.not. foundone .and. iloop .ge. 1)
            if (njets(iloop) .gt. 0) then
               i = njets(iloop)
               do while(icount(i,iloop) .eq. SumParticles(1,iloop)
     $              .and. i .gt. 1)     
                  i=i-1
               enddo
               foundone = (icount(i,iloop) .ne. SumParticles(1,iloop))
            endif
            if (.not. foundone) iloop=iloop-1
         enddo
         if (.not. foundone) then
            jetloop = .false.
            return
         endif
c         write(*,*) 'Incrementing',i,iloop
c         read(*,*) j
c
c	Now increment jet i by 1
c
         icount(i,iloop)=icount(i,iloop)+1
c
c	reset all "higher" jets to minimum new value.
c
         do j=i+1,njets(iloop)
            if (ijet(j,iloop) .le. nincoming) then
               icount(j,iloop) = 1
            else
               icount(j,iloop) = icount(i,iloop) !Avoid double counting, unless it is P
            endif
         enddo
         do kloop=iloop+1,SumParticles(0,0)
            do j=1,njets(kloop)
               icount(j,kloop)=1
            enddo
         enddo
c
c    Now check and make sure everything looks OK.If so, set the partons to the
c	appropriate value and set jetloop to be true.
c
         do kloop=1,SumParticles(0,0)
            do j=1,njets(kloop)
               jetloop=.true.
               if (icount(j,kloop) .le. SumParticles(1,kloop)) then
                  iline(ijet(j,kloop)) =
     $                 SumParticles(icount(j,kloop)+1,kloop)
               else
c                 write(*,*) 'Done ',j,icount(j),npartons
                  jetloop=.false.
                  return
               endif
            enddo
         enddo
c
c tjs 3/30/07
c set idcid values to new partons    
c
         iexternal = nincoming
         do i=1,ndcpart
            if (dcfinal(i)) then
               iexternal=iexternal+1
               idcid(i) = iline(iexternal)
            endif
         enddo
      
         if (.true.) then       !Use optimization
c
c     Do a simple check to make sure the number of fermions is even
c
            do i=-1,1
               nf(i)=0
            enddo
            do i=1,iline(0)
               if (info_p(3,iline(i)) .eq. 1) then !This is a fermion line
                  j = inverse(iline(i))-iline(i)
                  if (abs(j) .gt. 1) then
                     write(*,*) 'Error determining fermion in inc_jet',j
                     j=0
                  endif
                  nf(j) = nf(j)+1
               endif
            enddo
c
c     Warning, this doesn't work with majoranas
c
            if (nf(0) .eq. 0 .and. .not. major_exist) then
               possible = (nf(-1) .eq. nf(1)) !match fermion with anti
            else
               write(*,*) 'Using even number',nf(0), major_exist
               possible = ( mod(nf(-1)+nf(0)+nf(1),2) .eq. 0) !requires even number
c              possible = .true.
            endif
         else
            possible=.true.
         endif
      enddo                     !(possible)

      call order_jets(iline(0))

      end

      Subroutine inc_jet_old(iline,jetloop)
c***********************************************************************
c     Loops over the different possible partons in protons and jets
c***********************************************************************
      implicit none
!Constants
      include 'params.inc'
c      integer    npartons
c      parameter (npartons=11)

! Arguments

      integer iline(0:10)
      logical jetloop

! Local
      integer i,j,nf(-1:1)
      logical possible

! Global
      integer         icode(npartons),njet,ijet(10),icount(10)
      common /to_jets/icode,          njet,ijet,    icount

      logical              cross_opt
      common /to_crossopt/ cross_opt


      character*(max_string) iwave(max_particles),owave(max_particles)
      character*(8) str2(3,max_particles)
      integer info_p(5,max_particles),iposx(3,max_particles)
      common/to_external/iwave,owave,iposx,info_p,str2

      integer         nincoming,nincfirst
      common/to_proc2/nincoming,nincfirst
      character*(4*max_particles) particle(4)
      integer                               charge_c(max_particles)
      integer iparticle(0:max_particles,0:4),inverse(max_particles)
      common/to_model/iparticle,  particle,  inverse, charge_c
      
      
      logical         major_exist
      common/to_major/major_exist


! Data

!-----------
! Begin Code
!-----------
c
c	Iline(0) = # partons   iline(1)=parton_1 iline(2)=parton2 .....
c	njet = # protons+jets  things looping over
c	icount(1) tells what particle 1 is currently  (u, u~, d....etc)
c	ijet(1) tells you which parton is jet #1. For PP-> w+jj   ijet = { 1,2,4,5}
c       
c
      possible = .false.
      do while (jetloop .and. .not. possible ) 

         jetloop=.false.
         i = njet
c
c	Loop to find first jet which isn't at maximum value (eg it can be incremented)
c	Usually falls right through this. i=njet.
c
      do while(icount(i) .eq. npartons .and. i .gt. 1)     
         i=i-1
      enddo
c
c	Now increment jet i by 1
c
      icount(i)=icount(i)+1
c
c	reset all "higher" jets to minimum new value.
c
      do j=i+1,njet
         if (ijet(i) .le. nincoming .and. .not. cross_opt) then
            icount(j)=1              !Incremented initial state, start over
         else
            icount(j)=icount(i)      !Increment final state, start at this point
         endif
      enddo
c
c    Now check and make sure everything looks OK.If so, set the partons to the
c	appropriate value and set jetloop to be true.
c
      do j=1,njet
         jetloop=.true.
         if (icount(j) .le. npartons) then
c            jetloop=.true.
            iline(ijet(j)) = icode(icount(j))
         else
c            write(*,*) 'Done ',j,icount(j),npartons
            jetloop=.false.
            return
         endif
      enddo
      
      if (.true.) then            !Use optimization
c
c     Do a simple check to make sure the number of fermions is even
c
      do i=-1,1
         nf(i)=0
      enddo
      do i=1,iline(0)
         if (info_p(3,iline(i)) .eq. 1) then    !This is a fermion line
            j = inverse(iline(i))-iline(i)
            if (abs(j) .gt. 1) then
               write(*,*) 'Error determining fermion in inc_jet',j
               j=0
            endif
            nf(j) = nf(j)+1
         endif
      enddo
c
c     Warning, this doesn't work with majoranas
c
      if (nf(0) .eq. 0 .and. .not. major_exist) then
         possible = (nf(-1) .eq. nf(1))         !match fermion with anti
      else
         write(*,*) 'Using even number',nf(0), major_exist
         possible = ( mod(nf(-1)+nf(0)+nf(1),2) .eq. 0) !requires even number
c      possible = .true.
      endif
      else
         possible=.true.
      endif
      enddo  !(possible)

      call order_jets(iline(0))


      end



      Subroutine get_order_old2
c***********************************************************************
c     Determines the appropriate order of the different coupling
c     constants based on information about the external particles
c     and also based on user input.
c***********************************************************************
      implicit none
! Constants

      include 'params.inc'

! Local
      integer i

! Global Variables

      integer        iline(-maxlines:maxlines),idir(-maxlines:maxlines)
      integer        this_coup(max_coup) ,goal_coup(max_coup)
      common/to_proc/iline,idir,this_coup,goal_coup

      character*(10)            coup_name(max_coup)
      integer             ncoups
      common/to_couplings/ncoups,coup_name
!-----------
! Begin Code
!-----------
      do i=1,ncoups
         write(*,'(a,a$)') 'Enter maximum number of vertices for ',
     &        coup_name(i)
         read(*,*) goal_coup(i)
         if (goal_coup(i) .lt. 0) goal_coup(i)=0
      enddo
      end

      Subroutine get_order
c***********************************************************************
c     Determines the appropriate order of the different coupling
c     constants based on information about the external particles
c     and also based on user input.
c***********************************************************************
      implicit none
! Constants

      include 'params.inc'

! Local
      integer i,j
      logical done
      character*50 buff
      character*10 coupname
      integer coupvalue

! Global Variables

      integer        iline(-maxlines:maxlines),idir(-maxlines:maxlines)
      integer        this_coup(max_coup) ,goal_coup(max_coup)
      common/to_proc/iline,idir,this_coup,goal_coup

      character*(10)            coup_name(max_coup)
      integer             ncoups
      common/to_couplings/ncoups,coup_name
!-----------
! Begin Code
!-----------
      done = .false.
      do i=1,ncoups
         goal_coup(i) = 0
      enddo
      do while (.not. done)
         write(*,'(a,a)') 'Enter maximum number of vertices for'
     $           ,' eg QCD = 4 (type "end_coup" to finish)'         
         read(*,'(a)') ,buff

         call upper_case(buff)
 11      if (index(buff,"END_COUP") .gt. 0) then 
            done = .true.
            return
         endif
         call get_coup_name(buff,coupname,coupvalue)
         if (coupname(1:1) .ne. " ") then
            i = 1
            do while (coup_name(i) .ne. coupname .and. i .lt. ncoups)
               i=i+1
            enddo
            if (coup_name(i) .eq. coupname) then
               goal_coup(i) = coupvalue
               j = index(coupname," ")
               if ( j .eq. 0) then
                  j = 10
               endif
               write(*,*) 'Setting ',coupname(1:j),' = ', coupvalue 
            endif
         endif
       enddo
      end

      subroutine get_coup_name(buff,coupname,coupvalue)
c**********************************************************************
c     Extracts from buff, the coupling name and coupling value
c     Assumes it is in the format coup = value
c**********************************************************************
      implicit none
c
c     Arguments
c
      character*(*) buff
      character*(*) coupname
      integer coupvalue
c
c     Local
c
      integer i,j,k
c-----
c  Begin Code
c-----
      i = index(buff,"=")
      coupname = " "
      coupvalue = 0
c      write(*,*) "Got coupling ",buff(:i)
      if (i > 0) then
         j=1
         do while (j .lt. i .and. buff(j:j) .eq.  ' ')
            j=j+1
         enddo
         k=j
         do while (k .lt. i-1 .and. buff(k:k) .ne. ' ')
            k=k+1
         enddo
         coupname = buff(j:k)
         read(buff(i+1:),*,err=99,end=99) coupvalue         
      endif
c      write(*,*) coupname, j,k,i
 99   continue
      end

      Subroutine get_order_old
c***********************************************************************
c     Determines the appropriate order of the different coupling
c     constants based on information about the external particles
c     and also based on user input.
c     Input is from common block iline and
c***********************************************************************
      implicit none
! Constants

      include 'params.inc'
c      integer    maxlines  , max_coup
c      parameter (maxlines=8, max_coup=5)
c      integer    max_string    , max_particles
c      parameter (max_string=120, max_particles=2**7-1)

! Local

      integer i,nqfd,in1,wpart(0:3)
      integer reqorder(max_coup),imin,imax
      character*25 input
      character*10 snum
      logical done

! Global Variables

      integer        iline(-maxlines:maxlines),idir(-maxlines:maxlines)
      integer        this_coup(max_coup) ,goal_coup(max_coup)
      common/to_proc/iline,idir,this_coup,goal_coup

      character*(max_string) iwave(max_particles),owave(max_particles)
      character*(8) str2(3,max_particles)
      integer info_p(5,max_particles),iposx(3,max_particles)
      common/to_external/iwave,owave,iposx,info_p,str2

      logical         lwp,lwm,lz,decay,cross
      common/to_decay/lwp,lwm,lz,decay,cross


      data snum /'0123456789'/
      data wpart/0,0,0,0/
!-----------
! Begin Code
!-----------
      do i=1,max_coup
         reqorder(i)=0
      enddo
      do i=1,iline(0)
c         if (info_p(5,iline(i)) .ne. 0) then   !info_p(5,i) now used for particle ID
c            if (info_p(5,iline(i)) .le. max_coup) then
c               reqorder(info_p(5,iline(i)))=
c     &              reqorder(info_p(5,iline(i)))+1
c            else
c               write(*,*)'Order of particle too big in readproc'
c            endif
c         endif
      enddo
      imin = 0
      reqorder(2)=reqorder(2)+reqorder(3)+reqorder(4)
      do i=1,2
         if (reqorder(i) .gt. iline(0)-2) reqorder(i)=iline(0)-2
         imin=imin+reqorder(i)
      enddo

      if (imin .gt. iline(0)-2) then
         write(*,*)'Sorry this process is not possible at tree level'
         write(*,*)'Well try our best'
         reqorder(2)=iline(0)-2-reqorder(1)
c         stop
      endif

      imin=reqorder(1)
      imax=iline(0)-2-reqorder(2)-reqorder(5)
      write(*,'(a,i1,a,i1,a,i1,a)')
     &     'Enter the number of QCD vertices between ',
     &     imin,' and ',imax,' (',imax,'): '
      read(*,'(a)') input
      i=0
      done=.false.
      do while(i .lt. 25 .and. .not. done)
         i=i+1
         in1 = index(snum(imin+1:imax+1),input(i:i))
         if (in1 .ne. 0) then
            goal_coup(1) = imin+in1-1
            done=.true.
         else
            goal_coup(1) = imax
         endif
      enddo
      goal_coup(2) = iline(0)-2-goal_coup(1)-reqorder(4)
      goal_coup(3) = reqorder(3)
      goal_coup(4) = reqorder(4)


      imin = reqorder(2)
      imax = 9
      write(*,'(a,i1,a,i1,a,i1,a)')
     &     'Enter the number of QFD vertices between ',
     &     imin,' and ',imax,' (',imin,'): '
      read(*,'(a)') input
      i=0
      done=.false.
      do while(i .lt. 25 .and. .not. done)
         i=i+1
         in1 = index(snum(imin+1:imax+1),input(i:i))
         if (in1 .ne. 0) then
            goal_coup(2) = imin+in1-1
            done=.true.
         else
            goal_coup(2) = imin
         endif
      enddo

      imin = reqorder(4)
      imax = 9
      write(*,'(a,i1,a,i1,a,i1,a)')
     &     'Enter the number of BRS vertices between ',
     &     imin,' and ',imax,' (',imin,'): '
      read(*,'(a)') input
      i=0
      done=.false.
      do while(i .lt. 25 .and. .not. done)
         i=i+1
         in1 = index(snum(imin+1:imax+1),input(i:i))
         if (in1 .ne. 0) then
            goal_coup(4) = imin+in1-1
            done=.true.
         else
            goal_coup(4) = imin
         endif
      enddo


      write(*,'(a,i1)') 'The number of QFD vertices is ',goal_coup(2)
      if (goal_coup(3) .eq. 0 .and. goal_coup(2) .gt. 0) then
         write(*,'(a)') 'Would you like to include the Weak sector (n)?'
         read(*,'(a)') input
         nqfd=index(input,'y')
         if (nqfd .eq. 0) nqfd = index(input,'Y')
         if (nqfd .ne. 0) nqfd = 1
      elseif (goal_coup(3) .gt. 0) then
         write(*,'(a)') 'QFD required for this process ok?: '
         read(*,'(a)') input
      else
         write(*,'(a)') 'No QFD possible all QCD ok?: '
         read(*,'(a)') input
      endif
      if (nqfd .eq. 1) then
         goal_coup(3)=goal_coup(3)+goal_coup(2)
      endif
c      goal_coup(2)=9

      write(*,'(a,10i4)') 'Max Couplings',(goal_coup(i),i=1,max_coup)

c
c     Check if need to decay final particle
c
      if (wpart(0) .eq. 0) then
         call getparticle('w+w-z',wpart(1),wpart(0))
         if (wpart(0) .ne. 3) then
            write(*,*)'Warning couldnt find WZ boson codes'
         endif
      endif
      decay = .false.
      do i=1,wpart(0)
         if (iline(iline(0)) .eq. wpart(i)) decay=.true.
      enddo
      if (decay) then
         write(*,'(a)') 'Would you like to decay the final boson (n)?'
         read(*,'(a)') input
         i=index(input,'y')
         if (i .eq. 0) i = index(input,'Y')
         if (i .eq. 0) decay=.false.
      else
         write(*,'(a)') 'Last particle can not be decayed. ok?:'
         read(*,'(a)') input
      endif
c      call loadmodel(goal_coup(1),goal_coup(2),goal_coup(3))
c      if (goal_coup(1).gt. 0) call addQCD
c      if (goal_coup(2) +goal_coup(3)+goal_coup(4).gt. 0) call addQED
c      if (nqfd .gt. 0 .or. goal_coup(3)+goal_coup(4).gt. 0) call addQFD

      end



      subroutine read_dc(xbuff,iline,nparticles)
c***************************************************************************
c   Parses buff to find topology groups for decay chain
c
c   Uses subgroup ids so that a first mother, and its final state daughters
c   has id 10000000, while a daughter who is in its turn a mother gets
c   11000000 and so on down the chain. By sorting on group ids we 
c   automatically get the order 
c   (mother,final-state daughters,decaying daughters)
c   Additional particles are in group 0 and are last in the sorted chain.
c   The topological groups are then set such that the first final-state 
c   daughter in the list above defines a new group, and all daughters on 
c   same or lower level are put in this group. Example 
c   (skipping 5 trailing 0s in group ids):
c   
c            g g > (t~ > (W- > e-  ve~) b~ )(t  > b  (W+ > e+  ve ))
c
c subgroup:         100   110  110 110  100  200  200 210  210 210
c final:            F     F    T   T    T    F    T   F    T   T
c part nr:   1 2               4   5    3         6        7   8
c top.group:                   1   1    1         2        2   2
c
c   The sorted list of particle ids, subgroup numbers and "final" flag are
c   passed through the common block to_dc (where the incoming particles
c   are not included in ndcpart or the arrays.
c
c***************************************************************************
      implicit none

! Constants
      include 'params.inc'
      integer    char_A,              char_Z
c      parameter (char_A = ichar('A'), char_Z = ichar('Z'))
      integer    lcshift
c      parameter (lcshift = ichar('a') - ichar('A'))

! Arguments

      character*(*) xbuff
      integer iline(*),nparticles

! Global

      character*(4*max_particles) particle(4)
      integer                               charge_c(max_particles)
      integer iparticle(0:max_particles,0:4),inverse(max_particles)
      common/to_model/iparticle,  particle,  inverse, charge_c


      character*(max_string) iwave(max_particles),owave(max_particles)
      integer iposx(3,max_particles)
      integer info_p(5,max_particles)
      character*(8) str(3,max_particles)
      common/to_external/iwave,owave,iposx,info_p,str

      integer         nincoming,nincfirst
      common/to_proc2/nincoming,nincfirst

      integer ndcmax
      parameter(ndcmax=20)
      integer ndcpart,idcid(ndcmax),idcgroup(ndcmax)
      logical dcfinal(ndcmax)
      common/to_dc/ndcpart,idcid,idcgroup,dcfinal

      integer                 SumParticles(0:maxpartons,0:maxloops)
      integer                 icount(maxpartons,maxloops)
      integer                 njets(0:maxpartons)
      integer                 ijet(maxpartons,maxloops)
      common/to_SumParticles/ SumParticles,icount,ijet,njets

      logical flip(maxlines)
      common/to_flip/flip
!      data flip/maxlines*.true./

!     External 
      integer groupid
      external groupid

! Local
      integer npartmax
      parameter(npartmax=20)
      character*(max_string) buff
      integer i,in1,in2,in3,in4,j,k,ii,jj
      integer npar,igroup,ksort(npartmax),ktmp,igrtop,izeroes,nzeroes
      external izeroes,nzeroes
      integer nmopar(npartmax),kback(npartmax)!,ndapar(npartmax)
      logical final,foundp,moth(0:npartmax),flipdc(1:npartmax)
      integer npart,ipgroup(npartmax),ipext(npartmax),ipid(npartmax)
      logical pfinal(npartmax),ident,multiparts
      integer identfact(npartmax),identnum(npartmax)
      integer nsumparts,in,nchar,n,istrparts,maxpar,maxd
      integer isumgr(maxloops),ilabel(maxloops),isumid(maxloops)
      character*4 namesum,str1
      character*(max_string) strparts
      integer nmothers(0:npartmax),idaughters(0:npartmax,npartmax,3)
      integer i2daughters(0:npartmax,npartmax,3),ndaughters(npartmax)
      integer ipvalue(npartmax),i2pvalue(npartmax),imoth,nferm

!-----
!  Begin Code
!-----      
      char_A = ichar('A')
      char_Z = ichar('Z')
      lcshift = ichar('a') - ichar('A')
      buff   = xbuff
      do i=1,len(xbuff)
         if (ichar(buff(i:i)) .ge. char_A .and.
     &       ichar(buff(i:i)) .le. char_Z) then
            buff(i:i)=char(ichar(buff(i:i))+lcshift)
         endif
      enddo

      i=1                       ! index(xbuff,'>')+1
      npar  =0                  ! Number of parentheses
      igroup=0                  ! Present group number
      npart =0                  ! Number of particles found
      final =.true.             ! Present particle final state?
      moth(0)=.false.
      multiparts=.false.
      maxpar = 0

      do j=1,npartmax
        nmothers(j)=0
        flipdc(j)=.true.
      enddo

      if(index(buff,'(').ne.0) then
         write(*,*) 'Decay chain process ',buff(1:len_trim(buff))
      endif

C   Step through the string and look for parentheses, ">" and particles
      do while(i.le.len_trim(buff))

        if(buff(i:i).eq.'(')then
          npar=npar+1
          nmopar(npar)=0
c          ndapar(npar)=0
          moth(npar)=.true.
          if(buff(i-1:i-1).eq.')')
     $       igroup=igroup+10**(8-npar)
          igroup=igroup+10**(8-npar)
          final=.false.
          i=i+1
        else if(buff(i:i).eq.'>')then
c          ndapar(npar)=0
          if(.not.moth(npar)) then
             write(*,*) 'Error: ''>'' in wrong place '//
     $            '- must be in parentheses'
             STOP
          endif
          moth(npar)=.false.
          final=.true.
c       Set information about tree for sorting
          idaughters(npar,nmothers(npar),2)=npart+1

          i=i+1
        else if(buff(i:i).eq.')')then
          igroup=igroup-10**(8-npar)
          i=i+1
          if(nmopar(npar).ne.1)then
            write(*,*) 'Error: ',nmopar(npar),' mothers! Should be 1!'
            STOP
          endif

c       Set information about tree for sorting
          idaughters(npar,nmothers(npar),3)=npart

          npar=npar-1
          if(npar.lt.0) then
            write(*,*) 'Error: Too many closing parentheses!'
            STOP
          endif
          pfinal(npart)=.true.
c          print *,' set final to ',pfinal(npart)

        else if(buff(i:i).eq.'/')then
c       We've reached the "exclude particles", done reading
          goto 10
        else
C       Identify particles and set group and final flag
          in4 = index(particle(4),buff(i:i+3))-1
          in3 = index(particle(3),buff(i:i+2))-1
          in2 = index(particle(2),buff(i:i+1))-1
          in1 = index(particle(1),buff(i:i+0))-1
          foundp=.false.
          if (mod(in4,4) .eq. 0) then
            npart=npart+1
            ipid(npart)= iparticle(in4/4+1,4)
c            print*,npart,': Got one ',particle(4)(in4+1:in4+4)
            i=i+4
            foundp=.true.
          elseif (mod(in3,3) .eq. 0) then
            npart=npart+1
            ipid(npart)= iparticle(in3/3+1,3)
c            print*,npart,': Got one ',particle(3)(in3+1:in3+3)
            i=i+3
            foundp=.true.
          elseif (mod(in2,2) .eq. 0) then
            npart=npart+1
            ipid(npart)= iparticle(in2/2+1,2)
c            print*,npart,': Got one ',particle(2)(in2+1:in2+2)
            i=i+2
            foundp=.true.
          elseif (mod(in1,1) .eq. 0 .and. in1 .ge. 0) then
            npart=npart+1
            ipid(npart)= iparticle(in1/1+1,1)
c            print*,npart,': Got one ',particle(1)(in1+1:in1+1)
            i=i+1
            foundp=.true.
          else
            buff=buff(1:i-1)//buff(i+1:)
          endif
          if(foundp)then
            if(npar.eq.0)then
              pfinal(npart)=.true.
              ipgroup(npart)=0
            else
              pfinal(npart)=final
              ipgroup(npart)=igroup
              if(moth(npar)) then
                nmopar(npar)=nmopar(npar)+1
c       Set information about tree for sorting
                nmothers(npar)=nmothers(npar)+1
                if(npar.gt.maxpar) maxpar=npar
                idaughters(npar,nmothers(npar),1)=npart
c              else
c                ndapar(npar)=ndapar(npar)+1
              endif
            endif
c            print *,' ',ipid(npart),' with group ',ipgroup(npart),
c     $         ' and final = ',pfinal(npart)
          endif

        endif

      enddo

 10   if(npar.gt.0) then
        write(*,*) 'Error: Too few closing parentheses!'
        STOP
      endif
      if(npar.lt.0) then
        write(*,*) 'Error: Too many closing parentheses!'
        STOP
      endif
      if(npart.le.0) then
        write(*,*) 'Error: No particles found!'
        STOP
      endif

c      print *,'Particles: id, group, final'
c      do i=1,npart
c        print *,i,ipid(i),ipgroup(i),pfinal(i)
c      enddo

      nmothers(0)=1
      idaughters(0,1,1)=0
      idaughters(0,1,2)=1
      idaughters(0,1,3)=npart
      i2daughters(0,1,1)=0
      i2daughters(0,1,2)=1
      i2daughters(0,1,3)=npart

C   Check for multiparticle labels and make sure no same label occurs
C   in different groups
      do i=1,SumParticles(0,0)
        isumgr(i) = -1
        isumid(i) = 0
        ilabel(i) = 0
      enddo
      nsumparts=SumParticles(0,0)
      do i=1,npart
        do j=1,nsumparts
          if (ipid(i) .eq. SumParticles(0,j)) then
            multiparts=.true.
            write(*,*) 'Found multiparticle: ',i,j
            if(isumgr(j).eq.-1)then
              isumid(j)=ipid(i)
            else if(isumgr(j).ne.ipgroup(i))then
c              print *,'New group: ',isumgr(j),ipgroup(i)
              ilabel(j)=ilabel(j)+1
              strparts=''
              istrparts=0
              call part_string(ipid(i),namesum,nchar)
              if(nchar.ge.4) nchar=3
              write(namesum(nchar+1:nchar+1),'(i1)') ilabel(j)
              do while(index(particle(nchar+1),namesum(:nchar+1)).gt.0)
                ilabel(j)=ilabel(j)+1
                write(namesum(nchar+1:nchar+1),'(i1)') ilabel(j)
              enddo
              do k=1,sumparticles(1,j)
                call part_string(sumparticles(k+1,j),str1,n)
                strparts(istrparts+1:istrparts+n)=str1(1:n)
                istrparts=istrparts+n
              enddo
c              print *,'Adding ',namesum,sumparticles(1,j),strparts
              call AddLoopParticles(namesum(:nchar+1),strparts)
              in=index(particle(nchar+1),namesum(:nchar+1))-1
              isumid(j)=iparticle(in/(nchar+1)+1,nchar+1)
            else
c              print *,'Same group: ',isumgr(j),ipgroup(i)
            endif
            ipid(i)=isumid(j)
            isumgr(j)=ipgroup(i)
          endif
        enddo
      enddo
        
C   Sort the particles according to the group
      do i=1,npart
        ksort(i)=i
      enddo

      do i=npart,2,-1
        do j=2,i
          if((ipgroup(ksort(j)).lt.ipgroup(ksort(j-1)).or.
     $       ipgroup(ksort(j-1)).eq.0).and.ipgroup(ksort(j)).ne.0)then
            ktmp=ksort(j)
            ksort(j)=ksort(j-1)
            ksort(j-1)=ktmp
          endif
        enddo
      enddo

C   kback maps in other direction
      do i=1,npart
        kback(ksort(i))=i
      enddo

C   Sort mother-daughter info
c      print *,'Mother daughter1, daughtern (bef. sort)'
      do i=0,maxpar
c        print *,'par: ',i,': mothers: ',nmothers(i)
        do j=1,nmothers(i)
c          print *,(idaughters(i,j,k),k=1,3)
          i2daughters(i,j,1)=kback(idaughters(i,j,1))
          i2daughters(i,j,3)=i2daughters(i,j,1)+1+
     $       idaughters(i,j,3)-idaughters(i,j,2)
          i2daughters(i,j,2)=i2daughters(i,j,1)+1
        enddo
      enddo 

c      print *,'Mother daughter1, daughtern (after sort)'
c      do i=1,maxpar
c        print *,'par: ',i,': mothers: ',nmothers(i)
c        do j=1,nmothers(i)
c          print *,(i2daughters(i,j,k),k=1,3)
c        enddo
c      enddo 
 
C   Fill common block arrays
      ndcpart=npart
      do i=1,npart
        idcid(i)=ipid(ksort(i))
        idcgroup(i)=ipgroup(ksort(i))
        dcfinal(i)=pfinal(ksort(i))
        write(*,*) 'Part Info after sort: ',
     $     i,idcid(i),idcgroup(i),dcfinal(i)
      enddo

C     Check for bosons decaying to two fermions, in that case 
C     set majorana fermion flow flip for one of them to false
      do i=1,maxpar
        do j=1,nmothers(i)
          imoth=i2daughters(i,j,1)
          if(info_p(2,idcid(imoth)).ne.2)then
            nferm=0
            do ii=i2daughters(i,j,2),i2daughters(i,j,3)
              if(info_p(2,idcid(ii)).eq.2.and.dcfinal(ii))then
                nferm=nferm+1
              endif
            enddo
            if(nferm.eq.2)then
              do ii=i2daughters(i,j,2),i2daughters(i,j,3)
                if(info_p(2,idcid(ii)).eq.2.and.dcfinal(ii))then
                  flipdc(ii)=.false.
                  goto 100
                endif
              enddo              
 100          continue
            endif
          endif
        enddo
      enddo 

      do i=1,npart
        write(*,*) 'Final part info: ',
     $     i,idcid(i),idcgroup(i),dcfinal(i),flipdc(i)
      enddo


C   Define external particles and set group ids for them
      do i = 1, nincoming
         call setgroupid(i,0)
         call setgroupid(-i,0)
      enddo
      nparticles=0
      igroup=1
      igrtop=0
      do i=1,npart
        if(dcfinal(i))then
          nparticles=nparticles+1
          iline(nparticles)=idcid(i)
          flip(nparticles)=flipdc(i)
          call setgroupid(nparticles+nincoming,0)
          call setgroupid(-(nparticles+nincoming),0)
          if(idcgroup(i).ne.0)then
c         Check if new group, e.g.
c         if igroup=11000000 then 11100000 is same group, but
c         12000000 or 20000000 is new group
            if(idcgroup(i)-igroup.ge.izeroes(igroup) .or. .true.)then
c           New group of final state particles
              igroup=idcgroup(i)
c              igrtop=igrtop+1
              igrtop = idcgroup(i)
c              print *,'Set top. groupid for particle ',
c     $           nparticles+nincoming,' (',i,') to ',igrtop
c              print *,'Set to group mother'
              call setgroupid(nparticles+nincoming,igrtop)
c              call setmother(nparticles+nincoming)
            else
c           Same group as last one
              call setgroupid(nparticles+nincoming,igrtop)
c              print *,'Set top. groupid for particle ',
c     $           nparticles+nincoming,' (',i,') to ',igrtop
            endif
          endif
        endif
      enddo

c      read(*,*) i
c      call orderparticles
      return
      end

      subroutine write_dc(buff,nlen,iparticles,nparticles)
c***************************************************************************
c   Prints out decay chain given particles
c   Note that it doesn't print incoming particles, so these should 
c   already be in buff
c   - buff is character buffer (should be max_string in length)
c   - nlen is starting position in buff, and gives back ending position
c   - iparticles are particle id:s, including intermediate states
c   - nparticles is the number of id:s in iparticles. Must be = ndcpart.
c***************************************************************************

      implicit none

! Constants
      include 'params.inc'
      integer    char_A,              char_Z
c      parameter (char_A = ichar('A'), char_Z = ichar('Z'))
      integer    lcshift
c      parameter (lcshift = ichar('a') - ichar('A'))

! Arguments

      character*(*) buff
      integer nlen,iparticles(*),nparticles

! Global

      character*(4*max_particles) particle(4)
      integer                               charge_c(max_particles)
      integer iparticle(0:max_particles,0:4),inverse(max_particles)
      common/to_model/iparticle,  particle,  inverse, charge_c
      integer         nincoming,nincfirst
      common/to_proc2/nincoming,nincfirst

      integer ndcmax
      parameter(ndcmax=20)
      integer ndcpart,idcid(ndcmax),idcgroup(ndcmax)
      logical dcfinal(ndcmax)
      common/to_dc/ndcpart,idcid,idcgroup,dcfinal

c Local
      integer i,nchar,itemp, nopen
      integer npar,igroup,izeroes

c External 
      integer groupid
      external izeroes

      if(nparticles.ne.ndcpart) then
        write(*,*) 'Error: write_dc called with nparticles != ndcpart'
        stop
      endif

      npar=0
      igroup=0
      nopen = 0
      do i=1,nparticles
c        print *,i,idcid(i),idcgroup(i),dcfinal(i),igroup,izeroes(igroup)
        if(npar.gt.0.and.(idcgroup(i)-igroup.ge.izeroes(igroup).or.
     $     idcgroup(i).eq.0))then
           itemp=igroup
c           print *,'npar,itemp',npar,itemp
           do while(idcgroup(i)-itemp.ge.izeroes(itemp).or.
     $        (idcgroup(i).eq.0.and.npar.gt.0))
             buff(nlen:nlen)=')'
             nlen=nlen+1
             nopen = nopen - 1
             itemp = itemp+10**(8-npar)
             if(itemp.lt.0)then
               write(*,*) 'Error:Writing of process failed, itemp < 0'
               stop
             endif
             npar=npar-1
c             print *,'npar,itemp',npar,itemp
           enddo
c           if(idcgroup(i).gt.0) then
c             npar=npar+1
c             buff(nlen:nlen)='('
c             nlen=nlen+1
c           endif
        endif
        if (idcgroup(i).gt.igroup) then
           npar=npar+1
           buff(nlen:nlen) = '('
           nlen=nlen+1
           nopen=nopen+1
        endif
        call part_string(iparticles(i),buff(nlen:nlen+3),nchar)
        nlen = nlen+nchar
        if (.not.dcfinal(i)) then
            buff(nlen:nlen) = '>'
            nlen = nlen+1
        endif
        igroup=idcgroup(i)
        if(nlen.gt. max_string)then
           write(*,*) 'Error: Too long string of particles, exceeding ',
     $        max_string
           stop
        endif
      enddo
c
c     tjs 3/30/07
c     added closing parenthesis
c
      do i=1,nopen
         buff(nlen:nlen)=')'
         nlen=nlen+1
      enddo
      write(*,*) 'buff = ',buff
      return
      end
      
      function izeroes(i)
c***************************************************************************
c     If i=211000, izeroes returns 1000, i.e. a 1 with same number of 
c     trailing zeroes as i
c***************************************************************************

      implicit none
      integer izeroes,i,j

      izeroes=i
      if(i.lt.10) return
      j=0
      do while(mod(i,int(10**j)).eq.0)
        j=j+1
      enddo
      izeroes=int(10**(j-1))
      return
      end
      
      function nzeroes(i)
c***************************************************************************
c     If i=211000, izeroes returns 1000, i.e. a 1 with same number of 
c     trailing zeroes as i
c***************************************************************************

      implicit none
      integer nzeroes,i,j

      nzeroes=0
      if(i.lt.10) return
      j=0
      do while(mod(i,int(10**j)).eq.0)
        j=j+1
      enddo
      nzeroes=j-1
      return
      end


      subroutine OrderParticles(ndcpart,idcid,idcgroup,dcfinal)
c*************************************************************************
c     Routine to place the particles in a unique order such that
c     we can easily determine if two processes are identical
c     to avoid double counting.
c*************************************************************************
      implicit none
      integer ndcmax
      parameter(ndcmax=20)
      integer npartmax
      parameter(npartmax=20)      
c
c     Arguments
c
      integer ndcpart,idcid(ndcmax),idcgroup(ndcmax)
      logical dcfinal(ndcmax)

c
c     Local
c
      integer i,j,k,npart,ktmp,npar,igroup,itemp,maxd,mind,ii,jj
      integer ksort(npartmax),ipid(npartmax),ipgroup(npartmax)
      integer nmothers(0:npartmax),idaughters(0:npartmax,npartmax,3)
      integer i2daughters(0:npartmax,npartmax,3),ndaughters(npartmax)
      integer ipvalue(npartmax),i2pvalue(npartmax),imoth,maxpar
      logical pfinal(npartmax)
      integer izeroes,nzeroes
      external izeroes,nzeroes
c
c     Global
c
      integer ndcpart_g,idcid_g(ndcmax),idcgroup_g(ndcmax)
      logical dcfinal_g(ndcmax)
      common/to_dc/ndcpart_g,idcid_g,idcgroup_g,dcfinal_g
c-----
c  Begin Code
c-----

c      print *,'In OrderParticles'

c
c     First create local copy of global variables
c     Don't want to change globals, only local
c
      ndcpart = ndcpart_g
      do i = 1, ndcpart
         idcid(i) = idcid_g(i)
         idcgroup(i) = idcgroup_g(i)
         dcfinal(i) = dcfinal_g(i)
      enddo
      npart = ndcpart

C   Loop through particles to find mother-daughter structure
      do j=1,npartmax
        nmothers(j)=0
      enddo
      maxpar = 0
      nmothers(0)=1
      i2daughters(0,1,1)=0
      i2daughters(0,1,2)=1
      i2daughters(0,1,3)=npart

      npar=0
      igroup=0
c      print *,'i,idcid(i),idcgroup(i),dcfinal(i),igroup,izeroes(igroup)'
      do i=1,npart
c        print *,i,idcid(i),idcgroup(i),dcfinal(i),igroup,izeroes(igroup)
        if(npar.gt.0.and.(idcgroup(i)-igroup.ge.izeroes(igroup).or.
     $     idcgroup(i).eq.0))then
           itemp=igroup
c           print *,'npar,itemp',npar,itemp
           do while(idcgroup(i)-itemp.ge.izeroes(itemp).or.
     $        (idcgroup(i).eq.0.and.npar.gt.0))
c       Set information about tree for sorting
             i2daughters(npar,nmothers(npar),3)=i-1
c             print *,'idaughters end: ',npar,
c     $          i2daughters(npar,nmothers(npar),3)
             itemp = itemp+10**(8-npar)
             if(itemp.lt.0)then
               write(*,*) 'Error:Writing of process failed, itemp < 0'
               stop
             endif
             npar=npar-1
c             print *,'npar,itemp',npar,itemp
           enddo
c           if(idcgroup(i).gt.0) then
c             npar=npar+1
c             buff(nlen:nlen)='('
c             nlen=nlen+1
c           endif
        endif
        if (idcgroup(i).gt.igroup) then
           npar=npar+1
        endif
        if (.not.dcfinal(i)) then
c       Set information about tree for sorting
          nmothers(npar)=nmothers(npar)+1
          if(npar.gt.maxpar) maxpar=npar
          i2daughters(npar,nmothers(npar),1)=i
          i2daughters(npar,nmothers(npar),2)=i+1
c          print *,'i, npar, nmothers: ',i,npar,nmothers(npar)
c          print *,'idaughters: ',i2daughters(npar,nmothers(npar),1),
c     $       i2daughters(npar,nmothers(npar),2)
        endif
        igroup=idcgroup(i)
      enddo
c   Close parentheses
      do while(npar.gt.0)
c       Set information about tree for sorting
           i2daughters(npar,nmothers(npar),3)=npart
c           print *,'idaughters end: ',npar,
c     $        i2daughters(npar,nmothers(npar),3)
           npar=npar-1
      enddo

C     Start by defining particle labels for the particles,
C     in same order as particle ids; just a way to reduce number of 
C     particle ids
        do i=1,npart
          ksort(i)=i
        enddo
        do i=npart,2,-1
          do j=2,i
            if(idcid(ksort(j)).lt.idcid(ksort(j-1)))then
              ktmp=ksort(j)
              ksort(j)=ksort(j-1)
              ksort(j-1)=ktmp
            endif
          enddo
        enddo
        do i=1,npart
          ipvalue(ksort(i))=i
          if(i.gt.1.and.idcid(ksort(i)).eq.idcid(ksort(i-1)))then
            ipvalue(ksort(i))=ipvalue(ksort(i-1))
          endif
        enddo
c        print *,'Particle ids:',(ipvalue(i),i=1,npart)

c        do i=0,maxpar
c          do j=1,nmothers(i)
c            print *,'idaughters(',i,j,')=',(i2daughters(i,j,ii),ii=1,3)
c          enddo
c        enddo
        
        do i=1,npart
          ksort(i)=i
        enddo
C     First sort final state particles in all groups
        do i=0,maxpar
          do j=1,nmothers(i)
            k=i2daughters(i,j,2)
            mind=i2daughters(i,j,2)
            maxd=mind-1
            do while (dcfinal(k).and.k.le.i2daughters(i,j,3))
c           Find last final state particle
              maxd=k
              k=k+1
            enddo
            if(i.eq.0)then
c           If 0 pars, final state particles at end of list
              do ii=1,npart
                if(idcgroup(ii).eq.0)then
                  mind=ii
                  maxd=npart
                  goto 10
                endif
              enddo
            endif
10          do ii=maxd,mind+1,-1
              do jj=mind+1,ii
                if(idcid(ksort(jj)).lt.idcid(ksort(jj-1)))then
                  ktmp=ksort(jj)
                  ksort(jj)=ksort(jj-1)
                  ksort(jj-1)=ktmp
                endif
              enddo
            enddo
c            print *,'Sorted fs states: ',(ksort(ii),ii=
c     $         mind,maxd)
C         Calculate fs value for mother (if there is one)
            if(i.gt.0)then
              do ii=1,maxd-mind+1
                ipvalue(i2daughters(i,j,1))=ipvalue(i2daughters(i,j,1))+
     $             (npart+1)**ii*ipvalue(ksort(i2daughters(i,j,2)+ii-1))
                ndaughters(i2daughters(i,j,1))=ii
              enddo
c              print *,'Value for ',i2daughters(i,j,1),': ',i,j,
c     $           ipvalue(i2daughters(i,j,1))
            endif
          enddo
        enddo
C     Perform the sorting to avoid confusion
        do i=1,npart
          ipid(i)=idcid(ksort(i))
          ipgroup(i)=idcgroup(ksort(i))
          pfinal(i)=dcfinal(ksort(i))
          i2pvalue(i)=ipvalue(ksort(i))
          write(*,*) 'Part Info after sort: ',
     $       i,ksort(i),ipid(i)
        enddo
C       Synchronize arrays
        do ii=1,npart
          idcid(ii)=ipid(ii)
          idcgroup(ii)=ipgroup(ii)
          dcfinal(ii)=pfinal(ii)
          ipvalue(ii)=i2pvalue(ii)
        enddo

C     Now sort the mothers, starting with the outermost ones (with
C     largest number of parentheses). Algorithm: Compare value of
C     mothers which are within the decay products of same grandmother
C     and sort, thereby working one's way up to the top decays.

        do i=maxpar,1,-1
          j=1
          imoth=1
          do while (j.le.nmothers(i))
C         Find mother of mothers (grandmother)
            do while (i2daughters(i,j,1).gt.i2daughters(i-1,imoth,3))
              imoth=imoth+1
            enddo
            k=j+1
C         Find last daughter of present grandmother
            do while (k.le.nmothers(i).and.
     $         i2daughters(i,k,1).lt.i2daughters(i-1,imoth,3))
              k=k+1              
            enddo
            k=k-1

C         Sort mothers according to value
            do ii=j,k
              ksort(ii)=ii
            enddo
c            print *,'mothers, values: ',
c     $         (i2daughters(i,ksort(jj),1),jj=j,k),
c     $         (i2pvalue(i2daughters(i,ksort(jj),1)),jj=j,k)
            do ii=k,j+1,-1
              do jj=j+1,ii
                if(i2pvalue(i2daughters(i,ksort(jj),1)).lt.
     $             i2pvalue(i2daughters(i,ksort(jj-1),1)))then
                  ktmp=ksort(jj)
                  ksort(jj)=ksort(jj-1)
                  ksort(jj-1)=ktmp
                endif
              enddo
            enddo

c            print *,'Sorted mothers:',(ksort(ii),ii=j,k)

C         Add sorted mother values to grandmother
            if(i.gt.1)then
              jj=i2daughters(i-1,imoth,1)
              ndaughters(jj)=ndaughters(jj)+1
              do ii=j,k
                ipvalue(jj)=ipvalue(jj)+
     $             (npart+1)**ndaughters(jj)*
     $             ipvalue(i2daughters(i,ksort(ii),1))
                ndaughters(jj)=ndaughters(jj)+
     $             ndaughters(i2daughters(i,ksort(ii),1))
              enddo
c              print *,'ipvalue(',jj,')=',ipvalue(jj)
            endif

C         Set new mother info from sorting
            jj=i2daughters(i,j,1)
            do ii=j,k
              idaughters(i,ii,1)=jj
              idaughters(i,ii,2)=jj+1
              idaughters(i,ii,3)=jj+i2daughters(i,ksort(ii),3)-
     $           i2daughters(i,ksort(ii),2)+1
              jj=idaughters(i,ii,3)+1
            enddo
            imoth=imoth+1
            j=k+1
          enddo

C       Perform reshuffling of particles according to sort
          do ii=1,nmothers(i)
            do jj=0,idaughters(i,ii,3)-idaughters(i,ii,1)
c              print *,'new, old: ',
c     $           idaughters(i,ii,1)+jj,i2daughters(i,ksort(ii),1)+jj
              idcid(idaughters(i,ii,1)+jj)=
     $           ipid(i2daughters(i,ksort(ii),1)+jj)
              idcgroup(idaughters(i,ii,1)+jj)=
     $           ipgroup(i2daughters(i,ksort(ii),1)+jj)
              dcfinal(idaughters(i,ii,1)+jj)=
     $           pfinal(i2daughters(i,ksort(ii),1)+jj)
              ipvalue(idaughters(i,ii,1)+jj)=
     $           i2pvalue(i2daughters(i,ksort(ii),1)+jj)
            enddo
          enddo

C       Synchronize arrays
          do ii=1,npart
            ipid(ii)=idcid(ii)
            ipgroup(ii)=idcgroup(ii)
            pfinal(ii)=dcfinal(ii)
            i2pvalue(ii)=ipvalue(ii)
          enddo
          do ii=1,nmothers(i)
            do jj=1,3
              i2daughters(i,ii,jj)=idaughters(i,ii,jj)
            enddo
          enddo
c          print *,'Part. ids:',(idcid(ii),ii=1,npart)

        enddo ! i

c        print *,'Resulting particle ids:'
c        print *,(ipid(ii),ii=1,npart)

cC     Set new groups with the right order
c        igroup=0
c        npar=0
c        do i=1,npart
c          if(ipgroup(i).eq.0) cycle
c          if(i.eq.1.and.idcgroup(i).ne.igroup)then
c            npar=1
c            igroup=igroup+10**(8-npar)
c            idcgroup(i)=igroup
c            cycle
c          endif
c          if(ipgroup(i).eq.ipgroup(i-1))then
c            idcgroup(i)=igroup
c          else if(izeroes(ipgroup(i)).lt.izeroes(ipgroup(i-1)))then
c            npar=npar+1
c            igroup=igroup+10**(8-npar)
c            idcgroup(i)=igroup
c          else if(izeroes(ipgroup(i)).eq.izeroes(ipgroup(i-1)))then
c            igroup=igroup+10**(8-npar)
c            idcgroup(i)=igroup
c          else
cc            print *,'igroup,ipgroup(i):',igroup,ipgroup(i)
c            do while(izeroes(igroup).lt.izeroes(ipgroup(i)))
cc              print *,'nzeroes: ',nzeroes(igroup),8-npar+1
c              do while(nzeroes(igroup).lt.8-npar+1)
c                igroup=igroup-10**(8-npar)
c              enddo
c              npar=npar-1
c            enddo
c            igroup=igroup+10**(8-npar)
c            idcgroup(i)=igroup
cc            print *,'idcgroup,ipgroup:',idcgroup(i),ipgroup(i)
c          endif
c        enddo

      end

      logical function DupProcess(idum)
c*********************************************************************
c     Checks to see if a process has been already calculated
c     Current version does NOT check for matching coupling orders
c*********************************************************************
      implicit none
c
c     Constants
c
      include "params.inc"
      integer ndcmax
      parameter(ndcmax=20)
      integer    maxsubproc
      parameter (MaxSubProc=9999)
c
c     Arguments
c
      integer idum    !not used, inputs come from common block
c
c     Local
c
      integer nproc
      integer ndcpart,idcid(ndcmax),idcgroup(ndcmax)
      logical dcfinal(ndcmax)
      integer sv_ndcpart(MaxSubProc),sv_idcid(ndcmax,MaxSubProc)
      integer sv_incoming(0:2,MaxSubProc),sv_names(MaxSubProc)
      integer qonium(MaxSubProc)
      integer i,iproc,ipart
      logical foundmatch
c
c     Global
c

      integer        iline(-maxlines:maxlines),idir(-maxlines:maxlines)
      integer        this_coup(max_coup) ,goal_coup(max_coup)
      common/to_proc/iline,idir,this_coup,goal_coup
      integer         nincoming,nincfirst
      common/to_proc2/nincoming,nincfirst
      integer    nameprefix
      common/to_multiname/nameprefix

      data nproc/0/
      save sv_ndcpart,sv_incoming
c
c     function
c
      integer get_tag_onium
      external get_tag_onium

      include "onia.inc"
c-----
c  Begin Code
c-----      
      call OrderParticles(ndcpart,idcid,idcgroup,dcfinal)
      iproc = 1
      foundmatch = .false.
      do while (iproc .le. nproc .and. .not. foundmatch)
c
c     First check initial state particles
c
         if(nameprefix .ne. sv_names(iproc))then
            iproc = iproc+1
            cycle
         endif

         foundmatch = (nincoming .eq. sv_incoming(0,iproc))
         ipart = 0
         do while (foundmatch .and. ipart .lt. nincoming)
            ipart = ipart+1
            foundmatch = (iline(ipart) .eq. sv_incoming(ipart,iproc))
         enddo         
c
c     Now check final state particles
c
         foundmatch = (ndcpart .eq. sv_ndcpart(iproc) .and. foundmatch)
         ipart = 0
         do while (foundmatch .and. ipart .lt. ndcpart)
            ipart = ipart+1
            foundmatch = (idcid(ipart) .eq. sv_idcid(ipart,iproc))
         enddo
         if (.not. foundmatch) then
c            write(*,*) "Failed imatch",iproc,ipart,idcid(ipart),sv_idcid(ipart,iproc)
         endif
c
c        PA: Here we have to do additional an check in case of onium state
c
         foundmatch=(get_tag_onium(S_qn,L_qn,J_qn,C_qn)
     &  .eq.qonium(iproc).and.foundmatch)
         iproc = iproc+1
      enddo
      if (foundmatch) then
         write(*,*) "Found match",iproc,nproc
         write(*,'(25i5)') (idcid(i),i=1,ndcpart)         
         write(*,'(25i5)') (sv_idcid(i,iproc),i=1,ndcpart)
      else
         nproc = nproc+1
         write(*,*) "New Process",nproc
         write(*,'(25i5)') (idcid(i),i=1,ndcpart)         
         do i=1,ndcpart
            sv_idcid(i,nproc) = idcid(i)
         enddo
         sv_names(nproc) = nameprefix
c
c        PA: add info on onium
         qonium(nproc)=get_tag_onium(S_qn,L_qn,J_qn,C_qn)
c
         sv_ndcpart(nproc) = ndcpart
         sv_incoming(0,nproc) = nincoming
         do ipart=1,nincoming
            sv_incoming(ipart,nproc)=iline(ipart)
         enddo
      endif
      dupprocess = foundmatch
      end

      subroutine trimspace(str,maxlen)
      implicit none

      character*(*) str
      integer maxlen,i,j

      j=len_trim(str(1:maxlen))
      i=index(str(1:j),' ')
      do while(i.gt.0)
        str = str(1:i-1)//str(i+1:j)
        j=j-1
        i=index(str(1:j),' ')
      enddo

      return
      end


      subroutine no_spaces(buff,nchars)
c**********************************************************************
c     Given buff a buffer of words separated by spaces
c     returns it where all space are moved to the right
c     returns also the length of the single word.
c     maxlength is the length of the buffer
c     AUTHOR: FABIO MALTONI
c**********************************************************************
      implicit none
c
c     Constants
c
      integer    maxline
      parameter (maxline=80)
      character*1 null
      parameter  (null=' ')
c
c     Arguments
c
      character*(maxline) buff
      integer nchars,maxlength
c
c     Local
c
      integer i,j
      character*(maxline) temp
c-----
c  Begin Code
c-----
      nchars=0
c      write (*,*) "buff=",buff(1:maxlength)
      do i=1,maxline
         if(buff(i:i).ne.null) then
            nchars=nchars+1
            temp(nchars:nchars)=buff(i:i)
         endif
c         write(*,*) i,":",buff(1:maxlength),":",temp(1:nchars),":"
      enddo
      buff=temp
      end

      integer function get_tag_onium(S_qn,L_qn,J_qn,C_qn)
c
c     this function tags th onium state with the use of prime numbers 
c     (PA)

c
c     arguments
c
      integer S_qn,L_qn,J_qn,C_qn
c---
cBegin code
c---
      if (C_qn.gt.0) then
c
         if(S_qn.eq.0) then
            get_tag_onium=2
         elseif (S_qn.eq.1) then
            get_tag_onium=3
         endif
c
         if(L_qn.eq.0) then
            get_tag_onium=get_tag_onium*5
         elseif (L_qn.eq.1) then
            get_tag_onium=get_tag_onium*7
         endif
c
         if(J_qn.eq.0) then
            get_tag_onium=get_tag_onium*11
         elseif (J_qn.eq.1) then
            get_tag_onium=get_tag_onium*13
         elseif (J_qn.eq.2) then
            get_tag_onium=get_tag_onium*17
         endif
c
         if(C_qn.eq.1) then
            get_tag_onium=get_tag_onium*19
         elseif (C_qn.eq.8) then
            get_tag_onium=get_tag_onium*21
         endif
c
      else
        get_tag_onium=0
      endif
      return
      end

