      program gen_proc_id
c************************************************************************
c     Generates a unique directory name for a given process using
c     information from the particles.dat and interactions.dat files
c     as well as the requested process
c************************************************************************
      implicit none
c
c     Constants
c

c
c     Local
c
      logical more
      character*120 procname, process
      integer nchar,i, j, length
      integer particleid(99), nparticles, nincoming, maxorder


c-----
c  Begin Code
c-----
      call Load_Particles
      procname="Unknown"
      length = 8
      read(*,'(a)',end=99,err=99) process
      length = 1
      i = index(process,'!')
      if (i .gt. 0) then
         process = process(1:i)
      endif
c
c     First get incoming particles
c
      i = index(process,'>')
      procname ='p'
      length = 2
      call getparticle(process(1:i),particleid,nparticles)      
      nincoming = nparticles
      call gen_name(particleid,nparticles,procname(length:),nchar)
      length = length+nchar
c
c     Now check if there are required particles
c
      procname(length:length) = "_"
      length=length+1
      j = index(process(i+1:),'>')+i !Break for required particles
      if (j .gt. i) then
         call getparticle(process(i+1:j),particleid,nparticles)
         i = j
         call gen_name(particleid,nparticles,procname(length:80),nchar)
         length = length+nchar
      endif
c
c     Now get final state and excluded particles
c
      procname(length:length) = "_"
      length=length+1
      j = index(process(i+1:),'/')+i !Break for required particles
      if (j .eq. i) then  !No excluded particles
         call getparticle(process(i+1:),particleid,nparticles)
         call gen_name(particleid,nparticles,procname(length:80),nchar)
         length = length+nchar
         procname(length:length) = "_"
         length=length+1
      else
         call getparticle(process(i+1:j),particleid,nparticles)
         call gen_name(particleid,nparticles,procname(length:80),nchar)
         length = length+nchar
         procname(length:length) = "_"
         length=length+1
         call getparticle(process(j:),particleid,nparticles)
         call gen_name(particleid,nparticles,procname(length:80),nchar)
         length = length+nchar
      endif
      write(*,*) procname(1:length)
      maxorder = nincoming+nparticles-2
      call AppendOrder(procname,length,maxorder)
      call AppendModel(procname,length)
      call AppendMultiparticle(procname,length, process)
      call clean_name(procname,length)

 99   write(*,'(a)') procname(1:length-1)
      end

      subroutine AppendMultiparticle(procname, length, myprocess)
c*****************************************************************
c     Reads in multiparticle code definitions and creates a name
c     for them. Format requires first character is '#', then
c     a space separates the label from the particles to sum over
c*****************************************************************
      implicit none
c
c     Constants
c
      include 'params.inc'
c
c     Arguments
c
      character*(*) procname, myprocess
      integer length, maxorder
c
c     Local
c
      logical done, foundmatch
      character*80 process
      character*(max_chars) words(max_words) 
      integer particleid(99), nparticles, i, nchar, wlength(max_words)
      integer nwords
      integer myparticleid(99),mynparticles
c-----
c  Begin Code
c-----
      call getparticle(myprocess,myparticleid,mynparticles)
      open(unit=19,file='particles.dat',status='old',err=11)
      do while (.true.)
         read(19,'(a)', end=11,err=11) process
c         i = index(process,'!')
c         if (i .gt. 0) then
c            process = process(1:i)
c         endif         
         if (process(1:1) .ne. "#") then

            call parse_string(nwords,wlength,words,process)
            if (nwords .eq. 2) then

            call getparticle(words(1)(1:wlength(1)),particleid,
     $           nparticles)
            foundmatch = .false.
            do i=1,mynparticles
               if (particleid(1) .eq. myparticleid(i)) then
                  foundmatch=.true.
               endif
            enddo
            if (foundmatch) then
               procname(length:length) = "_"
               length=length+1
               procname(length:length+wlength(1))=words(1)(1:wlength(1))
               
               length=length+wlength(1)-1
               procname(length:length) = "_"
               length=length+1
               call getparticle(words(2)(1:wlength(2)),particleid,
     $              nparticles)
               call gen_name(particleid,nparticles,
     $              procname(length:),nchar)
               length=length+nchar-1
            endif
            endif !nwords = 2
         endif
      enddo
 11   return
      end

      subroutine AppendModel(procname, length)
c*****************************************************************
c     Reads in model name and appends to process name
c*****************************************************************
      implicit none
c
c     Constants
c
      include 'params.inc'
c
c     Arguments
c
      character*(*) procname
      integer length
c
c     Local
c
      logical done
      character*80 process
      character*(max_chars) words(max_words) 
      integer particleid(99), nparticles, i, nchar, wlength(max_words)
      integer nwords
c-----
c  Begin Code
c-----
      read(*,'(a)', end=11,err=11) process
      read(*,'(a)', end=11,err=11) process
      i = index(process,'!')
      if (i .gt. 0) then
         process = process(1:i)
      endif         
      call parse_string(nwords,wlength,words,process)
      procname(length:length) = "_"
      length=length+1
      procname(length:length+wlength(1)) = words(1)(1:wlength(1))
      length=length+wlength(1)-1
 11   return
      end

      subroutine AppendOrder(procname, length, maxorder)
c*****************************************************************
c     Cleans up name by replacing characters that can not be
c     used as file names.
c*****************************************************************
      implicit none
c
c     Constants
c
      include 'params.inc'
c
c     Arguments
c
      character*(*) procname
      integer length, maxorder
c
c     Local
c
      integer nqcd, i,j, ncoupling
      character*80 process
      logical done
c
c     Global
c
      character*(30) knowncouplings(max_coup)
      common/to_knowncouplings/knowncouplings
c-----
c  Begin Code
c-----
      done = .false.
      call get_n_coup(ncoupling)
c      write(*,*) 'Number of couplings ',ncoupling
      do j=1,ncoupling   !This needs to know the number of models
         write(*,*) 'Enter coupling ',knowncouplings(j)
         process = ' '
         read(*,'(a)',end=99,err=99) process
         i=index(process,'!')
         if (i .eq. 1) then
            process = ' '
         elseif (i .gt. 1) then
            process=process(1:i-1)
         endif
 99      if (process(1:1) .eq. ' ' .and. .false.) then 
            done = .true.
         else               
            nqcd=maxorder
            read(process,*,err=27) nqcd
 27         if (nqcd .gt. maxorder) nqcd=maxorder
            if (nqcd .lt. 10) then
               procname(length:length) = "0"
               write(procname(length+1:length+1),'(i1)') nqcd 
            else
               write(procname(length:length+1),'(i2)') nqcd 
            endif
            length=length+2
         endif
      enddo
c      read(*,'(a)',end=999,err=999) process
 999  end

      subroutine clean_name(procname, length)
c*****************************************************************
c     Cleans up name by replacing characters that can not be
c     used as file names.
c*****************************************************************
      implicit none
c
c     Constants
c
      integer    nsub
      parameter (nsub=1)
c
c     Arguments
c
      character*(*) procname
      integer length
c
c     Local
c
      integer i,j
      character*(nsub) cfind, creplace
      data  cfind    /'~'/
      data  creplace /'x'/
c-----
c  Begin Code
c-----
      do i=1,nsub
         do j=1,length
            if (procname(j:j) .eq. cfind(i:i)) then
               procname(j:j) = creplace(i:i)
            endif
         enddo
      enddo
      end

      subroutine gen_name(particleid,nparticles,procname,nchar)
c******************************************************************
c     Creates a string of characters corresponding to the 
c     "n" particles contained in array particleid but first
c     sorts the particles according to their code
c******************************************************************
      implicit none
c
c     Arguments
c
      integer nparticles                    !input number of particles
      integer particleid(nparticles)        !input particle id
      character*(*) procname                !output process name
      integer nchar                         !number of characters in procname
c
c     Local
c
      integer i,nlen
c-----
c  Begin Code
c-----
      nchar = 1
      call sort(particleid,nparticles)
      do i = 1 , nparticles
         call part_string(particleid(i),procname(nchar:nchar+3),nlen)
         nchar = nchar + nlen
      enddo
      nchar=nchar-1
      end

      subroutine Get_N_Coup(Ncoupling)
c************************************************************************
c     Determines the number of different models (QCD, QED etc)
c     in interactions.dat
c************************************************************************
      implicit none
c
c     Constants
c
      include 'params.inc'
c
c     Arguments
c
      integer Ncoupling
c
c     Local
c
      integer lun,nwords,length(max_words),i
      character*(max_chars) words(max_words)
      integer info(5), np
c-----
c  Begin Code
c-----
      lun=66
      ncoupling=0
      open(unit=lun,file='interactions.dat',status='old',err=200)
      do while (nwords .gt. 0)
         call readline(nwords,lun,length,words)
         if (words(1)(1:1) .eq. '#') then
         elseif (nwords .eq. 5) then
            call addcoup(words(5),ncoupling)
         elseif (nwords .eq. 8) then
            call addcoup(words(7),ncoupling)
            call addcoup(words(8),ncoupling)
         endif
      enddo
 200  close (lun)
      end

      subroutine addcoup(coupling,ncouplings)
c************************************************************************
c     Determines the number of different models (QCD, QED etc)
c     in interactions.dat
c************************************************************************
      implicit none
c
c     Constants
c
      include 'params.inc'
c
c     Arguments
c
      integer Ncouplings
      character*(*) coupling
c
c     Local
c
      integer i
c
c     Global
c      
      character*(30)           knowncouplings(max_coup)
      common/to_knowncouplings/knowncouplings
c-----
c  Begin Code
c-----
      i = 1
      do while (i .le. ncouplings .and.
     $     coupling .ne. knowncouplings(i))
         i=i+1
      enddo
      if (i .gt. max_coup) then
         write(*,*) 'Error too many couplings',max_coup
      elseif (i .gt. ncouplings) then
         write(*,*) 'Added coupling ',i,' ',coupling
         knowncouplings(i)=coupling
         ncouplings=i
      endif
      end

      subroutine optcheck(buff,line,num)
!****************************************************************************
!     Dummy routine
!****************************************************************************
      implicit none! Arguments

      character*(*) buff
      integer line,num

      return
      end
