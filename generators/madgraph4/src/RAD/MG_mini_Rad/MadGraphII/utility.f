!-----------------------
! madgraph - a Feynman Diagram package by Tim Stelzer and Bill Long 
! (c) 1993
!
! Filename: utility.f
!-----------------------
      subroutine lower_case(string)
      implicit none

! Constants

      integer    char_a,              char_z
c      parameter (char_a = ichar('A'), char_z = ichar('Z'))
      integer    lcshift
c      parameter (lcshift = ichar('a') - ichar('A'))


! Argument

      character*(*) string

! Local Variable

      integer i

!-----------
! Begin Code
!-----------
      char_a = ichar('A')
      char_z = ichar('Z')
      lcshift = ichar('a')-ichar('A')
      do i=1,len(string)
         if (ichar(string(i:i)) .ge. char_a .and.
     &       ichar(string(i:i)) .le. char_z) then
            string(i:i)=char(ichar(string(i:i))+lcshift)
         end if
      end do
      end

!-------------------------------------
      subroutine upper_case(string)
      implicit none

! Constants

      integer    char_a,              char_z
c      parameter (char_a = ichar('a'), char_z = ichar('z'))
      integer    ucshift
c      parameter (ucshift = ichar('A') - ichar('a'))

! Argument

      character*(*) string

! Local Variable

      integer i

!-----------
! Begin Code
!-----------
      char_A = ichar('a')
      char_Z = ichar('z')
      ucshift = ichar('A')-ichar('a')
c      write(*,*) string
      do i=1,len(string)
         if (ichar(string(i:i)) .ge. char_a .and.
     &       ichar(string(i:i)) .le. char_z) then
            string(i:i)=char(ichar(string(i:i))+ucshift)
         end if
      end do
c      write(*,*) string
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

      Subroutine Load_Particles
c*************************************************************************
c     Routine to add particles to MadGraphII.  First tries to read
c     from file particles.dat.  If this is not found, then loads
c     defaults.
c*************************************************************************
c
c     Constants
c
      include 'params.inc'
c      integer    max_words   , max_chars   , maxline
c      parameter (max_words=20, max_chars=20, maxline=80)
c
c     Local
c
      integer lun,nwords,length(max_words),i
      character*(max_chars) words(max_words)
      integer info(5), np

      lun=66
      open(unit=lun,file='particles.dat',status='old',err=200)
c
c     The first particle must be the unknown particle ??
c
      np = 0
      do i=1,4
         info(i)=0
      enddo
      call addparticle('?? ','  ','   ',info,'?? ','??','??','??')

      nwords=1
      do while (nwords .gt. 0)
         call readline(nwords,lun,length,words)
         if (words(1)(1:1) .eq. '#') then

         elseif (nwords .eq. 9 .or. nwords .eq. 8) then   !8 for no model info

c     Set up the line type in info(4) Wavy,Solid,Dashed,Curly
            call upper_case(words(4))
            if (words(4)(1:1) .eq. 'W') info(4)=1
            if (words(4)(1:1) .eq. 'S') info(4)=2
            if (words(4)(1:1) .eq. 'D') info(4)=3
            if (words(4)(1:1) .eq. 'C') info(4)=4
            
c     Set up color structure Triple, Octet, Singlet
            call upper_case(words(7))
            if (words(7)(1:1) .eq. 'T') info(1)=3
            if (words(7)(1:1) .eq. 'O') info(1)=8
            if (words(7)(1:1) .eq. 'S') info(1)=1

c     Get particle ID 
c            call upper_case(words(9))
            if (nwords .eq. 9) then
               read(words(9),*,err=123) model !This is really the particle ID
            else
c               write(*,*) (words(i),i=1,8)
 123           model = 0
            endif
c            write(*,*) 'Read model',model
c            model = 0
c            if (words(9)(1:3) .eq. 'QCD') model=1
c            if (words(9)(1:3) .eq. 'QED') model=2
c            if (words(9)(1:3) .eq. 'QFD') model=3
c            if (words(9)(1:3) .eq. 'BRS') model=4

c     Set up number of helicities 1,2,3
            call upper_case(words(3))
            if (words(3)(1:1) .eq. 'S') info(2)=1
            if (words(3)(1:1) .eq. 'F') info(2)=2
            if (words(3)(1:1) .eq. 'V') info(2)=3
c     If massless vector boson than only transverse modes remain
            call upper_case(words(5))
            call upper_case(words(6))
            if (info(2).eq.3) then
               if (words(5)(1:4) .eq. 'ZERO' .or.
     &             words(5)(1:3) .eq. '0D0') then
                  info(2) = 2
               endif
            endif

c     Some particles (gluon) we don't want label in diagrams
            if (words(8)(1:1) .eq. '_') words(8)=' '

c     Now add particle
            if (words(3)(1:1) .eq. 'S') then
               call addscalar(words(1)(1:length(1)),
     &              words(2)(1:length(2)),words(5)(1:length(5)),
     &              words(6)(1:length(6)),words(8)(1:length(8)),
     &              info(1),info(2),info(4),model)
            elseif(words(3)(1:1) .eq. 'F') then
               call addfermion(words(1)(1:length(1)),
     &              words(2)(1:length(2)),words(5)(1:length(5)),
     &              words(6)(1:length(6)),words(8)(1:length(8)),
     &              info(1),info(2),info(4),model)
            elseif(words(3)(1:1) .eq. 'V') then
               call addboson(words(1)(1:length(1)),
     &              words(2)(1:length(2)),words(5)(1:length(5)),
     &              words(6)(1:length(6)),words(8)(1:length(8)),
     &              info(1),info(2),info(4),model)
            else
               print*,'Warning particle type skipping: ',words(3)
            endif
            np=np+1
c            write(*,*) 'Adding particle: ',np, words(1),words(2)
         elseif (words(1)(1:1) .eq. '!') then
            print*,(words(i)(:length(i)),i=1,nwords)
         elseif (nwords .eq. 2) then        !This is a group of particles
            call AddLoopParticles(words(1),words(2))
         elseif (nwords .ne. 0) then
            print*,'Wrong number of inputs skipping line',
     &           nwords,' ',words(1)
         endif
      enddo
      print*,'Particles read from file.'
!      call AddLoopParticles("P","uu~dd~ss~cc~g")
!      call AddLoopParticles("J","uu~dd~ss~cc~g")
!      call AddLoopParticles("L","e+e-mu+mu-ta+ta-veve~")
      close(lun)
      return
 200  write(*,*) 'Need file particles.dat'
      stop
c 200  call addsmparticles
      end


      subroutine AddLoopParticles(code,sumover)
c***********************************************************************
c     This routine defines the particles codes that may be used for
c     looping over multiple particles (eg p,j,l+,l-,lv,lv~)
c     sumover gives list of particles that code can sum over
c***********************************************************************
      implicit none
c
c     Constants
c
      include 'params.inc'
c
c     Arguments
c
      character*(*) code, sumover
c
c     Local
c
      character*(5) buff
      character*(72) sumbuff
      integer n,ipart,npart,nloop
      integer i
      character*(4) str1
c
c     Global
c
      character*(4*max_particles) particle(4)
      integer                               charge_c(max_particles)
      integer iparticle(0:max_particles,0:4),inverse(max_particles)
      common/to_model/iparticle,  particle,  inverse, charge_c

      integer                 SumParticles(0:maxpartons,0:maxloops)
      integer                 icount(maxpartons,maxloops)
      integer                 njets(0:maxpartons)
      integer                 ijet(maxpartons,maxloops)
      common/to_SumParticles/ SumParticles,icount,ijet,njets

!-----
!  Begin Code
!-----      
!
! Convert to lowercase characters
!
      buff   = code
      sumbuff = sumover
      call lower_case(buff)
      call lower_case(sumbuff)

!
! Make sure code isn't already in use
!
      n = index(buff,null)-1
      if (n .gt. 0) then
         if (mod(index(particle(n),buff(1:n))-1,n) .eq. 0 .and.
     $        index(particle(n),buff(1:n))-1 .ge. 0) then
            print*,'Error particle code already in use',n,' ',buff(1:n)
            return
         endif
      endif
!
! Add to list of particles
!
      iparticle(0,n)=iparticle(0,n)+1
      npart = iparticle(0,n)
      ipart = iparticle(0,1)+iparticle(0,2)+iparticle(0,3)
     &     +iparticle(0,4)+iparticle(0,0)
      iparticle(npart,n) = ipart
      inverse(ipart) = ipart
      if (n .gt. 0) then
         particle(n)((npart-1)*n+1:npart*n)=buff(1:n)
      endif
!
!     Add additional loop option
!
      nloop = SumParticles(0,0)+1
      if (nloop .gt. 10) then
         write(*,*) 'Too many looped codes entered in readmodel.f ' ,
     $        buff(1:n), ' not implemented'
         return
      endif
      SumParticles(0,0)=nloop
      SumParticles(0,nloop) = ipart  !Particle code for loop 
      call getparticle(sumbuff,SumParticles(2,nloop),
     $     SumParticles(1,nloop))      
      
      if (.true.) then
      write(*,*) 'Added loop options for ',buff(1:n),
     $     sumparticles(0,nloop)
      write(*,*) 'Looping over ',sumparticles(1,nloop),' particles'
      do i=1,sumparticles(1,nloop)
         call part_string(sumparticles(i+1,nloop),str1,n)
         write(*,'($a,a)') str1(1:n),' '
      enddo
      write(*,*) ' '
      endif
      end


      subroutine readline(nwords,lun,length,words)
c***********************************************************************
c     Given an open file lun, reads one line and parses
c     it, looking for words separated by one or more spaces.  Returns
c     the words in words(nwords), the number of words found in nwords
c     If no words are found reads the next line.
c     If the end of the file is reached or an error occurs, returns
c     nwords=0.  
c     length is the length of each word
c***********************************************************************
      implicit none
c
c     Constants
c
      include 'params.inc'
c
c     Arguments
c
      integer nwords,lun,length(max_words)
      character*(max_chars) words(max_words)
c
c     Local
c
      character*(maxline) cline

c-----
c  Begin Code
c-----
      nwords=0
      do while(nwords .le. 0) 
         read(lun,'(a80)',end=200,err=200) cline
         call parse_string(nwords,length,words,cline)
      enddo
 200  return
      end

      subroutine parse_string(nwords,length,words,cline)
c**********************************************************************
c     Given cline a buffer of words separated by spaces
c     returns words(nwords) all of the words placed into separate
c     variables, and nwords the number of words found. Also
c     length(i) the length of each word is returned
c**********************************************************************
      implicit none
c
c     Constants
c
      include 'params.inc'
c      integer    max_words   , max_chars   , maxline
c      parameter (max_words=20, max_chars=20, maxline=80)
c      character*1 null
c      parameter  (null=' ')
c
c     Arguments
c
      integer nwords,length(max_words)
      character*(max_chars) words(max_words)
      character*(maxline) cline
c
c     Local
c
      integer i,j

c-----
c  Begin Code
c-----
      nwords=0
      i=1
      do while(i .lt. maxline .and. nwords .lt. max_words)
         do while(cline(i:i) .eq. null .and. i .lt. maxline)
            i=i+1
         enddo
         if (i .lt. maxline) then
            j = index(cline(i:),null)+i-1
            if (j-i .gt. max_chars-2) then
               print*,'Warning word too long, truncating'
               print*,cline(i:j)
               print*,'stored as ',cline(i:i+max_chars-2)
               j = i+max_chars-2
            endif
            nwords=nwords+1
            words(nwords)=cline(i:j)
            length(nwords) = j-i+1
            i = index(cline(i:),null)+i-1
         endif
      enddo
      if (nwords .eq. max_words) then
         print*,'Warning, leaving readline with max number of words',
     &        nwords
      endif
      end

      subroutine setcolorinfo(icolor,ipart,words,length,info,pcolor)
c*************************************************************************     
c     Determine color structure of interactions.  Requires particles
c     are input in the proper order. Particle 1 is either 3 or 8,
c     particle 2 is either 3-bar or 8, and particle 3 is either 8,3.
c     Unless of course all particles are singlets 1.
c*************************************************************************
      implicit none
c
c     Parameters
c
      include 'params.inc'
c
c     Arguments
c
      integer icolor(8), ipart(0:5), length(max_words)  !Inputs
      integer pcolor(4,8)                               !Input
      character*(max_chars) words(max_words)            !Inputs
      integer info(0:4)                                 !Output
c
c     Local
c
      integer i,j,jpart
c
c     Global
c
      character*(max_string) iwave(max_particles),owave(max_particles)
      integer iposx(3,max_particles)
      integer info_p(5,max_particles)
      character*(8) str(3,max_particles)

      character*(4*max_particles) particle(4)
      integer                               charge_c(max_particles)
      integer iparticle(0:max_particles,0:4),inverse(max_particles)
      common/to_model/iparticle,  particle,  inverse, charge_c


      common/to_external/iwave,owave,iposx,info_p,str
c-----
c  Begin Code
c-----
      if (icolor(8) .eq. 3) then  
         info(0)=2              !f(a,b,c)
         info(pcolor(1,8))=1
         info(pcolor(2,8))=2
         info(pcolor(3,8))=3
      elseif (icolor(8) .eq. 2 .and. icolor(3) .eq. 0) then  
         info(0)=6              !f(a,b)  delta (a,b)
         info(pcolor(1,8))=1
         info(pcolor(2,8))=2
c         write(*,*) 'pcolor',pcolor(1,8),pcolor(2,8)
c         stop
      elseif (icolor(8) .eq. 2 .and. icolor(3) .eq. 2) then 
         info(0)=5              !T(a,i,k)T(B,k,j)
         info(pcolor(1,8))=1    !First octet
         info(pcolor(2,8))=2    !second octet
         info(pcolor(1,3))=4    !First triplet
         info(pcolor(2,3))=3    !Second triplet
      elseif (icolor(8) .eq. 1 .and. icolor(3) .eq. 2) then 
         info(0)=1              !T(a,i,j)
         info(pcolor(1,8))=1    !octet    (a)
         info(pcolor(1,3))=2    !triplet is incoming (i)
         info(pcolor(2,3))=3    !triplet is outgoing (j)
         if (icolor(1) .eq. 1) info(pcolor(1,1))=4            
      elseif (icolor(8) .eq. 0 .and. icolor(3) .eq. 2) then
         info(0)=4              !delta(i,j)
c
c        Now be careful to get correct incoming vs outgoing
c
         info(pcolor(1,3))=3
         info(pcolor(2,3))=2
         info(pcolor(1,1))=1    !Singlet is here
         if (icolor(1) .ne. 1) then
c            write(*,*) 'Carefull delta function w/ 2 singlets'
            info(pcolor(2,1))=4 !Second singlet?
         endif
      elseif (icolor(8) .eq. 0 .and. icolor(3) .eq. 0) then
         info(0)=0              !No color factor
         info(1)=1
         info(2)=2
         info(3)=3
         info(4)=4
      elseif (icolor(8) .eq. 4) then  
         info(0)=3              !f(a,b,c)f(a,b,c)
         info(1)=1
         info(2)=2
         info(3)=3
         info(4)=4
      elseif (icolor(8) .eq. 2 .and. icolor(3) .eq. 2) then
         info(0)=5              !T[i,j,a,b]
         info(pcolor(1,3))=1
         info(pcolor(2,3))=2
         info(pcolor(1,8))=3
         info(pcolor(2,8))=4
      else
         write(*,'(a,8i4)') 'Warning unknown color factor',
     &        (icolor(j),j=1,8)
         write(*,'(a,3a)')'Particles: ',
     &        (words(i)(1:length(i)),i=1,3)
      endif
c      write(*,'(a,3a)')'Setting Color Info Particles: ',
c     &     (words(i)(1:length(i)),i=1,3)
c
c      write(*,*) 'Color factor',(info(i),i=0,3)
      end


      subroutine set_coupling(type,icoup)
c**************************************************************************
c     Routine to determine which coupling interaction contains. If no match
c     is found, a new coupling is defined
c**************************************************************************
      implicit none
c
c     Constants
c
      include 'params.inc'
c
c     Arguments
c
      character*(*) type
      integer icoup(0:max_coup)
c
c     Local
c
      integer i
c
c     Global
c
      character*(10)            coup_name(max_coup)
      integer             ncoups
      common/to_couplings/ncoups,coup_name

      data ncoups/0/
c-----
c  Begin Code
c-----
      i=1
      do while (.true.)
         if (i .gt. ncoups) then
            if (i .gt. max_coup) then
               write(*,*) 'Exceeded maximum number of couplings!'
               write(*,*) 'Change max_coup in params.inc'
               stop
            endif
            write(*,*) 'Adding new coupling ',type,i
            coup_name(i)=type
            ncoups=i
         endif
         if (type .eq. coup_name(i)) then
            icoup(i)=icoup(i)+1
            return
         endif
         i=i+1
      enddo
      end

      subroutine sort(array,n)
      implicit none

! Arguments

      integer n
      integer array(n)

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
               done = .false.
            end if
         end do
         if (done) return
      end do
      end 

