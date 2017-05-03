!-----------------------
! madgraph - a Feynman Diagram package by Tim Stelzer and Bill Long 
! (c) 1993
!
! Filename: particles.f
!-----------------------
      Subroutine AddSMParticles
c***********************************************************************
c     Add standard Model Particles, quarks, leptons, bosons
c     Variable info contains the following
c        info(1) = color factors  either 1,3 or 8
c        info(2) = nhelicities    either 1,2 or 3
c        info(3) = type           1==fermion 0=boson   
c        info(4) = linetype       1==photon  2=fermion 3=scalar 4=gluon
c        req     = pdg particle id
c***********************************************************************
      implicit none

! Local
      integer info(5),i
      character*80 buffi

c-----
c  Begin Code
c-----      
      print*,'Using default Standard Model particles'

c
c     This must be the first one, it is the unknown particle
      buffi  = ' '
      do i=1,4
         info(i)=0
      enddo
      call addparticle('?? ',buffi,buffi,info,'?? ','??','??','??')
c
c     Add Standard Model Particles  
c            ALL      d,u,c,s,b,t,  d~,u~,c~,s~,b~,t~
c            QED      e+,e-,mu+,mu-,ta+,ta-,A
c            QFD      W+,W-,Z,ve,vm,vt,ve~,vm~,vt~
c            QCD      g
c
c     format for adding Dirac Fermion+antiFermion to particle list
c     call addFermi('name','antiname','mass','code',color,nhel,line_type,req)
c     req: 1->QCD 2->QED 3->QFD

c    SPECIAL  PROTON, JET
      call addFermion('p ','p~ ','ZERO ','ZERO ','p',3,2,2,0)
      call addFermion('j ','j~ ','ZERO ','ZERO ','j',3,2,2,0)

c    QUARKS
      call addFermion('d ','d~ ','ZERO ','ZERO ','d',3,2,2,0)
c      call addFermion('u ','u~ ','UMASS','UWIDTH','u',3,2,2,0)
      call addFermion('u ','u~ ','ZERO ','ZERO ','u',3,2,2,0)
      call addFermion('s ','s~ ','ZERO ','ZERO ','s',3,2,2,0)
c      call addFermion('c ','c~ ','CMASS','CWIDTH','c',3,2,2,0)
      call addFermion('c ','c~ ','ZERO ','ZERO ','c',3,2,2,0)
c      call addFermion('b ','b~ ','ZERO ','ZERO ','b',3,2,2,0)
      call addFermion('b ','b~ ','BMASS','BWIDTH','b',3,2,2,0)
      call addFermion('t ','t~ ','TMASS','TWIDTH','t',3,2,2,0)

      call addFermion('r ','r ','fmass(1)','zero ','r',8,2,2,1)

c    LEPTONS
c      call addFermion('e- ','e+ ','ZERO ','ZERO ','e',1,2,2,2)
      call addFermion('e- ','e+ ','FMASS(1)','ZERO ','e',1,2,2,2)
      call addFermion('mu- ','mu+ ','ZERO ','ZERO ','mu',1,2,2,2)
      call addFermion('ta- ','ta+ ','ZERO ','ZERO ','ta',1,2,2,2)
      call addFermion('ve ','ve~ ','ZERO ','ZERO ','ve',1,2,2,3)
      call addFermion('vm ','vm~ ','ZERO ','ZERO ','vm',1,2,2,3)
      call addFermion('vt ','vt~ ','ZERO ','ZERO ','vt',1,2,2,3)

c
c     format for adding vector bosons to particle list
c     call addboson('name','antiname','mass','code',color,nhel,line_type,req)
c     req: 1->QCD 2->QED 3->QFD 4->GHOST

c    Bosons
      call addboson('g ','g ','ZERO ','ZERO ',' ',8,2,4,1)
      call addboson('a ','a ','ZERO ','ZERO ','A',1,2,1,2)
      call addboson('z ','z ','ZMASS','ZWIDTH','Z',1,3,1,3)
c      call addboson('w+ ','w- ','WMASS','WWIDTH','W',1,3,1,3)
      call addboson('w- ','w+ ','WMASS','WWIDTH','W',1,3,1,3)
      call addscalar('h ','h ','HMASS','HWIDTH','h',1,1,3,3)
c      call addscalar('x+ ','x- ','WMASS','WWIDTH','X+',1,1,3,4)
c      call addscalar('x- ','x+ ','WMASS','WWIDTH','X-',1,1,3,4)
c      call addscalar('x3 ','x3 ','ZMASS','ZWIDTH','X3',1,1,3,4)
      end

      
      
      subroutine addFermion(code,anticode,mass,width,name,
     $     icolor,ihel,iline,ireq)
c***************************************************************************
c     Adds Dirac Fermion both the particle and the antiparticle to the
c     list of active particles.
c***************************************************************************
      implicit none
c
c     Constant
c
      include 'params.inc'
      character*(*) buffp_tail
      parameter (   buffp_tail=',NHEL(????),+1*IC(????),W(1,????))')
      character*(*) buffa_tail
      parameter (   buffa_tail=',NHEL(????),-1*IC(????),W(1,????))')
      character*(*) buffi_head               
      parameter (   buffi_head='IXXXXX(P(0,????),')
      character*(*) buffo_head                
      parameter (   buffo_head='OXXXXX(P(0,????),')
c
c     Arguments
c
      character*(*) code,anticode,mass,width,name
      integer icolor,ihel,iline,ireq
c
c     Local
c
      character*(max_string) siwave,sowave
      integer info(5),npart,jline,kline,np
c
c     Global
c
      character*(4*max_particles) particle(4)
      integer                               charge_c(max_particles)
      integer iparticle(0:max_particles,0:4),inverse(max_particles)
      common/to_model/iparticle,  particle,  inverse, charge_c


      logical         major_exist
      common/to_major/major_exist
!      data  major_exist/.false./

      
c-----
c  Begin Code
c-----

      
      info(1)= icolor
      info(2)= ihel
      info(3)= 1
      info(4)= iline
      info(5)= ireq
      siwave = buffi_head//mass//buffp_tail
      sowave = buffo_head//mass//buffp_tail
      call addparticle(code,siwave,sowave,info,anticode,mass,width,name)
      siwave = buffo_head//mass//buffa_tail
      sowave = buffi_head//mass//buffa_tail
c
c     Check for Majorona type fermion
c
      call getparticle(code,jline,npart)
      if (npart .eq. 1) then
c         call getparticle(anticode,kline,np)
         if (code .eq. anticode) then   !Majorana
            if (.not. major_exist) then
               write(*,*) 'Majorana particles exist',
     $              ' code will run slower.'
               major_exist=.true.
            endif
            info(2) = -2
c            write(*,*) 'Warning Majorana particles may need ',
c     &           'CC checked.',code,anticode
            call addparticle(' ',siwave,sowave,info,code,
     $           mass,width,name)
c            write(*,*) 'Majorona Results:',jline,inverse(jline),
c     &           charge_c(jline),inverse(inverse(jline)),
c     &           charge_c(inverse(jline))
            
         else                                  !Dirac
            info(5)=-info(5)    !Antiparticle has anti code
            call addparticle(anticode,siwave,sowave,info,code,
     $           mass,width,name)
            info(5)=-info(5)                   !Antiparticle has anti code

c
c     Now add charge conjugate fields for Dirac Fermions
c     uses the fact that particle numbers are added sequentially to add
c     the charge-conjugate field. I am defining this field to be a spinor
c     with the same quantum numbers, but with 'ficticious flow' in the opposite
c     direction. See Denner, Eck, Hahn, Kublbeck CERN-TH.6549/92
c
            if (inverse(jline) .ne. jline+1) then
               write(*,*) 'Warning from addparticle:'
               write(*,*) 'Particles not added sequentially',
     &              jline,inverse(jline),code,anticode
            endif
            siwave = buffo_head//mass//buffa_tail
            sowave = buffi_head//mass//buffa_tail

            call addparticle(' ',siwave,sowave,info,' ',
     $           mass,width,name)

            siwave = buffi_head//mass//buffp_tail
            sowave = buffo_head//mass//buffp_tail
            call addparticle(' ',siwave,sowave,info,' ',
     $           mass,width,name)

c            charge_c(jline)   = jline+3        !Hagiwara
c            charge_c(jline+3) = jline
c            charge_c(jline+1) = jline+2
c            charge_c(jline+2) = jline+1
c            inverse(jline+2)  = jline+3
c            inverse(jline+3)  = jline+2            

            charge_c(jline)   = jline+2       !Tim's method
            charge_c(jline+2) = jline
            charge_c(jline+1) = jline+3
            charge_c(jline+3) = jline+1
            inverse(jline+2)  = jline+3
            inverse(jline+3)  = jline+2            
         endif
      else
         print*,'Error Fermion not entered properly',npart,code
      endif
      end
      
      
      
      

      subroutine addBoson(code,anticode,mass,width,name,
     $     icolor,ihel,iline,ireq)
c***************************************************************************
c     Adds vector boson to the
c     list of active particles.
c***************************************************************************
      implicit none
c
c     Constant
c
      include 'params.inc'
      character*(*) buff_head               
      parameter (   buff_head='VXXXXX(P(0,????),')
      character*(*) buffi_tail
      parameter (   buffi_tail=',NHEL(????),-1*IC(????),W(1,????))')
      character*(*) buffo_tail
      parameter (   buffo_tail=',NHEL(????),+1*IC(????),W(1,????))')
c      integer    max_string
c      parameter (max_string=120)
c
c     Arguments
c
      character*(*) code,anticode,mass,width,name
      integer icolor,ihel,iline,ireq
c
c     Local
c
      character*(max_string) siwave,sowave
      integer info(5),npart,jline
c
c     Global
c
      character*(4*max_particles) particle(4)
      integer                               charge_c(max_particles)
      integer iparticle(0:max_particles,0:4),inverse(max_particles)
      common/to_model/iparticle,  particle,  inverse, charge_c
c-----
c  Begin Code
c-----
      info(1)= icolor
      info(2)= ihel
      info(3)= 0   
      info(4)= iline
      info(5)= ireq
      siwave = buff_head//mass//buffi_tail
      sowave = buff_head//mass//buffo_tail
      call addparticle(code,siwave,sowave,info,anticode,mass,width,name)
      call getparticle(code,jline,npart)
      if (npart .eq. 1) then
         if (inverse(jline) .eq. jline) then   !Own anti-particle
         else                                  
            info(5)=-info(5)                   !Antiparticle has anti code
            call addparticle(anticode,siwave,sowave,info,code,
     $           mass,width,name)
            info(5)=-info(5)                   !Antiparticle has anti code
         endif
      else
         print*,'Error Boson not entered properly',npart,code
      endif
      end

      subroutine addscalar(code,anticode,mass,width,name,
     $     icolor,ihel,iline,ireq)
c***************************************************************************
c     Adds vector boson to the
c     list of active particles.
c***************************************************************************
      implicit none
c
c     Constant
c
      include 'params.inc'
      character*(*) buff_head               
      parameter (   buff_head='SXXXXX(P(0,????)')
      character*(*) buffi_tail
      parameter (   buffi_tail=',-1*IC(????),W(1,????))')
      character*(*) buffo_tail
      parameter (   buffo_tail=',+1*IC(????),W(1,????))')
c
c     Arguments
c
      character*(*) code,anticode,mass,width,name
      integer icolor,ihel,iline,ireq
c
c     Local
c
      character*(max_string) siwave,sowave
      integer info(5),jline,npart
c
c     Global
c
      character*(4*max_particles) particle(4)
      integer                               charge_c(max_particles)
      integer iparticle(0:max_particles,0:4),inverse(max_particles)
      common/to_model/iparticle,  particle,  inverse, charge_c
c-----
c  Begin Code
c-----
      info(1)= icolor
      info(2)= ihel
      info(3)= 0   
      info(4)= iline
      info(5)= ireq
      siwave = buff_head//buffi_tail
      sowave = buff_head//buffo_tail
      call addparticle(code,siwave,sowave,info,anticode,mass,width,name)
      call getparticle(code,jline,npart)
      if (npart .eq. 1) then
         if (inverse(jline) .eq. jline) then   !Own anti-particle
         else                                  !Dirac
            info(5)=-info(5)                   !Antiparticle has anti code
            call addparticle(anticode,siwave,sowave,info,code,
     $           mass,width,name)
            info(5)=-info(5)                   !Antiparticle has anti code
         endif
      else
         print*,'Error Scalar not entered properly',npart,code
      endif
      end




c RF 23 Mar. 2006  added the tensor: (This is probably not needed,
c                        because never an external particle.)

      subroutine addtensor(code,anticode,mass,width,name,
     $     icolor,ihel,iline,ireq)
c***************************************************************************
c     Adds tensor to the
c     list of active particles.
c***************************************************************************
      implicit none
c
c     Constant
c
      include 'params.inc'
      character*(*) buff_head               
      parameter (   buff_head='TXXXXX(P(0,????),')
      character*(*) buffi_tail
      parameter (   buffi_tail=',NHEL(????),-1*IC(????),W(1,????))')
      character*(*) buffo_tail
      parameter (   buffo_tail=',NHEL(????),+1*IC(????),W(1,????))')
c
c
c     Arguments
c
      character*(*) code,anticode,mass,width,name
      integer icolor,ihel,iline,ireq
c
c     Local
c
      character*(max_string) siwave,sowave
      integer info(5),jline,npart
c
c     Global
c
      character*(4*max_particles) particle(4)
      integer                               charge_c(max_particles)
      integer iparticle(0:max_particles,0:4),inverse(max_particles)
      common/to_model/iparticle,  particle,  inverse, charge_c
c-----
c  Begin Code
c-----
      info(1)= icolor
      info(2)= ihel
      info(3)= 0   
      info(4)= iline
      info(5)= ireq
c****************************KEK*************************************
      siwave = buff_head//mass//buffi_tail
      sowave = buff_head//mass//buffo_tail
c****************************KEK*************************************
      call addparticle(code,siwave,sowave,info,anticode,mass,width,name)
      call getparticle(code,jline,npart)
      if (npart .eq. 1) then
         if (inverse(jline) .eq. jline) then   !Own anti-particle
         else                                  !Dirac
            info(5)=-info(5)                   !Antiparticle has anti code
            call addparticle(anticode,siwave,sowave,info,code,
     $           mass,width,name)
            info(5)=-info(5)                   !Antiparticle has anti code
         endif
      else
         print*,'Error Tensor not entered properly',npart,code
      endif
      end

cend RF



      subroutine addparticle(code,siwave,sowave,info,anticode,
     $     mass,width,name)
c***************************************************************************
c     subroutine to add another type of particle to MadGraph
c     siwave and sowave must be very similar, such that the positions
c     of the wavefunctions are identical.  ixxxxxx(....) and oxxxxx(...) 
c     work fine.
c     written 1/30/94
c***************************************************************************
      implicit none

! Constants

      include 'params.inc'

! Arguments

      character*(*) code,anticode
      character*(*) name,mass,width
      character*(max_string) siwave,sowave
      integer info(5)

! Local

      integer i,n,npart,ipart,n1,in
      character*(5) buff,inbuff

! Global
      character*(max_string) iwave(max_particles),owave(max_particles)
      integer iposx(3,max_particles)
      integer info_p(5,max_particles)
      character*(8) str(3,max_particles)

      common/to_external/iwave,owave,iposx,info_p,str

      character*(4*max_particles) particle(4)
      integer                               charge_c(max_particles)
      integer iparticle(0:max_particles,0:4),inverse(max_particles)
      common/to_model/iparticle,  particle,  inverse, charge_c

!-----
!  Begin Code
!-----      
!
! Convert to lowercase characters
!
      buff   = code
      inbuff = anticode
      call lower_case(buff)
      call lower_case(inbuff)

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
      if (iparticle(0,0)+iparticle(0,1)+iparticle(0,2)+iparticle(0,3)+
     $     iparticle(0,4) .eq. max_particles) then
         write(*,*) 'Exceeded Maximum number of particles',max_particles
         return
      endif
      iparticle(0,n)=iparticle(0,n)+1
      npart = iparticle(0,n)
      ipart = iparticle(0,1)+iparticle(0,2)+iparticle(0,3)
     &     +iparticle(0,4)+iparticle(0,0)
      iparticle(npart,n) = ipart
      if (n .gt. 0) then
         particle(n)((npart-1)*n+1:npart*n)=buff(1:n)
      endif
      charge_c(ipart) = ipart   !Default to being own charge_c
!
! Check inverse, make charge_c default to inverse
!
      n1 = index(inbuff,null)-1
      if (n1 .gt. 0) then
         in = index(particle(n1),inbuff(1:n1))-1
         if (mod(in,n1) .eq. 0 .and. in .ge. 0) then
            inverse(iparticle(npart,n))    = iparticle(in/n1+1,n1)
            inverse(iparticle(in/n1+1,n1)) = iparticle(npart,n) 
            charge_c(iparticle(npart,n))   = iparticle(in/n1+1,n1)
            charge_c(iparticle(in/n1+1,n1))= iparticle(npart,n) 
c         write(*,*) 'Setting up inverse ',buff,inbuff,
c     &        iparticle(npart,n),iparticle(in/n1+1,n1)
         endif
      endif
!
! Find Position for P,ihel,W
!
      iwave(ipart)      = siwave
      owave(ipart)      = sowave
      do i=1,5
         info_p(i,ipart)=info(i)
      enddo
      str(1,ipart) = name
      str(2,ipart) = mass
      str(3,ipart) = width
c      print*,'Added particle ',str(1,ipart),info_p(5,ipart),ipart,buff
c      print*,' ',n,n1,buff(1:n),inbuff(1:n1)
c      print*,'added particle ',buff(1:n),str(1,ipart)
      iposx(1,ipart) = index(siwave,'P(0,????')+4
      iposx(2,ipart) = index(siwave,'NHEL(????)')+5
      iposx(3,ipart) = index(siwave,'W(1,????')+4
c      write(*,'(i4,a)') ipart,iwave(ipart)
c      write(*,'(i4,a)') ipart,owave(ipart)
      if (iposx(1,ipart) .ne. index(sowave,'P(0,????')+4) then
         print*,'Error adding external particle, P??? '
         print*,siwave
         print*,sowave
      endif
      if (iposx(2,ipart) .ne. index(sowave,'NHEL(????)')+5) then
         print*,'Error adding external particle, NHEL(????) '
         print*,siwave
         print*,sowave
      endif
      if (iposx(3,ipart) .ne. index(sowave,'W(1,????')+4) then
         print*,'Error adding external particle, W??? '
         print*,siwave
         print*,sowave
      endif
      end

      subroutine getparticle(xbuff,iline,nparticles)
c***************************************************************************
c     Parses buff to find what particles are contained and returns
c     those in iline
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

! Local

      integer i,in1,in2,in3,in4
      character*(max_string) buff


! Global

      character*(4*max_particles) particle(4)
      integer                               charge_c(max_particles)
      integer iparticle(0:max_particles,0:4),inverse(max_particles)
      common/to_model/iparticle,  particle,  inverse, charge_c


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
      nparticles=0
c      do i=1,4
c         write(*,'(i2,a75)') i,particle(i)
c      enddo
      i=1
      do while(i .le. len(xbuff))
         in4 = index(particle(4),buff(i:i+3))-1
         in3 = index(particle(3),buff(i:i+2))-1
         in2 = index(particle(2),buff(i:i+1))-1
         in1 = index(particle(1),buff(i:i+0))-1
         if (mod(in4,4) .eq. 0) then
            nparticles=nparticles+1
            iline(nparticles)= iparticle(in4/4+1,4)
c            print*,'Got one',nparticles,' ',buff(i:i+3)
            i=i+4
         elseif (mod(in3,3) .eq. 0) then
            nparticles=nparticles+1
            iline(nparticles)= iparticle(in3/3+1,3)
c            print*,'Got one',nparticles,' ',buff(i:i+2)
            i=i+3
         elseif (mod(in2,2) .eq. 0) then
            nparticles=nparticles+1
            iline(nparticles)= iparticle(in2/2+1,2)
c            print*,'Got one',nparticles,' ',xbuff(i:i+1)
            i=i+2
         elseif (mod(in1,1) .eq. 0 .and. in1 .ge. 0) then
            nparticles=nparticles+1
            iline(nparticles)= iparticle(in1/1+1,1)
c            print*,'Got one',nparticles,' ',xbuff(i:i+0)
            i=i+1
         else
            i=i+1
         endif
c         if (nparticles .gt. maxlines) then
c            write(*,*) 'Error too many particles',nparticles
c            i=len(xbuff)+5
c         endif
      enddo
      end



      subroutine write_external(iline)
c************************************************************************
c     Write External Wave Functions
c************************************************************************
      implicit none
! Constants
      include 'params.inc'
      include 'onia.inc'
c      integer    max_particles       
c      parameter (max_particles=2**7-1)
c      integer    max_string
c      parameter (max_string=120)

! Arguments
      integer iline(0:10)

! Local
      integer i,ipart
      character*4 cnum
      character*(max_string) buff
     
! Global
      character*(max_string) iwave(max_particles),owave(max_particles)
      character*(8) str(3,max_particles)
      integer iposx(3,max_particles)
      integer info_p(5,max_particles)

      common/to_external/iwave,owave,iposx,info_p,str
      integer         nincoming,nincfirst
      common/to_proc2/nincoming,nincfirst

      character*(4*max_particles) particle(4)
      integer                               charge_c(max_particles)
      integer iparticle(0:max_particles,0:4),inverse(max_particles)
      common/to_model/iparticle,  particle,  inverse, charge_c

      logical             first_time
      common/to_firstproc/first_time

      logical DipolSub
      data DipolSub/.false./

!-----
!  Begin Code
!-----      
c      write(*,*) 'Writing to pmass'
      if (first_time.and..not.DipolSub) then
         open (unit=itnum,file='pmass.inc',status='unknown')
         if (onium) then
           open(unit=24,file='qmass.inc',status='unknown')
         endif
      endif
      do i=1,iline(0)
         if (first_time.and..not.onium) then
            if(str(2,iline(i)).eq.'ZERO')then
               write(itnum,'(6x,a,i3,2a)')'pmass(',i,') = ',
     $              str(2,iline(i))
            else
c     Need to include abs() for negative masses in SUSY
               write(itnum,'(6x,a,i3,3a)')'pmass(',i,') = abs(',
     $              str(2,iline(i))(1:len_trim(str(2,iline(i)))),')'
            endif
         endif
c
c  Onium stuff
c
         if (first_time.and.onium) then
            if (i.lt.(iline(0)-1)) then
              if(str(2,iline(i)).eq.'ZERO')then
                 write(itnum,'(6x,a,i3,2a)')'pmass(',i,') = ',
     $                str(2,iline(i))
              else
c     Need to include abs() for negative masses in SUSY
                 write(itnum,'(6x,a,i3,3a)')'pmass(',i,') = abs(',
     $                str(2,iline(i))(1:len_trim(str(2,iline(i)))),')'
              endif
            endif
            if (i.eq.(iline(0)-1)) then
                 write(itnum,'(6x,a,i3,5a)')'pmass(',i,') = abs(',
     $           str(2,iline(i))(1:len_trim(str(2,iline(i)))),')+abs(',
     $           str(2,iline(i+1))(1:len_trim(str(2,iline(i+1)))),')'
            endif
            if (i.ge.(iline(0)-1)) then
              write(24,'(6x,a,i3,3a)')'qmass(',i-iline(0)+2,') = abs(',
     $              str(2,iline(i))(1:len_trim(str(2,iline(i)))),')'
            endif
         endif


         if (i .le. nincoming) then
            ipart=inverse(iline(i))
            buff = iwave(ipart)
         else
            ipart = iline(i)
            buff = owave(ipart)
         endif
         call makestringamp(i,cnum)
         buff(iposx(1,ipart):iposx(1,ipart)+3)=cnum
         buff(iposx(3,ipart)-10:iposx(3,ipart)-7)=cnum
         if (iposx(2,ipart) .ge. 6) then
            buff(iposx(2,ipart):iposx(2,ipart)+3)=cnum
         else
c     print*,'Sorry no helicity here',iposx(2,ipart)
         endif
         call optcheck(buff,i,iposx(3,ipart))
      enddo
      if (first_time.and..not.DipolSub) close(unit=itnum)
      if (first_time.and.onium) close(unit=24)
      if (first_time.and..not.DipolSub) then
         open (unit=itnum,file='nexternal.inc',status='unknown')
         write(itnum,'(6x,a)') 'integer    nexternal'      
         if (onium) then
       write(itnum,'(6x,a,i2,a)')'parameter (nexternal= ',iline(0)-1,')'
         else
         write(itnum,'(6x,a,i2,a)')'parameter (nexternal= ',iline(0),')'
         endif
         write(itnum,'(6x,a)') 'integer    nincoming'      
         write(itnum,'(6x,a,i2,a)')'parameter (nincoming= ',
     $      nincoming,')'
         close(itnum)
         open (unit=itnum,file='dipole.inc',status='unknown')
         close(itnum)
         open (unit=itnum,file='ncombs.inc',status='unknown')
         write(itnum,'(6x,a)') 'integer    n_max_cl'      
         write(itnum,'(6x,a,i5,a)')'parameter (n_max_cl= ',
     &        ishft(1,iline(0)+1),')'
         close(itnum)
c         first_time=.false.
      endif
      end


      subroutine part_string(i,str,nlen)
c************************************************************************
c     Given particle i, returns the appropriate string for it
c     and its length
c************************************************************************
      implicit none

! Constants

      include 'params.inc'
c      integer    max_particles
c      parameter (max_particles=2**7-1)
      
! Arguments

      character*4 str
      integer     i,nlen

! Local
      
      integer npos

! Global

      character*(4*max_particles) particle(4)
      integer                               charge_c(max_particles)
      integer iparticle(0:max_particles,0:4),inverse(max_particles)
      common/to_model/iparticle,  particle,  inverse, charge_c

!-----
!  Begin Code
!-----      

      nlen=1
      npos=1
      do while (iparticle(npos,nlen) .ne. i .and. nlen .le. 4)
         npos=npos+1
         if (npos .gt. iparticle(0,nlen)) then
            npos = 1
            nlen = nlen+1
         endif
      enddo
      if (nlen .gt. 4) then
         nlen=4
         str='????'
      else
         str = particle(nlen)(nlen*(npos-1)+1:nlen*(npos))
      endif
c      print*,'found string',i,str(1:nlen)
      end

      Logical function Majarana(ipart)
c***********************************************************************
c     Returns .true. if particle is majarana, false if
c     it is not.
c***********************************************************************
      implicit none

! Constants

      include 'params.inc'
      
! Arguments

      integer ipart

! Global

      character*(4*max_particles) particle(4)
      integer                               charge_c(max_particles)
      integer iparticle(0:max_particles,0:4),inverse(max_particles)
      common/to_model/iparticle,  particle,  inverse, charge_c

      character*(8) str(3,max_particles)
      character*(max_string) iwave(max_particles),owave(max_particles)
      integer info_p(5,max_particles),iposx(3,max_particles)
      common/to_external/iwave,owave,iposx,info_p,str

!-----
!  Begin Code
!-----
      majarana = .false.
      if (info_p(3,ipart) .eq. 1)   then           !Fermion
         if (info_p(2,ipart)          .eq. -2 .or.
     $       info_p(2,inverse(ipart)) .eq. -2)      then
            majarana=.true.
         endif
      endif
      end
