      subroutine check3v(l1,l2,l3,n,fail,more,oneit,ngraphs,color)
c***************************************************************************
c     Subroutine to check if vertex exists with lines l1 l2 and l3
c     If one does than pass is true, and more is set if another possible
c     vertex exists.  The line is than written to the screen and any
c     unknown particles are given the appropriate code
c***************************************************************************
c
c     Constants
c
      include 'params.inc'
c
c     Arguments
c
      integer l1,l2,l3,n,ngraphs
      logical fail,more,oneit
      integer color(0:2*maxlines,0:maxfactors,0:maxterms)
c
c     Local
c
      integer iorder(3),ll(3),li(3),i,iorder2(3),nli(3)
      character*(max_string) buff
      logical chargc, got_fermion, check_cc
c
c     Global
c
      integer   coup3(0:max_coup,maxv3)
      integer                         ipos3(4,maxv3), info3(0:4,maxv3)
      character*(max_string) v3wave(maxv3)
      common/to_internal/    v3wave,  ipos3         , info3,coup3

      integer        iline(-maxlines:maxlines),idir(-maxlines:maxlines)
      integer        this_coup(max_coup) ,goal_coup(max_coup)
      common/to_proc/iline,idir,this_coup,goal_coup

      integer wnum(-maxlines:maxlines),wcount,wlen(maxgraphs*maxlines)
      integer alen(maxgraphs)
      common/toopt/wcount,wnum,wlen,alen

      character*(max_string) wstrings(maxgraphs*maxlines)
      character*(max_string) astrings(maxgraphs)
      common/tocode/wstrings,astrings

      logical                cc(maxv3),pass_cc(maxv3)
      common /to_chargeconjg/cc       ,pass_cc

      character*(10)            coup_name(max_coup)
      integer             ncoups
      common/to_couplings/ncoups,coup_name

      logical          this_block(0:max_blocks), need_block(max_blocks)
      common/to_blocks/this_block              , need_block

      character*(max_string) iwave(max_particles),owave(max_particles)
      integer iposx(3,max_particles)
      integer info_p(5,max_particles)
      character*(8) str(3,max_particles)
      common/to_external/iwave,owave,iposx,info_p,str

      character*(4*max_particles) particle(4)
      integer                               charge_c(max_particles)
      integer iparticle(0:max_particles,0:4),inverse(max_particles)
      common/to_model/iparticle,  particle,  inverse, charge_c

c
c     External 
c
      integer v3

      data check_cc /.true./
c-----
c  Begin Code
c-----
      oneit = .true.
      more  = .false.
      ll(1)=iline(l1)
      ll(2)=iline(l2)
      ll(3)=iline(l3)
      li(1)= l1
      li(2)= l2
      li(3)= l3
      do i=1,3
         iorder(i) =i
         iorder2(i)=ll(i)
         nli(i) = li(i)
      enddo
      call sort2(ll,iorder,3)
      call sort2(iorder2,nli,3)
      iplace = v3(ll(1),ll(2),ll(3),n)
c      write(*,*) 'Checking vertex',ll(1),ll(2),ll(3),iplace
      if (iplace .ne. 0) then
c         write(*,'(a,5i6)') 'Found vert',iplace,(ll(i),i=1,3)
c     &        ,ipos3(4,iplace)
         fail = .false.
         buff = v3wave(iplace)
         do i=2,3
            call makestring(li(iorder(i)),
     $        buff(ipos3(i,iplace):ipos3(i,iplace)+3))
         enddo
         if (ll(1) .eq. 1) then           !Calculates Wavefuntion
            iline(li(iorder(1))) = ipos3(4,iplace)
            call optcheck(buff,li(iorder(1)),ipos3(1,iplace))
         else                             !Calculates Graph Amplitude
            oneit = .false.
            call makestring(li(iorder(1)),
     $        buff(ipos3(1,iplace):ipos3(1,iplace)+3))
c
c     Might need this trap for maxgraphs since I can exceed it here by accident
c
c            if (ngraphs .lt. maxgraphs) then
               ngraphs=ngraphs+1
c            else
c               write(*,*) 'Exceeded maxgraphs',maxgraphs
c            endif
            call makestringamp(ngraphs,
     $           buff(ipos3(4,iplace):ipos3(4,iplace)+3))
            buff(ipos3(4,iplace)+4:ipos3(4,iplace)+5)='))'
            astrings(ngraphs)=buff(:ipos3(4,iplace)+5)
            alen(ngraphs) = ipos3(4,iplace)+5
            ngraphs=ngraphs-1
         endif
         if (v3(ll(1),ll(2),ll(3),n+1) .ne. 0) then
            more = .true.
         endif
         do i=1,3
            iorder(i) = info3(i,iplace)
         enddo
c         write(*,*) 'info order  ',(info3(i,iplace), i=1,3)
c         write(*,*) 'Original nli',(nli(i),i=1,3)
         call sort2(iorder,nli,3)   !This is suppose to get things in order
c         write(*,*) 'Swapped  nli',(nli(i),i=1,3)

c
c     Not using charge conjugate for color at the moment.
c
c         if (chargc) info3(0,iplace)=-info3(0,iplace)!CC the color factors
c         write(*,*) 'Using Vertex ',iplace
         call createcolor(nli,info3(0,iplace),3,color)
c         if (chargc) info3(0,iplace)=-info3(0,iplace)!Return to original value
         this_block(coup3(0,iplace))=.true.           !Which common blocks need
c         write(*,*) 'Used vertex',iplace,coup3(0,iplace)
c         write(*,'(a,5i4)') 'Goal  ',(goal_coup(i),i=1,5)
c         write(*,'(a,5i4)') 'Total ',(this_coup(i),i=1,5)
c         write(*,'(a,6i4)') 'Add   ',(coup3(i,iplace),i=1,5), iplace
         do i=1,ncoups
            this_coup(i)=this_coup(i)+coup3(i,iplace)
            if (this_coup(i) .gt. goal_coup(i)) then
               fail=.true.
c               write(*,*) 'Failed vert3.f', goal_coup(i),this_coup(i)
c     &              , coup3(i,iplace)
            endif
         enddo
      else
         fail = .true.
c         print*,'sorry no such vertex',ll(1),ll(2),ll(3)
         
         if (n .eq. 1 .and. check_cc) then
c            write(*,*) 'Checking cc in vert3'
c
c                  Now we'll check to see if vertex would have worked using 
c                  charge conjugate for one of the fermion lines.
c
            got_fermion = .false.
            i = 1
            do while (.not. got_fermion .and. i .le. 3)
               if (info_p(2,ll(i)) .eq. 1) then         !This is fermion
                  got_fermion = .true.
               else
                  i=i+1
               endif
            enddo
            if (got_fermion) then
               ll(i)= charge_c(ll(i))
               call sort2(ll,iorder,3)
               if (v3(ll(1),ll(2),ll(3),n) .gt. 0) then
                  check_cc=.false.
c                  write(*,*) 'Need to use CC sum'
               endif
            endif
         endif
      endif           
      end

      subroutine addFFV(partbuff,coup,info,icoup)
c**************************************************************************
c     Sets up the IOV vertex with all the permutations
c     info is the color info for the IOV configuration
c     icoup is the coupling constant
c*************************************************************************
      implicit none

! Constants
      include 'params.inc'

c      integer    max_string
c      parameter (max_string=120)
c      integer    max_coup
c      parameter (max_coup=5)
c      integer    max_particles       
c      parameter (max_particles=2**7-1)
      character*(*) IOV_head                          
      parameter    (IOV_head='IOVXXX(W(1,1???),W(1,2???),W(1,3???),')
      character*(*) IOV_tail                          
      parameter    (IOV_tail=',AMP(????))')
      character*(*) JIO_head                   
      parameter    (JIO_head='JIOXXX(W(1,1???),W(1,2???),')
      character*(*) JIO_tail                   
      parameter    (JIO_tail=',W(1,3???))')
      character*(*) FVO_head                
      parameter    (FVO_head='FVOXXX(W(1,2???),W(1,3???),')
      character*(*) FVO_tail
      parameter    (FVO_tail=',W(1,1???))')
      character*(*) FVI_head 
      parameter    (FVI_head='FVIXXX(W(1,1???),W(1,3???),')
      character*(*) FVI_tail
      parameter    (FVI_tail=',W(1,2???))')
      character*(*) MVI_head 
      parameter    (MVI_head='MVIXXX(W(1,1???),W(1,3???),')
      character*(*) MVO_head                
      parameter    (MVO_head='MVOXXX(W(1,2???),W(1,3???),')
      
! Arguments

      character*(*) partbuff,coup
      integer info(0:4),icoup(0:max_coup)

! Local
      
      integer ipart(0:4),ipart2(0:4),i
      character*(max_string) buff
      character*8 fmass,fwidth,vmass,vwidth

! External

      logical majarana

! Global

      character*(4*max_particles) particle(4)
      integer                               charge_c(max_particles)
      integer iparticle(0:max_particles,0:4),inverse(max_particles)
      common/to_model/iparticle,  particle,  inverse, charge_c

      character*(max_string) iwave(max_particles),owave(max_particles)
      character*(8) str(3,max_particles)
      integer info_p(5,max_particles),iposx(3,max_particles)
      common/to_external/iwave,owave,iposx,info_p,str

c-----
c  Begin Code
c-----
c      print*,'adding: ',partbuff
      call getparticle(partbuff,ipart(1),ipart(0)) !parse particle string

      do i=0,4
         ipart2(i)=ipart(i)                        
      enddo
      buff = IOV_head//coup//IOV_tail
      call add3vertex(ipart2,buff,info,icoup)      !Send the IOV version

      do i=0,4
         ipart2(i)=ipart(i)
      enddo
      ipart2(3)=1                                  !3rd particle needed V
      ipart2(4)=inverse(ipart(3))                  !get antiparticle
      vmass  = str(2,ipart(3))                     !Mass of Vector Boson
      vwidth = str(3,ipart(3))                     !Width of Vector Boson
      buff = JIO_head//coup//','//vmass//','//vwidth//JIO_tail
      call add3vertex(ipart2,buff,info,icoup)      !Send the JIOXXX version

      do i=0,4
         ipart2(i)=ipart(i)
      enddo
      ipart2(2) = 1                                !2nd particle needed O
      ipart2(4) = inverse(ipart(2))                !get antiparticle
      fmass  = str(2,ipart(2))                     !Mass of Fermion
      fwidth = str(3,ipart(2))                     !Width of fermion
      buff = FVI_head//coup//','//fmass//','//fwidth//FVI_tail
      call add3vertex(ipart2,buff,info,icoup)      !Send the FVIXXX version

      do i=0,4
         ipart2(i)=ipart(i)
      enddo
      if (ipart(1) .ne. ipart(2)) then             !Only if two particles diff
         ipart2(1) = 1                             !1st particle needed I
         ipart2(4) = inverse(ipart(1))             !get antiparticle
         fmass  = str(2,ipart(1))                  !Mass of Fermion
         fwidth = str(3,ipart(1))                  !Width of fermion
         buff = FVO_head//coup//','//fmass//','//fwidth//FVO_tail
         call add3vertex(ipart2,buff,info,icoup)   !Send the FVOXXX version
      endif
      end

      subroutine addFFS(partbuff,coup,info,icoup)
c**************************************************************************
c     Sets up the FFS vertex with all the permutations (q~qh) is order
c     info is the color info for the IOV configuration
c     icoup is the coupling constant
c*************************************************************************
      implicit none

! Constants

      include 'params.inc'
c      integer    max_string
c      parameter (max_string=120)
c      integer    max_coup
c      parameter (max_coup=5)
c      integer    max_particles       
c      parameter (max_particles=2**7-1)
      character*(*) IOS_head                          
      parameter    (IOS_head='IOSXXX(W(1,1???),W(1,2???),W(1,3???),')
      character*(*) IOS_tail                          
      parameter    (IOS_tail=',AMP(????))')
      character*(*) HIO_head
      parameter    (HIO_head='HIOXXX(W(1,1???),W(1,2???),')
      character*(*) HIO_tail
      parameter    (HIO_tail=',W(1,3???))')
      character*(*) FSO_head
      parameter    (FSO_head='FSOXXX(W(1,2???),W(1,3???),')
      character*(*) FSO_tail
      parameter    (FSO_tail=',W(1,1???))')
      character*(*) FSI_head
      parameter    (FSI_head='FSIXXX(W(1,1???),W(1,3???),')
      character*(*) FSI_tail
      parameter    (FSI_tail=',W(1,2???))')
      
! Arguments

      character*(*) partbuff,coup
      integer info(0:4),icoup(0:max_coup)

! Local
      
      integer ipart(0:4),ipart2(0:4),i
      character*(max_string) buff
      character*8 smass,swidth,fmass,fwidth


! Global

      character*(4*max_particles) particle(4)
      integer                               charge_c(max_particles)
      integer iparticle(0:max_particles,0:4),inverse(max_particles)
      common/to_model/iparticle,  particle,  inverse, charge_c

      character*(max_string) iwave(max_particles),owave(max_particles)
      character*(8) str(3,max_particles)
      integer info_p(5,max_particles),iposx(3,max_particles)
      common/to_external/iwave,owave,iposx,info_p,str

c-----
c  Begin Code
c-----
c      print*,'adding: ',partbuff
      call getparticle(partbuff,ipart(1),ipart(0)) !parse particle string
      do i=0,4
         ipart2(i)=ipart(i)
      enddo
      smass  = str(2,ipart(3))
      swidth = str(3,ipart(3))
      buff = IOS_head//coup//IOS_tail
      call add3vertex(ipart2,buff,info,icoup)  !Send the IOS version

      do i=0,4
         ipart2(i)=ipart(i)
      enddo
      ipart2(3)=1
      ipart2(4)=inverse(ipart(3))
      buff = HIO_head//coup//','//smass//','//swidth//HIO_tail
      call add3vertex(ipart2,buff,info,icoup)

      do i=0,4
         ipart2(i)=ipart(i)
      enddo
      ipart2(2) = 1
      ipart2(4) = inverse(ipart(2))
      fmass  = str(2,ipart(2))
      fwidth = str(3,ipart(2))
      buff = FSI_head//coup//','//fmass//','//fwidth//FSI_tail
      call add3vertex(ipart2,buff,info,icoup)

      do i=0,4
         ipart2(i)=ipart(i)
      enddo
      if (ipart(1) .ne. ipart(2)) then
         ipart2(1) = 1
         ipart2(4) = inverse(ipart(1))
         fmass  = str(2,ipart(1))
         fwidth = str(3,ipart(1))
         buff = FSO_head//coup//','//fmass//','//fwidth//FSO_tail
         call add3vertex(ipart2,buff,info,icoup)
      endif
      end

      subroutine addVVS(partbuff,coup,info,icoup)
c**************************************************************************
c     Sets up the FFS vertex with all the permutations (vvh) is order
c     info is the color info for the IOV configuration
c     icoup is the coupling constant
c*************************************************************************
      implicit none

! Constants

      include 'params.inc'
c      integer    max_string
c      parameter (max_string=120)
c      integer    max_coup
c      parameter (max_coup=5)
c      integer    max_particles       
c      parameter (max_particles=2**7-1)
      character*(*) VVS_head                          
      parameter    (VVS_head='VVSXXX(W(1,1???),W(1,2???),W(1,3???),')
      character*(*) VVS_tail                          
      parameter    (VVS_tail=',AMP(????))')
      character*(*) HVV_head
      parameter    (HVV_head='HVVXXX(W(1,1???),W(1,2???),')
      character*(*) HVV_tail
      parameter    (HVV_tail=',W(1,3???))')
      character*(*) JVS_head1
      parameter    (JVS_head1='JVSXXX(W(1,1???),W(1,3???),')
      character*(*) JVS_tail1
      parameter    (JVS_tail1=',W(1,2???))')
      character*(*) JVS_head2
      parameter    (JVS_head2='JVSXXX(W(1,2???),W(1,3???),')
      character*(*) JVS_tail2
      parameter    (JVS_tail2=',W(1,1???))')
      
! Arguments

      character*(*) partbuff,coup
      integer info(0:4),icoup(0:max_coup)

! Local
      
      integer ipart(0:4),ipart2(0:4),i
      character*(max_string) buff
      character*8 smass,swidth,vmass,vwidth


! Global

      character*(4*max_particles) particle(4)
      integer                               charge_c(max_particles)
      integer iparticle(0:max_particles,0:4),inverse(max_particles)
      common/to_model/iparticle,  particle,  inverse, charge_c

      character*(max_string) iwave(max_particles),owave(max_particles)
      character*(8) str(3,max_particles)
      integer info_p(5,max_particles),iposx(3,max_particles)
      common/to_external/iwave,owave,iposx,info_p,str

c-----
c  Begin Code
c-----
c      print*,'adding: ',partbuff
      call getparticle(partbuff,ipart(1),ipart(0)) !parse particle string
      do i=0,4
         ipart2(i)=ipart(i)
      enddo
      smass  = str(2,ipart(3))
      swidth = str(3,ipart(3))
      buff = VVS_head//coup//VVS_tail
      call add3vertex(ipart2,buff,info,icoup)  !Send the IOS version

      do i=0,4
         ipart2(i)=ipart(i)
      enddo
      ipart2(3)=1
      ipart2(4)=inverse(ipart(3))
      buff = HVV_head//coup//','//smass//','//swidth//HVV_tail
      call add3vertex(ipart2,buff,info,icoup)

      do i=0,4
         ipart2(i)=ipart(i)
      enddo
      ipart2(2) = 1
      ipart2(4) = inverse(ipart(2))
      vmass  = str(2,ipart(2))
      vwidth = str(3,ipart(2))
      buff = JVS_head1//coup//','//vmass//','//vwidth//JVS_tail1
      call add3vertex(ipart2,buff,info,icoup)

      do i=0,4
         ipart2(i)=ipart(i)
      enddo
      if (ipart(1) .ne. ipart(2)) then
         ipart2(1) = 1
         ipart2(4) = inverse(ipart(1))
         vmass  = str(2,ipart(1))
         vwidth = str(3,ipart(1))
         buff = JVS_head2//coup//','//vmass//','//vwidth//JVS_tail2
         call add3vertex(ipart2,buff,info,icoup)
      endif
      end

      subroutine addVSS(partbuff,coup,info,icoup)
c**************************************************************************
c     Sets up the VSS vertex with all the permutations (vhh) is order
c     info is the color info for the IOV configuration
c     icoup is the coupling constant
c*************************************************************************
      implicit none

! Constants

      include 'params.inc'
c      integer    max_string
c      parameter (max_string=120)
c      integer    max_coup
c      parameter (max_coup=5)
c      integer    max_particles       
c      parameter (max_particles=2**7-1)
      character*(*) VSS_head                          
      parameter    (VSS_head='VSSXXX(W(1,1???),W(1,2???),W(1,3???),')
      character*(*) VSS_tail                          
      parameter    (VSS_tail=',AMP(????))')
      character*(*) HVS_head1
      parameter    (HVS_head1='HVSXXX(W(1,1???),W(1,2???),')
      character*(*) HVS_tail1
      parameter    (HVS_tail1=',W(1,3???)')
      character*(*) HVS_head2
      parameter    (HVS_head2='HVSXXX(W(1,1???),W(1,3???),')
      character*(*) HVS_tail2
      parameter    (HVS_tail2=',W(1,2???))')
      character*(*) JSS_head 
      parameter    (JSS_head='JSSXXX(W(1,2???),W(1,3???),')
      character*(*) JSS_tail
      parameter    (JSS_tail=',W(1,1???))')
      
! Arguments

      character*(*) partbuff,coup
      integer info(0:4),icoup(0:max_coup)

! Local
      
      integer ipart(0:4),ipart2(0:4),i
      character*(max_string) buff
      character*8 smass,swidth,vmass,vwidth


! Global

      character*(4*max_particles) particle(4)
      integer                               charge_c(max_particles)
      integer iparticle(0:max_particles,0:4),inverse(max_particles)
      common/to_model/iparticle,  particle,  inverse, charge_c

      character*(max_string) iwave(max_particles),owave(max_particles)
      character*(8) str(3,max_particles)
      integer info_p(5,max_particles),iposx(3,max_particles)
      common/to_external/iwave,owave,iposx,info_p,str

c-----
c  Begin Code
c-----
c      print*,'adding: ',partbuff
      call getparticle(partbuff,ipart(1),ipart(0)) !parse particle string
      do i=0,4
         ipart2(i)=ipart(i)
      enddo
      smass  = str(2,ipart(3))
      swidth = str(3,ipart(3))
      buff = VSS_head//coup//VSS_tail
      call add3vertex(ipart2,buff,info,icoup)  !Send the HVS version

      do i=0,4
         ipart2(i)=ipart(i)
      enddo
      ipart2(3)=1
      ipart2(4)=inverse(ipart(3))
      smass  = str(2,ipart(3))
      swidth = str(3,ipart(3))
      buff = HVS_head1//coup//','//smass//','//swidth//HVS_tail1
      call add3vertex(ipart2,buff,info,icoup)  !Send the HVS version

      do i=0,4
         ipart2(i)=ipart(i)
      enddo
c
c     Note not symmectric if S1<->S2 need - sign, add here automatically
c
      if (ipart(2) .ne. ipart(3)) then   
         smass  = str(2,ipart(2))
         swidth = str(3,ipart(2))
         ipart2(2)=1
         ipart2(4)=inverse(ipart(2))
         buff = HVS_head2//'-'//coup//','//smass//','//swidth//HVS_tail2
         call add3vertex(ipart2,buff,info,icoup)
      endif

      do i=0,4
         ipart2(i)=ipart(i)
      enddo
      ipart2(1) = 1
      ipart2(4) = inverse(ipart(1))
      vmass  = str(2,ipart(1))
      vwidth = str(3,ipart(1))
      buff = JSS_head//coup//','//vmass//','//vwidth//JSS_tail
      call add3vertex(ipart2,buff,info,icoup)

      end

      subroutine addSSS(partbuff,coup,info,icoup)
c**************************************************************************
c     Sets up the SSS vertex with all the permutations (hhh) is order
c     info is the color info for the IOV configuration
c     icoup is the coupling constant
c*************************************************************************
      implicit none

! Constants

      include 'params.inc'
c      integer    max_string
c      parameter (max_string=120)
c      integer    max_coup
c      parameter (max_coup=5)
c      integer    max_particles       
c      parameter (max_particles=2**7-1)
      character*(*) SSS_head                          
      parameter    (SSS_head='SSSXXX(W(1,1???),W(1,2???),W(1,3???),')
      character*(*) SSS_tail                          
      parameter    (SSS_tail=',AMP(????))')
      character*(*) HSS_head1
      parameter    (HSS_head1='HSSXXX(W(1,1???),W(1,2???),')
      character*(*) HSS_tail1
      parameter    (HSS_tail1=',W(1,3???))')
      character*(*) HSS_head2
      parameter    (HSS_head2='HSSXXX(W(1,1???),W(1,3???),')
      character*(*) HSS_tail2
      parameter    (HSS_tail2=',W(1,2???))')
      character*(*) HSS_head3
      parameter    (HSS_head3='HSSXXX(W(1,2???),W(1,3???),')
      character*(*) HSS_tail3
      parameter    (HSS_tail3=',W(1,1???))')
      
! Arguments

      character*(*) partbuff,coup
      integer info(0:4),icoup(0:max_coup)

! Local
      
      integer ipart(0:4),ipart2(0:4),i
      character*(max_string) buff
      character*8 smass,swidth


! Global

      character*(4*max_particles) particle(4)
      integer                               charge_c(max_particles)
      integer iparticle(0:max_particles,0:4),inverse(max_particles)
      common/to_model/iparticle,  particle,  inverse, charge_c

      character*(max_string) iwave(max_particles),owave(max_particles)
      character*(8) str(3,max_particles)
      integer info_p(5,max_particles),iposx(3,max_particles)
      common/to_external/iwave,owave,iposx,info_p,str

c-----
c  Begin Code
c-----
c      print*,'adding: ',partbuff
      call getparticle(partbuff,ipart(1),ipart(0)) !parse particle string
      do i=0,4
         ipart2(i)=ipart(i)
      enddo
      buff = SSS_head//coup//SSS_tail
      call add3vertex(ipart2,buff,info,icoup)  !Send the SSS version
c
c     Calculate scalar 1
c
      do i=0,4
         ipart2(i)=ipart(i)
      enddo
      smass  = str(2,ipart(1))
      swidth = str(3,ipart(1))
      ipart2(1) = 1
      ipart2(4) = inverse(ipart(1))
      buff = HSS_head1//coup//','//smass//','//swidth//HSS_tail1
      call add3vertex(ipart2,buff,info,icoup)  !Send the HSS version

c
c     Calculate scalar 2
c
      do i=0,4
         ipart2(i)=ipart(i)
      enddo
      if (ipart(2) .ne. ipart(1)) then
         smass  = str(2,ipart(2))
         swidth = str(3,ipart(2))
         ipart2(2)=1
         ipart2(4)=inverse(ipart(2))
         buff = HSS_head2//coup//','//smass//','//swidth//HSS_tail2
         call add3vertex(ipart2,buff,info,icoup)
      endif

c
c     Calculate scalar 3
c
      do i=0,4
         ipart2(i)=ipart(i)
      enddo
      if (ipart(3) .ne. ipart(2) .and. ipart(3) .ne. ipart(1)) then
         ipart2(3) = 1
         ipart2(4) = inverse(ipart(3))
         smass  = str(2,ipart(3))
         swidth = str(3,ipart(3))
         buff = HSS_head3//coup//','//smass//','//swidth//HSS_tail3
         call add3vertex(ipart2,buff,info,icoup)
      endif

      end

      subroutine addVVV(partbuff,coup,info,icoup)
c**************************************************************************
c     Sets up the VVV vertex with all the permutations
c     info is the color info for the VVV configuration
c     icoup is the coupling constant
c     should be sent in the form w- w+ v
c*************************************************************************
      implicit none

! Constants

      include 'params.inc'
c      integer    max_string
c      parameter (max_string=120)
c      integer    max_coup
c      parameter (max_coup=5)
c      integer    max_particles       
c      parameter (max_particles=2**7-1)
      character*(*) VVV_head                          
      parameter    (VVV_head='VVVXXX(W(1,1???),W(1,2???),W(1,3???),')
      character*(*) VVV_tail                          
      parameter    (VVV_tail=',AMP(????))')
      character*(*) JVV_head
      parameter    (JVV_head='JVVXXX(W(1,1???),W(1,2???),')
      character*(*) JVV_tail
      parameter    (JVV_tail=',W(1,3???))')
      character*(*) JVV2_head
      parameter    (JVV2_head='JVVXXX(W(1,3???),W(1,1???),')
      character*(*) JVV2_tail
      parameter    (JVV2_tail=',W(1,2???))')
      character*(*) JVV3_head
      parameter    (JVV3_head='JVVXXX(W(1,2???),W(1,3???),')
      character*(*) JVV3_tail
      parameter    (JVV3_tail=',W(1,1???))')
      
! Arguments

      character*(*) partbuff,coup
      integer info(0:4),icoup(0:max_coup)

! Local
      
      integer ipart(0:4),ipart2(0:4),i
      character*(max_string) buff
      character*8 wmass,wwidth,vmass,vwidth


! Global

      character*(4*max_particles) particle(4)
      integer                               charge_c(max_particles)
      integer iparticle(0:max_particles,0:4),inverse(max_particles)
      common/to_model/iparticle,  particle,  inverse, charge_c

      character*(max_string) iwave(max_particles),owave(max_particles)
      character*(8) str(3,max_particles)
      integer info_p(5,max_particles),iposx(3,max_particles)
      common/to_external/iwave,owave,iposx,info_p,str

c-----
c  Begin Code
c-----
c      print*,'adding: ',partbuff
      call getparticle(partbuff,ipart(1),ipart(0)) !parse particle string
      do i=0,4
         ipart2(i)=ipart(i)
      enddo
      vmass  = str(2,ipart(3))
      vwidth = str(3,ipart(3))
      wmass  = str(2,ipart(1))
      wwidth = str(3,ipart(1))
      buff = VVV_head//coup//VVV_tail
      call add3vertex(ipart2,buff,info,icoup)  !Send the IOV version

      do i=0,4
         ipart2(i)=ipart(i)
      enddo
      ipart2(3)=1
      ipart2(4)=inverse(ipart(3))
      buff = JVV_head//coup//','//vmass//','//vwidth//JVV_tail
      call add3vertex(ipart2,buff,info,icoup)

      if (ipart(1) .ne. ipart(2)) then         !Not 3 identical particles
         do i=0,4
            ipart2(i)=ipart(i)
         enddo
         ipart2(2) = 1
         ipart2(4) = inverse(ipart(2))
         buff = JVV2_head//coup//','//wmass//','//wwidth//JVV2_tail
         call add3vertex(ipart2,buff,info,icoup)

         do i=0,4
            ipart2(i)=ipart(i)
         enddo
         ipart2(1) = 1
         ipart2(4) = inverse(ipart(1))
         buff = JVV3_head//coup//','//wmass//','//wwidth//JVV3_tail
         call add3vertex(ipart2,buff,info,icoup)
      endif

      end

      subroutine add3vertex(ipart,wavebuff1,info,icoup)
c***************************************************************************
c     subroutine to add a three particle vertex to madgraph
c     written 3/2/94
c***************************************************************************
      implicit none

! Constants

      include 'params.inc'

! Arguments

      character*(*) wavebuff1
      integer info(0:4),icoup(0:max_coup)

! Local

      integer ipart(0:4),iplace,iorder(3),i,iorder2(3)
      character*(max_string) wavebuff

! Global
      integer   coup(0:max_coup,maxv3)
      integer                         ipos3(4,maxv3), info3(0:4,maxv3)
      character*(max_string) v3wave(maxv3)
      common/to_internal/    v3wave,  ipos3         , info3,coup
      logical                cc(maxv3),pass_cc(maxv3)
      common /to_chargeconjg/cc       ,pass_cc
      integer                             imaja
      logical                tcc,tpasscc
      common/set_chargeconjg/tcc,tpasscc, imaja

      integer           nchars
      character*12             postfix
      common/to_postfix/nchars,postfix


!-----
!  Begin Code
!-----      
c      call getparticle(partbuff,ipart(1),ipart(0))   !parse particle string
      if (ipart(0) .eq. 3) then 
         do i=1,3
            iorder2(i)=i
            iorder(i) =i
         enddo

c         write(*,*) 'Particl Code', ipart(1),ipart(2),ipart(3)
c         write(*,*) 'Unsort Color',(info(i),i=1,3)

         call sort2(ipart(1),iorder2,3)

c         write(*,*) 'iorder2     ',(iorder2(i),i=1,3)

c         write(*,*) 'Sort 2 Color',(info3(i,iplace),i=1,3)

         call sort2(iorder2,iorder,3)

         call add3vert(ipart(1),ipart(2),ipart(3),iplace)
c         write(*,*) 'Color info',iplace,(info(i),i=0,3)
c         write(*,'(a,6i6)') 'Added 3 vertex',iplace,(ipart(i),i=1,3)
c     &        tcc
         if (iplace .eq. 0) then
            print*,'Error vertex not entered'
            return
         else
            do i=1,3
               info3(iorder2(i),iplace)=info(i)
            enddo         
         endif
c
c     Set up Charge Conjugate Stuff
c
         cc(iplace)=tcc
         pass_cc(iplace)=tpasscc


         call add_postfix(wavebuff1,wavebuff,postfix,nchars)


         v3wave(iplace) = wavebuff
         ipos3(iorder(1),iplace) = index(wavebuff,'W(1,1??')+4 
         ipos3(iorder(2),iplace) = index(wavebuff,'W(1,2??')+4 
         ipos3(iorder(3),iplace) = index(wavebuff,'W(1,3??')+4 
         info3(0,iplace) = info(0)


         do i=1,3
            info3(iorder(i),iplace)=info(i)
         enddo
         
c         write(*,*) 'Sorted Color',(info3(i,iplace),i=1,3)

         do i=0,max_coup
            coup(i,iplace) = icoup(i)
         enddo
c         write(*,'(5i4)') iplace,(icoup(i),i=1,4)

c         if (iplace .lt. max_blocks) then
c            coup(0,iplace)=iplace          !Set coup to vertex number
c         endif

         if (ipart(1) .ne. 1) then  !All 3 particles are known
            ipos3(4,iplace) = index(wavebuff,'AMP(?')+4
         else  !Store new particles code in position 4
c            call getparticle(resultbuff,ipart(1),ipart(0))
c            if (ipart(0) .ne. 1) then
c               print*,'Warning could not decode result ',resultbuff
c            else
               ipos3(4,iplace) = ipart(4)
c               ipos3(4,iplace) = ipart(1)
c               print*,wavebuff
c               print*,'New particle is',ipos3(4,iplace)
c               print*,'color order',(info3(i,iplace),i=0,3)
c            endif
         endif
c         write(*,'(a79)') v3wave(iplace)
c         write(*,'(5i6)') iplace,(ipos3(i,iplace),i=1,4)
      else
         print*,'Sorry only 3 particle vertices allowed'
c         print*,partbuff
c         print*,resultbuff
         print*,ipart(0),ipart(1),ipart(2),ipart(3)
      endif
      end
 
      subroutine add_postfix(wavebuff,wavebuff1,postfix,nchars)
c************************************************************************
c     Appends postfix to subroutine names for user created subroutines
c************************************************************************
      implicit none
c
c     Arguments
c
      character*(*) wavebuff,wavebuff1,postfix
      integer       nchars
c
c     Local
c
      integer i,j,n,nc
c
c     External
c
      integer index
c-----
c  Begin Code
c-----
      if (nchars .gt. 0) then
         i = index(wavebuff(1:6),'X')-1
         if (i .eq. -1) i=6
         j = min (i+nchars+1,index(wavebuff,'('))
         n = j
         nc = 1
         do while (n .ne. 0)
            nc = n+nc
            n  = index(wavebuff(nc:),')')
         enddo
         wavebuff1 = wavebuff(1:i)//postfix(1:nchars)//wavebuff(j:nc)
      else
         wavebuff1 = wavebuff
      endif
      end

