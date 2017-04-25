!-----------------------
! madgraph - a Feynman Diagram package by Tim Stelzer and Bill Long 
! (c) 1993
!
! Filename: readmodel.f
!-----------------------
!
! This file contains the routines needed to read in the particles 
! and interactions of a user defined model
!
      Subroutine Load_Interactions()
c*************************************************************************
c     Routine to add interactions to MadGraphII.  First tries to read
c     from file interactions.dat.  If this is not found, then loads
c     defaults.
c*************************************************************************
      implicit none
c
c     Constants
c
      include 'params.inc'
c      integer    max_words   , max_chars   , maxline
c      parameter (max_words=20, max_chars=20, maxline=80)
c
c     Arguments
c
c      integer qcd,qed,qfd
c
c     Local
c
      integer lun,nwords,length(max_words),i
      character*(max_chars) words(max_words)
      character*(maxline) cline
c
c     Global
c
      character*(10)            coup_name(max_coup)
      integer             ncoups
      common/to_couplings/ncoups,coup_name

!-----
!  Begin Code
!-----      
c---
c  tjs 8/15/2008
c  added initialization of ncoups here
c---
      ncoups = 0
      lun=66
      open(unit=lun,file='interactions.dat',status='old',err=200)
      print*,'Reading interactions from file'
      nwords=1
      do while (nwords .gt. 0)
         nwords = 0
         do while (nwords .le. 0)
            read(lun,'(a80)',end=100,err=200) cline
            call parse_string(nwords,length,words,cline)
         enddo
c         write(*,*) 'Interaction ', cline
         Call addinteraction(nwords,length,words)
      enddo
 100  close(lun)
      return
 200  write(*,*) 'Error: need file interactions.dat'
      stop
c 200  print*,'Using default interactions'
c      call addQCD
c      call addQED
c      call addQFD
      end


      subroutine addinteraction(nwords,length,words)
c*************************************************************************
c     Routine to add interactions to MadGraphII.  First tries to read
c     from file interactions.dat.  If this is not found, then loads
c     defaults.
c*************************************************************************
      implicit none
c
c     Constants
c
      include 'params.inc'
c
c     Arguments
c
      integer nwords,length(max_words)
      character*(max_chars) words(max_words)
c
c     Local
c
      integer i,j,n,jpart
      integer info(0:4),ipart(0:5),itype(4),icoup(0:max_coup),icolor(8)
      integer ione,itwo,ithree
      integer pcolor(4,8)   !Puts particles numbers of singlet/triplet/oct
      character*20 ctemp
      integer itemp
c
c     Global
c
      character*(max_string) iwave(max_particles),owave(max_particles)
      integer iposx(3,max_particles)
      integer info_p(5,max_particles)
      character*(8) str(3,max_particles)
      common/to_external/iwave,owave,iposx,info_p,str


      character*(4*max_particles) particle(4)
      integer                               charge_c(max_particles)
      integer iparticle(0:max_particles,0:4),inverse(max_particles)
      common/to_model/iparticle,  particle,  inverse, charge_c

      integer                          imaja
      logical                cc,passcc
      common/set_chargeconjg/cc,passcc,imaja

      integer           nchars
      character*12             postfix
      common/to_postfix/nchars,postfix

      character*(40)       particleID, interactionID
      common/to_modelinput/particleID, interactionID
c
c     tjs oct 3 2008. specialColor used only in readmodel.f
c
      character*20           colr_str
      common/to_specialColor/colr_str

c     External
c
      logical majarana
!-----
!  Begin Code
!-----      
      colr_str = ""
      do i=1,8
         icolor(i)=0
         do j=1,4
            pcolor(j,i)=0
         enddo
      enddo
      do i=1,4
         itype(i) = 0
      enddo
      do i=0,max_coup
         icoup(i)=0
      enddo
      if (words(1)(1:1) .eq. '#') then
         i=1
         do while (interactionid .eq."???" .and. i .le. nwords)
            if (index(words(i),'interactions_') .ne. 0) then
               InteractionID = words(i)
c               write(*,*) 'Interaction ID ',interactionid, i
            endif
            i=i+1
         enddo
         return
      endif   
      if (nwords .eq. 0) return
      if (words(1)(1:1) .eq. '!')
     &     print*,(words(i)(:length(i)),i=1,nwords)
c      write(*,*) 'New interaction---------------------------------'

c RF, Oct 14 2008: possibility to have new color structure and postfix 
      if (nwords .eq. 5 .or. nwords .eq. 6 .or. nwords .eq. 7) then !3 vert
c
c     3 particle interaction, determine particles/color structure
c
c
c     TJS Oct 3 2008  &  RF Oct 14 2008
c     Added check for special color factor using get_cpostfix
c
         if (nwords .eq. 6 .or. nwords .eq. 7) then          !New type of interaction
            nchars = length(6)-1
            call get_cpostfix(words(6))
            if (colr_str .ne. "") then
               write(*,*) "Special color factor=",colr_str
            endif
            if (nwords. eq. 7) then  ! both postfix and new color structure
               postfix = words(7)
               nchars  = length(7)-1
               nchars = min(nchars,12)
            endif
         else
            nchars  = 0
            colr_str = ""
         endif

         ipart(0)=3
         do j=1,8
            do i=1,4
               pcolor(i,j) = 0
            enddo
            icolor(j) = 0
         enddo
         do i=1,3
            call lower_case(words(i)(1:length(i)))
            call getparticle(words(i)(1:length(i)),ipart(i),n)
            if (n .ne. 1) then
               print*,'Error particle unknown: ',words(i),n,length(i)
            else
               j       = info_p(3,ipart(i))+1
               itype(j)= itype(j)+1
               if (j .eq. 1 .and. info_p(2,ipart(i)) .eq. 1) then
                  itype(1)=itype(1)-1 !Scalar not vector Boson
                  itype(3)=itype(3)+1
c RF 23 Mar. 2006
               elseif (j .eq. 1 .and. info_p(2,ipart(i)) .eq. 4) then
                  itype(1)=itype(1)-1 !Tensor not vector Boson
                  itype(4)=itype(4)+1
cend  RF
               endif
            endif
            j = info_p(1,ipart(i))
            icolor(j) = icolor(j)+1
            pcolor(icolor(j),j)=i            !Stores color particles in order
         enddo
c
c     Here I need routine to make sure 3 comes first and then 3bar
c
         if (icolor(3) .eq. 2) then           !Need to put 3bar first
            jpart = ipart(pcolor(2,3))
c            write(*,*) 'Checking ord',pcolor(2,3),jpart,inverse(jpart)
            if (jpart .gt. inverse(jpart) ) then
               i = pcolor(1,3)
               pcolor(1,3)=pcolor(2,3)
               pcolor(2,3)=i
c               write(*,*) 'switched',pcolor(1,3),pcolor(2,3)
            endif
         endif

         call setcolorinfo(icolor,ipart,words,length,info,pcolor)


         call upper_case(words(5))         
         do i=1,max_coup
            icoup(i)=0
         enddo
         call set_coupling(words(5)(1:length(5)),icoup)  !QCD/QED/QFD/BRS

         cc = .false.

c
c     Here goes the FFV vertex
c
c         write(*,*) 'Interaction type', itype(1),itype(2),itype(3)
         if (itype(2) .eq. 2 .and. itype(1) .eq. 1) then !FFV
            imaja = 0
            if (majarana(ipart(1))) imaja=imaja+1
            if (majarana(ipart(2))) imaja=imaja+1
            if  (imaja .gt. 0) then !Majorana, don't pass cc
               passcc = .false.
            else 
               passcc = .true.
            endif
c
c           First two entries (fermions) should be particles, not
c           anti-particles. If they are antiparticles, need to
c           take inverse, and charge conjugate so we have flowing
c           out fermion

            do i=1,2
               if (ipart(i) .gt. inverse(ipart(i))) then
c                  write(*,*) 'Using CC for FFV vertex ',i,words(i)
c                  ipart(i) = inverse(ipart(i))
                  ipart(i) = charge_c(ipart(i))
               endif
            enddo
c
c           Change first particle to inverse due to way interactions
c           are specified in interactions.dat 
c            
            ipart(1) = inverse(ipart(1))

c
c            write(*,*) 'Postfix is ',postfix(1:nchars)
            call addFFV1(ipart,words(4)(1:length(4)),info,icoup)
c
c           Now add the reversed flow case (if its not already there)
c
            if (charge_c(ipart(2)) .ne. ipart(1)) then  
c
c              Here we take CC of each component but also switch
c              order of two fermions since order gives fermion flow
c
               j = (charge_c(ipart(2)))
               ipart(2) = (charge_c(ipart(1)))
               ipart(1) = j
c               ipart(3) = charge_c(ipart(3))
c
c              Since particles 1 and 2 have swapped positions, need
c              to update pcolor

c               write(*,*) 'Pre Pcolor 8 3 3',pcolor(1,8),
c     &              pcolor(1,3),pcolor(2,3)

               do i=1,8
                  do j=1,4
                     if (pcolor(j,i) .eq. 1) then
                        pcolor(j,i) = 2                        
                     elseif (pcolor(j,i) .eq. 2) then
                        pcolor(j,i) = 1
                     endif
                  enddo 
               enddo


c               write(*,*) 'PostPcolor 8 3 3',pcolor(1,8),
c     &              pcolor(1,3),pcolor(2,3)

c
c              Since we have taken CC, we also need to switch the order
c              of the triplets since the 3 is now the 3 bar etc.
c
c               j = pcolor(1,3)
c               pcolor(1,3) = pcolor(2,3)
c               pcolor(2,3) = j

c
c             Just for completeness swap the particle names in words
c
               if (.false.) then
                  write(*,'(a,3a)')'Pre switch Particles: ',
     &              (words(i)(1:length(i)),i=1,3)
                  ctemp = words(1)
                  itemp = length(1)
                  words(1)=words(2) // "c"
                  length(1)=length(2)+1
                  words(2) = ctemp // "c"
                  length(2) = itemp+1
                  write(*,'(a,3a)')'Postswitch Particles: ',
     &              (words(i)(1:length(i)),i=1,3)
               endif

               call setcolorinfo(icolor,ipart,words,length,info,pcolor)

               nchars=nchars+1
               postfix(nchars:nchars) = 'C'   !Adds C to routine
               call addFFV1(ipart,words(4)(1:length(4)+1),info,icoup)
            endif
c---------------------------------------------------------------------
c***********************KEK***************************************************

c     Here goes the FFT vertex

         elseif (itype(2) .eq. 2 .and. itype(4) .eq. 1) then !FFT
            imaja = 0
            if (majarana(ipart(1))) imaja=imaja+1
            if (majarana(ipart(2))) imaja=imaja+1
            if  (imaja .gt. 0) then !Majorana, don't pass cc
               passcc = .false.
            else 
               passcc = .true.
            endif
c
c           First two entries (fermions) should be particles, not
c           anti-particles. If they are antiparticles, need to
c           take inverse, and charge conjugate so we have flowing
c           out fermion
            
            do i=1,2
               if (ipart(i) .gt. inverse(ipart(i))) then
c                  write(*,*) 'Using CC for FFT vertex ',i,words(i)
c                  ipart(i) = inverse(ipart(i))
                  ipart(i) = charge_c(ipart(i))
               endif
            enddo
c
c           Change first particle to inverse due to way interactions
c           are specified in interactions.dat 
c            
            ipart(1) = inverse(ipart(1))

c
c            write(*,*) 'Postfix is ',postfix(1:nchars)
            call addFFT1(ipart,words(4)(1:length(4)),info,icoup)
c
c           Now add the reversed flow case (if its not already there)
c
            if (charge_c(ipart(2)) .ne. ipart(1)) then  
c
c              Here we take CC of each component but also switch
c              order of two fermions since order gives fermion flow
c
               j = (charge_c(ipart(2)))
               ipart(2) = (charge_c(ipart(1)))
               ipart(1) = j
c               ipart(3) = charge_c(ipart(3))
c
c              Since particles 1 and 2 have swapped positions, need
c              to update pcolor

c               write(*,*) 'Pre Pcolor 8 3 3',pcolor(1,8),
c     &              pcolor(1,3),pcolor(2,3)

               do i=1,8
                  do j=1,4
                     if (pcolor(j,i) .eq. 1) then
                        pcolor(j,i) = 2                        
                     elseif (pcolor(j,i) .eq. 2) then
                        pcolor(j,i) = 1
                     endif
                  enddo 
               enddo


c               write(*,*) 'PostPcolor 8 3 3',pcolor(1,8),
c     &              pcolor(1,3),pcolor(2,3)

c
c              Since we have taken CC, we also need to switch the order
c              of the triplets since the 3 is now the 3 bar etc.
c
c               j = pcolor(1,3)
c               pcolor(1,3) = pcolor(2,3)
c               pcolor(2,3) = j

c
c             Just for completeness swap the particle names in words
c
               if (.false.) then
                  write(*,'(a,3a)')'Pre switch Particles: ',
     &              (words(i)(1:length(i)),i=1,3)
                  ctemp = words(1)
                  itemp = length(1)
                  words(1)=words(2) // "c"
                  length(1)=length(2)+1
                  words(2) = ctemp // "c"
                  length(2) = itemp+1
                  write(*,'(a,3a)')'Postswitch Particles: ',
     &              (words(i)(1:length(i)),i=1,3)
               endif

               call setcolorinfo(icolor,ipart,words,length,info,pcolor)

               nchars=nchars+1
               postfix(nchars:nchars) = 'C'   !Adds C to routine
               call addFFT1(ipart,words(4)(1:length(4)+1),info,icoup)
            endif
c***********************KEK***************************************************
c     FFS Vertex
c--------------------------------------------------------------------
         elseif(itype(2) .eq. 2 .and. itype(3) .eq. 1) then !FFS
            imaja = 0
            if (majarana(ipart(1))) imaja=imaja+1
            if (majarana(ipart(2))) imaja=imaja+1
c
c           Check for Majorana for passing charge Conjugation info
c
            if  (majarana(ipart(1)) .or.
     &           majarana(ipart(2))) then !Majorana, don't pass cc
               passcc = .false.
            else 
               passcc = .true.
            endif

c
c           First two entries (fermions) should be particles, not
c           anti-particles. If they are antiparticles, need to
c           take inverse, and charge conjugate so we have flowing
c           out fermion

            do i=1,2
               if (ipart(i) .gt. inverse(ipart(i))) then
c                  write(*,*) 'Using CC for FFS vertex ',i,words(i)
c                  ipart(i) = inverse(ipart(i))
                  ipart(i) = charge_c(ipart(i))
               endif
            enddo
c
c           Change first particle to inverse due to way interactions
c           are specified in interactions.dat 
c            

            ipart(1) = inverse(ipart(1))
            call addFFS1(ipart,words(4)(1:length(4)),info,icoup)
c
c     Now check for CC case if two fermions are not identical
c
            if (charge_c(ipart(2)) .ne. ipart(1)) then
c               write(*,*) 'Adding CC vertex',ipart(1),ipart(2)
c
c              Here we take CC of each component but also switch
c              order of two fermions since order gives fermion flow
c
               j = (charge_c(ipart(2)))
               ipart(2) = (charge_c(ipart(1)))
               ipart(1) = j

c
c              Since particles 1 and 2 have swapped positions, need
c              to update pcolor

               do i=1,8
                  do j=1,4
                     if (pcolor(j,i) .eq. 1) then
                        pcolor(j,i) = 2                        
                     elseif (pcolor(j,i) .eq. 2) then
                        pcolor(j,i) = 1
                     endif
                  enddo
               enddo

c
c             Just for completeness swap the particle names in words
c
               if (.false.) then
c               write(*,'(a,3a)')'Pre switch Particles: ',
c     &              (words(i)(1:length(i)),i=1,3)
               ctemp = words(1)
               itemp = length(1)
               words(1)=words(2)
               length(1)=length(2)+1
               words(2) = ctemp 
               length(2) = itemp+1
c               write(*,'(a,3a)')'Postswitch Particles: ',
c     &              (words(i)(1:length(i)),i=1,3)
               endif

               nchars=nchars+1
               postfix(nchars:nchars) = 'C'   !Adds C to routine

c               write(*,*) 'Color info postswitch',(info(i),i=1,3) 

               call setcolorinfo(icolor,ipart,words,length,info,pcolor)
               call addFFS1(ipart,words(4)(1:length(4)+1),info,icoup)

            endif
c
c     Here go all the other ones
c
         elseif(itype(1) .eq. 2 .and. itype(3) .eq. 1) then !VVS
            call addVVS1(ipart,words(4)(1:length(4)),info,icoup)
         elseif(itype(1) .eq. 1 .and. itype(3) .eq. 2) then !VSS
c            write(*,*) 'Adding vss'
            call addVSS1(ipart,words(4)(1:length(4)),info,icoup)
         elseif(itype(1) .eq. 3) then !VVV
            call addVVV1(ipart,words(4)(1:length(4)),info,icoup)
         elseif(itype(3) .eq. 3) then !SSS
            call addSSS1(ipart,words(4)(1:length(4)),info,icoup)
c RF 23 Mar. 2006
         elseif(itype(1) .eq. 2 .and. itype(4) .eq. 1) then !VVT
            call addVVT1(ipart,words(4)(1:length(4)),info,icoup)
         elseif(itype(3) .eq. 1 .and. itype(4) .eq. 2) then !TTS
            call addTTS1(ipart,words(4)(1:length(4)),info,icoup)
cend RF

c RF 29 Mar. 2006
         elseif(itype(3) .eq. 2 .and. itype(4) .eq. 1) then !SST
            call addSST1(ipart,words(4)(1:length(4)),info,icoup)
cend RF
         else
            print*,'Warning unknown interaction',(itype(i),i=1,3)
            print*,'Particles: ',(words(i)(1:length(i)),i=1,3)
         endif
c
c
c     4 particle interactions
c
      elseif (nwords .eq. 8 .or. nwords .eq. 9) then
         ipart(0)=4
         do i=1,4
            call lower_case(words(i)(1:length(i)))
            call getparticle(words(i)(1:length(i)),ipart(i),n)
            if (n .ne. 1) then
               print*,'Error particle unknown: ',words(i),n,length(i)
            else
               j       = info_p(3,ipart(i))+1
               itype(j)= itype(j)+1
               if (j .eq. 1 .and. info_p(2,ipart(i)) .eq. 1) then
                  itype(1)=itype(1)-1 !Scalar not vector Boson
                  itype(3)=itype(3)+1
c RF 23 Mar. 2006
               elseif (j .eq. 1 .and. info_p(2,ipart(i)) .eq. 4) then
                  itype(1)=itype(1)-1 !Tensor not vector Boson
                  itype(4)=itype(4)+1
cend RF
               endif
            endif
            j = info_p(1,ipart(i))
            icolor(j) = icolor(j)+1
            pcolor(icolor(j),j)=i
         enddo

c
c     Here I need routine to make sure 3 comes first and then 3bar
c
         if (icolor(3) .eq. 2) then !Need to put 3bar first
c         write(*,*) 'pcolor',pcolor(1,3),pcolor(2,3),
c     &        ipart(pcolor(1,3)), ipart(pcolor(2,3))
c         write(*,*) 'pcolor',pcolor(1,3),pcolor(2,3),
c     &        inverse(ipart(pcolor(1,3))), inverse(ipart(pcolor(2,3)))
            jpart = ipart(pcolor(2,3))
            if (jpart .gt. inverse(jpart)) then
               i = pcolor(1,3)
               pcolor(1,3)=pcolor(2,3)
               pcolor(2,3)=i
c               write(*,*) 'switched',pcolor(1,3),pcolor(2,3)
            endif
         endif
         call setcolorinfo(icolor,ipart,words,length,info,pcolor)
         call upper_case(words(6))
         do i=1,max_coup
            icoup(i)=0
         enddo
         call set_coupling(words(7)(1:length(7)),icoup)
         call set_coupling(words(8)(1:length(8)),icoup)

c RF 23 Mar. 2006 for the 4-vertex postfix:
         cc = .false.
         if (nwords .eq. 9) then          !New type of interaction
            postfix = words(9)
            nchars  = length(9)-1
            nchars = min(nchars,12)
         else
            nchars  = 0
         endif
cend RF

         if (itype(1) .eq. 4) then !VVVV
            if (info(0) .eq. 3) then     !GGGG
               call addgggg1(ipart,words(5)(1:length(5)),
     &              words(6)(1:length(6)),info,icoup)
            elseif (info(0) .eq. 0) then     !Color Singlet
               if ((ipart(1) .eq. ipart(2) .or.
     &              ipart(1) .eq. inverse(ipart(2))) .and.
     &             (ipart(1) .eq. ipart(3) .or.
     &              ipart(1) .eq. inverse(ipart(3))) .and.
     &             (ipart(1) .eq. ipart(4) .or.
     &              ipart(1) .eq. inverse(ipart(4))) ) then
                  call addWWWW1(ipart,words(5)(1:length(5)),
     &                 words(6)(1:length(6)),info,icoup)
               elseif (ipart(1) .eq. inverse(ipart(3))) then
                  call addW3W31(ipart,words(5)(1:length(5)),
     &                 words(6)(1:length(6)),info,icoup)
               else
c
c      tjs 4-24-07  Assume new vertex is 4W format instead of WZ
c
                 call addWWWW1(ipart,words(5)(1:length(5)),
     &              words(6)(1:length(6)),info,icoup)
c                  print*,'Unknown 4 Vector Verted in readmodel.f'
               endif
            else
               print*,'Unknown color in 4 vertex readmodel.f',info(0)
            endif
         elseif (itype(1) .eq. 2 .and. itype(3) .eq. 2) then !VVSS
            call addVVSS1(ipart,words(5)(1:length(5)),info,icoup)
c            print*,'Adding interaction',(words(i)(1:length(i)),i=1,4)
         elseif (itype(1) .eq. 3 .and. itype(3) .eq. 1) then !VVVS
c RF 10 Apr. 2006
            call addVVVS1(ipart,words(5)(1:length(5)),
     $           words(6)(1:length(6)),info,icoup)
c            call addVVVS1(ipart,words(5)(1:length(5)),info,icoup)
c            print*,'Adding interaction',(words(i)(1:length(i)),i=1,4)
cend RF

c RF 23 Mar. 2006
         elseif (itype(4) .eq. 2 .and. itype(3) .eq. 2) then !TTSS
            call addTTSS1(ipart,words(5)(1:length(5)),
     $           words(6)(1:length(6)),info,icoup)
cend RF

c RF 10 Apr. 2006
         elseif (itype(3) .eq. 4) then                       !SSSS
            call addSSSS1(ipart,words(5)(1:length(5)),
c     $           words(6)(1:length(6)),info,icoup)
cend RF
c RF 10 Jul. 2006
     $                                  info,icoup)
cend RF


c RF 29 Mar. 2006
         elseif (itype(1) .eq. 3 .and. itype(4) .eq. 1) then !VVVT
            call addVVVT1(ipart,words(5)(1:length(5)),
     $           words(6)(1:length(6)),info,icoup)

cend RF

c***********************KEK******************************************

         elseif (itype(1).eq.1 .and. itype(2) .eq. 2 .and.
     $ itype(4) .eq. 1)then !FFVT
            imaja = 0
            if (majarana(ipart(1))) imaja=imaja+1
            if (majarana(ipart(2))) imaja=imaja+1
            if  (imaja .gt. 0) then !Majorana, don't pass cc
               passcc = .false.
            else 
               passcc = .true.
            endif

           do i=1,2
               if (ipart(i) .gt. inverse(ipart(i))) then
                  ipart(i) = charge_c(ipart(i))
               endif
            enddo
            
            ipart(1) = inverse(ipart(1))


            call addFFVT1(ipart,words(5)(1:length(5)),
     $           words(6)(1:length(6)),info,icoup)

         else
            print*,'Warning unknown interaction',(itype(i),i=1,3)
            print*,'Particles: ',(words(i)(1:length(i)),i=1,4)
         endif
c      else     Bad number of words
c         print*,'Sorry 4 particle interaction not implemented', nwords,
c     $        words(1)
c         print*,'Particles: ',(words(i)(1:length(i)),i=1,nwords)
      endif
      If (colr_str .ne. "") then
         write(*,*) "Warning from reading interactions.dat:"
         write(*,*) "Special color for vertex not implemented=",colr_str
         colr_str = ""
      endif
      end
      
      Subroutine Get_CPostfix(buff)
c************************************************************************
c     Extracts postfix and color information from extra column in
c     interaction.dat line. Note postfix and colr_str  set for common_block
c************************************************************************
      implicit none
      include 'params.inc'
c
c     Arguments
c
      character*(max_chars) buff     
c
c     Local
c
      integer n
c
c     Global
c
      integer           nchars
      character*12             postfix
      common/to_postfix/nchars,postfix
c
c     tjs oct 3 2008. specialColor used only in readmodel.f
c
      character*20           colr_str
      common/to_specialColor/colr_str

c-----
c  Begin Code
c-----
      postfix = buff
      colr_str = ""
      if (buff(1:1) .eq. "^") then !New color               
         n = index(buff(2:),"^")         
         colr_str = buff(2:n)
         postfix = buff(n+2:)
         nchars = nchars - n - 1
         if (nchars .lt. 0 ) nchars = 0
      endif
      nchars = min(nchars,12)
c      write(*,*) "Special Color factors"
c      write(*,*) buff
c      write(*,*) n,"color=",colr_str
c      write(*,*) nchars,"postfix=",postfix
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
      logical gotid
c
c     Global
c
      character*(40)       particleID, interactionID
      common/to_modelinput/particleID, interactionID
c
c     tjs 8/15/2008
c     add initialization of variable to readparticles routine
c
      logical         major_exist
      common/to_major/major_exist
c     
c     tjs 10/3/2008 
c     add option for treating everything as color single NoColor=.true.
c
      logical             NoColor
      common  /to_nocolor/NoColor

c-----
c  Begin Code
c-----
      nocolor = .false.
      major_exist = .false.
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
      gotid = .false.
      nwords=1
      do while (nwords .gt. 0)
         call readline(nwords,lun,length,words)
         if (words(1)(1:1) .eq. '#') then
            i=1
            do while (particleid .eq. "???" .and. i .le. nwords)
               if (index(words(i),'particles_') .ne. 0) then
                  particleID = words(i)
                  write(*,*) 'Got it',i,words(i)
                  gotid = .true.
               endif
               i=i+1
            enddo

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
            if (nocolor) info(1) = 1

c     Get particle ID 
c            call upper_case(words(9))
            if (nwords .eq. 9) then
               read(words(9),*,err=122) model !This is really the particle ID
               goto 123
 122           model = 0
            else
c               write(*,*) (words(i),i=1,8)
               model = 0
            endif
c            write(*,*) 'Read model',model
c            model = 0
c            if (words(9)(1:3) .eq. 'QCD') model=1
c            if (words(9)(1:3) .eq. 'QED') model=2
c            if (words(9)(1:3) .eq. 'QFD') model=3
c            if (words(9)(1:3) .eq. 'BRS') model=4

c     Set up number of helicities 1,2,3
 123        call upper_case(words(3))
            if (words(3)(1:1) .eq. 'S') info(2)=1
            if (words(3)(1:1) .eq. 'F') info(2)=2
            if (words(3)(1:1) .eq. 'V') info(2)=3

c RF 23 Mar. 2006
            if (words(3)(1:1) .eq. 'T') info(2)=4
cend RF

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
c RF 23 Mar. 2006
            elseif(words(3)(1:1) .eq. 'T') then
               call addtensor(words(1)(1:length(1)),
     &              words(2)(1:length(2)),words(5)(1:length(5)),
     &              words(6)(1:length(6)),words(8)(1:length(8)),
     &              info(1),info(2),info(4),model)
cend RF
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
      if (nloop .gt. maxloops) then
         write(*,*) 'Too many looped codes entered in readmodel.f ' ,
     $        buff(1:n), ' not implemented'
         stop 'Too many multiparton labels'
      endif
      SumParticles(0,0)=nloop
      SumParticles(0,nloop) = ipart  !Particle code for loop 
      call getparticle(sumbuff,SumParticles(2,nloop),
     $     SumParticles(1,nloop))      
      
      if (.true.) then
      if(SumParticles(1,nloop).gt.maxpartons)then
         write(*,*) 'Too many particles in multilabel ',
     $        SumParticles(1,nloop),maxpartons
         stop 'Too many particles in multilabel'
      endif
      
      write(*,*) 'Added loop options for ',buff(1:n),
     $     sumparticles(0,nloop)
      write(*,*) 'Looping over ',sumparticles(1,nloop),' particles'
      do i=1,sumparticles(1,nloop)
         call part_string(sumparticles(i+1,nloop),str1,n)
         write(*,'(a,a$)') str1(1:n),' '
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

c
c     tjs oct 3 2008. specialColor used only in readmodel.f
c
      character*20           colr_str
      common/to_specialColor/colr_str


c-----
c  Begin Code
c-----
      if (icolor(8) .eq. 3) then  
         info(0)=2              !f(a,b,c)
         info(pcolor(1,8))=1
         info(pcolor(2,8))=2
         info(pcolor(3,8))=3
         if (colr_str .eq. "dabc") then
            info(0)=7
            colr_str = ""
         endif
c RF 15 Mar. 2006
         if (icolor(1) .eq. 1) info(pcolor(1,1))=4            
cend RF

      elseif (icolor(8) .eq. 2 .and. icolor(3) .eq. 0) then  
         info(0)=6              !f(a,b)  delta (a,b)
         info(pcolor(1,8))=1
         info(pcolor(2,8))=2
c RF 15 Mar. 2006
         if(icolor(1).eq.1 .or. icolor(1).eq.2) info(pcolor(1,1))=3
         if(                    icolor(1).eq.2) info(pcolor(2,1))=4
cend RF
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


