!-----------------------
! madgraph - a Feynman Diagram package by Tim Stelzer and Bill Long 
! (c) 1993
!
! Filename: nvert4.f
!-----------------------
 
      subroutine addGGGG1(ipart,coup1,coup2,info,icoup)
c**************************************************************************
c     Sets up the GGGG vertex with all the permutations
c     of the color structure
c     info is the color info for the W3W3 configuration
c     coup1 is coupling of W w/ boson 1
c     coup2 is coupling of W w/ boson 2
c     Order of particles should be W- A W+ A
c     icoup is the coupling constant
c*************************************************************************
      implicit none

! Constants

      include 'params.inc'
      character*(*) VVVV_head                          
      parameter    (VVVV_head='GGGGXX(W(1,1???),W(1,2???),'//
     &                               'W(1,3???),W(1,4???),')
      character*(*) VVVV_head1                          
      parameter    (VVVV_head1='GGGGXX(W(1,3???),W(1,1???),'//
     &                               'W(1,2???),W(1,4???),')
      character*(*) VVVV_head2                          
      parameter    (VVVV_head2='GGGGXX(W(1,2???),W(1,3???),'//
     &                               'W(1,1???),W(1,4???),')
      character*(*) VVVV_tail                          
      parameter    (VVVV_tail=',AMP(????))')
      character*(*) JVVV1_head 
      parameter    (JVVV1_head='JGGGXX(W(1,1???),W(1,2???),W(1,3???),')
      character*(*) JVVV1_tail 
      parameter    (JVVV1_tail=',W(1,4???))')
      character*(*) JVVV2_head 
      parameter    (JVVV2_head='JGGGXX(W(1,3???),W(1,1???),W(1,2???),')
      character*(*) JVVV2_tail 
      parameter    (JVVV2_tail=',W(1,4???))')
      character*(*) JVVV3_head 
      parameter    (JVVV3_head='JGGGXX(W(1,2???),W(1,3???),W(1,1???),')
      character*(*) JVVV3_tail 
      parameter    (JVVV3_tail=',W(1,4???))')

! Arguments

      character*(*) coup1,coup2
      integer info(0:4),icoup(0:max_coup),ipart(0:5)

! Local
      
      integer ipart2(0:5),i,info2(0:4)
      character*(8) vmass1,vwidth1,vmass2,vwidth2
      character*(max_string) buff


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
      vmass1  = str(2,ipart(2))
      vwidth1 = str(3,ipart(2))
      vmass2  = str(2,ipart(4))
      vwidth2 = str(3,ipart(4))
      if (vmass1(1:4) .ne. 'ZERO' .and. vmass1(1:3) .ne. '0D0') then
         print*,'Warning nonzero mass in 4 g sector',vmass1
         print*,'skipping interaction'
      else
      do i=0,5
         ipart2(i)=ipart(i)
      enddo
      buff = VVVV_head//coup1//VVVV_tail
      call add4vertex(ipart2,buff,info,icoup)  !Send the W3W3 version

      do i=0,5
         ipart2(i)=ipart(i)
      enddo
      ipart2(4)=1
      ipart2(5)=inverse(ipart(4))

      buff = JVVV1_head//coup1//JVVV1_tail
      call add4vertex(ipart2,buff,info,icoup)

      info2(0) = info(0)
      info2(1) = 2
      info2(2) = 3
      info2(3) = 1
      info2(4) = 4
      do i=0,5
         ipart2(i)=ipart(i)
      enddo
      ipart2(4) = 1
      ipart2(5) = inverse(ipart(4))
      buff = JVVV2_head//coup1//JVVV2_tail
      call add4vertex(ipart2,buff,info2,icoup)

      do i=1,5
         ipart2(i)=ipart(i)
      enddo
      buff = VVVV_head1//coup1//VVVV_tail
      call add4vertex(ipart2,buff,info2,icoup)  !Send the W3W3 version

      info2(1) = 3
      info2(2) = 1
      info2(3) = 2
      info2(4) = 4
      do i=0,5
         ipart2(i)=ipart(i)
      enddo
      ipart2(4) = 1
      ipart2(5) = inverse(ipart(4))
      buff = JVVV3_head//coup1//JVVV3_tail
      call add4vertex(ipart2,buff,info2,icoup)

      do i=1,5
         ipart2(i)=ipart(i)
      enddo
      buff = VVVV_head2//coup1//VVVV_tail
      call add4vertex(ipart2,buff,info2,icoup)  !Send the W3W3 version

      endif
      end

      subroutine addWWWW1(ipart,coup1,coup2,info,icoup)
c**************************************************************************
c     Sets up the WWWW vertex with all the permutations
c     info is the color info for the WWWW configuration
c     coup1 is coupling of W w/ boson 1
c     coup2 is coupling of W w/ boson 2
c     Order of particles should be W- A W+ A
c     icoup is the coupling constant
c*************************************************************************
      implicit none

! Constants

      include 'params.inc'
      character*(*) VVVV_head                          
      parameter    (VVVV_head='WWWWXX(W(1,1???),W(1,2???),'//
     &                               'W(1,3???),W(1,4???),')
      character*(*) VVVV_tail                          
      parameter    (VVVV_tail=',AMP(????))')
      character*(*) JVVV1_head 
      parameter    (JVVV1_head='JWWWXX(W(1,1???),W(1,2???),W(1,3???),')
      character*(*) JVVV1_tail 
      parameter    (JVVV1_tail=',W(1,4???))')
      character*(*) JVVV2_head 
      parameter    (JVVV2_head='JWWWXX(W(1,2???),W(1,1???),W(1,4???),')
      character*(*) JVVV2_tail 
      parameter    (JVVV2_tail=',W(1,3???))')
      character*(*) JVVV3_head 
      parameter    (JVVV3_head='JWWWXX(W(1,1???),W(1,4???),W(1,3???),')
      character*(*) JVVV3_tail 
      parameter    (JVVV3_tail=',W(1,2???))')
      character*(*) JVVV4_head 
      parameter    (JVVV4_head='JWWWXX(W(1,2???),W(1,3???),W(1,4???),')
      character*(*) JVVV4_tail 
      parameter    (JVVV4_tail=',W(1,1???))')

c      character*(*) WMASS        , WWIDTH
c      parameter    (WMASS='WMASS', WWIDTH='WWIDTH')
      
! Arguments

      character*(*) coup1,coup2
      integer info(0:4),icoup(0:max_coup),ipart(0:5)

! Local
      
      integer ipart2(0:5),i
      character*(8) vmass1,vwidth1,vmass2,vwidth2,wmass3,wwidth3
      character*(max_string) buff


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
      if (ipart(0) .eq. 4) then
         wmass3  = str(2,ipart(1))
         wwidth3 = str(3,ipart(1))
         vmass1  = str(2,ipart(2))
         vwidth1 = str(3,ipart(2))
         vmass2  = str(2,ipart(4))
         vwidth2 = str(3,ipart(4))
         do i=0,5
            ipart2(i)=ipart(i)
         enddo
         buff = VVVV_head//coup1//','//coup2//VVVV_tail
         call add4vertex(ipart2,buff,info,icoup) !Send the WWWW version

      do i=0,5
         ipart2(i)=ipart(i)
      enddo
      ipart2(4)=1
      ipart2(5)=inverse(ipart(4))

      buff = JVVV1_head//coup1//','//coup2//','
     $        //vmass2//','//vwidth2//JVVV1_tail
      call add4vertex(ipart2,buff,info,icoup)

      if (ipart(3) .ne. ipart(4)) then       
         do i=0,5
            ipart2(i)=ipart(i)
         enddo
         ipart2(3) = 1
         ipart2(5) = inverse(ipart(3))
         buff = JVVV2_head//coup2//','//coup1//','
     $        //wmass3//','//wwidth3//JVVV2_tail
         call add4vertex(ipart2,buff,info,icoup)
      endif

      if (ipart(2) .ne. ipart(3) .and. ipart(2) .ne. ipart(4)) then       
         do i=0,5
            ipart2(i)=ipart(i)
         enddo
         ipart2(2) = 1
         ipart2(5) = inverse(ipart(2))
         buff = JVVV3_head//coup2//','//coup1//','//vmass1//','
     $        //vwidth1//JVVV3_tail
         call add4vertex(ipart2,buff,info,icoup)
      endif

      if ( ipart(1) .ne. ipart(2) .and.
     $     ipart(1) .ne. ipart(3) .and. ipart(1) .ne. ipart(4)) then       
         do i=0,5
            ipart2(i)=ipart(i)
         enddo
         ipart2(1) = 1
         ipart2(5) = inverse(ipart(1))
         buff = JVVV4_head//coup1//','//coup2//','
     $        //wmass3//','//wwidth3//JVVV4_tail
         call add4vertex(ipart2,buff,info,icoup)
      endif

      else
         print*,'Sorry must have 4 particles in W3W3',(ipart(i),i=0,4)
      endif
      end

      subroutine addW3W31(ipart,coup1,coup2,info,icoup)
c**************************************************************************
c     Sets up the W3W3 vertex with all the permutations
c     info is the color info for the W3W3 configuration
c     coup1 is coupling of W w/ boson 1
c     coup2 is coupling of W w/ boson 2
c     Order of particles should be W- A W+ A
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
      character*(*) VVVV_head                          
      parameter    (VVVV_head='W3W3XX(W(1,1???),W(1,2???),'//
     &                               'W(1,3???),W(1,4???),')
      character*(*) VVVV_tail                          
      parameter    (VVVV_tail=',AMP(????))')
      character*(*) JVVV1_head 
      parameter    (JVVV1_head='JW3WXX(W(1,1???),W(1,2???),W(1,3???),')
      character*(*) JVVV1_tail 
      parameter    (JVVV1_tail=',W(1,4???))')
      character*(*) JVVV2_head 
      parameter    (JVVV2_head='JW3WXX(W(1,2???),W(1,1???),W(1,4???),')
      character*(*) JVVV2_tail 
      parameter    (JVVV2_tail=',W(1,3???))')
      character*(*) JVVV3_head 
      parameter    (JVVV3_head='JW3WXX(W(1,1???),W(1,4???),W(1,3???),')
      character*(*) JVVV3_tail 
      parameter    (JVVV3_tail=',W(1,2???))')
      character*(*) JVVV4_head 
      parameter    (JVVV4_head='JW3WXX(W(1,2???),W(1,3???),W(1,4???),')
      character*(*) JVVV4_tail 
      parameter    (JVVV4_tail=',W(1,1???))')

c      character*(*) WMASS        , WWIDTH
c      parameter    (WMASS='WMASS', WWIDTH='WWIDTH')
      
! Arguments

      character*(*) coup1,coup2
      integer info(0:4),icoup(0:max_coup),ipart(0:5)

! Local
      
      integer ipart2(0:5),i
      character*(8) vmass1,vwidth1,vmass2,vwidth2,wmass3,wwidth3
      character*(max_string) buff


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
      if (ipart(0) .eq. 4) then
      vmass1  = str(2,ipart(2))
      vwidth1 = str(3,ipart(2))
      vmass2  = str(2,ipart(4))
      vwidth2 = str(3,ipart(4))
      wmass3  = str(2,ipart(3))
      wwidth3 = str(3,ipart(3))
      do i=0,5
         ipart2(i)=ipart(i)
      enddo
      buff = VVVV_head//coup1//','//coup2//VVVV_tail
      call add4vertex(ipart2,buff,info,icoup)  !Send the W3W3 version

      do i=0,5
         ipart2(i)=ipart(i)
      enddo
      ipart2(4)=1
      ipart2(5)=inverse(ipart(4))

      buff = JVVV1_head//coup1//','//coup2//','
     $       //vmass2//','//vwidth2//JVVV1_tail

c      write(*,*) 'Adding',buff

      call add4vertex(ipart2,buff,info,icoup)

      if (ipart(3) .ne. ipart(4)) then       
         do i=0,5
            ipart2(i)=ipart(i)
         enddo
         ipart2(3) = 1
         ipart2(5) = inverse(ipart(3))
         buff = JVVV2_head//coup2//','//coup1//','
     $        //wmass3//','//wwidth3//JVVV2_tail
         call add4vertex(ipart2,buff,info,icoup)
      endif

      if (ipart(2) .ne. ipart(3) .and. ipart(2) .ne. ipart(4)) then       
         do i=0,5
            ipart2(i)=ipart(i)
         enddo
         ipart2(2) = 1
         ipart2(5) = inverse(ipart(2))
         buff = JVVV3_head//coup2//','//coup1//','
     $        //vmass1//','//vwidth1//JVVV3_tail
         call add4vertex(ipart2,buff,info,icoup)
      endif

      if ( ipart(1) .ne. ipart(2) .and.
     $     ipart(1) .ne. ipart(3) .and. ipart(1) .ne. ipart(4)) then       
         do i=0,5
            ipart2(i)=ipart(i)
         enddo
         ipart2(1) = 1
         ipart2(5) = inverse(ipart(1))
         buff = JVVV4_head//coup1//','//coup2//','
     $        //wmass3//','//wwidth3//JVVV4_tail
         call add4vertex(ipart2,buff,info,icoup)
      endif

      else
         print*,'Sorry must have 4 particles in W3W3',(ipart(i),i=0,4)
      endif
      end

      subroutine addVVSS1(ipart,coup,info,icoup)
c**************************************************************************
c     Sets up the VVSS vertex with all the permutations
c     info is the color info for the VVSS configuration
c     coup is coupling of W w/ boson 1
c     Order of particles should be VVSS
c     icoup is the coupling constant
c*************************************************************************
      implicit none

! Constants

      include 'params.inc'
      character*(*) VVSS_head                          
      parameter    (VVSS_head='VVSSXX(W(1,1???),W(1,2???),'//
     &                               'W(1,3???),W(1,4???),')
      character*(*) VVSS_tail                          
      parameter    (VVSS_tail=',AMP(????))')
      character*(*) HVVS1_head 
      parameter    (HVVS1_head='HVVSXX(W(1,1???),W(1,2???),W(1,3???),')
      character*(*) HVVS1_tail 
      parameter    (HVVS1_tail=',W(1,4???))')
      character*(*) HVVS2_head 
      parameter    (HVVS2_head='HVVSXX(W(1,2???),W(1,1???),W(1,4???),')
      character*(*) HVVS2_tail 
      parameter    (HVVS2_tail=',W(1,3???))')
      character*(*) JVSS1_head 
      parameter    (JVSS1_head='JVSSXX(W(1,1???),W(1,4???),W(1,3???),')
      character*(*) JVSS1_tail 
      parameter    (JVSS1_tail=',W(1,2???))')
      character*(*) JVSS2_head 
      parameter    (JVSS2_head='JVSSXX(W(1,2???),W(1,3???),W(1,4???),')
      character*(*) JVSS2_tail 
      parameter    (JVSS2_tail=',W(1,1???))')

      
! Arguments

      character*(*) coup
      integer info(0:4),icoup(0:max_coup),ipart(0:5)

! Local
      
      integer ipart2(0:5),i
      character*(8) vmass,vwidth,smass,swidth
      character*(max_string) buff


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
      if (ipart(0) .eq. 4) then
      do i=0,5
         ipart2(i)=ipart(i)
      enddo
      buff = VVSS_head//coup//VVSS_tail
      call add4vertex(ipart2,buff,info,icoup)  !Send the VVSS version

      do i=0,5
         ipart2(i)=ipart(i)
      enddo
      ipart2(4)=1
      ipart2(5)=inverse(ipart(4))
      smass  = str(2,ipart(4))
      swidth = str(3,ipart(4))

      buff = HVVS1_head//coup//','//smass//','//swidth//HVVS1_tail
      call add4vertex(ipart2,buff,info,icoup)

      if (ipart(3) .ne. ipart(4)) then       
         do i=0,5
            ipart2(i)=ipart(i)
         enddo
         smass  = str(2,ipart(3))
         swidth = str(3,ipart(3))
         ipart2(3) = 1
         ipart2(5) = inverse(ipart(3))
         buff=HVVS2_head//coup//','//smass//','//swidth//HVVS2_tail
         call add4vertex(ipart2,buff,info,icoup)
      endif

      do i=0,5
         ipart2(i)=ipart(i)
      enddo
      ipart2(2) = 1
      ipart2(5) = inverse(ipart(2))
      vmass  = str(2,ipart(2))
      vwidth = str(3,ipart(2))
      buff = JVSS1_head//coup//','//vmass//','//vwidth//JVSS1_tail
      call add4vertex(ipart2,buff,info,icoup)

      if (ipart(1) .ne. ipart(2)) then       
         do i=0,5
            ipart2(i)=ipart(i)
         enddo
         vmass  = str(2,ipart(1))
         vwidth = str(3,ipart(1))
         ipart2(1) = 1
         ipart2(5) = inverse(ipart(1))
         buff=JVSS2_head//coup//','//vmass//','//vwidth//JVSS2_tail
         call add4vertex(ipart2,buff,info,icoup)
      endif

      else
         print*,'Sorry must have 4 particles in W3W3',(ipart(i),i=0,4)
      endif
      end


c RF 10 Apr. 2006 changed this subroutine to call VVVS-vertices with two coupling constants.
      subroutine addVVVS1(ipart,coup1,coup2,info,icoup)
c**************************************************************************
c     Sets up the VVVS vertex with all the permutations
c     info is the color info for the VVVS configuration
c     coup is coupling of W w/ boson 1
c     Order of particles should be VVVS
c     icoup is the coupling constant
c*************************************************************************
      implicit none

! Constants

      include 'params.inc'
      character*(*) VVVS_head                          
      parameter    (VVVS_head='VVVSXX(W(1,1???),W(1,2???),'//
     &                               'W(1,3???),W(1,4???),')
      character*(*) VVVS_tail                          
      parameter    (VVVS_tail=',AMP(????))')
      character*(*) HVVV1_head 
      parameter    (HVVV1_head='HVVVXX(W(1,1???),W(1,2???),W(1,3???),')
      character*(*) HVVV1_tail 
      parameter    (HVVV1_tail=',W(1,4???))')
c      character*(*) JVVS1_head 
c      parameter    (JVVS1_head='JVVSXX(W(1,1???),W(1,3???),W(1,4???),')
c      character*(*) JVVS1_tail 
c      parameter    (JVVS1_tail=',W(1,2???))')
c RF 22 Mar. 2006
      character*(*) JVVS1_head 
      parameter    (JVVS1_head='JVVSXX(W(1,1???),W(1,2???),W(1,4???),')
      character*(*) JVVS1_tail 
      parameter    (JVVS1_tail=',W(1,3???))')
cend RF
      character*(*) JVVS2_head 
c      parameter    (JVVS2_head='JVSSXX(W(1,2???),W(1,3???),W(1,4???),')
      parameter    (JVVS2_head='JVVSXX(W(1,2???),W(1,3???),W(1,4???),')
      character*(*) JVVS2_tail 
      parameter    (JVVS2_tail=',W(1,1???))')

      
! Arguments

      character*(*) coup1, coup2
      integer info(0:4),icoup(0:max_coup),ipart(0:5)

! Local
      
      integer ipart2(0:5),i
      character*(8) vmass,vwidth,smass,swidth
      character*(max_string) buff


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
c
c     Calculate amplitude
c
      if (ipart(0) .eq. 4) then
      do i=0,5
         ipart2(i)=ipart(i)
      enddo
      buff = VVVS_head//coup1//','//coup2//VVVS_tail
c      buff = VVVS_head//coup//VVVS_tail
      call add4vertex(ipart2,buff,info,icoup)  !Send the VVVS version
c
c     Now setup to calculate scalar field
c
      do i=0,5
         ipart2(i)=ipart(i)
      enddo
      ipart2(4)=1
      ipart2(5)=inverse(ipart(4))
      smass  = str(2,ipart(4))
      swidth = str(3,ipart(4))

      buff = HVVV1_head//coup1//','//coup2//','//smass//','
     $     //swidth//HVVV1_tail
c      buff = HVVV1_head//coup//','//smass//','//swidth//HVVV1_tail
      call add4vertex(ipart2,buff,info,icoup)
c
c     Now setup to calculate vector field particle 2
c
      do i=0,5
         ipart2(i)=ipart(i)
      enddo
      ipart2(3) = 1
      ipart2(5) = inverse(ipart(3))
      vmass  = str(2,ipart(3))
      vwidth = str(3,ipart(3))
      buff = JVVS1_head//coup1//','//coup2//','//vmass//','//
     $     vwidth//JVVS1_tail
c      buff = JVVS1_head//coup//','//vmass//','//vwidth//JVVS1_tail
c
ctjsx  3-21-2006     
c     If this is all octets, we need to change f^abc to f^acb since the
c     order of the particles has changed in the call

      if (info(0) .eq. 3) then   !f^12a f^a34  switch to f^12a f^a43
         info(3) = 4
         info(4) = 3
      endif

      call add4vertex(ipart2,buff,info,icoup)
c
ctjsx  3-21-2006     
c     If this is all octets, we need to change f^abc to f^acb since the
c     order of the particles has changed in the call

      if (info(0) .eq. 3) then   !switch back to f^12a f^a34  
         info(3) = 3
         info(4) = 4
      endif


c
c     Now setup to calculate vector field particle 1 (if diff from 2)
c     not implemented yet
      
      if (ipart(1) .ne. ipart(2) .or. ipart(1) .ne. ipart(3)) then
         write(*,*) 'Error in nvert4.f'
         write(*,*) 'VVVS interaction only implemented for identical V'
!         pause
      endif
      if (ipart(1) .ne. ipart(2)) then       
         do i=0,5
            ipart2(i)=ipart(i)
         enddo
         vmass  = str(2,ipart(1))
         vwidth = str(3,ipart(1))
         ipart2(1) = 1
         ipart2(5) = inverse(ipart(1))
         buff=JVVS2_head//coup1//','//coup2//','//vmass//','//
     $        vwidth//JVVS2_tail
c         buff=JVVS2_head//coup//','//vmass//','//vwidth//JVVS2_tail
         call add4vertex(ipart2,buff,info,icoup)
      endif

      else
         print*,'Sorry must have 4 particles in W3W3',(ipart(i),i=0,4)
      endif
      end





c RF 23 Mar. 2006 added the ttss vertex:

      subroutine addTTSS1(ipart,coup1,coup2,info,icoup)
c**************************************************************************
c     Sets up the TTSS vertex with all the permutations
c     of the color structure
c*************************************************************************
      implicit none

! Constants

      include 'params.inc'
      character*(*) TTSS_head                          
      parameter    (TTSS_head='TTSSXX(W(1,1???),W(1,2???),'//
     &                               'W(1,3???),W(1,4???),')
      character*(*) TTSS_tail                          
      parameter    (TTSS_tail=',AMP(????))')
      character*(*) HTTS1_head 
      parameter    (HTTS1_head='HTTSXX(W(1,1???),W(1,2???),W(1,3???),')
      character*(*) HTTS1_tail 
      parameter    (HTTS1_tail=',W(1,4???))')
      character*(*) HTTS2_head 
      parameter    (HTTS2_head='HTTSXX(W(1,1???),W(1,2???),W(1,4???),')
      character*(*) HTTS2_tail 
      parameter    (HTTS2_tail=',W(1,3???))')
      character*(*) UTSS1_head 
      parameter    (UTSS1_head='UTSSXX(W(1,1???),W(1,3???),W(1,4???),')
      character*(*) UTSS1_tail 
      parameter    (UTSS1_tail=',W(1,2???))')
      character*(*) UTSS2_head 
      parameter    (UTSS2_head='UTSSXX(W(1,2???),W(1,3???),W(1,4???),')
      character*(*) UTSS2_tail 
      parameter    (UTSS2_tail=',W(1,1???))')

! Arguments

      character*(*) coup1,coup2
      integer info(0:4),icoup(0:max_coup),ipart(0:5)

! Local
      
      integer ipart2(0:5),i,info2(0:4)
      character*(8) smass,swidth,vmass,vwidth
      character*(max_string) buff


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
c-----
      if (ipart(0) .eq. 4) then
      do i=0,5
         ipart2(i)=ipart(i)
      enddo
      buff = TTSS_head//coup1//','//coup2//TTSS_tail
      call add4vertex(ipart2,buff,info,icoup)  !Send the TTSS version


      do i=0,5
         ipart2(i)=ipart(i)
      enddo
      ipart2(4)=1
      ipart2(5)=inverse(ipart(4))
      smass  = str(2,ipart(4))
      swidth = str(3,ipart(4))

      buff = HTTS1_head//coup1//','//coup2//','
     &                         //smass//','//swidth//HTTS1_tail
      call add4vertex(ipart2,buff,info,icoup)

      if (ipart(3) .ne. ipart(4)) then       
         do i=0,5
            ipart2(i)=ipart(i)
         enddo
         smass  = str(2,ipart(4))
         swidth = str(3,ipart(4))
         ipart2(3) = 1
         ipart2(5) = inverse(ipart(3))
         buff=HTTS2_head//coup1//','//coup2//','
     &                         //smass//','//swidth//HTTS2_tail
         call add4vertex(ipart2,buff,info,icoup)
      endif

      do i=0,5
         ipart2(i)=ipart(i)
      enddo
      ipart2(2) = 1
      ipart2(5) = inverse(ipart(2))
      vmass  = str(2,ipart(2))
      vwidth = str(3,ipart(2))
      buff = UTSS1_head//coup1//','//coup2//','
     &                       //vmass//','//vwidth//UTSS1_tail
      call add4vertex(ipart2,buff,info,icoup)

      if (ipart(1) .ne. ipart(2)) then       
         do i=0,5
            ipart2(i)=ipart(i)
         enddo
         vmass  = str(2,ipart(1))
         vwidth = str(3,ipart(1))
         ipart2(1) = 1
         ipart2(5) = inverse(ipart(1))
         buff=UTSS2_head//coup1//','//coup2//','
     &                        //vmass//','//vwidth//UTSS2_tail
         call add4vertex(ipart2,buff,info,icoup)
      endif

      else
         print*,'Sorry must have 4 particles in TTSS',(ipart(i),i=0,4)
      endif
      end

cend RF


c RF 10 Apr. 2006,
c bug fixed by RF 10 Jul. 2006

      subroutine addSSSS1(ipart,coup,info,icoup)
c**************************************************************************
c     Sets up the SSSS vertex with all the permutations
c     info is the color info for the SSSS configuration
c     coup is coupling
c     icoup is the coupling constant
c*************************************************************************
      implicit none

! Constants

      include 'params.inc'
      character*(*) SSSS_head                          
      parameter    (SSSS_head='SSSSXX(W(1,1???),W(1,2???),'//
     &                               'W(1,3???),W(1,4???),')
      character*(*) SSSS_tail                          
      parameter    (SSSS_tail=',AMP(????))')
      character*(*) HSSS1_head 
      parameter    (HSSS1_head='HSSSXX(W(1,1???),W(1,2???),W(1,3???),')
      character*(*) HSSS1_tail 
      parameter    (HSSS1_tail=',W(1,4???))')
      character*(*) HSSS2_head 
      parameter    (HSSS2_head='HSSSXX(W(1,1???),W(1,2???),W(1,4???),')
      character*(*) HSSS2_tail 
      parameter    (HSSS2_tail=',W(1,3???))')
      character*(*) HSSS3_head 
      parameter    (HSSS3_head='HSSSXX(W(1,1???),W(1,3???),W(1,4???),')
      character*(*) HSSS3_tail 
      parameter    (HSSS3_tail=',W(1,2???))')
      character*(*) HSSS4_head 
      parameter    (HSSS4_head='HSSSXX(W(1,2???),W(1,3???),W(1,4???),')
      character*(*) HSSS4_tail 
      parameter    (HSSS4_tail=',W(1,1???))')

      
! Arguments

      character*(*) coup
      integer info(0:4),icoup(0:max_coup),ipart(0:5)

! Local
      
      integer ipart2(0:5),i
      character*(8) vmass1,vwidth1,vmass2,vwidth2,
     &              vmass3,vwidth3,vmass4,vwidth4
      character*(max_string) buff


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
      if (ipart(0) .eq. 4) then
         vmass3  = str(2,ipart(3))
         vwidth3 = str(3,ipart(3))
         vmass1  = str(2,ipart(1))
         vwidth1 = str(3,ipart(1))
         vmass2  = str(2,ipart(2))
         vwidth2 = str(3,ipart(2))
         vmass4  = str(2,ipart(4))
         vwidth4 = str(3,ipart(4))
         do i=0,5
            ipart2(i)=ipart(i)
         enddo
         buff = SSSS_head//coup//SSSS_tail
         call add4vertex(ipart2,buff,info,icoup) !Send the SSSS version

      do i=0,5
         ipart2(i)=ipart(i)
      enddo
      ipart2(4)=1
      ipart2(5)=inverse(ipart(4))

      buff = HSSS1_head//coup//','
     $                //vmass4//','//vwidth4//HSSS1_tail
      call add4vertex(ipart2,buff,info,icoup)

      if (ipart(3) .ne. ipart(4)) then       
         do i=0,5
            ipart2(i)=ipart(i)
         enddo
         ipart2(3) = 1
         ipart2(5) = inverse(ipart(3))
         buff = HSSS2_head//coup//','
     $                   //vmass3//','//vwidth3//HSSS2_tail
         call add4vertex(ipart2,buff,info,icoup)
      endif

      if (ipart(2) .ne. ipart(3) .and. ipart(2) .ne. ipart(4)) then       
         do i=0,5
            ipart2(i)=ipart(i)
         enddo
         ipart2(2) = 1
         ipart2(5) = inverse(ipart(2))
         buff = HSSS3_head//coup//','
     $                   //vmass2//','//vwidth2//HSSS3_tail
         call add4vertex(ipart2,buff,info,icoup)
      endif

      if ( ipart(1) .ne. ipart(3) .and.
     $     ipart(1) .ne. ipart(3) .and. ipart(1) .ne. ipart(4)) then       
         do i=0,5
            ipart2(i)=ipart(i)
         enddo
         ipart2(1) = 1
         ipart2(5) = inverse(ipart(1))
         buff = HSSS4_head//coup//','
     $                   //vmass1//','//vwidth1//HSSS4_tail
         call add4vertex(ipart2,buff,info,icoup)
      endif
      

      else
         print*,'Sorry must have 4 particles in SSSS',(ipart(i),i=0,4)
      endif
      end

cend RF




c RF 29 Mar. 2006
      subroutine addVVVT1(ipart,coup1,coup2,info,icoup)
c**************************************************************************
c     Sets up the VVVT vertex with all the permutations
c     info is the color info for the VVVS configuration
c     coup is coupling of W w/ boson 1
c     Order of particles should be VVVS
c     icoup is the coupling constant
c*************************************************************************
      implicit none

! Constants

      include 'params.inc'
      character*(*) VVVT_head                          
      parameter    (VVVT_head='VVVTXX(W(1,1???),W(1,2???),'//
     &                               'W(1,3???),W(1,4???),')
      character*(*) VVVT_tail                          
      parameter    (VVVT_tail=',AMP(????))')
      character*(*) UVVV1_head 
      parameter    (UVVV1_head='UVVVXX(W(1,1???),W(1,2???),W(1,3???),')
      character*(*) UVVV1_tail 
      parameter    (UVVV1_tail=',W(1,4???))')
      character*(*) JVVT1_head 
      parameter    (JVVT1_head='JVVTXX(W(1,1???),W(1,2???),W(1,4???),')
      character*(*) JVVT1_tail 
      parameter    (JVVT1_tail=',W(1,3???))')
      character*(*) JVVT2_head 
      parameter    (JVVT2_head='JVVTXX(W(1,2???),W(1,3???),W(1,4???),')
      character*(*) JVVT2_tail 
      parameter    (JVVT2_tail=',W(1,1???))')

      
! Arguments

      character*(*) coup1,coup2
      integer info(0:4),icoup(0:max_coup),ipart(0:5)

! Local
      
      integer ipart2(0:5),i
      character*(8) vmass,vwidth,smass,swidth
      character*(max_string) buff


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
c
c     Calculate amplitude
c
      if (ipart(0) .eq. 4) then
      do i=0,5
         ipart2(i)=ipart(i)
      enddo
      buff = VVVT_head//coup1//','//coup2//VVVT_tail
      call add4vertex(ipart2,buff,info,icoup)  !Send the VVVT version
c
c     Now setup to calculate scalar field
c
      do i=0,5
         ipart2(i)=ipart(i)
      enddo
      ipart2(4)=1
      ipart2(5)=inverse(ipart(4))
      smass  = str(2,ipart(4))
      swidth = str(3,ipart(4))

      buff = UVVV1_head//coup1//','//coup2//','//smass//','
     $     //swidth//UVVV1_tail
      call add4vertex(ipart2,buff,info,icoup)   !Calculate Scalar 
c
c     Now setup to calculate vector field particle 2
c
      do i=0,5
         ipart2(i)=ipart(i)
      enddo
      ipart2(3) = 1
      ipart2(5) = inverse(ipart(3))
      vmass  = str(2,ipart(3))
      vwidth = str(3,ipart(3))
      buff = JVVT1_head//coup1//','//coup2//','//vmass//','//
     $     vwidth//JVVT1_tail

c
ctjsx  3-21-2006     
c     If this is all octets, we need to change f^abc to f^acb since the
c     order of the particles has changed in the call

      if (info(0) .eq. 3) then   !f^12a f^a34  switch to f^12a f^a43
         info(3) = 4
         info(4) = 3
      endif

      call add4vertex(ipart2,buff,info,icoup)
c
ctjsx  3-21-2006     
c     If this is all octets, we need to change f^abc to f^acb since the
c     order of the particles has changed in the call

      if (info(0) .eq. 3) then   !switch back to f^12a f^a34  
         info(3) = 3
         info(4) = 4
      endif


c
c     Now setup to calculate vector field particle 1 (if diff from 2)
c     not implemented yet
      
      if (ipart(1) .ne. ipart(2) .or. ipart(1) .ne. ipart(3)) then
         write(*,*) 'Error in nvert4.f'
         write(*,*) 'VVVS interaction only implemented for identical V'
!         pause
      endif
      if (ipart(1) .ne. ipart(2)) then       
         do i=0,5
            ipart2(i)=ipart(i)
         enddo
         vmass  = str(2,ipart(1))
         vwidth = str(3,ipart(1))
         ipart2(1) = 1
         ipart2(5) = inverse(ipart(1))
         buff=JVVT2_head//coup1//','//coup2//','//vmass//','//
     $        vwidth//JVVT2_tail
         call add4vertex(ipart2,buff,info,icoup)
      endif

      else
         print*,'Sorry must have 4 particles in W3W3',(ipart(i),i=0,4)
      endif
      end





cend RF


c*********************************KEK***********************************************
      subroutine addFFVT1(ipart,coup1,coup2,info,icoup)
c**************************************************************************
c     Sets up the FFVT vertex with all the permutations
c     info is the color info for the FFVT configuration
c     coup is coupling 
c     Order of particles should be FFVT
c     icoup is the coupling constant
c*************************************************************************
      implicit none

! Constants

      include 'params.inc'
      character*(*) IOVT_head                          
      parameter    (IOVT_head='IOVTXX(W(1,1???),W(1,2???),'//
     &                               'W(1,3???),W(1,4???),')
      character*(*) IOVT_tail                          
      parameter    (IOVT_tail=',AMP(????))')
      character*(*) UIOV1_head 
      parameter    (UIOV1_head='UIOVXX(W(1,1???),W(1,2???),W(1,3???),')
      character*(*) UIOV1_tail 
      parameter    (UIOV1_tail=',W(1,4???))')
      character*(*) JIOT1_head 
      parameter    (JIOT1_head='JIOTXX(W(1,1???),W(1,2???),W(1,4???),')
      character*(*) JIOT1_tail 
      parameter    (JIOT1_tail=',W(1,3???))')
      character*(*) FVTI1_head 
      parameter    (FVTI1_head='FVTIXX(W(1,1???),W(1,3???),W(1,4???),')
      character*(*) FVTI1_tail 
      parameter    (FVTI1_tail=',W(1,2???))')
      character*(*) FVTO1_head 
      parameter    (FVTO1_head='FVTOXX(W(1,2???),W(1,3???),W(1,4???),')
      character*(*) FVTO1_tail 
      parameter    (FVTO1_tail=',W(1,1???))')
      
! Arguments

      character*(*) coup1,coup2
      integer info(0:4),icoup(0:max_coup),ipart(0:5)

! Local
      
      integer ipart2(0:5),i
      character*(8) fmass,fwidth,vmass,vwidth,ymass,ywidth
      character*(max_string) buff


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
c
c     Calculate amplitude
c
      if (ipart(0) .eq. 4) then
      do i=0,5
         ipart2(i)=ipart(i)
      enddo
      buff = IOVT_head//coup1//','//coup2//IOVT_tail
      call add4vertex(ipart2,buff,info,icoup)  !Send the IOVT version
c
c
      do i=0,5
         ipart2(i)=ipart(i)
      enddo
      ipart2(4)=1
      ipart2(5)=inverse(ipart(4))
      ymass  = str(2,ipart(4))
      ywidth = str(3,ipart(4))

      buff = UIOV1_head//coup1//','//coup2//','//ymass//','
     $     //ywidth//UIOV1_tail
      call add4vertex(ipart2,buff,info,icoup)  
c
c     Now setup to calculate vector field particle 
c
      do i=0,5
         ipart2(i)=ipart(i)
      enddo
      ipart2(3) = 1
      ipart2(5) = inverse(ipart(3))
      vmass  = str(2,ipart(3))
      vwidth = str(3,ipart(3))
      buff = JIOT1_head//coup1//','//coup2//','//vmass//','//
     $     vwidth//JIOT1_tail

      call add4vertex(ipart2,buff,info,icoup)

c
c     Now setup to calculate Flowing OUT Fermion 
c
      
      do i=0,5
         ipart2(i)=ipart(i)
      enddo
      ipart2(2) = 1
      ipart2(5) = inverse(ipart(2))
      fmass  = str(2,ipart(2))
      fwidth = str(3,ipart(2))
      buff = FVTI1_head//coup1//','//coup2//','//fmass//','//
     $     fwidth//FVTI1_tail

      call add4vertex(ipart2,buff,info,icoup)

c
c     Now setup to calculate Flowing IN Fermion 
c
      
      do i=0,5
         ipart2(i)=ipart(i)
      enddo
      ipart2(1) = 1
      ipart2(5) = inverse(ipart(1))
      fmass  = str(2,ipart(1))
      fwidth = str(3,ipart(1))
      buff = FVTO1_head//coup1//','//coup2//','//fmass//','//
     $     fwidth//FVTO1_tail

      call add4vertex(ipart2,buff,info,icoup)
      endif
      end

c****************************KEK*************************************


