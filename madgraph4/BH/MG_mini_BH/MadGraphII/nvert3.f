      subroutine addFFV1(ipart,coup,info,icoup)
c**************************************************************************
c     Sets up the IOV vertex with all the permutations
c     info is the color info for the IOV configuration
c     icoup is the coupling constant
c*************************************************************************
      implicit none

! Constants

      include 'params.inc'
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
      
! Arguments

      character*(*) coup
      integer info(0:4),icoup(0:max_coup),ipart(0:4)

! Local
      
      integer ipart2(0:4),i
      character*(max_string) buff
      character*8 fmass,fwidth,vmass,vwidth


! Global

      character*(4*max_particles) particle(4)
      integer                               charge_c(max_particles)
      integer iparticle(0:max_particles,0:4),inverse(max_particles)
      common/to_model/iparticle,  particle,  inverse, charge_c

      character*(max_string) iwave(max_particles),owave(max_particles)
      character*(8) str(3,max_particles)
      integer info_p(5,max_particles),iposx(3,max_particles)
      common/to_external/iwave,owave,iposx,info_p,str
      integer                          imaja
      logical                cc,passcc
      common/set_chargeconjg/cc,passcc,imaja

c-----
c  Begin Code
c-----

c      if (.not. cc) then
c      passcc=.false.
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

c      endif
c      if (imaja .eq. 2 .or. .not. cc) then
c      passcc=.true.
      do i=0,4
         ipart2(i)=ipart(i)
      enddo
      ipart2(2) = 1                                !2nd particle needed O
      ipart2(4) = inverse(ipart(2))                !get antiparticle
      fmass  = str(2,ipart(2))                     !Mass of Fermion
      fwidth = str(3,ipart(2))                     !Width of fermion
      buff = FVI_head//coup//','//fmass//','//fwidth//FVI_tail
      call add3vertex(ipart2,buff,info,icoup)      !Send the FVIXXX version
c      endif

c      if (imaja .eq. 1 .or. .not. cc) then
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
c      endif
      end

      subroutine addFFS1(ipart,coup,info,icoup)
c**************************************************************************
c     Sets up the FFS vertex with all the permutations (q~qh) is order
c     info is the color info for the IOV configuration
c     icoup is the coupling constant
c*************************************************************************
      implicit none

! Constants

      include 'params.inc'
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

      character*(*) coup
      integer info(0:4),icoup(0:max_coup),ipart(0:4)

! Local
      
      integer ipart2(0:4),i
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
      integer                          imaja
      logical                cc,passcc
      common/set_chargeconjg/cc,passcc,imaja

c-----
c  Begin Code
c-----
c      passcc=.false.
c      if (.not. cc) then
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
c      endif

c      if (.not. cc .or. imaja .eq. 2) then
c      passcc=.true.
      do i=0,4
         ipart2(i)=ipart(i)
      enddo
      ipart2(2) = 1
      ipart2(4) = inverse(ipart(2))
      fmass  = str(2,ipart(2))
      fwidth = str(3,ipart(2))
      buff = FSI_head//coup//','//fmass//','//fwidth//FSI_tail
      call add3vertex(ipart2,buff,info,icoup)
c      endif

c      if (.not. cc .or. imaja .eq. 1) then
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
c      endif
      end

      subroutine addVVS1(ipart,coup,info,icoup)
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

      character*(*) coup
      integer info(0:4),icoup(0:max_coup),ipart(0:4)

! Local
      
      integer ipart2(0:4),i
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
      do i=0,4
         ipart2(i)=ipart(i)
      enddo
      smass  = str(2,ipart(3))
      swidth = str(3,ipart(3))
      buff = VVS_head//coup//VVS_tail

c
c     Here I need to swap the color for the two scalars because
c     they get read in opposite of the colors for the FFV routine
c

c      do i=1,3
c         if (info(i) .eq. 2) then
c            info(i)=3
c         elseif (info(i).eq.3) then
c            info(i)=2
c         endif
c      enddo

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
c
ctjsx  3-21-2006     
c     If this is all octets, we need to change f^abc to f^acb since the
c     order of the particles has changed in the call
c
      if (info(0) .eq. 2) then   !f^123  switch to f^132
         info(2) = 3
         info(3) = 2
      endif
      call add3vertex(ipart2,buff,info,icoup)    !This is VS  -> V
      if (info(0) .eq. 2) then   !switch back f^123
c        info(2) = 3
c        info(3) = 2
c RF 22 Mar. 2006 
c     I think it should be this:
         info(2) = 2
         info(3) = 3
cend RF

      endif

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

      subroutine addVSS1(ipart,coup,info,icoup)
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

      character*(*) coup
      integer info(0:4),icoup(0:max_coup),ipart(0:4)

! Local
      
      integer ipart2(0:4),i
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
      do i=0,4
         ipart2(i)=ipart(i)
      enddo
      smass  = str(2,ipart(3))
      swidth = str(3,ipart(3))

      buff = VSS_head//coup//VSS_tail
      call add3vertex(ipart2,buff,info,icoup)  !Send the VSS version

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

      subroutine addSSS1(ipart,coup,info,icoup)
c**************************************************************************
c     Sets up the SSS vertex with all the permutations (hhh) is order
c     info is the color info for the IOV configuration
c     icoup is the coupling constant
c
c     bug fixed 8/26/2005 typeO in permuting scalars for ipart(2)=1
c
c*************************************************************************
      implicit none

! Constants

      include 'params.inc'
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

      character*(*) coup
      integer info(0:4),icoup(0:max_coup),ipart(0:4)

! Local
      
      integer ipart2(0:4),i
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
      do i=0,4
         ipart2(i)=ipart(i)
      enddo
      buff = SSS_head//coup//SSS_tail
      call add3vertex(ipart2,buff,info,icoup)  !Send the SSS version

      do i=0,4
         ipart2(i)=ipart(i)
      enddo
      smass  = str(2,ipart(3))
      swidth = str(3,ipart(3))
      buff = HSS_head1//coup//','//smass//','//swidth//HSS_tail1

      ipart2(3) = 1                    !Unknown particle
      ipart2(4) = inverse(ipart(3))    !Result particle
      call add3vertex(ipart2,buff,info,icoup)  !Send the HVS version

      do i=0,4
         ipart2(i)=ipart(i)
      enddo
      if (ipart(2) .ne. ipart(3)) then
         smass  = str(2,ipart(2))
         swidth = str(3,ipart(2))
         ipart2(2)=1
         ipart2(4)=inverse(ipart(2))   
         buff = HSS_head2//coup//','//smass//','//swidth//HSS_tail2
         call add3vertex(ipart2,buff,info,icoup)
      endif

      do i=0,4
         ipart2(i)=ipart(i)
      enddo

      if (ipart(1) .ne. ipart(3) .and. ipart(1) .ne. ipart(2)) then
         ipart2(1) = 1
         ipart2(4) = inverse(ipart(1))
         smass  = str(2,ipart(1))
         swidth = str(3,ipart(1))
         buff = HSS_head3//coup//','//smass//','//swidth//HSS_tail3
         call add3vertex(ipart2,buff,info,icoup)
      endif

      end

      subroutine addVVV1(ipart,coup,info,icoup)
c**************************************************************************
c     Sets up the VVV vertex with all the permutations
c     info is the color info for the VVV configuration
c     icoup is the coupling constant
c     should be sent in the form w- w+ v
c*************************************************************************
      implicit none

! Constants

      include 'params.inc'
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

      character*(*) coup
      integer info(0:4),icoup(0:max_coup),ipart(0:4)

! Local
      
      integer ipart2(0:4),i
      character*(max_string) buff
      character*8 v1mass,v1width,v2mass,v2width,v3mass,v3width


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
      do i=0,4
         ipart2(i)=ipart(i)
      enddo
      v3mass  = str(2,ipart(3))
      v3width = str(3,ipart(3))
      v1mass  = str(2,ipart(1))
      v1width = str(3,ipart(1))
      v2mass  = str(2,ipart(2))
      v2width = str(3,ipart(2))

      buff = VVV_head//coup//VVV_tail
      call add3vertex(ipart2,buff,info,icoup)  !Send the VVV version

      do i=0,4
         ipart2(i)=ipart(i)
      enddo
      ipart2(3)=1                             !Unknown particle
      ipart2(4)=inverse(ipart(3))             !Result particle
      buff = JVV_head//coup//','//v3mass//','//v3width//JVV_tail
      call add3vertex(ipart2,buff,info,icoup)

      if (ipart(2) .ne. ipart(3)) then      !Not 3 identical particles
         do i=0,4
            ipart2(i)=ipart(i)
         enddo
         ipart2(2) = 1
         ipart2(4) = inverse(ipart(2))
         buff = JVV2_head//coup//','//v2mass//','//v2width//JVV2_tail
         call add3vertex(ipart2,buff,info,icoup)
      endif
c RF 25 Aug 2006
      if (ipart(1).ne. ipart(3) .and. ipart(1) .ne. ipart(2)) then
         do i=0,4
            ipart2(i)=ipart(i)
         enddo
         ipart2(1) = 1
         ipart2(4) = inverse(ipart(1))
         buff = JVV3_head//coup//','//v1mass//','//v1width//JVV3_tail
         call add3vertex(ipart2,buff,info,icoup)
      endif
cend RF

      end




c RF 23 Mar. 2006:  included the tts and vvt subroutines:

      subroutine addTTS1(ipart,coup,info,icoup)
c**************************************************************************
c     Sets up the TTS vertex with all the permutations (uuh) is order
c     info is the color info for the IOV configuration
c     icoup is the coupling constant
c
c     bug fixed 8/26/2005 typeO in permuting scalars for ipart(2)=1
c
c*************************************************************************
      implicit none

! Constants

      include 'params.inc'
      character*(*) TTS_head                          
      parameter    (TTS_head='TTSXXX(W(1,1???),W(1,2???),W(1,3???),')
      character*(*) TTS_tail                          
      parameter    (TTS_tail=',AMP(????))')
      character*(*) HTT_head1
      parameter    (HTT_head1='HTTXXX(W(1,1???),W(1,2???),')
      character*(*) HTT_tail1
      parameter    (HTT_tail1=',W(1,3???))')
      character*(*) UTS_head2
      parameter    (UTS_head2='UTSXXX(W(1,1???),W(1,3???),')
      character*(*) UTS_tail2
      parameter    (UTS_tail2=',W(1,2???))')
      character*(*) UTS_head3
      parameter    (UTS_head3='UTSXXX(W(1,2???),W(1,3???),')
      character*(*) UTS_tail3
      parameter    (UTS_tail3=',W(1,1???))')
      
! Arguments

      character*(*) coup
      integer info(0:4),icoup(0:max_coup),ipart(0:4)

! Local
      
      integer ipart2(0:4),i
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
      do i=0,4
         ipart2(i)=ipart(i)
      enddo
      buff = TTS_head//coup//TTS_tail
      call add3vertex(ipart2,buff,info,icoup)  !Send the TTS version

      do i=0,4
         ipart2(i)=ipart(i)
      enddo
      smass  = str(2,ipart(3))
      swidth = str(3,ipart(3))
      buff = HTT_head1//coup//','//smass//','//swidth//HTT_tail1
      ipart2(3) = 1                    !Unknown particle
      ipart2(4) = inverse(ipart(3))    !Result particle
      call add3vertex(ipart2,buff,info,icoup)  !Send the HTT version

      do i=0,4
         ipart2(i)=ipart(i)
      enddo
      if (ipart(2) .ne. ipart(3)) then
         smass  = str(2,ipart(2))
         swidth = str(3,ipart(2))
         ipart2(2)=1
         ipart2(4)=inverse(ipart(2))
         buff = UTS_head2//coup//','//smass//','//swidth//UTS_tail2
         call add3vertex(ipart2,buff,info,icoup)
      endif

      do i=0,4
         ipart2(i)=ipart(i)
      enddo

      if (ipart(1) .ne. ipart(2)) then
         smass  = str(2,ipart(1))
         swidth = str(3,ipart(1))
         ipart2(1) = 1
         ipart2(4) = inverse(ipart(1))
         buff = UTS_head3//coup//','//smass//','//swidth//UTS_tail3
         call add3vertex(ipart2,buff,info,icoup)
      endif

      end




      subroutine addVVT1(ipart,coup,info,icoup)
c**************************************************************************
c     Sets up the VVT vertex with all the permutations (jju) is order
c     info is the color info for the VVS configuration
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
      character*(*) VVT_head                          
      parameter    (VVT_head='VVTXXX(W(1,1???),W(1,2???),W(1,3???),')
      character*(*) VVT_tail                          
      parameter    (VVT_tail=',AMP(????))')
      character*(*) UVV_head
      parameter    (UVV_head='UVVXXX(W(1,1???),W(1,2???),')
      character*(*) UVV_tail
      parameter    (UVV_tail=',W(1,3???))')
      character*(*) JVT_head1
      parameter    (JVT_head1='JVTXXX(W(1,1???),W(1,3???),')
      character*(*) JVT_tail1
      parameter    (JVT_tail1=',W(1,2???))')
      character*(*) JVT_head2
      parameter    (JVT_head2='JVTXXX(W(1,2???),W(1,3???),')
      character*(*) JVT_tail2
      parameter    (JVT_tail2=',W(1,1???))')
      
! Arguments

      character*(*) coup
      integer info(0:4),icoup(0:max_coup),ipart(0:4)

! Local
      
      integer ipart2(0:4),i
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
      do i=0,4
         ipart2(i)=ipart(i)
      enddo
c****************************KEK*************************************
      smass  = str(2,ipart(1))                     !Mass of Vector
      swidth = str(3,ipart(1))    
      buff = VVT_head//coup//','//smass//VVT_tail
c****************************KEK************************************* 
c      buff = VVT_head//coup//VVT_tail

      call add3vertex(ipart2,buff,info,icoup)  !Send the VVT version

      do i=0,4
         ipart2(i)=ipart(i)
      enddo
      ipart2(3)=1
      ipart2(4)=inverse(ipart(3))
      smass  = str(2,ipart(3))
      swidth = str(3,ipart(3))
      vmass  = str(2,ipart(2))
      buff = UVV_head//coup//','//vmass//','//smass//
     $','//swidth//UVV_tail
      call add3vertex(ipart2,buff,info,icoup)   !VV -> T

      do i=0,4
         ipart2(i)=ipart(i)
      enddo
      ipart2(2) = 1
      ipart2(4) = inverse(ipart(2))
      vmass  = str(2,ipart(2))
      vwidth = str(3,ipart(2))
      buff = JVT_head1//coup//','//vmass//','//vwidth//JVT_tail1
c
ctjsx  3-21-2006     
c     If this is all octets, we need to change f^abc to f^acb since the
c     order of the particles has changed in the call
c
      if (info(0) .eq. 2) then   !f^123  switch to f^132
         info(2) = 3
         info(3) = 2
      endif
      call add3vertex(ipart2,buff,info,icoup)    !This is VT  -> V
      if (info(0) .eq. 2) then   !switch back f^123
         info(2) = 2
         info(3) = 3
      endif


      do i=0,4
         ipart2(i)=ipart(i)
      enddo
      if (ipart(1) .ne. ipart(2)) then
         ipart2(1) = 1
         ipart2(4) = inverse(ipart(1))
         vmass  = str(2,ipart(1))
         vwidth = str(3,ipart(1))
         buff = JVT_head2//coup//','//vmass//','//vwidth//JVT_tail2
         call add3vertex(ipart2,buff,info,icoup)
      endif
      end

cend RF




c RF 29 Mar. 2006
      subroutine addSST1(ipart,coup,info,icoup)
c**************************************************************************
c     Sets up the TTS vertex with all the permutations (uuh) is order
c     info is the color info for the IOV configuration
c     icoup is the coupling constant
c
c     bug fixed 8/26/2005 typeO in permuting scalars for ipart(2)=1
c
c*************************************************************************
      implicit none

! Constants

      include 'params.inc'
      character*(*) SST_head                          
      parameter    (SST_head='SSTXXX(W(1,1???),W(1,2???),W(1,3???),')
      character*(*) SST_tail                          
      parameter    (SST_tail=',AMP(????))')
      character*(*) USS_head1
      parameter    (USS_head1='USSXXX(W(1,1???),W(1,2???),')
      character*(*) USS_tail1
      parameter    (USS_tail1=',W(1,3???))')
      character*(*) HST_head2
      parameter    (HST_head2='HSTXXX(W(1,1???),W(1,3???),')
      character*(*) HST_tail2
      parameter    (HST_tail2=',W(1,2???))')
      character*(*) HST_head3
      parameter    (HST_head3='HSTXXX(W(1,2???),W(1,3???),')
      character*(*) HST_tail3
      parameter    (HST_tail3=',W(1,1???))')
      
! Arguments

      character*(*) coup
      integer info(0:4),icoup(0:max_coup),ipart(0:4)

! Local
      
      integer ipart2(0:4),i
      character*(max_string) buff
      character*8 smass,swidth,hmass


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
      do i=0,4
         ipart2(i)=ipart(i)
      enddo
c****************************KEK************************************
      smass  = str(2,ipart(1))                     !Mass of Scalar
      swidth = str(3,ipart(1))    
      buff = SST_head//coup//','//smass//SST_tail
c****************************KEK************************************* 
c     buff = SST_head//coup//SST_tail
      call add3vertex(ipart2,buff,info,icoup)  !Send the SST version

      do i=0,4
         ipart2(i)=ipart(i)
      enddo
      smass  = str(2,ipart(3))
      swidth = str(3,ipart(3))
      hmass  = str(2,ipart(2))
c****************************KEK*************************************
       buff = USS_head1//coup//','//hmass//','
     $//smass//','//swidth//USS_tail1
c****************************KEK*************************************
      ipart2(3) = 1                    !Unknown particle
      ipart2(4) = inverse(ipart(3))    !Result particle
      call add3vertex(ipart2,buff,info,icoup)  !Send the USS version

      do i=0,4
         ipart2(i)=ipart(i)
      enddo
      if (ipart(2) .ne. ipart(3)) then
         smass  = str(2,ipart(2))
         swidth = str(3,ipart(2))
         ipart2(2)=1
         ipart2(4)=inverse(ipart(2))
         buff = HST_head2//coup//','//smass//','//swidth//HST_tail2
         call add3vertex(ipart2,buff,info,icoup)
      endif

      do i=0,4
         ipart2(i)=ipart(i)
      enddo

      if (ipart(1) .ne. ipart(2)) then
         smass  = str(2,ipart(1))
         swidth = str(3,ipart(1))
         ipart2(1) = 1
         ipart2(4) = inverse(ipart(1))
         buff = HST_head3//coup//','//smass//','//swidth//HST_tail3
         call add3vertex(ipart2,buff,info,icoup)
      endif

      end
c*******************************************************************************

cend RF



c****************************KEK****************************************************************************************
      subroutine addFFT1(ipart,coup,info,icoup)
c**************************************************************************
c     Sets up the IOV vertex with all the permutations
c     info is the color info for the IOV configuration
c     icoup is the coupling constant
c*************************************************************************
      implicit none

! Constants

      include 'params.inc'
      character*(*) IOT_head                          
      parameter    (IOT_head='IOTXXX(W(1,1???),W(1,2???),W(1,3???),')
      character*(*) IOT_tail                          
      parameter    (IOT_tail=',AMP(????))')
      character*(*) UIO_head                   
      parameter    (UIO_head='UIOXXX(W(1,1???),W(1,2???),')
      character*(*) UIO_tail                   
      parameter    (UIO_tail=',W(1,3???))')
      character*(*) FTO_head                
      parameter    (FTO_head='FTOXXX(W(1,2???),W(1,3???),')
      character*(*) FTO_tail
      parameter    (FTO_tail=',W(1,1???))')
      character*(*) FTI_head 
      parameter    (FTI_head='FTIXXX(W(1,1???),W(1,3???),')
      character*(*) FTI_tail
      parameter    (FTI_tail=',W(1,2???))')
      
! Arguments

      character*(*) coup
      integer info(0:4),icoup(0:max_coup),ipart(0:4)

! Local
      
      integer ipart2(0:4),i
      character*(max_string) buff
      character*8 fmass,fwidth,ymass,ywidth


! Global

      character*(4*max_particles) particle(4)
      integer                               charge_c(max_particles)
      integer iparticle(0:max_particles,0:4),inverse(max_particles)
      common/to_model/iparticle,  particle,  inverse, charge_c

      character*(max_string) iwave(max_particles),owave(max_particles)
      character*(8) str(3,max_particles)
      integer info_p(5,max_particles),iposx(3,max_particles)
      common/to_external/iwave,owave,iposx,info_p,str
      integer                          imaja
      logical                cc,passcc
      common/set_chargeconjg/cc,passcc,imaja

c-----
c  Begin Code
c-----

c      if (.not. cc) then
c      passcc=.false.
      do i=0,4
         ipart2(i)=ipart(i)                        
      enddo
      fmass  = str(2,ipart(1))                     !Mass of Fermion
      fwidth = str(3,ipart(1))    
      buff = IOT_head//coup//','//fmass//IOT_tail
      call add3vertex(ipart2,buff,info,icoup)      !Send the IOT version

      do i=0,4
         ipart2(i)=ipart(i)
      enddo
      ipart2(3)=1                                  !3rd particle needed T
      ipart2(4)=inverse(ipart(3))                  !get antiparticle
      ymass  = str(2,ipart(3))                     !Mass of Tensor Boson
      ywidth = str(3,ipart(3))                     !Width of Tensor Boson
      buff = UIO_head//coup//','//fmass//','
     $//ymass//','//ywidth//UIO_tail
      call add3vertex(ipart2,buff,info,icoup)      !Send the UIOXXX version

c      endif
c      if (imaja .eq. 2 .or. .not. cc) then
c      passcc=.true.
      do i=0,4
         ipart2(i)=ipart(i)
      enddo
      ipart2(2) = 1                                !2nd particle needed O
      ipart2(4) = inverse(ipart(2))                !get antiparticle
      fmass  = str(2,ipart(2))                     !Mass of Fermion
      fwidth = str(3,ipart(2))                     !Width of fermion
      buff = FTI_head//coup//','//fmass//','//fwidth//FTI_tail
      call add3vertex(ipart2,buff,info,icoup)      !Send the FTIXXX version
c      endif

c      if (imaja .eq. 1 .or. .not. cc) then
      do i=0,4
         ipart2(i)=ipart(i)
      enddo
      if (ipart(1) .ne. ipart(2)) then             !Only if two particles diff
         ipart2(1) = 1                             !1st particle needed I
         ipart2(4) = inverse(ipart(1))             !get antiparticle
         fmass  = str(2,ipart(1))                  !Mass of Fermion
         fwidth = str(3,ipart(1))                  !Width of fermion
         buff = FTO_head//coup//','//fmass//','//fwidth//FTO_tail
         call add3vertex(ipart2,buff,info,icoup)   !Send the FTOXXX version
      endif
c      endif
      end
c****************************KEK*************************************
