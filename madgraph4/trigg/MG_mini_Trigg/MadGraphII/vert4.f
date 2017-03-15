!-----------------------
! madgraph - a Feynman Diagram package by Tim Stelzer and Bill Long 
! (c) 1993
!
! Filename: vert4.f
!-----------------------
 
      subroutine check4v(l1,l2,l3,l4,n,fail,more,oneit,ngraphs,color)
c***************************************************************************
c     Subroutine to check if vertex exists with lines l1 l2 and l3
c     If one does than pass is true, and more is set if another possible
c     vertex exists.  The line is than written to the screen and any
c     unknown particles are given the appropriate code
c***************************************************************************
      implicit none
c
c     Constants
c
      include 'params.inc'
c
c     Arguments
c
      integer l1,l2,l3,l4,n,ngraphs
      logical fail,more,oneit
      integer color(0:2*maxlines,0:maxfactors,0:maxterms)
c
c     Local
c
      integer iorder(4),ll(4),li(4),i,iorder2(4),nli(4),iplace
      character*(max_string) buff
c
c     Global
c
      integer   coup4(0:max_coup,maxv4)
      integer                         ipos4(5,maxv4), info4(0:4,maxv4)
      character*(max_string) v4wave(maxv4)
      common/to_internal4/    v4wave,  ipos4         , info4,coup4

      integer        iline(-maxlines:maxlines),idir(-maxlines:maxlines)
      integer        this_coup(max_coup) ,goal_coup(max_coup)
      common/to_proc/iline,idir,this_coup,goal_coup

      character*(10)            coup_name(max_coup)
      integer             ncoups
      common/to_couplings/ncoups,coup_name

      integer wnum(-maxlines:maxlines),wcount,wlen(maxgraphs*maxlines)
      integer alen(maxgraphs)
      common/toopt/wcount,wnum,wlen,alen

      logical          this_block(0:max_blocks), need_block(max_blocks)
      common/to_blocks/this_block              , need_block

      character*(max_string) wstrings(maxgraphs*maxlines)
      character*(max_string) astrings(maxgraphs)
      common/tocode/wstrings,astrings

c
c     External 
c
      integer v4

c-----
c  Begin Code
c-----
      oneit = .true.
      more  = .false.
      ll(1)=iline(l1)
      ll(2)=iline(l2)
      ll(3)=iline(l3)
      ll(4)=iline(l4)
      li(1)= l1
      li(2)= l2
      li(3)= l3
      li(4)= l4
      do i=1,4
         iorder(i) =i
         iorder2(i)=ll(i)
         nli(i) = li(i)
      enddo
      call sort2(ll,iorder,4)
      call sort2(iorder2,nli,4)
      iplace = v4(ll(1),ll(2),ll(3),ll(4),n)
      if (iplace .ne. 0) then
         fail = .false.
         buff = v4wave(iplace)
         do i=2,4
            call makestring(li(iorder(i)),
     $        buff(ipos4(i,iplace):ipos4(i,iplace)+3))
         enddo
         if (ll(1) .eq. 1) then           !Calculates Wavefuntion
            iline(li(iorder(1))) = ipos4(5,iplace)
            call optcheck(buff,li(iorder(1)),ipos4(1,iplace))
         else                             !Calculates Graph Amplitude
            oneit = .false.
            call makestring(li(iorder(1)),
     $        buff(ipos4(1,iplace):ipos4(1,iplace)+3))
            ngraphs=ngraphs+1
            call makestringamp(ngraphs,
     $           buff(ipos4(5,iplace):ipos4(5,iplace)+3))
            buff(ipos4(5,iplace)+4:ipos4(5,iplace)+5)='))'
            astrings(ngraphs)=buff(:ipos4(5,iplace)+5)
            alen(ngraphs) = ipos4(5,iplace)+5
            ngraphs=ngraphs-1
         endif
         if (v4(ll(1),ll(2),ll(3),ll(4),n+1) .ne. 0) then
            more = .true.
         endif
         do i=1,4
            iorder(i) = info4(i,iplace)
         enddo
         call sort2(iorder,nli,4)
         call createcolor(nli,info4(0,iplace),4,color)
         this_block(coup4(0,iplace))=.true.

c         write(*,*) 'Performing 4 particle',ncoups
c         write(*,'(4i4)') (this_coup(i),i=1,4)
c         write(*,'(4i4)') (coup4(i,iplace),i=1,4)
c         write(*,'(4i4)') (goal_coup(i),i=1,4)

         do i=1,ncoups
            this_coup(i)=this_coup(i)+coup4(i,iplace)
            if (this_coup(i) .gt. goal_coup(i)) then
               fail=.true.
c               write(*,*) 'Failed'
            endif
         enddo
      else
         fail = .true.
c         print*,'sorry no such vertex'
      endif
      end

      subroutine addWWWW(partbuff,coup1,coup2,info,icoup)
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

      character*(*) WMASS        , WWIDTH
      parameter    (WMASS='WMASS', WWIDTH='WWIDTH')
      
! Arguments

      character*(*) partbuff,coup1,coup2
      integer info(0:4),icoup(0:max_coup)

! Local
      
      integer ipart(0:5),ipart2(0:5),i
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
c      print*,'adding WWWW: ',partbuff
      call getparticle(partbuff,ipart(1),ipart(0)) !parse particle string
      if (ipart(0) .eq. 4) then
      vmass1  = str(2,ipart(2))
      vwidth1 = str(3,ipart(2))
      vmass2  = str(2,ipart(4))
      vwidth2 = str(3,ipart(4))
      do i=0,5
         ipart2(i)=ipart(i)
      enddo
      buff = VVVV_head//coup1//','//coup2//','
     $     //wmass//','//wwidth//VVVV_tail
      call add4vertex(ipart2,buff,info,icoup)  !Send the WWWW version

      do i=0,5
         ipart2(i)=ipart(i)
      enddo
      ipart2(4)=1
      ipart2(5)=inverse(ipart(4))

      buff = JVVV1_head//coup1//','//coup2//','
     $     //wmass//','//wwidth//','//vmass2//','
     $     //vwidth2//JVVV1_tail
      call add4vertex(ipart2,buff,info,icoup)

      if (ipart(3) .ne. ipart(4)) then       
         do i=0,5
            ipart2(i)=ipart(i)
         enddo
         ipart2(3) = 1
         ipart2(5) = inverse(ipart(3))
         buff = JVVV2_head//coup2//','//coup1//','
     $        //wmass//','//wwidth//','//wmass//','
     $        //wwidth//JVVV2_tail
         call add4vertex(ipart2,buff,info,icoup)
      endif

      if (ipart(2) .ne. ipart(3) .and. ipart(2) .ne. ipart(4)) then       
         do i=0,5
            ipart2(i)=ipart(i)
         enddo
         ipart2(2) = 1
         ipart2(5) = inverse(ipart(2))
         buff = JVVV3_head//coup2//','//coup1//','
     $        //wmass//','//wwidth//','//vmass1//','
     $        //vwidth1//JVVV3_tail
         call add4vertex(ipart2,buff,info,icoup)
      endif

      if ( ipart(1) .ne. ipart(3) .and.
     $     ipart(1) .ne. ipart(3) .and. ipart(1) .ne. ipart(4)) then       
         do i=0,5
            ipart2(i)=ipart(i)
         enddo
         ipart2(1) = 1
         ipart2(5) = inverse(ipart(1))
         buff = JVVV4_head//coup1//','//coup2//','
     $        //wmass//','//wwidth//','//wmass//','
     $        //wwidth//JVVV4_tail
         call add4vertex(ipart2,buff,info,icoup)
      endif

      else
         print*,'Sorry must have 4 particles in W3W3',(ipart(i),i=0,4)
      endif
      end

      subroutine addW3W3(partbuff,coup1,coup2,info,icoup)
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

      character*(*) WMASS        , WWIDTH
      parameter    (WMASS='WMASS', WWIDTH='WWIDTH')
      
! Arguments

      character*(*) partbuff,coup1,coup2
      integer info(0:4),icoup(0:max_coup)

! Local
      
      integer ipart(0:5),ipart2(0:5),i
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
c      print*,'adding W3W3: ',partbuff
      call getparticle(partbuff,ipart(1),ipart(0)) !parse particle string
      if (ipart(0) .eq. 4) then
      vmass1  = str(2,ipart(2))
      vwidth1 = str(3,ipart(2))
      vmass2  = str(2,ipart(4))
      vwidth2 = str(3,ipart(4))
      do i=0,5
         ipart2(i)=ipart(i)
      enddo
      buff = VVVV_head//coup1//','//coup2//','
     $     //wmass//','//wwidth//VVVV_tail
      call add4vertex(ipart2,buff,info,icoup)  !Send the W3W3 version

      do i=0,5
         ipart2(i)=ipart(i)
      enddo
      ipart2(4)=1
      ipart2(5)=inverse(ipart(4))

      buff = JVVV1_head//coup1//','//coup2//','
     $     //wmass//','//wwidth//','//vmass2//','
     $     //vwidth2//JVVV1_tail
      call add4vertex(ipart2,buff,info,icoup)

      if (ipart(3) .ne. ipart(4)) then       
         do i=0,5
            ipart2(i)=ipart(i)
         enddo
         ipart2(3) = 1
         ipart2(5) = inverse(ipart(3))
         buff = JVVV2_head//coup2//','//coup1//','
     $        //wmass//','//wwidth//','//wmass//','
     $        //wwidth//JVVV2_tail
         call add4vertex(ipart2,buff,info,icoup)
      endif

      if (ipart(2) .ne. ipart(3) .and. ipart(2) .ne. ipart(4)) then       
         do i=0,5
            ipart2(i)=ipart(i)
         enddo
         ipart2(2) = 1
         ipart2(5) = inverse(ipart(2))
         buff = JVVV3_head//coup2//','//coup1//','
     $        //wmass//','//wwidth//','//vmass1//','
     $        //vwidth1//JVVV3_tail
         call add4vertex(ipart2,buff,info,icoup)
      endif

      if ( ipart(1) .ne. ipart(3) .and.
     $     ipart(1) .ne. ipart(3) .and. ipart(1) .ne. ipart(4)) then       
         do i=0,5
            ipart2(i)=ipart(i)
         enddo
         ipart2(1) = 1
         ipart2(5) = inverse(ipart(1))
         buff = JVVV4_head//coup1//','//coup2//','
     $        //wmass//','//wwidth//','//wmass//','
     $        //wwidth//JVVV4_tail
         call add4vertex(ipart2,buff,info,icoup)
      endif

      else
         print*,'Sorry must have 4 particles in W3W3',(ipart(i),i=0,4)
      endif
      end

      subroutine addVVSS(partbuff,coup,info,icoup)
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
      parameter    (HVVS1_tail='W(1,4???))')
      character*(*) HVVS2_head 
      parameter    (HVVS2_head='HVVSXX(W(1,2???),W(1,1???),W(1,4???),')
      character*(*) HVVS2_tail 
      parameter    (HVVS2_tail='W(1,3???))')
      character*(*) JVSS1_head 
      parameter    (JVSS1_head='JVSSXX(W(1,1???),W(1,4???),W(1,3???),')
      character*(*) JVSS1_tail 
      parameter    (JVSS1_tail='W(1,2???))')
      character*(*) JVSS2_head 
      parameter    (JVSS2_head='JVSSXX(W(1,2???),W(1,3???),W(1,4???),')
      character*(*) JVSS2_tail 
      parameter    (JVSS2_tail='W(1,1???))')

      
! Arguments

      character*(*) partbuff,coup
      integer info(0:4),icoup(0:max_coup)

! Local
      
      integer ipart(0:5),ipart2(0:5),i
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
c      print*,'adding VVSS: ',partbuff
      call getparticle(partbuff,ipart(1),ipart(0)) !parse particle string
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

      buff = HVVS1_head//coup//','//smass//','//swidth//','//HVVS1_tail
      call add4vertex(ipart2,buff,info,icoup)

      if (ipart(3) .ne. ipart(4)) then       
         do i=0,5
            ipart2(i)=ipart(i)
         enddo
         smass  = str(2,ipart(4))
         swidth = str(3,ipart(4))
         ipart2(3) = 1
         ipart2(5) = inverse(ipart(3))
         buff=HVVS2_head//coup//','//smass//','//swidth//','//HVVS2_tail
         call add4vertex(ipart2,buff,info,icoup)
      endif

      do i=0,5
         ipart2(i)=ipart(i)
      enddo
      ipart2(2) = 1
      ipart2(5) = inverse(ipart(2))
      vmass  = str(2,ipart(2))
      vwidth = str(3,ipart(2))
      buff = JVSS1_head//coup//','//vmass//','//vwidth//','//JVSS1_tail
      call add4vertex(ipart2,buff,info,icoup)

      if (ipart(1) .ne. ipart(2)) then       
         do i=0,5
            ipart2(i)=ipart(i)
         enddo
         vmass  = str(2,ipart(1))
         vwidth = str(3,ipart(1))
         ipart2(1) = 1
         ipart2(5) = inverse(ipart(1))
         buff=JVSS2_head//coup//','//vmass//','//vwidth//','//JVSS2_tail
         call add4vertex(ipart2,buff,info,icoup)
      endif

      else
         print*,'Sorry must have 4 particles in W3W3',(ipart(i),i=0,4)
      endif
      end


      integer function packv4(i,j,k,l)
      implicit none

! Arguments

      integer i,j,k,l

! Local Variables

      integer ii,jj,kk,ll,temp

!-----------
!  Begin code
!-----------
      ii = i
      jj = j
      kk = k
      ll = l

      if (ii .gt. jj) then
         temp = ii
         ii = jj
         jj = temp
      end if
      if (jj .gt. kk) then
         temp = jj
         jj = kk
         kk = temp
      end if
      if (kk .gt. ll) then
         temp = kk
         kk = ll
         ll = temp
      end if
      if (ii .gt. jj) then
         temp = ii
         ii = jj
         jj = temp
      end if
      if (jj .gt. kk) then
         temp = jj
         jj = kk
         kk = temp
      end if
      if (ii .gt. jj) then
         temp = ii
         ii = jj
         jj = temp
      end if

      packv4 = ((ii*256 + jj)*256 + kk)*256 + ll
c      if ( ((ii*256+jj)*256+kk)*256+ll .ge. 2**32-1) then
c         write(*,*) ((ii*256 + jj)*256 + kk)*256 + ll, ii,jj,kk,ll
c         write(*,*) packv4
c      endif

      end

!------------------------------------
      integer function v4(l1,l2,l3,l4,n)
      implicit none

! Constants

      include 'params.inc'
c      integer   maxv4
c      parameter(maxv4=64)

! Arguments

      integer l1,l2,l3,l4,n

! Local Variables

      integer i,nfound,vertex

! External Functions

      integer packv4

! Global Variables

      integer           nv4,v4_table(maxv4)
      common /v4_block/ nv4,v4_table


!-----------
!  Begin code
!-----------
      vertex = packv4(l1,l2,l3,l4)
      nfound = 0
      do i=1,nv4
         if (vertex .eq. v4_table(i)) then
            nfound = nfound + 1
            if ( nfound .eq. n) then
               v4 = i
               return
            end if
         end if
      end do
      v4 = 0
      end

      subroutine add4vert(i,j,k,l,iplace)
      implicit none

! Constants

      include 'params.inc'
c      integer   maxv4
c      parameter(maxv4=64)

! Arguments

      integer i,j,k,l,iplace

! Local Variables

      integer vertex

! External Functions

      integer packv4

! Global Variables

      integer           nv4,v4_table(maxv4)
      common /v4_block/ nv4,v4_table

!-----------
! Begin Code
!-----------
      iplace=-1
      if (nv4 .eq. maxv4) then
         print*,' V4 vertex table size exceeded '
         stop
      end if
      vertex = packv4(i,j,k,l)
      nv4 = nv4 + 1
      v4_table(nv4) = vertex
      iplace = nv4
      end

      subroutine add4vertex(ipart,wavebuff1,info,icoup)
c***************************************************************************
c     subroutine to add a three particle vertex to madgraph
c     written 4/20/94
c***************************************************************************
      implicit none

! Constants

      include 'params.inc'

! Arguments

      character*(*) wavebuff1
      integer info(0:4),icoup(0:max_coup)

! Local

      integer ipart(0:5),iplace,iorder(4),i,iorder2(4)
      character*(max_string) wavebuff

! Global
      integer   coup4(0:max_coup,maxv4)
      integer                         ipos4(5,maxv4), info4(0:4,maxv4)
      character*(max_string) v4wave(maxv4)
      common/to_internal4/    v4wave,  ipos4         , info4,coup4

c RF 23 Mar. 2006
      integer           nchars
      character*12             postfix
      common/to_postfix/nchars,postfix
cend RF

!-----
!  Begin Code
!-----      
c      call getparticle(partbuff,ipart(1),ipart(0))   !parse particle string
      if (ipart(0) .eq. 4) then 
         do i=1,4
            iorder2(i)=i
            iorder(i) =i
         enddo
         call sort2(ipart(1),iorder2,4)
         call sort2(iorder2,iorder,4)
         call add4vert(ipart(1),ipart(2),ipart(3),ipart(4),iplace)
         if (iplace .eq. 0) then
            print*,'Error vertex not entered'
            return
         endif
c RF 23 Mar. 2006 Note also changed argument of subroutine to wavebuff1 (used to be wavebuff) 
         call add_postfix(wavebuff1,wavebuff,postfix,nchars)
cend RF

         v4wave(iplace) = wavebuff
         ipos4(iorder(1),iplace) = index(wavebuff,'W(1,1??')+4 
         ipos4(iorder(2),iplace) = index(wavebuff,'W(1,2??')+4 
         ipos4(iorder(3),iplace) = index(wavebuff,'W(1,3??')+4 
         ipos4(iorder(4),iplace) = index(wavebuff,'W(1,4??')+4 
         info4(0,iplace) = info(0)
         do i=1,4
            info4(iorder(i),iplace)=info(i)
         enddo
         do i=0,max_coup
            coup4(i,iplace) = icoup(i)
         enddo
         if (ipart(1) .ne. 1) then  !All 3 particles are known
            ipos4(5,iplace) = index(wavebuff,'AMP(?')+4
         else  !Store new particles code in position 5
            ipos4(5,iplace) = ipart(5)
c            print*,'New particle is',ipos4(5,iplace)
         endif
c         print*,'added ',ipart(5),ipart(1),ipart(2),ipart(3),ipart(4)
c         print*,wavebuff
      else
         print*,'Sorry only 4 particle vertices allowed'
         print*,ipart(0),ipart(1),ipart(2),ipart(3),ipart(4)
      endif
      end


