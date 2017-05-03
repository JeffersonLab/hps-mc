!-----------------------
! madgraph - a Feynman Diagram package by Tim Stelzer and Bill Long 
! (c) 1993
!
! Filename: QCD.f
!-----------------------

      subroutine addQCD
c***********************************************************************
c     Add Vertices for QCD interactions
c     q q~ g  : ggg : gggg
c     coup(1)   = QCD
c     coup(2)   = QED
c***********************************************************************
      implicit none

! Constants

      include 'params.inc'
c      integer    max_coup
c      parameter (max_coup=5)
c
c     Local
c
      character*80 buff
      integer info(0:4),coup(max_coup)
      integer i,g,ipart(0:5)
c-----
c  Begin Code
c-----      
      do i=1,max_coup
         coup(i)=0
      enddo
      coup(1) = 1

c     q~qg
      info(0) = 1          !T(glue,q,q~)
      info(1) = 2
      info(2) = 3
      info(3) = 1
      call addFFV('d~dg','GG ',info,coup)
      call addFFV('u~ug','GG ',info,coup)
      call addFFV('s~sg','GG ',info,coup)
      call addFFV('c~cg','GG ',info,coup)
      call addFFV('b~bg','GG ',info,coup)
      call addFFV('t~tg','GG ',info,coup)

c     ggg
      info(0) = 2         !F(1,2,3)
      info(1) = 1
      info(2) = 2
      info(3) = 3
      call addVVV('ggg','G ',info,coup)
c      call addMFV('rrg','GG',info,coup)
      call addFFV('rrg','GG',info,coup)

c     gggg
c
c     Here we are doing special things to deal with color structure of gggg
c     interaction. Essentially need to add 3 diagrams every time vertex
c     shows up.
c
      coup(1) = 2
      info(0) = 3         !F(1,2,4)F(2,3,4)

      info(1) = 1
      info(2) = 2
      info(3) = 3
      info(4) = 4
      call getparticle('g ',ipart(1),ipart(0))
      g = ipart(1)
      ipart(0)=4
      do i=1,5
         ipart(i)=g
      enddo
      ipart(4) = 1
      buff = 'JGGGXX(W(1,1???),W(1,2???),W(1,3???),G ,W(1,4???))'
      call add4vertex(ipart,buff,info,coup)
      info(1) = 2
      info(2) = 3
      info(3) = 1
      info(4) = 4
      ipart(0)=4
      do i=1,5
         ipart(i)=g
      enddo
      ipart(4) = 1
      buff = 'JGGGXX(W(1,3???),W(1,1???),W(1,2???),G ,W(1,4???))'
      call add4vertex(ipart,buff,info,coup)
      info(1) = 3
      info(2) = 1
      info(3) = 2
      info(4) = 4
      ipart(0)=4
      do i=1,5
         ipart(i)=g
      enddo
      ipart(4) = 1
      buff = 'JGGGXX(W(1,2???),W(1,3???),W(1,1???),G ,W(1,4???))'
      call add4vertex(ipart,buff,info,coup)

      do i=1,4
         ipart(i)=g
      enddo
      ipart(5)= 1
      info(1) = 1
      info(2) = 2
      info(3) = 3
      info(4) = 4
      buff = 'GGGGXX(W(1,1???),W(1,2???),W(1,3???),W(1,4???),'//
     &     'G ,AMP(????))'
      call add4vertex(ipart,buff,info,coup)
      info(1) = 2
      info(2) = 3
      info(3) = 1
      info(4) = 4
      buff = 'GGGGXX(W(1,3???),W(1,1???),W(1,2???),W(1,4???),'//
     &     'G ,AMP(????))'
c      buff = 'GGGGXX(W3???,W1???,W2???,W4???,G ,AMP(????))'
      call add4vertex(ipart,buff,info,coup)
      info(1) = 3
      info(2) = 1
      info(3) = 2
      info(4) = 4
      buff = 'GGGGXX(W(1,2???),W(1,3???),W(1,1???),W(1,4???),'//
     &     'G ,AMP(????))'
c      buff = 'GGGGXX(W2???,W3???,W1???,W4???,G ,AMP(????))'
      call add4vertex(ipart,buff,info,coup)
      end
