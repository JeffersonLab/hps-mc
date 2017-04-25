!-----------------------
! madgraph - a Feynman Diagram package by Tim Stelzer and Bill Long 
! (c) 1993
!
! Filename: qed.f
!-----------------------

      subroutine addQED
c***********************************************************************
c     Add Vertices for QED interactions
c     l l~ photon 
c
c     structure for 3 particle vertices
c     call add3vertex(known_buff,Helas_buff,Calc_buff,info,coup)
c     where:
c          Known_buff is the particles already calculated 'e+e-a' or 'e+e-??'
c          Helas_Buff is the helas call with W1???,W2???,W3??? the 3 wf's
c          Calc_buff  is the particle code of the calculated wf.
c          info(0)    is the type of color interaction
c                     =0  -> No color structure    ie e+e-a
c                     =1  -> T matrix              ie qq~g
c                     =2  -> F(a,b,c) Matrix       ie ggg
c                     =3  -> F(a,b,e)F(c,d,e)      ie gggg
c                     =4  -> delta(a,b)            ie qq~a
c          info(1)    is position of the first particle in the color structure
c          info(2)    is position of the second particle in the color structure
c          info(3)    is position of the third particle in the color structure
c          info(4)    is position of the fourth particle in the color structure
c          coup       is the order of the different coupling constants
c          coup(1)   = QCD
c          coup(2)   = QED
c***********************************************************************
      implicit none

! Constants

      include 'params.inc'
c      integer    max_coup
c      parameter (max_coup=5)
c
c     Local
c
      integer info(0:4),coup(max_coup)
      integer i
c-----
c  Begin Code
c-----      
      do i=1,4
         info(i)=-999
      enddo
      do i=1,max_coup
         coup(i)=0
      enddo
      coup(2) = 1

c     qq~a
      info(0) = 4             !delta(a,q,q~)     a is dummy         
      info(1) = 2
      info(2) = 3
      info(3) = 1
      if (.true.) then
      call addFFV('d~da','GAD ',info,coup)
      call addFFV('u~ua','GAU ',info,coup)
      call addFFV('s~sa','GAD ',info,coup)
      call addFFV('c~ca','GAU ',info,coup)
      call addFFV('b~ba','GAD ',info,coup)
      call addFFV('t~ta','GAU ',info,coup)

c     l+l-a
      info(0) = 0
      call addFFV('e+e-a','GAL ',info,coup)
      call addFFV('mu+mu-a','GAL ',info,coup)
      call addFFV('ta+ta-a','GAL ',info,coup)
      endif

      end
