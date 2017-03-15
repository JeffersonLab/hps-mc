!-----------------------
! madgraph - a Feynman Diagram package by Tim Stelzer and Bill Long 
! (c) 1993
!
! Filename: qfd.f
!-----------------------

      subroutine addQFD
c***********************************************************************
c     Add Vertices for QCD interactions
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
c          info(2)    is position of the second particle in the color struct.
c          info(3)    is position of the third particle in the color structure
c          info(4)    is position of the fourth particle in the color struct.
c          coup       is the order of the different coupling constants
c          coup(1)   = QCD
c          coup(2)   = QED
c          coup(4)   = BRS
c          coup(5)   = SUSY
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

c     QUARKS:
      info(0) = 4             !delta(a,q,q~)     a is dummy         
      info(1) = 2
      info(2) = 3
      info(3) = 1

      if (.true.) then
c     qq~z
      call addFFV('d~dz','GZD ',info,coup)
      call addFFV('u~uz','GZU ',info,coup)
      call addFFV('s~sz','GZD ',info,coup)
      call addFFV('c~cz','GZU ',info,coup)
      call addFFV('b~bz','GZD ',info,coup)
      call addFFV('t~tz','GZU ',info,coup)

c     qq~h
      call addFFS('b~bh','GCHB ',info,coup)
      call addFFS('t~th','GCHT ',info,coup)
      endif

c     qq'w
      call addFFV('d~uw-','GWF ',info,coup)
      call addFFV('s~cw-','GWF ',info,coup)
      call addFFV('b~tw-','GWF ',info,coup)
      call addFFV('u~dw+','GWF ',info,coup)
      call addFFV('c~sw+','GWF ',info,coup)
      call addFFV('t~bw+','GWF ',info,coup)

c     LEPTONS:
      info(0) = 0             !no color structure
      if (.true.) then

c     l+l-z
      call addFFV('e+e-z','GZL ',info,coup)
      call addFFV('mu+mu-z','GZL ',info,coup)
      call addFFV('ta+ta-z','GZL ',info,coup)
      call addFFV('ve~vez','GZN ',info,coup)
      call addFFV('vm~vmz','GZN ',info,coup)
      call addFFV('vt~vtz','GZN ',info,coup)

c     l+ v w-
      call addFFV('vt~ta-w+','GWF ',info,coup)
      call addFFV('vm~mu-w+','GWF ',info,coup)
      call addFFV('ve~e- w+','GWF ',info,coup)
      call addFFV('ta+vtw-','GWF ',info,coup)
      call addFFV('mu+vmw-','GWF ',info,coup)
      call addFFV('e+ vew-','GWF ',info,coup)

c     BOSONS:

      call addVVV('w-w+a','GWWA ',info,coup)
      call addVVV('w-w+z','GWWZ ',info,coup)
      call addVVS('w-w+h','GWWH ',info,coup)
      call addVVS('zzh','GZZH ',info,coup)
      call addSSS('hhh','GHHH ',info,coup)

      coup(2) = 2
      call addw3w3('w-aw+a','GWWA ','GWWA ',info,coup)
      call addw3w3('w-zw+a','GWWZ ','GWWA ',info,coup)
      call addw3w3('w-zw+z','GWWZ ','GWWZ ',info,coup)
      call addwwww('w-w+w-w+','GWWZ ','GWWA ',info,coup)
      call addvvss('w-w+hh','GWWHH ',info,coup)
      call addvvss('zzhh','GZZHH ',info,coup)
c      call addssss('hhhh','GHHHH ',info,coup)

c     Ghosts:

c            Boson Ghost interactions
      coup(4) = 1
      coup(2) = 0
      if (.false.) then
      call addVVS('zw+x-','-GZWXP',info,coup)
      call addVVS('zw-x+','GZWXP',info,coup)
      call addVVS('aw+x-','-GAWXP',info,coup)
      call addVVS('aw-x+','GAWXP',info,coup)
      call addVSS('w+hx-','GWHX',info,coup)
      call addVSS('w-hx+','GWHX',info,coup)
      call addVSS('zhx3 ','GZHX',info,coup)
      coup(2) = 1
      call addvvss('zw+hx-','GZWHX',info,coup)
      call addvvss('zw-hx+','GZWHX',info,coup)
      call addvvss('aw+hx-','GAWHX',info,coup)
      call addvvss('aw-hx+','GAWHX',info,coup)

c            Fermion Ghost interactions

      call addFFS('e+ ve x-','GIOS(1,e,ve,xm)',info,coup)
      call addFFS('ve~ e- x+','GIOS(1,ve,e,xp)',info,coup)
      call addFFS('e+ e- x3','GIOS(1,e,e,x3)',info,coup)
      endif

      coup(2) = 1
      coup(4) = 0
      info(0) = 0             !No color for lepton sector
      call addFFS('e+ e- h','GIOS(1,e,e,h) ',info,coup)
      endif
      end
