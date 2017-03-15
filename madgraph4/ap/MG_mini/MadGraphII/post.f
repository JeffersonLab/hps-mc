!-----------------------
! madgraph - a Feynman Diagram package by Tim Stelzer and Bill Long 
! (c) 1993
!
! Filename: post.f
!-----------------------
      subroutine gen_ps(graphs,tops,iline)
c************************************************************************
c     Driver for writing out postscript code of Feynman diagrams
c************************************************************************
      implicit none
c
c     Constants
c
      include 'params.inc'
c      integer    graphs_per_row,   scale,    rows_per_page
c      parameter (graphs_per_row=3, scale=10, rows_per_page=5)
c      integer    maxtops,     maxnodes,  maxgraphs
c      parameter (maxtops=2500,maxnodes=5,maxgraphs=999)
c      integer    maxlines 
c      parameter (maxlines=8)
c      integer    max_particles
c      parameter (max_particles=2**7-1)
c
c     Arguments
c
      integer iline(0:maxlines)
      integer tops(0:4,0:maxnodes,0:maxtops)
      integer graphs(0:maxgraphs,0:maxlines) 
c
c     Local
c
      integer xplotsize,yplotsize,npages
      integer nswitch,i,k,next
c
c     Global
c
      character*25 name
      integer iname
      common/to_name/iname,name

      integer             switchline(maxlines,2**maxlines)
      common /to_majarana/switchline

      character*(4*max_particles) particle(4)
      integer                               charge_c(max_particles)
      integer iparticle(0:max_particles,0:4),inverse(max_particles)
      common/to_model/iparticle,  particle,  inverse, charge_c
c-----
c  Begin Code
c-----
c
c     Start by initializing line types needed
c
      print*,'Writing Feynman graphs in file ',name(1:iname)//'.ps'
      open(unit=4,file=name(1:iname)//'.ps',status='unknown')
      xplotsize = (graphs_per_row*15)*scale+150
      yplotsize = (rows_per_page*15)*scale+30
      npages    = int((graphs(0,0)-1)/(graphs_per_row*rows_per_page))
      npages    = npages +1
      call post_header(xplotsize,yplotsize,npages)
c
c     Loop over all orderings of Majarana Fermions
c
      nswitch = 1
      next= iline(0)
      if (graphs(0,0) .gt. max_draw) then
         write(*,*) 'Only drawing first ',max_draw,' diagrams.'
      endif
      do i=1,min(graphs(0,0),max_draw)
         do while (i .gt. switchline(maxlines,nswitch))
            do k=1,iline(0)
               if (switchline(k,nswitch)*(switchline(k,nswitch+1))
     $              .eq. -1) then
                  iline(k)=charge_c(iline(k)) !This line for crashing arrow
               endif
            enddo
            nswitch=nswitch+1
            if (nswitch .gt. 2**maxlines) then
               print*,'Error dont know orientation' 
               switchline(maxlines,nswitch)= maxgraphs
            endif
         enddo
         call PositionVerts(graphs,tops,i,next)
      enddo
      do k=1,iline(0)
         if (switchline(k,nswitch)*(switchline(k,1))
     $        .eq. -1) then
            iline(k)=charge_c(iline(k))
         endif
      enddo
      write(4,'(a)') 'showpage'
      write(4,'(a)') '%%trailer'
      close(4)
      end


      subroutine post_header(x,y,npages)
      implicit none
      integer x,y,npages

      write(4,10) '%!PS-Adobe-2.0'
      write(4,12) '%%BoundingBox: -20 -20 ',x,y+20
      write(4,10) '%%DocumentFonts: Helvetica'
      write(4,12) '%%Pages: ',npages
      write(4,10) '%!MadGraph'
      write(4,10) '%'
      write(4,10) '% Feynman Diagrams drawn by MadGraph'
      write(4,10) '%'
      write(4,10) '% Using FEYNMAN DRAW'
      write(4,10) '% A program by David J. Summers to draw'
      write(4,10) '% Feynman diagrams. (c) 1992'
      write(4,10) '% Version 2.7 (c) 1993'
      write(4,10) '%'
      write(4,10) '% Comments and questions to '
      write(4,10) '%  D.J.Summers@uk.ac.durham'
      write(4,10) '%'
      write(4,*)
      write(4,10) '/Fnopoints 10 def '
      write(4,10) '/Fr 2.5 def '
      write(4,10) '/pi 3.14159265359 def'
      write(4,*)
      write(4,10) '/Frmod {dup 360 div floor 360 mul sub} def'
      write(4,10) '/Fsign {0 ge {1} {-1} ifelse} def'
      write(4,*)
      write(4,10) '/Fbasis'
      write(4,10) '{/Fby exch def /Fbx exch def /Fdist Fbx Fbx'
      write(4,10) 'mul Fby Fby mul add sqrt def /Fxl Fbx Fdist'
      write(4,10) 'div Fr mul def /Fyl Fby Fdist div Fr mul def'
      write(4,10) '/Fxt Fby Fdist div Fr mul def /Fyt Fbx neg'
      write(4,10) 'Fdist div Fr mul def } def'
      write(4,*)
      write(4,10) '/Fstraight '
      write(4,10) '{/Fshape exch def /Ftype exch def /Fy2 exch'
      write(4,10) 'def /Fx2 exch def /Fy1 exch def /Fx1 exch def'
      write(4,10) 'Fx2 Fx1 sub Fy2 Fy1 sub Fbasis /Fttype 1'
      write(4,10) 'Ftype 2 mod 2 mul abs sub def Ftype 0 ge'
      write(4,10) '{/Fddist Fdist Fr div def /Fn Fddist round'
      write(4,10) 'def Ftype 1 gt {/Fn Fn Fddist Fn sub Fsign'
      write(4,10) 'add def} if} {/Fn Fdist Fr div 2 div round 2'
      write(4,10) 'mul def} ifelse Fx1 Fy1 moveto 0 1 Fnopoints'
      write(4,10) 'Fn mul {/Fi exch def Fshape lineto } for'
      write(4,10) 'stroke } def'
      write(4,*)
      write(4,10) '/Floop'
      write(4,10) '{/Fshape exch def /Fe exch def /Ftype exch'
      write(4,10) 'def /Fy2 exch def /Fx2 exch def /Fy1 exch def'
      write(4,10) '/Fx1 exch def /Flam 1 Fe Fe mul sub 2 div Fe'
      write(4,10) 'div def /Fxc Fx1 Fx2 Flam Fy2 Fy1 sub mul add'
      write(4,10) 'add 2 div def /Fyc Fy1 Fy2 Flam Fx1 Fx2 sub'
      write(4,10) 'mul add add 2 div def /Frr Fx1 Fxc sub dup'
      write(4,10) 'mul Fy1 Fyc sub dup mul add sqrt def /Fth1'
      write(4,10) 'Fy1 Fyc sub Fx1 Fxc sub atan def /Fth2 Fy2'
      write(4,10) 'Fyc sub Fx2 Fxc sub atan def Fe 0 lt Fth2'
      write(4,10) 'Fth1 lt and {/Fth1 Fth1 360 sub def} if Fe 0'
      write(4,10) 'gt Fth2 Fth1 gt and {/Fth2 Fth2 360 sub def}'
      write(4,10) 'if /Fdth Fth2 Fth1 sub Fsign def /Fttype 1'
      write(4,10) 'Ftype 2 mod abs 2 mul sub def Ftype 0 ge'
      write(4,10) '{/Fddist Fth2 Fth1 sub abs 180 div pi mul Frr'
      write(4,10) 'mul Fr div def /Fn Fddist round def Ftype 1'
      write(4,10) 'gt {/Fn Fn Fddist Fn sub Fsign add def} if}'
      write(4,10) '{/Fn Fth2 Fth1 sub abs 180 div pi mul Frr mul'
      write(4,10) 'Fr div 2 div round 2 mul def} ifelse Fx1 Fy1'
      write(4,10) 'moveto 0 1 Fnopoints Fn mul {/Fi exch def'
      write(4,10) '/Fth Fth1 Fth2 Fth1 sub Fi mul Fnopoints div'
      write(4,10) 'Fn div add Frmod def Fth sin neg Fdth mul Fth'
      write(4,10) 'cos Fdth mul Fbasis Fshape lineto } for'
      write(4,10) 'stroke } def'
      write(4,*)
      write(4,10) '/Farrow'
      write(4,10) '{ moveto Fxt Fxl sub Fyt Fyl sub'
      write(4,10) 'rlineto Fxl 2 mul Fxt sub Fyl 2 mul Fyt sub'
      write(4,10) 'rlineto Fxl 2 mul Fxt add neg Fyl 2 mul Fyt'
      write(4,10) 'add neg rlineto fill } def'
      write(4,*)
      write(4,10) '/Fphoton'
      write(4,10) '{{ Fx1 Fx2 Fx1 sub Fi mul Fnopoints div Fn'
      write(4,10) 'div Fxt Fi 180 mul Fnopoints div Frmod sin'
      write(4,10) 'mul Fttype mul 2 div add add Fy1 Fy2 Fy1 sub'
      write(4,10) 'Fi mul Fnopoints div Fn div Fyt Fi 180 mul'
      write(4,10) 'Fnopoints div Frmod sin mul Fttype mul 2 div'
      write(4,10) 'add add } Fstraight } def'
      write(4,*)
      write(4,*)
      write(4,10) '/Fphotond'
      write(4,10) '{{ Fx1 Fx2 Fx1 sub Fi mul Fnopoints div Fn'
      write(4,10) 'div Fxt Fi 180 mul Fnopoints div Frmod sin'
      write(4,10) 'mul Fttype mul 2 div add add Fy1 Fy2 Fy1 sub'
      write(4,10) 'Fi mul Fnopoints div Fn div Fyt Fi 180 mul'
      write(4,10) 'Fnopoints div Frmod sin mul Fttype mul 2 div'
      write(4,10) 'add add } Fstraight Fx1'
      write(4,10) 'Fx2 add 2 div Fy1 Fy2 add 2 div Farrow} def'
      write(4,*)
      write(4,10) '/Fphotonl'
      write(4,10) '{exch dup 3 1 roll 0 ge {{ Fxc Fth cos Frr'
      write(4,10) 'mul Fxt Fi 180 mul Fnopoints div Frmod sin'
      write(4,10) 'mul Fttype mul 2 div add add Fyc Fth sin Frr'
      write(4,10) 'mul Fyt Fi 180 mul Fnopoints div Frmod sin'
      write(4,10) 'mul Fttype mul 2 div add add }} {{ Fxc Fth'
      write(4,10) 'cos Frr mul Fxt 1 Fi 180 mul Fnopoints div'
      write(4,10) 'Frmod cos sub mul Fttype mul 2 div add add'
      write(4,10) 'Fyc Fth sin Frr mul Fyt 1 Fi 180 mul'
      write(4,10) 'Fnopoints div Frmod cos sub mul Fttype mul 2'
      write(4,10) 'div add add }} ifelse Floop } def'
      write(4,*)
      write(4,10) '/Fgluon'
      write(4,10) '{2 sub { Fx1 Fx2 Fx1 sub Fi mul Fnopoints div'
      write(4,10) 'Fn div Fxt 1 Fi 180 mul Fnopoints div cos sub'
      write(4,10) 'mul Fttype mul Fxl Fi 180 mul Fnopoints div'
      write(4,10) 'sin mul add add add Fy1 Fy2 Fy1 sub Fi mul'
      write(4,10) 'Fnopoints div Fn div Fyt 1 Fi 180 mul'
      write(4,10) 'Fnopoints div cos sub mul Fttype mul Fyl Fi'
      write(4,10) '180 mul Fnopoints div sin mul add add add }'
      write(4,10) 'Fstraight } def'
      write(4,*)
      write(4,10) '/Fgluonl'
      write(4,10) '{exch 2 sub exch { Fxc Fth cos Frr mul Fxt 1'
      write(4,10) 'Fi 180 mul Fnopoints div cos sub mul Fttype'
      write(4,10) 'mul Fxl Fi 180 mul Fnopoints div sin mul add'
      write(4,10) 'add add Fyc Fth sin Frr mul Fyt 1 Fi 180 mul'
      write(4,10) 'Fnopoints div cos sub mul Fttype mul Fyl Fi'
      write(4,10) '180 mul Fnopoints div sin mul add add add }'
      write(4,10) 'Floop} def'
      write(4,*)
      write(4,10) '/Ffermion'
      write(4,10) '{/Fy2 exch def /Fx2 exch def /Fy1 exch def'
      write(4,10) '/Fx1 exch def newpath Fx2 Fx1 sub Fy2 Fy1 sub'
      write(4,10) 'Fbasis Fx1 Fy1 moveto Fx2 Fy2 lineto stroke Fx1'
      write(4,10) 'Fx2 add 2 div Fy1 Fy2 add 2 div Farrow } def'
      write(4,*)
      write(4,10) '/Fscalar'
      write(4,10) '{newpath moveto lineto stroke} def'
      write(4,*)
      write(4,10) '/Ffermionl'
      write(4,10) '{/Fe exch def /Fy2 exch def /Fx2 exch def'
      write(4,10) '/Fy1 exch def /Fx1 exch def newpath /Flam 1 Fe'
      write(4,10) 'Fe mul sub 2 div Fe div def /Fxc Fx1 Fx2 Flam Fy2'
      write(4,10) 'Fy1 sub mul add add 2 div def /Fyc Fy1 Fy2'
      write(4,10) 'Flam Fx1 Fx2 sub mul add add 2 div def /Frr'
      write(4,10) 'Fx1 Fxc sub dup mul Fy1 Fyc sub dup mul add'
      write(4,10) 'sqrt def /Fth1 Fy1 Fyc sub Fx1 Fxc sub atan'
      write(4,10) 'def /Fth2 Fy2 Fyc sub Fx2 Fxc sub atan def Fe'
      write(4,10) '0 lt Fth2 Fth1 lt and {/Fth1 Fth1 360 sub'
      write(4,10) 'def} if Fe 0 gt Fth2 Fth1 gt and {/Fth2 Fth2'
      write(4,10) '360 sub def} if /Fthc Fth1 Fth2 add 2 div def'
      write(4,10) 'Fxc Fyc Frr Fth1 Fth2 Fe 0 gt {arcn} {arc}'
      write(4,10) 'ifelse stroke Fthc sin Fe 0 lt {neg} if Fthc'
      write(4,10) 'cos Fe 0 gt {neg} if Fbasis Fxc Fthc cos Frr'
      write(4,10) 'mul add Fyc Fthc sin Frr mul add Farrow } def'
      write(4,*)
      write(4,10) '/Fscalarl'
      write(4,10) '{/Fe exch def /Fy2 exch def /Fx2 exch def'
      write(4,10) '/Fy1 exch def /Fx1 exch def newpath /Flam 1 Fe'
      write(4,10) 'Fe mul sub 2 div Fe div def /Fxc Fx1 Fx2 Flam Fy2'
      write(4,10) 'Fy1 sub mul add add 2 div def /Fyc Fy1 Fy2'
      write(4,10) 'Flam Fx1 Fx2 sub mul add add 2 div def /Frr'
      write(4,10) 'Fx1 Fxc sub dup mul Fy1 Fyc sub dup mul add'
      write(4,10) 'sqrt def /Fth1 Fy1 Fyc sub Fx1 Fxc sub atan'
      write(4,10) 'def /Fth2 Fy2 Fyc sub Fx2 Fxc sub atan def Fe'
      write(4,10) '0 lt Fth2 Fth1 lt and {/Fth1 Fth1 360 sub'
      write(4,10) 'def} if Fe 0 gt Fth2 Fth1 gt and {/Fth2 Fth2'
      write(4,10) '360 sub def} if /Fthc Fth1 Fth2 add 2 div def'
      write(4,10) 'Fxc Fyc Frr Fth1 Fth2 Fe 0 gt {arcn} {arc}'
      write(4,10) 'ifelse stroke } def'
      write(4,*)
      write(4,10) '/Fblob '
      write(4,10) '{/Fshade exch def newpath Fr mul 0 360 arc gsave'
      write(4,10) '1 Fshade sub setgray fill grestore stroke} def'
      write(4,*)
      write(4,10) '/Fhiggs'
      write(4,10) '{/Fy2 exch def /Fx2 exch def /Fy1 exch def'
      write(4,10) '/Fx1 exch def gsave Fx1 Fx2 sub dup mul'
      write(4,10) 'Fy1 Fy2 sub dup mul add sqrt dup Fr div'
      write(4,10) '2 div round 2 mul 1 add div /dashln exch def'
      write(4,10) '[dashln dashln] 0 setdash Fx1 Fy1 moveto'
      write(4,10) 'Fx2 Fy2 lineto stroke grestore} def'
      write(4,*)
      write(4,*)
      write(4,10) '/Fhiggsd'
      write(4,10) '{/Fy2 exch def /Fx2 exch def /Fy1 exch def'
      write(4,10) '/Fx1 exch def gsave Fx1 Fx2 sub dup mul'
      write(4,10) 'Fy1 Fy2 sub dup mul add sqrt dup Fr div'
      write(4,10) '2 div round 2 mul 1 add div /dashln exch def'
      write(4,10) '[dashln dashln] 0 setdash Fx1 Fy1 moveto'
      write(4,10) 'Fx2 Fy2 lineto stroke grestore Fx1 Fx2 add 2 div'
      write(4,10) 'Fy1 Fy2 add 2 div Farrow} def'
      write(4,*)
      write(4,10) '/Fhiggsl'
      write(4,10) '{/Fe exch def /Fy2 exch def /Fx2 exch def'
      write(4,10) '/Fy1 exch def /Fx1 exch def /Flam gsave 1 Fe'
      write(4,10) 'Fe mul sub 2 div Fe div def /Fxc Fx1 Fx2 Flam'
      write(4,10) 'Fy2 Fy1 sub mul add add 2 div def /Fyc Fy1'
      write(4,10) 'Fy2 Flam Fx1 Fx2 sub mul add add 2 div def'
      write(4,10) '/Frr Fx1 Fxc sub dup mul Fy1 Fyc sub dup mul'
      write(4,10) 'add sqrt def /Fth1 Fy1 Fyc sub Fx1 Fxc sub'
      write(4,10) 'atan def /Fth2 Fy2 Fyc sub Fx2 Fxc sub atan'
      write(4,10) 'def Fe 0 lt Fth2 Fth1 lt and {/Fth1 Fth1 360'
      write(4,10) 'sub def} if Fe 0 gt Fth2 Fth1 gt and {/Fth2'
      write(4,10) 'Fth2 360 sub def} if /Fthc Fth1 Fth2 add 2'
      write(4,10) 'div def Fxc Fyc Frr Fth1 Fth2 Fe 0 gt {arcn}'
      write(4,10) '{arc} ifelse Fth2 Fth1 sub abs 180 div pi mul'
      write(4,10) 'Frr mul dup Fr div 2 div round 2 mul 1 add'
      write(4,10) 'div /dashln exch def [dashln dashln] 0'
      write(4,10) 'setdash stroke grestore} def'
      write(4,*)
      write(4,10) '/Fghost'
      write(4,10) '{/Fy2 exch def /Fx2 exch def /Fy1 exch def'
      write(4,10) '/Fx1 exch def Fx2 Fx1 sub Fy2 Fy1 sub Fbasis'
      write(4,10) '/Fn Fx1 Fx2 sub dup mul Fy1 Fy2 sub dup mul'
      write(4,10) 'add sqrt Fr div round def 0 1 Fn {/Fi exch'
      write(4,10) 'def Fx2 Fx1 sub Fi Fn div mul Fx1 add Fy2 Fy1'
      write(4,10) 'sub Fi Fn div mul Fy1 add Fr 10 div 0 360 arc'
      write(4,10) 'fill} for Fx1 Fx2 add 2 div Fy1 Fy2 add 2 div'
      write(4,10) 'Farrow } def'
      write(4,*)
      write(4,10) '/Fghostl'
      write(4,10) '{/Fe exch def /Fy2 exch def /Fx2 exch def'
      write(4,10) '/Fy1 exch def /Fx1 exch def /Flam 1 Fe Fe mul'
      write(4,10) 'sub 2 div Fe div def /Fxc Fx1 Fx2 Flam Fy2'
      write(4,10) 'Fy1 sub mul add add 2 div def /Fyc Fy1 Fy2'
      write(4,10) 'Flam Fx1 Fx2 sub mul add add 2 div def /Frr'
      write(4,10) 'Fx1 Fxc sub dup mul Fy1 Fyc sub dup mul add'
      write(4,10) 'sqrt def /Fth1 Fy1 Fyc sub Fx1 Fxc sub atan'
      write(4,10) 'def /Fth2 Fy2 Fyc sub Fx2 Fxc sub atan def Fe'
      write(4,10) '0 lt Fth2 Fth1 lt and {/Fth1 Fth1 360 sub'
      write(4,10) 'def} if Fe 0 gt Fth2 Fth1 gt and {/Fth2 Fth2'
      write(4,10) '360 sub def} if /Fthc Fth1 Fth2 add 2 div def'
      write(4,10) '/Fn Fth2 Fth1 sub abs 180 div pi mul Frr mul'
      write(4,10) 'Fr div round def 0 1 Fn {/Fi exch def Fth2'
      write(4,10) 'Fth1 sub Fi Fn div mul Fth1 add dup cos Frr'
      write(4,10) 'mul Fxc add exch sin Frr mul Fyc add Fr 10'
      write(4,10) 'div 0 360 arc fill} for Fthc sin Fe 0 lt'
      write(4,10) '{neg} if Fthc cos Fe 0 gt {neg} if Fbasis Fxc'
      write(4,10) 'Fthc cos Frr mul add Fyc Fthc sin Frr mul add'
      write(4,10) 'Farrow } def'
      write(4,*)
      write(4,10) '/Fproton'
      write(4,10) '{/Fy2 exch def /Fx2 exch def /Fy1 exch def'
      write(4,10) '/Fx1 exch def Fx2 Fx1 sub Fy2 Fy1 sub Fbasis'
      write(4,10) 'Fx1 Fxt 2 div add Fy1 Fyt 2 div add moveto'
      write(4,10) 'Fx2 Fxt 2 div add Fy2 Fyt 2 div add lineto'
      write(4,10) 'Fx1 Fxt 2 div sub Fy1 Fyt 2 div sub moveto'
      write(4,10) 'Fx2 Fxt 2 div sub Fy2 Fyt 2 div sub lineto'
      write(4,10) 'Fx1 Fx2 add 2 div Fxt Fxl sub add Fy1 Fy2 add'
      write(4,10) '2 div Fyt Fyl sub add moveto Fx1 Fx2 add Fxl'
      write(4,10) 'add 2 div Fy1 Fy2 add Fyl add 2 div lineto'
      write(4,10) 'Fx1 Fx2 add 2 div Fxt Fxl add sub Fy1 Fy2 add'
      write(4,10) '2 div Fyt Fyl add sub lineto stroke} def'
      write(4,*)
      write(4,10) '/Fmax {2 copy lt {exch} if pop} def'
      write(4,10) '/Fstart {gsave currentpoint'
      write(4,10) 'translate 0 0 moveto 0 rm Fr 4 mul'
      write(4,10) 'scalefont setfont} def'
      write(4,10) '/Fsubspt {gsave currentpoint Fcharheight 5 div'
      write(4,10) 'sub translate 0.6 0.6 scale 0 0 moveto 0} def'
      write(4,10) '/Fsupspt {gsave currentpoint Fcharheight 0.6 mul'
      write(4,10) 'add translate 0.6 0.6 scale 0 0 moveto 0} def'
      write(4,10) '/Feend {currentpoint pop Fmax 0.6 mul'
      write(4,10) 'grestore currentpoint pop add Fmax} def'
      write(4,10) '/Fendd {pop grestore} def'
      write(4,10) '/Fshow {exch 0 moveto show currentpoint pop} def'
      write(4,10) '/Fcharheight'
      write(4,10) '{gsave (X) true charpath flattenpath pathbbox'
      write(4,10) '3 1 roll pop sub exch pop grestore} def'
      write(4,*)
      write(4,10) '/Foverline'
      write(4,10) '{exch 0 moveto gsave dup true charpath'
      write(4,10) 'flattenpath pathbbox Fcharheight 10 div dup'
      write(4,10) '2 div setlinewidth add dup 4'
      write(4,10) '1 roll newpath moveto pop lineto stroke'
      write(4,10) 'grestore show currentpoint pop} def'
      write(4,*)
      write(4,10) '/Funderline '
      write(4,10) '{exch 0 moveto gsave dup true charpath'
      write(4,10) 'flattenpath pathbbox pop exch Fcharheight'
      write(4,10) '10 div dup 2 div setlinewidth'
      write(4,10) 'sub dup 3 1 roll newpath moveto lineto stroke'
      write(4,10) 'grestore show currentpoint pop} def'
      write(4,*)
      write(4,10) '/rm /Times-Roman findfont def'
      write(4,10) '/it /Times-Italic findfont def'
      write(4,10) '/sy /Symbol findfont def'
c
c     These are new lines added by Rob Szalapski
c
      write(4,*)
      write(4,10) '/wedge '
      write(4,10) '/{ /ystop exch def /xstop exch def /ystart exch def'
      write(4,10) '/xstart exch def /delx xstop xstart sub def /dely '
      write(4,10) 'ystop ystart sub def /dist delx dup mul dely dup '
      write(4,10) 'mul add sqrt def /halfdist dist 2 div def'
      write(4,10) '/angle dely delx atan def xstart ystart moveto '
      write(4,10) 'angle rotate 0 halfdist rlineto dist halfdist'
      write(4,10) 'neg rlineto dist neg halfdist neg rlineto '
      write(4,10) '0 halfdist rlineto }def'
      write(4,*)
      write(4,10) '/ch_photon'
      write(4,10) '{/ystop exch def /xstop exch def /ystart exch def'
      write(4,10) '/xstart exch def /xmid xstart xstop add 2 div def'
      write(4,10) '/ymid ystart ystop add 2 div def'
      write(4,10) '/dx xstop xstart sub def /dy ystop ystart sub def'
      write(4,10) '/length dx dup mul dy dup mul add sqrt def'
      write(4,10) '/xunit dx length div def /yunit dy length div def'
      write(4,10) '/x1 xmid xunit -4.8 mul add def'
      write(4,10) '/y1 ymid yunit -4.8 mul add def'
      write(4,10) '/x2 xmid xunit 4.8 mul add def'
      write(4,10) '/y2 ymid yunit 4.8 mul add def'
      write(4,10) '/y2 ymid yunit 4.8 mul add def'
      write(4,10) 'xstart ystart x1 y1 1 Fphoton '
      write(4,10) 'x2 y2 xstop ystop 1 Fphoton'
      write(4,10) '/x1 xmid xunit -5.2 mul add def '
      write(4,10) '/y1 ymid yunit -5.2 mul add def'
      write(4,10) '/x2 xmid xunit 5.2 mul add def '
      write(4,10) '/y2 ymid yunit 5.2 mul add def'
      write(4,10) 'gsave x1 y1 x2 y2 wedge fill grestore} def'
      write(4,*)
      write(4,10) '/ch_higgs '
      write(4,10) '{ /ystop exch def /xstop exch def /ystart exch def'
      write(4,10) '/xstart exch def '
      write(4,10) '/xmid xstart xstop add 2 div def'
      write(4,10) '/ymid ystart ystop add 2 div def'
      write(4,10) '/dx xstop xstart sub def /dy ystop ystart sub def'
      write(4,10) '/length dx dup mul dy dup mul add sqrt def'
      write(4,10) '/xunit dx length div def /yunit dy length div def'
      write(4,10) '/x1 xmid xunit -4.8 mul add def '
      write(4,10) '/y1 ymid yunit -4.8 mul add def'
      write(4,10) '/x2 xmid xunit 4.8 mul add def '
      write(4,10) '/y2 ymid yunit 4.8 mul add def'
      write(4,10) 'xstart ystart x1 y1 Fhiggs '
      write(4,10) 'x2 y2 xstop ystop Fhiggs'
      write(4,10) '/x1 xmid xunit -5.2 mul add def'
      write(4,10) '/y1 ymid yunit -5.2 mul add def'
      write(4,10) '/x2 xmid xunit 5.2 mul add def'
      write(4,10) '/y2 ymid yunit 5.2 mul add def'
      write(4,10) 'gsave x1 y1 x2 y2 wedge fill grestore} def'

      write(4,10) '% End of the header'
      write(4,*)

 10   format(a)
 12   format(a,2i8)

      end














