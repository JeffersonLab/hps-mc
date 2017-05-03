!-----------------------
! madgraph - a Feynman Diagram package by Tim Stelzer and Bill Long 
! (c) 1993
!
! Filename: drawfeyn.f
!-----------------------

!*************************************************************************
!     This file contains routines for generating ps files for the Feynman
!     diagrams.
!*************************************************************************


      Subroutine PositionVerts(graphs,tops,igraph,next)
!*************************************************************************
!     For graph i determine an asthetic placement of the vertices and
!     external lines.  Basic idea is to minimize Sum(length^2) of all lines
!*************************************************************************
      implicit none

! Constants

      include 'params.inc'
! Arguments

      integer graphs(0:maxgraphs,0:maxlines) 
      integer tops(0:4,0:maxnodes,0:maxtops),next
      integer igraph
      
! Local Variables

      integer nverts,ntop
      integer i,iy,xoff,yoff,nden,j
      integer jline(-maxlines:maxlines,2)
      integer c(-maxlines:maxnodes,maxrows)
      integer lnum,ivert,nlines
      real xshift,yshift,x1,x2,y1,y2,ypos(maxlines)
      logical done
      real pverts(-maxlines:maxlines,2),goodverts(-maxlines:maxlines,2)
      integer perms(maxrows,maxcols),k(maxrows),count,iter
      real mysum,minsum
      integer rows,irow,icol,pgraph,npage,jdir
      integer n_incoming

      real xscale,yscale           !Scaling factors for diagrams
      real m1,m3,xint,x3,x4,y3,y4  !used to see if lines cross
      real dx2, dy2, minlength     !Used to avoid line getting too short
      logical crossed

! Global Variables

      character*(max_string) iwave(max_particles),owave(max_particles)
      integer iposx(3,max_particles)
      integer info_p(5,max_particles)
      character*(8) str(3,max_particles)
      common/to_external/iwave,owave,iposx,info_p,str

      integer        iline(-maxlines:maxlines),idir(-maxlines:maxlines)
      integer        this_coup(max_coup) ,goal_coup(max_coup)
      common/to_proc/iline,idir,this_coup,goal_coup
      character*60 proc
      integer iproc
      common/to_write/iproc,proc


      character*(4*max_particles) particle(4)
      integer                               charge_c(max_particles)
      integer iparticle(0:max_particles,0:4),inverse(max_particles)
      common/to_model/iparticle,  particle,  inverse, charge_c

!-----------
! Begin Code
!-----------

      minlength = 0.

c
c     determine x and y scale for each diagram using historical 
c     value that scale=0 when graphs_per_row = 3 and 
c     rows_per_page=5
c
      xscale = scale*3.0/graphs_per_row
      yscale = scale*5.0/rows_per_page

! Initialize things

      do i=1,maxlines
         iline(-i) = graphs(igraph,i)
      enddo
      pgraph = mod(igraph-1,graphs_per_page)+1
      if (pgraph .eq. 1) then
         npage = (igraph-1)/graphs_per_page + 1
c         print*,'New page',npage
         xoff = graphs_per_row*15*xscale
         yoff = (rows_per_page*15)*yscale
         if (npage .gt. 1) write(4,'(a)') 'showpage'
         write(4,'(a,2I8)') '%%Page:',npage,npage
         write(4,'(a)') '%%PageBoundingBox:-20 -20 600 800'
         write(4,'(a)') '%%PageFonts: Helvetica'
         write(4,'(a)') '/Helvetica findfont 10 scalefont setfont'
         write(4,*) xoff/2+15,yoff+20,' moveto'
         write(4,'(a)') '(Diagrams by MadGraph) show'
         write(4,*) xoff/2+145,yoff+20,' moveto'
         write(4,'(a,a,a)')  '(',proc(2:iproc), ') show'
      endif
      icol = mod(pgraph-1,graphs_per_row)
      irow = rows_per_page-int((pgraph-1)/graphs_per_row)-1


      xoff = icol*15*xscale+100
      yoff = irow*15*yscale+50
c
c     Incoming particles
c
      n_incoming = 2
      if (n_incoming .eq. 1) then
         pverts(-1,1) = 0
         pverts(-1,2) = 10
      else
         pverts(-1,1) = 0
         pverts(-1,2) = 10
         pverts(-2,1) = 0
         pverts(-2,2) = 0
      endif
      iy = 10
      do i=3,next                         !Final leg positions
         ypos(i-2)=iy
         if (next .eq. 3) then
            ypos(1) = 5
         else
            iy = iy - 10./(next-3)
         endif
      enddo
      ntop   = graphs(igraph,0)
      nverts = tops(1,0,ntop)

! Determine which vertices are connected set up C(iline,ivert)

      do i=-maxlines,maxlines
         jline(i,1)=0
         jline(i,2)=0
      enddo
      do i=1,next
         jline(i,1) =-i
         jline(i,2) =-i
      enddo
      do ivert = 1,nverts
         nlines = tops(0,ivert,ntop)
         do i = 1,nlines
            lnum=tops(i,ivert,ntop)
            if (jline(lnum,1) .eq. 0) then
               jline(lnum,1) = i
               jline(lnum,2) = ivert
            elseif (jline(lnum,1) .lt. 0) then
               C(i,ivert) = jline(lnum,2)      != -lnum
               jline(lnum,1) = ivert
            else
               C(i,ivert) = jline(lnum,2)
               C(jline(lnum,1),jline(lnum,2))= ivert
               jline(lnum,1) = ivert
            endif
         enddo
      enddo

! Loop over all configurations of final legs orders

      do i=1,next-2                           
         k(i)=i
      enddo
      count=0
      if (next .eq. 3) then
         rows=1
         count=1
         perms(1,1)=1
      elseif (next .eq. 4) then
         rows = 2
         call perm2(k,perms,rows,count)
      elseif (next .eq. 5) then
         rows = 3
         call perm3(k,perms,rows,count)
      elseif (next .eq. 6) then
         rows = 4
         call perm4(k,perms,rows,count)
      elseif (next .eq. 7) then
         rows = 5
         call perm5(k,perms,rows,count)
      elseif (next .eq. 8) then
         rows = 6
         call perm6(k,perms,rows,count)
      elseif (next .eq. 9) then
         rows = 7
         call perm7(k,perms,rows,count)
c      elseif (next .eq. 10) then
c         rows = 8
c         call perm8(k,perms,rows,count)
      else
         write(*,*) 'Warning from drawfeyn.f too many permutations.',
     &        '  Graphs might not look very good.'
      endif

      minsum = -1
      do iter=1,count
         do i=3,next                      !Final state particles
            if (next .le. 4) then
               pverts(-i,1)=10
            elseif(next .eq. 5) then
               pverts(-i,1)=9
c            elseif(next .eq. 6) then
c               pverts(-i,1)=8
c            elseif(next .eq. 7) then
c               pverts(-i,1)=7
            else
               pverts(-i,1)=8
            endif
            pverts(-i,2)=ypos(perms(i-2,iter))
         enddo

! Guess initial values for vertices, neg = external

         do ivert=1,nverts
            pverts(i,1)=0.
            pverts(i,2)=0.
         enddo
         do ivert=1,nverts
            xshift = 0
            yshift = 0
            nlines = tops(0,ivert,ntop)
            nden=0
            do i=1,nlines
               if (c(i,ivert) .lt. 0) then            !This is external 
                  xshift = xshift + pverts(c(i,ivert),1)
                  yshift = yshift + pverts(c(i,ivert),2)
                  nden=nden+1
               endif
            enddo
            if (nden .gt. 0) then
               pverts(ivert,1)=xshift/nden
               pverts(ivert,2)=yshift/nden
            else
               pverts(ivert,1)=-1
               pverts(ivert,2)=-1         !These are unassigned to start
            endif
c            write(*,*) ivert,pverts(ivert,1),pverts(ivert,2)
         enddo


! Now try to order

      done=.false.
      do while (.not. done)
         done=.true.
         do ivert=1,nverts
            xshift = 0
            yshift = 0
            nlines = tops(0,ivert,ntop)
            nden = 0
            do i=1,nlines
               if (pverts(c(i,ivert),1) .ge. 0) then   !This has been assigned
                  nden = nden +1 
                  xshift = xshift + pverts(c(i,ivert),1)
                  yshift = yshift + pverts(c(i,ivert),2)
c                  write(*,*) i, pverts(c(i,ivert),1)
               endif
c               write(*,*) 'shift ',ivert,xshift,yshift
c     $              ,pverts(c(i,ivert),1)
            enddo
            if (nden .gt. 0) then
               xshift=xshift/nden
               yshift=yshift/nden
               if (abs(pverts(ivert,1)-xshift) .gt. .1) done=.false.
               if (abs(pverts(ivert,2)-yshift) .gt. .1) done=.false.
               pverts(ivert,1)=xshift
               pverts(ivert,2)=yshift
c               write(*,*) 'opt ',ivert,pverts(ivert,1),pverts(ivert,2)

            else
               done=.false.
            endif
         enddo
      enddo
c
c     Minimize total line length^2
c
      mysum = 0
      do i=1,next
         dx2 = (pverts(jline(i,1),1)-pverts(jline(i,2),1))**2
         dy2 = (pverts(jline(i,1),2)-pverts(jline(i,2),2))**2
         mysum=mysum + dx2 + dy2
         if (sqrt(dx2 + dy2) .lt. minlength) mysum = mysum+minlength**2
c         if (sqrt(dx2 + dy2) .lt. minlength) mysum = mysum+9999
c         mysum=mysum + 
      enddo
      do i=1,nverts-1
         dx2 = (pverts(jline(-i,1),1)-pverts(jline(-i,2),1))**2
         dy2 = (pverts(jline(-i,1),2)-pverts(jline(-i,2),2))**2
         mysum=mysum + dx2 + dy2
         if (sqrt(dx2 + dy2) .lt. minlength) mysum = mysum+minlength**2
c         if (sqrt(dx2 + dy2) .lt. minlength) mysum = mysum+9999
c         mysum=mysum + 
c         mysum=mysum + 
      enddo
c
c     Make sure final external lines don't cross line1=(x1,y1)->(x2,y2)
c     by finding  intersection and seeing if its in region graphed
c
c
      i=3
      i=1-nverts
      crossed=.false.
      do while  (i .le. next .and. .not. crossed)
         j=i+1
         if (j.eq. 0) j=1
         do while (j .le. next .and. .not. crossed)
c            write(*,*) i,j
            x1 = pverts(jline(i,1),1)
            x2 = pverts(jline(i,2),1)
            y1 = pverts(jline(i,1),2)
            y2 = pverts(jline(i,2),2)
            x3 = pverts(jline(j,1),1)
            x4 = pverts(jline(j,2),1)
            y3 = pverts(jline(j,1),2)
            y4 = pverts(jline(j,2),2)
c
c     See if the share a vertex. If so, don't worry about crossing
c
            if ( ((x1.eq.x3).and.(y1.eq.y3)) .or.
     &           ((x1.eq.x4).and.(y1.eq.y4)) .or.
     &           ((x2.eq.x3).and.(y2.eq.y3)) .or.
     &           ((x2.eq.x4).and.(y2.eq.y4)) ) then
               crossed=.false.
            else

            m1 = (y2-y1)/((x2-x1)+.00001)
            m3 = (y4-y3)/((x4-x3)+.00001)
            if (m1 .ne. m3) then
               xint = ((y1-y3)+(m3*x3-m1*x1))/(m3-m1)
c               write(*,'(a,4f8.0)') 'x1,y1,x2,y2 ',x1,y1,x2,y2
c               write(*,'(a,4f8.0)') 'x3,y3,x4,y4 ',x3,y3,x4,y4
c               write(*,'(a,4f8.0)') 'xint ',xint
            else
               xint = -9999             !Parallel lines, don't intersect
            endif
            if (xint .lt. max(x1,x2)+.1.and. xint.gt. min(x1,x2)-.1.and.
     &          xint .lt. max(x3,x4)+.1.and. xint.gt. min(x3,x4)-.1)then
               crossed=.true.
               mysum = 2*mysum
            endif
            endif
            j=j+1
            if (j.eq. 0) j=1
         enddo
         i=i+1
         if (i.eq. 0) i=1
      enddo

      if (mysum .ge. 999999) then
         print*,'Warning mysum is large',mysum
      endif
      if (mysum .gt. 0 .or. minsum .eq. -1) then 
         if (mysum .lt. minsum .or. minsum .lt. 0) then
c         write(*,*) 'Good',mysum
            minsum = mysum
            do i=-maxlines,maxlines
               goodverts(i,1) = pverts(i,1)
               goodverts(i,2) = pverts(i,2)
            enddo
         endif
      endif

      enddo          !iteration
c      write(*,*) igraph,minsum
c
c     Here it would be good to add special case for s-channel
c     processes. They look best if propagator is horizontal.
c
c      write(*,*) 'Done'
      do i=-maxlines,maxlines
         pverts(i,1) = goodverts(i,1)
         pverts(i,2) = goodverts(i,2)
      enddo

      if (minsum .eq. 99999) then
         print*,'Warning graph not drawn',igraph
         return
      endif
      do i=3,next
         pverts(-i,1) = 10
      enddo
      do i=1,next                               !Draw external lines
         x1 = xscale*pverts(jline(i,1),1)+xoff
         y1 = yscale*pverts(jline(i,1),2)+yoff
         x2 = xscale*pverts(jline(i,2),1)+xoff
         y2 = yscale*pverts(jline(i,2),2)+yoff
c         jdir=+1 
c         if (inverse(iline(i)) .lt. iline(i)) jdir=-1
         jdir = inverse(iline(i)) -  iline(i)
c         write(*,*) 'Line', i,jdir
         if (i .le. 2) jdir=-jdir
         if (i .le. 2) then
            call drawline(x2,y2,x1,y1,info_p(4,iline(i)),jdir,
     $        str(1,inverse(iline(i))))
         else
            call drawline(x1,y1,x2,y2,info_p(4,iline(i)),jdir,
     $        str(1,iline(i)))
         endif
      enddo
      do i=1,nverts-1
         x1 = xscale*pverts(jline(-i,1),1)+xoff
         y1 = yscale*pverts(jline(-i,1),2)+yoff
         x2 = xscale*pverts(jline(-i,2),1)+xoff
         y2 = yscale*pverts(jline(-i,2),2)+yoff
c         jdir=1 
c         if (inverse(iline(-i)) .lt. iline(-i)) jdir=-1
         jdir = inverse(iline(-i)) -  iline(-i)
         call drawline(x1,y1,x2,y2,info_p(4,graphs(igraph,i)),jdir,
     $        str(1,graphs(igraph,i)))
      enddo
      write(4,*) xoff+4*xscale,yoff-yscale,'  moveto'
      write(4,'(a,i4,a)') '(graph ',igraph,')   show'
      write(4,*) xoff-xscale,yoff+10*yscale,'  moveto'
      write(4,*) '(1)   show'
      write(4,*) xoff-xscale,yoff-2,'  moveto'
      write(4,*) '(2)   show'
      do i=3,next                                  !Label final lines numbers
         write(4,*) xoff+10*xscale,yoff+yscale*pverts(-i,2),'  moveto'
         write(4,'(a,i3,a)') '(',i,') show'
      enddo
      end


      Subroutine drawline(x1,y1,x2,y2,itype,jdir,name)
!***************************************************************************
!     Routine to draw postscript for feynman diagram line
!     from x1,y1 to x2,y2 with appropriate label
!***************************************************************************
      implicit none
      
! Arguments

      real x1,y1,x2,y2,dx,dy,d
      integer itype,jdir
      character*5 name

! Local Variables

!-----------
! Begin Code
!-----------
      d  = sqrt((x1-x2)**2+(y1-y2)**2)
      if (d .gt. 0) then
         dx = (x1-x2)/d
         dy = (y1-y2)/d
      else
         write(*,*) 'Error zero line length ',name
         return
      endif

      if (dy .lt. 0) then
         dy=-dy
         dx=-dx
      endif

      if (itype .eq. 4) then
         write(4,*) x1,y1,x2,y2,' 0  Fgluon'
      elseif (itype .eq. 2) then
         if (jdir .gt. 0) then
            write(4,*) x1,y1,x2,y2,'  Ffermion'
         elseif (jdir .lt. 0) then
            write(4,*) x2,y2,x1,y1,'  Ffermion'
         else
            write(4,*) x1,y1,'  moveto'
            write(4,*) x2,y2,'  lineto'
         endif
      elseif(itype .eq. 3) then
c
c     The top two should be fhiggsd, not fhiggs
c
         if (jdir .gt. 0) then
            write(4,*) x1,y1,x2,y2,'   Fhiggs'
         elseif (jdir .lt. 0) then
            write(4,*) x2,y2,x1,y1,'   Fhiggs'
         else
            write(4,*) x1,y1,x2,y2,'  Fhiggs'
         endif
      elseif (itype .eq. 1) then
         if (jdir .gt. 0) then
            write(4,*) x1,y1,x2,y2,' 0  Fphotond'
         elseif (jdir .lt. 0) then
            write(4,*) x2,y2,x1,y1,' 0  Fphotond'
         else
            write(4,*) x1,y1,x2,y2,' 0  Fphoton'
         endif
c         write(4,*) x1,y1,x2,y2,' 0  Fphotond'
      else
         write(*,*) 'Unknown Feynman line, using photon.',itype
         write(4,*) x1,y1,x2,y2,' 0  Fphoton'
      endif
      write(4,*) (x1+x2)/2.+5.*dy, (y1+y2)/2.-5.*dx-4., '  moveto'
      write(4,'(3a)') '(',name,')   show'
      end

