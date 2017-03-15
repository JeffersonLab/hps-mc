!-----------------------
! madgraph - a Feynman Diagram package by Tim Stelzer and Bill Long 
! (c) 1993
!
! Filename: gencolor.f
!-----------------------
 
      Subroutine MatrixColor(graphcolor,sflows)
!**************************************************************************
!     write out all color factors and interference terms
!**************************************************************************
      implicit none

! Constants

      include 'params.inc'

! Arguments

      integer graphcolor(0:2,0:maxflows,0:maxgraphs)
      double precision  sflows(maxflows,maxflows)

! Local Variables

      integer igraph,jgraph,iterm,jterm,j,nflows
      integer identpart(max_particles)
      integer ngraphs,iplace(maxgraphs),nrows,irow,ikill
      double precision  sum,den
      double precision  color_mat(maxgraphs)
      double precision  red_color(maxcolors,maxgraphs),denom(maxgraphs)
      double precision  eigen_val(maxcolors)

! Global Variables

      integer        iline(-maxlines:maxlines),idir(-maxlines:maxlines)
      integer        this_coup(max_coup) ,goal_coup(max_coup)
      common/to_proc/iline,idir,this_coup,goal_coup

      integer         nincoming,nincfirst
      common/to_proc2/nincoming,nincfirst

      integer iposx(3,max_particles)
      integer info_p(5,max_particles)
      character*(8) str(3,max_particles)
      character*(max_string) iwave(max_particles),owave(max_particles)

      common/to_external/iwave,owave,iposx,info_p,str

      integer            symfact(maxgraphs)
      common /tosymmetry/symfact

!-----------
! Begin Code
!-----------
      nflows=-1
      do j=1,max_particles
         identpart(j)=0                   !Number of identical final parts
      enddo
      den=1d0
      do j=1,nincoming
         den = den*dble(info_p(1,iline(j)))  !Average over initial colors
      enddo
      do j=nincoming+1,iline(0)                !over all final state lines
         identpart(iline(j)) = identpart(iline(j))+1
         den=den*identpart(iline(j))          ! n! for identical final particle
      enddo
      den=1
c      print*,'Denominator',den
      ngraphs=graphcolor(0,0,0)
      nrows = 0
      do igraph=1,ngraphs
         do jgraph=1,ngraphs
            sum=0
            do iterm=1,graphcolor(0,0,igraph)
               do jterm=1,graphcolor(0,0,jgraph)
                  nflows=max(nflows,graphcolor(0,iterm,igraph))
                  sum=sum+sflows(graphcolor(0,iterm,igraph),
     &                 graphcolor(0,jterm,jgraph))*
     &                 dble(graphcolor(1,iterm,igraph))*
     &                 dble(graphcolor(1,jterm,jgraph))/
     &                 dble(graphcolor(2,iterm,igraph))/
     &                 dble(graphcolor(2,jterm,jgraph))
               enddo
            enddo
            if (abs(sum) .lt. 1e-8) sum=0
            color_mat(jgraph)=sum/den
         enddo
         call compresscolor(igraph,ngraphs,color_mat,red_color,
     &       nrows,iplace,denom)
      enddo
      print*,'Reduced color matrix',nrows,ngraphs
      jgraph=0
      ikill =0
      do irow=1,nrows
         jgraph=jgraph+1
         do while(jgraph .le. ngraphs .and. iplace(jgraph) .ne. irow)
            call killcolumn(red_color,jgraph-ikill,nrows,ngraphs)
            ikill=ikill+1
            jgraph=jgraph+1
         enddo
         if (jgraph .gt. ngraphs) then
            print*,'Error did not find all rows',jgraph,ngraphs,irow
            stop
         endif
      enddo

      call eigensys(red_color,eigen_val,nrows)
c      do jgraph=1,ngraphs
c         print*,jgraph,iplace(jgraph),symfact(jgraph)
c      enddo
c      call gencode(red_color,eigen_val,ngraphs,nrows,iplace,denom)
c      call eigensys(sflows,eigen_val,nflows)
c      do jgraph=1,nflows
c         write(*,*) jgraph,eigen_val(jgraph)
c         do irow=1,nflows
c            write(*,'(i6,f20.8)') irow,red_color(irow,jgraph)
c         enddo
c      enddo
c      stop
      call genampcode(sflows,nflows)

      end

      Subroutine gencode(eigen_vec,eigen_val,ngraphs,nrows,iplace,denom)
!***************************************************************************
!     This routine writes out the fortran code to do color factors
!     for all of the graphs.
!     Input:
!          Eigen_vec,Eigenval  -> eigen vectors and values of color matrix
!          ngraphs             -> number of graphs
!
!***************************************************************************
      implicit none
! Constants

      include 'params.inc'
c      integer   maxgraphs,    maxcolors
c      parameter(maxgraphs=999,maxcolors=250)
c      double precision zero
c      parameter(zero=0d0)

! Arguments

      double precision Eigen_vec(maxcolors,maxgraphs)
      double precision Eigen_val(maxcolors)
      double precision denom(maxgraphs)
      integer ngraphs,nrows,iplace(maxgraphs)

! Local Variables

      integer i,j
      character*3 str1,str2
      character*40 buff
      double precision temp_vec(maxgraphs),sum

! Global Variables

      integer            symfact(maxgraphs)
      common /tosymmetry/symfact
      double precision    den2(maxgraphs)
      integer                  neig,nrows2,jplace(maxgraphs)
      common /to_colormat/den2,neig,nrows2,jplace

!-----------
! Begin Code
!-----------
      neig=0
      nrows2=nrows
      do i=1,ngraphs
         den2(i)=denom(i)*symfact(i)
c         print*,i,symfact(i)
      enddo
      do i=1,nrows
         if (abs(eigen_val(i)) .gt. 1d-9) then
            neig=neig+1
            call makestringamp(neig,str1)
            buff = '      DATA EIGEN_VAL('//str1//')/'
            sum=0d0
            do j=1,ngraphs
               temp_vec(j)=eigen_vec(iplace(j),i)/denom(i)/denom(j)
               sum = sum+temp_vec(j)**2
            enddo
            sum=sqrt(sum)
c            print*,'In writecode i,sum',i,sum,denom(i)
c            do j=1,ngraphs
c               temp_vec(j)=temp_vec(j)/sum
c            enddo
c            write(99,20) buff,eigen_val(i),'/'
c            write(99,20) buff,eigen_val(i)*sum*sum,'/'
c            do j=1,ngraphs
c               call makestringamp(j,str2)
c               buff = '      DATA EIGEN_VEC('//str2//','//str1//')/'
c               if (abs(temp_vec(j)) .gt. 1d-12) then
c                  write(99,20) buff,temp_vec(j)*dble(symfact(j)),'/'
c               else
c                  write(99,20) buff,zero,'/'
c               endif
c            enddo
            do j=1,nrows
               temp_vec(j) = eigen_vec(j,i)      !/denom(j)
               call makestringamp(j,str2)
               buff = '      DATA EIGEN_VEC('//str2//','//str1//')/'
               if (abs(temp_vec(j)) .gt. 1d-12) then
                  write(99,20) buff,temp_vec(j),'/'
               else
                  write(99,20) buff,zero,'/'
               endif
            enddo
         endif
      enddo
      i=1
c      write(99,'($a,i4)') '      DATA JGRAPH/',iplace(i)
      do while(i .lt. ngraphs-1)
         i=i+1
c         write(99,'($a,i4)') ',',iplace(i)
         if ((i-8)/10 .eq. real(i-8)/10.) then
c            write(99,*)
c            write(99,'($a)') '     & '
         endif
      enddo
c      write(99,'(a,i4,a)')',',iplace(ngraphs),'/'
      do i=1,ngraphs
         jplace(i)=iplace(i)
      enddo
         
 20   format (a31,1pd24.16,a2)
 30   format (a,i4,',',i4,',',i4,',',i4,',',i4,',',i4,',',i4,',',i4,',')
      end

      Subroutine genampcode(sflows,nflows)
!***************************************************************************
!     This routine writes out the fortran code to do color factors
!     for all of the graphs.
!     Input:
!          Eigen_vec,Eigenval  -> eigen vectors and values of color matrix
!          ngraphs             -> number of graphs
!
!***************************************************************************
      implicit none
! Constants

      include 'params.inc'
      integer    npl    
      parameter (npl = 6)

! Arguments

      double precision sflows(maxflows,maxflows)
      integer nflows

! Local Variables

      integer i,j,idenom,k
      character*3 str1,str2,str3
      character*50 buff
      double precision temp_vec(maxgraphs),sum

! External
      
      integer igcf

! Global Variables

      integer            symfact(maxgraphs)
      common /tosymmetry/symfact
      double precision    den2(maxgraphs)
      integer                  neig,nrows2,jplace(maxgraphs)
      common /to_colormat/den2,neig,nrows2,jplace
      integer           isflows(2,maxflows,maxflows)
      common/to_isflow/isflows

      integer flows(0:2*maxlines,0:maxfactors,0:24,0:maxflows)
      integer         graphcolor(0:2,0:maxflows,0:maxgraphs)
      common/to_color/graphcolor,flows


!-----------
! Begin Code
!-----------
      neig=nflows
c      nrows2=nrows
c      do i=1,ngraphs
c         den2(i)=denom(i)*symfact(i)
cc         print*,i,symfact(i)
c      enddo
      do i=1,nflows
         idenom=1
         do j=1,nflows
            if (isflows(2,j,i) .ne.0) then
               idenom = abs(idenom*isflows(2,j,i)/
     &              igcf(idenom,isflows(2,j,i)))
            endif
         enddo
         call makestringamp(i,str1)
         buff = '      DATA EIGEN_VAL('//str1//')/'
         buff = '      DATA Denom('//str1//')/'
         write(99,22) buff,idenom,'/'
c         do j=i,nflows,npl
         do j=1,nflows,npl
c            temp_vec(j) = isflows(1,j,i)*idenom/isflows(2,j,i)
            call makestringamp(j,str2)
            call makestringamp(min(j+npl-1,nflows),str3)
            buff = '      DATA (CF(i,'//str1//'),i='
     &           //str2//','//str3//') /'
            write(99,'(a35$)') buff
            k=0
            do while (j+k .le. nflows .and. k .lt. npl)
               if (k .ne. 0) write(99,'(a$)') ','
               if (isflows(1,j+k,i) .ne. 0) then
                  write(99,'(i5$)') isflows(1,j+k,i)*
     &                 (idenom/isflows(2,j+k,i))
               else
                  write(99,'(i5$)') zero
               endif
               k=k+1
            enddo
            write(99,'(a)') '/'
         enddo
c
c     Now write comment line which shows color connection for this amp.
c
         call write_color_comment(flows(0,0,0,i),99)

      enddo
 20   format (a31,1pd24.16,a2)
 22   format (a25,i10,a)
 23   format (i4, ',' ,i4, ',' ,i4, ',' ,i4, ',' ,i4,a2)
 30   format(a,i4,',',i4,',',i4,',',i4,',',i4,',',i4,',',i4,',',i4,',')
      end


      Subroutine CompressColor(irow,ngraphs,new,red_color,
     &     nrows,iplace,den)
!***********************************************************************
!  Compresses color matrix down to reduce size
!***********************************************************************
      implicit none
! Constants

      include 'params.inc'
c      integer    maxgraphs
c      parameter (maxgraphs=999)
c      integer    maxcolors
c      parameter (maxcolors=250)

! Arguments

      double precision  red_color(maxcolors,maxgraphs),den(maxgraphs)
      double precision  new(maxgraphs)
      integer ngraphs,irow,iplace(maxgraphs),nrows

! Local 

      logical found,gotfac
      integer i,j

!  External

      logical equal

!-----------
! Begin Code
!-----------

c      sum=0d0
      do i=irow,ngraphs
         den(i)=1d0
      enddo
      found=.false.
      i = 0
      do while (.not. found .and. i .lt. nrows)
         found = .true.
         gotfac = .false.
         i = i+1
         j = 0
         do while (found .and. j .lt. ngraphs)
            j=j+1
            if (.not. gotfac) then
               if (new(j) .ne. 0d0) then
                  den(irow)=red_color(i,j)/new(j)
                  gotfac = .true.
               endif
            endif
            found = equal(new(j)*den(irow),red_color(i,j))
         enddo
      enddo
      if (.not. found) then
         if (nrows .gt. maxcolors) then
            print*,'Error too many color combination, reset maxcolors',
     &           maxcolors
         endif
         nrows=nrows+1
         iplace(irow) = nrows
         den(irow) = 1d0 
         do i=1,ngraphs
            red_color(nrows,i)=new(i)
         enddo
c         print*,'Added new one',nrows,irow,den(irow)
      else
         iplace(irow) = i
c         if (.not. equal((den(irow)/den(i)),1d0)) then
c            print*,'Warning Color reduction not factor of 1',
c     $            irow,den(irow)/den(i)
c         endif
c         print*,i,den(i),den(irow)
      endif
      end

      logical function equal(x,y)
!************************************************************************
!     Determines if two double precision numbers are equal to tol
!************************************************************************
      implicit none

!  Constants
      
      double precision tol
      parameter       (tol = 1d-10)

!  Arguments

      double precision x,y

!-----------
! Begin Code
!-----------
      equal=.false.
      if (x .eq. 0d0 .and. y .eq. 0d0) then
         equal = .true.
      elseif(x+y .eq. 0) then
         equal=.false.
      else
         equal = (abs(x-y)/abs(x+y) .lt. tol)
      endif
      end

      subroutine killcolumn(red_color,jcol,nrows,ngraphs)
!***********************************************************************
!  Compresses color matrix down to reduce size
!***********************************************************************
      implicit none

! Constants

      include 'params.inc'
c      integer    maxgraphs
c      parameter (maxgraphs=999)
c      integer    maxcolors
c      parameter (maxcolors=250)

! Arguments

      double precision  red_color(maxcolors,maxgraphs)
      integer jcol,nrows,ngraphs

! Local

      integer icol,irow

!---------
! Begin Code
!---------

      do irow=1,nrows
         do icol=jcol,ngraphs
            red_color(irow,icol)=red_color(irow,icol+1)
         enddo
      enddo
      end

