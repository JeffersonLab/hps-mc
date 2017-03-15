!-----------------------
! madgraph - a Feynman Diagram package by Tim Stelzer and Bill Long 
! (c) 1993
!
! Filename: permute.f
!-----------------------
 
      subroutine test permute
      implicit none

! Constants

      include 'params.inc'
c      integer    maxrows,  maxcols
c      parameter (maxrows=6,maxcols=720)

! Local Variables

      integer rows,cols
      integer i,j,perm(maxrows,maxcols),k(maxrows),count

!-----------
! Begin Code
!-----------
      rows=6
      cols=120
      do i=1,rows
         k(i)=i-1
      enddo
      call perm6(k,perm,rows,count)
      do i=1,cols
         write(*,*) (perm(j,i),j=2,rows)
      enddo
      end

!-------------------------------------
      subroutine perm7(k,n,rows,count)
      implicit none

! Constants

      include 'params.inc'
c      integer    maxrows,  maxcols
c      parameter (maxrows=6,maxcols=720)

! Arguments

      integer rows
      integer k(maxrows),n(maxrows,maxcols),count

! Local Variables

      integer i,j,temp

!-----------
! Begin Code
!-----------
      do j=1,7
         temp=k(j)
         k(j)=k(1)
         call perm6(k(2),n,rows,count)
         do i=0,719
            n(rows-6,count-i)=temp
         enddo
         k(j)=temp
      enddo
      end

!-------------------------------------
      subroutine perm6(k,n,rows,count)
      implicit none

! Constants

      include 'params.inc'
c      integer    maxrows,  maxcols
c      parameter (maxrows=6,maxcols=720)

! Arguments

      integer rows
      integer k(maxrows),n(maxrows,maxcols),count

! Local Variables

      integer i,j,temp

!-----------
! Begin Code
!-----------
      do j=1,6
         temp=k(j)
         k(j)=k(1)
         call perm5(k(2),n,rows,count)
         do i=0,119
            n(rows-5,count-i)=temp
         enddo
         k(j)=temp
      enddo
      end

!-------------------------------------
      subroutine perm5(k,n,rows,count)
      implicit none

! Constants

      include 'params.inc'
c      integer    maxrows,  maxcols
c      parameter (maxrows=6,maxcols=720)

! Arguments

      integer rows
      integer k(maxrows),n(maxrows,maxcols),count

! Local Variables

      integer i,j,temp

!-----------
! Begin Code
!-----------
      do j=1,5
         temp=k(j)
         k(j)=k(1)
         call perm4(k(2),n,rows,count)
         do i=0,23
            n(rows-4,count-i)=temp
         enddo
         k(j)=temp
      enddo
      end

!-------------------------------------
      subroutine perm4(k,n,rows,count)
      implicit none

! Constants

      include 'params.inc'
c      integer    maxrows,  maxcols
c      parameter (maxrows=6,maxcols=720)

! Arguments

      integer rows
      integer k(maxrows),n(maxrows,maxcols),count

! Local Variables

      integer i,j,temp

!-----------
! Begin Code
!-----------
      do j=1,4
         temp=k(j)
         k(j)=k(1)
         call perm3(k(2),n,rows,count)
         do i=0,5
            n(rows-3,count-i)=temp
         enddo
         k(j)=temp
      enddo
      end

!-------------------------------------
      subroutine perm3(k,n,rows,count)
      implicit none

! Constants

      include 'params.inc'
c      integer    maxrows,  maxcols
c      parameter (maxrows=6,maxcols=720)

! Arguments

      integer rows
      integer k(maxrows),n(maxrows,maxcols),count

! Local Variables

      integer j,temp

!-----------
! Begin Code
!-----------
      do j=1,3
         temp=k(j)
         k(j)=k(1)
         call perm2(k(2),n,rows,count)
         n(rows-2,count-1)=temp
         n(rows-2,count)  =temp
         k(j)=temp
      enddo
      end

!-------------------------------------
      subroutine perm2(k,n,rows,count)
      implicit none

! Constants

      include 'params.inc'
c      integer    maxrows,  maxcols
c      parameter (maxrows=6,maxcols=720)

! Arguments

      integer rows
      integer k(maxrows),n(maxrows,maxcols),count

!-----------
! Begin Code
!-----------
      count=count+1
      n(rows-1,count)=k(1)
      n(rows,count)  =k(2)
      count=count+1
      n(rows-1,count)=k(2)
      n(rows,count)  =k(1)
      end



