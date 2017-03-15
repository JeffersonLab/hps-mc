!-----------------------
! madgraph - a Feynman Diagram package by Tim Stelzer and Bill Long 
! (c) 1993
!
! Filename: eigen_lapack.f
!-----------------------
 
      Subroutine Eigensys(red_color,eigen_val,nrows)
!**************************************************************************
!     This routine interfaces gencolor to the eigensystem routines
!     from the Lapack program library from netlib.
!**************************************************************************
      implicit none

! Constants

      include 'params.inc'
c      integer    maxgraphs
c      parameter (maxgraphs=999)
c      integer    maxcolors
c      parameter (maxcolors=250)

! Arguments

      double precision  red_color(maxcolors,maxgraphs)
      double precision  eigen_val(maxcolors)
      integer nrows

! Local Variables

      double precision  work(4*maxcolors)
      integer info

!-----------
! Begin Code
!-----------
      call dsyev('V','L',nrows,red_color,maxcolors,eigen_val,
     &           work,4*maxcolors,info)
      if (info .ne. 0) then
         print*,' Error in eigenvalue finder, info = ',info
      end if
      end
