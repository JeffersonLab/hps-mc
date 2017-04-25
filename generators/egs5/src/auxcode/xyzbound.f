!-------------------------------xyzbound.f------------------------------
! Version: 070116-1700
!-----------------------------------------------------------------------
!23456789|123456789|123456789|123456789|123456789|123456789|123456789|12

! ----------------------------------------------------------------------
! Auxiliary (geometry) subroutine for use with the EGS5 Code System
! ----------------------------------------------------------------------
! Subroutine xyzbound reads in an checks the boundary coordinates.
! ----------------------------------------------------------------------

      subroutine xyzbound(maxx,maxy,maxz)

      implicit none

      include 'auxcommons/aux_h.f'        ! Auxiliary-code "header" file
      include 'auxcommons/geoxyz.f'              ! Auxiliary-code COMMON

      real*8                                           ! Local variables
     * width
      integer
     * maxbd,maxx,maxy,maxz,i,ngroup,igroup,nn,nnn,in

 100  FORMAT(/,T20,'INPUT BOUNDARIES IN THE X DIRECTION')
 200  FORMAT(/,T20,'INPUT BOUNDARIES IN THE Y DIRECTION')
 300  FORMAT(/,T20,'INPUT BOUNDARIES IN THE Z DIRECTION')

1350  FORMAT(' INNER boundary for INDEX=',I3)
1360  FORMAT(F10.0)
1370  FORMAT(//,' BOUNDARY OUT OF ORDER**************')
1380  FORMAT(1X,T10,G15.7)
1390  FORMAT(' OUTER boundary for INDEX=',I3)
1420  FORMAT(' INITIAL boundary: ')
1440  FORMAT(1X,G15.7)
1460  FORMAT(' WIDTH in this group, NO. OF REGIONS in group: ')
1470  FORMAT(F10.0,I5)
1480  FORMAT(1X,G15.7,I5)
1500  FORMAT(/,' Program stopped in SUBROUTINE GetXYZ',/,
     *               ' because MXXPLNS, MXYPLNS, MXZPLNS too small')
1510  FORMAT(' Boundaries:',/,(6G15.7))

!     --------------------
!     Get the x-boundaries
!     --------------------
      maxbd = MXXPLNS
      write(6,100)
      if (maxx .gt. 0) then     ! Just pick up boundaries, one-at-a-time
        do i=1,maxx
          write(6,1350) i
!1350  FORMAT(' INNER boundary for INDEX=',I3)
          read(5,1360) xbound(i)
!1360  FORMAT(F10.0)
          if (i .ne. 1 .and. xbound(i) .le. xbound(i-1)) write(6,1370)
          write(6,1380) xbound(i)
!1380  FORMAT(1X,T10,G15.7)
        end do
        write(6,1390) maxx
!1390  FORMAT(' OUTER boundary for INDEX=',I3)
        read(5,1360) xbound(maxx+1)
        write(6,1380) xbound(maxx+1)
!1380  FORMAT(1X,T10,G15.7)
      else  ! maxx < 0,  Input GROUPS of regions 
            ! Assume maxbd set to MXXPLNS
        write(6,1420)
        read(5,1360) xbound(1)
        write(6,1440) xbound(1)
        ngroup = -maxx              ! Number of groups in this direction
        maxx = 0
        do igroup=1,ngroup
          write(6,1460)
          read(5,1470) width,nn
          if (nn .le. 0) nn = 1
          if (width .le. 0.) width = 1.D0
          write(6,1480) width,nn
          nnn = min(nn,maxbd-maxx) ! Ensures not adding too many regions
          if (nnn .ne. 0) then
            do in=maxx+1,maxx+nnn
              xbound(in+1) = xbound(in) + width
            end do
          end if
          if (nn .ne. nnn) then
            write(6,1500)
            stop
          end if
          maxx = maxx + nnn
        end do
      end if
      write(6,1510) (xbound(i),i=1,maxx+1)
      imax = maxx

!     --------------------
!     Get the y-boundaries
!     --------------------
      maxbd = MXYPLNS
      write(6,200)
      if (maxy .gt. 0) then     ! Just pick up boundaries, one-at-a-time
        do i=1,maxy
          write(6,1350) i
          read(5,1360) ybound(i)
          if (i .ne. 1 .and. ybound(i) .le. ybound(i-1)) write(6,1370)
          write(6,1380) ybound(i)
        end do
        write(6,1390) maxy
        read(5,1360) ybound(maxy+1)
        write(6,1380) ybound(maxy+1)
      else  ! maxy < 0,  Input GROUPS of regions 
            ! Assume maxbd set to MXYPLNS
        write(6,1420)
        read(5,1360) ybound(1)
        write(6,1440) ybound(1)
        ngroup = -maxy              ! Number of groups in this direction
        maxy = 0
        do igroup=1,ngroup
          write(6,1460)
          read(5,1470) width,nn
          if (nn .le. 0) nn = 1
          if (width .le. 0.) width = 1.D0
          write(6,1480) width,nn
          nnn = min(nn,maxbd-maxy) ! Ensures not adding too many regions
          if (nnn .ne. 0) then
            do in=maxy+1,maxy+nnn
              ybound(in+1) = ybound(in) + width
            end do
          end if
          if (nn .ne. nnn) then
            write(6,1500)
            stop
          end if
          maxy = maxy + nnn
        end do
      end if
      write(6,1510) (ybound(i),i=1,maxy+1)
      jmax = maxy

!     --------------------
!     Get the z-boundaries
!     --------------------
      maxbd = MXZPLNS
      write(6,300)
      if (maxz .gt. 0) then     ! Just pick up boundaries, one-at-a-time
        do i=1,maxz
          write(6,1350) i
          read(5,1360) zbound(i)
          if (i .ne. 1 .and. zbound(i) .le. zbound(i-1)) write(6,1370)
          write(6,1380) zbound(i)
        end do
        write(6,1390) maxz
        read(5,1360) zbound(maxz+1)
        write(6,1380) zbound(maxz+1)
      else  ! maxz < 0,  Input GROUPS of regions 
            ! Assume maxbd set to MXZPLNS
        write(6,1420)
        read(5,1360) zbound(1)
        write(6,1440) zbound(1)
        ngroup = -maxz              ! Number of groups in this direction
        maxz = 0
        do igroup=1,ngroup
          write(6,1460)
          read(5,1470) width,nn
          if (nn .le. 0) nn = 1
          if (width .le. 0.) width = 1.D0
          write(6,1480) width,nn
          nnn = min(nn,maxbd-maxz) ! Ensures not adding too many regions
          if (nnn .ne. 0) then
            do in=maxz+1,maxz+nnn
              zbound(in+1) = zbound(in) + width
            end do
          end if
          if (nn .ne. nnn) then
            write(6,1500)
            stop
          end if
          maxz = maxz + nnn
        end do
      end if
      write(6,1510) (zbound(i),i=1,maxz+1)
      kmax = maxz

      return

      end

!-------------------------------xyzbound.f------------------------------
