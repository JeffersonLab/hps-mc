      subroutine block_stat(i,tag)
      implicit none
c
c     arguments
c
      integer config,perm_pos
      common /to_config/ config,perm_pos
      double precision    S,X1,X2,PSWGT,JAC
      common /PHASESPACE/ S,X1,X2,PSWGT,JAC
      integer    max_blok
      parameter (max_blok=  20)
      integer    max_perm
      parameter (max_perm=  720)
      integer i
      character*20 tag
c
c     global
c
      integer good_points(max_blok,max_blok,max_perm),bad_points(max_blok,max_blok,max_perm)
      common /to_stat/good_points, bad_points
c---
c Begin code
c---
      if (jac.le.0) then
        bad_points(i,config,perm_pos)=bad_points(i,config,perm_pos)+1
      else
        good_points(i,config,perm_pos)=good_points(i,config,perm_pos)+1
      endif

      return
      end

      subroutine clear_block_stat(config,perm_pos)
      implicit none
c
c     arguments
c
      integer    max_blok
      parameter (max_blok=  20)
      integer    max_perm
      parameter (max_perm=  720)
      integer i,config,perm_pos
c
c     global
c
      integer good_points(max_blok,max_blok,max_perm),bad_points(max_blok,max_blok,max_perm)
      common /to_stat/good_points, bad_points
c---
c Begin code
c---
      do i=1,max_blok
               bad_points(i,config,perm_pos)=0
               good_points(i,config,perm_pos)=0
      enddo

      return
      end


