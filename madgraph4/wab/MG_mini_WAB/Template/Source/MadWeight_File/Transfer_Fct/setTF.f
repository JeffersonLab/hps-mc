      subroutine setTF
      implicit none
c     
      include 'TF_param.inc'

      call Init_MET_LHCO

      include 'transfer_card.inc'

      return
 99   write(*,*) 'error in reading'
      return
      end
