!-----------------------------------------------------------------------
!                             SUBROUTINE PELASTINO  
!  Version: 060314-0810
!-----------------------------------------------------------------------
!23456789|123456789|123456789|123456789|123456789|123456789|123456789|12

      subroutine prelastino
C
C  This is subroutine simply writes out the current problem multiple
C  scattering parameters so that they can be compared in hatch/rmsfit
C  with the data in the gsdist.dat file, if GS dist is requested.
C
      implicit none

      include 'include/egs5_h.f'
      include 'include/egs5_media.f'
      include 'pegscommons/dcsstr.f'

      integer i,n,didGS

!  write just the material listing for this file, even when
!  GS is not being used

      didGS = 0
      write(17,*) nsdcs
      do n = 1, nsdcs
        write(17,5001) (mednam(n,i),i=1,24)
        write(17,*)  didGS,charD(n),efrch(n),efrcl(n),
     *              egrdhi(n),egrdlo(n)
      end do
5001  format(' MEDIUM=',24A1)

      return
      END
