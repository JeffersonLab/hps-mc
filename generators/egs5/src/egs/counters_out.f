!------------------------------counters_out.f---------------------------
! Version: 051227-1600
!-----------------------------------------------------------------------
!23456789|123456789|123456789|123456789|123456789|123456789|123456789|12

      subroutine counters_out(ioflag)
      
      implicit none

      include 'include/counters.f'

      integer ioflag                                         ! Arguments

      if (ioflag.eq.0) then       ! Open unit=99 and initialize counters
        OPEN(UNIT=99,FILE='egs5job.out99',STATUS='UNKNOWN')
        iannih           = 0
        iaphi            = 0
        ibhabha          = 0
        ibrems           = 0 
        icollis          = 0
        icompt           = 0
        iedgbin          = 0
        ieii             = 0
        ielectr          = 0
        ihatch           = 0
        ihardx           = 0
        ikauger          = 0
        ikshell          = 0
        ikxray           = 0
        ilauger          = 0
        ilshell          = 0
        ilxray           = 0
        imoller          = 0
        imscat           = 0
        ipair            = 0
        iphoto           = 0
        iphoton          = 0
        iraylei          = 0
        ishower          = 0
        iuphi            = 0
        itmxs            = 0
        noscat           = 0
        iblock           = 0
      else if (ioflag.eq.1) then
        write(99,*) 'Values for subroutine-entry counters:'
        write(99,*)
        write(99,*) 'iannih =',iannih
        write(99,*) 'iaphi  =',iaphi
        write(99,*) 'ibhabha=',ibhabha
        write(99,*) 'ibrems =',ibrems
        write(99,*) 'icollis=',icollis
        write(99,*) 'icompt =',icompt
        write(99,*) 'iedgbin=',iedgbin
        write(99,*) 'ieii   =',ieii
        write(99,*) 'ielectr=',ielectr
        write(99,*) 'ihatch =',ihatch
        write(99,*) 'ihardx =',ihardx
        write(99,*) 'ikauger=',ikauger
        write(99,*) 'ikshell=',ikshell
        write(99,*) 'ikxray =',ikxray
        write(99,*) 'ilauger=',ilauger
        write(99,*) 'ilshell=',ilshell
        write(99,*) 'ilxray =',ilxray
        write(99,*) 'imoller=',imoller
        write(99,*) 'imscat =',imscat
        write(99,*) 'ipair  =',ipair
        write(99,*) 'iphoto =',iphoto
        write(99,*) 'iphoton=',iphoton
        write(99,*) 'iraylei=',iraylei
        write(99,*) 'ishower=',ishower
        write(99,*) 'iuphi  =',iuphi
        write(99,*) 'itmxs  =',itmxs
        write(99,*) 'noscat =',noscat
        write(99,*) 'iblock =',iblock
        CLOSE(UNIT=99)
      else
        write(6,*) ' *** Error using subroutine counters_out'
        write(6,*) '       ioflag=',ioflag
        CLOSE(UNIT=99)
        write(6,*) '     Program stopped'
        stop
      end if      

      return
      end
!----------------------last line of counters_out.f----------------------
