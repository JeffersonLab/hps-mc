      Integer Function NextUnopen()
c********************************************************************
C     Returns an unallocated FORTRAN i/o unit.
c********************************************************************

      Logical EX
C
      Do 10 N = 10, 300
         INQUIRE (UNIT=N, OPENED=EX)
         If (.NOT. EX) then
            NextUnopen = N
            Return
         Endif
 10   Continue
      Stop ' There is no available I/O unit. '
C               *************************
      End



      subroutine OpenData(Tablefile)
c********************************************************************
c generic subroutine to open the table files in the right directories
c********************************************************************
      implicit none
c
      Character Tablefile*40,up*3,dir*15
      data up,dir/'../','../lib/Pdfdata/'/
      Integer IU,NextUnopen
      External NextUnopen
      common/IU/IU
c
c--   start
c
      IU=NextUnopen()

c     first try in the current directory

      Open(IU, File=Tablefile, Status='OLD', Err=100)
      return
 100  continue
c     move up one step
      Open(IU, File=dir//Tablefile, Status='OLD', Err=101)
      return
 101  continue
c     move up one step
      Open(IU, File=up//dir//Tablefile, Status='OLD', Err=102)
      return
 102  continue
c     move up one more step
      Open(IU, File=up//up//dir//Tablefile, Status='OLD', Err=103)
      return
 103  continue
c     move up one more step
      Open(IU, File=up//up//up//dir//Tablefile, Status='OLD', Err=104)
      return
 104  continue
c     move up one more step
      Open(IU, File=up//up//up//up//dir//Tablefile,Status='OLD',Err=105)
      return
 105  continue




      print*,'table for the pdf NOT found!!!'
      
      return
      end

