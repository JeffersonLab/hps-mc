      subroutine open_file_mdl(lun,filename,fopened)
c***********************************************************************
c     opens file input-card.dat in current directory or above
c***********************************************************************
      implicit none
c
c     Arguments
c
      integer lun
      logical fopened
      character*(*) filename
      character*30  tempname
      integer fine
      integer dirup
      
c-----
c  Begin Code
c-----
c
c     first check that we will end in the main directory
c
      open(unit=lun,file="Source/makefile",status='old',err=20)
      dirup=0
      goto 100
 20   close(lun)

      open(unit=lun,file="../Source/makefile",status='old',err=30)
      dirup=1
      goto 100
 30   close(lun)

      open(unit=lun,file="../../Source/makefile",status='old',err=40)
      dirup=2
      goto 100
 40   close(lun)

      open(unit=lun,file="../../../Source/makefile",status='old',err=50)
      dirup=3
      goto 100
 50   close(lun)

      open(unit=lun,file="../../../../Source/makefile",status='old',err=60)
      dirup=4
      goto 100
 60   close(lun)

      open(unit=lun,file=filename,status='old',err=70)
      dirup=5
      goto 100
 70   close(lun)

 100  continue
      close(lun)

      fopened=.true.
      tempname=filename
      fine=index(tempname,' ')
      if(fine.eq.0) fine=len(tempname)
c
c 	  if I have to read a card
c
	  if(index(filename,"_card").gt.0) then
	     tempname='/Cards/'//tempname(1:fine)
	     fine=fine+7
      endif
      
      if(dirup.eq.0) open(unit=lun,file=tempname(1:fine),status='old',err=110)
      if(dirup.eq.1) open(unit=lun,file='../'//tempname(1:fine),status='old',err=110)
      if(dirup.eq.2) open(unit=lun,file='../../'//tempname(1:fine),status='old',err=110)
      if(dirup.eq.3) open(unit=lun,file='../../../'//tempname(1:fine),status='old',err=110)
      if(dirup.eq.4) open(unit=lun,file='../../../../'//tempname(1:fine),status='old',err=110)
      if(dirup.eq.5) open(unit=lun,file=filename,status='old',err=110)
      return

 110  fopened=.false.
      close(lun) 
      write (*,*) 'Warning: file ',tempname(1:fine),' is not in the main directory'

      return
      end
