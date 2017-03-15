c *************************************************************************
c **                                                                     **
c **                    LHA format reading routines                      **
c **                                                                     **
c *************************************************************************


c +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
c ++
c ++ LHA_islatin -> islatin=true if letter is a latin letter
c ++
c +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

      subroutine LHA_islatin_m(letter,islatin)
      implicit none

      logical islatin
      character letter
      character*26 ref
      integer i

      islatin=.false.
      ref='abcdefghijklmnopqrstuvwxyz'

      call case_trap2_m(letter)

      do i=1,26
	if(letter .eq. ref(i:i)) islatin=.true.
      end do

      end

c +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
c ++
c ++ LHA_isnum -> isnum=true if letter is a number
c ++
c +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

      subroutine LHA_isnum_m(letter,isnum)
      implicit none

      logical isnum
      character letter
      character*10 ref
      integer i

      isnum=.false.
      ref='1234567890'

      do i=1,10
	if(letter .eq. ref(i:i)) isnum=.true.
      end do

      end

c +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
c ++
c ++ LHA_firststring -> first is the first "word" of string
c ++ Warning: string is returned with first REMOVED!
c ++
c +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

      subroutine LHA_firststring_m(first,string)


      implicit none
      character*(*) string
      character*20 first
      integer*4 i

      i=1
      do while(string(i:i) .eq. ' ') 
         i=i+1
      end do
      
      string=string(i:len(string))
      first=string(1:index(string,' ')-1)
      string=string(index(string,' '):len(string))
      end
c +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
c ++
c ++ LHA_blockread -> read a LHA line and return parameter name (evntually found in 
c ++ a ref file) and value
c ++
c +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

      subroutine LHA_blockread_m(blockname,buff,par,val)

      implicit none
      character*132 buff,buffer,curr_ref,curr_buff
      character*20 blockname,val,par,temp,first_ref,first_line
      logical fopened
      integer ref_file/20/
      logical islast,isnum
      character*20 temp_val

c     *********************************************************************
c     A default variable name is conscructed (e.g. block_id1_id2)
c
      temp_val=''
      par=blockname
      curr_buff=buff
      isnum=.true.

10    val=temp_val
      call LHA_firststring_m(temp_val,curr_buff)
      call LHA_isnum_m(temp_val(1:1),isnum)
      if (isnum) then
           if(val .ne. '') then 
		par(index(par,' '):index(par,' '))='_'
                par(index(par,' '):index(par,' ')+index(val,' '))=val(1:index(val,' ')-1)
           end if
           goto 10
      end if
      call case_trap2_m(par)

c     *********************************************************************
c     Try to find a correspondance in IdentNameFile
c

      call open_file_mdl(ref_file,'ident_card.dat',fopened)
      if(.not. fopened) goto 99	! If the file does not exist -> no matter, use default!
		
      do while(.true.) !run over reference file

        ! Seek a corresponding blockname
        read(ref_file,'(a132)',end=99) buffer
        call LHA_firststring_m(temp,buffer)
        call case_trap2_m(temp)
        if(temp .eq. blockname) then
                ! Seek for a corresponding LHA code
		curr_ref=buffer
		curr_buff=buff
		islast=.false.
		do while(.not. islast)
			call LHA_firststring_m(first_ref,curr_ref)
                	call LHA_firststring_m(first_line,curr_buff)
			call LHA_islatin_m(first_ref(1:1),islast)
			if (islast) then
				par=first_ref
				val=first_line ! If found set param name & value
				goto 99
			end if
			if (first_ref .ne. first_line) goto 98
		end do
	end if
98    end do
99    return	
      end


c +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
c ++
c ++ Load_lha_para -> Open a LHA file and load all model param in a table
c ++
c +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

      subroutine load_lha_para_m(npara,param,value,param_name)

      implicit none

      character*(*) param_name
      integer maxpara
      parameter (maxpara=1000)
      character*20 param(maxpara),value(maxpara),val,par
      character*20 blockname
      integer npara
      logical fopened
      integer iunit,GL,logfile
      character*20 ctemp
      character*132 buff
      character*5 tag
      character*132 temp
      data iunit/21/
      data logfile/22/
      GL=0
      npara=1

      param(1)=' '
      value(1)=' '

      ! Try to open param-card file
      call open_file_mdl(iunit,param_name,fopened)
      if(.not.fopened) then
         print *,'Error: Could not open file ',param_name
         print *,'Exiting'
         stop
      endif

      ! Try to open log file
      open (unit = logfile, file = "param.log")

      ! Scan the data file
      do while(.true.)  
         read(iunit,'(a132)',end=99,err=99) buff
            if(buff(1:1) .ne.'#') then ! Skip comments

		  tag=buff(1:5)
		  call case_trap2_m(tag) ! Select decay/block tag

                  if(tag .eq. 'block') then ! If we are in a block, get the blockname
		     temp=buff(7:132)
		     call LHA_firststring_m(blockname,temp)
                     call case_trap2_m(blockname)
		     goto 50
		  endif

                  if(tag .eq. 'decay') then ! If we are in a decay, directly try to get back the correct name/value pair
                    blockname='decay'
		    temp=buff(7:132)
		    call LHA_blockread_m(blockname,temp,par,val)
                    GL=1
                  endif

		  if(blockname .ne. 'decay')  then ! If we are in valid block, try to get back a name/value pair
			call LHA_blockread_m(blockname,buff,par,val)
			GL=1
		  end if

c                  *******************
c                  *if LHA_blockread has been called, record name and value
c                  *******************

                   if(GL .eq. 1) then
                      value(npara)=val
                      ctemp=value(npara)
                      call case_trap2_m(ctemp)
                      value(npara)=ctemp
                      param(npara)=par
                      ctemp=param(npara)
                      call case_trap2_m(ctemp)
                      param(npara)=ctemp
                      npara=npara+1
                      GL=0
		      write (logfile,*) 'Parameter ',ctemp,' has been read with value ',val
                   endif
c                  *******************
               endif
 50   enddo
      npara=npara-1
      close(iunit)
      close(logfile)
 99   return
      end


      subroutine load_para_m(npara,param,value)
c----------------------------------------------------------------------
c Read the params from the run_card.dat file
c----------------------------------------------------------------------
      implicit none
c
c     parameters
c
      integer maxpara
      parameter (maxpara=1000)
c
c     arguments
c
      character*20 param(maxpara),value(maxpara)
      integer npara
c
c     local
c
      logical fopened,done
      integer iunit
      character*20 ctemp
      integer k,i,l1,l2
      character*132 buff
      data iunit/21/
c
c----------
c     start
c----------
c
c     read the run_card.dat
c
      npara=0
      param(1)=' '
      value(1)=' '
c
c     open file
c
      call open_file_mdl(iunit,'run_card.dat',fopened)
      if(.not.fopened) return
c
c     read in values
c
      done=.false.
      do while(.not.done)
         read(iunit,'(a132)',end=99,err=99) buff
         if(buff(1:1).ne.'#' .and. index(buff,"=").gt.0) then
            l1=index(buff,"=")
            l2=index(buff,"!")
            if(l2.eq.0) l2=l1+20  !maybe there is no comment...
            npara=npara+1
c
             value(npara)=buff(1:l1-1)
             ctemp=value(npara)
             call case_trap2_m(ctemp)
             value(npara)=ctemp
c
             param(npara)=" "//buff(l1+1:l2-1)
             ctemp=param(npara)
             call case_trap2_m(ctemp)
             param(npara)=ctemp
c
         endif
      enddo
      close(iunit)

 99   return
      end



      subroutine get_real_m(npara,param,value,name,var,def_value)
c----------------------------------------------------------------------------------
c     finds the parameter named "name" in param and associate to "value" in value
c----------------------------------------------------------------------------------
      implicit none

c
c     parameters
c
      integer maxpara
      parameter (maxpara=1000)
c
c     arguments
c
      integer npara
      character*20 param(maxpara),value(maxpara)
      character*(*)  name
      real*8 var,def_value_num
      character*20 c_param,c_name,def_value
c
c     local
c
      logical found
      integer i
c
c     start
c
      i=1
      found=.false.
      do while(.not.found.and.i.le.npara)
          call firststring_m(c_param,param(i))
          call firststring_m(c_name,name)
         call case_trap2_m(c_name)
         call case_trap2_m(c_param)
         found = (c_param .eq. c_name)
         if (found) read(value(i),*) var
c         write (*,*) c_name,c_param,def_value
         i=i+1
      enddo
      if (.not.found) then
      read(def_value,*) def_value_num
         write (*,*) "Warning: parameter ",name," not found"
         write (*,*) "         setting it to default value ",def_value_num
         var=def_value_num
      endif
      return

      end
c

      subroutine get_integer_m(npara,param,value,name,var,def_value)
c----------------------------------------------------------------------------------
c     finds the parameter named "name" in param and associate to "value" in value
c----------------------------------------------------------------------------------
      implicit none
c
c     parameters
c
      integer maxpara
      parameter (maxpara=1000)
c
c     arguments
c
      integer npara
      character*20 param(maxpara),value(maxpara)
      character*(*)  name
      integer var,def_value
      character*20 c_param,c_name
c
c     local
c
      logical found
      integer i
c
c     start
c
      i=1
      found=.false.
      do while(.not.found.and.i.le.npara)
         call firststring_m(c_param,param(i))
          call firststring_m(c_name,name)
         found = (c_param .eq. c_name)
         if (found) read(value(i),*) var
c         if (found) write (*,*) name,var
         i=i+1
      enddo
      if (.not.found) then
         write (*,*) "Warning: parameter ",name," not found"
         write (*,*) "         setting it to default value ",def_value
         var=def_value
      endif
      return

      end
c
      subroutine get_string_m(npara,param,value,name,var,def_value)
c----------------------------------------------------------------------------------
c     finds the parameter named "name" in param and associate to "value" in value
c----------------------------------------------------------------------------------
      implicit none

c
c     parameters
c
      integer maxpara
      parameter (maxpara=1000)
c
c     arguments
c
      integer npara
      character*20 param(maxpara),value(maxpara)
      character*(*)  name
      character*(*)  var,def_value
      character*20 c_param,c_name
c
c     local
c
      logical found
      integer i
c
c     start
c
      i=1
      found=.false.
      do while(.not.found.and.i.le.npara)
         call firststring_m(c_param,param(i))
          call firststring_m(c_name,name)
         found = (c_param .eq. c_name)
         if (found) read(value(i),*) var
c         if (found) write (*,*) name,var
         i=i+1
      enddo
      if (.not.found) then
         write (*,*) "Warning: parameter ",name," not found"
         write (*,*) "         setting it to default value ",def_value
         var=def_value
      endif
      return

      end
c
      subroutine get_logical_m(npara,param,value,name,var,def_value)
c----------------------------------------------------------------------------------
c     finds the parameter named "name" in param and associate to "value" in value
c----------------------------------------------------------------------------------
      implicit none

c
c     parameters
c
      integer maxpara
      parameter (maxpara=1000)
c
c     arguments
c
      integer npara
      character*20 param(maxpara),value(maxpara)
      character*(*)  name
      logical  var,def_value
      character*20 c_param,c_name
c
c     local
c
      logical found
      integer i
c
c     start
c
      i=1
      found=.false.
      do while(.not.found.and.i.le.npara)
         call firststring_m(c_param,param(i))
          call firststring_m(c_name,name)
         found = (c_param .eq. c_name)
         if (found) read(value(i),*) var
c         if (found) write (*,*) name,var
         i=i+1
      enddo
      if (.not.found) then
         write (*,*) "Warning: parameter ",name," not found"
         write (*,*) "         setting it to default value ",def_value
         var=def_value
      endif
      return

      end
c



      subroutine case_trap2_m(name)
c**********************************************************
c change the string to lowercase if the input is not
c**********************************************************
      implicit none
c
c     ARGUMENT
c
      character*20 name
c
c     LOCAL
c
      integer i,k

      do i=1,20
         k=ichar(name(i:i))
         if(k.ge.65.and.k.le.90) then  !upper case A-Z
            k=ichar(name(i:i))+32
            name(i:i)=char(k)
         endif
      enddo

      return
      end

c +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
c ++
c ++ firststring -> return the first "word" of string
c ++ & remove whitespaces around
c ++ Needed to correct a bug in "get_" routines
c ++ Michel Herquet - CP3 - 05-04-2006
c ++
c +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

      subroutine firststring_m(first,string)

      implicit none
      character*(*) string
      character*20 first
      character*20 temp

      temp=string
      do while(temp(1:1) .eq. ' ')
        temp=temp(2:len(temp))
      end do
      first=temp(1:index(temp,' ')-1)

      end

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
c         if I have to read a card
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
      write (*,*) 'Warning: cannot find file ',filename

      return
      end

