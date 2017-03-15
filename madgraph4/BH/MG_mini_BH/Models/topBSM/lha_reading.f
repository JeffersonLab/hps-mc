           subroutine lh_readin(param_name)
c----------------------------------------------------------------------
c Read the parameters from the lh file
c
c 1. Input values for the EW sector
c 2. Higgs mass and width
c 3. Fermion masses (pole and MSbar) and widths
c----------------------------------------------------------------------
      implicit none
c
c     parameters
c
      integer maxpara
      parameter (maxpara=200)
c
c     local
c
	  character*(*) param_name	
          integer npara,l1,l2,id
          character*132 buff
          real*8 real_value
          real*8 value(maxpara)
          integer ivalue(maxpara),n
          character*20 name(maxpara),bn
          logical block_found,done,fopened
      integer iunit,i,name_length,idum
c
c       block info
c
      character*20 block_name
c
c   Common
c
          include 'coupl.inc'
	  include 'input.inc'
c
c     Common to lh_readin and printout
c
      double precision  alpha, sin2w, gfermi, alfas
      double precision  mtMS,mbMS,mcMS,mtaMS!MSbar masses
      double precision  Vud,Vus             !CKM matrix elements
      common/values/    alpha,sin2w,gfermi,alfas,
     &                  mtMS,mbMS,mcMS,mtaMS,
     &                  Vud
c
c----------
c     start
c----------
c
c     open file
c
      iunit=14
      call open_file_mdl(iunit,param_name,fopened)
           done=.false.

       n=0
           do while(.not.done)
           block_found=.false.
c
c looks for the blocks or for decay
c
      do while(.not.block_found)
       read(iunit,'(a132)',end=99,err=99) buff
c--     change to lower case
           call case_trap(buff,20)
       if(buff(1:5).eq.'block') then
         if(index(buff,"#").ne.0) l1=index(buff,"#")-1
         block_name=buff(6:min(l1,26))
         call no_spaces(block_name,name_length)
c        write(*,*) block_name(1:name_length)
         block_found=.true.
           elseif(buff(1:5).eq.'decay') then
               n=n+1
               l1=30
               if(index(buff,"#").ne.0) l1=index(buff,"#")-1 ! ignore comments
               read(buff(6:l1),*) ivalue(n),value(n)
               name(n)="decay"
       endif
      end do
c
c

      if(block_found) then
 	  do while(.true.)
          read(iunit,'(a132)',end=99,err=99) buff
     	  call case_trap(buff,20)
          if(buff(1:1).eq.'b'.or.buff(1:1).eq.'d') then
          	backspace iunit
          	exit
          endif	         	
            if(buff(1:1).ne.'#') then  !if it not a comment
              n=n+1	       
              l1=30
              if(index(buff,"#").ne.0) l1=index(buff,"#")-1 ! ignore comments       
c
c  WARNING:... not all blocks have the same sintax!! You need to change it
c  depending on the block you are reading
c
             if(block_name(1:5).eq."mgckm") then
                read(buff(1:l1),*) ivalue(n),idum,value(n)
              else
                read(buff(1:l1),*) ivalue(n),value(n)
              endif  
              name(n)=block_name(1:name_length)
c              write(*,"(1x,i2,2x,e16.8,1x,a)") 
c     &        ivalue(n),value(n),name(n)
           	  endif
      end do ! do while in the block
      else
	  done=.true.
	  endif
	  end do ! do while the entire file
	  

99	continue      
	
	   bn="sminputs"
       call set_it(n,ivalue,value,name,1,bn,alpha,128.9d0)
       alpha=1d0/alpha
       call set_it(n,ivalue,value,name,2,bn,gfermi,0.1166d-4)
       call set_it(n,ivalue,value,name,3,bn,alfas,0.119d0)
       call set_it(n,ivalue,value,name,4,bn,zmass,91.188d0)
       call set_it(n,ivalue,value,name,6,bn,tmass,174.3d0)
       call set_it(n,ivalue,value,name,7,bn,lmass,1.777d0)
	   bn="mgsmparam"
       call set_it(n,ivalue,value,name,1,bn,sin2w,0.2312d0)
       call set_it(n,ivalue,value,name,2,bn,wmass,80.419d0)
       bn="mgyukawa"
       call set_it(n,ivalue,value,name,4,bn,mcMS,1.25d0)
       call set_it(n,ivalue,value,name,5,bn,mbMS,4.2d0)
       call set_it(n,ivalue,value,name,6,bn,mtMS,174d0)
       call set_it(n,ivalue,value,name,15,bn,mtaMS,1.777d0)
       bn="mgckm"
       call set_it(n,ivalue,value,name,1,bn,vud,1d0)
       bn="mass"
       call set_it(n,ivalue,value,name,4,bn,cmass,1.4d0)
       call set_it(n,ivalue,value,name,5,bn,bmass,4.7d0)
       call set_it(n,ivalue,value,name,6,bn,tmass,tmass*1d0)
       call set_it(n,ivalue,value,name,15,bn,lmass,lmass*1d0)
       call set_it(n,ivalue,value,name,25,bn,hmass,120d0)
       call set_it(n,ivalue,value,name,23,bn,zmass,zmass*1d0)
       call set_it(n,ivalue,value,name,24,bn,wmass,wmass*1d0)
       call set_it(n,ivalue,value,name,6000045,bn,s0mass,100d0)
       call set_it(n,ivalue,value,name,6000046,bn,o0mass,100d0)
       call set_it(n,ivalue,value,name,6000047,bn,s1mass,100d0)
       call set_it(n,ivalue,value,name,6000048,bn,o1mass,100d0)
       call set_it(n,ivalue,value,name,6000049,bn,s2mass,100d0)
       call set_it(n,ivalue,value,name,6000050,bn,g1mass,100d0)
       call set_it(n,ivalue,value,name,6000051,bn,g2mass,100d0)
       call set_it(n,ivalue,value,name,6000052,bn,g3mass,100d0)
       call set_it(n,ivalue,value,name,6000053,bn,g4mass,100d0)
       call set_it(n,ivalue,value,name,6000054,bn,g5mass,100d0)
       call set_it(n,ivalue,value,name,6000055,bn,g6mass,100d0)
       call set_it(n,ivalue,value,name,6000056,bn,g7mass,100d0)
       call set_it(n,ivalue,value,name,6000057,bn,g8mass,100d0)
       call set_it(n,ivalue,value,name,6000058,bn,g9mass,100d0)
       call set_it(n,ivalue,value,name,6000059,bn,g0mass,100d0)
       bn="decay"
       call set_it(n,ivalue,value,name,6,bn,twidth,1.5083d0)
       call set_it(n,ivalue,value,name,25,bn,hwidth,0.0037d0)
       call set_it(n,ivalue,value,name,23,bn,zwidth,2.441d0)
       call set_it(n,ivalue,value,name,24,bn,wwidth,2.0476d0)

       call set_it(n,ivalue,value,name,6000045,bn,s0width,1d0)
       call set_it(n,ivalue,value,name,6000046,bn,o0width,1d0)
       call set_it(n,ivalue,value,name,6000047,bn,s1width,1d0)
       call set_it(n,ivalue,value,name,6000048,bn,o1width,1d0)
       call set_it(n,ivalue,value,name,6000049,bn,s2width,1d0)
       call set_it(n,ivalue,value,name,6000050,bn,g1width,1d0)
       call set_it(n,ivalue,value,name,6000051,bn,g2width,1d0)
       call set_it(n,ivalue,value,name,6000052,bn,g3width,1d0)
       call set_it(n,ivalue,value,name,6000053,bn,g4width,1d0)
       call set_it(n,ivalue,value,name,6000054,bn,g5width,1d0)
       call set_it(n,ivalue,value,name,6000055,bn,g6width,1d0)
       call set_it(n,ivalue,value,name,6000056,bn,g7width,1d0)
       call set_it(n,ivalue,value,name,6000057,bn,g8width,1d0)
       call set_it(n,ivalue,value,name,6000058,bn,g9width,1d0)
       call set_it(n,ivalue,value,name,6000059,bn,g0width,1d0)
       bn='mguser'
       call set_it(n,ivalue,value,name, 1,bn,s0scalarf  ,0d0)
       call set_it(n,ivalue,value,name, 2,bn,s0axialf   ,0d0)
       call set_it(n,ivalue,value,name, 3,bn,o0scalarf  ,0d0)
       call set_it(n,ivalue,value,name, 4,bn,o0axialf   ,0d0)
       call set_it(n,ivalue,value,name, 5,bn,s1quleft   ,0d0)
       call set_it(n,ivalue,value,name, 6,bn,s1quright  ,0d0)
       call set_it(n,ivalue,value,name, 7,bn,s1qdleft   ,0d0)
       call set_it(n,ivalue,value,name, 8,bn,s1qdright  ,0d0)
       call set_it(n,ivalue,value,name, 9,bn,s1qcleft   ,0d0)
       call set_it(n,ivalue,value,name,10,bn,s1qcright  ,0d0)
       call set_it(n,ivalue,value,name,11,bn,s1qsleft   ,0d0)
       call set_it(n,ivalue,value,name,12,bn,s1qsright  ,0d0)
       call set_it(n,ivalue,value,name,13,bn,s1qtleft   ,0d0)
       call set_it(n,ivalue,value,name,14,bn,s1qtright  ,0d0)
       call set_it(n,ivalue,value,name,15,bn,s1qbleft   ,0d0)
       call set_it(n,ivalue,value,name,16,bn,s1qbright  ,0d0)
       call set_it(n,ivalue,value,name,17,bn,s1eleft    ,0d0)
       call set_it(n,ivalue,value,name,18,bn,s1eright   ,0d0)
       call set_it(n,ivalue,value,name,19,bn,s1muleft   ,0d0)
       call set_it(n,ivalue,value,name,20,bn,s1muright  ,0d0)
       call set_it(n,ivalue,value,name,21,bn,s1taleft   ,0d0)
       call set_it(n,ivalue,value,name,22,bn,s1taright  ,0d0)
       call set_it(n,ivalue,value,name,23,bn,s1ne        ,0d0)
       call set_it(n,ivalue,value,name,24,bn,s1nm        ,0d0)
       call set_it(n,ivalue,value,name,25,bn,s1nt        ,0d0)
       call set_it(n,ivalue,value,name,26,bn,o1quleft   ,0d0)
       call set_it(n,ivalue,value,name,27,bn,o1quright  ,0d0)
       call set_it(n,ivalue,value,name,28,bn,o1qdleft   ,0d0)
       call set_it(n,ivalue,value,name,29,bn,o1qdright  ,0d0)
       call set_it(n,ivalue,value,name,30,bn,o1qcleft   ,0d0)
       call set_it(n,ivalue,value,name,31,bn,o1qcright  ,0d0)
       call set_it(n,ivalue,value,name,32,bn,o1qsleft   ,0d0)
       call set_it(n,ivalue,value,name,33,bn,o1qsright  ,0d0)
       call set_it(n,ivalue,value,name,34,bn,o1qtleft   ,0d0)
       call set_it(n,ivalue,value,name,35,bn,o1qtright  ,0d0)
       call set_it(n,ivalue,value,name,36,bn,o1qbleft   ,0d0)
       call set_it(n,ivalue,value,name,37,bn,o1qbright  ,0d0)
       call set_it(n,ivalue,value,name,38,bn,kapmpl     ,0d0)
       call set_it(n,ivalue,value,name,39,bn,addn       ,0d0)
       call set_it(n,ivalue,value,name,40,bn,mstring    ,0d0)
      return
      end

      
      subroutine set_it(npara,ivalue,value,name,id,
     &                  block_name,var,def_value)
c----------------------------------------------------------------------------------
c     finds the parameter value  in block_name and associate var to it.
c     If it is not found a default is given.
c----------------------------------------------------------------------------------
      implicit none

c
c     parameters
c
      integer maxpara
      parameter (maxpara=200)
c
c     arguments
c
      integer npara,ivalue(maxpara),id
      character*20  block_name,name(maxpara)
      real*8 var,def_value,value(maxpara)
c
c     local
c
      logical found
      integer i
c
c     start
c
	  found=.false.
      do i=1,npara
         found = (id.eq.ivalue(i)).and.(name(i).eq.block_name)
 	               if(found) then
         	var=value(i)
            exit
          endif	
      enddo
      
      if (.not.found) then
c         write (*,*) "Warning: parameter not found"
c         write (*,*) "         setting it to default value ",def_value
         var=def_value
      endif
      return

      end
      
      
      subroutine case_trap(string,length)
c**********************************************************    
c change string to lowercase if the input is not
c**********************************************************
      implicit none
c
c     ARGUMENT
c      
      character*(*) string
      integer length
c
c     LOCAL
c
      integer i,k

      do i=1,length
         k=ichar(string(i:i))
         if(k.ge.65.and.k.le.90) then  !upper case A-Z
            k=ichar(string(i:i))+32   
            string(i:i)=char(k)        
         endif
      enddo

      return
      end


      subroutine no_spaces(buff,nchars)
c**********************************************************************
c     Given buff a buffer of words separated by spaces
c     returns it where all space are moved to the right
c     returns also the length of the single word.
c     maxlength is the length of the buffer
c**********************************************************************
      implicit none
c
c     Constants
c
      integer    maxline
      parameter (maxline=20)
      character*1 null
      parameter  (null=' ')
c
c     Arguments
c
      character*(maxline) buff
      integer nchars,maxlength
c
c     Local
c
      integer i,j
      character*(maxline) temp
c-----
c  Begin Code
c-----
      nchars=0
c      write (*,*) "buff=",buff(1:maxlength)
      do i=1,maxline
         if(buff(i:i).ne.null) then
            nchars=nchars+1
            temp(nchars:nchars)=buff(i:i)
         endif
c         write(*,*) i,":",buff(1:maxlength),":",temp(1:nchars),":"
      enddo
      buff=temp      
      end

      
