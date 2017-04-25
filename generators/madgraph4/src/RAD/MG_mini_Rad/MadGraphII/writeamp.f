!-----------------------
! madgraph - a Feynman Diagram package by Tim Stelzer and Bill Long 
! (c) 1993
!
! Filename: writesub.f
!-----------------------
      subroutine write_symmetry
!***************************************************************************
c
c     Here we will write out the identical particle information
c
!***************************************************************************
      implicit none

! Constants

      include 'params.inc'

! Arguments


! Local Variables

      integer i,j,ident
      integer jmatch(0:maxlines,0:maxlines)
      logical done

! External
      
! Global Variables
      integer        iline(-maxlines:maxlines),idir(-maxlines:maxlines)
      integer        this_coup(max_coup) ,goal_coup(max_coup)
      common/to_proc/iline,idir,this_coup,goal_coup

!-----------
! Begin Code
!-----------

      jmatch(0,0) = 0
      ident = 0
      done = .false.
      do i=3,iline(0)
         j = 1
         done = .false.
         do while (j .le. jmatch(0,0) .and. .not. done)
            if (jmatch(0,j) .eq. iline(i)) then
               done=.true.
            else
               j=j+1
            endif
         enddo
         if (j .gt. jmatch(0,0)) then
            jmatch(0,0)=j
            jmatch(0,j)=iline(i)
            jmatch(j,0)=1
            jmatch(1,j)=i
         else
            jmatch(j,0)=jmatch(j,0)+1
            if (jmatch(j,0) .eq. 2) ident=ident+1  !Number of identical groups
            jmatch(jmatch(j,0),j)=i
         endif
      enddo
      open(unit=isubfile,file='mg.sym',status='unknown')
      write(isubfile,*) ident
      do j=1,jmatch(0,0)
         if (jmatch(j,0) .gt. 1) then
            write(isubfile,*) jmatch(j,0)
            do i=1,jmatch(j,0)
               write(isubfile,*) jmatch(i,j)
            enddo
         endif
      enddo
      close(isubfile)
      end
 
      subroutine writeamp(ngraphs,nexternal,nqed,nqcd,nproc,ncolor)
!***************************************************************************
!     writes out the subroutines
!***************************************************************************
      implicit none

! Constants

      include 'params.inc'
      include 'onia.inc'
      integer    ln,ln2   
      parameter (ln=92,ln2=58 )

! Arguments

      integer ngraphs,nqed,nqcd,nexternal,nproc,ncolor


! Local Variables

      character*75 buff
      character*20 name
c      character*4 temp
      integer i,jchar,ishift,nlength,j,k,ic,nflows
      integer idenom,inum
      logical injamp

! External
      
      logical equal
      integer igcf

! Global Variables
      double precision               den(maxgraphs)
      integer           neigen,nrows,jplace(maxgraphs)
      common /to_colormat/ den,neigen,nrows,jplace

      integer flows(0:2*maxlines,0:maxfactors,0:24,0:maxflows)
      integer         graphcolor(0:2,0:maxflows,0:maxgraphs)
      common/to_color/graphcolor,flows

      integer alen(maxgraphs)
      integer wnum(-maxlines:maxlines),wcount,wlen(maxgraphs*maxlines)
      common/toopt/wcount,wnum,wlen,alen

      character*25 gname
      integer iname
      common/to_name/iname,gname
      character*60 proc
      integer iproc
      common/to_write/iproc,proc
      integer            symfact(maxgraphs)
      common /tosymmetry/symfact

      character*70       cversion
      common/to_version/ cversion


      logical              StandAlone
      common/to_StandAlone/StandAlone

      logical DipolSub
      data DipolSub/.false./

      integer info_ijk(maxdipoles,2,5),nr,dipol
      logical offdiag
      common /ijk/ info_ijk,nr,offdiag
      common /dipolenumber/ dipol

      integer ncomb
      common /ihel/ ncomb

      integer         nincoming,nincfirst
      common/to_proc2/nincoming,nincfirst

      integer        iline(-maxlines:maxlines),idir(-maxlines:maxlines)
      integer        this_coup(max_coup) ,goal_coup(max_coup)
      common/to_proc/iline,idir,this_coup,goal_coup

      character*(max_string) iwave(max_particles),owave(max_particles)
      integer iposx(3,max_particles)
      integer info_p(5,max_particles)
      character*(8) str(3,max_particles)
      common/to_external/iwave,owave,iposx,info_p,str

      character*(8) mass(max_particles)
      common /masses/ mass

      character*4 onium_type
      common/ to_onium_type/ onium_type

!-----------
! Begin Code
!-----------
      if (onium) then
        if (L_qn.eq.0) then
          onium_type ='?S??'
        else
          onium_type ='?P??'
        endif
        write(onium_type(1:1),'(i1)') 2*S_qn+1
        write(onium_type(3:3),'(i1)') J_qn
        write(onium_type(4:4),'(i1)') C_qn      
        write(*,*) 'quantum state of onium:',onium_type
      endif


      ishift = iname
      nlength = ishift
      if (nproc .gt. 0) then
         open(unit=ln,file=gname(1:iname)//'.f',status='unknown')
         write(isubfile,'(a$)') gname(1:iname),'.o '
      else
         open(unit=ln,status='scratch')
      endif
      name = gname(1:iname)
      call upper_case(name)

      print*,'Writing function ',name(1:iname) ,
     &     ' in file ',GNAME(1:iname)//'.f.'

      call writesum(nproc,ngraphs,ncolor)

      if (onium) then
        buff = '     SUBROUTINE '
      else
        buff = 'REAL*8 FUNCTION '
      endif

      jchar = 17
      buff(jchar:jchar+ishift) = name(1:iname)
c      if(name(1:5).ne.'DIPOL') DipolSub=.false.
      jchar = jchar+ishift
      if(Dipolsub) then
       if(offdiag) then
        ishift=28
        buff(jchar:jchar+ishift) = '(P,NHEL,IC,P1,IHEL,STOREJAMP)'
       else
        ishift=13
        buff(jchar:jchar+ishift) = '(P,NHEL,IC,P1)'
       endif
      elseif (onium) then
        ishift= 15
        buff(jchar:jchar+ishift) = '(P,NHEL,IC,JAMP)'
      else
       ishift= 10
       buff(jchar:jchar+ishift) = '(P,NHEL,IC)'
      endif
      jchar =jchar+ishift
      ishift = 4
      write(ln,'(6x,a)') buff(1:jchar)

! write comments

      write(ln,5) ' '
      write(ln,5) cversion
      write(ln,5) 'RETURNS AMPLITUDE SQUARED SUMMED/AVG OVER COLORS'
      write(ln,5) 'FOR THE POINT WITH EXTERNAL LINES W(0:6,NEXTERNAL)'
c      write(ln,5) 'FOR THE POINT IN PHASE SPACE P(0:3,N_PARTICLES)'
      write(ln,5) ' ' 
      if (onium) then
        write(ln,5) 'FOR PROCESS '//proc(1:iproc)
     &   //onium_type(1:3)//'['//onium_type(4:4)//']'
      else
        write(ln,5) 'FOR PROCESS '//proc(1:iproc)
      endif
      write(ln,5) ' '

      write(ln,10) 'IMPLICIT NONE'

! write parameters

      write(ln,5) ' '
      write(ln,5) 'CONSTANTS'
      write(ln,5) ' '

c      if(.not.standalone) write(ln,10) 'include "genps.inc"'

      buff = 'INTEGER    NGRAPHS,    NEIGEN'
      write(ln,10) buff(1:30)
      buff = 'PARAMETER (NGRAPHS=????,NEIGEN=???)'
      write(buff(20:23),'(i4)') ngraphs
      write(buff(32:34),'(i3)') neigen
c      write(buff(46:46),'(i1)') nexternal
      write(ln,10) buff(1:36)

      if(.not.standalone.and..not.DipolSub)
     .            write(ln,10) 'include "genps.inc"'
      if(standalone.or.DipolSub)
     .            write(ln,10) 'include "nexternal.inc"'
      if(DipolSub)write(ln,10) 'include "dipole.inc"'

      buff = 'INTEGER    NWAVEFUNCS     , NCOLOR'
      write(ln,10) buff(1:34)
      buff = 'PARAMETER (NWAVEFUNCS=????, NCOLOR=????) '
      write(buff(23:26),'(i4)') WCOUNT
      write(buff(36:39),'(i4)') NCOLOR
c      write(buff(36:39),'(i4)') NROWS
      write(ln,10) buff(1:41)
      write(ln,10) 'REAL*8     ZERO'
      write(ln,10) 'PARAMETER (ZERO=0D0)'

! write argument declarations

      write(ln,5) ' '
      write(ln,5) 'ARGUMENTS '
      write(ln,5) ' '
 
      if(Dipolsub) then
         write(ln,10) 'REAL*8 P(0:3,NEXTERNAL),P1(0:3,NEXTERNAL)'
      elseif(onium) then
        write(ln,10) 'REAL*8 P(0:3,NEXTERNAL+1)'
      else
         write(ln,10) 'REAL*8 P(0:3,NEXTERNAL)'
      endif

      if(Dipolsub.and.(offdiag.eqv..false.)) then
         write(ln,10) 'INTEGER NHEL(NEXTERNAL), IC(NEXTERNAL)'
      elseif(Dipolsub.and.offdiag) then
         write(ln,10) 'INTEGER NHEL(NEXTERNAL), IC(NEXTERNAL),IHEL'
         buff= 'COMPLEX*16 STOREJAMP(???,2,NCOLOR)'
         write(buff(22:24),'(i3)') ncomb
         write(ln,10) buff(1:34)
      elseif(onium) then
        write(ln,10) 'INTEGER NHEL(NEXTERNAL+1), IC(NEXTERNAL+1)'
      else
         write(ln,10) 'INTEGER NHEL(NEXTERNAL), IC(NEXTERNAL)'
      endif


! write local declarations

      write(ln,5) ' '
      write(ln,5) 'LOCAL VARIABLES '
      write(ln,5) ' '

      write(ln,10) 'INTEGER I,J'
      write(ln,10) 'COMPLEX*16 ZTEMP'
      if (.not.onium) then 
        write(ln,10) 'REAL*8 DENOM(NCOLOR), CF(NCOLOR,NCOLOR)'
      endif
      write(ln,10) 'COMPLEX*16 AMP(NGRAPHS), JAMP(NCOLOR)'

c by RF Jan. 2006
c      write(ln,10) 'COMPLEX*16 W(6,NWAVEFUNCS)'   !,WX1(6),WX2(6)'
      write(ln,10) 'COMPLEX*16 W(18,NWAVEFUNCS)'   !,WX1(6),WX2(6)'
cend RF
      if(Dipolsub) then
       if(offdiag) then
        write(ln,10) 'COMPLEX*16 W1(18),SUBV'
       endif
       if(.not.offdiag) then
        write(ln,10) 'COMPLEX*16 SUBV'
       endif
       write(ln,10) 'REAL*8 SUB'
      endif
c      write(ln,10) 'REAL*8 RMAX,IMAX'


! write global declarations

      write(ln,5) ' '
      write(ln,5) 'GLOBAL VARIABLES'
      write(ln,5) ' '
      if(.not.standalone.and..not.DipolSub) then
      write(92,10) 'Double Precision amp2(maxamps), jamp2(0:maxamps)'
      write(92,10) 'common/to_amps/  amp2,       jamp2'
	  endif
	  
      write(ln,10) 'include "coupl.inc"'
c      write(ln,10) 'include "coupmssm.inc"'

      if (.not.onium)  then
        write(ln,5) ' '
        write(ln,5) 'COLOR DATA'
        write(ln,5) ' '

        do while (.true.)
           read(99,'(a)',end=99) buff
           write(ln,'(a)') buff
        enddo
 99     continue
      endif

      write(ln,5) '----------'
      write(ln,5) 'BEGIN CODE'
      write(ln,5) '----------'
      
      do while (.true.)
         read(91,'(a)',end=999) buff
         write(ln,'(a)') buff
      enddo
 999  continue

      nflows=flows(0,0,0,0)

c      write(*,*) 'Writing flows',nflows
      do i=1,nflows
c
c     First determine a common denominator for these amplitudes
c
         idenom=1
         inum = 999999
         do j=1,ngraphs
            do k=1,graphcolor(0,0,j)         !Number of terms/flows
               if (graphcolor(0,k,j) .eq. i) then
                  inum = min(inum,abs(graphcolor(1,k,j)))
c                  write(*,*) i,j,k,idenom,graphcolor(2,k,j)
                  idenom=abs(idenom*graphcolor(2,k,j)/
     &                 igcf(idenom,graphcolor(2,k,j)))
c                  write(*,*) i,j,k,idenom,graphcolor(2,k,j)
               endif
            enddo
         enddo
         if (idenom .ne. 1) inum=1
         if (inum .eq. 1 .and. idenom .eq. 1) then
            write(ln,'(a,i4,a$)')    '      JAMP(',i,') = '
            ic = 19
         elseif (inum .ne. 1) then
            write(ln,'(a,i4,a,i6,a$)')    '      JAMP(',i,') = ',
     &           inum,'*( '
            ic = 28 
         elseif (idenom .ne. 1) then
            write(ln,'(a,i4,a$)')    '      JAMP(',i,') = ('
            ic = 20 
         endif
         do j=1,ngraphs
            do k=1,graphcolor(0,0,j)         !Number of terms/flows
            if (graphcolor(0,k,j) .eq. i) then                 
               if (ic .gt. 60) then                    
                  write(ln,*)                          
                  write(ln,'(a$)')'     &             '
                  ic = 19
               endif
               den(j)=1d0
               den(j)=dble(graphcolor(1,k,j))/graphcolor(2,k,j)*
     &              idenom*symfact(j)/inum
c               write(*,*) i,j,k,idenom,graphcolor(1,k,j),
c     &              graphcolor(2,k,j),den(j)
c               if (equal(den(j)/den(i),1d0)) then
               if (equal(den(j),1d0)) then
                  write(ln,'(a,i4,a$)') '+AMP(',j,')'
                  ic = ic+10
c               elseif (equal(den(j)/den(i),-1d0)) then
               elseif (equal(den(j),-1d0)) then
                  ic = ic+10
                  write(ln,'(a,i4,a$)') '-AMP(',j,')'
c               elseif (den(j)/den(i) .gt. 0d0) then
               elseif (den(j) .gt. 0d0) then
                  if (ic .gt. 50) then
                     write(ln,*)
                     write(ln,'(a$)')'     & '
                     ic = 8
                  endif
                  ic = ic+22
                  write(ln,'(a,e10.3,a,i4,a$)') '+'
     $                ,den(j),'*AMP(',j,')'
c               elseif (den(j)/den(i) .lt. 0d0) then
               elseif (den(j) .lt. 0d0) then
                  if (ic .gt. 50) then
                     write(ln,*)
                     write(ln,'(a$)')'     & '
                     ic = 8
                  endif
                  ic = ic+22
                  write(ln,'(a,e10.3,a,i4,a$)') '-'
     $                ,abs(den(j)),'*AMP(',j,')'
c     $                 ,den(j)/den(i),'*AMP(',j,')'
               endif
            endif
         enddo
         enddo
         if (idenom .ne. 1) then
            write(ln,'(a,i6$)') ')/',idenom 
         elseif (inum .ne. 1) then
            write(ln,'(a$)') ')'
         endif
         write(ln,*)
      enddo
      if (DipolSub.and..not.offdiag) then
         write(92,42) 'CALL DIPOLESUB(P1,',info_ijk(dipol,1,1),','
     &        ,info_ijk(dipol,1,2),',',info_ijk(dipol,1,3),','
     &        ,info_ijk(dipol,1,4),',',info_ijk(dipol,1,5),','
     &        ,info_ijk(dipol,2,1),',',info_ijk(dipol,2,2),','
     &        ,info_ijk(dipol,2,3),',',info_ijk(dipol,2,4),','
     &        ,info_ijk(dipol,2,5),','
         write(92,41) mass(info_ijk(dipol,1,1)),','
     &        ,mass(info_ijk(dipol,1,2)),',',mass(info_ijk(dipol,1,3))
     &        ,',w(1,1),w(1,1),SUB,SUBV)'
      elseif(DipolSub.and.offdiag) then
         buff='CALL VXXXXX(P(0,??),ZERO,-NHEL(??),??*IC(??),W1)'
         write(buff(17:18),'(i2)') info_ijk(dipol,1,4)
         write(buff(32:33),'(i2)') info_ijk(dipol,1,4)
         if(info_ijk(dipol,1,4).le.nincoming) then
            write(buff(36:37),'(a2)') '-1'
         else
            write(buff(36:37),'(a2)') '+1'
         endif
         write(buff(42:43),'(i2)') info_ijk(dipol,1,4)
         write(ln,10) buff(1:48)
         write(92,42) 'CALL DIPOLESUB(P1,',info_ijk(dipol,1,1),','
     &        ,info_ijk(dipol,1,2),',',info_ijk(dipol,1,3),','
     &        ,info_ijk(dipol,1,4),',',info_ijk(dipol,1,5),','
     &        ,info_ijk(dipol,2,1),',',info_ijk(dipol,2,2),','
     &        ,info_ijk(dipol,2,3),',',info_ijk(dipol,2,4),','
     &        ,info_ijk(dipol,2,5),','
         write(92,43)mass(info_ijk(dipol,1,1)),','
     &        ,mass(info_ijk(dipol,1,2)),',',mass(info_ijk(dipol,1,3))
     &        ,',W1,W(1,',info_ijk(dipol,1,4),'),SUB,SUBV)'
      endif


c      write(ln,10) 'RMAX = 1D-53'
c      write(ln,10) 'IMAX = 1D-53'

      if (.not.onium) then
        buff(1:nlength) = name(1:nlength)
        buff(nlength+1:nlength+8) = ' = 0.D0'
        write(ln,10) buff(1:nlength+8)
        write(ln,10) 'DO I = 1, NCOLOR'
        write(ln,10) '    ZTEMP = (0.D0,0.D0)'
        write(ln,10) '    DO J = 1, NCOLOR'
        write(ln,10) '        ZTEMP = ZTEMP + CF(J,I)*JAMP(J)'
c      write(ln,10) '        RMAX  = MAX(RMAX,ABS( DBLE(ZTEMP)))'
c      write(ln,10) '        IMAX  = MAX(IMAX,ABS(DIMAG(ZTEMP)))'
        write(ln,10) '    ENDDO'
        if(Dipolsub.and.offdiag.eqv..true.) then
c         write(ln,5) '   store the JAMPs'
         write(ln,10) '    STOREJAMP(IHEL,1,I)=ZTEMP'
c         write(ln,5) '   calculate diagonal terms'
        endif


        buff(1:4) = '    '
        buff(5:nlength+5) = name(1:nlength)
        buff(nlength+6:nlength+6) = '='
        buff(nlength+7:2*nlength+7) = name(1:nlength)
        if(Dipolsub) then
           buff(2*nlength+7:2*nlength+41)=
     .        '+ZTEMP*DCONJG(JAMP(I))/DENOM(I)*SUB'
           write(ln,10) buff(1:2*nlength+41)
        else
           buff(2*nlength+7:2*nlength+40)=
     .        '+ZTEMP*DCONJG(JAMP(I))/DENOM(I)'
           write(ln,10) buff(1:2*nlength+40)
        endif
        if(Dipolsub.and.offdiag) then
           write(ln,10)
     .        '    STOREJAMP(IHEL,2,I)=DCONJG(JAMP(I))/DENOM(I)*SUBV'
        endif

c
c     Numeric cancellation stuff commented out here
c
c      buff= '    ZERROR = DCMPLX(ABS(ZTEMP)/RMAX,ABS(ZTEMP)/IMAX)'
c      write(ln,10) buff(1:52)
c      write(ln,10) '    IF (DBLE(ZERROR) .LT. -1D-9) THEN'
c      buff(1:15) = '        PRINT*,'
c      buff(16:16) = char(39)
c      buff(17:47) ='WARNING LARGE CANCELLATIONS IN '
c      buff(48:48+nlength) = name(1:nlength)//char(39)
c      write(ln,10) buff(1:48+nlength)
c      buff(1:15) = '        PRINT*,'
c      buff(16:16) = char(39)
c      buff(17:37) ='Number of digits lost '
c      buff(38:38) = char(39)
c      buff(39:64) = ',INT(-LOG10(DBLE(ZERROR)))'
c      write(ln,10) buff(1:64)
c      write(ln,10) '    ENDIF'
c      write(ln,10) '    IF (DIMAG(ZERROR) .LT. -1D-9) THEN'
c      buff(1:15) = '       PRINT*,'
c      buff(16:16) = char(39)
c      buff(17:47) ='WARNING LARGE CANCELLATIONS IN '
c      buff(48:48+nlength) = name(1:nlength)//char(39)
c      write(ln,10) buff(1:48+nlength)
c      buff(1:14) = '       PRINT*,'
c      buff(15:15) = char(39)
c      buff(16:36) ='Number of digits lost '
c      buff(37:37) = char(39)
c      buff(38:64) = ',INT(-LOG10(DIMAG(ZERROR)))'
c      write(ln,10) buff(1:64)
c      write(ln,10) '    ENDIF'
      

      write(ln,10) 'ENDDO'

      endif ! end if not onium     

      If(.not.standalone.and..not.DipolSub) then
        write(ln,10) 'Do I = 1, NGRAPHS'
        write(ln,10) '    amp2(i)=amp2(i)+amp(i)*dconjg(amp(i))'
        write(ln,10) 'Enddo'
        write(ln,10) 'Do I = 1, NCOLOR'
        write(ln,10) '    Jamp2(i)=Jamp2(i)+Jamp(i)*dconjg(Jamp(i))'
        write(ln,10) 'Enddo'
      endif

      buff(1:47)='CALL GAUGECHECK(JAMP,ZTEMP,EIGEN_VEC,EIGEN_VAL,'
      buff(48:62)='NCOLOR,NEIGEN)'
      write(ln,15) buff(1:62)
      write(ln,10) 'END'
      write(ln,10) ' '
      write(ln,10) ' '
            
	  if(standalone) then
      write(ln,10) 'subroutine switchmom(p1,p,ic,jc,nexternal)'
      write(ln,10) 'implicit none'
      write(ln,10) 'integer nexternal'
      write(ln,10) 'integer jc(nexternal),ic(nexternal)'
      write(ln,10) 'real*8 p1(0:3,nexternal),p(0:3,nexternal)'
      write(ln,10) 'integer i,j'
      write(ln,10) 'do i=1,nexternal'
      write(ln,10) '   do j=0,3'
      write(ln,10) '      p(j,ic(i))=p1(j,i)'
      write(ln,10) '   enddo'
      write(ln,10) 'enddo'
      write(ln,10) 'do i=1,nexternal'
      write(ln,10) '   jc(i)=1'
      write(ln,10) 'enddo'
      write(ln,10) 'jc(ic(1))=-1'
      write(ln,10) 'jc(ic(2))=-1'
      write(ln,10) 'end'
      endif

      close(ln)
c
c   Now write array saying which amplitudes contribute to which JAMPs
c
      if(.not.standalone.and..not.DipolSub)then

      if (nproc .gt. 0) then
         open(unit=ln2,file='coloramps.inc',status='unknown')
      else
         open(unit=ln2,status='scratch')
      endif

      write(ln2,'(a,i4,a,i4,a)')'      LOGICAL ICOLAMP(',NGRAPHS,
     $   ',',NCOLOR,')'
      write(ln2,'(a$)')      '      DATA ICOLAMP/'
      ic = 18
      do i=1,nflows
        do j=1,ngraphs
          if (ic .gt. 60) then                    
            write(ln2,*)                          
            write(ln2,'(a$)')'     &             '
            ic = 18
          endif
          injamp=.false.
          do k=1,graphcolor(0,0,j) !Number of terms/flows
            if (graphcolor(0,k,j) .eq. i) then                 
              injamp=.true.
            endif
          enddo
          if(injamp)then
            write(ln2,'(a$)') '.true.'
            ic = ic+7
          else
            write(ln2,'(a$)') '.false.'
            ic = ic+8
          endif
          if(i.lt.nflows.or.j.lt.ngraphs)then
            write(ln2,'(a$)') ','
          else
            write(ln2,'(a$)') '/'
          endif
        enddo
      enddo
      close(ln2)
      
      endif ! .not.standalone and .not.DipolSub
c
c     now write  files for MadOnia
c

      if(onium) then
        open(unit=93,file='color_data.inc',status='unknown')

        buff = 'INTEGER    NCOLOR'
        write(93,10) buff(1:34)
        buff = 'PARAMETER ( NCOLOR=????) '
        write(buff(20:23),'(i4)') NCOLOR
        write(93,10) buff(1:34)
        write(93,10) 'INTEGER I'
C
        write(93,10) 'REAL*8 DENOM(NCOLOR), CF(NCOLOR,NCOLOR)'
        write(93,5) ' '
        write(93,5) 'COLOR MATRIX'
        write(93,5) ' '

        do while (.true.)
          read(99,'(a)',end=90) buff
          write(93,'(a)') buff
        enddo
90      continue
        close(93)
c
        open(unit=94 ,file='oniumtype.mg',status='unknown')
        buff = onium_type(1:4)
        write(94,*) buff
        close(94)

      endif


!>>>>>>> 1.14.2.5

 5    format('C ',A)
 10   format(6X,A)
 15   format('C',6X,A)
 20   format(', P',I1)
 30   format(',P',I1,'(0:3)')
 41   format(5X,'&',4X,A8,A,A8,A,A8,A)
 42   format(6X,A18,12(I2,A))
 43   format(5X,'&',4X,A8,A,A8,A,A8,A,I2,A)
      end

 
      subroutine writesum(nproc,ngraphs,ncolor)
!***************************************************************************
!     writes out the subroutine which sums over helicities and
!     and does all crossings
!***************************************************************************
      implicit none

! Constants

      include 'params.inc'
      include 'onia.inc'

! Arguments

      integer nproc, ngraphs, ncolor

! Local Variables

      character*75 buff,fname
      character*130 buff1
      character*20 name
      character*4 str1
      integer i,jchar,ishift,j,flen,nexternal,ichar,nlen,k,l
      integer isym,jhel,initc,iden(maxcross)
      integer nhel(maxlines,3**maxlines),step(maxlines),ihel
      integer ncross,ipart(maxlines,maxcross),ic(maxlines,maxcross)
      integer jline(0:4),imatch(0:maxcross)
      integer icross,iboson
      logical found
      integer nwp,nwm,nz,ngot
      integer num_external


! Global Variables

      integer        iline(-maxlines:maxlines),idir(-maxlines:maxlines)
      integer        this_coup(max_coup) ,goal_coup(max_coup)
      common/to_proc/iline,idir,this_coup,goal_coup

      character*25 gname
      integer iname
      common/to_name/iname,gname
      integer         nincoming,nincfirst
      common/to_proc2/nincoming,nincfirst
      character*60 proc
      integer iproc
      common/to_write/iproc,proc

      integer info_p(5,max_particles),iposx(3,max_particles)
      character*(max_string) iwave(max_particles),owave(max_particles)
      character*(8) str(3,max_particles)
      common/to_external/iwave,owave,iposx,info_p,str

      character*(4*max_particles) particle(4)
      integer                               charge_c(max_particles)
      integer iparticle(0:max_particles,0:4),inverse(max_particles)
      common/to_model/iparticle,  particle,  inverse, charge_c

      logical         lwp,lwm,lz,decay,cross
      common/to_decay/lwp,lwm,lz,decay,cross

      character*70       cversion
      common/to_version/ cversion

      logical              StandAlone
      common/to_StandAlone/StandAlone


      character*4 onium_type
      common /to_onium_type/onium_type

      data StandAlone/.false./

      logical DipolSub
      data DipolSub/.false./

      integer info_ijk(maxdipoles,2,5),nr,dipol
      logical offdiag
      common /ijk/ info_ijk,nr,offdiag
	  common /dipolenumber/ dipol

      integer ncomb
      common /ihel/ ncomb

      integer isymn1
      common /isym/ isymn1

      character*(8) mass(max_particles)
      common /masses/ mass

      character*(max_string) dipprocess(maxdipoles)
      common /process/ dipprocess


!-----------
! Begin Code
!-----------
c
c     These lines for artificial decay
c
      if (decay) then
         call getparticle('w+',nwp,ngot)
         if (ngot .ne. 1) print*,'Error finding W+',nwp,ngot
         call getparticle('w-',nwm,ngot)
         if (ngot .ne. 1) print*,'Error finding W+',nwp,ngot
         call getparticle('z',nz,ngot)
         if (ngot .ne. 1) print*,'Error finding W+',nwp,ngot
         lwp=.false.
         lwm=.false.
         lz =.false.
         if (iline(iline(0)) .eq. nwp) lwp=.true.
         if (iline(iline(0)) .eq. nwm) lwm=.true.
         if (iline(iline(0)) .eq. nz ) lz =.true.
         if (lz) then
            call getparticle('e+e-',jline(1),jline(0))
         elseif(lwp) then
            call getparticle('e+ve',jline(1),jline(0))
         elseif(lwm) then
            call getparticle('ve~e-',jline(1),jline(0))
         else
            print*,'Warning last particle not Z or W+ or W-'
            print*,'Not decaying'
            decay=.false.
         endif
      endif
      if (decay) then
         iboson=iline(iline(0))
         iline(0)=iline(0)+1
         iline(iline(0)-1)=jline(1)
         iline(iline(0))=jline(2)
c         print*,'We have decay',decay,lz
      else
c         print*,'No decay'
      endif

      if (cross .and. .false.) then
         call crossing(ncross,ipart,imatch)
      else
         ncross=1
         imatch(0) =1
         imatch(1) =1
         do i=1,iline(0)
            ipart(i,1)=i
         enddo
      endif
c
c set up all the different crossings which are not identical
c
      icross = 0
      do i=1,ncross
         if (imatch(i) .gt. icross) then
            call getdenom(isym,initc,jhel,step,ipart(1,i))
            iden(imatch(i)) = isym*initc*jhel
            if(DipolSub) iden(imatch(i)) = isymn1*initc*jhel
c           write(*,*) 'iden',i,imatch(i),iden(imatch(i))
            if (imatch(i) .ne. icross+1) then
               print*,'Error in imatch writesub.f',i,imatch(i),icross+1
            endif
            icross=imatch(i)
         endif
      enddo
      if (icross .ne. imatch(0)) then
         print*,'Warning icross .ne. imatch(0)',icross,imatch(0)
      endif
c      write(*,*) 'iden(1)',iden(1)

      ishift = iname
c      nlength = ishift

      name = gname(1:iname)
      call upper_case(name)
      nexternal = iline(0)
      call sethel(nhel,step,nexternal,ihel)

c      buff = 'REAL*8 FUNCTION S'
c      jchar = 18

        buff = 'SUBROUTINE S'
        jchar = 13
        buff(jchar:jchar+ishift) = name(1:iname)
        jchar = jchar+ishift
        ishift= 7
        buff(jchar:jchar+ishift) = '(P1,ANS)'
        jchar =jchar+ishift
        write(92,10) buff(1:jchar)

      call genparton(ncross,ipart,buff(12:jchar),nproc,imatch)

      jchar=jchar-4
      FNAME= buff(13:jchar)
c      FNAME(JCHAR-12:JCHAR-12+13)=',NHEL(1,IHEL))'
      FNAME(JCHAR-12:JCHAR-12+19)=',NHEL(1,IHEL),JC(1))'
      FLEN = jchar+19

! write comments

      write(92,5) ' '
      write(92,5) cversion
	  if(standalone) write (92,5) "MadGraph StandAlone Version"
	  if(Onium) write (92,5) "MadOnia Version"
      write(92,5) 'RETURNS AMPLITUDE SQUARED SUMMED/AVG OVER COLORS'
      write(92,5) 'AND HELICITIES'
      write(92,5) 'FOR THE POINT IN PHASE SPACE P(0:3,NEXTERNAL)'
      write(92,5) ' '
      if (Onium) then
        write(92,5) 'FOR PROCESS '//proc(1:iproc)
     &  //onium_type(1:3)//'['//onium_type(4:4)//']'
      else
        write(92,5) 'FOR PROCESS '//proc(1:iproc)
      endif
      write(92,5) ' '
      do i=1,ncross
         buff = 'Crossing ??? is '
         write(buff(10:12),'(i3)') imatch(i)
         ichar = 17
         if(DipolSub) then
          buff= 'BORN AMPLITUDE FOR THIS DIPOLE IS '
          ichar = 35
         endif
         do j=1,2
            call part_string(inverse(iline(ipart(j,i))),str1,nlen)
            buff(ichar:ichar+nlen-1) = str1(1:nlen)
            ichar=ichar+nlen
            buff(ichar:ichar)=' '
            ichar=ichar+1
         enddo
         buff(ichar:ichar+3) = '-> '
         ichar=ichar+3
         do j=3,iline(0)
            call part_string(iline(ipart(j,i)),str1,nlen)
            buff(ichar:ichar+nlen-1) = str1(1:nlen)
            ichar=ichar+nlen
            buff(ichar:ichar)=' '
            ichar=ichar+1
         enddo
         write(92,5) buff(1:ichar)
         dipprocess(dipol)=buff(35:ichar)
      enddo

      write(92,10) 'IMPLICIT NONE'
      if (Onium) write(92,10) 'INCLUDE "coupl.inc"'

! write parameters

      if(DipolSub.and.info_ijk(dipol,2,1).eq.info_ijk(dipol,2,2)) then
       offdiag=.true.
      else
       offdiag=.false.
      endif
      write(92,5) ' '
      write(92,5) 'CONSTANTS'
      write(92,5) ' '
      if(.not.standalone.and..not.DipolSub)
     .        write(92,10) 'Include "genps.inc"'
      if(standalone.or.DipolSub)
     .        write(92,10) 'include "nexternal.inc"'
      if(DipolSub) write(92,10) 'include "dipole.inc"'
      if(DipolSub) write(92,10) 'include "coupl.inc"'
      buff = 'INTEGER                 NCOMB,     NCROSS'
      write(92,10) buff(1:50)
      buff = 'PARAMETER (             NCOMB=????, NCROSS=???)'
c      write(buff(22:22),'(i1)') nexternal

      if (onium) then
          if(em_decay) then 
            write(buff(31:34),'(i4)') ihel
          else
            write(buff(31:34),'(i4)') (2*J_qn+1)*ihel/4
          endif
      else
        write(buff(31:34),'(i4)') ihel
      endif
      ncomb=ihel
c
c      write(buff(43:45),'(i3)') ncross
      write(buff(44:46),'(i3)') icross
      write(92,10) buff(1:47)
      write(92,10) 'INTEGER    THEL'
      write(92,10) 'PARAMETER (THEL=NCOMB*NCROSS)'
     
      If(onium) then
        write(92,10) 'INTEGER NC'
        write(92,10) 'PARAMETER (NC=3)'
      endif

      write(92,5) ' '
      write(92,5) 'ARGUMENTS '
      write(92,5) ' '
      write(92,10) 'REAL*8 P1(0:3,NEXTERNAL),ANS(NCROSS)'

      write(92,5) ' '
      write(92,5) 'LOCAL VARIABLES '
      write(92,5) ' '


      if(onium) then
        if (S_qn.eq.1.and.L_qn.eq.0) then
          if (em_decay) then
            write(92,10) 'INTEGER NHEL(NEXTERNAL+1,NCOMB),NTRY' 
            write(92,10) 'REAL*8 PROJ_PSILEPT'
          else
            write(92,10) 'INTEGER NHEL(NEXTERNAL,NCOMB),NTRY'
            write(92,10) 'REAL*8 PROJ_PSI '
          endif
        else
          write(92,10) 'INTEGER NHEL(NEXTERNAL,NCOMB),NTRY'
          if(S_qn.eq.0.and.L_qn.eq.0) write(92,10) 'REAL*8 PROJ_ETA '
          if(S_qn.eq.1.and.L_qn.eq.1) write(92,10) 'REAL*8 PROJ_CHI '
          if(S_qn.eq.0.and.L_qn.eq.1) write(92,10) 'REAL*8 PROJ_H'
        endif
        write(92,10) 'REAL*8 T, P(0:3,NEXTERNAL)'
      else
        write(92,10) 'INTEGER NHEL(NEXTERNAL,NCOMB),NTRY'
        write(92,10) 'REAL*8 T, P(0:3,NEXTERNAL)'
        if (DipolSub.and. (offdiag.eqv..false.))
     .       write (92,10) 'REAL*8 P2(0:3,NEXTERNAL)' !,SUB,SUBV'

        buff = 'REAL*8 '
        buff(8:7+iname)=name(1:iname)
        write(92,10) buff(1:7+iname)
        if(DipolSub) then
         write(92,10) 'REAL*8 ZERO'
         write(92,10) 'PARAMETER(ZERO=0d0)'
        endif

      endif

      write(92,10) 'INTEGER IHEL,IDEN(NCROSS),IC(NEXTERNAL,NCROSS)'
      if(DipolSub.and.offdiag) then
       write(92,10) 'INTEGER IPROC,JC(NEXTERNAL), I,L,K'
      else
       write(92,10) 'INTEGER IPROC,JC(NEXTERNAL), I'
      endif
      write(92,10) 'LOGICAL GOODHEL(NCOMB,NCROSS)'

      if(DipolSub.and.(offdiag.eqv..true.)) then
       buff='COMPLEX*16 STOREJAMP(NCOMB,2,???)'
       write(buff(30:32),'(i3)') ncolor
       write(92,10) buff(1:33)
      endif
      if(standalone.or.DipolSub)  write(92,10) 'DATA NTRY/0/'

      if(onium) then
        write(92,10) 'DOUBLE PRECISION CONV'
        if (S_qn.eq.1.and.L_qn.eq.0.and..not.em_decay) then
          write(92,10) 'DOUBLE PRECISION ANS_T,ANS_L'
          write(92,10) 'COMMON /TO_POLARIZ/ANS_T,ANS_L'
        endif
      endif


	  	  	     
      if(.not.standalone.and..not.DipolSub) then
      	  
      write(92,10) 'INTEGER NGRAPHS'
      write(92,10) 'REAL*8 hwgt, xtot, xtry, xrej, xr, yfrac(0:ncomb)'
      write(92,10) 'INTEGER idum, ngood, igood(ncomb), jhel, j, jj'
      write(92,10) 'LOGICAL warned'
      write(92,10) 'REAL     xran1'
      write(92,10) 'EXTERNAL xran1'
      write(92,5) ' '
      write(92,5) 'GLOBAL VARIABLES'
      write(92,5) ' '
      if(onium) then
        write(92,10) 'DOUBLE PRECISION QMASS(2)'
        write(92,10) 'COMMON/to_qmass/qmass'
      endif
      write(92,10) 'Double Precision amp2(maxamps), jamp2(0:maxamps)'
      write(92,10) 'common/to_amps/  amp2,       jamp2'
      write(92,10)
      write(92,10) 'character*79         hel_buff'
      write(92,10) 'common/to_helicity/  hel_buff'
      write(92,10)
      write(92,10) 'REAL*8 POL(2)'
      write(92,10) 'common/to_polarization/ POL'
      write(92,10)
      write(92,10) 'integer          isum_hel'
      write(92,10) 'logical                    multi_channel'
      write(92,10) 'common/to_matrix/isum_hel, multi_channel'
      write(92,10) 'INTEGER MAPCONFIG(0:LMAXCONFIGS), ICONFIG'
      WRITE(92,10) 'common/to_mconfigs/mapconfig, iconfig'
      write(92,10) 'DATA NTRY,IDUM /0,-1/'
      write(92,10) 'DATA xtry, xrej, ngood /0,0,0/'
      write(92,10) 'DATA warned, isum_hel/.false.,0/'
      write(92,10) 'DATA multi_channel/.true./'
	  write(92,10) 'SAVE yfrac, igood, jhel'

      buff = 'DATA NGRAPHS /'
      write(buff(16:24),'(i4,a)') ngraphs,'/'
      write(92,10) buff(1:30)

      buff = 'DATA jamp2(0) /'
      write(buff(16:24),'(i4,a)') ncolor,'/'
      write(92,10) buff(1:30)

      endif
	  
      write(92,10) 'DATA GOODHEL/THEL*.FALSE./'

      if (onium) then

       call get_helset_onium(nexternal,ihel,nhel)

      else ! not onium

      BUFF = 'DATA (NHEL(IHEL,????),IHEL=1,??) /'
      write(buff(30:31),'(i2)') nexternal
      do i=1,ihel
         write(buff(17:20),'(i4)') i
         jchar=35
         ishift = 3
         do j=1,nexternal
            write(buff(jchar:jchar+ishift),'(I2,a)') nhel(j,i),','
            jchar=jchar+ishift
         enddo
         jchar=jchar-1
         write(buff(jchar:jchar),'(a1)') '/'
         write(92,10) buff(1:jchar)
      enddo

      endif

      
      if(onium) then
       num_external=nexternal-1
      else
       num_external=nexternal
      endif


      BUFF = 'DATA (  IC(IHEL,???),IHEL=1,??) /'
        write(buff(29:30),'(i2)') num_external
      icross=0
      do i=1,ncross
         if (imatch(i) .gt. icross) then
            write(buff(17:19),'(i3)') imatch(i)
            jchar=34
            ishift = 3
            do j=1,num_external
c            write(buff(jchar:jchar+ishift),'(I3,a)') ic(j,i),','
            write(buff(jchar:jchar+ishift),'(I2,a)') ipart(j,i),','
            jchar=jchar+ishift
            enddo
            jchar=jchar-1
            write(buff(jchar:jchar),'(a1)') '/'
            write(92,10) buff(1:jchar)
            icross=icross+1
         endif
      enddo
      BUFF = 'DATA (IDEN(IHEL),IHEL=???,???) /'
      do i=1,icross-4,4
c      do i=1,ncross-4,4
         write(buff(23:25),'(i3)') i
         write(buff(27:29),'(i3)') i+3
         jchar=33
         ishift = 5
         do j=0,3
            write(buff(jchar:jchar+ishift),'(I4,a)') iden(j+i),','
            jchar=jchar+ishift
         enddo
         jchar=jchar-1
         write(buff(jchar:jchar),'(a1)') '/'
         write(92,10) buff(1:jchar)
      enddo
      write(buff(23:25),'(i3)') i
      write(buff(27:29),'(i3)') icross
c      write(buff(27:29),'(i3)') ncross
      jchar=33
      ishift = 5
      do j=0,icross-i
c      do j=0,ncross-i
         write(buff(jchar:jchar+ishift),'(I4,a)') iden(j+i),','
         jchar=jchar+ishift
      enddo
      jchar=jchar-1
      write(buff(jchar:jchar),'(a1)') '/'
c      if (i .le. ncross) then
      if (i .le. icross) then
         write(92,10) buff(1:jchar)
      endif
      
      write(92,5) '----------'
      write(92,5) 'BEGIN CODE'
      write(92,5) '----------'
c      buff(1:1)='S'
c      buff(2:iname+1) = name(1:iname)
c      buff(iname+2:iname+7) = ' = 0d0'
c      write(92,10) buff(1:iname+7)
      write(92,10) 'NTRY=NTRY+1'
      write(92,10) 'DO IPROC=1,NCROSS'
      if (.not.DipolSub) then
         write(92,10) 'CALL SWITCHMOM(P1,P,IC(1,IPROC),JC,NEXTERNAL)'
      else
         write(92,5) 'CALL SWITCHMOM(P1,P2,IC(1,IPROC),JC,NEXTERNAL)'
      endif
      if(DipolSub) then
         write(92,40) ' CALL TRANSFORM(P1,P,',info_ijk(dipol,1,1),','
     &   ,info_ijk(dipol,1,2),',',info_ijk(dipol,1,3),','
     &   ,info_ijk(dipol,1,4),',',info_ijk(dipol,1,5),
     &    ',',mass(info_ijk(dipol,1,1)),',',mass(info_ijk(dipol,1,2)),
     &    ',',mass(info_ijk(dipol,1,3)),')'
      endif
c
c     Carefull here, if there are no crossings then we don't want
c     to flip the signs of the incoming particles because they already
c     are flipped
c
      if (nincoming .gt. 0) then
         write(92,10) 'DO IHEL=1,NEXTERNAL'
         write(92,10) '   JC(IHEL) = +1'
         write(92,10) 'ENDDO'
      endif


      if (.not. standalone.and..not.DipolSub) then
         write(92,10) ' '
         write(92,10) 'IF (multi_channel) THEN'
         write(92,10) '    DO IHEL=1,NGRAPHS'
         write(92,10) '        amp2(ihel)=0d0'
         write(92,10) '        jamp2(ihel)=0d0'
         write(92,10) '    ENDDO'
         write(92,10) '    DO IHEL=1,int(jamp2(0))'
         write(92,10) '        jamp2(ihel)=0d0'
         write(92,10) '    ENDDO'
         write(92,10) 'ENDIF'
      endif

      WRITE(92,10) 'ANS(IPROC) = 0D0'

      if(onium) then    
        if (S_qn.eq.1.and.L_qn.eq.0.and..not.em_decay) then
          WRITE(92,10) 'ANS_T = 0D0'
          WRITE(92,10)  'ANS_L = 0D0'
        endif
      endif




      if(.not.standalone.and..not.DipolSub) then
       write(92,10) 'write(hel_buff,''(16i5)'') (0,i=1,nexternal)'
	   write(92,10) 'IF (ISUM_HEL .EQ. 0 .OR. NTRY .LT. 10) THEN'
      endif 

      
      write(92,11) 'DO IHEL=1,NCOMB'
      write(92,11) '   IF (GOODHEL(IHEL,IPROC) .OR. NTRY .LT. 2) THEN'

      If(onium) then
       if(S_qn.eq.0.and.L_qn.eq.0) then
         write(92,11) '      T=PROJ_ETA(P1 ,NHEL(1,IHEL),JC(1))'
       elseif (S_qn.eq.1.and.L_qn.eq.1) then
         if(J_qn.eq.0) then
           write(92,11) '      T=PROJ_CHI(P1 ,NHEL(1,IHEL),JC(1),0) '
         elseif(J_qn.eq.1) then
           write(92,11) '      T=PROJ_CHI(P1 ,NHEL(1,IHEL),JC(1),1) '
         elseif(J_qn.eq.2) then
           write(92,11) '      T=PROJ_CHI(P1 ,NHEL(1,IHEL),JC(1),2) '
         endif
       elseif (S_qn.eq.0.and.L_qn.eq.1) then
         write(92,11) '      T=PROJ_H(P1 ,NHEL(1,IHEL),JC(1))     '
       elseif(S_qn.eq.1.and.L_qn.eq.0.and..not.em_decay) then
         write(92,11) '      T=PROJ_PSI(P1 ,NHEL(1,IHEL),JC(1))'
         write(92,11) '      IF (NHEL(NEXTERNAL,IHEL).NE.0) then'
         write(92,11) '      ANS_T=ANS_T+T'
         write(92,11) '      ELSE'
         write(92,11) '      ANS_L=ANS_L+T'
         write(92,11) '      ENDIF'
       elseif(S_qn.eq.1.and.L_qn.eq.0.and.em_decay) then
         write(92,11) '      T=PROJ_PSILEPT(P1,NHEL(1,IHEL),JC(1))'
       endif
      else
        buff(1:8) = '        '
        buff(8:9)= 'T='
        i=33
        buff(10:10+flen-i) = fname(1:flen-i)
        buff(10+flen-i+1:10+flen-1) = fname(flen-i+2:flen)
        if(DipolSub) then
           if(offdiag.eqv..false.) then
              buff(flen-3:10+flen+2)=',P1)'
              write(92,11) buff(1:10+flen+2)
           endif
           if(offdiag) then
              buff(flen-3:10+flen+18)=',P1,IHEL,STOREJAMP)'
              write(92,11) buff(1:10+flen+18)
           endif
        else
           write(92,11),buff(1:10+flen-1)
        endif

      endif

      if(.not.standalone.and..not.DipolSub) then
      write(92,13) ' DO JJ=1,nincoming'
      write(92,13) '   IF(POL(JJ).NE.1d0.AND.'
      write(92,14) '      NHEL(JJ,IHEL).EQ.INT(SIGN(1d0,POL(JJ)))) THEN'
      write(92,13) '     T=T*ABS(POL(JJ))'
      write(92,13) '   ELSE IF(POL(JJ).NE.1d0)THEN'
      write(92,13) '     T=T*(2d0-ABS(POL(JJ)))'
      write(92,13) '   ENDIF'
      write(92,13) ' ENDDO'
      endif
      write(92,11) '     ANS(IPROC)=ANS(IPROC)+T'
      buff = '     IF (T .NE. 0D0 .AND. .NOT. '
      buff(36:) ='GOODHEL(IHEL,IPROC)) THEN'
      write(92,11) buff(1:60)
      write(92,11)  '         GOODHEL(IHEL,IPROC)=.TRUE.'
       if(.not.standalone.and..not.DipolSub) 
     . WRITE(92,11) '         NGOOD = NGOOD +1'
       if(.not.standalone.and..not.DipolSub) 
     . WRITE(92,11) '         IGOOD(NGOOD) = IHEL'
      write(92,11) '     ENDIF'
      write(92,11) '   ENDIF'
      write(92,11) 'ENDDO'
c
c     Remove for StandAlone
c
      if (.not. StandAlone.and..not.DipolSub) then 

c      WRITE(92,11) 'XR = 1D0/DBLE(NGOOD)'
c      WRITE(92,11) 'DO IHEL=0,NGOOD'
c      WRITE(92,11) '    YFRAC(IHEL)=XR*IHEL'
c      WRITE(92,11) 'ENDDO'
      WRITE(92,11) 'JHEL = 1'
      WRITE(92,11) 'ISUM_HEL=MIN(ISUM_HEL,NGOOD)'

      WRITE(92,10) 'ELSE              !RANDOM HELICITY'
c      WRITE(92,11) 'XR = XRAN1(IDUM)'
c      WRITE(92,11)'DO WHILE (IHEL .LT. NGOOD .AND. XR .GT. YFRAC(IHEL))'
c      WRITE(92,11) '    IHEL=IHEL+1'
c      WRITE(92,11) 'ENDDO'
      WRITE(92,11) 'DO J=1,ISUM_HEL'
      WRITE(92,11) '    JHEL=JHEL+1'
      WRITE(92,11) '    IF (JHEL .GT. NGOOD) JHEL=1'
      WRITE(92,11) '    HWGT = REAL(NGOOD)/REAL(ISUM_HEL)'
      write(92,11) '    IHEL = IGOOD(JHEL)'

      If(onium) then
       if(S_qn.eq.0.and.L_qn.eq.0) then
         write(92,11) '      T=PROJ_ETA(P1 ,NHEL(1,IHEL),JC(1))'
       elseif (S_qn.eq.1.and.L_qn.eq.1) then
         if(J_qn.eq.0) then
           write(92,11) '      T=PROJ_CHI(P1 ,NHEL(1,IHEL),JC(1),0) '
         elseif(J_qn.eq.1) then
           write(92,11) '      T=PROJ_CHI(P1 ,NHEL(1,IHEL),JC(1),1) '
         elseif(J_qn.eq.2) then
           write(92,11) '      T=PROJ_CHI(P1 ,NHEL(1,IHEL),JC(1),2) '
         endif
       elseif (S_qn.eq.0.and.L_qn.eq.1) then
         write(92,11) '      T=PROJ_H(P1 ,NHEL(1,IHEL),JC(1))     '
       elseif(S_qn.eq.1.and.L_qn.eq.0.and..not.em_decay) then
         write(92,11) '      T=PROJ_PSI(P1 ,NHEL(1,IHEL),JC(1))'
         write(92,11) '      IF (NHEL(NEXTERNAL,IHEL).NE.0) then'
         write(92,11) '      ANS_T=ANS_T+T'
         write(92,11) '      ELSE'
         write(92,11) '      ANS_L=ANS_L+T'
         write(92,11) '      ENDIF'
       elseif(S_qn.eq.1.and.L_qn.eq.0.and.em_decay) then
         write(92,11) '      T=PROJ_PSILEPT(P1,NHEL(1,IHEL),JC(1))'
       endif
      else
        buff(1:8) = '        '
        buff(8:9)= 'T='
        i=33
        buff(10:10+flen-i) = fname(1:flen-i)  
        buff(10+flen-i+1:10+flen-1) = fname(flen-i+2:flen)
        write(92,10) ' '//buff(1:10+flen-1)
      endif


      if(.not.standalone.and..not.DipolSub) then
      write(92,13) 'DO JJ=1,nincoming'
      write(92,13) '  IF(POL(JJ).NE.1d0.AND.'
      write(92,14) '     NHEL(JJ,IHEL).EQ.INT(SIGN(1d0,POL(JJ)))) THEN'
      write(92,13) '    T=T*ABS(POL(JJ))'
      write(92,13) '  ELSE IF(POL(JJ).NE.1d0)THEN'
      write(92,13) '    T=T*(2d0-ABS(POL(JJ)))'
      write(92,13) '  ENDIF'
      write(92,13) 'ENDDO'
      endif
      write(92,11) '    ANS(IPROC)=ANS(IPROC)+T*HWGT'
      WRITE(92,11) 'ENDDO'
c      if (.not. StandAlone) then
      write(92,11) 'IF (ISUM_HEL .EQ. 1) THEN'
      write(92,11) '    WRITE(HEL_BUFF,''(16i5)'')' //
     &                  '(NHEL(i,IHEL),i=1,nexternal)'
      write(92,11) 'ENDIF'
c      endif
      WRITE(92,10) 'ENDIF'

      WRITE(92,10) 'IF (MULTI_CHANNEL) THEN'
      WRITE(92,11) 'XTOT=0D0'
      WRITE(92,11) 'DO IHEL=1,MAPCONFIG(0)'
      WRITE(92,13) 'XTOT=XTOT+AMP2(MAPCONFIG(IHEL))'
      WRITE(92,11) 'ENDDO'
      WRITE(92,11) 'IF (XTOT.NE.0D0) THEN'
      WRITE(92,13) 'ANS(IPROC)=ANS(IPROC)*AMP2(MAPCONFIG(ICONFIG))/XTOT'
      WRITE(92,11) 'ELSE'
      WRITE(92,13) 'ANS(IPROC)=0D0'
      WRITE(92,11) 'ENDIF'
      WRITE(92,10) 'ENDIF'

      endif

c      write(92,10) 'DO IHEL=1,NCOMB'
c      write(92,10) '    IF (GOODHEL(IHEL,IPROC) .OR. NTRY .LT. 2) THEN'
c      buff(1:8) = '        '
c      buff(8:9)= 'T='
c      i=33
c      buff(10:10+flen-i) = fname(1:flen-i)  
c      buff(10+flen-i+1:10+flen-1) = fname(flen-i+2:flen)
c      write(92,10) buff(1:10+flen-1)
c      print*,buff(1:10+flen-1)
c      write(92,10) '       ANS(IPROC)=ANS(IPROC)+T'
c      buff = '        IF (T .GT. 0D0 .AND. .NOT. '
c      buff(36:) ='GOODHEL(IHEL,IPROC)) THEN'
c      write(92,10) buff(1:60)
c      write(92,10) '            GOODHEL(IHEL,IPROC)=.TRUE.'
c      write(92,5) '            WRITE(*,*) IHEL,T'
c      write(92,10) '        ENDIF'
c      write(92,10) '    ENDIF'
c      write(92,10) 'ENDDO'


      buff(1:1) = 'S'
c      if (DipolSub.and.offdiag.eqv..false.) then
c         write(92,10) 'ANS(IPROC)=ANS(IPROC)*SUB'
c      endif
      if (DipolSub .and. offdiag.eqv..true.) then
       write(92,10) 'DO I=1,NCOMB'
       write(92,10) ' DO L=1,NCOMB'
       write(92,10)
     & ' IF(I.NE.L.AND.GOODHEL(I,IPROC).AND.GOODHEL(L,IPROC)) THEN'
       write(92,5)
     & '   check that the right helicity combinations are combined'
       buff1(1:5)='  IF('
       j=6
        DO i=1,nexternal
         if(i.ne.info_ijk(dipol,1,4)) then
          if(i.lt.nexternal) then
           buff='NHEL(??,I).EQ.NHEL(??,L).AND.'
           write(buff(6:7),'(i2)') i
           write(buff(20:21),'(i2)') i
           buff1(j:j+29)=buff
           j=j+30
          endif
          if(i.eq.nexternal) then
           buff='NHEL(??,I).EQ.NHEL(??,L)) THEN'
           write(buff(6:7),'(i2)') i
           write(buff(20:21),'(i2)') i
           buff1(j:j+31)=buff
           j=j+31
          endif
         endif
         if(i.eq.info_ijk(dipol,1,4)) then
          if(i.lt.nexternal) then
           buff='NHEL(??,I).EQ.-NHEL(??,L).AND.'
           write(buff(6:7),'(i2)') i
           write(buff(21:22),'(i2)') i
           buff1(j:j+31)=buff
           j=j+31
          endif
          if(i.eq.nexternal) then
           buff='NHEL(??,I).EQ.-NHEL(??,L)) THEN'
           write(buff(6:7),'(i2)') i
           write(buff(21:22),'(i2)') i
           buff1(j:j+32)=buff
           j=j+32
          endif
         endif
c         do l=1,j
c           if(l.gt.50 .and.k.eq.0 .and. buff1(l:l+5).eq.'.AND.') then
c            write(92,10) buff1(1:l+5)
c            k=k+1
c           endif
c         enddo
        ENDDO
      k=0
      l=1
      do i=1,j
       if(i.gt.50 .and.k.eq.0 .and. buff1(i:i+5).eq.'.AND.') then
        write(92,10) buff1(1:i+5)
        k=k+1
        l=i+5
       endif
       if(i.gt.100.and.k.eq.1.and. buff1(i:i+5).eq.'.AND.') then
        write(92,16) buff1(l:i+5)
        k=k+1
        l=i+5
       endif
       if(i.gt.150.and.k.eq.2.and. buff1(i:i+5).eq.'.AND.') then
        write(92,16) buff1(l:i+5)
        k=k+1
        l=i+5
       endif
      enddo
        write(92,16) buff1(l:j)
      write(92,10) '     T=0d0'
      buff='     DO K=1,???'
      write(buff(13:15),'(i3)') ncolor
      write(92,10) buff(1:15)
      write(92,5) '    combine the amplitudes of the different NHELs'
      write(92,10)'      T=T+DREAL(STOREJAMP(I,1,K)*STOREJAMP(L,2,K))'
      write(92,10)'     ENDDO'
      write(92,10)'    ANS(IPROC)=ANS(IPROC)+T'
      write(92,10)'  ENDIF'
      write(92,10)' ENDIF'
      write(92,10)' ENDDO'
      write(92,10)'ENDDO'
      endif

      if(onium) then
       call Onium_conv
        write(92,10) 'ANS(IPROC)=ANS(IPROC)/DBLE(IDEN(IPROC))*conv'
      else
        write(92,10) 'ANS(IPROC)=ANS(IPROC)/DBLE(IDEN(IPROC))'
      endif

      write(92,10) 'ENDDO'
      write(92,10) 'END'
      write(92,10) ' '
      write(92,10) ' '

c
c     Now if a decay was made, replace decayed particle in iline
c
      if (decay) then
         iline(0)=iline(0)-1
         iline(iline(0)) = iboson
      endif
 5    format('C ',A)
 10   format(6X,A)
 11   FORMAT(10X,A)
 12   FORMAT(5X,'&',4X,A)
 13   FORMAT(14X,A)
 14   FORMAT(5X,'&',8X,A)
 15   format('C',6X,A)
 16   FORMAT(5X,'&',5X,A)
 20   format(', P',I1)
 30   format(',P',I1,'(0:3)')
 40   format(6X,A,5(I2,A),A8,A,A8,A,A8,A)
 41   format(6X,A18,10(I2,A))
      end


      subroutine writesub_f90(ngraphs,nexternal,nqed,nqcd)
!***************************************************************************
!     writes out the subroutine for Fortran 90 
!***************************************************************************
      implicit none

! Constants


! Arguments

      integer ngraphs,nqed,nqcd,nexternal

! Local Variables


!-----------
! Begin Code
!-----------
      end


      subroutine writesum_f90
!***************************************************************************
!     writes out the subroutine - f90 version
!***************************************************************************
      implicit none

      end

      
      subroutine sethel(nhel,step,npart,ihel)
c************************************************************************
c     Sets up all possible helicity combinations in nhel(ipart,ihel)
c     given the number of particles, and the step size to be used
c     for each particle
c     The first state is always -1, the next is 0 or 1 depending onstep
c************************************************************************
      implicit none

!     Constants
      
      include 'params.inc'

!     Arguments
      
      integer nhel(maxlines,3**maxlines),step(maxlines),npart,ihel

!     Local

      integer ipart,iflip

c**************KEK**********************   
      integer ql,ql2,ql3
c**************KEK**********************

      logical gotone

      ihel=1
      do ipart=1,npart
c*****************KEK1**************************
         if(step(ipart).gt.0) then
         ql=0
         else 
         ql=-1
         endif 
c****************KEK1***************************
         nhel(ipart,ihel)=-1+ql
      enddo
      gotone=.true.
      do while (gotone)
c         write(*,'(8I4)'),ihel,(nhel(ipart,ihel),ipart=1,npart)
         ihel=ihel+1
         do ipart=1,npart
            nhel(ipart,ihel)=nhel(ipart,ihel-1)
         enddo
         gotone = .false.
         iflip = npart
         do while(.not. gotone .and. iflip .gt. 0)
c*****************KEK2**************************
         if(step(iflip).gt.0) then
         ql=0
         ql2=1
         else
         ql=-1  
         ql2=-1
         endif 
c****************KEK2***************************
            nhel(iflip,ihel) = nhel(iflip,ihel)+step(iflip)*ql2
            ql3=1-ql
            if (nhel(iflip,ihel) .gt. ql3) then
               nhel(iflip,ihel) = -1+ql
               iflip =iflip-1
            else
               gotone=.true.
            endif
         enddo
      enddo
      ihel=ihel-1
      end


!-------------------------------------
      subroutine lower_case(string)
      implicit none

! Constants

      integer    char_a,              char_z
c      parameter (char_a = ichar('A'), char_z = ichar('Z'))
      integer    lcshift
c      parameter (lcshift = ichar('a') - ichar('A'))


! Argument

      character*(*) string

! Local Variable

      integer i

!-----------
! Begin Code
!-----------
      char_a = ichar('A')
      char_z = ichar('Z')
      lcshift = ichar('a')-ichar('A')
      do i=1,len(string)
         if (ichar(string(i:i)) .ge. char_a .and.
     &       ichar(string(i:i)) .le. char_z) then
            string(i:i)=char(ichar(string(i:i))+lcshift)
         end if
      end do
      end

!-------------------------------------
      subroutine upper_case(string)
      implicit none

! Constants

      integer    char_a,              char_z
c      parameter (char_a = ichar('a'), char_z = ichar('z'))
      integer    ucshift
c      parameter (ucshift = ichar('A') - ichar('a'))

! Argument

      character*(*) string

! Local Variable

      integer i

!-----------
! Begin Code
!-----------
      char_A = ichar('a')
      char_Z = ichar('z')
      ucshift = ichar('A')-ichar('a')
c      write(*,*) string
      do i=1,len(string)
         if (ichar(string(i:i)) .ge. char_a .and.
     &       ichar(string(i:i)) .le. char_z) then
            string(i:i)=char(ichar(string(i:i))+ucshift)
         end if
      end do
c      write(*,*) string
      end


      subroutine writebuff2(buff,lun)

      implicit none

! Arguments

      character*(*) buff
      integer lun

! Local Variables

      integer length,i

!-----------
! Begin Code
!-----------
      length = len(buff)
      if (length .le. 60) then
         write (lun,100) buff
      else
         i = 60
         do while (buff(i:i) .ne. '+' .and. buff(i:i).ne.'-')
            i = i - 1
         end do
         if (length-i .gt. 60) then
            print*,'Sorry buff too long in writebuff2',length,i
            print*,buff(:i)
            print*,buff(i:i+60)
            print*,buff(i+60:)
         endif
         write (lun,100) buff(:i)
         write (lun,200) buff(i+1:)
      end if
 100  format ('      ',a)
 200  format ('     &     ',a)
      end



      subroutine get_helset_onium(nexternal,ihel,nhel)

      include 'params.inc'
      include 'onia.inc'
c
c     argument
c
      integer nexternal 
      integer nhel(maxlines,3**maxlines),ihel
c
c     local 
c
      integer jchar,ishift
      character*75 buff

      BUFF = 'DATA (NHEL(IHEL,????),IHEL=1,??) /'
      if ((J_qn.eq.1).and..not.em_decay) then
        write(buff(30:31),'(i2)') nexternal-1
        do i=1,ihel/4
c        polarisation -1
         write(buff(17:20),'(i4)') 3*i-2
         jchar=35
         ishift = 3
         do j=1,nexternal-2
            write(buff(jchar:jchar+ishift),'(I2,a)') nhel(j,4*i),','
            jchar=jchar+ishift
         enddo
         write(buff(jchar:jchar+ishift),'(I2,a)') -1,','
         jchar=jchar+ishift
         jchar=jchar-1
         write(buff(jchar:jchar),'(a1)') '/'
         write(92,10) buff(1:jchar)
c        polarisation 0
         write(buff(17:20),'(i4)') 3*i-1
         jchar=35
         ishift = 3
         do j=1,nexternal-2
            write(buff(jchar:jchar+ishift),'(I2,a)') nhel(j,4*i),','
            jchar=jchar+ishift
         enddo
         write(buff(jchar:jchar+ishift),'(I2,a)') 0,','
         jchar=jchar+ishift
         jchar=jchar-1
         write(buff(jchar:jchar),'(a1)') '/'
         write(92,10) buff(1:jchar)
c        polarisation 1
         write(buff(17:20),'(i4)') 3*i
         jchar=35
         ishift = 3
         do j=1,nexternal-2
            write(buff(jchar:jchar+ishift),'(I2,a)') nhel(j,4*i),','
            jchar=jchar+ishift
         enddo
         write(buff(jchar:jchar+ishift),'(I2,a)') 1,','
         jchar=jchar+ishift
         jchar=jchar-1
         write(buff(jchar:jchar),'(a1)') '/'
         write(92,10) buff(1:jchar)
      enddo

      elseif(S_qn.eq.1.and.L_qn.eq.0.and.em_decay) then

      write(buff(30:31),'(i2)') nexternal
      do i=1,ihel
         write(buff(17:20),'(i4)') i
         jchar=35
         ishift = 3
         do j=1,nexternal
            write(buff(jchar:jchar+ishift),'(I2,a)') nhel(j,i),','
            jchar=jchar+ishift
         enddo
         jchar=jchar-1
         write(buff(jchar:jchar),'(a1)') '/'
         write(92,10) buff(1:jchar)
      enddo

      elseif (J_qn.eq.2) then
        write(buff(30:31),'(i2)') nexternal-1
        do i=1,ihel/4
c        polarisation -2
         write(buff(17:20),'(i4)') 5*i-4
         jchar=35
         ishift = 3
         do j=1,nexternal-2
            write(buff(jchar:jchar+ishift),'(I2,a)') nhel(j,4*i),','
            jchar=jchar+ishift
         enddo
         write(buff(jchar:jchar+ishift),'(I2,a)') -2,','
         jchar=jchar+ishift
         jchar=jchar-1
         write(buff(jchar:jchar),'(a1)') '/'
         write(92,10) buff(1:jchar)
c        polarisation -1
         write(buff(17:20),'(i4)') 5*i-3
         jchar=35
         ishift = 3
         do j=1,nexternal-2
            write(buff(jchar:jchar+ishift),'(I2,a)') nhel(j,4*i),','
            jchar=jchar+ishift
         enddo
         write(buff(jchar:jchar+ishift),'(I2,a)') -1,','
         jchar=jchar+ishift
         jchar=jchar-1
         write(buff(jchar:jchar),'(a1)') '/'
         write(92,10) buff(1:jchar)
c        polarisation 0
         write(buff(17:20),'(i4)') 5*i-2
         jchar=35
         ishift = 3
         do j=1,nexternal-2
            write(buff(jchar:jchar+ishift),'(I2,a)') nhel(j,4*i),','
            jchar=jchar+ishift
         enddo
         write(buff(jchar:jchar+ishift),'(I2,a)') 0,','
         jchar=jchar+ishift
         jchar=jchar-1
         write(buff(jchar:jchar),'(a1)') '/'
         write(92,10) buff(1:jchar)
c        polarisation -1
         write(buff(17:20),'(i4)') 5*i-1
         jchar=35
         ishift = 3
         do j=1,nexternal-2
            write(buff(jchar:jchar+ishift),'(I2,a)') nhel(j,4*i),','
            jchar=jchar+ishift
         enddo
         write(buff(jchar:jchar+ishift),'(I2,a)')  1,','
         jchar=jchar+ishift
         jchar=jchar-1
         write(buff(jchar:jchar),'(a1)') '/'
         write(92,10) buff(1:jchar)
c        polarisation -2
         write(buff(17:20),'(i4)') 5*i
         jchar=35
         ishift = 3
         do j=1,nexternal-2
            write(buff(jchar:jchar+ishift),'(I2,a)') nhel(j,4*i),','
            jchar=jchar+ishift
         enddo
         write(buff(jchar:jchar+ishift),'(I2,a)')  2,','
         jchar=jchar+ishift
         jchar=jchar-1
         write(buff(jchar:jchar),'(a1)') '/'
         write(92,10) buff(1:jchar)
      enddo

      elseif(J_qn.eq.0) then
        write(buff(30:31),'(i2)') nexternal-1
        do i=1,ihel/4
c        write hel=0 for scalar state
         write(buff(17:20),'(i4)') i
         jchar=35
         ishift = 3
         do j=1,nexternal-2
            write(buff(jchar:jchar+ishift),'(I2,a)') nhel(j,4*i),','
            jchar=jchar+ishift
         enddo
         write(buff(jchar:jchar+ishift),'(I2,a)') 0,','
         jchar=jchar+ishift
         jchar=jchar-1
         write(buff(jchar:jchar),'(a1)') '/'
         write(92,10) buff(1:jchar)
        enddo
      endif

 10   format(6X,A)
       return
       end


      subroutine Onium_conv
c
      character*4 onium_type
      common/ to_onium_type/ onium_type

      if(onium_type.eq."1S01") then
          write(92,10) 
     &    'conv=1d0/(2d0*dble(Nc))/dble(Nc)'
          write(92,10)
     &    'conv=conv*(qmass(1)+qmass(2))'
          write(92,10) 
     &     'conv=conv/(2d0*qmass(1)*qmass(2))'
          write(92,10) 'conv=conv*ldme_1S01'
        endif
c
        if(onium_type.eq."1S08") then
          write(92,10)
              write(92,10) 
     &        'conv=1d0/(dble(Nc**2-1))*2d0'
          write(92,10)
     &    'conv=conv*(qmass(1)+qmass(2))'
          write(92,10)
     &     'conv=conv/(2d0*qmass(1)*qmass(2))'
              write(92,10) 'conv=conv*ldme_1S08'
        endif 
c
        if(onium_type.eq."3S11") then
          write(92,10)
     &        'conv=1d0/(3d0*2d0*dble(Nc))/dble(Nc)'
          write(92,10)
     &    'conv=conv*(qmass(1)+qmass(2))'
          write(92,10)
     &     'conv=conv/(2d0*qmass(1)*qmass(2))'
              write(92,10) 'conv=conv*ldme_3S11'
        endif
c
        if(onium_type.eq."3S18") then
          write(92,10)
     &        'conv=1d0/(dble(Nc**2-1)*3d0)*2d0'
          write(92,10)
     &    'conv=conv*(qmass(1)+qmass(2))'
          write(92,10)
     &     'conv=conv/(2d0*qmass(1)*qmass(2))'
              write(92,10) 'conv=conv*ldme_3S18'
        endif
c
        if(onium_type.eq."1P11") then
          write(92,10)
     &        'conv=1d0/(3d0*2d0*dble(Nc))/dble(Nc)'
          write(92,10)
     &    'conv=conv*(qmass(1)+qmass(2))'
          write(92,10)
     &     'conv=conv/(2d0*qmass(1)*qmass(2))'
              write(92,10) 'conv=conv*ldme_1P11'
        endif
c
        if(onium_type.eq."1P18") then
          write(92,10)
     &        'conv=1d0/(dble(Nc**2-1)*3d0)*2d0'
          write(92,10)
     &    'conv=conv*(qmass(1)+qmass(2))'
          write(92,10)
     &     'conv=conv/(2d0*qmass(1)*qmass(2))'
              write(92,10) 'conv=conv*ldme_1P18'
        endif
c
        if(onium_type.eq."3P01") then
          write(92,10)
     &        'conv=1d0/(2d0*dble(Nc))/dble(Nc)'
          write(92,10)
     &    'conv=conv*(qmass(1)+qmass(2))'
          write(92,10)
     &     'conv=conv/(2d0*qmass(1)*qmass(2))'
              write(92,10) 'conv=conv*ldme_3P01'
        endif
c
        if(onium_type.eq."3P08") then
          write(92,10)
     &        'conv=1d0/(dble(Nc**2-1))*2d0'
          write(92,10)
     &    'conv=conv*(qmass(1)+qmass(2))'
          write(92,10)
     &     'conv=conv/(2d0*qmass(1)*qmass(2))'
              write(92,10) 'conv=conv*ldme_3P08'
        endif
c
        if(onium_type.eq."3P11") then
          write(92,10)
     &        'conv=1d0/(3d0*2d0*dble(Nc))/dble(Nc)'
          write(92,10)
     &    'conv=conv*(qmass(1)+qmass(2))'
          write(92,10)
     &     'conv=conv/(2d0*qmass(1)*qmass(2))'
              write(92,10) 'conv=conv*ldme_3P11'
        endif
c
        if(onium_type.eq."3P18") then
          write(92,10)
     &        'conv=1d0/(dble(Nc**2-1)*3d0)*2d0'
          write(92,10)
     &    'conv=conv*(qmass(1)+qmass(2))'
          write(92,10)
     &     'conv=conv/(2d0*qmass(1)*qmass(2))'
              write(92,10) 'conv=conv*ldme_3P18'
        endif
c
        if(onium_type.eq."3P21") then
          write(92,10)
     &        'conv=1d0/(5d0*2d0*dble(Nc))/dble(Nc)'
          write(92,10)
     &    'conv=conv*(qmass(1)+qmass(2))'
          write(92,10)
     &     'conv=conv/(2d0*qmass(1)*qmass(2))'
              write(92,10) 'conv=conv*ldme_3P21'
        endif
c
        if(onium_type.eq."3P28") then
          write(92,10)
     &        'conv=1d0/(dble(Nc**2-1)*5d0)*2d0'
          write(92,10)
     &    'conv=conv*(qmass(1)+qmass(2))'
          write(92,10)
     &     'conv=conv/(2d0*qmass(1)*qmass(2))'
              write(92,10) 'conv=conv*ldme_3P28'
        endif

 10   format(6X,A)
        end
      
