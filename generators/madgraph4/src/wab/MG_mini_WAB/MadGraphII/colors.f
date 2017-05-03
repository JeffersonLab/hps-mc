!-----------------------
! madgraph - a Feynman Diagram package by Tim Stelzer and Bill Long 
! (c) 1993
!
! Filename: colors.f
!-----------------------

      subroutine simpcolors(color,i)
!**************************************************************************
!     A set of routines to handle color matrix algebra
!     color(0,0,0)       = number of terms
!     color(0,0,term)    = number of factors for term
!     color(1,1,term)    = numerator coeficient of term
!     color(2,1,term)    = denominator coeficient of term
!     color(3,1,term)    = Color flow # for this term
!     color(4,1,term)    = # of flows for this graph
!     color(0,1,term)    = 2 because have numerator and denominator
!     color(0,fact,term) = number of colors in factor
!     color(i,fact,term) = color line of ith element in factor,term
!**************************************************************************
      implicit none

! Constants

      include 'params.inc'
      
! Arguments

      integer color(0:2*maxlines,0:maxfactors,0:maxterms),i

! Local Variables

      integer j,nlines


!     Global

      character*(max_string) iwave(max_particles),owave(max_particles)
      integer iposx(3,max_particles)
      integer info_p(5,max_particles)
      character*(8) str(3,max_particles)
      common/to_external/iwave,owave,iposx,info_p,str

      integer        iline(-maxlines:maxlines),idir(-maxlines:maxlines)
      integer        this_coup(max_coup) ,goal_coup(max_coup)
      common/to_proc/iline,idir,this_coup,goal_coup
      logical         sumgluons
      common/to_cflow/sumgluons

!-----------
! Begin Code
!-----------
c      call printcolors(color)
      if (i .lt. 0 .and. .not. sumgluons) then 
         nlines=iline(0)-3                     !Number of internal lines
         do j=1,nlines
            if (info_p(1,iline(i*j)) .eq. 3) then !Triplet line, sum it first
               call SumColors(color,j*i)
c               call addterms(color)
               call clearzeros(color)
            endif
         enddo
         do j=1,nlines
            if (info_p(1,iline(i*j)) .eq. 8) then !Octet line, sum it second
               call SumColors(color,j*i)
               call clearzeros(color)
c               call addterms(color)
            endif
         enddo
      else
         nlines=iline(0)                        !Number of external lines
         do j=1,nlines                          !Here order doesn't matter
            call SumColors(color,j*i)
            call clearzeros(color)
c            call addterms(color)
         enddo
      endif
      end


      Subroutine SumColors(color,tcolor)
!**************************************************************************
!     Find Identical color line (tcolor) and sum over it.
!**************************************************************************
      implicit none

! Constants

      include 'params.inc'

! Arguments

      integer color(0:2*maxlines,0:maxfactors,0:maxterms),tcolor

! Local Variables

      integer nterms,nfactors,nelements,summe(2,3)
      integer iterm,ifact,ielement,nfound,nt

      integer imaxfactors, imaxterms

! Global

      logical         sumgluons
      common/to_cflow/sumgluons
      data imaxfactors, imaxterms /1,1/
!-----------
! Begin Code
!-----------
      nterms=color(0,0,0)
      iterm =1
      if (nterms .gt. imaxterms) then
         imaxterms = nterms
c         write(*,*) 'New max terms',nterms
      endif
      do while (iterm .le. nterms)
c         call printcolors(color)
         nfactors=color(0,0,iterm)
         if (nfactors .gt. imaxfactors) then
            imaxfactors = nfactors
c            write(*,*) 'New max factors',nfactors
         endif
         nfound=0
         nt=0
         do ifact=2,nfactors
            nelements=color(0,ifact,iterm)
            do ielement=1,abs(nelements)
               if (color(ielement,ifact,iterm) .eq. tcolor) then  !found one
                  if (nfound .gt. 2) then
                     print*,'Warning more that two colors',tcolor
                     stop
                  endif
                  nfound=nfound+1
                  summe(nfound,1)=iterm
                  summe(nfound,2)=ifact
                  summe(nfound,3)=ielement
                  if(color(0,ifact,iterm) .lt. 0) then
                     nt=nt+1                             !number of T matrices
                  endif
               endif
            enddo
         enddo
         if (nfound .ne. 2) then
            iterm=iterm+1
         else
            if (nt .eq. 2 ) then
               if (summe(1,2) .ne. summe(2,2)) then
c
c     Here we need to check if gluon is being summed, and if that is ok
c
                  if ((summe(1,3) .le. 2 .and. summe(2,3) .le. 2) .or.
     &                 sumgluons) then 
                     if (summe(1,3) .lt. summe(2,3) ) then !put in order
                        call ttsum(color,summe(1,1),summe(1,2),
     &                       summe(1,3),summe(2,2),summe(2,3))
                     else
                        call ttsum(color,summe(2,1),summe(2,2),
     &                       summe(2,3),summe(1,2),summe(1,3))
                     endif
                  else
                     iterm=iterm+1   !Can't sum this one yet
                  endif
               else
                  call tsum(color,summe(1,1),summe(1,2),summe(1,3),
     &                 summe(2,2),summe(2,3))
               endif
            else if (nt .eq. 1 ) then
               if (color(0,summe(1,2),summe(1,1)) .gt. 0) then
                  call ftsum(color,summe(1,1),summe(1,2),summe(1,3),
     &                 summe(2,2),summe(2,3))
               else
                  call ftsum(color,summe(2,1),summe(2,2),summe(2,3),
     &                 summe(1,2),summe(1,3))
               endif
            else if (nt .eq. 0 ) then
               if (summe(1,2) .ne. summe(2,2)) then
                  call ffsum(color,summe(1,1),summe(1,2),summe(1,3),
     &                 summe(2,2),summe(2,3))
               else
                  call fsum(color,summe(1,1),summe(1,2),summe(1,3),
     &                 summe(2,2),summe(2,3))
               endif
            endif
         endif
         nterms=color(0,0,0)
      enddo
      end
      
      
      Subroutine TraceT(color)
!**************************************************************************
!     A set of routines to handle color matrix algebra
!     This deals with taking the trace of an arbitrary number of T matrices
!**************************************************************************
      implicit none

! Constants

      include 'params.inc'
c      integer   maxlines,  maxfactors,  maxterms
c      parameter(maxlines=8,maxfactors=9,maxterms=250)

! Arguments

      integer color(0:2*maxlines,0:maxfactors,0:maxterms)

! Local Variables

      integer nterms,nfactors,nelements
      integer iterm,ifact,i,j

!-----------
! Begin Code
!-----------
      nterms=color(0,0,0)
      do iterm=1,nterms
         nfactors=color(0,0,iterm)
         ifact=1
         do while(ifact .lt. nfactors)
            ifact=ifact+1
            nelements=color(0,ifact,iterm)
            if (nelements .lt. 0) then                   !This is a T matrix
               if (color(1,ifact,iterm) .eq. color(2,ifact,iterm)) then
                  if (nelements .eq. -2) then !T matrix no glue
                     color(1,1,iterm)=abs(NC)*color(1,1,iterm)!T(i,i) = 3
                     color(0,0,iterm)=color(0,0,iterm)-1
                     nfactors=color(0,0,iterm)
                     do i=ifact,nfactors
                        color(0,i,iterm)=color(0,i+1,iterm)
                        do j=1,abs(color(0,i,iterm))
                           color(j,i,iterm)=color(j,i+1,iterm)
                        enddo
                     enddo
                     ifact=ifact-1           !set factor back since 1 deleted
                  elseif (nelements .eq. -3)  then        !T matrix 1 glue
                     color(1,1,iterm) = 0                 !tr[T(a)]=0
                  elseif (nelements .lt. -3) then
                     color(0,ifact,iterm) = -color(0,ifact,iterm)-2
                     do i=1,-nelements-2
                        color(i,ifact,iterm)=color(i+2,ifact,iterm)
                     enddo
                  else
                     print*,'Error T matrix w/ wrong # elements',
     &                    nelements
                     stop
                  endif
               endif
            endif     !Identical quark sums
         enddo        !factors
      enddo           !Terms
      end


      Subroutine ClearZeros(color)
!**************************************************************************
!     Looks for terms with zero's in constant factor, and removes them
!     Also removes factors of 1
!     And tries to reduce numerator and denominator
!**************************************************************************
      implicit none

! Constants

      include 'params.inc'
c      integer   maxlines,  maxfactors,  maxterms
c      parameter(maxlines=8,maxfactors=9,maxterms=250)
c      integer   Nc
c      parameter(Nc=3)

! Arguments

      integer color(0:2*maxlines,0:maxfactors,0:maxterms)

! Local Variables

      integer nterms,nfactors,nelements
      integer iterm,ifact,ielement,i,j,m,n,r
      logical reduce

!-----------
! Begin Code
!-----------
      nterms=color(0,0,0)              !Eliminate F[a] because is zero
      do iterm=1,nterms
         nfactors=color(0,0,iterm)
         do ifact=2,nfactors
            if (color(0,ifact,iterm) .eq. 1)  then    !F[a] = 0
               color(1,1,iterm)=0
            endif
         enddo
      enddo
      
      nterms=color(0,0,0)
      iterm=0
      do while (iterm .lt. nterms)
         iterm=iterm+1
         if (color(1,1,iterm) .eq. 0.) then            !numerator is zero
            color(0,0,0)=color(0,0,0)-1                !deleteing term
            do i=iterm,color(0,0,0)
               color(0,0,i)=color(0,0,i+1)
               do ifact=1,color(0,0,i+1)
                  color(0,ifact,i)=color(0,ifact,i+1)
                  do ielement=1,abs(color(0,ifact,i+1))
                     color(ielement,ifact,i)=color(ielement,ifact,i+1)
                  enddo
               enddo
            enddo
            nterms=color(0,0,0)                       !reset number of terms
            iterm =iterm-1                            !reset term working on
         endif
      enddo

      nterms=color(0,0,0)            !Clear factors with zero elements
      do iterm=1,nterms
         nfactors=color(0,0,iterm)
         ifact=0
         do while (ifact .lt. nfactors)
            ifact=ifact+1
            nelements=color(0,ifact,iterm)
            if (nelements .eq. 0)   then           !This is f w/ no glue = Nc
               color(1,1,iterm)=color(1,1,iterm)*abs(Nc)               
               do i=ifact,nfactors-1
                  ielement=color(0,i+1,iterm)
                  do j=0,ielement
                     color(j,i,iterm)=color(j,i+1,iterm)
                  enddo
               enddo
               ifact=ifact-1
               color(0,0,iterm)=color(0,0,iterm)-1
               nfactors=color(0,0,iterm)
            elseif (color(0,ifact,iterm) .eq. 1) then   !F matrix w/ 1 glue
               color(1,1,iterm) = 0             !This is zero
            endif
         enddo
      enddo

      nterms=color(0,0,0)
      do iterm=1,nterms
         reduce=.true.
         m=color(1,1,iterm)
         n=color(2,1,iterm)         !denominator
         if (n .ne. 0) then 
         do while (reduce)
            r=mod(m,n)
            if (r .eq. 0) then
               reduce=.false.
               color(1,1,iterm)=color(1,1,iterm)/n
               color(2,1,iterm)=color(2,1,iterm)/n
            endif
            m=n
            n=r
         enddo
         endif
      enddo
      end


      integer function igcf(in,im)
c************************************************************************
c     Returns greatest common factor between n and m
c************************************************************************
      implicit none
c
c     Arguments
c
      integer in,im
c
c     Local
c
      integer n,m,r
      logical reduce
c-----
c  Begin Code
c-----
      reduce=.true.
      igcf = 1
      m=im
      n=in
      if (n .ne. 0) then 
         do while (reduce)
            r=mod(m,n)
            if (r .eq. 0) then
               reduce=.false.
               igcf = n
            endif
            m=n
            n=r
         enddo
      endif
      end

      


      Subroutine PrintColors(color)
!**************************************************************************
!     Print out T,F matrices
!**************************************************************************
      implicit none

! Constants

      include 'params.inc'

! Arguments

      integer color(0:2*maxlines,0:maxfactors,0:maxterms)

! Local Variables

      integer nterms,nfactors,nelements
      integer iterm,ifact,i
      real sum

!-----------
! Begin Code
!-----------
      nterms=color(0,0,0)
      sum=0
      do iterm=1,nterms
         nfactors=color(0,0,iterm)
         write(*,22) 'Term',iterm,' Coefficient: ',
     &        isign(color(1,1,iterm),color(2,1,iterm)),
     &        '/',abs(color(2,1,iterm))
         sum=sum+real(color(1,1,iterm))/real(color(2,1,iterm))
         do ifact=2,nfactors
            nelements=color(0,ifact,iterm)
            if (nelements .gt. 0) then
               write(*,'(a$)') 'F['
               write(*,'(i3$)') color(1,ifact,iterm)
               do i=2,nelements
                  write(*,32) color(i,ifact,iterm)
               enddo
               write(*,'(a)') ']'
            elseif (nelements .lt. 0) then
               write(*,'(a$)') 'T['
               write(*,'(i3$)') color(1,ifact,iterm)
               do i=2,-nelements
                  write(*,32) color(i,ifact,iterm)
               enddo
               write(*,'(a)') ']'
            else
            endif
         enddo
      enddo
 22   format (a,i3,a,i5,a,i5,i5)
c 22   format (a,i2,a,i3,a,i3,i3)
 32   format (',',i3,$)
      end

      Subroutine write_color_comment(color,ilun)
!**************************************************************************
!     Write comments for T and F matrices in color
!**************************************************************************
      implicit none

! Constants

      include 'params.inc'

! Arguments

      integer iflow, ilun
      integer color(0:2*maxlines,0:maxfactors,0:maxterms)

! Local Variables

      integer nterms,nfactors,nelements
      integer iterm,ifact,i
      real sum

!-----------
! Begin Code
!-----------
      nterms=color(0,0,0)
      sum=0
      do iterm=1,1                !Only print first (leading) term
         write(ilun,'(a$)') 'C               '
         nfactors=color(0,0,iterm)
         sum=sum+real(color(1,1,iterm))/real(color(2,1,iterm))
         do ifact=2,nfactors
            nelements=color(0,ifact,iterm)
            if (nelements .gt. 0) then
               write(ilun,'(a$)') 'F['
               write(ilun,'(i1$)') color(1,ifact,iterm)
               do i=2,nelements
                  write(ilun,32) color(i,ifact,iterm)
               enddo
               write(ilun,'(a$)') ']'
            elseif (nelements .lt. 0) then
               write(ilun,'(a$)') 'T['
               write(ilun,'(i2$)') color(1,ifact,iterm)
               do i=2,-nelements
                  write(ilun,32) color(i,ifact,iterm)
               enddo
               write(ilun,'(a$)') ']'
            else
            endif
         enddo
         write(ilun,'(a)') ' '
      enddo
c 22   format (a,i3,a,i5,a,i5,i5)
 32   format (',',i2,$)
      end



      subroutine setcolors(color)
!**************************************************************************
!     A set of routines to handle color matrix algebra
!     color(0,0,0)       = number of terms
!     color(0,0,term)    = number of factors for term
!     color(1,1,term)    = numerator coeficient of term
!     color(2,1,term)    = denominator coeficient of term
!     color(3,1,term)    = Color flow of this term
!     color(4,1,term)    = # of Color flows for graph
!     color(0,1,term)    = 4 because have numerator and denominator
!     color(0,fact,term) = number of colors in factor
!     color(i,fact,term) = color line of ith element in factor,term
!**************************************************************************
      implicit none

! Constants

      include 'params.inc'
c      integer   maxlines,  maxfactors,  maxterms
c      parameter(maxlines=8,maxfactors=9,maxterms=250)

! Arguments

      integer color(0:2*maxlines,0:maxfactors,0:maxterms)

!-----------
! Begin Code
!-----------
      color(0,0,0) = 1                       !1 term

      color(0,0,1) = 1                       !1 factors
      
      color(0,1,1) = 4                       !2 parts + num and denom
      color(1,1,1) = 1                       !numerator
      color(2,1,1) = 1                       !denominator
      color(3,1,1) = 1                       !1st flow
      color(4,1,1) = 1                       !1 flow to start

      end
      

      Subroutine copyterm(color,iterm,ifact1,ifact2)
!**************************************************************************
!     Copies iterm to end of color matrix, w/o includeing ifact1,ifact2
!**************************************************************************
      implicit none

! Constants

      include 'params.inc'
c      integer   maxlines,  maxfactors,  maxterms
c      parameter(maxlines=8,maxfactors=9,maxterms=250)

! Arguments

      integer color(0:2*maxlines,0:maxfactors,0:maxterms)
      integer ifact1,ifact2,iterm

! Local Variables

      integer nterms,nfactors,nelements,nfound
      integer ifactor,ielement

!-----------
! Begin Code
!-----------
      color(0,0,0) = color(0,0,0) +1
      nterms=color(0,0,0)
      nfactors=color(0,0,iterm)
      nfound = 0
      do ifactor=1,nfactors
         if (ifactor .ne. ifact1 .and. ifactor .ne. ifact2) then
            nelements=color(0,ifactor,iterm)
            color(0,ifactor-nfound,nterms)=color(0,ifactor,iterm)
            do ielement=1,abs(nelements)
               color(ielement,ifactor-nfound,nterms)
     &              =color(ielement,ifactor,iterm)
            enddo
         else
            nfound=nfound+1
         endif
      enddo
      color(0,0,nterms)=nfactors-nfound
      end
      

      integer function AddFlow(color,flows,iterm)
!***************************************************************************
!     Store a flow so other flows can be compared to it
!***************************************************************************
      implicit none

! Constants

      include 'params.inc'
c      integer   maxlines,  maxfactors,  maxterms,    maxflows
c      parameter(maxlines=8,maxfactors=9,maxterms=250,maxflows=200)

! Arguments

      integer color(0:2*maxlines,0:maxfactors,0:maxterms)
      integer flows(0:2*maxlines,0:maxfactors,0:24,0:maxflows)
      integer iterm

! Local Variables

      integer nterms,nfactors
      integer ifactor,ielement,nflows

!-----------
! Begin Code
!-----------
      if (flows(0,0,0,0) .lt. maxflows) then
      flows(0,0,0,0)=flows(0,0,0,0)+1              !one more flow added
      nflows=flows(0,0,0,0)
      nterms=0
      flows(0,0,0,nflows)=0              !no terms so far
      nterms=nterms+1
      flows(0,0,0,nflows)=flows(0,0,0,nflows)+1 !1 more term
      nfactors=color(0,0,iterm)
      flows(0,0,nterms,nflows)=nfactors
      do ifactor=1,nfactors
         flows(0,ifactor,nterms,nflows)=color(0,ifactor,iterm) !#elements
         do ielement=1,abs(color(0,ifactor,iterm))
            flows(ielement,ifactor,nterms,nflows)=
     &           color(ielement,ifactor,iterm)
         enddo
         flows(1,1,nterms,nflows)=1
         flows(2,1,nterms,nflows)=1 !Set fraction to 1
      enddo
      addflow=nflows     
c      call PrintColors(flows(0,0,0,nflows))
      else
         print*,'Sorry exceeded maximum number of flows',maxflows
      endif
      end

      
      Integer Function foundflow(color,flows,jterm)
!***************************************************************************
!     Looks to see if the flow already exists if flows, if so, return true
!***************************************************************************
      implicit none

! Constants

      include 'params.inc'

! Arguments

      integer color(0:2*maxlines,0:maxfactors,0:maxterms)
      integer flows(0:2*maxlines,0:maxfactors,0:24,0:maxflows)
      integer jterm

! Local Variables

      integer nterms,nfacts,nelements
      integer iterm,ifact,ielement
      logical lfactor
      integer fflow,fterm,nfterms,nflows,nffacts,ffact
      integer igterm,goodterms(10),addflow

!-----------
! Begin Code
!-----------
      nflows=flows(0,0,0,0)
      lfactor=.false.
      fflow=0
      nterms=1
      goodterms(nterms)=jterm
      do while(fflow .lt. nflows .and. .not. lfactor)         
         fflow=fflow+1
         lfactor=.true.
         igterm=0
         nfterms=flows(0,0,0,fflow)
         lfactor = (nfterms .eq. nterms)
         do while(igterm .lt. nterms .and. lfactor)
            igterm=igterm+1
            iterm=goodterms(igterm)
            nfacts=color(0,0,iterm)
            lfactor=.false.
            fterm=0
            do while(fterm .lt. nfterms .and. .not. lfactor)
               fterm=fterm+1
               lfactor=.true.
               ifact   = 1       !ignore constant factors
               nffacts = abs(flows(0,0,fterm,fflow))
               do while(ifact .lt. nfacts .and. lfactor)
                  ifact=ifact+1
                  ffact=1       !ignore constant factors
                  lfactor=.false.
                  do while(ffact .lt. nffacts .and. .not. lfactor)
                     ffact=ffact+1
                     ielement=-1 !Check the 0 component too
                     nelements=abs(color(0,ifact,iterm))
                     lfactor=.true.
                     do while (ielement .lt. nelements .and. lfactor)
                        ielement=ielement+1
                        lfactor=(color(ielement,ifact,iterm) .eq.
     &                       (flows(ielement,ffact,fterm,fflow)))
                     enddo
                  enddo
               enddo
            enddo
         enddo
      enddo
      if (lfactor) then
         foundflow=fflow
      else
         foundflow=addflow(color,flows,jterm)
c         write(*,*) 'Adding flow',foundflow,jterm
c         call printcolors(flows(0,0,0,foundflow))
      endif
      end
            
      
      Subroutine AddTerms(color)
!***************************************************************************
!     Looks for idential color factor terms, and if they exist
!     adds them together to make 1 term
!***************************************************************************
      implicit none

! Constants

      include 'params.inc'
c      integer   maxlines,  maxfactors,  maxterms
c      parameter(maxlines=8,maxfactors=9,maxterms=250)

! Arguments

      integer color(0:2*maxlines,0:maxfactors,0:maxterms)

! Local Variables

      integer nterms,nfactors,nelements
      integer iterm,ifact,ielement
      integer cterm,cfact,cfactors
      logical lgood

!-----------
! Begin Code
!-----------
      cterm=0
      do while (cterm .lt. color(0,0,0))
         cterm=cterm+1
         iterm=cterm
         lgood=.false.
         nterms=color(0,0,0)
         cfactors=color(0,0,cterm)
         do while (.not. lgood .and. iterm .lt. nterms)
            iterm=iterm+1
            cfact = 1
            nfactors=color(0,0,iterm)
            lgood= (nfactors .eq. cfactors)
            do while (lgood .and. cfact .lt. cfactors)
               cfact=cfact+1
               lgood=.false.
               ifact=1
               do while (.not. lgood .and. ifact .lt. nfactors)
                  ielement=-1
                  ifact=ifact+1
                  lgood=.true.
                  nelements=abs(color(0,ifact,iterm))
                  do while ( lgood .and. ielement .lt. nelements)
                     ielement=ielement+1
                     lgood = (color(ielement,ifact,iterm) .eq.
     &                    color(ielement,cfact,cterm)) 
                  enddo         !if lgood after enddo have good factor
               enddo            !nothing here, just sum over terms
            enddo               !if lgood after enddo have good term

            if (lgood .and. cterm .ne. iterm) then !add two terms
c               write(*,*) 'Combining terms',iterm,cterm
c               call printcolors(color)
               color(1,1,cterm)=color(1,1,cterm)*color(2,1,iterm)
     &              +color(1,1,iterm)*color(2,1,cterm)
               color(2,1,cterm)=color(2,1,cterm)*color(2,1,iterm)
c               write(*,*) 'Result',color(1,1,cterm),color(2,1,cterm)
               color(1,1,iterm)=0
               call ClearZeros(color)
c               call printcolors(color)
               cterm=cterm-1
            else
            endif
         enddo
      enddo
      end
      
                     
      subroutine orderF(color)
!***************************************************************************
!     Uses fact F(a,b,c)=F(c,a,b) to order 
!***************************************************************************
      implicit none

! Constants

      include 'params.inc'
c      integer   maxlines,  maxfactors,  maxterms
c      parameter(maxlines=8,maxfactors=9,maxterms=250)

! Arguments

      integer color(0:2*maxlines,0:maxfactors,0:maxterms)

! Local Variables

      integer nterms,nfactors,nelements
      integer iterm,ifact,ielement
      integer i,j,ismall,itemp(maxlines)

!-----------
! Begin Code
!-----------
      nterms=color(0,0,0)
      do iterm=1,nterms
         nfactors=color(0,0,iterm)
         do ifact=2,nfactors                  !first one is just constant
            nelements=color(0,ifact,iterm)    !positive if F, then order
            ismall=1
            do ielement=1,nelements
               if (color(ielement,ifact,iterm) .lt.
     &               color(ismall,ifact,iterm)) ismall=ielement
            enddo
            if (ismall .ne. 1) then           !rearrange the order
               do i=1,nelements
                  itemp(i)=color(i,ifact,iterm)
               enddo
               do i=1,nelements
                  j=i-1+ismall
                  if (j .gt. nelements) j=j-nelements
                  color(i,ifact,iterm) = itemp(j)
               enddo
            endif
         enddo
      enddo
      end
      
               
      subroutine OrderColors(color)
!**************************************************************************
!
!     This routine first orders the T matrices by the line number
!     of the first component. The key is we want all colors to be ordered
!     in an identical fashion so it is easy to see if two colors are the same
!
!     A set of routines to handle color matrix algebra
!     color(0,0,0)       = number of terms
!     color(0,0,term)    = number of factors for term
!     color(1,1,term)    = numerator coeficient of term
!     color(2,1,term)    = denominator coeficient of term
!     color(3,1,term)    = Color flow # for this term
!     color(4,1,term)    = # of flows for this graph
!     color(0,1,term)    = 2 because have numerator and denominator
!     color(0,fact,term) = number of colors in factor
!     color(i,fact,term) = color line of ith element in factor,term
!**************************************************************************
      implicit none

! Constants

      include 'params.inc'
      
! Arguments

      integer color(0:2*maxlines,0:maxfactors,0:maxterms)

! Local Variables

      integer iterm, ifactor, ielement, i
      integer nterms, nfactors,jline,ncline
      integer iorder(maxfactors),iseq(maxfactors),mapline(maxlines)
      integer ctemp(0:2*maxlines,maxfactors)

!-----------
! Begin Code
!-----------
c      write(*,*) 'Before ordering'
c      call printcolors(color)
      nterms=color(0,0,0)
      do iterm=1,nterms
c
c        First we build an array containing the first element of each factor
c
         nfactors=color(0,0,iterm)
         iseq(1) = 1
         do ifactor=2,nfactors
            iorder(ifactor)=color(1,ifactor,iterm)
            iseq(ifactor)  = ifactor
         enddo
         call sort2(iorder(2),iseq(2),nfactors-1)!iseq now tells me how  order
c
c     Copy elements to the correct order in a temp array, and renumber
c     internal lines, so least negative come first. (eg -1,-2,....)
c
         do jline=1,maxlines
            mapline(jline)=0           !Reset mapping to null
         enddo
         ncline = 0                    !Reset number of internal lines
         do ifactor=2,nfactors
            ctemp(0,ifactor)=color(0,iseq(ifactor),iterm)
            do ielement=1,abs(color(0,iseq(ifactor),iterm))
               jline = color(ielement,iseq(ifactor),iterm)
               if (jline .lt. 0) then         !Remap the internal lines too
                  if (mapline(-jline) .eq. 0) then
                     ncline=ncline-1          !One more internal line
                     mapline(-jline)=ncline
                  endif
                  jline=mapline(-jline)       !Reset to new mapped line
               endif
               ctemp(ielement,ifactor) = jline
            enddo
         enddo
c
c     Copy back to the color matrix.
c
         do ifactor=2,nfactors
            do ielement=0,abs(ctemp(0,ifactor))
               color(ielement,ifactor,iterm)=ctemp(ielement,ifactor)
            enddo
         enddo
      enddo
c      write(*,*) 'After ordering'
c      call printcolors(color)
      end

