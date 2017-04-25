      Subroutine crossing(ncross,jpart,imatch)
c************************************************************************
c     Determines all the different crossings for the particles
c     and which crossing are identical.  
c     ncross = number of crossings found
c     jpart  = particles put in, and all permutations
c     npart  = number of particles to all permuting
c     imatch = matching since some crossings will have same amplitude
c              keeps from double calculating things (just for speed)
c************************************************************************
      implicit none
c
c Constants
c
      include 'params.inc'
c
c     Arguments
c
      integer ncross,jpart(maxlines,maxcross),imatch(0:maxcross)
c
c     Local
c
      integer i,j,k,l,ipart(maxlines,maxcross),nt,ntb
      integer idiff(6,maxcross),n1,n2,ng,nfound,ndiff,p,nb1,nb2
      integer nu,nch,nub,ncb,nq,t1,t2,nbb,nb,npart
      integer kjet
      logical good

! Global Variables

      integer        iline(-maxlines:maxlines),idir(-maxlines:maxlines)
      integer        this_coup(max_coup) ,goal_coup(max_coup)
      common/to_proc/iline,idir,this_coup,goal_coup

      character*(4*max_particles) particle(4)
      integer                               charge_c(max_particles)
      integer iparticle(0:max_particles,0:4),inverse(max_particles)
      common/to_model/iparticle,  particle,  inverse, charge_c

      logical         lwp,lwm,lz,decay,cross
      common/to_decay/lwp,lwm,lz,decay,cross

      integer         icode(npartons),njet,ijet(10),icount(10)
      common /to_jets/icode,          njet,ijet,    icount
c----
c Begin Code
c----
      if (.false.) then
c         ng = icode(11)
c         nu = icode(2)
c         nch = icode(4)
c         nub= icode(7)
c         ncb= icode(9)
c         nb = icode(5)
c         nbb= icode(10)
      else
      call getparticle('t',nt,nfound)
      if (nfound .ne. 1) print*,'Warning couldnt find t',nt,nfound
      call getparticle('t~',ntb,nfound)
      if (nfound .ne. 1) print*,'Warning couldnt find t~',ntb,nfound
      call getparticle('g',ng,nfound)
      if (nfound .ne. 1) print*,'Warning couldnt find g',ng,nfound
      call getparticle('u',nu,nfound)
      if (nfound .ne. 1) print*,'Warning couldnt find u',nu,nfound
      call getparticle('c',nch,nfound)
      if (nfound .ne. 1) print*,'Warning couldnt find c',nc,nfound
      call getparticle('u~',nub,nfound)
      if (nfound .ne. 1) print*,'Warning couldnt find u~',nub,nfound
      call getparticle('c~',ncb,nfound)
      if (nfound .ne. 1) print*,'Warning couldnt find c~',ncb,nfound
      call getparticle('b~',nbb,nfound)
      if (nfound .ne. 1) print*,'Warning couldnt find b~',nbb,nfound
      call getparticle('b',nb,nfound)
      if (nfound .ne. 1) print*,'Warning couldnt find b',nb,nfound
      endif
c
c     Count number of partons:  OK jet definition set in readproc.f
c
      nq    = 0
      do i=1,iline(0)
         do j=1,11
            if (iline(i) .eq. icode(j)) nq=nq+1
         enddo
      enddo

      ndiff = 0
      ncross= 1
      npart = nq
      do i=1,npart
         do j=1,npart
            ipart(1,ncross) = iline(i)
            ipart(2,ncross) = iline(j)
            jpart(1,ncross) = i
            jpart(2,ncross) = j
            if (i .ne. j) then
               k=3
               do l=1,npart
                  if (l .ne. i .and. l .ne. j) then
                     ipart(k,ncross)=iline(l)
                     jpart(k,ncross)=l
                     k = k+1
                  endif
               enddo
               good = .true.
               do l=1,ncross-1
                  if ( ipart(1,ncross) .eq. ipart(1,l) .and.
     $                 ipart(2,ncross) .eq. ipart(2,l)) good=.false.
               enddo
c
c     Make sure don't cross anything which wasn't a jet or proton
c
c               write(*,'(a,8i5)') 'iline',(iline(k),k=1,npart)
c               write(*,'(a,8i5)') 'ipart',
c     &                            (ipart(k,ncross),k=1,npart)
c               write(*,'(a,8i5)') 'ijet',(ijet(k),k=1,njet)
               do k=1,npart
                  kjet = 1
                  if (ipart(k,ncross) .ne. iline(k)) then
                     do while(ijet(kjet) .lt. k 
     &                         .and. kjet .lt. njet)
                        kjet=kjet+1
                     enddo
                     if (ijet(kjet) .ne. k) good=.false.
c                     write(*,*) k,kjet,ijet(kjet),iline(k)
                  endif
               enddo
c
c     Make sure no b's in initial state
c
               if (.false.) then
                  if (ipart(1,ncross) .eq. nb) good=.false.
                  if (ipart(1,ncross) .eq. nbb) good=.false.
                  if (ipart(2,ncross) .eq. nb) good=.false.
                  if (ipart(2,ncross) .eq. nbb) good=.false.
               endif
c
c     Make sure no t's in initial state
c
               if (ipart(1,ncross) .eq. nt) good=.false.
               if (ipart(1,ncross) .eq. ntb) good=.false.
               if (ipart(2,ncross) .eq. nt) good=.false.
               if (ipart(2,ncross) .eq. ntb) good=.false.
c               write(*,*) 'Good',good
               if (good) then
                  do k=npart+1,iline(0)
                     ipart(k,ncross)=iline(k)
                     jpart(k,ncross)=k
                  enddo
c                  write(*,3)ncross,(ipart(k,ncross),k=1,iline(0))
 3                format( 10i4)
                  if (ncross .gt. maxcross) then
                     print*,'Warning too many crossings',ncross,maxcross
                     ncross=maxcross
                  endif
c
c Check if similar to previous pattern by seeing how many times
c       initial particle is found in the final state, but
c       in case of gluons, mark special with -1
c
                  n1=0
                  n2=0
                  nb1=0
                  nb2=0
c
c     First see how many times the parton appears on the other side
c     and   see how many times inverse appears on the other side.
c     also take note to if its a particle or antiparticle, and up
c     or down type.
c     
                  do p=3,npart
                     if (ipart(1,ncross) .eq. ipart(p,ncross)) n1=n1+1
                     if (ipart(2,ncross) .eq. ipart(p,ncross)) n2=n2+1
                     if (ipart(1,ncross) .eq.
     &                    inverse(ipart(p,ncross))) nb1=nb1+1
                     if (ipart(2,ncross) .eq.
     &                    inverse(ipart(p,ncross))) nb2=nb2+1
                  enddo
c
c     For Z production up and down quarks are different, store up = 1 down=2
c
                  t1=2
                  t2=2
                  if (lz) then
                     if ( ipart(1,ncross) .eq. nu  .or.
     &                    ipart(1,ncross) .eq. nub .or.
     &                    ipart(1,ncross) .eq. nc  .or.
     &                    ipart(1,ncross) .eq. ncb )     t1=1
                     if ( ipart(2,ncross) .eq. nu  .or.
     &                    ipart(2,ncross) .eq. nub .or.
     &                    ipart(2,ncross) .eq. nch  .or.
     &                    ipart(2,ncross) .eq. ncb )     t2=1
                     if (ipart(1,ncross) .eq. ng) t1=0
                     if (ipart(2,ncross) .eq. ng) t2=0
                  endif
c
c     Distinguish particle from antiparticle by multiplying by -1
c
                  if (ipart(1,ncross) .lt.
     &                 inverse(ipart(1,ncross))) then
                     t1=-t1
                  endif
                  if (ipart(2,ncross) .lt.
     &                 inverse(ipart(2,ncross))) then
                     t2=-t2
                  endif
c
c     Check for match
c
                  p=1
c
c     Use this one for Z production where Charge Conjugation is used
c
                  if (lz) then
                  do while ((n1 .ne. idiff(1,p) .or. n2 .ne. idiff(2,p)
     &                .or.   nb1.ne. idiff(3,p) .or. nb2.ne. idiff(4,p)
     &                .or. ((t1 .ne. idiff(5,p) .or. t2 .ne. idiff(6,p))
     &               .and.  (t1 .ne. -idiff(5,p)
     &                                      .or. t2 .ne. -idiff(6,p))))
     &                      .and. (p .le. ndiff))
                     p=p+1
                  enddo

                  elseif(goal_coup(2) .eq. 0 .and.
     &                 goal_coup(3) .eq. 0) then        !QCD use c,p
                  do while ((n1 .ne. idiff(1,p) .or. n2 .ne. idiff(2,p)
     &                .or.   nb1.ne. idiff(3,p) .or. nb2.ne. idiff(4,p))
     &                      .and. (p .le. ndiff))
                     p=p+1
                  enddo

                  else        !W production

                  do while ((n1 .ne. idiff(1,p) .or. n2 .ne. idiff(2,p)
     &                .or.   nb1.ne. idiff(3,p) .or. nb2.ne. idiff(4,p)
     &                .or.   t1 .ne. idiff(5,p) .or. t2 .ne. idiff(6,p))
     &                      .and. (p .le. ndiff))
                     p=p+1
                  enddo
                  endif
c
c     Remove Optimization
c

                  p=ndiff+1

                  if (p .gt. ndiff) then
                     ndiff=ndiff+1
                     idiff(1,ndiff)=n1
                     idiff(2,ndiff)=n2
                     idiff(3,ndiff)=nb1
                     idiff(4,ndiff)=nb2
                     idiff(5,ndiff)=t1
                     idiff(6,ndiff)=t2
                  endif
                  imatch(ncross)=p
                  imatch(0) = ndiff

                  ncross=ncross+1
               endif
            endif
         enddo
      enddo
      ncross=ncross-1
c      print*,'Crossing ',ndiff,ncross,(imatch(i),i=0,imatch(0))
      end

      subroutine getdenom(isym,initc,ihel,step,ipart)
c*************************************************************************
c     Given the initial and final state particles in iline
c     This routine gives the different factors for
c     isym    = 1/n! for identical final state particles
c     initc   = 1/Nch for averaging color of initial state particles
c     ihel    = 1/nhels for averaging over initial spin states
c************************************************************************
      implicit none
c
c     Constants
c
      include 'params.inc'
      integer    maxgroups
      parameter (maxgroups = maxlines-3)
c
c     Arguments
c
      integer ihel,initc,isym,step(maxlines),ipart(maxlines)
c
c     Local
c     
      integer j,identpart(max_particles,maxgroups),nexternal,i
      integer num_external

! Global Variables

      integer        iline(-maxlines:maxlines),idir(-maxlines:maxlines)
      integer        this_coup(max_coup) ,goal_coup(max_coup)
      common/to_proc/iline,idir,this_coup,goal_coup

      integer         nincoming,nincfirst
      common/to_proc2/nincoming,nincfirst

      integer info_p(5,max_particles),iposx(3,max_particles)
      character*(max_string) iwave(max_particles),owave(max_particles)
      character*(8) str(3,max_particles)
      common/to_external/iwave,owave,iposx,info_p,str

      integer                IndexGroupID(maxgroups)
      common /to_IndexGroup/ IndexGroupID

! Functions
      integer identdc
      external identdc

      include "onia.inc"
      
c-----
c Begin Code
c-----
      nexternal=iline(0)
      do i=1,maxgroups
         do j=1,max_particles
            identpart(j,i)=0      !Number of identical final parts
         enddo
      enddo
      call SetIndexGroupID
      initc=1
      isym=1
      num_external=nexternal
      if (onium) num_external=num_external-2
      do i=1,num_external
         if (i .le. nincoming) then                   
            initc = initc*(info_p(1,iline(ipart(i)))) !Average colors
         else
            j = IndexGroupID(i)
            identpart(iline(ipart(i)),j)=identpart(iline(ipart(i)),j)+1
            isym=isym*identpart(iline(ipart(i)),j) ! n! for identical final
c            write(*,*) "new isym",isym
         endif
      enddo
      print *,'isym before: ',isym
      isym=isym*identdc()
      print *,'isym after: ',isym

      ihel=1
      do i=1,nexternal
         if (info_p(2,iline(ipart(i))) .eq. 1) then
            step(i) = 3                        !Only 1 Helicity (scalar)
         elseif (abs(info_p(2,iline(ipart(i)))) .eq. 2) then
            step(i) = 2                        !2 Helicity states -1,1
            if (i .le. nincoming) ihel=ihel*2 
         elseif (info_p(2,iline(ipart(i))) .eq. 3) then
            step(i) = 1                        !3 Helicity state -1,0,1 W,Z
            if (i .le. nincoming) ihel=ihel*3 
c***********************KEK*************************************************
           elseif (info_p(2,iline(ipart(i))) .eq. 4) then
            step(i) = -1                        !5 Helicity state -2,-1,0,1,2
            if (i .le. nincoming) ihel=ihel*5 
c***********************KEK*************************************************
         else
            print*,'Unknown Helicity',info_p(2,iline(ipart(i))),
     &           iline(ipart(i)),ipart(i)
         endif
      enddo
c      print*,'Denominators',initc,ihel,isym,nexternal
      end

      Subroutine SetIndexGroupID
c********************************************************************
c     Sets sequential groupID for external particles
c     based on full groupid
c********************************************************************
      implicit none

! Constants

      include 'params.inc'
      integer    maxgroups
      parameter (maxgroups = maxlines-3)

! Arguments


! Local Variables

      integer i, j, myindex, mygroup
      integer mygroupid, fgroupid(maxgroups),ngroups


! Global Variables

      integer        iline(-maxlines:maxlines),idir(-maxlines:maxlines)
      integer        this_coup(max_coup) ,goal_coup(max_coup)
      common/to_proc/iline,idir,this_coup,goal_coup

      integer         nincoming,nincfirst
      common/to_proc2/nincoming,nincfirst

      integer                IndexGroupID(maxgroups)
      common /to_IndexGroup/ IndexGroupID

! External
      integer  GroupID
      external GroupID
!-----------
! Begin Code
!-----------
      ngroups=0

      do i = nincoming+1,iline(0)  !Loop over final state particles
         mygroup = groupid(i)
         myindex = 0
         do j = 1, ngroups
            if (mygroup .eq. fgroupid(j)) then
               myindex = j
            endif
         enddo
         if (myindex .eq. 0) then
            ngroups = ngroups+1
            myindex = ngroups
            fgroupid(ngroups)=mygroup
         endif
         indexgroupid(i)=myindex
      enddo
      end

      function identdc()
c********************************************************************
c     Checks for identical decay chains and returns symmetry factor
c     So far only for chains which are written in an identical manner
c********************************************************************
      implicit none

      integer identdc

! Global
      include 'params.inc'

      integer        iline(-maxlines:maxlines),idir(-maxlines:maxlines)
      integer        this_coup(max_coup) ,goal_coup(max_coup)
      common/to_proc/iline,idir,this_coup,goal_coup

      integer         nincoming,nincfirst
      common/to_proc2/nincoming,nincfirst

      integer ndcmax
      parameter(ndcmax=20)
      integer ndcpart,idcid(ndcmax),idcgroup(ndcmax)
      logical dcfinal(ndcmax)

! Local
      logical ident
      integer identfact(ndcmax),identnum(ndcmax)
      integer i,j,k,idtmp,ipart,igroup,ingroup,istgr

! Functions
      integer izeroes
      external izeroes

C     Sort decay chains according to particle ids to simplify

      call OrderParticles(ndcpart,idcid,idcgroup,dcfinal)

      do i=1,ndcpart
        identnum(i)=1
        identfact(i)=1
      enddo

C   Check for identical decay chains - need factor n! for that case

c      print *,'Particles: id        group'
c      do i=1,ndcpart
c        write(*,'(i13,i12)') idcid(i),idcgroup(i)
c      enddo

      do i=1,ndcpart
        if(.not.dcfinal(i))then
          do j=i+1,ndcpart
c         Look for identical particles with same mother/same group level
            if(idcid(i).eq.idcid(j).and..not.dcfinal(j).and.
     $         izeroes(idcgroup(i)).eq.
     $         izeroes(idcgroup(j)-idcgroup(i)))then
c           Check if same decay chains
              k=i+1
              ident=.true.
              do while(idcgroup(k)-idcgroup(i).lt.izeroes(idcgroup(i)))
                if(idcid(k).eq.idcid(j+k-i).and.
     $             idcgroup(k)-idcgroup(i).eq.
     $             idcgroup(j+k-i)-idcgroup(j))then
                  k=k+1
                else
                  ident=.false.
                  k=j
                endif
              enddo
              if(ident) then
                print *,'Found identicle chains for ',i,' and ',j
                identnum(i)=identnum(i)+1
                identfact(i)=identfact(i)*identnum(i)
              endif
            endif
          enddo
        endif
      enddo

      identdc=1
      do i=1,ndcpart
        identdc=identdc*identfact(i)
      enddo

      return
      end
