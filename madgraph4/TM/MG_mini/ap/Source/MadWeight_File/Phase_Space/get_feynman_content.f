cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
cc											cc
cc				          FILE CONTENT                                  cc
cc											cc
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
cc											cc
cc											cc
cc 	info_final_part: 	#initialize final state information			cc
cc											cc
cc											cc
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
cc	Author: O.Mattelaer and P. artoisenet (UCL-CP3)					cc
cc	last modif: 12/03/08								cc
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc







cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
c
c
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      subroutine info_final_part()
c
c	this subroutine initializes the following common blocks concerning final state information 
c
c      common block: num_part
c      ----------------------
c           num_inv: number of missing particles
c           num_jet: number of jets
c           num_e:number of electrons
c           num_ae:number of positrons
c           num_mu:number of muons
c           num_amu:number of anti-muons
c           num_ta:number of taus
c           num_ata:number of anti-taus
c
c      common block: end_par_type
c      --------------------------
c           jet(nexternal-2): containing madgraph number of the jets (last components set to 0)
c           bjet(nexternal-2): containing madgraph number of the bjets (if not consider like jet for permutations)
c           el(nexternal-2): containing madgraph number of the electrons (last components set to 0)
c           ae(nexternal-2): containing madgraph number of the positrons (last components set to 0)
c           mu(nexternal-2): containing madgraph number of the muons (last components set to 0)
c           amu(nexternal-2): containing madgraph number of the anti-muons (last components set to 0)
c           ta(nexternal-2): containing madgraph number of the muons (last components set to 0)
c           ata(nexternal-2): containing madgraph number of the anti-muons (last components set to 0)
c           neutrino(nexternal-2): containing madgraph number of the invisible particles (last components set to 0)
c
c
c     common block: madgraph_order_type
c     ---------------------------------
c           inv_matching_type_part(3:nexternal): function: MG order --> permutation order(classed by types of particles) 
c           matching_type_part(3:nexternal):     function:  permutation order  --> MG order
c

      implicit none
      
      include './genps.inc'

      logical use_perm,perm_with_b
      common/global_run_perm/use_perm,perm_with_b
      
      integer matching_type_part(3:nexternal) 
      integer inv_matching_type_part(3:nexternal)
      common/madgraph_order_type/matching_type_part,
     & inv_matching_type_part 
 
      integer i
      integer    maxflow
      parameter (maxflow=999)
      integer idup(nexternal,maxproc)
      integer mothup(2,nexternal,maxproc)
      integer icolup(2,nexternal,maxflow)
      include './leshouche.inc'
      
      logical pass     					!type agreement
      integer num_inv,num_jet,num_bjet,num_e,num_ae,num_mu,num_amu,
     + num_ta,num_ata,num_notfind
      COMMON/num_part/num_inv,num_jet,num_bjet,num_e,num_ae,num_mu,
     + num_amu,num_ta,num_ata
      
      
      integer jet(nexternal-2)
      integer bjet(nexternal-2)
      integer el(nexternal-2)
      integer ael(nexternal-2)
      integer mu(nexternal-2)
      integer amu(nexternal-2)
      integer ta(nexternal-2)
      integer ata(nexternal-2)
      integer neutrino(nexternal-2)
      common/end_par_type/jet,bjet,el,ael,mu,amu,ta,ata,neutrino
c
c     local
c         
      integer l,tag					!counter


c     init all variable:
      do l=1,nexternal-2
         jet(l)=0
         bjet(l)=0
         el(l)=0
         ael(l)=0
         mu(l)=0
         amu(l)=0
         ta(l)=0
         ata(l)=0
         neutrino(l)=0
      enddo
      num_inv=0
      num_jet=0
      num_bjet=0
      num_mu=0
      num_amu=0
      num_e=0
      num_ae=0
      num_ta=0
      num_ata=0
      num_notfind=0


cc***********************************************************************
cc
cc                    Identification of particles
cc
cc***********************************************************************


      do l=3,nexternal
c
c      invisible particule identification 
c
         pass=.false.
         if (abs(IDUP(l,1)).eq.12) then
            pass=.true.         !ve
         elseif (abs(IDUP(l,1)).eq.14)then
            pass=.true.         !vm 
         elseif (abs(IDUP(l,1)).eq.16)then
            pass=.true.         !vt
         elseif (abs(IDUP(l,1)).eq.18)then
            pass=.true.         !v'
         elseif (abs(IDUP(l,1)).eq.1000022)then
            pass=.true.         !chi0
         elseif (abs(IDUP(l,1)).eq.1000023)then
            pass=.true.         !chi0
         elseif (abs(IDUP(l,1)).eq.1000024)then
            pass=.true.         !chi0
         elseif (abs(IDUP(l,1)).eq.1000025)then
            pass=.true.         !chi0    
         elseif (abs(IDUP(l,1)).eq.1000035)then
            pass=.true.         !chi0
         endif
	 
         if (pass)then
            num_inv=num_inv+1
            neutrino(num_inv) = l
	    goto 20 !continue (next identification)
         endif
c
c      jet identification 
c
         pass=.false.
         if (abs(IDUP(l,1)).le.4)then
            pass=.true.         !quark u,d,s,c
         elseif(perm_with_b.and.abs(IDUP(l,1)).eq.5)then
            pass=.true.         !quark b
         elseif (abs(IDUP(l,1)).eq.21)then
            pass=.true.         !gluon
         endif
	 
         if (pass)then
            num_jet=num_jet+1
            jet(num_jet)=l
	 goto 20 ! continue (next identification)
         endif
c
c      bjet identification 
c
         pass=.false.
         if(.not.perm_with_b.and.abs(IDUP(l,1)).eq.5)then
            pass=.true.
         endif
	 
         if (pass)then
            write(*,*) 'find bjet',perm_with_b
            num_bjet=num_bjet+1
            bjet(num_bjet)=l
	 goto 20 ! continue (next identification)
         endif
c
c      electron identification 
c
         pass=.false.
         if (IDUP(l,1).eq.11) pass=.true. !electron
         
  	 if (pass)then
            num_e=num_e+1
            el(num_e)=l
	 goto 20 !continue (next identification)
         endif

c
c      positron identification
c
         pass=.false.
         if (IDUP(l,1).eq.-11) pass=.true. !electron
         if (pass)then
            num_ae=num_ae+1
            ael(num_ae)=l
         goto 20 !continue (next identification)
         endif


c
c      muon identification 
c
         pass=.false.
         if (IDUP(l,1).eq.13) pass=.true. !muon

         if (pass)then
            num_mu=num_mu+1
            mu(num_mu)=l
	 goto 20 !continue (next identification)
         endif
c
c      anti muon identification
c
         pass=.false.
         if (IDUP(l,1).eq.-13) pass=.true. !muon

         if (pass)then
            num_amu=num_amu+1
            amu(num_amu)=l
         goto 20 !continue (next identification)
         endif


c
c      tau identification
c
         pass=.false.
         if (IDUP(l,1).eq.15) pass=.true. !muon

         if (pass)then
            num_ta=num_ta+1
            ta(num_ta)=l
         goto 20 !continue (next identification)
         endif
c
c      anti muon identification
c
         pass=.false.
         if (IDUP(l,1).eq.-15) pass=.true. !muon

         if (pass)then
            num_ata=num_ata+1
            ata(num_ata)=l
         goto 20 !continue (next identification)
         endif

c
c        count un-identified particle 
c
	 if(.not.pass)then
	    num_notfind=num_notfind+1
            write(*,*)"Un-identified PID:",abs(IDUP(l,1))
	 endif	 
 20    enddo
 

         write(*,*)'               '
         write(*,*)'* * *  topology identification  * * *'
         write(*,*)' '

 
         write(*,*)'Final state content:'
         write(*,*)'--------------------'
         write(*,*) num_inv,' missing particles'
c         write(*,*) "those are",(neutrino(i),i=1,num_inv)
         write(*,*) num_jet,' light jets'
c         write(*,*) "those are",(jet(i),i=1,num_jet)
         if(.not.perm_with_b) then
            write(*,*) num_bjet,' b jets'           
         endif
         write(*,*) num_e,' electrons'
         write(*,*) num_ae,' positronss'
         write(*,*) num_mu,' muons'
         write(*,*) num_amu,' anti muons'
         write(*,*) num_ta,' taus'
         write(*,*) num_ata,' anti taus'
	 if(num_notfind.ge.1)then
            write(*,*) "!!!!!WARNING!!!!!" 
	    write(*,*) num_notfind,'unknow particle detected'
            write(*,*) "!!!!!WARNING!!!!!" 
         endif

C
C     define the function inv_matching_type_part/matching_type_part
C             ( MG order <--> Permutation order )
C
         tag=3
c jet
         do l=1,nexternal-2
            if (jet(l).ne.0)then
               inv_matching_type_part(tag)=jet(l)
               tag=tag+1
            endif           
         enddo
c bjet
          do l=1,nexternal-2
            if (bjet(l).ne.0)then
               inv_matching_type_part(tag)=bjet(l)
               tag=tag+1
            endif           
         enddo
c elec
         do l=1,nexternal-2
            if (el(l).ne.0)then
               inv_matching_type_part(tag)=el(l)
               tag=tag+1
           endif           
         enddo
c posit
         do l=1,nexternal-2
            if (ael(l).ne.0)then
               inv_matching_type_part(tag)=ael(l)
               tag=tag+1
           endif
         enddo
c muon
         do l=1,nexternal-2
            if (mu(l).ne.0)then
               inv_matching_type_part(tag)=mu(l)
               tag=tag+1
           endif           
         enddo
c anti muon
         do l=1,nexternal-2
            if (amu(l).ne.0)then
               inv_matching_type_part(tag)=amu(l)
               tag=tag+1
           endif
         enddo
c tau
         do l=1,nexternal-2
            if (ta(l).ne.0)then
               inv_matching_type_part(tag)=ta(l)
               tag=tag+1
           endif
         enddo
c anti tau
         do l=1,nexternal-2
            if (ata(l).ne.0)then
               inv_matching_type_part(tag)=ata(l)
               tag=tag+1
           endif
         enddo

c invisible
         do l=1,nexternal-2
            if (neutrino(l).ne.0)then
               inv_matching_type_part(tag)=neutrino(l)
               tag=tag+1
            endif           
         enddo
         do l=3,nexternal
            matching_type_part(inv_matching_type_part(l))=l
         enddo
c
c        write(*,*) "matching_type_part", (matching_type_part(l),l=3,nexternal)
        write(*,*) "inv_matching_type_part", 
     & (inv_matching_type_part(l),l=3,nexternal)



        return
	end
