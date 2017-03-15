c*********************************************************
c                  TF_param.inc                         **
c*********************************************************
        include 'genps.inc'
c
c external functions
c
        double precision pt,eta,phi,theta,rap,rho
	external pt,eta,phi,theta,rap,rho
c
c local
c
	double precision prov1,prov2,prov3,prov4,prov5
	double precision prov6,prov7,prov8,prov9,prov10
c
c LHCO input   
c
      integer tag_init(3:nexternal),type(nexternal-1),run_number,trigger
      double precision eta_init(nexternal-1),phi_init(nexternal-1),
     &pt_init(nexternal-1),j_mass(nexternal-1),ntrk(nexternal-1),
     &btag(nexternal-1),had_em(nexternal-1),dummy1(nexternal-1),
     &dummy2(nexternal-1)
      common/LHCO_input/eta_init,phi_init,pt_init,
     &j_mass,ntrk,btag,had_em,dummy1,dummy2,tag_init,type,run_number,
     &trigger
c
c	MET tag
c
	integer met_lhco,opt_lhco
	common/LHCO_met_tag/met_lhco,opt_lhco
c
c	generated
c
c
c     global
c
      double precision momenta(0:3,-max_branch:2*nexternal)
      double precision mvir2(-max_branch:2*nexternal)    
      common /to_diagram_kin/ momenta, mvir2
c
c parameters to init in transfert_card.dat
c	
