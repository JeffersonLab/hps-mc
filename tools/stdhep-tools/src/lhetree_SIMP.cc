#include <iostream>
#include <iomanip>
#include <fstream>
#include <sstream>
#include <assert.h>
#include "TFile.h"
#include "TTree.h"
#include "TLorentzVector.h"


using namespace std;

class LHEevent {
public:
  LHEevent() { };
  ~LHEevent() { };
  int pdgAp, pdgPiD, pdgRhoD;
  int pdgEpos, pdgEneg;

  TLorentzVector vEl; //beam ele
  TLorentzVector vAp, vRhoD, vPiD; //A', rho_D, pi_D
  TLorentzVector vEpos, vEneg; //pos, ele

  int index_rho = -2;

  unsigned npart;
  double weight;
  double scale;
  double xsec;
  double nevts;
  double nele;
};


bool rewind(std::istringstream *iss, std::string line) { 
  (*iss).clear();
  (*iss).str(line);
  (*iss).seekg(0,ios::beg);
  return true;
}



int main( int argc, char** argv ) { 
  std::cout << "start" << std::endl;
  ifstream ifs(argv[1]);

  int pdgid;
  int status,mo1,mo2,dau1,dau2;
  double px, py, pz, e, m;
  double d1,d2;

  unsigned npart;
  int dunno1;
  double weight, dunno2, dunno3, dunno4;

  TFile * f = new TFile(argv[2], "RECREATE");
  TTree * t = new TTree( "t","t");
  LHEevent myevent;
  myevent.xsec = strtod(argv[3], NULL);
  myevent.nevts = strtod(argv[4], NULL);


  t->Branch( "xsec", &(myevent.xsec) );
  t->Branch( "nevts", &(myevent.nevts) );
  t->Branch( "npart", &(myevent.npart) );
  t->Branch( "weight", &(myevent.weight) );
  t->Branch( "scale", &(myevent.scale) );
  t->Branch( "nele", &(myevent.nele) );
  t->Branch( "pdgAp", &(myevent.pdgAp) );
  t->Branch( "vAp", &(myevent.vAp) );
  t->Branch( "pdgRhoD", &(myevent.pdgRhoD) );
  t->Branch( "vRhoD", &(myevent.vRhoD) );
  t->Branch( "pdgPiD", &(myevent.pdgPiD) );
  t->Branch( "vPiD", &(myevent.vPiD) );
  t->Branch( "pdgEpos", &(myevent.pdgEpos) );
  t->Branch( "vEpos", &(myevent.vEpos) );
  t->Branch( "pdgEneg", &(myevent.pdgEneg) );
  t->Branch( "vEneg", &(myevent.vEneg) );
  t->Branch( "vEl", &(myevent.vEl) );

  int fill_count=0;
  int line_count=0;
  int ele_count=0;
  string line;
  while( !ifs.eof() ) {
    std::getline(ifs,line);
    std::istringstream iss;

    if( rewind(&iss,line) && (iss >> pdgid >> status >> mo1 >> mo2 >> dau1 >> dau2 
				     >> px >> py >> pz >> e  >> m >> d1 >> d2) 
	) {
#ifdef DEBUG
      cout << "index: " << line_count << "\tpdgid: " << pdgid << "\tstatus: " << status 
	   << "\tmo1: " << mo1 << "\tmo2: " << mo2 
	   << "\tpx: " << px << "\tpy: " << py 
      	   << "\te: " << e << "\td1: " << d1 << "\tlc: " << line_count << endl;
      cout.flush();
#endif
      line_count++;
      
      if ( TMath::Abs(pdgid) == 622) { //find A'
	myevent.vAp.SetPxPyPzE(px,py,pz,e);
	myevent.pdgAp = pdgid;
#ifdef DEBUG
	cout << "found Ap " << endl;
#endif
      } else if ( TMath::Abs(pdgid) == 625 ) { //find rho_D
	myevent.vRhoD.SetPxPyPzE(px,py,pz,e);
	myevent.pdgRhoD = pdgid;
	myevent.index_rho = line_count;
#ifdef DEBUG
	cout << "found rho_D, pdg: " << myevent.pdgRhoD << "\tpx: " << myevent.vRhoD.Px() << endl;
#endif
      } else if ( TMath::Abs(pdgid) == 624 ) { //find pi_D (dark pion)
	myevent.vPiD.SetPxPyPzE(px,py,pz,e);
	myevent.pdgPiD = pdgid;
#ifdef DEBUG
	cout << "found pi_D " << endl;
#endif
      } else if ( pdgid == -11 && status==1){ //find e+
	if(mo1 == myevent.index_rho || mo2 == myevent.index_rho){
	  myevent.vEpos.SetPxPyPzE(px,py,pz,e);
	  myevent.pdgEpos = pdgid;
	  ele_count++;
	}
#ifdef DEBUG
	cout << "found Epos pdg: " << myevent.pdgEpos << "\tpx: " << myevent.vEpos.Px() << endl;
#endif
      } else if ( pdgid == 11 && status==1){ //find e-
	if(mo1 == myevent.index_rho || mo2 == myevent.index_rho){//ensure its mother is the rho_D
	  myevent.vEneg.SetPxPyPzE(px,py,pz,e);
	  myevent.pdgEneg = pdgid;
	  ele_count++;
#ifdef DEBUG
	  cout << "found Eneg pdg: " << myevent.pdgEneg << "\tpx: " << myevent.vEneg.Px() << endl;
#endif
	} else {// else it is the beam ele
	  myevent.vEl.SetPxPyPzE(px,py,pz,e);
	  ele_count++;
#ifdef DEBUG
	  cout << "found Ele pdg: " << myevent.pdgEneg << "\tpx: " << myevent.vEneg.Px() << endl;
#endif
	}
      }
    } else if( rewind(&iss,line) && (iss >> npart >> dunno1 >> weight >> dunno2 >> dunno3 >> dunno4)
	       ){
#ifdef DEBUG
      cout << "weight: " << weight << "\tnpart: " << npart << endl;
#endif
      myevent.npart = npart;
      myevent.weight = weight;
      myevent.scale = dunno2;
    } else {
#ifdef DEBUG      
      cout << "------------------------------" << endl;
#endif
      npart=0;
    }

    if( !npart ){
#ifdef DEBUG
      cout << "!npart" << endl;
#endif
      myevent.nele = ele_count;
      
      if( myevent.nele ){
	t->Fill();
	fill_count++;
#ifdef DEBUG
	cout << "filled ..." << fill_count 
	     << endl;
#endif
      }
      
      line_count=0;
      
      myevent.vAp.SetPxPyPzE(0.,0.,0.,0.);
      myevent.vRhoD.SetPxPyPzE(0.,0.,0.,0.);
      myevent.vPiD.SetPxPyPzE(0.,0.,0.,0.);
      myevent.vEneg.SetPxPyPzE(0.,0.,0.,0.);
      myevent.vEpos.SetPxPyPzE(0.,0.,0.,0.);
      myevent.vEl.SetPxPyPzE(0.,0.,0.,0.);
      ele_count=0;
    }
    npart--;
  }
  cout << "fill_count: " << fill_count << endl;
  t->Write();
  f->Close();
  
}
