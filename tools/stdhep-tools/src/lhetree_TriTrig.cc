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
  int pdgEl;
  int pdgEp, pdgEm;
  
  TLorentzVector vEl; //beam electron
  TLorentzVector vEp, vEm; //outgoing leptons

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
  t->Branch( "pdgEp", &(myevent.pdgEp) );
  t->Branch( "vEp", &(myevent.vEp) );
  t->Branch( "pdgEm", &(myevent.pdgEm) );
  t->Branch( "vEm", &(myevent.vEm) );
  t->Branch( "pdgEl", &(myevent.pdgEl) );
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
      
      if ( ele_count==0 && pdgid == 11 && status==1){ //find beam e-
	myevent.vEl.SetPxPyPzE(px,py,pz,e);
	myevent.pdgEl = pdgid;
	ele_count++;
	
#ifdef DEBUG
	cout << "found beam e- pdg: " << pdgid << "\tpx: " << myevent.vEl.Px() << endl;
#endif
      } else if ( ele_count==1 && pdgid == 11 && status==1){ //find outgoing e-
	myevent.vEm.SetPxPyPzE(px,py,pz,e);
	myevent.pdgEm = pdgid;
#ifdef DEBUG
	cout << "found outgoing e- pdg: " << myevent.pdgEm << "\tpx: " << myevent.vEm.Px() << endl;
#endif
      } else if (pdgid == -11 && status==1){ //find outgoing e+
	myevent.vEp.SetPxPyPzE(px,py,pz,e);
	myevent.pdgEp = pdgid;
#ifdef DEBUG
	cout << "found outgoing e+ pdg: " << myevent.pdgEp << "\tpx: " << myevent.vEp.Px() << endl;
#endif
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
      myevent.vEm.SetPxPyPzE(0.,0.,0.,0.);
      myevent.vEp.SetPxPyPzE(0.,0.,0.,0.);
      myevent.vEl.SetPxPyPzE(0.,0.,0.,0.);
      ele_count=0;
    }
    npart--;
  }
  cout << "fill_count: " << fill_count << endl;
  t->Write();
  f->Close();
  
}
