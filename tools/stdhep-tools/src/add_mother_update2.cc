#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <string.h>
#include <stdhep_util.hh>

#include <unistd.h>

#include <TFile.h>
#include <TTree.h>
#include <TLorentzVector.h>

#include <iostream>
using namespace std;

// takes input stdhep file, adds a new particle and makes it the mother of all parentless particles, and writes to a new stdhep file
int main(int argc,char** argv)
{
	int nevhep;             /* The event number */
	vector<stdhep_entry> new_event;

	int id_beam = 623;
	int id_pair = 622;

	double mass = 0.1;
	double energy = 0.1;

	int c;

	while ((c = getopt(argc,argv,"hi1:i2:")) !=-1)
		switch (c)
		{
			case 'h':
				printf("-h: print this help\n");
				printf("-i: PDG ID of mother\n");
				return(0);
				break;
			case 'i1':
				id_beam = atoi(optarg);
				break;
                        case 'i2':
                                id_pair = atoi(optarg);
                                break;
			case '?':
				printf("Invalid option or missing option argument; -h to list options\n");
				return(1);
			default:
				abort();
		}

	if ( argc-optind < 3 )
	{
		printf("<input stdhep filename> <input root filename>  <output stdhep filename>\n");
		return 1;
	}

	int n_events;
	int istream = 0;
	int ostream = 1;

	n_events = open_read(argv[optind],istream);

	open_write(argv[optind+2],ostream,n_events);

        TFile *rootFile = new TFile(argv[optind+1]);
        TTree *tree = (TTree*)rootFile->Get("t");
        TLorentzVector *em= NULL;
        TLorentzVector *ep=NULL;
        TLorentzVector *el=NULL;
        tree->SetBranchAddress("vEl", &el);
        tree->SetBranchAddress("vEm", &em);
        tree->SetBranchAddress("vEp", &ep);
	int numofevents = 0;

	while (true) {
		if (!read_next(istream)) {
			close_read(istream);
			close_write(ostream);
			return(0);
		}

		struct stdhep_entry *temp1 = new struct stdhep_entry;
		temp1->isthep = 3; //documentation particle
		temp1->idhep = id_beam;
		for (int j=0;j<2;j++) temp1->jmohep[j] = 0;
		for (int j=0;j<2;j++) temp1->jdahep[j] = 0;
		for (int j=0;j<5;j++) temp1->phep[j] = 0.0;
		temp1->phep[3]+= energy;
		temp1->phep[4]+= mass;
		for (int j=0;j<4;j++) temp1->vhep[j] = 0.0;
		new_event.push_back(*temp1);

                struct stdhep_entry *temp2 = new struct stdhep_entry;
                temp2->isthep = 2; //documentation particle
                temp2->idhep = id_pair;
                for (int j=0;j<2;j++) temp2->jmohep[j] = 0;
                for (int j=0;j<2;j++) temp2->jdahep[j] = 0;
                for (int j=0;j<5;j++) temp2->phep[j] = 0.0;
                temp2->phep[3]+= energy;
                temp2->phep[4]+= mass;
                for (int j=0;j<4;j++) temp2->vhep[j] = 0.0;
                new_event.push_back(*temp2);

		nevhep = read_stdhep(&new_event);
		int offset = 2;

		
               	tree->GetEntry(numofevents);

		for (int i=2;i<new_event.size();i++) {
                        if (new_event[i].jmohep[0] == 1 + offset){
                                new_event[i].jmohep[0] = 2;
                                if (new_event[1].jdahep[0] == 0){
                                        new_event[1].jdahep[0] = i+1;
                                        for (int j=0;j<4;j++)
                                                new_event[1].vhep[j] = new_event[i].vhep[j];
                                        }
				TLorentzVector vector_pair = *ep + *em;
				new_event[1].phep[0] = vector_pair.Px();
				new_event[1].phep[1] = vector_pair.Py();
                                new_event[1].phep[2] = vector_pair.Pz();
                                new_event[1].phep[3] = vector_pair.E();
                                new_event[1].phep[4] = vector_pair.M();

                                new_event[1].jdahep[1] = i+1;
                        }
			else if (new_event[i].jmohep[0] == 0){ 
				new_event[i].jmohep[0] = 1;
				if (new_event[0].jdahep[0]==0) new_event[0].jdahep[0] = i+1;
				new_event[0].jdahep[1] = i+1;
			}
		}

		write_stdhep(&new_event,nevhep);
		write_file(ostream);
		nevhep++;
		
		numofevents++;
		delete temp1;
		delete temp2;
	}
}

