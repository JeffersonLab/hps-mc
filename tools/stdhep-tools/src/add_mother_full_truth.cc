#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <string.h>
#include <stdhep_util.hh>

#include <unistd.h>


#include <iostream>
using namespace std;

// the tool works for ap and rad MC for fully truth 
// takes input stdhep file and lhe file, adds new particles and makes them mothers of parentless particles, and writes to a new stdhep file
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
		printf("<input stdhep filename> <input lhe filename>  <output stdhep filename>\n");
		return 1;
	}

	int n_events;
	int istream = 0;
	int ostream = 1;

	n_events = open_read(argv[optind],istream);

	open_write(argv[optind+2],ostream,n_events);

        FILE * in_file = fopen(argv[optind+1],"r");

	while (true) {

		////// read event from stdhep file
		if (!read_next(istream)) {
			close_read(istream);
			fclose(in_file);
			close_write(ostream);
			return(0);
		}

		////// build event
		// entry 1: mother particle 1
		// entry 2: mother particle 2
		// other entries: copy of input stdhep event 
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

		////// read event from lhe file
                char line[1000];
                bool found_event = false;
                while (fgets(line,1000,in_file)!=NULL) {
                        if (strstr(line,"<event")!=NULL) {
                                found_event = true;
                                break;
                        }
                }
                if (!found_event) {
			close_read(istream);
                        fclose(in_file);
                        close_write(ostream);
                        return(0);
                }		

		struct stdhep_entry temp3; 
		struct stdhep_entry temp4; 
                int nup;
                fgets(line,1000,in_file);
                sscanf(line,"%d %*d %*f %*f %*f %*f",&nup);
                bool flag_id_611 = false;
                bool flag_id_m611 = false;
                for (int i=0;i<nup;i++) {
                        struct stdhep_entry temp; 
                        fgets(line,1000,in_file);
                        sscanf(line,"%d %d %d %d %*d %*d %lf %lf %lf %lf %lf %*f %*f",&(temp.idhep),&(temp.isthep),&(temp.jmohep[0]),&(temp.jmohep[1]),&(temp.phep[0]),&(temp.phep[1]),&(temp.phep[2]),&(temp.phep[3]),&(temp.phep[4]));

                        // temp.idhep == 611 for electron/pair of rad_mu and ap
                        // temp.idhep == -611 for positron/pair of rad_mu and ap
                        // temp.idhep == 11 for electron/pair of rad
                        // temp.idhep == -11 for positron/pair of rad
                        if(temp.idhep == 611) {
                                temp3 = temp;
                                flag_id_611 = true;
                        }
                        else if(flag_id_611 == false && temp.idhep == 11) temp3 = temp;
			else if(temp.idhep == -611){
                                temp4 = temp;
                                flag_id_m611 = true;
                        }
                        else if(flag_id_m611 == false && temp.idhep == -11) temp4 = temp;
                }
		
		////// Building truth for mother particles
		for (int i=2;i<new_event.size();i++) {
                        if (new_event[i].jmohep[0] == 1 + offset){
                                new_event[i].jmohep[0] = 2;
                                if (new_event[1].jdahep[0] == 0){
                                        new_event[1].jdahep[0] = i+1;
                                        for (int j=0;j<4;j++)
                                                new_event[1].vhep[j] = new_event[i].vhep[j];
                                }
				new_event[1].phep[0] = temp3.phep[0] + temp4.phep[0];
				new_event[1].phep[1] = temp3.phep[1] + temp4.phep[1];
                                new_event[1].phep[2] = temp3.phep[2] + temp4.phep[2];
                                new_event[1].phep[3] = temp3.phep[3] + temp4.phep[3];
                                new_event[1].phep[4] = sqrt(pow(new_event[1].phep[3], 2) - pow(new_event[1].phep[2], 2) - pow(new_event[1].phep[1], 2) - pow(new_event[1].phep[0], 2));

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
		
		delete temp1;
		delete temp2;
	}
}

