#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <string.h>
#include <stdhep_util.hh>

#include <gsl/gsl_rng.h>
#include <gsl/gsl_randist.h>

#include <unistd.h>

void remove_nulls(vector<stdhep_entry> *event)
{
	int i=0;
	while (i<event->size()) {
		if (event->at(i).isthep==0) {
			event->erase(event->begin()+i);
			for (int j=0;j<event->size();j++) {
				if (event->at(j).jmohep[0]>i)
					event->at(j).jmohep[0]--;
				if (event->at(j).jmohep[1]>i)
					event->at(j).jmohep[1]--;
				if (event->at(j).jdahep[0]>i)
					event->at(j).jdahep[0]--;
				if (event->at(j).jdahep[1]>i)
					event->at(j).jdahep[1]--;
			}
		}
		i++;
	}
}

void set_jdahep(vector<stdhep_entry> *event)
{
	for (int i=0;i<event->size();i++) {
		event->at(i).jdahep[0] = 0;
		event->at(i).jdahep[1] = 0;
		for (int j=0;j<event->size();j++) {
			if (event->at(j).jmohep[0]==i+1 || event->at(j).jmohep[1]==i+1) {
				if (event->at(i).jdahep[0] == 0)
					event->at(i).jdahep[0] = j+1;
				event->at(i).jdahep[1] = j+1;
			}
		}
	}
}

void displace_vertex(vector<stdhep_entry> *event, gsl_rng *r, double decay_length)
{
	int ap_id = -1;
	double vx[4];
	for (int i=0;i<event->size();i++) {
		if (event->at(i).idhep==622) {
			if (ap_id!=-1) {
				printf("multiple A' found\n");
				break;
			}
			ap_id = i;
			double gamma = event->at(i).phep[3]/event->at(i).phep[4];
            double beta = sqrt(1-pow(gamma,-2.0));
			double p = 0.0;
			for (int j=0;j<3;j++) p += event->at(i).phep[j]*event->at(i).phep[j];
			p = sqrt(p);
			double proper_time = gsl_ran_exponential(r,decay_length);
			double distance = gamma*beta*proper_time;
			for (int j=0;j<3;j++) vx[j] = distance * event->at(i).phep[j]/p;
            vx[3] = gamma*proper_time;
			//printf("gamma=%f p=%f distance=%f vx=%f,%f,%f\n",gamma,p,distance,vx[0],vx[1],vx[2]);
		}
	}
	if (ap_id<0) printf("no A' found\n");
	else for (int i=0;i<event->size();i++) {
		if (event->at(i).jmohep[0]==ap_id+1 || event->at(i).jmohep[1]==ap_id+1) {
			for (int j=0;j<4;j++) event->at(i).vhep[j] = vx[j];
		}
	}
}

// takes input stdhep file, applies beam rotation and width, and writes to a new stdhep file
int main(int argc,char** argv)
{
	int nevhep;             /* The event number */
	stdhep_event new_event;

	int rseed = 0;

	double decay_length = -1.0;

	int c;

	while ((c = getopt(argc,argv,"hs:l:")) !=-1)
		switch (c)
		{
			case 'h':
				printf("-h: print this help\n");
				printf("-s: RNG seed\n");
				printf("-l: A' decay length in mm\n");
				return(0);
				break;
			case 's':
				rseed = atoi(optarg);
				break;
			case 'l':
				decay_length = atof(optarg);
				break;
			case '?':
				printf("Invalid option or missing option argument; -h to list options\n");
				return(1);
			default:
				abort();
		}

	if ( argc-optind < 2 )
	{
		printf("<input stdhep filename> <output stdhep filename>\n");
		return 1;
	}

	FILE * in_file;

	in_file = fopen(argv[optind],"r");

	//initialize the RNG
	const gsl_rng_type * T;
	gsl_rng * r;
	gsl_rng_env_setup();

	T = gsl_rng_mt19937;
	r = gsl_rng_alloc (T);
	gsl_rng_set(r,rseed);

	int ostream = 0;

	open_write(argv[optind+1],ostream,1000);
	nevhep = 1;

	printf("Applying decay length of %f mm\n",decay_length>0?decay_length:0.0);

	while (true) {
		char line[1000];
		bool found_event = false;
		while (fgets(line,1000,in_file)!=NULL) {
			if (strstr(line,"<event")!=NULL) {
				found_event = true;
				break;
			}
		}
		if (!found_event) {
			fclose(in_file);
			close_write(ostream);
			return(0);
		}

		int nup;

		fgets(line,1000,in_file);
		sscanf(line,"%d %d %lf %*f %*f %*f",&nup,&new_event.idruplh,&new_event.eventweightlh);

		for (int i=0;i<nup;i++) {
			struct stdhep_entry *temp = new struct stdhep_entry;
			fgets(line,1000,in_file);
			//int icolup0,icolup1;
			//double phep0 = 100.0;
			char blah[1000];
			sscanf(line,"%d %d %d %d %*d %*d %lf %lf %lf %lf %lf %*f %*f",&(temp->idhep),&(temp->isthep),&(temp->jmohep[0]),&(temp->jmohep[1]),&(temp->phep[0]),&(temp->phep[1]),&(temp->phep[2]),&(temp->phep[3]),&(temp->phep[4]));
			//int status = sscanf(line,"%d %d %d %d %*d %*d %s",&(temp->idhep),&istup,&(temp->jmohep[0]),&(temp->jmohep[1]),blah);
			switch (temp->isthep) {
				case 1:
				case 2:
					break;
				case -1:
					temp->isthep = 3;
					break;
				default:
					temp->isthep = 0;
			}
			switch (temp->idhep) {
				case 611:
					temp->idhep = 11;
					break;
				case -611:
					temp->idhep = -11;
					break;
			}
			for (int j=0;j<4;j++) temp->vhep[j] = 0.0;
			for (int j=0;j<2;j++) temp->jdahep[j] = 0;
			new_event.particles.push_back(*temp);
		}
		remove_nulls(&new_event.particles);
		set_jdahep(&new_event.particles);
		if (decay_length>0) displace_vertex(&new_event.particles,r,decay_length);
        new_event.nevhep = nevhep;
        new_event.has_hepev4 = true;
		write_stdhep(&new_event);
		write_file(ostream);
		nevhep++;
	}
}

