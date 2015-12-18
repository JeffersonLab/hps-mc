#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <string.h>
#include <stdhep_util.hh>

#include <unistd.h>

// takes input stdhep file, adds a new particle and makes it the mother of all parentless particles, and writes to a new stdhep file
int main(int argc,char** argv)
{
	int nevhep;             /* The event number */
	vector<stdhep_entry> new_event;

	int id = 622;

	int c;

	while ((c = getopt(argc,argv,"hi:")) !=-1)
		switch (c)
		{
			case 'h':
				printf("-h: print this help\n");
				printf("-i: PDG ID of mother\n");
				return(0);
				break;
			case 'i':
				id = atoi(optarg);
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

	int n_events;
	int istream = 0;
	int ostream = 1;

	n_events = open_read(argv[optind],istream);

	open_write(argv[optind+1],ostream,n_events);

	while (true) {
		if (!read_next(istream)) {
			close_read(istream);
			close_write(ostream);
			return(0);
		}

		struct stdhep_entry *temp = new struct stdhep_entry;
		temp->isthep = 3; //documentation particle
		temp->idhep = id;
		for (int j=0;j<2;j++) temp->jmohep[j] = 0;
		for (int j=0;j<2;j++) temp->jdahep[j] = 0;
		for (int j=0;j<5;j++) temp->phep[j] = 0.0;
		temp->phep[2]+=0.1;
		temp->phep[3]+=0.1;
		for (int j=0;j<4;j++) temp->vhep[j] = 0.0;
		new_event.push_back(*temp);

		nevhep = read_stdhep(&new_event);

		for (int i=1;i<new_event.size();i++) {
			if (new_event[i].jmohep[0]==0 && new_event[i].jmohep[1]==0) {
				new_event[i].jmohep[0] = 1;
				new_event[i].jmohep[1] = 1;
				if (new_event[0].jdahep[0]==0) new_event[0].jdahep[0] = i+1;
				new_event[0].jdahep[1] = i+1;
			}
		}

		write_stdhep(&new_event,nevhep);
		write_file(ostream);
		nevhep++;
	}
}

