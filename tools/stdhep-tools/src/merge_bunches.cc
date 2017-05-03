#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <string.h>
#include <stdhep_util.hh>

// takes input stdhep file, merges a fixed number of events, and writes to a new stdhep file
int main(int argc,char** argv)
{
	int nevhep;             /* The event number */
	vector<stdhep_entry> new_event;

	if (argc!=4) 
	{
		printf("<input stdhep filename> <output stdhep filename> <number of events per event>\n");
		return 1;
	}
	int n_events;
	int istream = 0;
	int ostream = 1;

	n_events = open_read(argv[1],istream);

	open_write(argv[2],ostream,n_events);

	int n_merge = atoi(argv[3]);
	printf("Writing %d events per event\n",n_merge);

	nevhep = 0;

	while (true) {
		for (int i=0;i<n_merge;i++)
		{
			if (!read_next(istream)) {
				close_read(istream);
				close_write(ostream);
				return(0);
			}
			read_stdhep(&new_event);
		}

		write_stdhep(&new_event,nevhep+1);
		write_file(ostream);
		nevhep++;
	}
}

