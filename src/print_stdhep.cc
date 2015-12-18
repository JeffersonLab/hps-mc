#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <string.h>
#include <stdhep_util.hh>

// takes input stdhep file, merges a fixed number of events, and writes to a new stdhep file
int main(int argc,char** argv)
{
	stdhep_event new_event;
	int istream = 0;

	if (argc<2 || argc>3)
	{
		printf("<input stdhep filename> [number of events to read]\n");
		return 1;
	}

	int n_events = 0;
	int n_read = 0;

	n_events = open_read(argv[1],istream);

	if (argc==3)
	{
		n_read = atoi(argv[2]);
	}

	for (int i=0;n_read==0||i<n_read;i++)
	{
		if (!read_next(istream)) {
			close_read(istream);
			return(0);
		}

		read_stdhep(&new_event);

		printf("read event %d: nevhep = %d, nhep = %d\n",i+1,new_event.nevhep,new_event.particles.size());
        if (new_event.has_hepev4)
        {
            printf("HEPEV4 information: idruplh = %d, eventweightlh = %E\n",new_event.idruplh,new_event.eventweightlh);
        }
		for (int j=0;j<new_event.particles.size();j++) {
			print_entry(new_event.particles[j]);
		}
		new_event.particles.clear();
	}
}

