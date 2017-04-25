#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include <stdhep_util.hh>

// takes input stdhep files, merges one event from each file into a single event in a new stdhep file
int main(int argc,char** argv)
{
	vector<stdhep_entry> new_event;

	if (argc<3) 
	{
		printf("<input stdhep filenames> <output stdhep filename>\n");
		return 1;
	}
	int n_events = 500000;

	int n_inputs = argc-2;
	for (int i=0;i<n_inputs;i++) {
		open_read(argv[i+1],i);
	}

	int ostream = n_inputs;
	open_write(argv[argc-1],ostream,n_events);

	int nevhep = 1;
	bool end_of_files = false;

	while (true) {
		for (int i=0;i<n_inputs;i++)
		{
			if (!read_next(i)) {
				printf("End of file %s\n",argv[i+1]);
				close_read(i);
				if (!end_of_files && i!=0) {
					printf("fail; %s has fewer events than %s\n",argv[i+1],argv[1]);
				}
				end_of_files = true;
			}
			else {
				if (end_of_files) {
					printf("fail; %s has too many events\n",argv[i+1]);
				}
				int new_nevhep = read_stdhep(&new_event);
				if (nevhep!=new_nevhep) printf("Expected nevhep = %d, got %d in file %s\n",nevhep,new_nevhep,argv[i+1]);
			}
		}

		if (end_of_files) {
			close_write(ostream);
			return(0);
		}

		write_stdhep(&new_event,nevhep);
		write_file(ostream);
		nevhep++;
	}
}

