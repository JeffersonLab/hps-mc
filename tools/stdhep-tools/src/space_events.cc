#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <string.h>
#include <stdhep_util.hh>

#include <unistd.h>

// takes input stdhep files, merges a Poisson-determined number of events per event into a new stdhep file
int main(int argc,char** argv)
{
	int nevhep;             /* The event number */
	vector<stdhep_entry> new_event;

	int event_interval = 500;
	int output_n = 500000;
	int max_output_files = 0;
	int output_filename_digits = 2;

	int c;

	while ((c = getopt(argc,argv,"hn:e:N:")) !=-1)
		switch (c)
		{
			case 'h':
				printf("-h: print this help\n");
				printf("-e: interval between events\n");
				printf("-N: max number of files to write\n");
				printf("-n: output events per output file\n");
				return(0);
				break;
			case 'e':
				event_interval = atoi(optarg);
				break;
			case 'n':
				output_n = atoi(optarg);
				break;
			case 'N':
				max_output_files = atoi(optarg);
				output_filename_digits = strlen(optarg);
				break;
			case '?':
				printf("Invalid option or missing option argument; -h to list options\n");
				return(1);
			default:
				abort();
		}

	printf("Interval between nonempty events %d, output events per output file %d, max output files %d\n",event_interval,output_n,max_output_files);

	if ( argc-optind <2 )
	{
		printf("<input stdhep filenames> <output stdhep basename>\n");
		return 1;
	}

	int n_events;
	int istream = 0;
	int ostream = 1;
	int ilbl;


	char *output_basename = argv[argc-1];
	char output_filename[100];
	int file_n = 1;

	int events_written = 0;

	open_read(argv[optind++],istream);
	while (max_output_files==0||file_n-1<max_output_files) {
		sprintf(output_filename,"%s_%0*d.stdhep",output_basename,output_filename_digits,file_n++);
		open_write(output_filename,ostream,output_n);
		for (int nevhep = 0; nevhep < output_n; nevhep++)
		{
			if (events_written%event_interval==0)
			{
				while (!read_next(istream)) {
					close_read(istream);
					if (optind<argc-1)
					{
						open_read(argv[optind++],istream);
					}
					else
					{
						close_write(ostream);
						return(0);
					}
				}
				read_stdhep(&new_event);
			}
			else
			{
				add_filler_particle(&new_event);
			}

			write_stdhep(&new_event,nevhep+1);
			write_file(ostream);
			events_written++;
		}
		close_write(ostream);
	}
}

