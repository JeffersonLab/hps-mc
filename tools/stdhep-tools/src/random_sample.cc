#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <string.h>
#include <stdhep_util.hh>

#include <gsl/gsl_rng.h>
#include <gsl/gsl_randist.h>

#include <unistd.h>

// takes input stdhep files, merges a Poisson-determined number of events per event into a new stdhep file
int main(int argc,char** argv)
{
	int nevhep;             /* The event number */
	vector<stdhep_entry> new_event;

	vector<vector<stdhep_entry> *> input_events;

	double poisson_mu = -1.0;
	int output_n = 500000;
	int max_output_files = 1;
	int output_filename_digits = 1;

	int rseed = 0;

	int c;

	while ((c = getopt(argc,argv,"hn:m:N:s:")) !=-1)
		switch (c)
		{
			case 'h':
				printf("-h: print this help\n");
				printf("-m: mean number of events in an event\n");
				printf("-N: max number of files to write\n");
				printf("-n: output events per output file\n");
				printf("-s: RNG seed\n");
				return(0);
				break;
			case 'm':
				poisson_mu = atof(optarg);
				break;
			case 'n':
				output_n = atoi(optarg);
				break;
			case 'N':
				max_output_files = atoi(optarg);
				output_filename_digits = strlen(optarg);
				break;
			case 's':
				rseed = atoi(optarg);
				break;
			case '?':
				printf("Invalid option or missing option argument; -h to list options\n");
				return(1);
			default:
				abort();
		}

	printf("Mean events per output event %f, output events per output file %d, max output files %d\n",poisson_mu,output_n,max_output_files);

	if ( argc-optind <2 )
	{
		printf("<input stdhep filenames> <output stdhep basename>\n");
		return 1;
	}

	//initialize the RNG
	const gsl_rng_type * T;
	gsl_rng * r;
	gsl_rng_env_setup();

	T = gsl_rng_mt19937;
	r = gsl_rng_alloc (T);
	gsl_rng_set(r,rseed);


	int n_events;
	int istream = 0;
	int ostream = 1;
	int ilbl;


	char *output_basename = argv[argc-1];
	char output_filename[100];
	int file_n = 1;

	open_read(argv[optind++],istream);

	while (true) {
		bool no_more_data = false;
		while (!read_next(istream)) {
			close_read(istream);
			if (optind<argc-1)
			{
				open_read(argv[optind++],istream);
			}
			else
			{
				no_more_data = true;
				break;
			}
		}
		if (no_more_data) break;

		vector<stdhep_entry> * read_event = new vector<stdhep_entry>;
		read_stdhep(read_event);
		input_events.push_back(read_event);
	}

	printf("read %d events\n",input_events.size());

	if (poisson_mu<0) {
		poisson_mu = ((double) input_events.size())/output_n;
		printf("Setting mu to %f\n",poisson_mu);
	}

	while (file_n<=max_output_files) {
		sprintf(output_filename,"%s_%0*d.stdhep",output_basename,output_filename_digits,file_n++);
		open_write(output_filename,ostream,output_n);
		for (int nevhep = 0; nevhep < output_n; nevhep++)
		{
			int n_merge = gsl_ran_poisson(r,poisson_mu);
			if (n_merge==0)
				add_filler_particle(&new_event);
			for (int i=0;i<n_merge;i++)
			{
				int random_index = gsl_rng_uniform_int(r,input_events.size());
				append_stdhep(&new_event,input_events[random_index]);
			}

			write_stdhep(&new_event,nevhep+1);
			write_file(ostream);
		}
		close_write(ostream);
	}
}

