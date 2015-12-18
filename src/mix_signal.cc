#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <string.h>
#include <stdhep_util.hh>

#include <gsl/gsl_rng.h>
#include <gsl/gsl_randist.h>

#include <unistd.h>

// randomly replace at most 1 event in a background file with a selected event from a signal file
int main(int argc,char** argv)
{
	int nevhep;             /* The event number */
	vector<stdhep_entry> new_event;
	vector<stdhep_entry> sig_event;

	double signal_prob = 0.001;
	int n_events = 500000;

	int signal_n = 1;


	int rseed = 0;

	int c;

	while ((c = getopt(argc,argv,"hm:s:n:")) !=-1)
		switch (c)
		{
			case 'h':
				printf("-h: print this help\n");
				printf("-m: probability of a signal event\n");
				printf("-s: RNG seed\n");
				printf("-n: which signal event to use\n");
				return(0);
				break;
			case 'm':
				signal_prob = atof(optarg);
				break;
			case 's':
				rseed = atoi(optarg);
				break;
			case 'n':
				signal_n = atoi(optarg);
				break;
			case '?':
				printf("Invalid option or missing option argument; -h to list options\n");
				return(1);
			default:
				abort();
		}

	printf("Mixing in the %dth signal event, with probability %f\n",signal_n,signal_prob);

	if ( argc-optind != 3 )
	{
		printf("<input background stdhep filename> <input signal stdhep filename> <output stdhep filename>\n");
		return 1;
	}

	//initialize the RNG
	const gsl_rng_type * T;
	gsl_rng * r;
	gsl_rng_env_setup();

	T = gsl_rng_mt19937;
	r = gsl_rng_alloc (T);
	gsl_rng_set(r,rseed);


	int bkgd_stream = 0;
	int sig_stream = 1;
	int ostream = 2;
	int ilbl;


	open_read(argv[optind+1],sig_stream);
	for (int i=0;i<signal_n;i++) {
		if (!read_next(sig_stream)) {
			close_read(sig_stream);
			return(1);
		}
	}
	read_stdhep(&sig_event);
	close_read(sig_stream);

	open_read(argv[optind],bkgd_stream);
	open_write(argv[optind+2],ostream,n_events);
	bool sig_used = false;

	int evcount = 0;
	while (read_next(bkgd_stream)) {
		nevhep = read_stdhep(&new_event);
		if (!sig_used && gsl_rng_uniform(r)<signal_prob) {
			write_stdhep(&sig_event,nevhep);
			sig_used = true;
		} else {
			write_stdhep(&new_event,nevhep);
		}
		evcount++;
		write_file(ostream);
	}
	close_read(bkgd_stream);
	close_write(ostream);
	printf("Counted %d events\n",evcount);
}

