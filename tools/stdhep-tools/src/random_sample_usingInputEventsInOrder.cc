#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <string.h>
#include <stdhep_util.hh>

#include <gsl/gsl_rng.h>
#include <gsl/gsl_randist.h>

#include <unistd.h>
#include <iostream>
using namespace std;

// takes input stdhep files, merges a Poisson-determined number of events per event into a new stdhep file
int main(int argc,char** argv)
{
	double poisson_mu = -1; // # of electrons per bunch
        double poisson_mu_correction = 0; // correction parameter for mu of Poisson
	int output_n = 2500000;
	int output_filename_digits = 1;

	int rseed = 0;

	int c;

	while ((c = getopt(argc,argv,"hn:m:t:s:")) !=-1)
		switch (c)
		{
			case 'h':
				printf("-h: print this help\n");
				printf("-m: mean number of events in an event\n");
                                printf("-t: corretion parameter for mu\n");
				printf("-n: output events per output file\n");
				printf("-s: RNG seed\n");
				return(0);
				break;
			case 'm':
				poisson_mu = atof(optarg);
				break;
                        case 't':
                                poisson_mu_correction = atof(optarg);
                                break;
			case 'n':
				output_n = atoi(optarg);
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

	printf("Mean events per output event %f, output events per output file %d\n",poisson_mu,output_n);

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


	int istream = 0;
	int ostream = 1;
	int ilbl;


	char *output_basename = argv[argc-1];
	char output_filename[100];

        int n_events=0;
        int mark_optind=optind;
	while(optind<argc-1){
		n_events += open_read(argv[optind++],istream);
                close_read(istream);
        }
        printf("read %d events\n",n_events);

        if (poisson_mu<0) {
                poisson_mu = ((double) n_events)/output_n*(1-poisson_mu_correction);
                printf("Setting mu to %f\n",poisson_mu);
        }

        vector<stdhep_entry> new_event;
        open_read(argv[mark_optind++],istream);
	sprintf(output_filename,"%s_%0*d.stdhep",output_basename,output_filename_digits, 1);
	open_write(output_filename,ostream,output_n);
	for (int nevhep = 0; nevhep < output_n; nevhep++)
	{
		int n_merge = gsl_ran_poisson(r,poisson_mu);
		if (n_merge==0)
			add_filler_particle(&new_event);

                for (int i=0;i<n_merge;i++)
                {
                        while (!read_next(istream)) {
                                close_read(istream);
                                if (mark_optind<argc-1)
                                {
                                        open_read(argv[mark_optind++],istream);
                                }
                                else
                                {
                                        printf("Out of input events\n");
                                        close_write(ostream);
                                        return(0);
                                }
                        }

                        read_stdhep(&new_event);
                }

		write_stdhep(&new_event,nevhep+1);
		write_file(ostream);
	}
	close_write(ostream);
}

