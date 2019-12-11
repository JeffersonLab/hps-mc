#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <string.h>
#include <stdhep_util.hh>

#include <gsl/gsl_rng.h>
#include <gsl/gsl_randist.h>

#include <unistd.h>

#include <random>       // std::default_random_engine
#include <algorithm>    // std::shuffle
#include <iostream>
using namespace std;

// takes input stdhep files, merges a Poisson-determined number of events per event into a new stdhep file
int main(int argc,char** argv)
{
	double poisson_mu = -1.0;
        double poisson_mu_correction = 0; // correction parameter for mu of Poisson
	int output_n = 500000;
	int max_output_files = 1;
	int output_filename_digits = 1;
        int n_events_per_batch = 1000000;

	int rseed = 0;

	int c;

	while ((c = getopt(argc,argv,"hn:m:t:N:b:s:")) !=-1)
		switch (c)
		{
			case 'h':
				printf("-h: print this help\n");
				printf("-m: mean number of events in an event\n");
                                printf("-t: corretion parameter for mu\n");
				printf("-N: max number of files to write\n");
				printf("-b: number of events per batch for caching input events\n");
                                printf("-n: o\n");
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


	int istream = 0;
	int ostream = 1;
	int ilbl;


	char *output_basename = argv[argc-1];
	char output_filename[100];
	int file_n = 1;

        int n_events=0;
        int input_ind=optind;
        while(input_ind<argc-1){
                n_events += open_read(argv[input_ind++],istream);
                close_read(istream);
        }
        printf("read %d events\n",n_events);

        if (poisson_mu<0) {
                poisson_mu = ((double) n_events)/output_n*(1-poisson_mu_correction);
                printf("Setting mu to %f\n",poisson_mu);
        }

        
        int n_batches = n_events/n_events_per_batch + 1;
	printf("Number of batches for caching: %d\n", n_batches);
	
        vector<stdhep_entry> new_event;
        vector<vector<stdhep_entry> *> input_events;
	vector<int> event_list;
	int batch_num = 0;
	int n_events_used = 0;
	int event_num = 0;
	int nevhep = 0;
	vector<vector<stdhep_entry> *> events_no_used;

        while (file_n<=max_output_files) {
                sprintf(output_filename,"%s_%0*d.stdhep",output_basename,output_filename_digits,file_n++);
                open_write(output_filename,ostream,output_n);
		nevhep = 0; 
		input_ind = optind;
      	  	open_read(argv[input_ind++],istream);
	        while (true) {
        	        bool no_more_data = false;
                	while (!read_next(istream)) {
                       		close_read(istream);
				if (input_ind<argc-1)
					open_read(argv[input_ind++],istream);
				else
				{
					no_more_data = true;
					break;
                        	}
                	}
			if(!no_more_data){
				vector<stdhep_entry> *read_event = new vector<stdhep_entry>;
				read_stdhep(read_event);
                		input_events.push_back(read_event);
				event_list.push_back(event_num++);
			}
			if( ((input_events.size() == n_events_per_batch) && (batch_num < n_batches - 1)) || (no_more_data && (batch_num == n_batches - 1)) ){
				batch_num++;
				shuffle(event_list.begin(), event_list.end(), default_random_engine(rseed + 10));
				int event_list_index = 0;
				while (nevhep < output_n){
					int n_merge = gsl_ran_poisson(r,poisson_mu);
					if (n_merge==0)
						add_filler_particle(&new_event);
					else{
						if(event_list_index + n_merge > event_list.size()){
							if(batch_num == n_batches){
								for(int i = 0; i < input_events.size();i++)
									delete input_events[i];
								input_events.clear();
								event_list.clear();
							}
							else{
								n_events_used += event_list_index;
                                        	                for(int i = 0; i<event_list_index;i++)
                                                	                delete input_events[event_list[i]];
								events_no_used.clear();
								for(int i = event_list_index; i < event_list.size(); i++)
									events_no_used.push_back(input_events[event_list[i]]);
								input_events.clear();
								event_list.clear();
								event_num = 0;
								for(int i = 0; i< events_no_used.size(); i++){
									input_events.push_back(events_no_used[i]);
									event_list.push_back(event_num++);
								}
							}
							break;
						}
						else{
							for (int i=0;i<n_merge;i++)
								append_stdhep(&new_event, input_events[event_list[event_list_index + i]]);		

							event_list_index += n_merge;							
						}
					}
					write_stdhep(&new_event,++nevhep);
					write_file(ostream);
				}
				if(nevhep == output_n){
					close_write(ostream);
					break;
				}
			} 
	                if (no_more_data) break;
		}
		close_write(ostream);
	}
}

