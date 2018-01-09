#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <string.h>
#include <stdhep_util.hh>

#include <gsl/gsl_rng.h>
#include <gsl/gsl_randist.h>

#include <unistd.h>

void rotate_entry(stdhep_entry *entry, double theta)
{
	double px = entry->phep[0];
	double pz = entry->phep[2];
	double vx = entry->vhep[0];
	double vz = entry->vhep[2];
	entry->phep[0] = px*cos(theta) + pz*sin(theta);
	entry->phep[2] = pz*cos(theta) - px*sin(theta);
	entry->vhep[0] = vx*cos(theta) + vz*sin(theta);
	entry->vhep[2] = vz*cos(theta) - vx*sin(theta);
}

// takes input stdhep file, applies beam rotation and width, and writes to a new stdhep file
int main(int argc,char** argv)
{
	int nevhep;             /* The event number */
	vector<stdhep_entry> new_event;

	int rseed = 0;

	double theta = 0.0305;
	double sigma_x = 0.300;
	double sigma_y = 0.030;
    double target_z = 0.0;

    double skdeg = 15.0;

	int c;

	while ((c = getopt(argc,argv,"hs:x:y:r:z:q:")) !=-1)
		switch (c)
		{
			case 'h':
				printf("-h: print this help\n");
				printf("-s: RNG seed\n");
				printf("-x: beam sigma_x in mm\n");
				printf("-y: beam sigma_y in mm\n");
				printf("-q: beam skew angle in degrees\n");
				printf("-r: beam rotation in radians\n");
				printf("-z: target Z in mm\n");
				return(0);
				break;
			case 's':
				rseed = atoi(optarg);
				break;
			case 'q':
				skdeg = atof(optarg);
				break;
			case 'r':
				theta = atof(optarg);
				break;
			case 'x':
				sigma_x = atof(optarg);
				break;
			case 'y':
				sigma_y = atof(optarg);
				break;
			case 'z':
				target_z = atof(optarg);
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

        double skrad = skdeg * 0.01745;

	n_events = open_read(argv[optind],istream);

	open_write(argv[optind+1],ostream,n_events);

	printf("Rotating by %f radians; beam size %f mm in X, %f mm in Y, target at Z=%f mm\n",theta, sigma_x, sigma_y, target_z);

	while (true) {
		if (!read_next(istream)) {
			close_read(istream);
			close_write(ostream);
			return(0);
		}
		nevhep = read_stdhep(&new_event);

		double shift_x = 0.0, shift_y = 0.0;
		if (sigma_x>0) shift_x = sigma_x*gsl_ran_gaussian(r,sigma_x);
		if (sigma_y>0) shift_y = sigma_y*gsl_ran_gaussian(r,sigma_y);

		double temp_x, temp_y;
		temp_x = shift_x * cos(skrad) - shift_y * sin(skrad);
		temp_y = shift_y * cos(skrad) + shift_x * sin(skrad);

		shift_x = temp_x;
		shift_y = temp_y;

		for (int i=0;i<new_event.size();i++) {
                        rotate_entry(&(new_event[i]),theta);
			new_event[i].vhep[2]+=target_z;
			new_event[i].vhep[0]+=shift_x;
			new_event[i].vhep[1]+=shift_y;
		}

		write_stdhep(&new_event,nevhep);
		write_file(ostream);
	}
}

