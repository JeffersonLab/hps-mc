#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <string.h>
#include <stdhep_util.hh>

#include <gsl/gsl_rng.h>
#include <gsl/gsl_randist.h>

#include <unistd.h>

void rotate_entry(stdhep_entry *entry, double theta_x, double theta_y)
{
	double px = entry->phep[0];
        double py = entry->phep[1];
	double pz = entry->phep[2];
	double vx = entry->vhep[0];
        double vy = entry->vhep[1];
	double vz = entry->vhep[2];

        // rotate about z-axis (rotation in x-y)
        //entry->phep[0] = px*cos(theta_z) - py*sin(theta_z);
        //entry->phep[1] = px*sin(theta_z) + py*cos(theta_z);

        // rotate about x-axis (rotation in y-z)
        entry->phep[1] = py*cos(theta_y) - pz*sin(theta_y);
        entry->phep[2] = py*sin(theta_y) + pz*cos(theta_y);

        // rotate about y-axis (rotation in x-z)
        entry->phep[0] = px*cos(theta_x) + pz*sin(theta_x);
        entry->phep[2] = pz*cos(theta_x) - px*sin(theta_x);        
       
        // rotate vertex
        //entry->vhep[0] = vx*cos(theta_z) - vy*sin(theta_z);
        //entry->vhep[1] = vx*sin(theta_z) + vy*cos(theta_z);

        entry->vhep[1] = vy*cos(theta_y) + vz*sin(theta_y);
        entry->vhep[2] = vy*sin(theta_y) - vz*cos(theta_y);        

        entry->vhep[0] = vx*cos(theta_x) + vz*sin(theta_x);
        entry->vhep[2] = vz*cos(theta_x) - vx*sin(theta_x); 

}

// takes input stdhep file, applies beam rotation and shift to each event, and writes to a new stdhep file
int main(int argc,char** argv)
{
	int nevhep;             /* The event number */
	vector<stdhep_entry> new_event;

	int rseed = 0;

        double theta_x = 0.03017; // -0.33 mrad from Nominal (30.5mr)
        //double theta_x = 0.0305;
        double theta_y = -0.00033;
        double theta_z = 0.2618; // 15 degree beam rotation (2016 harp scans)
	//double sigma_x = 0.300;
	//double sigma_y = 0.030;
        double sigma_x = 0.125;
        double sigma_y = 0.030;
        double target_x = -0.224;
        double target_y = -0.08;
        double target_z = -4.3;

	int c;

	while ((c = getopt(argc,argv,"hs:x:y:r:t:X:Y:Z:")) !=-1)
		switch (c)
		{
			case 'h':
				printf("-h: print this help\n");
				printf("-s: RNG seed\n");
				printf("-x: beam sigma_x in mm\n");
				printf("-y: beam sigma_y in mm\n");
				printf("-r: beam x rotation in radians\n");
                                printf("-t: beam y rotation in radians\n");
                                printf("-X: target X in mm\n");
                                printf("-Y: target Y in mm\n");
                                printf("-Z: target Z in mm\n");
				return(0);
				break;
			case 's':
				rseed = atoi(optarg);
				break;
			case 'r':
				theta_x = atof(optarg);
				break;
                        case 't':
                                theta_y = atof(optarg);
                                break;
			case 'x':
				sigma_x = atof(optarg);
				break;
			case 'y':
				sigma_y = atof(optarg);
				break;
                        case 'X':
                                target_x = atof(optarg);
                                break;
                        case 'Y':
                                target_y = atof(optarg);
                                break;
                        case 'Z':
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

	n_events = open_read(argv[optind],istream);

	open_write(argv[optind+1],ostream,n_events);

	printf("Rotating by %f radians in X, %f radians in Y, and %f radians in Z;\nbeam size %f mm in X and %f mm in Y;\ntarget shifted by (X,Y,Z)=(%f,%f,%f) mm\n",theta_x,theta_y,theta_z, sigma_x,sigma_y, target_x,target_y,target_z);

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
                temp_x = shift_x * cos(theta_z) - shift_y * sin(theta_z);
                temp_y = shift_y * cos(theta_z) + shift_x * sin(theta_z);

                shift_x = temp_x;
                shift_y = temp_y;

		for (int i=0;i<new_event.size();i++) {
			rotate_entry(&(new_event[i]),theta_x,theta_y);
                        new_event[i].vhep[2]+=target_z;
			new_event[i].vhep[0]+=(shift_x + target_x);
			new_event[i].vhep[1]+=(shift_y + target_y);
		}

		write_stdhep(&new_event,nevhep);
		write_file(ostream);
	}
}

