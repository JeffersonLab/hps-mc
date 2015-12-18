#include <stdio.h>
#include <stdlib.h>
#include <stdhep_mcfio.h>
#include <stdhep.h>
#include <string.h>


// takes input stdhep file, explodes each event into single-particle events, and writes to a new stdhep file
int main(int argc,char** argv)
{
	int current_nhep;
	int split_nevhep = 0;
	int nevhep;             /* The event number */
	int nhep;               /* The number of entries in this event */
	int isthep[NMXHEP];     /* The Particle id */
	int idhep[NMXHEP];      /* The particle id */
	int jmohep[NMXHEP][2];    /* The position of the mother particle */
	int jdahep[NMXHEP][2];    /* Position of the first daughter... */
	double phep[NMXHEP][5];    /* 4-Momentum, mass */
	double vhep[NMXHEP][4];    /* Vertex information */

	char outputname[200];
	if (argc!=3)
	{
		printf("<input stdhep filename> <output stdhep filename>\n");
		return 1;
	}
	int n_events = 1000000000;
	printf("Reading %d events from %s\n",n_events,argv[1]);
	int istream = 0;
	int ostream = 1;
	int ilbl;
	StdHepXdrReadInit(argv[1],n_events,istream);

	printf("Writing to %s\n",argv[2]);
	StdHepXdrWriteOpen(argv[2],argv[2],n_events,ostream);
	StdHepXdrWrite(100,ostream);

	while (true) {
		do {
			if (StdHepXdrRead(&ilbl,istream)!=0) {
				printf("End of file\n");
				StdHepXdrEnd(istream);

				StdHepXdrWrite(200,ostream);
				StdHepXdrEnd(ostream);
				return(0);
			}
			if (ilbl!=1)
				printf("ilbl = %d\n",ilbl);
		} while (ilbl!=1);

		nevhep = hepevt_.nevhep;
		nhep = hepevt_.nhep;
		for (int i = 0;i<nhep;i++)
		{
			isthep[i] = hepevt_.isthep[i];
			idhep[i] = hepevt_.idhep[i];
			for (int j=0;j<2;j++) jmohep[i][j] = hepevt_.jmohep[i][j];
			for (int j=0;j<2;j++) jdahep[i][j] = hepevt_.jdahep[i][j];
			for (int j=0;j<5;j++) phep[i][j] = hepevt_.phep[i][j];
			for (int j=0;j<4;j++) vhep[i][j] = hepevt_.vhep[i][j];
		}

		hepevt_.nhep = 1;

		for (int i = 0;i<nhep;i++)
		{
			hepevt_.nevhep = split_nevhep++;
			hepevt_.isthep[0] = isthep[i];
			hepevt_.idhep[0] = idhep[i];
			for (int j=0;j<2;j++) hepevt_.jmohep[0][j] = jmohep[i][j];
			for (int j=0;j<2;j++) hepevt_.jdahep[0][j] = jdahep[i][j];
			for (int j=0;j<5;j++) hepevt_.phep[0][j] = phep[i][j];
			for (int j=0;j<4;j++) hepevt_.vhep[0][j] = vhep[i][j];
			StdHepXdrWrite(ilbl,ostream);
		}
	}
}

