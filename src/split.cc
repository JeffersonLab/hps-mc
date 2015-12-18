#include <stdio.h>
#include <stdlib.h>
#include <stdhep_mcfio.h>
#include <stdhep.h>
#include <string.h>

// Reads one stdhep file and splits it into multiple files, each of a given length.
int main(int argc,char** argv)
{
	char outputname[200];
	if (argc!=4) 
	{
		printf("<input stdhep filename> <output stdhep basename> <number of events per file>\n");
		return 1;
	}
	int n_events = atoi(argv[3]);
	printf("Reading %d events from %s\n",n_events,argv[1]);
	int istream = 0;
	int ostream = 1;
	int ilbl;
	StdHepXdrReadInit(argv[1],n_events,istream);
	int j = 1;

	while (true) {
		sprintf(outputname,"%s_%d.stdhep",argv[2],j);
		printf("Writing to %s\n",outputname);
		StdHepXdrWriteOpen(outputname,outputname,n_events,ostream);
		StdHepXdrWrite(100,ostream);

		for (int i=0;i<n_events;i++)
		{
			do {
				if (StdHepXdrRead(&ilbl,istream)!=0) {
					printf("End of file\n");
					printf("Last output file contains %d events\n",n_events);
					StdHepXdrEnd(istream);
					StdHepXdrWrite(200,ostream);
					StdHepXdrEnd(ostream);
					return(0);
				}
				if (ilbl!=1)
					printf("ilbl = %d\n",ilbl);
			} while (ilbl!=1);
			StdHepXdrWrite(ilbl,ostream);
		}
		StdHepXdrWrite(200,ostream);
		StdHepXdrEnd(ostream);
		j++;
	}
}
