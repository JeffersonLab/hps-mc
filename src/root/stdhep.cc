#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <stdhep_mcfio.h>
#include <stdhep.h>
#include <TFile.h>
#include <TH2D.h>
#include <TH1D.h>
#include <TCanvas.h>
#include <string.h>


int main(int argc,char** argv)
{
	char outputname[200];
	if (argc<3 || argc>4) 
	{
		printf("<input stdhep filename> <output ROOT filename> [number of events]\n");
		return 1;
	}


	int n_events = 0;
    if (argc==4) n_events = atoi(argv[3]);
	printf("Reading %d events from %s\n",n_events,argv[1]);
	int istream,ilbl;
	istream=0;
	int i,j;
	StdHepXdrReadInit(argv[1],n_events,istream);
	StdHepXdrRead(&ilbl,istream);

	TFile *f = new TFile(argv[2],"RECREATE");

	TH2D *thetaE = new TH2D("thetaE","E vs. theta",500,0.0,0.2,500,0.0,2.2);
	TH2D *e_thetaE = new TH2D("e_thetaE","electron E vs. theta",500,0.0,0.2,500,0.0,2.2);
	TH1D *e_E = new TH1D("e_E","electron E",500,0.0,2.2);
	TH2D *p_thetaE = new TH2D("p_thetaE","positron E vs. theta",500,0.0,0.2,500,0.0,2.2);
	TH2D *g_thetaE = new TH2D("g_thetaE","gamma E vs. theta",500,0.0,0.2,500,0.0,2.2);

	double theta;
	double *phep;
	char name[100];
	for (i=0;n_events==0||i<n_events;i++)
	{
		do {
			if (StdHepXdrRead(&ilbl,istream)!=0) {
				printf("End of file: ilbl = %d\n",ilbl);
                break;
			}
			if (ilbl!=1)
				printf("ilbl = %d\n",ilbl);
		} while (ilbl!=1);
        if (ilbl!=1) break;
		printf("Event %d\n",hepevt_.nevhep);
		for (j = 0; j < hepevt_.nhep; j++)
		{
			phep = hepevt_.phep[j];
			theta = atan2(sqrt(phep[0]*phep[0]+phep[1]*phep[1]),phep[2]);
			thetaE->Fill(theta,phep[3]);
			switch (hepevt_.idhep[j])
			{
				case 11:
					e_thetaE->Fill(theta,phep[3]);
					e_E->Fill(phep[3]);
					sprintf(name,"e-");
					break;
				case -11:
					p_thetaE->Fill(theta,phep[3]);
					sprintf(name,"e+");
					break;
				case 22:
					g_thetaE->Fill(theta,phep[3]);
					sprintf(name,"g");
					break;
			}
			if (j==0) strcat(name,"r");
			printf("%s\t%d\t%d\n",name,(int)(phep[3]*1e6),(i+2)*500*20);
		}
	}
	TCanvas *c1 = new TCanvas("c1", "c1",10,10,1000,750);
	c1->SetLogz();
	thetaE->Draw("COLZ");
	strcpy(outputname,argv[2]);
	c1->SaveAs(strcat(outputname,"_thetaE.png"));
	e_thetaE->Draw("COLZ");
	strcpy(outputname,argv[2]);
	c1->SaveAs(strcat(outputname,"e_thetaE.png"));
	p_thetaE->Draw("COLZ");
	strcpy(outputname,argv[2]);
	c1->SaveAs(strcat(outputname,"p_thetaE.png"));
	g_thetaE->Draw("COLZ");
	strcpy(outputname,argv[2]);
	c1->SaveAs(strcat(outputname,"g_thetaE.png"));

	f->Write();
	f->Close();
}
