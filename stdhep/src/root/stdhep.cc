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
#include <stdhep_util.hh>


int main(int argc,char** argv)
{
	char outputname[200];
	if (argc<3 || argc>4) 
	{
		printf("<input stdhep filename> <output basename> [number of events]\n");
		return 1;
	}

	stdhep_event new_event;
	int istream = 0;
	int n_events = 0;
	int n_read = 0;

	n_events = open_read(argv[1],istream);

    if (argc==4) n_read = atoi(argv[3]);
	printf("Reading %d events from %s\n",n_read,argv[1]);
	int j;

	strcpy(outputname,argv[2]);
	TFile *f = new TFile(strcat(outputname,".root"),"RECREATE");

	TH1D *e_E = new TH1D("e_E","electron E",500,0.0,2.2);
	TH2D *thetaE = new TH2D("thetaE","E vs. theta",500,0.0,0.2,500,0.0,2.2);
	TH2D *e_thetaE = new TH2D("e_thetaE","electron E vs. theta",500,0.0,0.2,500,0.0,2.2);
	TH2D *p_thetaE = new TH2D("p_thetaE","positron E vs. theta",500,0.0,0.2,500,0.0,2.2);
	TH2D *g_thetaE = new TH2D("g_thetaE","gamma E vs. theta",500,0.0,0.2,500,0.0,2.2);
	TH2D *thetayE = new TH2D("thetayE","E vs. theta_y",500,0.0,0.2,500,0.0,2.2);
	TH2D *e_thetayE = new TH2D("e_thetayE","electron E vs. theta_y",500,0.0,0.2,500,0.0,2.2);
	TH2D *p_thetayE = new TH2D("p_thetayE","positron E vs. theta_y",500,0.0,0.2,500,0.0,2.2);
	TH2D *g_thetayE = new TH2D("g_thetayE","gamma E vs. theta_y",500,0.0,0.2,500,0.0,2.2);
	TH2D *e_ep = new TH2D("e_ep","e(e-) vs. e(e+)",500,0.0,2.2,500,0.0,2.2);
	TH2D *e_ep1 = new TH2D("e_ep1","e(e-1) vs. e(e+)",500,0.0,2.2,500,0.0,2.2);
	TH2D *e_ep2 = new TH2D("e_ep2","e(e-2) vs. e(e+)",500,0.0,2.2,500,0.0,2.2);

	char name[100];
	for (int i=0;n_read==0||i<n_read;i++)
	{
		if (!read_next(istream)) {
			close_read(istream);
			break;
		}
		read_stdhep(&new_event);
		printf("read event %d: nevhep = %d, nhep = %d\n",i+1,new_event.nevhep,new_event.particles.size());

        double ele1=-10;
        double ele2=-10;
        double pos=-10;
		//for (j = 0; j < hepevt_.nhep; j++)
		//{
		for (int j=0;j<new_event.particles.size();j++) {
            if (new_event.particles[j].isthep!=1) continue;
            double *phep = new_event.particles[j].phep;
            double theta = atan2(sqrt(phep[0]*phep[0]+phep[1]*phep[1]),phep[2]);
			double theta_y = atan2(abs(phep[1]),phep[2]);
			thetaE->Fill(theta,phep[3]);
			thetayE->Fill(theta_y,phep[3]);
			switch (new_event.particles[j].idhep)
			{
				case 11:
					e_thetaE->Fill(theta,phep[3]);
					e_thetayE->Fill(theta_y,phep[3]);
					e_E->Fill(phep[3]);
					sprintf(name,"e-");
                    if (phep[3]>ele1) {
                        ele2=ele1;
                        ele1=phep[3];
                    } else if (phep[3]>ele2) {
                        ele2=phep[3];
                    }
					break;
				case -11:
					p_thetaE->Fill(theta,phep[3]);
					p_thetayE->Fill(theta_y,phep[3]);
					sprintf(name,"e+");
                    if (phep[3]>pos) pos=phep[3];
					break;
				case 22:
					g_thetaE->Fill(theta,phep[3]);
					g_thetayE->Fill(theta_y,phep[3]);
					sprintf(name,"g");
					break;
			}
			if (j==0) strcat(name,"r");
			//printf("%s\t%d\t%d\n",name,(int)(phep[3]*1e6),(i+2)*500*20);
		}
        //printf("ele1=\t%f\tele2=\t%f\tpos=\t%f\n",ele1,ele2,pos);
        e_ep->Fill(pos,ele1);
        e_ep1->Fill(pos,ele1);
        e_ep->Fill(pos,ele2);
        e_ep2->Fill(pos,ele2);
		new_event.particles.clear();
	}
    sprintf(name,"e+");
	TCanvas *c1 = new TCanvas("c1", "c1",10,10,1000,750);
	strcpy(outputname,argv[2]);
	c1->Print(strcat(outputname,".pdf["));

	c1->SetLogz();
	thetaE->Draw("COLZ");
	strcpy(outputname,argv[2]);
	c1->Print(strcat(outputname,".pdf"),"Title:thetaE");
	e_thetaE->Draw("COLZ");
	strcpy(outputname,argv[2]);
	c1->Print(strcat(outputname,".pdf"),"Title:e_thetaE");
	p_thetaE->Draw("COLZ");
	strcpy(outputname,argv[2]);
	c1->Print(strcat(outputname,".pdf"),"Title:p_thetaE");
	g_thetaE->Draw("COLZ");
	strcpy(outputname,argv[2]);
	c1->Print(strcat(outputname,".pdf"),"Title:g_thetaE");

    thetayE->Draw("COLZ");
	strcpy(outputname,argv[2]);
	c1->Print(strcat(outputname,".pdf"),"Title:thetayE");
	e_thetayE->Draw("COLZ");
	strcpy(outputname,argv[2]);
	c1->Print(strcat(outputname,".pdf"),"Title:e_thetayE");
	p_thetayE->Draw("COLZ");
	strcpy(outputname,argv[2]);
	c1->Print(strcat(outputname,".pdf"),"Title:p_thetayE");
	g_thetayE->Draw("COLZ");
	strcpy(outputname,argv[2]);
	c1->Print(strcat(outputname,".pdf"),"Title:g_thetayE");

	e_ep->Draw("COLZ");
	strcpy(outputname,argv[2]);
	c1->Print(strcat(outputname,".pdf"),"Title:e_ep");
	e_ep1->Draw("COLZ");
	strcpy(outputname,argv[2]);
	c1->Print(strcat(outputname,".pdf"),"Title:e_ep1");
	e_ep2->Draw("COLZ");
	strcpy(outputname,argv[2]);
	c1->Print(strcat(outputname,".pdf"),"Title:e_ep2");

	strcpy(outputname,argv[2]);
	c1->Print(strcat(outputname,".pdf]"));
	f->Write();
	f->Close();
}
