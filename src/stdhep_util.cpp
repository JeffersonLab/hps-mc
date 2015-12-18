#include <stdhep_util.hh>
#include <stdhep_mcfio.h>
#include <math.h>
#include <stdhep.h>
#include <hepev4.h>
#include <stdio.h>

//hepevt hepevt_;
bool xdr_init_done = false;
bool has_hepev4 = false;

int read_stdhep(vector<stdhep_entry> *new_event)
{
	int offset = new_event->size();
	for (int i = 0;i<hepevt_.nhep;i++)
	{
		struct stdhep_entry *temp = new struct stdhep_entry;
		temp->isthep = hepevt_.isthep[i];
		temp->idhep = hepevt_.idhep[i];
		for (int j=0;j<2;j++) {
			temp->jmohep[j] = hepevt_.jmohep[i][j];
			if (temp->jmohep[j]!=0) temp->jmohep[j]+=offset;
		}
		for (int j=0;j<2;j++) {
			temp->jdahep[j] = hepevt_.jdahep[i][j];
			if (temp->jdahep[j]!=0) temp->jdahep[j]+=offset;
		}
		for (int j=0;j<5;j++) temp->phep[j] = hepevt_.phep[i][j];
		for (int j=0;j<4;j++) temp->vhep[j] = hepevt_.vhep[i][j];
		new_event->push_back(*temp);
	}
	return hepevt_.nevhep;
}

void read_stdhep(stdhep_event *new_event)
{
    new_event->nevhep = hepevt_.nevhep;
	int offset = new_event->particles.size();
	for (int i = 0;i<hepevt_.nhep;i++)
	{
		struct stdhep_entry *temp = new struct stdhep_entry;
		temp->isthep = hepevt_.isthep[i];
		temp->idhep = hepevt_.idhep[i];
		for (int j=0;j<2;j++) {
			temp->jmohep[j] = hepevt_.jmohep[i][j];
			if (temp->jmohep[j]!=0) temp->jmohep[j]+=offset;
		}
		for (int j=0;j<2;j++) {
			temp->jdahep[j] = hepevt_.jdahep[i][j];
			if (temp->jdahep[j]!=0) temp->jdahep[j]+=offset;
		}
		for (int j=0;j<5;j++) temp->phep[j] = hepevt_.phep[i][j];
		for (int j=0;j<4;j++) temp->vhep[j] = hepevt_.vhep[i][j];
		new_event->particles.push_back(*temp);
	}
    if (has_hepev4)
    {
        new_event->has_hepev4 = true;
        new_event->idruplh = hepev4_.idruplh;
        new_event->eventweightlh = hepev4_.eventweightlh;
    }
    else new_event->has_hepev4 = false;
}

void write_stdhep(vector<stdhep_entry> *new_event, int nevhep)
{
	hepevt_.nhep = new_event->size();
	hepevt_.nevhep = nevhep;
	//vector<stdhep_entry>::iterator it;
	for (int i = 0; i<new_event->size(); i++)
	{
		struct stdhep_entry temp = new_event->at(i);
		hepevt_.isthep[i] = temp.isthep;
		hepevt_.idhep[i] = temp.idhep;
		for (int j=0;j<2;j++) hepevt_.jmohep[i][j] = temp.jmohep[j];
		for (int j=0;j<2;j++) hepevt_.jdahep[i][j] = temp.jdahep[j];
		for (int j=0;j<5;j++) hepevt_.phep[i][j] = temp.phep[j];
		for (int j=0;j<4;j++) hepevt_.vhep[i][j] = temp.vhep[j];
	}
    has_hepev4 = false;
	new_event->clear();
}

void write_stdhep(stdhep_event *new_event)
{
	hepevt_.nhep = new_event->particles.size();
	hepevt_.nevhep = new_event->nevhep;
	//vector<stdhep_entry>::iterator it;
	for (int i = 0; i<new_event->particles.size(); i++)
	{
		struct stdhep_entry temp = new_event->particles.at(i);
		hepevt_.isthep[i] = temp.isthep;
		hepevt_.idhep[i] = temp.idhep;
		for (int j=0;j<2;j++) hepevt_.jmohep[i][j] = temp.jmohep[j];
		for (int j=0;j<2;j++) hepevt_.jdahep[i][j] = temp.jdahep[j];
		for (int j=0;j<5;j++) hepevt_.phep[i][j] = temp.phep[j];
		for (int j=0;j<4;j++) hepevt_.vhep[i][j] = temp.vhep[j];
	}
    if (new_event->has_hepev4)
    {
        hepev4_.idruplh = new_event->idruplh;
        hepev4_.eventweightlh = new_event->eventweightlh;
        hepev4_.alphaqedlh = 0;
        hepev4_.alphaqcdlh = 0;
        for (int i=0; i<10; i++) hepev4_.scalelh[i] = 0;
        for (int i = 0; i<new_event->particles.size(); i++)
        {
            for (int j=0;j<3;j++) hepev4_.spinlh[i][j] = 0.0;
            for (int j=0;j<3;j++) hepev4_.icolorflowlh[i][j] = 0;
        }
        has_hepev4 = true;
    }
	new_event->particles.clear();
}

void add_filler_particle(vector<stdhep_entry> *new_event) //add a 10 MeV photon in the beam direction
{
	struct stdhep_entry *temp = new struct stdhep_entry;
	temp->isthep = 0; //stable particle
	temp->idhep = 22; //photon
	for (int j=0;j<2;j++) temp->jmohep[j] = 0;
	for (int j=0;j<2;j++) temp->jdahep[j] = 0;
	for (int j=0;j<5;j++) temp->phep[j] = 0.0;
	temp->phep[0]+=0.1*sin(0.0305); //30.5 mrad in +x direction
	temp->phep[2]+=0.1*cos(0.0305);
	temp->phep[3]+=0.1;
	for (int j=0;j<4;j++) temp->vhep[j] = 0.0;
	temp->vhep[2]+=0.1; //0.1 mm after target
	new_event->push_back(*temp);
}

int append_stdhep(vector<stdhep_entry> *event, const vector<stdhep_entry> *new_event)
{
	int offset = event->size();
	for (int i = 0;i<new_event->size();i++)
	{
		struct stdhep_entry temp = (*new_event)[i];
		for (int j=0;j<2;j++) {
			if (temp.jmohep[j]!=0) temp.jmohep[j]+=offset;
		}
		for (int j=0;j<2;j++) {
			if (temp.jdahep[j]!=0) temp.jdahep[j]+=offset;
		}
		event->push_back(temp);
	}
	return event->size();
}

int open_read(char *filename, int istream, int n_events)
{
	printf("Reading from %s; expecting %d events\n",filename,n_events);
	if (xdr_init_done)
		StdHepXdrReadOpen(filename,n_events,istream);
	else
	{
		StdHepXdrReadInit(filename,n_events,istream);
		xdr_init_done = true;
	}
	return n_events;
}

void open_write(char *filename, int ostream, int n_events)
{
	printf("Writing to %s; expecting %d events\n",filename, n_events);
	if (xdr_init_done)
		StdHepXdrWriteOpen(filename,filename,n_events,ostream);
	else
	{
		StdHepXdrWriteInit(filename,filename,n_events,ostream);
		xdr_init_done = true;
	}
	StdHepXdrWrite(100,ostream);
}


bool read_next(int istream)
{
	int ilbl;
    bool read_ok;
	do {
		if (StdHepXdrRead(&ilbl,istream)!=0) {
			printf("End of file\n");
			return false;
		}
        if (ilbl==1 || ilbl==4) read_ok = true;
        else
        {
            read_ok = false;
			printf("ilbl = %d\n",ilbl);
        }
        has_hepev4 = (ilbl==4);
	} while (!read_ok);
	return true;
}

void close_write(int ostream)
{
	StdHepXdrWrite(200,ostream);
	StdHepXdrEnd(ostream);
}

void write_file(int ostream)
{
    if (has_hepev4)
        StdHepXdrWrite(4,ostream);
    else
        StdHepXdrWrite(1,ostream);
    has_hepev4 = false;
}

void close_read(int istream)
{
	StdHepXdrEnd(istream);
}

void print_entry(stdhep_entry entry)
{
	printf("isthep = %d\tidhep = %d\t",entry.isthep,entry.idhep);
	for (int i=0;i<2;i++) printf("jmohep[%d] = %d\t",i,entry.jmohep[i]);
	//	printf("\n");
	for (int i=0;i<2;i++) printf("jdahep[%d] = %d\t",i,entry.jdahep[i]);
	printf("\n");
	for (int i=0;i<5;i++) printf("phep[%d] = %f\t",i,entry.phep[i]);
	printf("\n");
	for (int i=0;i<4;i++) printf("vhep[%d] = %f\t",i,entry.vhep[i]);
	printf("\n");
}
