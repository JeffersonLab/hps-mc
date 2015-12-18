#include <vector>
using namespace std;

extern bool xdr_init_done;
extern bool has_hepev4;

struct stdhep_entry {
	int isthep;     /* status code */
	int idhep;      /* The particle id */
	int jmohep[2];    /* The position of the mother particle */
	int jdahep[2];    /* Position of the first daughter... */
	double phep[5];    /* 4-Momentum, mass */
	double vhep[4];    /* Vertex information */
    //double spinlh[3];
    //double icolorflowlh[2];
};

struct stdhep_event {
    vector<stdhep_entry> particles;
    int nevhep;
    bool has_hepev4;
    int idruplh;
    double eventweightlh;
    //double alphaqedlh;
    //double alphaqcdlh;
    //double scalelh[10];
};

int read_stdhep(vector<stdhep_entry> *new_event);
void read_stdhep(stdhep_event *new_event);
void write_stdhep(vector<stdhep_entry> *new_event, int nevhep);
void write_stdhep(stdhep_event *new_event);
void add_filler_particle(vector<stdhep_entry> *new_event);
int append_stdhep(vector<stdhep_entry> *event, const vector<stdhep_entry> *new_event);

int open_read(char *filename, int istream, int n_events=1000000);
void open_write(char *filename, int ostream, int n_events);
void close_write(int ostream);
void write_file(int ostream);
bool read_next(int istream);
void close_read(int istream);
void print_entry(stdhep_entry entry);
