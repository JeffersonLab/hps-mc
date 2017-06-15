#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>

#include <stdhep_util.hh>
#include "stdhep_mcfio.h"

// Open a stdhep file which will print its header info.
// Can be used to get file information from shell scripts or Python.
int main(int argc,char** argv) {

    if (argc<2) {
        printf("<stdhep file>\n");
	return 1;
    }

    int is = 0;
    char* f = argv[1];

    open_read(f, is);

    return 0;
}

