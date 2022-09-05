/**
 * \page reconevio recon\_evio
 * 
 * Run the hps-java reconstruction on data files. The input format is evio which is the Jlab DAQ output format. 
 * 
 * \section params Job parameters
 * This table contains parameters special to the recon\_evio job. The general parameters are discussed on the \ref examples "examples main page".
 * 
 * | param           |                                                                         |
 * |-----------------|-------------------------------------------------------------------------|
 * | steering\_files | reconstruction steering file                                            |
 * | input\_files    | list of evio input files:<br>"path/to/input.evio": "input_name.evio"    |
 * | output\_files   | list of output recon slcio file:<br>"recon.slcio": "output\_name.slcio" |
 */