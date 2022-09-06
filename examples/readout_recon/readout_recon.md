Readout and reconstruction  {#readoutrecon}
==========================

This is an example of how to use the readout\_recon\_job which takes a slcio file as input, simulates the detector readout and subsequently runs the reconstruction on this data. The script can be extended to also include conversion of the reconstruction output to root files and analysis of those (see readout_recon_job.py).

#### Job parameters
This table contains parameters special to the readout\_recon job. The general parameters are discussed on the [examples main page](@ref examples).

| param                  |                                                                                               |
|------------------------|-----------------------------------------------------------------------------------------------|
| filter\_nevents\_read  | number of events that are read from input file?                                               |
| filter\_nevents\_write | number of events written to output file?                                                      |
| filter\_event\_iterval | ?                                                                                             |
| filter\_no\_cuts       | ?                                                                                             |
| event\_print\_interval | interval at which events will be printed out?                                                 |
| steering\_files        | steering files for readout and reconstruction                                                 |
| input\_files           | list of input slcio files:<br>"path/to/input": "input\_name.slcio"                            |
| output\_files          | list of output recon slcio file:<br>"input\_name\_filt\_readout\_recon.slcio": "output.slcio" |