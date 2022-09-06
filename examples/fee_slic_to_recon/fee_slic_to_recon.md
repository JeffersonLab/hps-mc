FEE SLIC to reconstruction pipeline {#feeslictorecon}
===================================

This is an example of how to use slic_to_recon_job.py. Here, the input files contain beam data. The slic component simulates the detector response which means that the output of this job will vary with different detector setups.
The used detector has to be specified in the job.json file. Once the detector response is simulated for each input file, the files are merged into one file.
In a next step the readout is simulated, here using the FEE trigger.
Lastly, the signals are reconstructed. The steering files for readout and reconstruction have to be specified in the job.json file.

#### Job parameters
This table contains parameters special to the slic\_to\_recon job. The general parameters are discussed on the [examples main page](@ref examples).
| param           |                                                                                                                                              |
|-----------------|----------------------------------------------------------------------------------------------------------------------------------------------|
| detector        | detector setup used for slic                                                                                                                 |
| ecal\_hit\_ecut | ?                                                                                                                                            |
| steering\_files  | steering files for readout and reconstruction                                                                                                |
| base\_name       | base name for naming output files:<br>\<base_name\>_readout.slcio<br> \<base_name\>_recon.slcio                                              |
| event\_interval  | interval at which signal events will be spaced in beam data;<br> set to desired value for beam files, set to 1 or leave out for signal files |
| input\_files     | list of stdhep beam files                                                                                                                    |
| output\_files       | readout and recon slcio files                                                                                                                |
