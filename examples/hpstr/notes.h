/**
 * \page hpstr hpstr
 * 
 * This example illustrates how to use ```hpstr_job.py```. In this job, hpstr is first used to convert the lcio input files into root files. In a second step, hpstr is used to run the analysis on the created root files.
 * 
 * \section params Job Parameters
 * This table contains parameters special to the hpstr job. The general parameters are discussed on the \ref examples "examples main page".
 * 
 * | param         |                                                                                                          |
 * |---------------|----------------------------------------------------------------------------------------------------------|
 * | config\_files | configuration files for recon and ana                                                                    |
 * | run\_mode     | ?                                                                                                        |
 * | input\_files  | "path/to/input.slcio": "recon_events.slcio"                                                              |
 * | output\_files | "recon\_files.root": "your\_reco\_filename.root"<br>"recon\_files\_ana.root": "your\_ana\_filename.root" |
 */
