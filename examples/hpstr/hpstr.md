HPSTR {#hpstr}
=====

This example illustrates how to use hpstr_job.py. In this job, hpstr is first used to convert the lcio input files into root files. In a second step, hpstr is used to run the analysis on the created root files.

#### Job Parameters
This table contains parameters special to the hpstr job. The general parameters are discussed on the [examples main page](@ref examples).

| param         |                                                                                                          |
|---------------|----------------------------------------------------------------------------------------------------------|
| config\_files | configuration files for recon and ana                                                                    |
| run\_mode     | ?                                                                                                        |
| input\_files  | "path/to/input.slcio": "recon_events.slcio"                                                              |
| output\_files | "recon_files.root": "your_reco_filename.root"<br>"recon_files_ana.root": "your_ana_filename.root"        |
