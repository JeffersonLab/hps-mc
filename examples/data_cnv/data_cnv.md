Data conversion -- evio to lcio  {#datacnv}
===============================

This is an example of how to use the EvioToLcio function to convert evio input files to lcio files, see data_cnv.py. The data_cnv_job then also runs hpstr to create a recon tuple on the basis of the lcio file.

#### Job parameters
This table contains parameters special to the data\_cnv job. The general parameters are discussed on the [examples main page](@ref examples).

| param          |                                  |
|----------------|----------------------------------|
| detector       | detector setup used for slic     |
| skip\_events   | ?                                |
| steering_files | steering file for reconstruction |
| config\_files  | ?                                |
