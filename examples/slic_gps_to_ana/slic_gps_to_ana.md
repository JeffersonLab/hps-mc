SLIC to analysis pipeline with gps macro from GEANT4 GPS {#slicgpstoana}
=====================================

slic_gps_to_ana_job.py runs slic on a GEANT4 GPS macro, converts the output to root files, runs the reconstruction and runs a hpstr analysis on these files.

#### Job parameters
This table contains parameters special to the slic\_gps\_to\_ana job. The general parameters are discussed on the [examples main page](@ref examples).

| param         |                                       |
|---------------|---------------------------------------|
| steering\_files | steering files for readout and recon |
| config\_files | configuration files for recon and ana |
| macros  | GEANT4 GPS macrpo .mac files                    |
