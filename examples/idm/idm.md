iDM generation to reconstruction pipeline {#idm}
=========================================

This is an example of how to use idm_job.py. The job generates simps using MadGraph5, transforms the output from lhe to stdhep, rotates the events into beam coordinates, and runs slic to simulate the detector response. Lastly, pile-up is simulated as well as the detector readout before the reconstruction is run.

#### Job parameters
This table contains parameters special to the simp job. The general parameters are discussed on the \ref examples "examples main page".
 
| param            |                                           |
|------------------|-------------------------------------------|
| Map              | A-prime mass [MeV]                        |
| mchi             | average dark fermion mass [MeV]           |
| dmchi            | dark fermion mass difference [MeV]        |
| steering\_files  | readout and reconstruction steering files |
| output\_files    | signal and reco output slcio files        |

#### Job template
This example includes a job template. The variables used in this template mostly have the same name and meaning as the job parameters above. Their values are set in `vars.json`.
The `mkjobs.sh` script will create a set of jobs for 6 different dark rho masses which are specified in `vars.json`.
