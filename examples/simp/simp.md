Simp generation to reconstruction pipeline {#simp}
==========================================

This is an example of how to use simp_job.py. The job generates simps using MadGraph5, transforms the output from lhe to stdhep, rotates the events into beam coordinates, and runs slic to simulate the detector response. Lastly, pile-up is simulated as well as the detector readout before the reconstruction is run.

#### Job parameters
This table contains parameters special to the simp job. The general parameters are discussed on the \ref examples "examples main page".
 
| param            |                                           |
|------------------|-------------------------------------------|
| map              | A-prime mass                              |
| mpid             | dark pi mass                              |
| mrhod            | dark rho mass                             |
| filter\_no\_cuts | ?                                         |
| steering\_files  | readout and reconstruction steering files |
| output\_files    | signal and reco output slcio files        |
