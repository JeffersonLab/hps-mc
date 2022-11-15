Tritrig and beam files -- merge to reconstruction pipline {#tritrigbeam}
=========================================================

tritrig_beam_slic_to_reco_job.py is used to combine simulated tritrig and beam data. It takes multiple stdhep beam files and a tritrig stdhep files as input (all in beam coords). Then slic is used to simulate the detector response to the beam and tritrig events separately before the files are merged into one slcio file. Lastly, the detector readout is simualted and the hps-java reconstruction is run.
Usually, 10 beam files are used per tritrig file to use all the tritrig events while still maintaining the correct ratio of trittrig to beam data. In this example, we only use three beam files to speed up the runtime of the example.

#### Job parameters
This table contains parameters special to the tritrig\_beam job. The general parameters are discussed on the [examples main page](@ref examples).

| param           |                                                                                                                                                                                           |
|-----------------|-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
| ecal\_hit\_ecut | ECAL energgy cut in MeV?                                                                                                                                                                  |
| steering\_files | readout and reconstruction steering files                                                                                                                                                 |
| config\_files   | configuration files for recon and ana                                                                                                                                                     |
| input\_files    | list of stdhep input files:<br>"path/to/tritrig_file.stdhep": "tritrig_events.stdhep"<br>"path/to/beam_file_1.stdhep": "beam_1.stdhep"<br>...<br>"path/to/beam_file_10": "beam_10" |
| output\_files   | list of readout and reconstruction slcio output files                                                                                                                                     |
