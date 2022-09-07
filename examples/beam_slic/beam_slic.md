Beam SLIC detector simulation {#beamslic}
=============================
This example uses slic_job.py to simulate the detector response to the incoming beam. For each input file, the slic component is run and the results are saved separate output files.

Analogously to this example, the slic_job can be used with other input files, e.g. tritrig or wab. The job.json file has to be adjusted accordingly but no changes to slic_job.py are necessary.

#### Job parameters
There are no special run parameters that need to be set for this job other than the general ones discussed on the [examples main page](@ref examples).

| param         |                                                                                                                                  |
|---------------|----------------------------------------------------------------------------------------------------------------------------------|
| input\_files  | list of stdhep input files:<br>"path/to/input_1.stdhep": "beam_1.stdhep"<br>...<br>"path/to/input_n.stdhep": "beam_n.stdhep" |
| output\_files | list of slcio ouput files:<br>"beam_1.slcio":"output_1.slcio"<br>...<br>"beam_n.slcio":"output_n.slcio"                      |
