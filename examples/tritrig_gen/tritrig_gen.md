Tritrig event generation {#tritriggen}
========================

This is an example of how to use tritrig_gen_job.py. The job generates tritrig events using MadGraph5 and transforms the output from lhe to stdhep to then tag the mother particle and rotate the events to beam coordinates.

#### Job parameters
There are no special run parameters that need to be set for this job other than the general ones discussed on the [examples main page](@ref examples).

#### Job template
This example includes a job template. The variables used in this template mostly have the same name and meaning as the job parameters above. Their values are set in `vars.json`. In addition to the job parameters, `vars.json` contains the variable target\_and\_current is only used to name the output files in a consistent and informative way.