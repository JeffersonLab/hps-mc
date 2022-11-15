WAB generation and sampling {#wabgensample}
===========================

This is an example of how to use wab_gen_sample_job.py. The job generates wab events using MadGraph4 and transforms the output from lhe to stdhep to then rotate the events into beam coordinates and sample them using a poisson distribution, calculating mu from provided cross section.

#### Job parameters
There are no special run parameters that need to be set for this job other than the general ones discussed on the [examples main page](@ref examples).

#### Job template
This example includes a job template. The variables used in this template mostly have the same name and meaning as the job parameters above. Their values are set in `vars.json`.