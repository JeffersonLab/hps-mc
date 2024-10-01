"""!
@file wab_gen_sample_job.py

Python script for generating sampled WAB events in StdHep format from an input LHE file.

The output events can be used as input to 'merge_job.py' with a beam StdHep file to for creating a
wab-beam sample file.
"""

from hpsmc.generators import MG5, MG4
from hpsmc.tools import BeamCoords, MergePoisson
from hpsmc.generators import StdHepConverter

job.description = 'WAB gen and sampling'

## generate tritrig in MG5
mg = MG4(name="wab")

# check that at least 80% of the requested events were generated or fail the job
# check = LHECount(minevents=params.nevents*0.8, inputs=["wab_unweighted_events.lhe.gz"])

## Convert wab events to stdhep
cnv = StdHepConverter(inputs=mg.output_files(), outputs=['wab_events.stdhep'])

## Rotate WAB events into beam coordinates
rot = BeamCoords()

## Sample wabs using poisson distribution, calculating mu from provided cross section
sample = MergePoisson(input_filter='wab',
                      input_files=['wab_unweighted_events_rot.stdhep'],
                      output_files=['wab_unweighted_events_rot_sampled.stdhep'],
                      xsec=7.55e10)

## Add components -> add check and sample as needed
job.add([mg, cnv, rot])
