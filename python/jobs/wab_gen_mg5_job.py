"""!
@file wab_gen_sample_job.py
Python script for generating sampled WAB events in StdHep format from an input LHE file.
The output events can be used as input to 'merge_job.py' with a beam StdHep file to for creating a
wab-beam sample file.
"""

from hpsmc.generators import MG5
from hpsmc.tools import BeamCoords
from hpsmc.generators import StdHepConverter

job.description = 'WAB gen and sampling'

## generate tritrig in MG5
mg = MG5(name="WAB")

## Convert wab events to stdhep
cnv = StdHepConverter(inputs=mg.output_files(), outputs=['wab_events.stdhep'])

## Rotate WAB events into beam coordinates
rot = BeamCoords()

## Add components -> add check and sample as needed
job.add([mg, cnv, rot])
