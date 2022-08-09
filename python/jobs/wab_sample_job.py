"""!
Python script for generating sampled WAB events in StdHep format from an input LHE file.

The output events can be used as input to 'merge_job.py' with a beam StdHep file to for creating a
wab-beam sample file.
"""

from hpsmc.tools import BeamCoords, MergePoisson
from hpsmc.generators import StdHepConverter

job.description = 'WAB sampling'

## Convert wab events to stdhep
cnv = StdHepConverter()

## Rotate WAB events into beam coordinates
rot = BeamCoords()

## Sample wabs using poisson distribution, calculating mu from provided LHE file
sample = MergePoisson(input_filter='wab',
                      lhe_file=next(iter(job.input_files.values())),
                      nevents=500000)

## Add components
job.add([cnv, rot, sample])
