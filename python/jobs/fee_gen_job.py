"""!
Simulation of beam.
"""
from hpsmc.generators import EGS5
from hpsmc.tools import BeamCoords, RandomSample, SLIC, ExtractEventsWithHitAtHodoEcal, JobManager

job.description = 'beam from generation to slic'

## Get job input file targets
inputs = list(job.input_files.values())

if 'nevents' in job.params:
    nevents = job.params['nevents']
else:
    nevents = 250000

## Generate beam
egs5 = EGS5(name="beam_v7_%s" % job.params['run_params'])

## Rotate events into beam coordinates
rot = BeamCoords()

## Sample events into new stdhep file
sample = RandomSample()

## Run the job
job.add([egs5, rot, sample])
