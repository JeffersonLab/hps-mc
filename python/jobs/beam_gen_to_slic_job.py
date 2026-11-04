"""!
@file beam_prep_and_slic_job.py

Transform events to beam coodinates, randomly sample them and simulating detector response using slic.
"""
from hpsmc.generators import EGS5
from hpsmc.tools import BeamCoords, RandomSample, SLIC

if 'nevents' in job.params:
    nevents = job.params['nevents']
else:
    nevents = 10000

if 'event_interval' in job.params:
    event_interval = job.params['event_interval']
else:
    event_interval = 1

## Generate the events
egs5 = EGS5(name="beam_v7_%s" % job.params['run_params'])

## Rotate events into beam coordinates
rot = BeamCoords()

## Sample events into new stdhep file
sample = RandomSample()

## Simulate detector response
slic = SLIC()

## Run the job
job.add([egs5, rot, sample, slic])
