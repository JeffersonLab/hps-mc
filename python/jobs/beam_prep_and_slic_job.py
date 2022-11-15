"""!
@file beam_prep_and_slic_job.py

Transform events to beam coodinates, randomly sample them and simulating detector response using slic.
"""
from hpsmc.tools import BeamCoords, RandomSample, SLIC

if 'nevents' in job.params:
    nevents = job.params['nevents']
else:
    nevents = 10000

if 'event_interval' in job.params:
    event_interval = job.params['event_interval']
else:
    event_interval = 1

## Get job input file targets
inputs = list(job.input_files.values())

## Rotate events into beam coordinates
rot = BeamCoords()

## Sample events into new stdhep file
sample = RandomSample()

## Simulate detector response

slic = SLIC(nevents=nevents*event_interval, ignore_job_params=['nevents'])

## Run the job
job.add([rot, sample, slic])