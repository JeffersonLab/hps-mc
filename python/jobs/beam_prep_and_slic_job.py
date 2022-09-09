"""!
@file beam_prep_and_slic_job.py

Transform events to beam coodinates, randomly sample them and simulating detector response using slic.
"""
from hpsmc.tools import BeamCoords, RandomSample, SLIC

## Get job input file targets
inputs = list(job.input_files.values())

## Rotate events into beam coordinates
rot = BeamCoords()

## Sample events into new stdhep file
sample = RandomSample()

## Simulate detector response
slic = SLIC()

## Run the job
job.add([rot, sample, slic])