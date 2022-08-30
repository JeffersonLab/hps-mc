"""!
Python script for creating beam background events.
"""
from hpsmc.tools import BeamCoords, RandomSample

job.description = 'Beam sampling'

## Rotate events into beam coordinates
rot = BeamCoords()

## Sample events into new stdhep file
sample = RandomSample()

## Run the job
job.add([rot, sample])
