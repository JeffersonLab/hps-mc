"""
Python script for creating beam background events.

Based on this Auger script:

https://github.com/JeffersonLab/hps-mc/blob/master/scripts/mc_scripts/slic/beam.xml

"""

# TODO: Change this to just process one stdhep at a time

import os
from hpsmc.job import Job
from hpsmc.tools import BeamCoords, RandomSample

job = Job()

# Rotate events into beam coordinates
rot = BeamCoords()

# Sample events into new stdhep file
sample = RandomSample()

# Run the job
job.add([rot, sample])
job.run()
