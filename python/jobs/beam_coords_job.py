"""!
Apply transforms to input StdHep events.
"""
from hpsmc.tools import BeamCoords

job.description = 'Beam rotation and transform'

## Apply beam transforms
rot = BeamCoords()

## Run the job
job.add([rot])
