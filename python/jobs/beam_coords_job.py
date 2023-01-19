"""!
@file beam_coords_job.py

Transform events in the input stdhep file to beam coordinates.
"""
from hpsmc.tools import BeamCoords

job.description = 'Beam rotation and transform'

# Apply beam transforms
rot = BeamCoords()

# Run the job
job.add([rot])
