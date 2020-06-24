from hpsmc.generators import EGS5
from hpsmc.tools import BeamCoords, RandomSample, SLIC

job.description = 'beam from generation to slic'

# Get job input file targets
inputs = job.input_files.values()

if 'nevents' in job.params:
    nevents = job.params['nevents']
else:
    nevents = 250000

# Generate beam
egs5 = EGS5(name="beam_v7_4pt55")

# Rotate events into beam coordinates
rot = BeamCoords()

# Sample events into new stdhep file
sample = RandomSample()

# Simulate events
slic = SLIC(nevents=nevents+1)

# Run the job
job.add([egs5, rot, sample, slic])
