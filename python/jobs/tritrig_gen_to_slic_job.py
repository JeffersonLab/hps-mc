from hpsmc.generators import MG5, StdHepConverter
from hpsmc.tools import BeamCoords, AddMother, SLIC

job.description = 'tritrig from generation to slic'

# Get job input file targets
inputs = list(job.input_files.values())

if 'nevents' in job.params:
    nevents = job.params['nevents']
else:
    nevents = 10000

# Generate tritrig in MG5
mg = MG5(name='tritrig', event_types=['unweighted'])

# Convert LHE output to stdhep
cnv = StdHepConverter()

# Add mother particle to tag trident particles
mom = AddMother()

# Rotate events into beam coords
rot = BeamCoords()

# Simulate signal events
slic = SLIC(nevents=nevents+1)

# run the job
job.add([mg, cnv, mom, rot, slic])
