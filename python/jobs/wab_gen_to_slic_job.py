from hpsmc.generators import MG4, StdHepConverter
from hpsmc.tools import BeamCoords, AddMother, SLIC

job.description = 'wab from generation to slic'

# Get job input file targets
inputs = job.input_files.values()

if 'nevents' in job.params:
    nevents = job.params['nevents']
else:
    nevents = 10000

# Generate wab in MG4
mg = MG4(name='wab', event_types=['unweighted'])

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
