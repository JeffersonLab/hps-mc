"""!
@file tritrig_gen_to_beam_coords_job.py

Simulate tritrig events, add mother particle information and rotate events into beam coordinates.
"""
from hpsmc.generators import MG5, StdHepConverter
from hpsmc.tools import Unzip, AddMotherFullTruth, BeamCoords

job.description = 'Generate tritrig events using MadGraph5 and convert to StdHep using EGS5'

# Generate tritrig in MG5
mg = MG5(name='tritrig')

## Unzip the LHE events to a local file
unzip = Unzip(inputs=mg.output_files())

# Convert LHE output to stdhep
cnv = StdHepConverter(inputs=mg.output_files(), outputs=['tritrig.stdhep'])

# Add mother particle to tag trident particles
mom = AddMotherFullTruth(inputs=[cnv.output_files()[0], unzip.output_files()[0]], outputs=['tritrig_mom.stdhep'])

# Rotate events into beam coords
rot = BeamCoords()

# Add ptag for gen file
job.ptag('gen', 'tritrig_mom_rot.stdhep')

# Run the job
job.add([mg, unzip, cnv, mom, rot])
