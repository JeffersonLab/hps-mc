"""
Python script for generating 'tritrig' events in MG5 and running 
through simulation, readout and reconstruction. 
"""

from hpsmc.generators import MG5, StdHepConverter
from hpsmc.tools import SLIC, JobManager, FilterBunches, BeamCoords, AddMother

# Generate tritrig in MG5
mg = MG5(name='tritrig')

# Convert LHE output to stdhep
cnv = StdHepConverter()

# Add mother particle to tag trident particles
mom = AddMother()

# Rotate events into beam coords
rot = BeamCoords()

# Print results
#p = StdHepTool(name="print_stdhep")

# generate events in slic
slic = SLIC()

# insert empty bunches expected by pile-up simulation
filter_bunches = FilterBunches(nevents=2000000)

# Run simulated events in readout to generate triggers
readout = JobManager(steering='readout')

# Run physics reconstruction
recon = JobManager(steering='recon')
 
# Add job components
job.add([mg, cnv, mom, rot, slic, filter_bunches, readout, recon])
