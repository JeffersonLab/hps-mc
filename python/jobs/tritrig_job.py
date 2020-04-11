"""
Python script for generating 'tritrig' events in MG5 and running 
through simulation, readout and reconstruction. 
"""

import sys, random

from hpsmc.job import Job
from hpsmc.run_params import RunParameters
from hpsmc.generators import MG5, StdHepConverter
from hpsmc.tools import SLIC, JobManager, FilterBunches, StdHepTool

job = Job()

# Generate tritrig in MG5
mg = MG5(name='tritrig')

# Convert LHE output to stdhep
cnv = StdHepConverter()

# Add mother particle to tag trident particles
mom = StdHepTool(name='add_mother')

# Rotate events into beam coords
rot = StdHepTool(name='beam_coords')

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
 
# run the job
job.components=[mg, cnv, mom, rot, slic, filter_bunches, readout, recon]
job.run()
