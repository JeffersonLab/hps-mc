"""
Python script for generating 'tritrig' events in MG5, converting to StdHep format,
and applying transformations. 
"""

import sys, random

from hpsmc.job import Job
from hpsmc.run_params import RunParameters
from hpsmc.generators import MG5, StdHepConverter
from hpsmc.tools import StdHepTool

job = Job()

# Generate tritrig in MG5
mg = MG5(name="tritrig")

# Convert LHE output to stdhep
cnv = StdHepConverter()

# Add mother particle to tag trident particles
mom = StdHepTool(name="add_mother",
                 append='_mom')

# Rotate events into beam coords
rot = StdHepTool(name="beam_coords",
                 replace={'_mom': '_rot'})

# Print results
p = StdHepTool(name="print_stdhep")
 
# Add components
job.add([mg, cnv, mom, rot, p])

# Run the job
job.run()
