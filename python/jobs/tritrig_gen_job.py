"""
Python script for generating 'tritrig' events in MG5. 
"""

import sys, random

from hpsmc.job import Job
from hpsmc.run_params import RunParameters
from hpsmc.generators import MG5

job = Job()

# Generate tritrig in MG5
mg = MG5(name='tritrig')

# run the job
job.components=[mg]
job.run()
