"""!
@file tritrig_gen_job.py

Simulate tritrig events.
"""
from hpsmc.generators import MG5

job.description = 'Generate tritrig events using MadGraph5'

## Generate tritrig in MG5
mg = MG5(name='tritrig')

## Run the job
job.add([mg])
