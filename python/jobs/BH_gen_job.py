"""!
@file BH_gen_job.py

Simulate Bethe-Heitler events.
"""
from hpsmc.generators import MG5

job.description = 'Generate Bethe-Heitler events using MadGraph5'

## Generate tritrig in MG5
mg = MG5(name='BH')

## Run the job
job.add([mg])
