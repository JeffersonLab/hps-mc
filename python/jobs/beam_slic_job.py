"""!
Simulation of beam signals in detector using SLIC.
"""
from hpsmc.tools import SLIC

job.description = 'beam detector sim via slic'

## Get job input file targets
inputs = list(job.input_files.values())

if 'nevents' in job.params:
    nevents = job.params['nevents']
else:
    nevents = 250000

## Simulate events
slic = SLIC(nevents=nevents+1)

## Run the job
job.add([slic])
