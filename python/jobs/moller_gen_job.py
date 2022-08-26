"""!
Simulation of Moller scattering events and detector signals (using SLIC).
"""
from hpsmc.generators import EGS5
from hpsmc.tools import BeamCoords, RandomSample, SLIC

job.description = 'Moller from generation to slic'

## Get job input file targets
inputs = list(job.input_files.values())

if 'nevents' in job.params:
    nevents = job.params['nevents']
else:
    nevents = 250000

## Generate beam
egs5 = EGS5(name="moller_v3")

## Rotate events into beam coordinates
rot = BeamCoords()

## Run the job
job.add([egs5, rot])
