"""!
@file moller4_gen_job.py

Simulation of Moller scattering events.
"""
from hpsmc.generators import EGS5
from hpsmc.tools import BeamCoords

job.description = 'Moller generation'

## Get job input file targets
inputs = list(job.input_files.values())

if 'nevents' in job.params:
    nevents = job.params['nevents']
else:
    nevents = 250000

## Generate beam
egs5 = EGS5(name="moller_v4")

## Rotate events into beam coordinates
rot = BeamCoords()

## Run the job
job.add([egs5, rot])
