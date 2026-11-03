"""!
@file moller_gen_job.py

Simulation of Moller scattering events.

Optional job parameter **egs5_program** selects the EGS5 program (default moller_v3).
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

## EGS5 program; moller_v5 adds Moller scattering off bound target electrons
## (see the bound_* parameters of hpsmc.generators.EGS5)
egs5_program = job.params.get('egs5_program', 'moller_v3')

## Generate beam
egs5 = EGS5(name=egs5_program)

## Rotate events into beam coordinates
rot = BeamCoords()

## Run the job
job.add([egs5, rot])
