"""!
@file phi_lhe_to_recon.py

Simulation of phi meson to charged kaon decays, detector signals.
"""
from hpsmc.generators import MG5
from hpsmc.tools import SLIC, JobManager, FilterBunches, BeamCoords, Unzip, DisplaceUni

job.description = 'Phi lhe to recon'

## Convert LHE output to stdhep
cnv = DisplaceUni(inputs=['phi.lhe'], outputs=['phi.stdhep'])

## Rotate into beam coords
rot = BeamCoords()

## Run events in slic
slic = SLIC()

## Run the job
job.add([cnv, rot, slic])

