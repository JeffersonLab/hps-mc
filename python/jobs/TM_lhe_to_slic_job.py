"""!
@file TM_lhe_to_slic.py

Simulation processing of generated TM samples to detector signals.
"""
from hpsmc.generators import MG5
from hpsmc.tools import SLIC, JobManager, FilterBunches, BeamCoords, Unzip, DisplaceTime

job.description = 'Phi lhe to slic'

## Convert LHE output to stdhep
cnv = DisplaceTime(inputs=['TM.lhe'], outputs=['TM.stdhep'])

## Rotate into beam coords
rot = BeamCoords()

## Run events in slic
slic = SLIC()

## Run the job
job.add([cnv, rot, slic])
