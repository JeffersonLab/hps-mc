"""!
@file phi_lhe_to_recon.py

Simulation of phi meson to charged kaon decays, detector signals, and readout, followed by reconstruction.
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

## Insert empty bunches expected by pile-up simulation
filter_bunches = FilterBunches()

## Run simulated events in readout to generate triggers
readout = JobManager(steering='readout')

## Run physics reconstruction
recon = JobManager(steering='recon')

## Run the job
job.add([cnv, rot, slic, filter_bunches, readout, recon])

