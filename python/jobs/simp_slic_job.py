"""!
@file simp_job.py

Simulation of SIMPs, detector signals, and readout, followed by reconstruction.
"""
from hpsmc.tools import SLIC, JobManager, BeamCoords

job.description = 'SIMP generation to recon'

inputs = list(job.input_files.values())

## Rotate into beam coords
rot = BeamCoords(inputs=inputs, outputs=["simp_mom_rot.stdhep"])

## Run events in slic
slic = SLIC()

## Run the job
job.add([rot, slic])
