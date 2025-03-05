"""!
@file moller4_gen_job.py

Simulation of Moller scattering events.
"""
from hpsmc.generators import EGS5
from hpsmc.tools import BeamCoords, SLIC, ExtractEventsWithHitAtHodoEcal, JobManager

job.description = 'Moller generation'

## Get job input file targets
inputs = list(job.input_files.values())

if 'nevents' in job.params:
    nevents = job.params['nevents']
else:
    nevents = 10000

## Generate beam
egs5 = EGS5(name="moller_v4")

## Rotate events into beam coordinates
rot = BeamCoords()

## Simulate events
slic = SLIC(nevents=nevents+1)

## Space Events. -- Can be skipped with Matt's new changes???
space_events = ExtractEventsWithHitAtHodoEcal(event_interval=250, num_hodo_hits=0)

## Readout
readout = JobManager(steering='readout')

## Recon
recon = JobManager(steering='recon')

## Run the job
job.add([egs5, rot, slic, space_events, readout, recon])
