"""!
Simulation of beam, beam signals in detector (using SLIC), and readout.
The simulation is followed by reconstruction of the events.
"""
from hpsmc.generators import EGS5
from hpsmc.tools import BeamCoords, RandomSample, SLIC, ExtractEventsWithHitAtHodoEcal, JobManager

job.description = 'beam from generation to slic'

## Get job input file targets
inputs = list(job.input_files.values())

if 'nevents' in job.params:
    nevents = job.params['nevents']
else:
    nevents = 250000

## Generate beam
egs5 = EGS5(name="beam_v7_%s" % job.params['run_params'])

## Rotate events into beam coordinates
rot = BeamCoords()

## Sample events into new stdhep file
sample = RandomSample()

## Simulate events
slic = SLIC(nevents=nevents + 1)

## Space signal events
space_events = ExtractEventsWithHitAtHodoEcal(event_interval=250, num_hodo_hits=0)

## Run simulated events in readout to generate triggers
readout = JobManager(steering='readout')

## Run physics reconstruction
recon = JobManager(steering='recon')

## Run the job
job.add([egs5, rot, sample, slic, space_events, readout, recon])
