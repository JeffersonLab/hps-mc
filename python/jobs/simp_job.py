"""!
@file simp_job.py

Simulation of SIMPs, detector signals, and readout, followed by reconstruction.
"""
from hpsmc.generators import MG5
from hpsmc.tools import SLIC, JobManager, FilterBunches, BeamCoords, Unzip, DisplaceUni

job.description = 'SIMP generation to recon'

# Generate tritrig in MG5
mg = MG5(name='simp',
         run_card='run_card.dat',
         param_card='param_card.dat',
         event_types=['unweighted'])

# Unzip LHE file
unzip = Unzip(inputs=['simp_unweighted_events.lhe.gz'], outputs=['simp.lhe'])

# Convert LHE output to stdhep (no displacement here because no ctau given)
cnv = DisplaceUni(inputs=['simp.lhe'], outputs=['simp.stdhep'])

# Rotate into beam coords
rot = BeamCoords()

# Run events in slic
slic = SLIC()

# Insert empty bunches expected by pile-up simulation
filter_bunches = FilterBunches()

# Run simulated events in readout to generate triggers
readout = JobManager(steering='readout')

# Run physics reconstruction
recon = JobManager(steering='recon')

# Run the job
job.add([mg, unzip, cnv, rot, slic, filter_bunches, readout, recon])
