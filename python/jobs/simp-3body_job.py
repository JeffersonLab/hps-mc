"""!
@file simp_job.py

Simulation of SIMPs, detector signals, and readout, followed by reconstruction.
"""
from hpsmc.generators import MG5
from hpsmc.tools import SLIC, JobManager, FilterBunches, BeamCoords, Unzip, DisplaceUni

job.description = 'SIMP generation to recon'

## Generate tritrig in MG5
mg = MG5(name='simp-3body',
         run_card='run_card.dat',
         param_card='param_card.dat',
         event_types=['unweighted'])

## Unzip LHE file
unzip = Unzip(inputs=['simp-3body_unweighted_events.lhe.gz'], outputs=['simp.lhe'])

