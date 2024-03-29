"""!
@file sim_to_ana_job.py

Run hps-sim with preexisting tritrig stdhep files.
"""
import os
from hpsmc.tools import Sim, JobManager, FilterBunches, HPSTR

job.description = 'Run hps-sim with preexisting tritrig stdhep files'

## generate events in slic
sim = Sim()

## insert empty bunches expected by pile-up simulation
filter_bunches = FilterBunches()

## Run simulated events in readout to generate triggers
readout = JobManager(steering='readout')

## Run physics reconstruction
recon = JobManager(steering='recon')

## Convert LCIO to ROOT
root_cnv = HPSTR(cfg='recon')

## Run an analysis on the ROOT file
ana = HPSTR(cfg='ana')

## Set persistency tags for output files
base_name, ext = os.path.splitext(list(job.input_files.values())[0])
job.ptag('recon', '{}_filt_readout_recon.slcio'.format(base_name))
job.ptag('ana', '{}_filt_readout_recon_ana.root'.format(base_name))

## Add job components
job.add([sim, filter_bunches, readout, recon, root_cnv, ana])
