"""!
@file readout_recon_job.py

Simulate pile-up, run readout, hps-java recon, and analysis.
"""
import os, logging
from hpsmc.tools import JobManager, FilterBunches, LCIOCount, HPSTR

## Initialize logger with default level
logger = logging.getLogger('hpsmc.job')

job.description = 'Simulate pile-up, run readout, hps-java recon, and analysis'

if 'filter_bunches' in job.params:
    filter_bunches = job.params['filter_bunches']
else:
    filter_bunches = False

## Assign ptags for output
input_files = list(job.input_files.values())
if len(input_files) > 1:
    raise Exception('This script accepts only one input file.')
output_base = os.path.splitext(os.path.basename(input_files[0]))[0]
job.ptag('filt', '%s_filt.slcio' % output_base)
job.ptag('readout', '%s_filt_readout.slcio' % output_base)
job.ptag('lcio_recon', '%s_filt_readout_recon.slcio' % output_base)
job.ptag('hpstr_recon', '%s_filt_readout_recon.root' % output_base)
job.ptag('hpstr_ana', '%s_filt_readout_recon_ana.root' % output_base)

## Insert empty bunches expected by pile-up simulation
if filter_bunches:
    filtered = FilterBunches()
    job.add([filtered])

## Run simulated events in readout to generate triggers
readout = JobManager(steering='readout')

count_readout = LCIOCount()

## Run physics reconstruction
reco = JobManager(steering='recon')

count_reco = LCIOCount()

## Convert LCIO to ROOT
cnv = HPSTR(cfg='recon')

## Run an analysis on the ROOT file
ana = HPSTR(cfg='ana')

job.add([readout, count_readout, reco, count_reco, cnv])
#, ana])
