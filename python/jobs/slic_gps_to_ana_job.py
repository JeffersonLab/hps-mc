"""!
@file slic_gps_to_ana_job.py

Run slic to analysis using gps macro.
"""
import os
from hpsmc.tools import SLIC, JobManager, FilterBunches, HPSTR

job.description = 'Run slic with preexisting tritrig stdhep files'

## generate events in slic
base_name = "gps"
input_filename = '{}.stdhep'.format(base_name)
sim = SLIC(inputs=[input_filename])

## insert empty bunches expected by pile-up simulation
filter_bunches = FilterBunches()

## Run simulated events in readout to generate triggers
readout = JobManager(steering='readout', ignore_job_params=['nevents'])

## Run physics reconstruction
recon = JobManager(steering='recon', ignore_job_params=['nevents'])

## Convert LCIO to ROOT
root_cnv = HPSTR(cfg='recon')

## Run an analysis on the ROOT file
ana = HPSTR(cfg='ana')

## Output names
job.ptag('sim', '{}.slcio'.format(base_name))
job.ptag('readout', '{}_filt_readout.slcio'.format(base_name))
job.ptag('recon', '{}_filt_readout_recon.slcio'.format(base_name))
job.ptag('recon_root', '{}_filt_readout_recon.root'.format(base_name))
job.ptag('ana', '{}_filt_readout_recon_ana.root'.format(base_name))

## Add job components
job.add([sim, filter_bunches, readout, recon, root_cnv, ana])
