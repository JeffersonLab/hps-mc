import os
from hpsmc.tools import JobManager, FilterBunches, LCIOCount, HPSTR

job.description = 'Filter bunches, run readout, hps-java recon, and then a HPSTR analysis'

# Assign ptags for output
input_files = job.input_files.values()
if len(input_files) > 1:
    raise Exception('This script accepts only one input file.')
output_base = os.path.splitext(os.path.basename(input_files[0]))[0]
job.ptag('filt', '%s_filt.slcio' % output_base)
job.ptag('readout', '%s_filt_readout.slcio' % output_base)
job.ptag('lcio_recon', '%s_filt_readout_recon.slcio' % output_base)
#job.ptag('hpstr_recon', '%s_filt_readout_recon.root' % output_base)
#job.ptag('hpstr_ana', '%s_filt_readout_recon_ana.root' % output_base)

count_input = LCIOCount()

# Insert empty bunches expected by pile-up simulation
filter_bunches = FilterBunches()

count_filt = LCIOCount()

# Run simulated events in readout to generate triggers
readout = JobManager(steering='readout')

count_readout = LCIOCount()

# Run physics reconstruction
reco = JobManager(steering='recon')

count_reco = LCIOCount()

# Convert LCIO to ROOT
#cnv = HPSTR(cfg='recon')

# Run an analysis on the ROOT file
#ana = HPSTR(cfg='ana')

job.add([count_input, filter_bunches, count_filt, readout, count_readout, reco, count_reco])
#, cnv, ana])
