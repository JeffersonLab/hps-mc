"""!
@file signal_pulser_overlay_to_recon_job.py
No idea what this is supposed to do.
"""
from hpsmc.tools import ExtractEventsWithHitAtHodoEcal, EvioToLcio, JobManager, FilterBunches, LCIOCount
from hpsmc.tools import HPSTR

job.description = 'signal-pulse from overlay to recon'

## Get job input file targets
inputs = list(job.input_files.values())

## Input signal events (slcio format)
signal_file_name = []

## Input pulser events (evio format)
pulser_file_name = []
for input in inputs:
    if "signal" in input:
        signal_file_name.append(input)
    if "pulser" in input:
        pulser_file_name.append(input)

## Check for expected input file targets
if len(signal_file_name) == 0:
    raise Exception("Missing required input file(s) for signal")
if len(pulser_file_name) == 0:
    raise Exception("Missing required input file(s) for pulser data")

## Base name of intermediate signal files
signal_name = 'signal'

## Base pulser of intermediate pulser files
pulser_name = 'pulser'

## Base name of merged files
signal_pulser_name = 'signal_pulser'

## Run physics reconstruction
recon = JobManager(steering='recon',
                   inputs=['%s_readout.slcio' % signal_pulser_name],
                   outputs=['%s_recon.slcio' % signal_pulser_name])

## Print number of recon events
count_recon = LCIOCount(inputs=recon.output_files())

## Convert LCIO to ROOT
cnv = HPSTR(inputs=recon.output_files(), cfg='cnv')

## Add the components
job.add([recon, count_recon, cnv])
