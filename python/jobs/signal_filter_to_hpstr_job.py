"""!
@file signal_filter_to_hpstr_job.py

No idea what this is supposed to do.
"""

from hpsmc.tools import ExtractEventsWithHitAtHodoEcal, EvioToLcio, JobManager, FilterBunches, LCIOCount, HPSTR

job.description = 'signal from filter to hpstr'

## Get job input file targets
inputs = list(job.input_files.values())

## Input signal events (slcio format)
signal_file_name = []

for input in inputs:
    if "signal" in input:
        signal_file_name.append(input)

## Check for expected input file targets
if len(signal_file_name) == 0:
    raise Exception("Missing required input file(s) for signal")

## Base name of intermediate signal files
signal_name = 'signal'

## Filter signal events and catenate files
filter_events = ExtractEventsWithHitAtHodoEcal(inputs=signal_file_name,
                                               outputs=['%s_filt.slcio' % signal_name],
                                               event_interval=250, num_hodo_hits=1)

## Count filtered events
count_filter = LCIOCount(inputs=filter_events.output_files())

## Run simulated events in readout to generate triggers
readout = JobManager(steering='readout',
                     inputs=filter_events.output_files(),
                     outputs=['%s_readout.slcio' % signal_name])

## Print number of readout events
count_readout = LCIOCount(inputs=readout.output_files())

## Run physics reconstruction
recon = JobManager(steering='recon',
                   inputs=readout.output_files(),
                   outputs=['%s_recon.slcio' % signal_name])

## Print number of recon events
count_recon = LCIOCount(inputs=recon.output_files())

tuple = HPSTR(cfg='recon',
              inputs=recon.output_files())

## Add the components
job.add([filter_events, count_filter, readout, count_readout, recon, count_recon, tuple])
