"""!
@file signal_pulser_overlay_to_recon_job.py

Processes slic to recon without beam overlay.
"""

from hpsmc.tools import ExtractEventsWithHitAtHodoEcal, EvioToLcio, JobManager, FilterBunches, LCIOCount, HPSTR

job.description = 'signal-pulse from overlay to recon'

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

## Filter signal events and catenate files before overlaying with pulser data
filter_events = ExtractEventsWithHitAtHodoEcal(inputs=signal_file_name, outputs=['%s_filt.slcio' % signal_name], event_interval=0, num_hodo_hits=1)

## Count filtered events
count_filter = LCIOCount(inputs=filter_events.output_files())

## Space overlaid events
space_signal = FilterBunches(inputs=filter_events.output_files(), filter_no_cuts=True, outputs=['%s_spaced.slcio' % signal_name], filter_event_interval=250)

## Print number of merged events
count_space_overlay = LCIOCount(inputs=space_signal.output_files())

## Run simulated events in readout to generate triggers
readout = JobManager(steering='readout',
                     inputs=space_signal.output_files(),
                     outputs=['%s_spaced_readout.slcio' % signal_name])

## Print number of readout events
count_readout = LCIOCount(inputs=readout.output_files())

## Run physics reconstruction
recon = JobManager(steering='recon',
                   inputs=readout.output_files(),
                   outputs=['%s_spaced_recon.slcio' % signal_name])

## Print number of recon events
count_recon = LCIOCount(inputs=recon.output_files())


## Add the components
job.add([filter_events, count_filter, space_signal,
         count_space_overlay, readout, count_readout, recon, count_recon])
