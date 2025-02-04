"""!
@file signal_pulser_overlay_to_recon_job.py

No idea what this is supposed to do.
"""

from hpsmc.tools import ExtractEventsWithHitAtHodoEcal, EvioToLcio, JobManager, FilterBunches, LCIOCount, HPSTR

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

## Filter signal events and catenate files before overlaying with pulser data
filter_events = ExtractEventsWithHitAtHodoEcal(inputs=signal_file_name,
                                               outputs=['%s_filt.slcio' % signal_name],
                                               event_interval=0, num_hodo_hits=1)

## Count filtered events
count_filter = LCIOCount(inputs=filter_events.output_files())

## Convert evio to lcio for raw pulser data
evio_to_lcio = EvioToLcio(steering='evio_to_lcio', inputs=pulser_file_name, output=['%s.slcio' % pulser_name])

## Count pulser events
count_pulser = LCIOCount(inputs=evio_to_lcio.output_files())

## Overlay signal with pulser data
overlay = JobManager(steering='overlay',
                     inputs=filter_events.output_files(),
                     overlay_file=pulser_file_name[0],
                     outputs=['%s.slcio' % signal_pulser_name])

## Space overlaid events
space_overlay = FilterBunches(inputs=overlay.output_files(),
                              filter_no_cuts=True,
                              outputs=['%s_spaced.slcio' % signal_pulser_name],
                              filter_event_interval=250)

## Print number of merged events
count_space_overlay = LCIOCount(inputs=space_overlay.output_files())

## Run simulated events in readout to generate triggers
readout = JobManager(steering='readout',
                     inputs=space_overlay.output_files(),
                     outputs=['%s_readout.slcio' % signal_pulser_name])

## Print number of readout events
count_readout = LCIOCount(inputs=readout.output_files())

## Run physics reconstruction
recon = JobManager(steering='recon',
                   inputs=readout.output_files(),
                   outputs=['%s_recon.slcio' % signal_pulser_name])

## Print number of recon events
count_recon = LCIOCount(inputs=recon.output_files())

## Convert LCIO to ROOT
cnv = HPSTR(inputs=recon.output_files(), cfg='cnv')

## Add the components
job.add([filter_events, count_filter, overlay, space_overlay,
         count_space_overlay, readout, count_readout, recon, count_recon, cnv])
