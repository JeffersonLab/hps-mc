from hpsmc.tools import FilterBunches, JobManager, LCIOCount, LCIOMerge

job.description = 'signal-beam from merge to recon'

# Get job input file targets
inputs = list(job.input_files.values())

# Input signal events (slcio format)
signal_file_name = []

# Input beam events (slcio format)
beam_file_name = []

for input in inputs:
    if "signal" in input:
        signal_file_name.append(input)
    if "beam" in input:
        beam_file_name.append(input)

# Check for expected input file targets
if len(signal_file_name) == 0:
    raise Exception("Missing required input file(s) for signal")
if len(beam_file_name) == 0:
    raise Exception("Missing required input file(s) for beam")

# Base name of intermediate signal files
signal_name = 'signal'

# Base name of intermediate beam files
beam_name = 'beam'

# Base name of merged files
signal_beam_name = 'signal-beam'

# Filter and space signal events and catenate files before merging
filter_events = FilterBunches(inputs=signal_file_name,
                              outputs=['%s_filt.slcio' % signal_name],
                              filter_event_interval=250, filter_ecal_pairs=True,
                              filter_ecal_hit_ecut=0.1)

# Count filtered events
count_filter = LCIOCount(inputs=filter_events.output_files())

# catenate beam files before merging
catenate_beam = FilterBunches(inputs=beam_file_name,
                              outputs=['%s_filt.slcio' % beam_name],
                              filter_event_interval=0, filter_ecal_pairs=False,
                              filter_no_cuts=True, filter_ecal_hit_ecut=-0.1)

# Count beam events
count_beam = LCIOCount(inputs=catenate_beam.output_files())

# Merge signal and beam events
merge = LCIOMerge(inputs=[filter_events.output_files()[0],
                          catenate_beam.output_files()[0]],
                  outputs=['%s.slcio' % signal_beam_name],
                  ignore_job_params=['nevents'])

# Print number of merged events
count_merge = LCIOCount(inputs=merge.output_files())

# Run simulated events in readout to generate triggers
readout = JobManager(steering='readout',
                     inputs=merge.output_files(),
                     outputs=['%s_readout.slcio' % signal_beam_name])

# Print number of readout events
count_readout = LCIOCount(inputs=readout.output_files())

# Run physics reconstruction
recon = JobManager(steering='recon',
                   inputs=readout.output_files(),
                   outputs=['%s_recon.slcio' % signal_beam_name])

# Print number of recon events
count_recon = LCIOCount(inputs=recon.output_files())

# Add the components
job.add([filter_events, count_filter, catenate_beam, count_beam, merge,
         count_merge, readout, count_readout, recon, count_recon])
