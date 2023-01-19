"""!
@file tritrig_beam_job.py

Merge tritrig and beam events, simulate signal events, and detector readout.
"""
import os
from hpsmc.tools import SLIC, JobManager, LCIOCount, LCIOMerge, ExtractEventsWithHitAtHodoEcal

# Get job input file targets
inputs = list(job.input_files.values())

job.description = 'tritrig beam slic to reco'

if 'nevents' in job.params:
    nevents = job.params['nevents']
else:
    nevents = 10000

# Input tritrig events (stdhep format)
tritrig_file_name = 'tritrig_events.stdhep'

# Input beam events (StdHep format)
beam_file_names = []
beam_slic_file_names = []
for i in range(len(inputs)):
    if inputs[i] != tritrig_file_name:
        beam_file_names.append(inputs[i])
        filename, file_extension = os.path.splitext(inputs[i])
        beam_slic_file = filename + '.slcio'
        beam_slic_file_names.append(beam_slic_file)

# Base name of intermediate tritrig files
tritrig_name = 'tritrig'

# Base name of intermediate beam files
beam_name = 'beam'

# Base name of merged files
tritrig_beam_name = 'tritrig_beam'

# Simulate signal events
slic = SLIC(inputs=[tritrig_file_name],
            outputs=['%s.slcio' % tritrig_name])

# Space signal events before merging
filter_bunches = ExtractEventsWithHitAtHodoEcal(inputs=slic.output_files(),
                                                outputs=['%s_filt.slcio' % tritrig_name],
                                                event_interval=250, num_hodo_hits=0)

# Count filtered events
count_filter = LCIOCount(inputs=filter_bunches.output_files())

# Simulate beam events
slic_beams = []
for i in range(len(beam_file_names)):
    slic_beams.append(SLIC(inputs=[beam_file_names[i]],
                      outputs=[beam_slic_file_names[i]],
                      nevents=nevents * 250,
                      ignore_job_params=['nevents'])
                      )

# concatonate beam events before merging
slic_beam_cat = ExtractEventsWithHitAtHodoEcal(inputs=beam_slic_file_names,
                                               outputs=['beam_cat.slcio'],
                                               event_interval=0, num_hodo_hits=0)

# Merge signal and beam events
merge = LCIOMerge(inputs=[filter_bunches.output_files()[0],
                          slic_beam_cat.outputs[0]],
                  outputs=['%s.slcio' % tritrig_beam_name],
                  ignore_job_params=['nevents'])

# Print number of merged events
count_merge = LCIOCount(inputs=merge.output_files())

# Run simulated events in readout to generate triggers
readout = JobManager(steering='readout',
                     inputs=merge.output_files(),
                     outputs=['%s_readout.slcio' % tritrig_beam_name])

# Print number of readout events
count_readout = LCIOCount(inputs=readout.output_files())

# Run physics reconstruction
recon = JobManager(steering='recon',
                   inputs=readout.output_files(),
                   outputs=['%s_recon.slcio' % tritrig_beam_name])

# Print number of recon events
count_recon = LCIOCount(inputs=recon.output_files())

comps = [slic, filter_bunches, count_filter]
for i in range(len(slic_beams)):
    comps.append(slic_beams[i])
comps.extend([slic_beam_cat, merge, count_merge, readout, count_readout, recon, count_recon])
job.add(comps)
