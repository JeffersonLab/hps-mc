from hpsmc.generators import StdHepConverter
from hpsmc.tools import BeamCoords, AddMother, MergePoisson, RandomSample, MergeFiles
from hpsmc.tools import SLIC, JobManager, ExtractEventsWithHitAtHodoEcal, HPSTR, LCIOCount, LCIOMerge, StdHepCount

# Get job input file targets
inputs = list(job.input_files.values())

job.description = 'tritrig beam'

if 'event_interval' in job.params:
    event_int = job.params['event_interval']
else:
    event_int = 250

if 'nevents' in job.params:
    nevents = job.params['nevents']
else:
    nevents = 10000

# Input tritrig events (LHE format)
tritrig_file_name = 'tritrig_events.stdhep'

# Input beam events (StdHep format)
beam_file_names = []
beam_slic_file_names = []
for i in range(1,11):
    beam_file_names.append('beam_%i.stdhep'%i)
    beam_slic_file_names.append('beam_%i.slcio'%i)

# Check for expected input file targets
if tritrig_file_name not in inputs:
    raise Exception("Missing required input file '%s'" % tritrig_file_name)
if beam_file_names[1] not in inputs:
    raise Exception("Missing required input file '%s'" % beam_file_names[1])

# Base name of intermediate tritrig files
tritrig_name = 'tritrig'

# Base name of intermediate beam files
beam_name = 'beam'

# Base name of merged files
tritrig_beam_name = 'tritrig-beam'

# Simulate signal events
slic_tt = SLIC(inputs=[tritrig_file_name],
            outputs=['%s.slcio' % tritrig_name])

# Space signal events before merging
filter_bunches_tt = ExtractEventsWithHitAtHodoEcal(inputs=slic_tt.output_files(),
                                                   outputs=['%s_filt.slcio' % tritrig_name],
                                                   event_interval=event_int, num_hodo_hits=0)

# Simulate beam events
slic_beams = [] 
for i in range(len(beam_file_names)):
    slic_beams.append(SLIC(inputs=[beam_file_names[i]],
                      outputs=[beam_slic_file_names[i]],
                      nevents=nevents*event_int,
                      ignore_job_params=['nevents'] )
                     )

# concatonate beam events before merging
slic_beam_cat = ExtractEventsWithHitAtHodoEcal(inputs=beam_slic_file_names,
                                                   outputs=['beam_cat.slcio'],
                                                   event_interval=0, num_hodo_hits=0)

# Merge signal and beam events
merge = LCIOMerge(inputs=[filter_bunches_tt.outputs[0],
                          slic_beam_cat.outputs[0]],
                          outputs=['%s.slcio' % tritrig_beam_name],
                          ignore_job_params=['nevents'])

# Run simulated events in readout to generate triggers
readout = JobManager(steering='readout',
                     inputs=merge.output_files(),
                     outputs=['%s_readout.slcio' % tritrig_beam_name])

# Run physics reconstruction
recon = JobManager(steering='recon',
                   inputs=readout.output_files(),
                   outputs=['%s_recon.slcio' % tritrig_beam_name])

# Convert LCIO to ROOT
#tuple = HPSTR(cfg='recon', 
#              inputs=recon.output_files(),
#              outputs=['%s_recon.root' % tritrig_beam_name])

# Run an analysis on the ROOT file
#ana = HPSTR(cfg='ana',
#            inputs=tuple.output_files(),
#            outputs=['%s_ana.root' % tritrig_beam_name])
 
# Add the components
comps = [slic_tt, filter_bunches_tt]
for i in range(len(slic_beams)): comps.append(slic_beams[i])
comps.extend([slic_beam_cat, merge, readout, recon])
job.add(comps)

