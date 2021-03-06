from hpsmc.generators import StdHepConverter
from hpsmc.tools import BeamCoords, AddMother, MergePoisson, RandomSample, MergeFiles
from hpsmc.tools import SLIC, JobManager, FilterBunches, HPSTR, LCIOCount, LCIOMerge, StdHepCount

# Get job input file targets
inputs = list(job.input_files.values())

job.description = 'tritrig beam'

if 'event_interval' in job.params:
    event_interval = job.params['event_interval']
else:
    event_interval = 250

if 'nevents' in job.params:
    nevents = job.params['nevents']
else:
    nevents = 10000

# Input tritrig events (LHE format)
tritrig_file_name = 'tritrig_events.lhe.gz'

# Input beam events (StdHep format)
beam_file_name = 'beam.stdhep'

# Check for expected input file targets
if tritrig_file_name not in inputs:
    raise Exception("Missing required input file '%s'" % tritrig_file_name)
if beam_file_name not in inputs:
    raise Exception("Missing required input file '%s'" % beam_file_name)

# Base name of intermediate tritrig files
tritrig_name = 'tritrig'

# Base name of intermediate beam files
beam_name = 'beam'

# Base name of merged files
tritrig_beam_name = 'tritrig-beam'

# Convert LHE output to stdhep
cnv = StdHepConverter(inputs=[tritrig_file_name],
                      outputs=['%s.stdhep' % tritrig_name])

# Add mother particle to tag trident particles
mom = AddMother(inputs=cnv.output_files(),
                outputs=['%s_mom.stdhep' % tritrig_name])

# Rotate events into beam coords
rot = BeamCoords(inputs=mom.output_files(),
                outputs=['%s_rot.stdhep' % tritrig_name])

# Simulate signal events
slic = SLIC(inputs=rot.output_files(),
            outputs=['%s.slcio' % tritrig_name])

# Space signal events before merging
filter_bunches = FilterBunches(inputs=slic.output_files(),
                               outputs=['%s_filt.slcio' % tritrig_name])

# Count filtered events
count_filter = LCIOCount(inputs=filter_bunches.output_files())

# Transform beam events
rot_beam = BeamCoords(inputs=[beam_file_name],
                      outputs=['%s_rot.stdhep' % beam_name])

# Sample the beam events
sample_beam = RandomSample(inputs=rot_beam.output_files(),
                           outputs=['%s_sampled.stdhep' % beam_name],
                           nevents=500000,
                           ignore_job_params=['nevents'])

# Print number of beam sampled events
count_beam = StdHepCount(inputs=sample_beam.output_files())

# Simulate beam events
slic_beam = SLIC(inputs=sample_beam.output_files(),
                 outputs=['%s.slcio' % beam_name],
                 nevents=nevents*event_interval,
                 ignore_job_params=['nevents'])

# Merge signal and beam events
merge = LCIOMerge(inputs=[filter_bunches.output_files()[0],
                          slic_beam.output_files()[0]],
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

# Convert LCIO to ROOT
#tuple = HPSTR(cfg='recon', 
#              inputs=recon.output_files(),
#              outputs=['%s_recon.root' % tritrig_beam_name])

# Run an analysis on the ROOT file
#ana = HPSTR(cfg='ana',
#            inputs=tuple.output_files(),
#            outputs=['%s_ana.root' % tritrig_beam_name])
 
# Add the components
job.add([cnv, mom, rot, slic, filter_bunches, count_filter, rot_beam, sample_beam,
         count_beam, slic_beam, merge, count_merge, readout, count_readout, 
         recon, count_recon])

