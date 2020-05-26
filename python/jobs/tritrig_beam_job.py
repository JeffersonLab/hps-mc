import sys, random

from hpsmc.job import Job
from hpsmc.generators import StdHepConverter
from hpsmc.tools import BeamCoords, AddMother, MergePoisson, RandomSample, MergeFiles
from hpsmc.tools import SLIC, JobManager, FilterBunches, HPSTR, LCIOCount

job = Job()

# Get job input file targets
inputs = job.input_files.values()

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

# Sample tritrig events using poisson distribution
sample = MergePoisson(lhe_file=tritrig_file_name, # for calculating mu
                      inputs=rot.output_files(),
                      outputs=['%s_sampled.stdhep' % tritrig_name],
                      nevents=500000)

# Transform beam events
rot_beam = BeamCoords(inputs=[beam_file_name],
                      outputs=['%s_rot.stdhep' % beam_name])

# Sample the beam events
sample_beam = RandomSample(inputs=rot_beam.output_files(),
                           outputs=['%s_sampled.stdhep' % beam_name])

# Merge signal and background events
merge = MergeFiles(inputs=[sample.output_files()[0],
                           sample_beam.output_files()[0]],
                   outputs=['%s.stdhep' % tritrig_beam_name])

# Run simulation
slic = SLIC(inputs=merge.output_files(),
            outputs=['%s.slcio' % tritrig_beam_name])

# Space events for readout simulation
filter_bunches = FilterBunches(nevents=2000000,
                               inputs=slic.output_files(),
                               outputs=['%s_filt.slcio' % tritrig_beam_name])

# Run simulated events in readout to generate triggers
readout = JobManager(steering='readout', 
                     inputs=filter_bunches.output_files(),
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
tuple = HPSTR(cfg='recon', 
              inputs=recon.output_files(),
              outputs=['%s_recon.root' % tritrig_beam_name])

# Run an analysis on the ROOT file
ana = HPSTR(cfg='ana',
            inputs=tuple.output_files(),
            outputs=['%s_ana.root' % tritrig_beam_name])
 
# Add the components
job.add([cnv, mom, rot, sample, rot_beam, sample_beam, merge, slic,
         filter_bunches, readout, count_readout, recon, count_recon, 
         tuple, ana])

# Run the job
job.run()
