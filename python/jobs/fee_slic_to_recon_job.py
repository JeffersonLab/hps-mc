from hpsmc.generators import StdHepConverter
from hpsmc.tools import BeamCoords, AddMother, MergePoisson, RandomSample, MergeFiles
from hpsmc.tools import SLIC, JobManager, ExtractEventsWithHitAtHodoEcal, HPSTR, LCIOCount, LCIOMerge, StdHepCount

# Get job input file targets
inputs = list(job.input_files.values())

job.description = 'fee'

if 'event_interval' in job.params:
    event_int = job.params['event_interval']
else:
    event_int = 250

if 'nevents' in job.params:
    nevents = job.params['nevents']
else:
    nevents = 10000

if 'nbeams' in job.params:
    nbeams = job.params['nbeams']
else:
    nbeams = 2

# Input beam events (StdHep format)
beam_file_names = []
beam_slic_file_names = []
for i in range(1,nbeams+1):
    beam_file_names.append('beam_%i.stdhep'%i)
    beam_slic_file_names.append('beam_%i.slcio'%i)

# Check for expected input file targets
if beam_file_names[nbeams-1] not in inputs:
    raise Exception("Missing required input file '%s'" % beam_file_names[nbeams-1])

# Base name of intermediate beam files
beam_name = 'beam'

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

# Run simulated events in readout to generate triggers
readout = JobManager(steering='readout',
                     inputs=slic_beam_cat.output_files(),
                     outputs=['fee_readout.slcio'])

# Run physics reconstruction
recon = JobManager(steering='recon',
                   inputs=readout.output_files(),
                   outputs=['fee_recon.slcio'])
 
# Add the components
comps = []
for i in range(len(slic_beams)): comps.append(slic_beams[i])
comps.extend([slic_beam_cat, readout, recon])
job.add(comps)

