from hpsmc.generators import MG5, StdHepConverter
from hpsmc.tools import SLIC, JobManager, FilterBunches, BeamCoords, AddMother

job.description = 'tritrig generation to recon'

# Generate tritrig in MG5
mg = MG5(name='tritrig')

# Convert LHE output to stdhep
cnv = StdHepConverter()

# Add mother particle to tag trident particles
mom = AddMother()

# Rotate events into beam coords
rot = BeamCoords()

# Print results
#p = StdHepTool(name="print_stdhep")

# generate events in slic
slic = SLIC()

# insert empty bunches expected by pile-up simulation
filter_bunches = FilterBunches(nevents=2000000)

# Run simulated events in readout to generate triggers
readout = JobManager(steering='readout')
#, append_tok='readout'

# Run physics reconstruction
recon = JobManager(steering='recon')
#, append_tok='recon'

# Set persistency tag for final output file
job.ptag('recon', 'tritrig_events_mom_rot_filt_readout_recon.slcio')
 
# Add job components
job.add([mg, cnv, mom, rot, slic, filter_bunches, readout, recon])
