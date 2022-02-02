from hpsmc.generators import MG5, StdHepConverter
from hpsmc.tools import Sim, JobManager, FilterBunches, BeamCoords, AddMother

job.description = 'tritrig signal generation to recon using hps-sim with no beam backgrounds'

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
sim = Sim()

# insert empty bunches expected by pile-up simulation
filter_bunches = FilterBunches()

# Run simulated events in readout to generate triggers
readout = JobManager(steering='readout')

# Run physics reconstruction
recon = JobManager(steering='recon')

# Set persistency tags for output files
job.ptag('gen', 'tritrig_unweighted_events_mom_rot.stdhep')
job.ptag('sim', 'tritrig_unweighted_events_mom_rot.slcio')
job.ptag('readout', 'tritrig_unweighted_events_mom_rot_filt_readout.slcio')
job.ptag('recon', 'tritrig_unweighted_events_mom_rot_filt_readout_recon.slcio')
 
# Add job components
job.add([mg, cnv, mom, rot, sim, filter_bunches, readout, recon])
