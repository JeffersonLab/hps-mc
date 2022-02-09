from hpsmc.generators import MG5, StdHepConverter
from hpsmc.tools import SLIC, JobManager, FilterBunches, BeamCoords, AddMother, HPSTR

job.description = 'Run hps-sim with preexisting tritrig stdhep files'

# generate events in slic
sim = SLIC()

# insert empty bunches expected by pile-up simulation
filter_bunches = FilterBunches()

# Run simulated events in readout to generate triggers
readout = JobManager(steering='readout')

# Run physics reconstruction
recon = JobManager(steering='recon')

# Convert LCIO to ROOT
root_cnv = HPSTR(cfg='recon')

# Run an analysis on the ROOT file
ana = HPSTR(cfg='ana')

# Set persistency tags for output files
job.ptag('sim', 'tritrig_unweighted_events_mom_rot.slcio')
job.ptag('readout', 'tritrig_unweighted_events_mom_rot_filt_readout.slcio')
job.ptag('recon', 'tritrig_unweighted_events_mom_rot_filt_readout_recon.slcio')
job.ptag('ana', 'tritrig_unweighted_events_mom_rot_filt_readout_recon_ana.root')
 
# Add job components
job.add([sim, filter_bunches, readout, recon, root_cnv, ana])
