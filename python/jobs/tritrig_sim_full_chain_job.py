"""!
Tritrig signal generation to recon using hps-sim with no beam backgrounds.
"""
from hpsmc.generators import MG5, StdHepConverter
from hpsmc.tools import Sim, JobManager, FilterBunches, BeamCoords, AddMother, HPSTR

job.description = 'tritrig signal generation to recon using hps-sim with no beam backgrounds'

## Generate tritrig in MG5
mg = MG5(name='tritrig')

## Convert LHE output to stdhep
stdhep_cnv = StdHepConverter()

## Add mother particle to tag trident particles
mom = AddMother()

## Rotate events into beam coords
rot = BeamCoords()

## \todo cleanup
# Print results
#p = StdHepTool(name="print_stdhep")

## Simulate events
sim = Sim()

## insert empty bunches expected by pile-up simulation
filter_bunches = FilterBunches()

## Run simulated events in readout to generate triggers
readout = JobManager(steering='readout')

## Run physics reconstruction
recon = JobManager(steering='recon')

## Convert LCIO to ROOT
root_cnv = HPSTR(cfg='recon')

## Run an analysis on the ROOT file
ana = HPSTR(cfg='ana')

## Set persistency tags for output files
job.ptag('gen', 'tritrig_unweighted_events_mom_rot.stdhep')
job.ptag('sim', 'tritrig_unweighted_events_mom_rot.slcio')
job.ptag('readout', 'tritrig_unweighted_events_mom_rot_filt_readout.slcio')
job.ptag('recon', 'tritrig_unweighted_events_mom_rot_filt_readout_recon.slcio')
job.ptag('ana', 'tritrig_unweighted_events_mom_rot_filt_readout_recon_ana.root')
 
## Add job components
job.add([mg, stdhep_cnv, mom, rot, sim, filter_bunches, readout, recon, root_cnv, ana])
