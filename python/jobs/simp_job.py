from hpsmc.generators import MG5, StdHepConverter
from hpsmc.tools import SLIC, JobManager, FilterBunches, StdHepTool, MoveFiles, BeamCoords

job.description = 'SIMP generation to recon'

# Generate tritrig in MG5
mg = MG5(name='simp',
         run_card='run_card.dat',
         param_card='param_card.dat',
         event_types=['unweighted'])

# Move LHE file
# TODO: remove this step
mv = MoveFiles(inputs=['simp_unweighted_events.lhe.gz'],
               outputs=['simp.lhe.gz'])

# Convert LHE output to stdhep
cnv = StdHepConverter()

# Rotate into beam coords
rot = BeamCoords()

# Run events in slic
slic = SLIC()

# Insert empty bunches expected by pile-up simulation
filter_bunches = FilterBunches(nevents=2000000)

# Run simulated events in readout to generate triggers
readout = JobManager(steering='readout')

# Run physics reconstruction
recon = JobManager(steering='recon')
 
# Run the job
job.add([mg, mv, cnv, rot, slic, filter_bunches, readout, recon])
