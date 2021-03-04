from hpsmc.generators import StdHepConverter
from hpsmc.tools import BeamCoords, AddMother, MergePoisson, RandomSample, MergeFiles
from hpsmc.tools import SLIC, JobManager, ExtractEventsWithHitAtHodoEcal, HPSTR, LCIOCount, LCIOMerge, StdHepCount

job.description = 'slic to anaMC'

# Simulate signal events
slic = SLIC()

# Convert LCIO to ROOT
tuple = HPSTR(cfg='mcTuple')

# Run an analysis on the ROOT file
ana = HPSTR(cfg='ana')
 
# Add the components
job.add([slic, tuple, ana])

