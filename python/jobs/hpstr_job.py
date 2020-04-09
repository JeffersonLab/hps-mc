from hpsmc.job import Job
from hpsmc.tools import HPSTR 

job = Job()

# Convert LCIO to ROOT
cnv = HPSTR(cfg='reco')

# Run an analysis on the ROOT file
ana = HPSTR(cfg='ana')

job.add([cnv, ana])
job.run()
