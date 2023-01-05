from hpsmc.generators import MG5, StdHepConverter
from hpsmc.tools import JobManager, HPSTR 

job.description = 're-run reconstruction and convert to hpstr analysis'

# Run physics reconstruction
recon = JobManager(steering='recon')

# Convert LCIO to ROOT
cnv = HPSTR(cfg='recon')

# Run an analysis on the ROOT file
#ana = HPSTR(cfg='ana')

# Add job components
job.add([recon, cnv])
#job.add([recon, cnv, ana])
#job.add([recon])
#job.add([cnv])

