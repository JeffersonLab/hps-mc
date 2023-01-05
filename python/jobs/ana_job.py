from hpsmc.generators import MG5, StdHepConverter
from hpsmc.tools import JobManager, HPSTR 

job.description = 're-run reconstruction and convert to hpstr analysis'

# Run an analysis on the ROOT file
ana = HPSTR(cfg='ana')

# Add job components
job.add([ana])

