from hpsmc.generators import MG5, StdHepConverter
from hpsmc.tools import JobManager, HPSTR 

job.description = 'convert to hpstr analysis'

# Convert LCIO to ROOT
cnv = HPSTR(cfg='recon')

job.add([cnv])

