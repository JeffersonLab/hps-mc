"""
Convert EVIO to LCIO and then process with HPSTR to produce a recon tuple.
"""

from hpsmc.job import Job
from hpsmc.tools import EvioToLcio, HPSTR

job = Job()

cnv = EvioToLcio(steering='recon')

tuple = HPSTR(run_mode=1, cfg='recon')

job.add([cnv, tuple])
job.run()