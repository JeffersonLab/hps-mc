"""
Convert EVIO to LCIO and then process with HPSTR to produce a recon tuple.
"""

from hpsmc.tools import EvioToLcio, HPSTR

cnv = EvioToLcio(steering='recon')

tuple = HPSTR(run_mode=1, cfg='recon')

job.add([cnv, tuple])
