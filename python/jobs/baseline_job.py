"""
Convert EVIO to LCIO and then process with HPSTR to produce a recon tuple.
"""

from hpsmc.tools import EvioToLcio, HPSTR

job.description = 'EVIO converter'

cnv = EvioToLcio(steering='recon')

#recon = HPSTR(run_mode=1, cfg='recon')

raw = HPSTR(run_mode=1, cfg='raw')

hist = HPSTR(is_data=1, cfg='hist')

job.add([cnv, raw, hist])
