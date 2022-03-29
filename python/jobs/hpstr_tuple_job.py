"""
Tupilize slcio with hpstr
"""
from hpsmc.tools import HPSTR

job.description = 'Tupilize lcio'

tuple = HPSTR(run_mode=1, cfg='recon')

job.add([tuple])
