"""!
@file hpstr_job.py

HPSTR reconstruction and analysis.
"""
from hpsmc.tools import HPSTR

job.description = 'HPSTR recon and analysis'

# Convert LCIO to ROOT
cnv = HPSTR(cfg='recon')

# Run an analysis on the ROOT file
ana = HPSTR(cfg='ana')

job.add([cnv, ana])
