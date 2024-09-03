"""!
@file hpstr_ana_job.py

HPSTR analysis.
"""
from hpsmc.tools import HPSTR

job.description = 'HPSTR analysis'

## Run an analysis on the ROOT file
ana = HPSTR(cfg='ana')

job.add([ana])
