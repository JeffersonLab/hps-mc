"""!
@file slic_to_anaMC_job.py

Run SLIC to analysis using mcTuple.
"""
from hpsmc.tools import SLIC, HPSTR

job.description = 'slic to anaMC'

## Simulate signal events
slic = SLIC()

## Convert LCIO to ROOT
tuple = HPSTR(cfg='mcTuple')

## Run an analysis on the ROOT file
ana = HPSTR(cfg='ana')

## Add the components
job.add([slic, tuple, ana])
