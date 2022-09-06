"""!
@file recon_evio_job.py

Run hps-java reconstruction on data files.
"""
from hpsmc.tools import EvioToLcio

job.description = 'hps-java recon'

## Assign ptags for output
input_files = list(job.input_files.values())

## Run physics reconstruction
reco = EvioToLcio(steering='recon', inputs=input_files, outputs=['recon.slcio'])

job.add([reco])
