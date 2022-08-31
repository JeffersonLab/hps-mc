"""!
Run hps-java reconstruction.
"""
from hpsmc.tools import EvioToLcio

job.description = 'hps-java recon'

## Assign ptags for output
input_files = list(job.input_files.values())

## Run physics reconstruction
reco = EvioToLcio(steering='recon', inputs=input_files, outputs=['recon.slcio'])

job.add([reco])
