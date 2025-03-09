"""!
@file data_cnv_job.py

Convert EVIO to LCIO and then process with HPSTR to produce a recon tuple.
"""
from hpsmc.tools import EvioToLcio, HPSTR

print("ZZ -> Print -> Modi") 

print(job.input_files) 

job.description = 'EVIO converter'

cnv = EvioToLcio(steering='recon')

tuple = HPSTR(run_mode=1, cfg='recon')

job.add([cnv, tuple])
