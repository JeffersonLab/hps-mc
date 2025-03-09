"""!
@file data_cnv_job.py

Convert EVIO to LCIO and then process with HPSTR to produce a recon tuple.
"""
from hpsmc.tools import EvioToLcio, HPSTR, SQLiteProc

job.description = 'EVIO converter'

sqlite = SQLiteProc()

cnv = EvioToLcio(steering='recon')

tuple = HPSTR(run_mode=1, cfg='recon')

job.add([sqlite, cnv, tuple])
