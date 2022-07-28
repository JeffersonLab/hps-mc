"""!
Job to count number of events in LCIO file.
"""
from hpsmc.tools import LCIOCount

job.description = 'LCIO count'

output_files = sorted(job.output_files.keys())
if len(output_files) < 1:
    raise Exception("Not enough output files were provided (at least 1 required).")

nevents = job.params['nevents']

count = LCIOCount(minevents=nevents, inputs=output_files)

job.add([count])
