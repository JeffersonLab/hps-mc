from hpsmc.tools import LCIOCount

output_files = sorted(job.params.output_files.keys())
if len(output_files) < 1:
    raise Exception("Not enough output files were provided (at least 1 required).")

nevents = job.params.nevents

count = LCIOCount(minevents=nevents, inputs=output_files)

job.components = [count]
