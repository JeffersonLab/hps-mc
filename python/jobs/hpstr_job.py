from hpsmc.job import Job
from hpsmc.tools import HPSTR 

# Initialize the job.
job = Job(name="HPSTR job")
job.initialize()

params = job.params
input_files = params.input_files
output_files = params.output_files

# Figure out input and output file lists depending on whether 
# JSON data is a dict or list.
if isinstance(params.input_files, dict):
    infiles = params.input_files.keys()
else:
    infiles = params.input_files
if isinstance(params.output_files, dict):
    outfiles = params.output_files.keys()
else:
    outfiles = params.output_files

hpstr = HPSTR(cfg="recoTuple_cfg.py",
              inputs=infiles,
              outputs=outfiles)

job.components.append(hpstr)

job.run()
