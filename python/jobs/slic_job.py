"""
Python script for generating 'wab-beam-tri' events.
"""

import os

from hpsmc.job import Job
from hpsmc.tools import SLIC, LCIOConcat

# job init
job = Job(name="SLIC job")
job.initialize()
params = job.params

if not len(params.input_files):
    raise Exception("Input file list is empty!")

if (len(params.input_files) != len(params.output_files)) and len(params.output_files) != 1:
    raise Exception("Input and output file lists are not the same length!")

# create a SLIC component for each input file
output_files = []
for io in zip(sorted(params.input_files.keys()), sorted(params.output_files.keys())):
    # each output file is named from the JSON list
    fname = os.path.splitext(io[1])[0] + ".slcio"
    output_files.append(fname)
    
    # generate events in slic using the input stdhep file
    slic = SLIC(description="Run detector simulation using SLIC",
                detector=params['detector'],
                inputs=[io[0]],
                outputs=[fname],
                nevents=params['nevents'],
                ignore_returncode=True)

    job.components.append(slic)

# concatenate LCIO files if a single output file was given in the JSON
if len(params.input_files) > 1 and len(params.output_files) == 1:
    concat = LCIOConcat(inputs=[output_files], 
                        outputs=[os.path.splitext(params.output_files.keys()[0])[0]])
    job.components.append(concat)
                        
job.run()
