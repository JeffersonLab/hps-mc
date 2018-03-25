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

if not len(input_files):
    raise Exception("Input file list is empty!")

# create a SLIC component for each input file
output_files = []
for i in params.input_files.keys():
    fname = os.path.splitext(i)[0] + ".slcio"
    output_files.append(fname)
    
    # generate events in slic
    slic = SLIC(description="Run detector simulation using SLIC",
                detector=params['detector'],
                inputs=[i],
                outputs=[fname],
                nevents=params['nevents'],
                ignore_returncode=True)

    job.components.append(slic)

# concatenate LCIO files if a single output file was given in the JSON
if len(input_files) > 1 and len(output_files) == 1:
    concat = LCIOConcat(inputs=[output_files], 
                        output=[os.path.splitext(params.output_files.keys()[0])[0]])
    job.components.append(concat)
                        
job.run()
