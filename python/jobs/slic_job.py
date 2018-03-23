"""
Python script for generating 'wab-beam-tri' events.
"""

import sys, os

import hpsmc.func as func
from hpsmc.job import Job
from hpsmc.run_params import RunParameters
from hpsmc.generators import MG5, StdHepConverter
from hpsmc.tools import StdHepTool, SLIC, JobManager

# job init
job = Job(name="SLIC job")
job.initialize()
params = job.params

# create a SLIC component for each input file
for i in params.input_files.keys():
    fname = os.path.splitext(i)[0] + ".slcio"
    
    # generate events in slic
    slic = SLIC(description="Run detector simulation using SLIC",
                detector=params['detector'],
                inputs=[i],
                outputs=[fname],
                nevents=params['nevents'],
                ignore_returncode=True)

    job.components.append(slic)
                        
job.run()
