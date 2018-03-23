"""
Python script for running trigger simulation with EngRun2015 configuration.
"""

import sys, random, os

from hpsmc.job import Job
from hpsmc.tools import JobManager

# engrun2015 default settings
def_params = {
    "detector": "HPS-EngRun2015-Nominal-v6-0-fieldmap",
    "run": 5772,
    "readout_steering": "/org/hps/steering/readout/EngineeringRun2015TrigPairs1_Pass2.lcsim"
}

# job init
job = Job(name="engrun2015 readout job")
job.set_default_params(def_params)
job.initialize()
params = job.params

# set number of events if present in params
nevents = -1
if "nevents" in params:
    nevents = params['nevents']

# run readout on all input files, assigning input files to output files from JSON names
for io in zip(sorted(params.input_files.keys()), sorted(params.output_files.keys())):
    
    # run simulated events in readout to generate triggers
    readout = JobManager(description="Run the readout simulation to create triggers",
                         steering_resource=params['readout_steering'],
                         run=params['run'],
                         detector=params['detector'],
                         inputs=[io[0]],
                         outputs=[os.path.splitext(io[1])[0]],
                         nevents=nevents)

    job.components.append(readout)
                        
job.run()
