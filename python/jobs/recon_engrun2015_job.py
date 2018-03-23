#!/usr/bin/env python

"""

Python script for running EngRun2015 physics reconstruction.

This script can be run in three different ways:

- Providing one input file and one output file.

- Providing multiple input and output files.

- Providing multiple input files and one output file.

The first two cases are handled in the same way, by writing an output for the input.

The last case sends all the input files to the JobManager and writes a single output.

Output file names are in all cases based on the keys provided by the user in the JSON file.

"""

import os

from hpsmc.job import Job
from hpsmc.tools import JobManager

# engrun2015 default parameters
def_params = {
    "nevents": -1,
    "detector": "HPS-EngRun2015-Nominal-v6-0-fieldmap",
    "run": 5772,
    "recon_steering": "/org/hps/steering/recon/EngineeringRun2015FullReconMC.lcsim",
}

# job init
job = Job(name="MC reconstruction using EngRun2015 configuration")
job.set_default_params(def_params)
job.initialize()
params = job.params

# at least one input file is required
if not len(params.input_files):
    raise Exception("Input file list is empty!")

# at least one output file is required
if not len(params.output_files):
    raise Exception("Output file list is empty!")

# if using multiple input and multiple output files, the lists have to be same length
if (len(params.input_files) != len(params.output_files)) and len(params.output_files) != 1:
    raise Exception("Input and output file lengths have different lengths!")

if len(params.input_files) == len(params.output_files):
    # multiple input files to multiple output files
    for io in zip(sorted(params.input_files.keys()), sorted(params.output_files.keys())):

        # run physics reconstruction for each input file and write a separate output file
        recon = JobManager(description="Run the MC recon",
                           steering_resource=params['recon_steering'],
                           run=params['run'],
                           detector=params['detector'],
                           inputs=[io[0]],
                           outputs=[os.path.splitext(io[1])[0]])

        job.components.append(recon)
elif len(params.output_files) == 1:
    # write a single output file from multiple input files 
    recon = JobManager(description="Run the MC recon",
                       steering_resource=params['recon_steering'],
                       run=params['run'],
                       detector=params['detector'],
                       inputs=params.input_files.keys(),
                       outputs=[os.path.splitext(params.output_files.keys()[0])[0]])
    job.components.append(recon)
else:
    # do not think this should ever happen (?)
    raise Exception("Input and output file lists do not make sense!")

job.run()
