#!/usr/bin/env python

"""
Creates text format, single event N-tuples from LCIO recon file inputs.
"""

import sys, random, os.path

from hpsmc.job import Job
from hpsmc.tools import JobManager, TarFiles

# Initialize the job.
job = Job(name="Make tuples job")
job.initialize()
params = job.params
input_files = job.params.input_files
output_files = job.params.output_files

# Get base output name from output file key.
output_archive = job.params.output_files.keys()[0]
base,ext = os.path.splitext(output_archive)

# Make text tuple outputs.
make_tuples = JobManager(steering_resource=params.tuple_steering,
                   run=params.run,
                   detector=params.detector,
                   inputs=input_files.values(),
                   outputs=[base])

# Make list of files to archive.
tuple_files = []
for tuple_type in ["fee", "moller", "tri"]:
                   #, "fulltruth"]:
    tuple_file = base + "_" + tuple_type + ".txt"
    tuple_files.append(tuple_file)
    
# Tar the files into an output archive.
tar_files = TarFiles(inputs=tuple_files,
                     outputs=[output_archive])

# run the job
job.components=[make_tuples, tar_files]
job.run()
