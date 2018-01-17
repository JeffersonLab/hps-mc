#!/usr/bin/env python

"""
Creates a tar archive with ROOT tuple files from LCIO recon file inputs.
"""

import sys, random, os.path

from hpsmc.job import Job
from hpsmc.tools import JobManager, TarFiles, MakeTree

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
job.components.append(make_tuples)

# Create components to convert each of the text tuple files to ROOT.
tuple_outputs = []
for tuple_type in ["fee", "moller", "tri"]:
    #, "fulltruth"]:
    tuple_base = base + "_" + tuple_type
    tuple_input = tuple_base + ".txt"
    tuple_output = tuple_base + ".root"
    tuple_outputs.append(tuple_output)
    make_tree = MakeTree(inputs=[tuple_input], outputs=[tuple_output])
    job.components.append(make_tree)
       
# Tar the files into an output archive.
tar_files = TarFiles(inputs=tuple_outputs,
                     outputs=[output_archive])
job.components.append(tar_files)

# run the job
job.run()
