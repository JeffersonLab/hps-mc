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
input_files = sorted(job.params.input_files.keys())
output_files = job.params.output_files

output_base,ext = os.path.splitext(input_files[0])
if (len(input_files) > 1):
    # Strip off extension from name if multiple file inputs.
    output_base,ext = os.path.splitext(output_base)

# Job parameters may optionally specify number of events to read from LCIO file.
if hasattr(params, "nevents"):
    nevents = params.nevents
else:
    nevents = -1

# Make text tuple outputs.
make_tuples = JobManager(steering_resource=params.tuple_steering,
                   run=params.run,
                   detector=params.detector,
                   inputs=input_files,
                   outputs=[output_base],
                   nevents=nevents)
job.components.append(make_tuples)

# Create components to convert each of the text tuple files to ROOT.
tuple_outputs = []
for tuple_type in ["fee", "moller", "tri", "fulltruth"]:
    tuple_base = output_base + "_" + tuple_type
    tuple_input = tuple_base + ".txt"
    tuple_output = tuple_base + ".root"
    tuple_outputs.append(tuple_output)
    make_tree = MakeTree(inputs=[tuple_input], outputs=[tuple_output])
    job.components.append(make_tree)
       
# Tar the files into an output archive.
output_archive = output_base
tar_files = TarFiles(inputs=tuple_outputs,
                     outputs=[output_archive])
job.components.append(tar_files)

# run the job
job.run()
