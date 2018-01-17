#!/usr/bin/env python

"""
Creates a ROOT tree from one or more input tuples in text format.
"""

from hpsmc.job import Job
from hpsmc.tools import MakeTree

# Initialize the job.
job = Job(name="Make ROOT tree job")
job.initialize()
input_files = job.params.input_files
output_files = job.params.output_files

# Setup input and output files.
txt_files = input_files.keys()
output_file = output_files.keys()[0]

# Make ROOT Tree.
make_tree = MakeTree(inputs=txt_files,
                     outputs=[output_file])

# run the job
job.components=[make_tree]
job.run()
