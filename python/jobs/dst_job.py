#!/usr/bin/env python

"""
Create ROOT DST output from LCIO recon files using the HPS DST Maker.
"""

from hpsmc.job import Job
from hpsmc.tools import DST 

# Initialize the job.
job = Job(name="DST job")
job.initialize()
params = job.params
input_files = job.params.input_files
output_files = job.params.output_files

# Run DST maker.
dst = DST(inputs=input_files.keys(),
        outputs=output_files.keys())

# Add job components.
job.components.append(dst)

# Run the job.
job.run()
