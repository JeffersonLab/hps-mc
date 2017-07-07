#!/usr/bin/env python

"""
Job for concatenating LCIO files together.
"""

from hpsmc.job import Job
from hpsmc.tools import LCIOConcat

job = Job(name="LCIO concat job")
job.initialize()

input_files = sorted(job.params.input_files.keys())

if len(input_files) < 2:
    raise Exception("Not enough input files were provided.")

print input_files

concat = LCIOConcat(inputs=input_files, outputs=["events_combined.slcio"])

job.components = [concat]

job.run()
