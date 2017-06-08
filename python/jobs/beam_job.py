#!/usr/bin/env python

"""
Python script for creating beam background events.

Based on this Auger script:

https://github.com/JeffersonLab/hps-mc/blob/master/scripts/mc_scripts/slic/beam.xml

"""

import os
from hpsmc.job import Job
from hpsmc.tools import StdHepTool

job = Job(name="beam job")
job.initialize()

if len(job.input_files) > 1:
    raise Exception("Too many input files (expected 1 but got %d)." % len(job.input_files))

input_file = "beam.stdhep"
base,ext = os.path.splitext(input_file) 

rot = StdHepTool(name="beam_coords",
                 inputs=[input_file],
                 outputs=[base+"_rot.stdhep"])

sample = StdHepTool(name="random_sample",
                    inputs=[base+"_rot.stdhep"],
                    outputs=[base+"_sampled"])

job.components = [rot, sample]

job.run()
