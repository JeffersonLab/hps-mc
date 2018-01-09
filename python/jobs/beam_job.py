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

params = job.params

if len(job.input_files) > 1:
    raise Exception("Too many input files (expected 1 but got %d)." % len(job.input_files))

input_file = "beam.stdhep"
base,ext = os.path.splitext(input_file)

rot = StdHepTool(name="beam_coords",
                 inputs=[input_file],
                 outputs=[base+"_rot.stdhep"])

"""
Add optional beam parameters.
"""
if hasattr(params, "beam_sigma_x"):
    rot.args.extend(["-x", str(params.beam_sigma_x)])
if hasattr(params, "beam_sigma_y"):
    rot.args.extend(["-y", str(params.beam_sigma_y)])
if hasattr(params, "target_z"):
    rot.args.extend(["-z", str(params.target_z)])
if hasattr(params, "beam_rotation"):
    rot.args.extend(["-r", str(params.beam_rotation)])
if hasattr(params, "beam_skew"):
    rot.args.extend(["-q", str(params.beam_skew)])
            
sample = StdHepTool(name="random_sample",
                    inputs=[base+"_rot.stdhep"],
                    outputs=[base+"_sampled"])

job.components = [rot, sample]

job.run()
