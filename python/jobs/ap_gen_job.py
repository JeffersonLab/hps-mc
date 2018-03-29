#!/usr/bin/env python

"""
Job script to generate A-prime events, convert to StdHep and apply transformations.

Run with '--job-steps 1' to only generate the LHE output and not generate StdHep files
with transforms.
"""

import sys, os, argparse

from hpsmc.job import Job
from hpsmc.generators import MG4, StdHepConverter
from hpsmc.tools import Unzip, StdHepTool, MoveFiles

# define default job parameters
def_params = {
    "nevents": 10000,
    "run_params": "1pt05",
    "apmass": 40.0,
    "ctau": 1.0,
    "z": -5.0,
}

# setup job including default parameters
job = Job(name="AP event gen job")
job.set_default_params(def_params)
job.initialize()

# get job params
params = job.params

# base file name
filename = "aprime"

# generate A-prime events using Madgraph 4
ap = MG4(name="ap",
         run_card="run_card_"+params['run_params']+".dat",
         params={"APMASS": params['apmass']},
         outputs=[filename],
         nevents=params['nevents'])

# unzip the LHE events to local file
unzip = Unzip(inputs=[filename+"_events.lhe.gz"])

# displace the time of decay using ctau param
displ = StdHepTool(name="lhe_tridents_displacetime",
                   inputs=[filename+"_events.lhe"],
                   outputs=[filename+".stdhep"],
                   args=["-l", str(params['ctau'])])

# rotate events into beam coordinates and move vertex by 5 mm
rot = StdHepTool(name="beam_coords",
                 inputs=[filename+".stdhep"],
                 outputs=[filename+"_rot.stdhep"],
                 args=["-z", str(params['z'])])

# print rotated AP events
dump = StdHepTool(name="print_stdhep",
                  inputs=[filename+"_rot.stdhep"])

# define job components
job.components = [ap, unzip, displ, rot, dump]

# move final StdHep file to user-defined output file name if output_files is set in JSON params
if len(params.output_files):
    mv = MoveFiles(inputs=[filename+"_rot.stdhep"], outputs=[params.output_files.keys()[0]])
    job.components.append(mv)

# run the job
job.run()
