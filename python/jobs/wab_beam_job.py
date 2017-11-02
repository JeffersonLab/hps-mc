#!/usr/bin/env python

"""
Python script for generating 'wab-beam' events.
"""
import sys, random

import hpsmc.func as func
from hpsmc.job import Job
from hpsmc.run_params import RunParameters
from hpsmc.generators import MG5, StdHepConverter
from hpsmc.tools import StdHepTool, SLIC

# job init
job = Job(name="tritrig job")
job.initialize()
params = job.params

# expected input files
wab_input = "wab.lhe.gz"
beam_input = "beam.stdhep"

# check for required inputs
if beam_input not in params.input_files:
    raise Exception("Missing '%s' input file in job params." % beam_input)
if wab_input not in params.input_files:
    raise Exception("Missing '%s' input file in job params." % wab_input)

# get run params
run_params = RunParameters(key=params.run_params)

# calculate mu for wab sampling
mu = func.mu(wab_input, run_params)

# convert wab events to stdhep
stdhep_cnv = StdHepConverter(run_params=run_params,
                             inputs=[wab_input],
                             outputs=["wab.stdhep"])

# rotate wab events into beam coords
rot_wab = StdHepTool(name="beam_coords",
                     inputs=["wab.stdhep"],
                     outputs=["wab_rot.stdhep"],
                     args=["-z", str(params.z)])

# sample wabs using poisson distribution
sample_wab = StdHepTool(name="merge_poisson",
                       inputs=["wab_rot.stdhep"],
                       outputs=["wab_sampled"],
                       args=["-m", str(mu), "-N", "1", "-n", "500000"])

# rotate beam background events into beam coordinates
rot_beam = StdHepTool(name="beam_coords",
                      inputs=["beam.stdhep"],
                      outputs=["beam_rot.stdhep"],
                      args=["-z", str(params.z)])
    
# sample beam background events 
sample_beam = StdHepTool(name="random_sample",
                         inputs=["beam_rot.stdhep"],
                         outputs=["beam_sampled"],
                         args=["-s", str(params.seed)])

# merge beam and wab events
merge = StdHepTool(name="merge_files",
                   inputs=["beam_sampled_1.stdhep", "wab_sampled_1.stdhep"],
                   outputs=["wab-beam.stdhep"])

# generate events in slic
slic = SLIC(description="Run detector simulation using SLIC",
            detector=params.detector,
            inputs=["wab-beam.stdhep"],
            outputs=["wab-beam.slcio"],
            nevents=params.nevents,
            ignore_returncode=True)

# run the job
job.components=[stdhep_cnv, rot_wab, sample_wab, rot_beam, sample_beam, merge, slic] 
job.run()
