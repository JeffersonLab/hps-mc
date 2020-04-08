"""
Python script for running readout 'simp' events through reconstruction.
"""

import sys, random

from hpsmc.job import Job
from hpsmc.tools import SLIC, JobManager, FilterBunches

job = Job(name="simp job")
job.initialize()

params = job.params
input_files = params.input_files

simp_files = []
for input_file in input_files:
    simp_files.append(input_file)

procname = "simp"
# run physics reconstruction
recon = JobManager(steering_resource=params.recon_steering,
                   java_args=["-DdisableSvtAlignmentConstants"],
                   defs={"detector": params.detector, "run": params.run},
                   #inputs=[procname+"_readout.slcio"],
                   inputs=simp_files,
                   outputs=[procname+"_recon"])
 
# run the job
job.components=[recon] 
job.run()
