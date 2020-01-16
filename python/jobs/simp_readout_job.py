#!/usr/bin/env python

"""
Python script for running generating 'simp' events through PU simulation, readout.
"""

import sys, random

from hpsmc.job import Job
from hpsmc.tools import SLIC, JobManager, FilterMCBunches

job = Job(name="simp job")
job.initialize()

params = job.params
input_files = params.input_files

simp_files = []
for input_file in input_files:
    simp_files.append(input_file)

procname = "simp"

# insert empty bunches expected by pile-up simulation
filter_bunches = FilterMCBunches(java_args=["-DdisableSvtAlignmentConstants"],
                                 inputs=simp_files,
                                 outputs=[procname+"_filt.slcio"],
                                 ecal_hit_ecut=0.05,
                                 enable_ecal_energy_filter=True,
                                 nevents=2000000,
                                 event_interval=250)

# run simulated events in readout to generate triggers
readout = JobManager(steering_resource=params.readout_steering,
                     java_args=["-DdisableSvtAlignmentConstants"],
                     run=params.run,
                     detector=params.detector,
                     inputs=[procname+"_filt.slcio"],
                     outputs=[procname+"_readout"])

# run the job
job.components=[filter_bunches, readout]
job.run()
