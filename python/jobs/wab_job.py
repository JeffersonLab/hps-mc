#!/usr/bin/env python

"""
Python script for generating WAB events in MG4.
"""

import sys, random

from hpsmc.job import Job
from hpsmc.run_params import RunParameters
from hpsmc.generators import MG4, StdHepConverter

job = Job(name="wab job")
job.initialize()

params = job.params

procname = "wab"

# generate tritrig in MG5
mg = MG4(description="Generate wab events using MG4",
         name=procname,
         run_card="run_card_"+params.run_params+".dat",
         param_card="MG_mini_WAB/AP_6W_XSec2_HallB/Cards/param_card.dat",
         outputs=[procname],
         nevents=params.nevents)

# convert LHE output to stdhep
stdhep_cnv = StdHepConverter(description="Convert LHE events to StdHep using EGS5",
                             run_params=RunParameters(key=params.run_params),
                             inputs=[procname+"_events.lhe.gz"],
                             outputs=[procname+".stdhep"])
 
# run the job
job.components=[mg, stdhep_cnv] 
job.run()
