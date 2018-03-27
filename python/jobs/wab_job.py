#!/usr/bin/env python

"""
Python script for generating WAB events in MG4.
"""

from hpsmc.job import Job
from hpsmc.run_params import RunParameters
from hpsmc.generators import MG4, StdHepConverter
from hpsmc.tools import LHECount

# default parameters
def_params = {
    "nevents": 10000,
    "run_params": "1pt05",
    "min_event_frac": 0.8
}

# setup job with defaults
job = Job(name="wab job")
job.set_default_params(def_params)
job.initialize()

# get job params
params = job.params

# base name for MG process and file names
procname = "wab"

# generate tritrig in MG5
mg = MG4(description="Generate wab events using MG4",
         name=procname,
         run_card="run_card_"+params['run_params']+".dat",
         param_card="MG_mini_WAB/AP_6W_XSec2_HallB/Cards/param_card.dat",
         outputs=[procname],
         nevents=params['nevents'])

# check that at least 80% of the requested events were generated or fail the job
check = LHECount(minevents=params['nevents'] * params['min_event_frac'], inputs=[procname + "_unweighted_events.lhe.gz"])

# run the job
job.components=[mg, check]
job.run()
