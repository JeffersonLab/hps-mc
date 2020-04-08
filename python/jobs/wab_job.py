"""
Python script for generating WAB events in MG4.
"""

from hpsmc.job import Job
from hpsmc.run_params import RunParameters
from hpsmc.generators import MG4, StdHepConverter
from hpsmc.tools import LHECount

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

# check that at least 80% of the requested events were generated or fail the job
check = LHECount(minevents=params.nevents*0.8, inputs=["wab_unweighted_events.lhe.gz"])

# run the job
job.components=[mg, check]
job.run()
