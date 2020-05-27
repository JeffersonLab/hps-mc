"""
Python script for generating WAB events in MG4.
"""

from hpsmc.generators import MG4
from hpsmc.tools import LHECount

# generate tritrig in MG5
mg = MG4(name="wab")

# check that at least 80% of the requested events were generated or fail the job
#check = LHECount(minevents=params.nevents*0.8, inputs=["wab_unweighted_events.lhe.gz"])

# run the job
#job.components=[mg, check]
job.add([mg])
