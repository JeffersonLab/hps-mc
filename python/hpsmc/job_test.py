import os, sys

from hpsmc.run_params import RunParameters
from hpsmc.generators import EGS5
from hpsmc.base import Job
from hpsmc.tools import StdHepTool

rp = RunParameters(key="1pt05")

egs5 = EGS5(name="moller_v3", bunches=5000)
egs5.run_params = rp 

stdhep_print = StdHepTool(name="print_stdhep", args=["10"], inputs=["moller.stdhep"])

job = Job(name="EGS5 Test", components=[egs5, stdhep_print]) 

job.setup()
job.run()
job.cleanup()
