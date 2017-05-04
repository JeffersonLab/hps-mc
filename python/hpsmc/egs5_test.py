from hpsmc import run_params
from run_params import RunParameters
from generators import EGS5
from hpsmc.base import Job

import sys

print "----- EGS5 test job -----"

rp = RunParameters(key="1pt05")

print
print "Running moller_v3 test"
print
egs5 = EGS5("moller_v3", bunches=50000)
egs5.run_params = rp 

job = Job(name="EGS5 Test", components=[egs5])
job.setup()
job.run()
job.cleanup()
