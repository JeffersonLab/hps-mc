from hpsmc import run_params
from run_params import RunParameters
from generators import EGS5

import sys

print "----- EGS5 test job -----"

rp = RunParameters(key="4pt4")

print
print "Running beam_v5 test"
print 
egs5 = EGS5("beam_v5", bunches=1000)
egs5.run_params = rp 
egs5.setup()
egs5.run()

print
print "Running moller_v3 test"
print
egs5 = EGS5("moller_v3")
egs5.run_params = rp 
egs5.setup()
egs5.run()
