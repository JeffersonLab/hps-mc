import os, sys

from hpsmc.base import Job
from hpsmc.run_params import RunParameters
from hpsmc.generators import EGS5
from hpsmc.tools import StdHepTool, SLIC, HPSJava

# generate stdhep file using EGS5
egs5 = EGS5(name="moller_v3", bunches=5000, run_params=RunParameters(key="1pt05"))

# print out stdhep data
stdhep_print = StdHepTool(name="print_stdhep", args=["10"], inputs=["moller.stdhep"])

# run stdhep file in slic
slic = SLIC(args=["-g", "/u/ey/jeremym/hps-dev/hps-java/hps-java/detector-data/detectors/HPS-EngRun2015-Nominal-v3/HPS-EngRun2015-Nominal-v3.lcdd",  "-i",  "./moller.stdhep",  "-o", "slic_events.slcio", "-r", "10"])

# run simple hps-java job
hps_java = HPSJava(args=["-i", "slic_events.slcio", "-r", "/org/hps/steering/EventMarker.lcsim"])

# create job using defined component
job = Job(name="EGS5 Test", components=[egs5, stdhep_print, slic, hps_java]) 

# setup, run and cleanup job
job.setup()
job.run()
job.cleanup()
