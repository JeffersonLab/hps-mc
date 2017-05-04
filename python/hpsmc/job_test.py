import os, sys

from hpsmc.base import Job
from hpsmc.run_params import RunParameters
from hpsmc.generators import EGS5
from hpsmc.tools import StdHepTool
from hpsmc.tools import SLIC

egs5 = EGS5(name="moller_v3", bunches=5000, run_params=RunParameters(key="1pt05"))
stdhep_print = StdHepTool(name="print_stdhep", args=["10"], inputs=["moller.stdhep"])
slic = SLIC(args=["-g", "/u/ey/jeremym/hps-dev/hps-java/hps-java/detector-data/detectors/HPS-EngRun2015-Nominal-v3/HPS-EngRun2015-Nominal-v3.lcdd",  "-i",  "./moller.stdhep",  "-o", "slic_events.slcio", "-r", "10"])

job = Job(name="EGS5 Test", components=[egs5, stdhep_print, slic]) 

job.setup()
job.run()
job.cleanup()
