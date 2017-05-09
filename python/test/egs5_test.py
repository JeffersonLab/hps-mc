from hpsmc import run_params
from run_params import RunParameters
from generators import EGS5
from hpsmc.base import Job

import sys

egs5 = EGS5(name="moller_v3",
    bunches=5000,
    run_params=RunParameters(key="1pt05"))

job = Job(name="EGS5 Test", components=[egs5])
job.setup()
job.run()
job.cleanup()
