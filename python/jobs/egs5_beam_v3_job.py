#!/usr/bin/env python

from hpsmc.job import Job
from hpsmc.generators import EGS5
from hpsmc.run_params import RunParameters

job = Job()
job.add(EGS5(name="beam_v3"))
job.run()
