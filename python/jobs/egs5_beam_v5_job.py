#!/usr/bin/env python

from hpsmc.job import Job
from hpsmc.generators import EGS5
from hpsmc.run_params import RunParameters

"""
default_params = {
    "run_params": "1pt05",
    "bunches": 500
}
"""

job = Job()
job.add(EGS5(name="beam_v5"))
job.run()
