#!/usr/bin/env python

from hpsmc.job import Job
from hpsmc.generators import EGS5
from hpsmc.run_params import RunParameters

# default params with 1.05 GeV beam params and 500 bunches of electrons
default_params = {
    "run_params": "1pt05",
    "bunches": 500
}

job = Job(name="EGS5 beam_v5 job")
job.set_default_params(default_params)
job.initialize()

params = job.params

# generates a file called 'beam.stdhep'
egs5 = EGS5(name="beam_v5",
    bunches=params['bunches'],
    run_params=RunParameters(key=params['run_params']),
    outputs=["beam.stdhep"])

job.components = [egs5]

job.run()
