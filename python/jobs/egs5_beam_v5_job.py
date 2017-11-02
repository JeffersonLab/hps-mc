#!/usr/bin/env python

from hpsmc.job import Job
from hpsmc.generators import EGS5
from hpsmc.run_params import RunParameters

job = Job(name="EGS5 beam_v5 job")
job.initialize()

params = job.params

egs5 = EGS5(name="beam_v5",
    bunches=params.bunches,
    run_params=RunParameters(key=params.run_params),
    outputs=["beam.stdhep"])

job.components = [egs5]

job.run()
