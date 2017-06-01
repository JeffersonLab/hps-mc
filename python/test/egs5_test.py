from hpsmc.job import Job
from hpsmc.generators import EGS5
from hpsmc.run_params import RunParameters

egs5 = EGS5(name="moller_v3",
    bunches=5000,
    run_params=RunParameters(key="1pt05"),
    outputs=["events.stdhep"])

job = Job(name="EGS5 Test", components=[egs5])
job.params = {}

job.run()
