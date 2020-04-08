from hpsmc.job import Job
from hpsmc.tools import HPSTR 

job = Job()
job.add(HPSTR())
job.run()
