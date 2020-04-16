"""
Dummy job script.
"""

import os

from hpsmc.job import Job
from hpsmc.component import DummyComponent

job = Job()
job.add(DummyComponent())  
job.run()
