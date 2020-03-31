"""
Python script for generating slic events.
"""

import os

from hpsmc.job import Job
from hpsmc.tools import SLIC

job = Job(name="slic_job")
job.initialize()
job.add(SLIC())           
job.run()
