"""
Python script for generating slic events.
"""

import os

from hpsmc.job import Job
from hpsmc.tools import SLIC

job = Job()
job.add(SLIC())           
job.run()
