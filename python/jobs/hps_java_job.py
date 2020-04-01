"""
Python script for running hps-java.
"""

import sys, random, os

from hpsmc.job import Job
from hpsmc.tools import JobManager

job = Job()
job.add(JobManager())
job.run()
