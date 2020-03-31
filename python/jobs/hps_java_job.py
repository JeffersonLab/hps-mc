"""
Python script for running hps-java.
"""

import sys, random, os

from hpsmc.job import Job
from hpsmc.tools import JobManager

job = Job(name="hps_java_job")
job.add(JobManager(description="Run hps-java with a steering file"))
job.run()
