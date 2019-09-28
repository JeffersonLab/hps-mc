#!/usr/bin/env python

from hpsmc.tools import JavaTest
from hpsmc.job import Job

job = Job(name="Java test")
job.initialize()
job.components=[JavaTest(ignore_returncode=True)]
job.run()
