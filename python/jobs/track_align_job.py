"""!
@file java_job.py

Do a single hps-java run with a steering file.
"""

import os
import logging
from hpsmc.tools import JobManager

## Initialize logger with default level
logger = logging.getLogger('hpsmc.job')

job.description = 'single hps-java run with input steering file'

## Assign ptags for output
input_files = list(job.input_files.values())
if len(input_files) > 1:
    raise Exception('This script accepts only one input file.')

java_run = JobManager(
    steering = job.params["steering"],
    outputs = list(job.output_files.keys())
    )

job.add([java_run])
