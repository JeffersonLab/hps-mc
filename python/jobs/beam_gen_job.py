"""!
@file beam_gen_job.py

Generate beam events.
"""
from hpsmc.generators import EGS5

job.description = 'beam generation'

if 'nevents' in job.params:
    nevents = job.params['nevents']
else:
    nevents = 250000

# Generate beam
egs5 = EGS5(name="beam_v7_%s" % job.params['run_params'])

# Run the job
job.add([egs5])
