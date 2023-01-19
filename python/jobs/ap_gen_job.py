"""!
@file ap_gen_job.py

Job script to generate A-prime events.
"""

from hpsmc.generators import MG4

job.description = 'ap generation'

if 'nevents' in job.params:
    nevents = job.params['nevents']
else:
    nevents = 10000

# Generate rad in MG4
mg = MG4(name='ap', event_types=['unweighted'])

# run the job
job.add([mg])
