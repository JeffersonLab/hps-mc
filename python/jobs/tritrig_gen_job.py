"""
Python script for generating 'tritrig' events in MG5. 
"""

from hpsmc.generators import MG5

# Generate tritrig in MG5
mg = MG5(name='tritrig')

# run the job
job.add([mg])
