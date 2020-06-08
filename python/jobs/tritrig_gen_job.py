from hpsmc.generators import MG5

job.description = 'tritrig generation using MadGraph5'

# Generate tritrig in MG5
mg = MG5(name='tritrig')

# run the job
job.add([mg])
