from hpsmc.generators import MG5, StdHepConverter

job.description = 'Generate tritrig events using MadGraph5 and convert to StdHep using EGS5'

# Generate tritrig in MG5
mg = MG5(name='tritrig')

# Convert LHE output to stdhep
cnv = StdHepConverter()

# run the job
job.add([mg, cnv])
