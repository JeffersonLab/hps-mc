from hpsmc.generators import EGS5

job.description = 'EGS5 beam v3'

job.add(EGS5(name="beam_v3"))

## alternatively
# job.description = 'EGS5 beam v5'

# job.add(EGS5(name="beam_v5"))