"""!
@file gen-simp-3body.py

Generation of SIMP events with a 3-body decay of the dark vector meson.
"""
from hpsmc.generators import MG5

job.description = 'SIMP 3-body decay generation'

## Generate tritrig in MG5
mg = MG5(name='simp-3body',
         run_card='run_card.dat',
         param_card='param_card.dat',
         event_types=['unweighted'])

## Run the job
job.add([mg])
