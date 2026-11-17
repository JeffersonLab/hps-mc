"""!
@file wab_prep_and_slic_job.py

Add mother particle information to wab events and rotate them into beam coordinates before simulating detector response using slic.
"""
from hpsmc.generators import StdHepConverter
from hpsmc.tools import AddMother, BeamCoords, SLIC

job.description = 'Convert wab events to StdHep and simulate detector response'

if 'nevents' in job.params:
    nevents = job.params['nevents']
else:
    nevents = 10000

## Get job input file targets
inputs = list(job.input_files.values())

## Convert LHE output to stdhep
cnv = StdHepConverter(inputs=inputs, outputs=['wab.stdhep'])

## Add mother particle to tag trident particles
mom = AddMother(inputs=cnv.output_files(), outputs=['wab_mom.stdhep'])

## Rotate events into beam coords
rot = BeamCoords(inputs=['wab_mom.stdhep'])

## Simulate detector response
slic = SLIC(inputs=rot.output_files(), nevents=nevents + 1)

## Run the job
job.add([cnv, mom, rot, slic])
