"""!
wab from generation to slic
"""
from hpsmc.generators import MG5, StdHepConverter
from hpsmc.tools import BeamCoords, AddMother, SLIC

job.description = 'wab from generation to slic'

## Get job input file targets
inputs = list(job.input_files.values())

if 'nevents' in job.params:
    nevents = job.params['nevents']
else:
    nevents = 10000

## Generate wab in MG5
mg = MG5(name='WAB', event_types=['unweighted'])

## Convert LHE output to stdhep
cnv = StdHepConverter()

## Add mother particle to tag trident particles
mom = AddMother()

## Rotate events into beam coords
rot = BeamCoords()

## Simulate signal events
slic = SLIC(nevents=nevents + 1)

## run the job
job.add([mg, cnv, mom, rot, slic])
