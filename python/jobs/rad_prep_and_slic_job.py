"""!
@file rad_prep_and_slic_job.py

Add mother particle information to rad events and rotate them into beam coordinates before simulating detector response using slic.
"""
from hpsmc.generators import StdHepConverter
from hpsmc.tools import Unzip, AddMotherFullTruth, BeamCoords, SLIC

job.description = 'Convert rad events to StdHep and simulate detector response'

## Get job input file targets
inputs = list(job.input_files.values())

## Unzip the LHE events to a local file
unzip = Unzip(inputs=inputs, outputs=["rad.lhe"])

## Convert LHE output to stdhep
cnv = StdHepConverter(inputs=inputs, outputs=['rad.stdhep'])

## Add mother particle to tag trident particles
mom = AddMotherFullTruth(inputs=[cnv.output_files()[0], unzip.output_files()[0]], outputs=['rad_mom.stdhep'])

## Rotate events into beam coords
rot = BeamCoords(inputs=['rad_mom.stdhep'])

## Simulate detector response
slic = SLIC(inputs=rot.output_files())

## Run the job
job.add([unzip, cnv, mom, rot, slic])
