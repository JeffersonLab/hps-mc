"""!
@file rad_gen_job.py

Simulation of radiative events.
"""
from hpsmc.generators import MG5, StdHepConverter
from hpsmc.tools import Unzip, BeamCoords, AddMotherFullTruth

job.description = 'rad generation'

## Get job input file targets
inputs = list(job.input_files.values())

if 'nevents' in job.params:
    nevents = job.params['nevents']
else:
    nevents = 10000

## Generate rad in MG5
mg = MG5(name='RAD', event_types=['unweighted'])

## Convert LHE output to stdhep
cnv = StdHepConverter(name="lhe_rad", inputs=mg.output_files())

## Unzip the LHE events to a local file
unzip = Unzip(inputs=mg.output_files(), outputs=["rad.lhe"])

## Add mother particle to tag trident particles
mom = AddMotherFullTruth(inputs=[cnv.output_files()[0], unzip.output_files()[0]], outputs=["rad_mom.stdhep"])

## Rotate events into beam coords
rot = BeamCoords(inputs=mom.output_files(), outputs=["rad_rot.stdhep"])

## run the job
job.add([mg, cnv, unzip, mom, rot])
