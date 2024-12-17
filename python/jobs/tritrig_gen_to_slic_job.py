"""!
@file tritrig_gen_to_beam_coords_job.py

Simulate tritrig events, add mother particle information and rotate events into beam coordinates.
"""
from hpsmc.generators import MG5, StdHepConverter
from hpsmc.tools import Unzip, AddMotherFullTruth, BeamCoords, SLIC

job.description = 'Generate tritrig events and simulate passage through detector'

if 'nevents' in job.params:
    nevents = job.params['nevents']
else:
    nevents = 10000

## Generate tritrig in MG5
mg = MG5(name='tritrig')

## Unzip the LHE events to a local file
unzip = Unzip(inputs=mg.output_files())

## Convert LHE output to stdhep
cnv = StdHepConverter(inputs=mg.output_files(),
                      outputs=['tritrig.stdhep'])

## Add mother particle to tag trident particles
mom = AddMotherFullTruth(inputs=[cnv.output_files()[0], unzip.output_files()[0]],
                         outputs=['tritrig_mom.stdhep'])

## Rotate events into beam coords
rot = BeamCoords(inputs=['tritrig_mom.stdhep'],
                 outputs=['tritrig_mom_rot.stdhep'])

## Add ptag for gen file
job.ptag('gen', 'tritrig_mom_rot.stdhep')

## SLIC
slic = SLIC(inputs=['tritrig_mom_rot.stdhep'],
            outputs=['tritrig_mom_rot.slcio'],
            nevents=nevents + 1)

## Run the job
job.add([mg, unzip, cnv, mom, rot, slic])
