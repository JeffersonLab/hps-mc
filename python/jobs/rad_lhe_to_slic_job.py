"""!
@file rad_lhe_to_slic_job.py

Radiative (rad) events from a pre-generated LHE file through SLIC.

"""
from hpsmc.generators import StdHepConverter
from hpsmc.tools import Unzip, AddMotherFullTruth, BeamCoords, SLIC

job.description = 'rad from LHE to slic'

## Pre-generated LHE mapped in by the job JSON
inputs = list(job.input_files.values())

nevents = job.params.get('nevents', 10000)

## EGS5 LHE->StdHep converter
converter = job.params.get('lhe_converter', 'lhe_rad')

## Unzip the LHE events to a local file
unzip = Unzip(inputs=inputs, outputs=['rad.lhe'])

## Convert LHE output to stdhep, propagating particles through the target
cnv = StdHepConverter(name=converter, inputs=inputs)

## Add mother particles to tag trident particles
mom = AddMotherFullTruth(inputs=[cnv.output_files()[0], unzip.output_files()[0]],
                         outputs=['rad_mom.stdhep'])

## Rotate events into beam coords
rot = BeamCoords(inputs=mom.output_files(), outputs=['rad_mom_rot.stdhep'])

## SLIC
slic = SLIC(inputs=rot.output_files(), outputs=['rad_mom_rot.slcio'],
            nevents=nevents + 1)

## Run the job
job.add([unzip, cnv, mom, rot, slic])
