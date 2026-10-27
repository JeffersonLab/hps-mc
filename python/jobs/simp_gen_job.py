"""!
@file simp_job.py

Simulation of SIMPs, detector signals, and readout, followed by reconstruction.
"""
from hpsmc.generators import MG5
from hpsmc.tools import SLIC, AddMotherFullTruth, JobManager, FilterBunches, BeamCoords, Unzip, DisplaceUni

job.description = 'SIMP generation to recon'

## Generate tritrig in MG5
mg = MG5(name='simp',
         run_card='run_card.dat',
         param_card='param_card.dat',
         event_types=['unweighted'])

## Unzip LHE file
unzip = Unzip(inputs=['simp_unweighted_events.lhe.gz'], outputs=['simp.lhe'])

## Convert LHE output to stdhep (no displacement here because no ctau given)
cnv = DisplaceUni(inputs=['simp.lhe'], outputs=['simp.stdhep'])

mom = AddMotherFullTruth(inputs=["simp.stdhep", unzip.output_files()[0]], outputs=["simp_mom.stdhep"])

## Rotate into beam coords
rot = BeamCoords()

## Run events in slic
slic = SLIC()

## Run the job
job.add([mg, unzip, cnv, mom, rot, slic])
