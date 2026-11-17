"""!
@file simp_lhe_gts_job.py

SIMP gen_to_slic from the pre-generated unweighted LHE pool (not stdhep -
the stdhep pool was already beam-coords-rotated once, so re-rotating it
double-applies the beam spread transform).
"""
from hpsmc.tools import SLIC, BeamCoords, DisplaceUni

job.description = 'SIMP LHE to slic'

inputs = list(job.input_files.values())

## Convert LHE output to stdhep (no displacement - no ctau given)
cnv = DisplaceUni(inputs=inputs, outputs=["simp.stdhep"])

## Rotate into beam coords
rot = BeamCoords(inputs=cnv.output_files(), outputs=["simp_mom_rot.stdhep"])

## Run events in slic
slic = SLIC(inputs=rot.output_files())

job.add([cnv, rot, slic])
