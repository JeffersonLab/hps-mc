"""!
@file misalign_job.py

Intentionally misalign a detector by changing a set of millepede parameters.

"""
import os
from hpsmc.alignment import WriteMisalignedDet, ConstructDetector

job.description = 'generate a new, misaligned detector'

misalign_writer = WriteMisalignedDet()
construct_det = ConstructDetector()

job.add([misalign_writer, construct_det])
