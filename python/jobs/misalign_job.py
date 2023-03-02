"""!
@file misalign_job.py

Intentionally misalign a detector by changing a set of millepede parameters.

Besides the main purpose of running the pede minimizer, we also do other helpful
tasks that are commonly done with alignment.
1. Apply the results from the pede minimizer to a new iteration of the detector
2. Construct the new iteration of the detector so that another round of tracking
    can be done
3. Merge the histogram files generated from the previous round of tracking for
    later analysis/drawing - this merging is only done if those histogram files
    exist and an output histogrma file is defined in the job JSON.
    - The histogram files are looked for by replacing the input _mille.bin suffix
      on the input files with _gblplots.bin which is the format of the outputs
      done by the tracking example.
"""
import os
from hpsmc.alignment import WriteMisalignedDet, ConstructDetector

job.description = 'generate a new, misaligned detector'

misalign_writer = WriteMisalignedDet()
construct_det = ConstructDetector()

job.add([ misalign_writer, construct_det ])
