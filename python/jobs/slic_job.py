"""!
@file slic_job.py

Simulation of signals in detector using SLIC.
"""
import os
from hpsmc.tools import SLIC

job.description = 'detector sim via slic'

## Get job input file targets
inputs = list(job.input_files.values())

## get file names
output_names = []
for i in range(len(inputs)):
    filename, file_extension = os.path.splitext(inputs[i])
    outname = filename + '.slcio'
    output_names.append(outname)

if 'nevents' in job.params:
    nevents = job.params['nevents']
else:
    nevents = 250000

## Simulate events
slic_outs = []
for i in range(len(inputs)):
    slic_outs.append(SLIC(inputs=[inputs[i]], outputs=[output_names[i]], nevents=nevents+1))

## Run the job
job.add(slic_outs)
