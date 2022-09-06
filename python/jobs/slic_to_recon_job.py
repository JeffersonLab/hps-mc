"""!
@file slic_to_recon_job.py

Simulation of signals in detector (using SLIC) and readout.
The simulation is followed by reconstruction of the events.
"""
import os
from hpsmc.tools import SLIC, JobManager, ExtractEventsWithHitAtHodoEcal

## Get job input file targets
inputs = list(job.input_files.values())

job.description = 'slic to recon'

# event_int needs to be set to 250 for beam files; should be = 1 for signal files
if 'event_interval' in job.params:
    event_int = job.params['event_interval']
else:
    event_int = 1

if 'nevents' in job.params:
    nevents = job.params['nevents']
else:
    nevents = 10000

if 'base_name' in job.params:
    base_name = job.params['base_name']
else:
    base_name = ''

## Input beam events (StdHep format)
slic_file_names = []
for i in range(len(inputs)):
    filename, file_extension = os.path.splitext(inputs[i])
    slic_file = filename + '.slcio'
    slic_file_names.append(slic_file)

## Simulate beam events
slic_comps = [] 
for i in range(len(inputs)):
    slic_comps.append(SLIC(inputs=[inputs[i]],
                      outputs=[slic_file_names[i]],
                      nevents=nevents*event_int,
                      ignore_job_params=['nevents'])
                     )

## concatenate beam events before merging
cat_out_name = base_name + '_slic_cat.slcio'
slic_cat = ExtractEventsWithHitAtHodoEcal(inputs=slic_file_names,
                                          outputs=[cat_out_name],
                                          event_interval=0, num_hodo_hits=0)

## Run simulated events in readout to generate triggers
readout_out_name = base_name + '_readout.slcio'
readout = JobManager(steering='readout',
                     inputs=slic_cat.output_files(),
                     outputs=[readout_out_name])

## Run physics reconstruction
recon_out_name = base_name + '_recon.slcio'
recon = JobManager(steering='recon',
                   inputs=readout.output_files(),
                   outputs=[recon_out_name])
 
## Add the components
comps = slic_comps
comps.extend([slic_cat, readout, recon])
job.add(comps)
