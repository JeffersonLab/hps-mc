"""!
@file tritrig_gen_to_recon_job.py

Simulate tritrig events, add mother particle information and rotate events into beam coordinates, then
run through SLIC, space events, do readout and recon and finally make a minidst root file.
"""
from hpsmc.generators import MG5, StdHepConverter
from hpsmc.tools import Unzip, AddMotherFullTruth, BeamCoords, SLIC, FilterBunches, JobManager, ProcessMiniDst

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

## insert empty bunches expected by pile-up simulation
filter_bunches = FilterBunches(inputs=['tritrig_mom_rot.slcio'],
                               outputs=['tritrig_mom_rot_spaced.slcio'])

## Run simulated events in readout to generate triggers
readout = JobManager(steering='readout',
                     inputs=['tritrig_mom_rot_spaced.slcio'],
                     outputs=['tritrig_mom_readout.slcio'])

## Run physics reconstruction
recon = JobManager(steering='recon',
                   inputs=['tritrig_mom_readout.slcio'],
                   outputs=['tritrig_mom_recon.slcio'])

minidst = ProcessMiniDst(inputs=['tritrig_mom_recon.slcio'],
                         outputs=['tritrig_mom_minidst.root'])
##


## Run the job
job.add([mg, unzip, cnv, mom, rot, slic, filter_bunches, readout, recon, minidst])
