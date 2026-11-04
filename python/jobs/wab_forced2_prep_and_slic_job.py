"""!
@file wab_forced2_prep_and_slic_job.py

Same as wab_forced1_prep_and_slic_job.py, but loads the SLIC
ForcedPairConversionL1Downstream plugin so the radiated photon in every
event is forced to pair-convert in the downstream (second, shielded) L1
sensor rather than the upstream one, carrying a per-event MC weight equal
to the unforced conversion probability.

Requires a SLIC build with the custom ForcedPairConversion plugins (not
included in the shared/standard SLIC install) -- set [SLIC]:slic_dir in an
extra_config .hpsmc override to point at such a build.
"""
import os
from hpsmc.generators import StdHepConverter
from hpsmc.tools import AddMother, BeamCoords, SLIC

job.description = 'Convert wab events to StdHep, force pair conversion in downstream L1 sensor, and simulate detector response'

if 'nevents' in job.params:
    nevents = job.params['nevents']
else:
    nevents = 10000

## Get job input file targets
inputs = list(job.input_files.values())

## Convert LHE output to stdhep
cnv = StdHepConverter(inputs=inputs, outputs=['wab.stdhep'])

## Add mother particle to tag trident particles
mom = AddMother(inputs=cnv.output_files(), outputs=['wab_mom.stdhep'])

## Rotate events into beam coords
rot = BeamCoords(inputs=['wab_mom.stdhep'])

## Write a macro that loads the ForcedPairConversionL1Downstream plugin and
## points the generator at the rotated stdhep file. SLIC always processes
## -m before -r (see CommandLineProcessor::processOptions), so this is safe
## to combine with the normal -g/-o/-d/-r flags built by SLIC.cmd_args().
## NOTE: this job script is exec'd (via _load_script()) before the job
## manager chdir's into job.rundir (via _setup()), so bare relative paths
## resolve against the wrong directory -- use job.rundir explicitly instead
## of os.getcwd()/os.path.abspath.
macro_path = os.path.join(job.rundir, 'forced_pair_conversion.mac')
with open(macro_path, 'w') as f:
    f.write('/generator/filename %s\n' % os.path.join(job.rundir, rot.output_files()[0]))
    f.write('/slic/plugins/load ForcedPairConversionL1Downstream\n')

## Simulate detector response, forcing pair conversion.
## NOTE: 'macros' must be set as an attribute, not a constructor kwarg --
## Component.__init__ silently discards unrecognized kwargs, and SLIC.__init__
## unconditionally sets self.macros = [] right before calling it.
slic = SLIC(nevents=nevents + 1)
slic.macros = [macro_path]

## Run the job
job.add([cnv, mom, rot, slic])
