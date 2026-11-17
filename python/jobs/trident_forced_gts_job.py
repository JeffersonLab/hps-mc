"""!
@file trident_forced_gts_job.py

Pure-GPS-gun trident generation: fires a GPS beam-electron (see
trident_forced_l1upstream.mac) and forces it to undergo a one-step trident
interaction, e- + N -> e- + e+ + e-, in a Layer 1 sensor via the SLIC
ForcedTridentL1Upstream plugin, carrying a per-event MC weight equal to the
real local interaction probability for that slab.

No stdhep conversion/rotation step exists here (unlike the wab_forced* jobs)
-- the gun itself is the generator. The base angular/energy distribution is
defined in trident_forced_l1upstream.mac; this script only writes a small
override macro (loaded second, so its commands win) that sets the GPS gun's
starting z-position from the job's target_z parameter, so that value can be
set per-species/per-job via the JSON config instead of being hardcoded in
the macro.

Required job parameter: target_z (mm, GPS gun starting z-position).
"""
import os
from hpsmc.tools import SLIC

job.description = 'Generate forced-trident events directly via a GPS beam-electron gun in SLIC'

if 'nevents' in job.params:
    nevents = job.params['nevents']
else:
    nevents = 10000

target_z = job.params['target_z']

## Base macro defines the GPS gun (energy, beam spot, angular distribution)
## and loads the ForcedTridentL1Upstream plugin; validated separately (see
## macros/trident_forced_l1upstream_gps_test.mac smoke test).
base_macro = '/sdf/group/hps/users/mgraham/MCProduction/slic-hps-fork/macros/trident_forced_l1upstream.mac'

## Write a small override macro that sets the GPS gun's z-position from this
## job's target_z parameter. SLIC runs macros in the order given on the
## command line, so loading this one after base_macro lets its
## /gps/pos/centre line win over the (otherwise unused) default in
## base_macro.
## NOTE: this job script is exec'd (via _load_script()) before the job
## manager chdir's into job.rundir (via _setup()), so bare relative paths
## resolve against the wrong directory -- use job.rundir explicitly instead
## of os.getcwd()/os.path.abspath.
override_macro_path = os.path.join(job.rundir, 'trident_forced_target_z.mac')
with open(override_macro_path, 'w') as f:
    f.write('/gps/pos/centre 0 0 %s mm\n' % str(target_z))

## Simulate detector response, forcing the trident. No inputs exist for this
## pure-GPS-gun job -- SLIC.cmd_args() falls back to '-i <input>' only when
## macros is empty, which is never the case here, but a placeholder is still
## required to satisfy SLIC's "no inputs given" guard.
## NOTE: 'macros' must be set as an attribute, not a constructor kwarg --
## Component.__init__ silently discards unrecognized kwargs, and SLIC.__init__
## unconditionally sets self.macros = [] right before calling it.
slic = SLIC(inputs=['placeholder.stdhep'], outputs=['trident_forced.slcio'], nevents=nevents)
slic.macros = [base_macro, override_macro_path]

## Run the job
job.add([slic])
