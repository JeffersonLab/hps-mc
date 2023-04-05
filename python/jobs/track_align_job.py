"""!
@file track_align_job.py

This job is focused on running the hps-java JobManager once. The user
can decide which steering file to use; however, the user must know that
this job provides many command-line definitions to be substituted into
the SimpleGblAliDriver config parameters. Look at the steering files
in the examples/alignment/tracking directory as an example.
"""

import os
import logging
from hpsmc.tools import JobManager

## Initialize logger with default level
logger = logging.getLogger('hpsmc.job')

job.description = 'single hps-java run with input steering file'

## Assign ptags for output
input_files = list(job.input_files.values())
if len(input_files) > 1:
    raise Exception('This script accepts only one input file.')

java_run = JobManager(
    steering=job.params["steering"],
    outputs=list(job.output_files.keys())
    )

# update defs to include our track_align defaults for the SimpleGBLTrajAliDriver
traj_ali_driver_defaults = {
    'enableAlignmentCuts': True,
    'doCOMAlignment': True,
    'minMom': 0.5,
    'maxMom': 5.0,
    'nHitsCut': 6,
    'debugAlignmentDs': False,
    'correctTrack': True,
    'includeNoHitScatters': False,
    'gblRefitIterations': 0,
    'storeTrackStates': True,
    'compositeAlign': True,
    'constrainedFit': False,
    'momC': 4.55,
    'constrainedBSFit': False,
    'bsZ': -7.7,
    'trackSide': -1,
    #'writeMilleBinary' : True,
    #'milleBinaryFileName' : '${outputFile}.bin',
    'writeMilleChisqCut': 5,
    'enableStandardCuts': False,
    'maxTrackChisqFourHits': 60.,
    'maxTrackChisqFiveHits': 60.,
    'maxTrackChisqSixHits': 60.,
    'inputCollectionName': 'KalmanFullTracks'
    }
if 'defs' not in job.params:
    # no user-defined defs
    java_run.defs = traj_ali_driver_defaults
else:
    # some user defined defs, make sure others get their defaults
    java_run.defs = job.params['defs']
    for key, val in traj_ali_driver_defaults.items():
        if key not in java_run.defs:
            java_run.defs[key] = val

job.add([java_run])
