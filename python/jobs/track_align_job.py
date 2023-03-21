"""!
@file java_job.py

Do a single hps-java run with a steering file.
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
    steering = job.params["steering"],
    outputs = list(job.output_files.keys())
    )

# update defs to include our track_align defaults for the SimpleGBLTrajAliDriver
traj_ali_driver_defaults = {
    'enableAlignmentCuts' : True,
    'doCOMAlignment' : True,
    'minMom' : 0.5,
    'maxMom' : 5.0,
    'nHitsCut' : 6,
    'debugAlignmentDs' : False,
    'correctTrack' : True,
    'includeNoHitScatters' : False,
    'gblRefitIterations' : 0,
    'storeTrackStates' : True,
    'compositeAlign' : True,
    'constrainedFit' : False,
    'momC' : 4.55,
    'constrainedBSFit' : False,
    'bsZ' : -7.7,
    'trackSide' : -1,
    #'writeMilleBinary' : True,
    #'milleBinaryFileName' : '${outputFile}.bin',
    'writeMilleChi2Cut' : 5,
    'enableStandardCuts' : False,
    'maxTrackChisq4hits' : 60.,
    'maxTrackChisq5hits' : 60.,
    'maxTrackChisq6hits' : 60.,
    'inputCollectionName' : 'KalmanFullTracks'
    }
if java_run.defs is None :
    java_run.defs = traj_ali_driver_defaults
else :
    for key, val in traj_ali_driver_defaults.items() :
        if key not in java_run.defs :
            java_run.defs[key] = val

job.add([java_run])
