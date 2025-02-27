"""!
@file ap_slic_job.py

Job script to convert A-prime events to StdHep, apply transformations,
and resulting simulate signal events using SLIC.
"""

import logging
from hpsmc.generators import StdHepConverter
from hpsmc.tools import DisplaceUni, Unzip, BeamCoords, AddMotherFullTruth, SLIC

## Initialize logger with default level
logger = logging.getLogger('hpsmc.job')

job.description = 'ap to slic'

## Get job input file targets
inputs = list(job.input_files.values())

if 'nevents' in job.params:
    nevents = job.params['nevents']
else:
    nevents = 10000

if 'ap_decay_dist' in job.params:
    ap_decay_dist = job.params['ap_decay_dist']
else:
    ap_decay_dist = "lhe_uniform"

## Unzip the LHE events to a local file
unzip = Unzip(inputs=["ap_unweighted_events.lhe.gz"], outputs=["ap_unweighted_events.lhe"])

if ap_decay_dist == "lhe_uniform":
    ## Convert LHE output to stdhep for uniform signal
    cnv = StdHepConverter(name="lhe_uniform", inputs=["ap_unweighted_events.lhe"])
    job.add([cnv])
elif ap_decay_dist == "lhe_prompt":
    ## Convert LHE output to stdhep for prompt signal
    cnv = StdHepConverter(name="lhe_prompt", inputs=["ap_unweighted_events.lhe"])
    job.add([cnv])
elif ap_decay_dist == "displace_time":
    if 'ctau' in job.params:
        ## Displace the time of decay using the ctau param
        cnv = DisplaceUni(inputs=["ap_unweighted_events.lhe"])
        job.add([cnv])
    else:
        logger.error("Missing parameter ctau")
else:
    logger.error("Invalid ap decay distribution: ap_decay_dist = %s" % ap_decay_dist)

## Add mother particle to tag trident particles
mom = AddMotherFullTruth(inputs=["ap_displace.stdhep", unzip.output_files()[0]], outputs=["ap_mom.stdhep"])

## Rotate events into beam coords
rot = BeamCoords(inputs=mom.output_files(), outputs=["ap_rot.stdhep"])

## Simulate signal events
slic = SLIC(nevents=nevents + 1, inputs=rot.output_files(), outputs=["ap.slcio"])

## run the job
job.add([unzip, mom, rot, slic])
