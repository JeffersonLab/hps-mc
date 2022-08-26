"""!
Job script to generate displaced A-prime events, convert to StdHep, apply transformations,
and resulting simulate signal events using SLIC.
"""

from hpsmc.generators import MG4, StdHepConverter
from hpsmc.tools import DisplaceTime, Unzip, BeamCoords, AddMotherFullTruth, SLIC

job.description = 'ap-displaced from generation to slic'

## Get job input file targets
inputs = list(job.input_files.values())

if 'nevents' in job.params:
    nevents = job.params['nevents']
else:
    nevents = 10000

## Generate rad in MG4
mg = MG4(name='ap', event_types=['unweighted'])

## Convert LHE output to stdhep
cnv = StdHepConverter(name="lhe_uniform", inputs=mg.output_files())
## for prompt signal, change the above to
# cnv = StdHepConverter(name="lhe_prompt", inputs=mg.output_files())
## alternatively, on can displace the time of decay using the ctau param
# cnv = DisplaceTime(inputs=mg.output_files())

## Unzip the LHE events to a local file
unzip = Unzip(inputs=mg.output_files(), outputs=["ap.lhe"])

## Add mother particle to tag trident particles
mom = AddMotherFullTruth(inputs=[cnv.output_files()[0], unzip.output_files()[0]], outputs=["ap_mom.stdhep"])

## Rotate events into beam coords
rot = BeamCoords(inputs=mom.output_files(), outputs=["ap_rot.stdhep"])

## Simulate signal events
slic = SLIC(nevents=nevents+1, inputs=rot.output_files(), outputs=["ap.slcio"])

## run the job
job.add([mg, cnv, unzip, mom, rot, slic])
