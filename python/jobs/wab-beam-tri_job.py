"""
Python script for generating 'wab-beam-tri' events.
"""

import sys, random

import hpsmc.func as func
from hpsmc.job import Job
from hpsmc.run_params import RunParameters
from hpsmc.generators import MG5, StdHepConverter
from hpsmc.tools import StdHepTool, SLIC, JobManager

# job init
job = Job(name="tritrig job")
job.initialize()
params = job.params

# expected input files
tri_input = "tri.lhe.gz"
wab_input = "wab.lhe.gz"
beam_input = "beam.stdhep"

# check for required inputs
if beam_input not in params.input_files:
    raise Exception("Missing '%s' input file." % beam_input)
if wab_input not in params.input_files:
    raise Exception("Missing '%s' input file." % wab_input)
if tri_input not in params.input_files:
    raise Exception("Missing '%s' input file." % tri_input)

# get run params
run_params = RunParameters(key=params.run_params)

# calculate mu for tri sampling
mu = func.mu(run_params, tri_input)

# convert tri events to stdhep
stdhep_tri = StdHepConverter(run_params=run_params,
                             inputs=[tri_input],
                             outputs=["tri.stdhep"])

# rotate tri events into beam coords
rot_tri = StdHepTool(name="beam_coords",
                     inputs=["tri.stdhep"],
                     outputs=["tri_rot.stdhep"],
                     args=["-z", str(params.z)])

# sample tris using poisson distribution
sample_tri = StdHepTool(name="merge_poisson",
                       inputs=["tri_rot.stdhep"],
                       outputs=["tri_sampled"],
                       args=["-m", str(mu), "-N", "1", "-n", "500000"])

# calculate mu for wab sampling
mu = func.mu(run_params, wab_input)

# convert wab events to stdhep
stdhep_wab = StdHepConverter(run_params=run_params,
                             inputs=[wab_input],
                             outputs=["wab.stdhep"])

# rotate wab events into beam coords
rot_wab = StdHepTool(name="beam_coords",
                     inputs=["wab.stdhep"],
                     outputs=["wab_rot.stdhep"],
                     args=["-z", str(params.z)])

# sample wabs using poisson distribution
sample_wab = StdHepTool(name="merge_poisson",
                       inputs=["wab_rot.stdhep"],
                       outputs=["wab_sampled"],
                       args=["-m", str(mu), "-N", "1", "-n", "500000"])

# rotate beam background events into beam coordinates
rot_beam = StdHepTool(name="beam_coords",
                      inputs=["beam.stdhep"],
                      outputs=["beam_rot.stdhep"],
                      args=["-z", str(params.z)])
    
# sample beam backgroun events 
sample_beam = StdHepTool(name="random_sample",
                         inputs=["beam_rot.stdhep"],
                         outputs=["beam_sampled"],
                         args=["-N", "1"])

# merge beam, wab and tri events
merge = StdHepTool(name="merge_files",
                   inputs=["beam_sampled_1.stdhep", "wab_sampled_1.stdhep", "tri_sampled_1.stdhep"],
                   outputs=["wab-beam-tri.stdhep"])

# generate events in slic
slic = SLIC(description="Run detector simulation using SLIC",
            detector=params.detector,
            inputs=["wab-beam-tri.stdhep"],
            outputs=["wab-beam-tri.slcio"],
            nevents=params.nevents,
            ignore_returncode=True)

# run simulated events in readout to generate triggers
readout = JobManager(description="Run the readout simulation to create triggers",
                     steering_resource=params.readout_steering,
                     java_args=["-DdisableSvtAlignmentConstants"],
                     defs={"detector": params.detector, "run": params.run},
                     inputs=["wab-beam-tri.slcio"],
                     outputs=["wab-beam-tri_readout"])

# run physics reconstruction
recon = JobManager(description="Run the MC recon",
                   steering_resource=params.recon_steering,
                   java_args=["-DdisableSvtAlignmentConstants"],
                   defs={"detector": params.detector, "run": params.run},
                   inputs=["wab-beam-tri_readout.slcio"],
                   outputs=["wab-beam-tri_recon"])
                        
# run the job
job.components=[stdhep_tri, rot_tri, sample_tri, stdhep_wab, rot_wab, sample_wab, rot_beam, sample_beam, merge, slic, readout, recon]

job.run()
