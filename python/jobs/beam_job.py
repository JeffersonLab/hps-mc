"""
Python script for creating beam background events
https://github.com/JeffersonLab/hps-mc/blob/master/scripts/mc_scripts/slic/beam.xml
"""

import sys, random

from hpsmc.base import Job, JobStandardArgs, JobParameters
from hpsmc.run_params import RunParameters
from hpsmc.generators import MG5, StdHepConverter
from hpsmc.tools import SLIC, JobManager, FilterMCBunches, StdHepTool

# job = Job(name="beam job")
# job.parse_args()

cl = JobStandardArgs()
cl.parse_args()

job_num = cl.job
output_dir = cl.output_dir
seed = cl.seed
params = JobParameters(cl.params)

# set base output filename from params
#filename = params.filename

input_filename = "beam"

rot = StdHepTool(name="beam_coords",
                 inputs=[input_filename+".stdhep"],
                 outputs=[input_filename+"_rot.stdhep"],
                 rand_seed=seed)

sample = StdHepTool(name="random_sample",
                    inputs=[input_filename+"_rot.stdhep"],
                    outputs=[input_filename+"_sampled"],
                    rand_seed=seed)
"""
print_stdhep = StdHepTool(name="print_stdhep",
                          inputs=[input_filename+"_sampled_1.stdhep"])                          

if "wab.stdhep" in cl.input_files:
    # TODO: do wabs here
    pass

# generate events in slic
slic = SLIC(description="Run detector simulation using SLIC",
            detector=params.detector,
            inputs=[proc_name+"_rot.stdhep"], 
            outputs=[proc_name+".slcio"], 
            nevents=params.nevents)

# insert empty bunches expected by pile-up simulation
filter_bunches = FilterMCBunches(description="Run utility to space out events",
                                 java_args=["-DdisableSvtAlignmentConstants"],
                                 inputs=[proc_name+".slcio"],
                                 outputs=[proc_name+"_filt.slcio"],
                                 ecal_hit_ecut=0.05,
                                 enable_ecal_energy_filter=True,
                                 nevents=2000000,
                                 event_interval=250)

# run simulated events in readout to generate triggers
readout = JobManager(description="Run the readout simulation to create triggers",
                     steering_resource=params.readout_steering,
                     java_args=["-DdisableSvtAlignmentConstants"],
                     defs={"detector": params.detector, "run": params.run},
                     inputs=[proc_name+"_filt.slcio"],
                     outputs=[proc_name+"_readout"])

# run physics reconstruction
recon = JobManager(description="Run the MC recon",
                   steering_resource=params.recon_steering,
                   java_args=["-DdisableSvtAlignmentConstants"],
                   defs={"detector": params.detector, "run": params.run},
                   inputs=[proc_name+"_readout.slcio"],
                   outputs=[filename+"_recon"])
                        
# set output files to copy
output_files = [filename+"_recon.slcio"]
"""

# create new job with components from above definitions
job = Job(job_args=cl,
          name="beam job",
          components=[rot, sample])

# , print_stdhep

job.copy_input_files()

# setup each job component
job.setup()

# run the full job
job.run()

# copy files from the run dir to output dir
#job.copy_output_files()

# run cleanup of each component
job.cleanup()
