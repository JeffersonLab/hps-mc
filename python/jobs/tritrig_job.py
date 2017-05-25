"""
Python script for generating 'tritrig' events in MG5 and running through simulation, readout and reconstruction. 
"""

import sys, random

from hpsmc.base import Job, JobStandardArgs, JobParameters
from hpsmc.run_params import RunParameters
from hpsmc.generators import MG5, StdHepConverter
from hpsmc.tools import SLIC, JobManager, FilterMCBunches, StdHepTool

cl = JobStandardArgs()
cl.parse_args()

job_num = cl.job
output_dir = cl.output_dir
seed = cl.seed
params = JobParameters(cl.params)

print params

# set base output filename from params
filename = params.filename

# used for intermediate file names
proc_name = "tritrig"

# generate tritrig in MG5
mg = MG5(description="Generate tritrig events using MG5",
         name="tritrig",
         run_card="run_card_"+params.run_params+".dat",
         outputs=[filename],
         rand_seed=seed,
         nevents=params.nevents)

# convert LHE output to stdhep
stdhep_cnv = StdHepConverter(description="Convert LHE events to StdHep using EGS5",
                             run_params=RunParameters(key=params.run_params),
                             rand_seed=seed,
                             inputs=[proc_name+"_events.lhe.gz"],
                             outputs=[proc_name+".stdhep"])

# rotate events into beam coords
rot = StdHepTool(description="Rotate events into beam coords",
                 name="beam_coords",
                 inputs=[proc_name+".stdhep"],
                 outputs=[proc_name+"_rot.stdhep"])

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

# create new job with components from above definitions
job = Job(name="tritrig job",
          components=[mg, stdhep_cnv, rot, slic, filter_bunches, readout, recon],
          output_dir=output_dir,
          output_files=output_files,
          job_num=job_num,
          ignore_return_codes=True,
          append_job_num=True)
 
# setup each job component
job.setup()

# run the full job
job.run()

# copy files from the run dir to output dir
job.copy_output_files()

# run cleanup of each component
job.cleanup()
