"""
Python script for generating 'tritrig' events in MG5 and running through simulation, readout and reconstruction. 
"""

import sys, random

from hpsmc.base import Job, JobStandardArgs
from hpsmc.run_params import RunParameters
from hpsmc.generators import MG5, StdHepConverter
from hpsmc.tools import SLIC, JobManager, FilterMCBunches, StdHepTool

proc_name = "tritrig"

job_args = JobStandardArgs(proc_name)
job_args.parse_args()
job_args.print_args()
    
# generate tritrig in MG4
mg = MG5(description="Generate tritrig events using MG4",
          name=proc_name,
          run_card="run_card_" + job_args.run_param_key + ".dat",
          outputs=[job_args.filename],
          rand_seed=job_args.seed,
          nevents=job_args.nevents)

# convert LHE output to stdhep
stdhep_cnv = StdHepConverter(description="Convert LHE events to StdHep using EGS5",
                             run_params=RunParameters(key=job_args.run_param_key),
                             rand_seed=job_args.seed,
                             inputs=[proc_name + "_events.lhe.gz"],
                             outputs=[job_args.filename + ".stdhep"])

# rotate events into beam coords
rot = StdHepTool(description="Rotate events into beam coords",
                 name="beam_coords",
                 inputs=[job_args.filename + ".stdhep"],
                 outputs=[job_args.filename + "_rot.stdhep"])

# generate events in slic
slic = SLIC(description="Run detector simulation using SLIC",
            detector=job_args.cond_detector,
            inputs=[job_args.filename + "_rot.stdhep"], 
            outputs=[job_args.filename + ".slcio"], 
            nevents=job_args.nevents)

# insert empty bunches expected by pile-up simulation
filter_bunches = FilterMCBunches(description="Run utility to space out events",
                                 java_args=["-DdisableSvtAlignmentConstants"],
                                 inputs=[job_args.filename + ".slcio"],
                                 outputs=[job_args.filename + "_filt.slcio"],
                                 ecal_hit_ecut=0.05,
                                 enable_ecal_energy_filter=True,
                                 nevents=2000000,
                                 event_interval=250)

# run simulated events in readout to generate triggers
readout = JobManager(description="Run the readout simulation to create triggers",
                     steering_resource=job_args.readout_steering,
                     java_args=["-DdisableSvtAlignmentConstants"],
                     defs={"detector": job_args.cond_detector, "run": job_args.cond_run},
                     inputs=[job_args.filename + "_filt.slcio"],
                     outputs=[job_args.filename + "_readout"])

# run physics reconstruction
recon = JobManager(description="Run the MC recon",
                   steering_resource=job_args.recon_steering,
                   java_args=["-DdisableSvtAlignmentConstants"],
                   defs={"detector": job_args.cond_detector, "run": job_args.cond_run},
                   inputs=[job_args.filename + "_readout.slcio"],
                   outputs=[job_args.filename + "_recon"])
                        
# set output files to copy
output_files = [job_args.filename + "_recon.slcio"]
                        
# create new job with components from above definitions
job = Job(name=proc_name + " job",
          components=[mg, stdhep_cnv, rot, slic, filter_bunches, readout, recon],
          output_dir=job_args.output_dir,
          output_files=output_files,
          job_num=job_args.job_num,
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
