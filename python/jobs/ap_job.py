import sys, os, argparse

from hpsmc.generators import MG4, StdHepConverter
from hpsmc.base import Job, JobParameters, JobStandardArgs
from hpsmc.run_params import RunParameters
from hpsmc.tools import Unzip, StdHepTool, SLIC, FilterMCBunches, JobManager, LCIOTool

cl = JobStandardArgs()
cl.parse_args()

job_num = cl.job
output_dir = cl.output_dir
seed = cl.seed
params = JobParameters(cl.params)

print params

# set base filename from params
filename = params.filename

# generate A-prime events using Madgraph 4
ap = MG4(name="ap",
         run_card="run_card_"+params.run_params+".dat",
         params={"APMASS": params.apmass},
         outputs=[filename],
         seed=seed,
         nevents=params.nevents)

# unzip the LHE events to local file
unzip = Unzip(inputs=[filename+"_events.lhe.gz"])

# displace the time of decay using ctau param
displ = StdHepTool(name="lhe_tridents_displacetime",
                   inputs=[filename+"_events.lhe"],
                   outputs=[filename+".stdhep"],
                   args=["-s", str(seed), "-l", str(params.ctau)])

# rotate events into beam coordinates and move vertex by 5 mm
rot = StdHepTool(name="beam_coords",
                 inputs=[filename+".stdhep"],
                 outputs=[filename+"_rot.stdhep"],
                 args=["-s", str(seed), "-z", str(params.z)])

# print rotated AP events
dump = StdHepTool(name="print_stdhep",
                  inputs=[filename+"_rot.stdhep"])

# generate events in slic
slic = SLIC(detector=params.detector,
            inputs=[filename + "_rot.stdhep"], 
            outputs=[filename + ".slcio"], 
            nevents=params.nevents)

# insert empty bunches expected by pile-up simulation
filter_bunches = FilterMCBunches(java_args=["-DdisableSvtAlignmentConstants"],
                                 inputs=[filename+".slcio"],
                                 outputs=[filename+"_filt.slcio"],
                                 ecal_hit_ecut=0.05,
                                 enable_ecal_energy_filter=True,
                                 nevents=2000000,
                                 event_interval=250)

# run simulated events in readout to generate triggers
readout = JobManager(description="Run the readout simulation to create triggers",
                     steering_resource=params.readout_steering,
                     java_args=["-DdisableSvtAlignmentConstants"],
                     defs={"detector": params.detector, "run": params.run},
                     inputs=[filename+"_filt.slcio"],
                     outputs=[filename+"_readout"])

# run physics reconstruction
recon = JobManager(steering_resource=params.recon_steering,
                   java_args=["-DdisableSvtAlignmentConstants"],
                   defs={"detector": params.detector, "run": params.run},
                   inputs=[filename+"_readout.slcio"],
                   outputs=[filename+"_recon"])

# count output recon events
count = LCIOTool(name="count",
                 args=["-f", filename+"_recon.slcio"])
                        
# define the job
job = Job(name="AP job",
          components=[ap, unzip, displ, rot, dump, slic, filter_bunches, readout, recon, count],
          output_files=[filename+"_recon.slcio"],
          job_num=job_num,
          output_dir=output_dir,
          append_job_num=True)

# setup and run the job
job.setup()
job.run()
job.copy_output_files()
job.cleanup()
