"""
Python script for generating 'tritrig' events in MG5 and running through simulation, readout and reconstruction. 
"""

import sys, random

from hpsmc.job import Job
from hpsmc.run_params import RunParameters
from hpsmc.generators import MG5, StdHepConverter
from hpsmc.tools import SLIC, JobManager, FilterMCBunches, StdHepTool

job = Job(name="tritrig job")
job.parse_args()

params = job.params
filename = params.filename

# used for intermediate file names
procname = "tritrig"

# generate tritrig in MG5
mg = MG5(description="Generate tritrig events using MG5",
         name=procname,
         run_card="run_card_"+params.run_params+".dat",
         outputs=[filename],
         nevents=params.nevents)

# convert LHE output to stdhep
stdhep_cnv = StdHepConverter(description="Convert LHE events to StdHep using EGS5",
                             run_params=RunParameters(key=params.run_params),
                             inputs=[procname+"_events.lhe.gz"],
                             outputs=[procname+".stdhep"])

# rotate events into beam coords
rot = StdHepTool(description="Rotate events into beam coords",
                 name="beam_coords",
                 inputs=[procname+".stdhep"],
                 outputs=[procname+"_rot.stdhep"])

# generate events in slic
slic = SLIC(description="Run detector simulation using SLIC",
            detector=params.detector,
            inputs=[procname+"_rot.stdhep"], 
            outputs=[procname+".slcio"], 
            nevents=params.nevents,
            ignore_returncode=True)

# insert empty bunches expected by pile-up simulation
filter_bunches = FilterMCBunches(description="Run utility to space out events",
                                 java_args=["-DdisableSvtAlignmentConstants"],
                                 inputs=[procname+".slcio"],
                                 outputs=[procname+"_filt.slcio"],
                                 ecal_hit_ecut=0.05,
                                 enable_ecal_energy_filter=True,
                                 nevents=2000000,
                                 event_interval=250)

# run simulated events in readout to generate triggers
readout = JobManager(description="Run the readout simulation to create triggers",
                     steering_resource=params.readout_steering,
                     java_args=["-DdisableSvtAlignmentConstants"],
                     defs={"detector": params.detector, "run": params.run},
                     inputs=[procname+"_filt.slcio"],
                     outputs=[procname+"_readout"])

# run physics reconstruction
recon = JobManager(description="Run the MC recon",
                   steering_resource=params.recon_steering,
                   java_args=["-DdisableSvtAlignmentConstants"],
                   defs={"detector": params.detector, "run": params.run},
                   inputs=[procname+"_readout.slcio"],
                   outputs=[filename+"_recon"])
                        
 
# run the job
job.components=[mg, stdhep_cnv, rot, slic, filter_bunches, readout, recon] 
job.run()
