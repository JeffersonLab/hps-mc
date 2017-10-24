#!/usr/bin/env python

import sys, os, argparse

from hpsmc.job import Job
from hpsmc.generators import MG4, StdHepConverter
from hpsmc.tools import Unzip, StdHepTool, SLIC, FilterMCBunches, JobManager, LCIOTool

job = Job(name="AP job")
job.initialize()

params = job.params
filename = "aprime"

# generate A-prime events using Madgraph 4
ap = MG4(name="ap",
         run_card="run_card_"+params.run_params+".dat",
         params={"APMASS": params.apmass},
         outputs=[filename],
         nevents=params.nevents)

# unzip the LHE events to local file
unzip = Unzip(inputs=[filename+"_events.lhe.gz"])

# displace the time of decay using ctau param
displ = StdHepTool(name="lhe_tridents_displacetime",
                   inputs=[filename+"_events.lhe"],
                   outputs=[filename+".stdhep"],
                   args=["-l", str(params.ctau)])

# rotate events into beam coordinates and move vertex by 5 mm
rot = StdHepTool(name="beam_coords",
                 inputs=[filename+".stdhep"],
                 outputs=[filename+"_rot.stdhep"],
                 args=["-z", str(params.z)])

# print rotated AP events
dump = StdHepTool(name="print_stdhep",
                  inputs=[filename+"_rot.stdhep"])

# generate events in slic
slic = SLIC(detector=params.detector,
            inputs=[filename + "_rot.stdhep"], 
            outputs=[filename + ".slcio"],
            nevents=params.nevents,
            ignore_returncode=True)

# insert empty bunches expected by pile-up simulation
filter_bunches = FilterMCBunches(java_args=["-DdisableSvtAlignmentConstants"],
                                 inputs=[filename+".slcio"],
                                 outputs=[filename+"_filt.slcio"],
                                 ecal_hit_ecut=0.05,
                                 enable_ecal_energy_filter=True,
                                 nevents=2000000,
                                 event_interval=250)

# run simulated events in readout to generate triggers
readout = JobManager(steering_resource=params.readout_steering,
                     java_args=["-DdisableSvtAlignmentConstants"],
                     run=params.run,
                     detector=params.detector,
                     inputs=[filename+"_filt.slcio"],
                     outputs=[filename+"_readout"])

# run physics reconstruction
recon = JobManager(steering_resource=params.recon_steering,
                   java_args=["-DdisableSvtAlignmentConstants"],
                   run=params.run,
                   detector=params.detector,
                   inputs=[filename+"_readout.slcio"],
                   outputs=[filename+"_recon"])

# count output recon events
count = LCIOTool(name="count",
                 args=["-f", filename+"_recon.slcio"])
                        
# define job components
job.components = [ap, unzip, displ, rot, dump, slic, filter_bunches, readout, recon, count]

# run the job
job.run()
