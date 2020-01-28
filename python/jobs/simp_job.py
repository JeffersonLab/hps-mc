#!/usr/bin/env python

"""
Python script for generating 'simp' events in MG5 and running through simulation, readout and reconstruction. 
"""

import sys, random

from hpsmc.job import Job
from hpsmc.run_params import RunParameters
from hpsmc.generators import MG5, StdHepConverter
from hpsmc.tools import SLIC, JobManager, FilterMCBunches, StdHepTool

job = Job(name="simp job")
job.initialize()

params = job.params

# used for intermediate file names
procname = "simp"

# generate tritrig in MG5
mg = MG5(name=procname,
         #run_card="run_card_"+params.run_params+".dat",
         run_card="run_card.dat",
         param_card="param_card.dat",
         params={"map":params.map,
                 "mpid":params.mpid,
                 "mrhod":params.mrhod},
         outputs=[procname],
         nevents=params.nevents)

# convert LHE output to stdhep
stdhep_cnv = StdHepConverter(run_params=RunParameters(key=params.run_params),
                             inputs=[procname+"_unweighted_events.lhe.gz"],
                             outputs=[procname+".stdhep"])

# rotate events into beam coords
rot = StdHepTool(name="beam_coords",
                 inputs=[procname+".stdhep"],
                 outputs=[procname+"_rot.stdhep"])

# print out rotated simp events
dump = StdHepTool(name="print_stdhep",
                  inputs=[procname+"_rot.stdhep"])

# generate events in slic
slic = SLIC(detector=params.detector,
             inputs=[procname+"_rot.stdhep"], 
             outputs=[procname+".slcio"], 
             nevents=params.nevents,
             ignore_returncode=True)

# insert empty bunches expected by pile-up simulation
filter_bunches = FilterMCBunches(java_args=["-DdisableSvtAlignmentConstants"],
                                 inputs=[procname+".slcio"],
                                 outputs=[procname+"_filt.slcio"],
                                 ecal_hit_ecut=0.05,
                                 enable_ecal_energy_filter=True,
                                 nevents=2000000,
                                 event_interval=250)

# run simulated events in readout to generate triggers
readout = JobManager(steering_resource=params.readout_steering,
                     java_args=["-DdisableSvtAlignmentConstants"],
                     run=params.run,
                     detector=params.detector,
                     inputs=[procname+"_filt.slcio"],
                     outputs=[procname+"_readout"])

# run physics reconstruction
recon = JobManager(steering_resource=params.recon_steering,
                   java_args=["-DdisableSvtAlignmentConstants"],
                   defs={"detector": params.detector, "run": params.run},
                   inputs=[procname+"_readout.slcio"],
                   outputs=[procname+"_recon"])
 
# run the job
job.components=[mg, stdhep_cnv, rot, dump, slic, filter_bunches, readout, recon] 
job.run()
