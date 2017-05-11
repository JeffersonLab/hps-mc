import os, sys, random

from hpsmc.base import Job
from hpsmc.run_params import RunParameters
from hpsmc.generators import MG4, StdHepConverter
from hpsmc.tools import SLIC, JobManager, FilterMCBunches, DST

job_rand_seed = random.randint(1, 1000000)

print "Set job test rand seed to '%s'" % str(job_rand_seed)

# generate tritrig in MG4
mg4 = MG4(description="Generate tritrig events using MG4",
          name="tritrig",
          run_card="run_card_1pt05.dat",
          outputs=["tritrig"],
          rand_seed=job_rand_seed,
          nevents=1000)

# convert LHE output to stdhep
stdhep_cnv = StdHepConverter(description="Convert LHE events to StdHep using EGS5",
                             run_params=RunParameters(key="1pt05"),
                             rand_seed=job_rand_seed,
                             inputs=["tritrig_events.lhe.gz"],
                             outputs=["tritrig.stdhep"])

# generate events in slic
slic = SLIC(description="Run detector simulation using SLIC",
            detector="HPS-EngRun2015-Nominal-v3-fieldmap",
            inputs=["tritrig.stdhep"], 
            outputs=["tritrig.slcio"], 
            nevents=1000)

# insert empty bunches expected by pile-up simulation
filter_bunches = FilterMCBunches(description="Run utility to space out events",
                                 java_args=["-DdisableSvtAlignmentConstants"],
                                 inputs=["tritrig.slcio"],
                                 outputs=["tritrig_filt.slcio"],
                                 ecal_hit_ecut=0.05,
                                 enable_ecal_energy_filter=True,
                                 nevents=2000000,
                                 event_interval=250)

# run simulated events in readout to generate triggers
readout = JobManager(description="Run the readout simulation to create triggers",
                     steering_resource="/org/hps/steering/readout/EngineeringRun2015TrigPairs1_Pass2.lcsim",
                     java_args=["-DdisableSvtAlignmentConstants"],
                     defs={"detector": "HPS-EngRun2015-Nominal-v3-fieldmap", "run": "5772"},
                     inputs=["tritrig_filt.slcio"],
                     outputs=["tritrig_readout"])

# run physics reconstruction
recon = JobManager(description="Run the MC recon",
                   steering_resource="/org/hps/steering/recon/EngineeringRun2015FullReconMC.lcsim",
                   java_args=["-DdisableSvtAlignmentConstants"],
                   defs={"detector": "HPS-EngRun2015-Nominal-v3-fieldmap", "run": "5772"},
                   inputs=["tritrig_readout.slcio"],
                   outputs=["tritrig_recon"])

# create recon tuples
make_tuples = JobManager(description="Create tuples",
                         steering_resource="/org/hps/steering/analysis/MakeTuplesMC.lcsim",
                         inputs=["tritrig_recon.slcio"],
                         outputs=["tuple"])

# run hps-dst to generate ROOT DST output
make_dst = DST(description="Create DST file",
               inputs=["tritrig_recon.slcio"], 
               outputs=["dst.root"],
               nevents=1000)
                        
# set output files to copy
output_files = ["tritrig_recon.slcio", 
                {"dst.root": "mydst.root"}]
                        
# create new job with components from above definitions
job = Job(name="tritrig job test",
          components=[mg4, stdhep_cnv, slic, filter_bunches, readout, recon, make_dst, make_tuples],
          output_dir="/tmp/tritrig_test",
          output_files=output_files,
          job_num=1234,
          append_job_num=True)
 
# setup each job component
job.setup()

# run the full job
job.run()

# copy files from the run dir to output dir
job.copy_output_files()

# run cleanup of each component
job.cleanup()
