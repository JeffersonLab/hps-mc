import sys, random, argparse

from hpsmc.base import Job
from hpsmc.run_params import RunParameters
from hpsmc.generators import MG5, StdHepConverter
from hpsmc.tools import SLIC, JobManager, FilterMCBunches, DST

"""
Example job:
python tritrig_job.py -p 1pt05 -d "HPS-EngRun2015-Nominal-v3-fieldmap" -n 1000 -j 1 -f tritrig_xxx -o /tmp/tritrig_xxx -r 5772
"""

proc_name = "tritrig"

parser = argparse.ArgumentParser(description="Run a tritrig job")
parser.add_argument("-p", "--params", help="Run parameter key e.g. '1pt05'", required=False)
parser.add_argument("-d", "--detector", help="Detector name", required=True)
parser.add_argument("-n", "--nevents", help="Number of events", required=True)
parser.add_argument("-j", "--job", help="Job number", required=False)
parser.add_argument("-s", "--seed", help="Random seed for all components", required=False)
parser.add_argument("-f", "--filename", help="Base file name (do not include an extension!)", required=False)
parser.add_argument("-o", "--output-dir", help="Job output dir", required=False)
parser.add_argument("-r", "--run", help="Run number for conditions system", required=True)
cl = parser.parse_args()
print repr(cl)

if cl.job:
    job_num = int(cl.job)
else:
    job_num = 1
    
nevents = int(cl.nevents)

if cl.seed:
    seed = int(cl.seed)
else:
    seed = job_num
    
if cl.filename:
    filename = cl.filename
else:
    filename = proc_name + "_events"
    
run_param_key = cl.params

if cl.output_dir:
    output_dir = cl.output_dir
else:
    output_dir = None
    
cond_run = int(cl.run)
cond_detector = cl.detector
    
print    
print "---- Job Params ----"
print "job_num = %d" % job_num
print "nevents = %d" % nevents
print "seed = %d" % seed
print "filename = %s" % filename
print "run_param_key = %s" % run_param_key
print "output_dir = %s" % output_dir
print "cond_run = %d" % cond_run
print "cond_detector = %s" % cond_detector
print

# generate tritrig in MG4
mg = MG5(description="Generate tritrig events using MG4",
          name=proc_name,
          run_card="run_card_" + run_param_key + ".dat",
          outputs=[filename],
          rand_seed=seed,
          nevents=nevents)

# convert LHE output to stdhep
stdhep_cnv = StdHepConverter(description="Convert LHE events to StdHep using EGS5",
                             run_params=RunParameters(key=run_param_key),
                             rand_seed=seed,
                             inputs=[proc_name + "_events.lhe.gz"],
                             outputs=[filename + ".stdhep"])

# generate events in slic
slic = SLIC(description="Run detector simulation using SLIC",
            detector=cond_detector,
            inputs=[filename + ".stdhep"], 
            outputs=[filename + ".slcio"], 
            nevents=nevents)

# insert empty bunches expected by pile-up simulation
filter_bunches = FilterMCBunches(description="Run utility to space out events",
                                 java_args=["-DdisableSvtAlignmentConstants"],
                                 inputs=[filename + ".slcio"],
                                 outputs=[filename + "_filt.slcio"],
                                 ecal_hit_ecut=0.05,
                                 enable_ecal_energy_filter=True,
                                 nevents=2000000,
                                 event_interval=250)

# run simulated events in readout to generate triggers
readout = JobManager(description="Run the readout simulation to create triggers",
                     steering_resource="/org/hps/steering/readout/EngineeringRun2015TrigPairs1_Pass2.lcsim",
                     java_args=["-DdisableSvtAlignmentConstants"],
                     defs={"detector": cond_detector, "run": cond_run},
                     inputs=[filename + "_filt.slcio"],
                     outputs=[filename + "_readout"])

# run physics reconstruction
recon = JobManager(description="Run the MC recon",
                   steering_resource="/org/hps/steering/recon/EngineeringRun2015FullReconMC.lcsim",
                   java_args=["-DdisableSvtAlignmentConstants"],
                   defs={"detector": cond_detector, "run": cond_run},
                   inputs=[filename + "_readout.slcio"],
                   outputs=[filename + "_recon"])
                        
# set output files to copy
output_files = [filename + "_recon.slcio"]
                        
# create new job with components from above definitions
job = Job(name=proc_name + " job",
          components=[mg, stdhep_cnv, slic, filter_bunches, readout, recon],
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
