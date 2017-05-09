import os, sys

from hpsmc.base import Job
from hpsmc.run_params import RunParameters
from hpsmc.generators import MG4, StdHepConverter
from hpsmc.tools import SLIC, HPSJava

# generate tritrig in MG4
mg4 = MG4(name="tritrig",
    run_card="run_card_1pt05.dat",
    outputs=["tritrig_events"])

# convert lhe output to stdhep
stdhep_cnv = StdHepConverter(run_params=RunParameters(key="1pt05"),
    inputs=["tritrig_events.lhe"], 
    outputs=["tritrig_events.stdhep"])

# generate events in slic
slic = SLIC(detector="HPS-EngRun2015-Nominal-v3-fieldmap", 
    inputs=["tritrig_events.stdhep"], 
    outputs=["sim_events.slcio"], 
    nevents=1000)

# run simulated events in readout to generate triggers
readout = HPSJava(steering_resource="/org/hps/steering/readout/EngineeringRun2015TrigPairs1_Pass2.lcsim",
    defs={"detector": "HPS-EngRun2015-Nominal-v3-fieldmap", "run": "5772"},
    inputs=["sim_events.slcio"],
    outputs=["readout_events"])

# create new job with components from above
job = Job(name="tritrig job test", components=[mg4, stdhep_cnv, slic, readout])
 
# run the job
job.setup()
job.run()
job.cleanup()