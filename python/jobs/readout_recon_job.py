import sys, random

from hpsmc.job import Job
from hpsmc.run_params import RunParameters
from hpsmc.generators import MG5, StdHepConverter
from hpsmc.tools import SLIC, JobManager, FilterBunches, StdHepTool

job = Job()

# Insert empty bunches expected by pile-up simulation
filter_bunches = FilterBunches(nevents=2000000,
                               append='_filt')

# Run simulated events in readout to generate triggers
readout = JobManager(steering='readout',
                    replace={'_filt': '_readout'})

# Run physics reconstruction
reco = JobManager(steering='recon',
                   replace={'_readout': '_recon'})

job.add([filter_bunches, readout, reco])
job.run()
