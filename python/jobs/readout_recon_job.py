from hpsmc.tools import JobManager, FilterBunches, LCIOCount

job.description = 'Filter bunches with readout and recon'

# Insert empty bunches expected by pile-up simulation
filter_bunches = FilterBunches(nevents=2000000)

# Run simulated events in readout to generate triggers
readout = JobManager(steering='readout')

# Run physics reconstruction
reco = JobManager(steering='recon')

job.add([filter_bunches, readout, reco])
