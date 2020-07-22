from hpsmc.tools import JobManager, FilterBunches, LCIOCount

job.description = 'Filter bunches with readout and recon'

count_input = LCIOCount()

# Insert empty bunches expected by pile-up simulation
filter_bunches = FilterBunches()

count_filt = LCIOCount()

# Run simulated events in readout to generate triggers
readout = JobManager(steering='readout')

count_readout = LCIOCount()

# Run physics reconstruction
reco = JobManager(steering='recon')

count_reco = LCIOCount()

job.add([count_input, filter_bunches, count_filt, readout, count_readout, reco, count_reco])
