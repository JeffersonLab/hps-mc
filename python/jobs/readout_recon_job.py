from hpsmc.tools import JobManager, FilterBunches, LCIOCount

job.description = 'Filter bunches with readout and recon'

# Insert empty bunches expected by pile-up simulation
filter_bunches = FilterBunches(nevents=2000000)

count_filter = LCIOCount()

# Run simulated events in readout to generate triggers
readout = JobManager(steering='readout')

# Run physics reconstruction
reco = JobManager(steering='recon')

#job.ptag('recon', reco.output_files()[0])

job.add([filter_bunches, count_filter, readout, reco])
