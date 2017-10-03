#!/usr/bin/env python

"""
Script to generate 'tritrig-wab-beam' events (Oh my!) from sets of input LCIO files.

Based on this Auger script:

https://github.com/JeffersonLab/hps-mc/blob/master/scripts/mc_scripts/readout/tritrig-wab-beam_1pt05_Nominal.xml

This script accepts an arbitrary number of input files per job but the two lists for 'tritrig' and 'wab-beam'
must be the same length or an error will be raised.

"""

from hpsmc.job import Job
from hpsmc.tools import FilterMCBunches, LCIOTool, JobManager

job = Job(name="tritrig job")
job.initialize()

params = job.params
input_files = params.input_files

tritrig_files = []
wab_files = [] 
for input_file in input_files:
    if "tritrig" in input_file:
        tritrig_files.append(input_file)
    elif "wab-beam" in input_file:
        wab_files.append(input_file)

if len(tritrig_files) != len(wab_files):
    raise Exception("The 'tritrig' and 'wab-beam' input file lists must have the same length.")

# filter and space tritrig files
filter_tritrig = FilterMCBunches(java_args=["-DdisableSvtAlignmentConstants"],
                                 inputs=tritrig_files,
                                 outputs=["tritrig_filter.slcio"],
                                 ecal_hit_ecut=0.05,
                                 enable_ecal_energy_filter=True,
                                 nevents=2000000,
                                 event_interval=250)

# filter and space wab-beam files
filter_wab = FilterMCBunches(java_args=["-DdisableSvtAlignmentConstants"],
                             inputs=wab_files,
                             outputs=["wab-beam_filter.slcio"],
                             event_interval=0,
                             enable_ecal_energy_filter=False,
                             args=["-a"])

# merge the files together
merge = LCIOTool(name="merge",
                 args=["-f", "tritrig_filter.slcio", "-f", "wab-beam_filter.slcio", "-o", "merged.slcio"])

# run simulated events in readout to generate triggers
readout = JobManager(steering_resource=params.readout_steering,
                     java_args=["-DdisableSvtAlignmentConstants"],
                     run=params.run,
                     detector=params.detector,
                     inputs=["merged.slcio"],
                     outputs=["readout"])

# run physics reconstruction
recon = JobManager(steering_resource=params.recon_steering,
                   java_args=["-DdisableSvtAlignmentConstants"],
                   run=params.run,
                   detector=params.detector,
                   inputs=["readout.slcio"],
                   outputs=["tritrig-wab-beam"])
 
job.components = [filter_tritrig, filter_wab, merge, readout, recon]
job.run()
