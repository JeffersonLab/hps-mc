"""
Python script for generating 'wab-beam' events.
"""

import sys, random

import hpsmc.func as func
from hpsmc.job import Job
from hpsmc.run_params import RunParameters
from hpsmc.generators import MG5, StdHepConverter
from hpsmc.tools import StdHepTool

job = Job(name="tritrig job")
job.initialize()

params = job.params

wab_input = "wab.lhe.gz"
beam_input = "beam.stdhep"

if beam_input not in params.input_files:
    raise Exception("Missing '%s input file." % beam_input)

if wab_input not in params.input_files:
    raise Exception("Missing '%s' input file." % wab_input)

run_params = RunParameters(key=params.run_params)
mu = func.mu(run_params, wab_input)

stdhep_cnv = StdHepConverter(run_params=run_params,
                             inputs=[wab_input],
                             outputs=["wab.stdhep"])

rot_wab = StdHepTool(name="beam_coords",
                     inputs=["wab.stdhep"],
                     outputs=["wab_rot.stdhep"],
                     args=["-s", str(params.seed), "-z", str(params.z)])

"""
merge_poisson -m"$mu" -N1 -n500000  rot_wab.stdhep sampled_wab -s 23${num}
"""
sample_wab = StdHepTool(name="merge_poisson",
                       inputs=["wab_rot.stdhep"],
                       outputs=["wab_sampled"],
                       args=["-m", str(mu), "-N", "1", "-n", "500000", "-s", str(params.seed)])

"""
beam_coords beam.stdhep rot_beam.stdhep -s 14${num} -z -5.0
"""
rot_beam = StdHepTool(name="beam_coords",
                      inputs=["beam.stdhep"],
                      outputs=["beam_rot.stdhep"],
                      args=["-s", str(params.seed), "-z", str(params.z)])
        
""" 
random_sample rot_beam.stdhep sampled_beam -s 15${num}
"""
sample_beam = StdHepTool(name="random_sample",
                         inputs=["beam_rot.stdhep"],
                         outputs=["beam_sampled"],
                         args=["-s", str(params.seed)])

"""
merge_files sampled_beam_1.stdhep sampled_wab_1.stdhep  wab-beam.stdhep
"""
merge = StdHepTool(name="merge_files",
                   inputs=["beam_sampled_1.stdhep", "wab_sampled_1.stdhep"],
                   outputs=["wab-beam.stdhep"])

# run the job
job.components=[stdhep_cnv, rot_wab, sample_wab, rot_beam, sample_beam, merge] 
job.run()
