"""
Python script for creating beam background events.

Based on this Auger script:

https://github.com/JeffersonLab/hps-mc/blob/master/scripts/mc_scripts/slic/beam.xml

"""

from hpsmc.base import Job
from hpsmc.tools import StdHepTool

job = Job(name="beam job")
job.parse_args()

input_file = job.input_files["beam.stdhep"]
input_basename = "beam"

rot = StdHepTool(name="beam_coords",
                 inputs=[input_file],
                 outputs=[input_basename+"_rot.stdhep"])

sample = StdHepTool(name="random_sample",
                    inputs=[input_basename+"_rot.stdhep"],
                    outputs=[input_basename+"_sampled"])

job.components = [rot, sample]

job.run()