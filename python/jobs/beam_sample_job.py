"""
Python script for creating beam background events.

Based on this Auger script:

https://github.com/JeffersonLab/hps-mc/blob/master/scripts/mc_scripts/slic/beam.xml

"""

import os
from hpsmc.job import Job
from hpsmc.tools import StdHepTool, MoveFiles

# default parameters
def_params = { 
    "beam_sigma_x": 0.300,
    "beam_sigma_y": 0.030,
    "target_z": 0.0,
    "beam_rotation": 0.0305
}

# define job with defaults
job = Job(name="beam job")
job.set_default_params(def_params)
job.initialize()

# get params
params = job.params

# create component to rotate into beam coords for each input
rotated_files = []
for i in job.input_files.keys():
   
    fname = os.path.splitext(i)[0] + "_rot.stdhep"
    rotated_files.append(fname)

    rot = StdHepTool(name="beam_coords",
                 inputs=[i],
                 outputs=[fname])

    # add beam parameters which may come from JSON or defaults
    rot.args.extend(["-x", str(params['beam_sigma_x']),
                     "-y", str(params['beam_sigma_y']),
                     "-z", str(params['target_z'])])

    # need to check for this as it has no reasonable default
    if "beam_skew" in params:
        rot.args.extend(["-q", str(params.beam_skew)])

    job.components.append(rot)

# create component to sample each input to new output
sampled_files = []
for i in rotated_files:
    fname = i.replace("_rot.stdhep", "_sampled")
    sampled_files.append(fname + "_1.stdhep")
    sample = StdHepTool(name="random_sample",
                    inputs=[i],
                    outputs=[fname])
    job.components.append(sample)

# create component to move files to reasonable output names
output_files = []
for i in sampled_files:
    fname = i.replace("_1.stdhep", ".stdhep")
    output_files.append(fname)

# move files to strip the number appended by sampling tool
mv = MoveFiles(inputs=sampled_files, outputs=output_files)
job.components.append(mv)

# run the job
job.run()
