"""
Merge ROOT files job script for hps-mc

This job merges multiple ROOT files into a single output file using hadd.

Example JSON parameters:
{
    "job_id": 1,
    "input_files": {
        "/path/to/input1.root": "input1.root",
        "/path/to/input2.root": "input2.root",
        "/path/to/input3.root": "input3.root"
    },
    "output_files": {
        "merged.root": "merged_output.root",
        "merged_stats.json": "merged_stats.json"
    },
    "output_dir": "output",
    "force": true,
    "compression": 6,
    "validate": true,
    "write_stats": true
}
"""

from hpsmc.tools import MergeROOT

# The 'job' object is provided by the framework when this script is executed
# via exec() in the Job.run() method

# Set job description
job.description = "Merge ROOT files using hadd"

# Get list of input files from the job parameters.
# input_files maps {source_path: local_name}; the values are the local (staged) file names in the run dir,
# which is what hadd must read (the keys are the original/remote source paths, not present on the compute node).
input_list = list(job.input_files.values())

# Get the output file name (first .root file in output_files)
output_file = None
for key in job.output_files.keys():
    if key.endswith('.root'):
        output_file = key
        break
if output_file is None:
    output_file = list(job.output_files.keys())[0]

# Set up optional parameters with defaults
force_overwrite = job.params.get('force', True)
compression_level = job.params.get('compression', None)
validate_merge = job.params.get('validate', True)
write_stats = job.params.get('write_stats', True)
job_id = job.params.get('job_id', None)

# Create the MergeROOT component
merge = MergeROOT(
    name="merge_root",
    inputs=input_list,
    outputs=[output_file],
    force=force_overwrite,
    compression=compression_level,
    validate=validate_merge,
    write_stats=write_stats,
    job_id=job_id
)

# Add component to the job
job.add(merge)
