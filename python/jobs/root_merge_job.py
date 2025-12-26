"""
Merge ROOT files job script for hps-mc

This job merges multiple ROOT files into a single output file using hadd.

Example JSON parameters:
{
    "job_id": 1,
    "input_files": {
        "input1.root": "/path/to/input1.root",
        "input2.root": "/path/to/input2.root",
        "input3.root": "/path/to/input3.root"
    },
    "output_files": {
        "merged.root": "merged_output.root"
    },
    "output_dir": "output",
    "force": true,
    "compression": 6,
    "validate": true
}

Usage:
    python job.py run merge_root_job.py job_params.json
    or from within hps-mc installation:
    python python/hpsmc/job.py run python/jobs/merge_root_job.py job_params.json
"""

from hpsmc.tools import MergeROOT

# The 'job' object is provided by the framework when this script is executed
# via exec() in the Job.run() method

# Set job description
job.description = "Merge ROOT files using hadd"

# Get list of input files from the job parameters
# The keys of input_files dict are the local file names
input_list = list(job.input_files.keys())

# Get the output file name
# The key of output_files dict is the source (local name)
output_file = list(job.output_files.keys())[0]

# Set up optional parameters with defaults
force_overwrite = job.params.get('force', True)
compression_level = job.params.get('compression', None)
validate_merge = job.params.get('validate', True)

# Create the MergeROOT component
merge = MergeROOT(
    name="merge_root",
    inputs=input_list,
    outputs=[output_file],
    force=force_overwrite,
    compression=compression_level,
    validate=validate_merge
)

# Add component to the job
job.add(merge)
