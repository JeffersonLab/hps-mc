# MergeROOT Tool for hps-mc

This package provides a MergeROOT tool for the hps-mc framework to merge ROOT files using the `hadd` utility.

## Files

- **`MergeROOT` class** (to be added to `python/hpsmc/tools.py`): The component class that wraps hadd
- **`merge_root_job.py`** (to be placed in `python/jobs/`): Job script for merging ROOT files
- **`merge_root_params.json`**: Example parameter file for a single merge job
- **`merge_workflow_example.json`**: Example parameter file for batch workflow

## Installation

1. Add the `MergeROOT` class to your `python/hpsmc/tools.py` file
2. Place `merge_root_job.py` in the `python/jobs/` directory
3. Make sure ROOT is installed and `hadd` is available in your PATH

## Usage

### Running a Single Job

Create a JSON parameter file (e.g., `my_merge.json`):

```json
{
    "job_num": 1,
    "input_files": {
        "input1.root": "/path/to/file1.root",
        "input2.root": "/path/to/file2.root",
        "input3.root": "/path/to/file3.root"
    },
    "output_files": {
        "merged.root": "output_merged.root"
    },
    "output_dir": "output",
    "force": true,
    "compression": 6
}
```

Run the job:

```bash
python python/jobs/merge_root_job.py my_merge.json
```

### Creating a Workflow

For batch processing multiple merge operations:

```bash
hps-mc-workflow -n 10 -w merge_workflow python/jobs/merge_root_job.py merge_workflow_example.json
```

This creates a workflow file `merge_workflow.json` containing 10 jobs.

### Submitting to Batch System

At SLAC (using LSF):
```bash
hps-mc-bsub merge_workflow.json -W 60 -q long
```

At JLab (using PBS):
```bash
hps-mc-jsub merge_workflow.json
```

## Parameters

### Required Parameters

- **`input_files`**: Dictionary mapping local filenames to source paths
  - Keys: local file names used within the job
  - Values: absolute paths to input ROOT files
  - Supports wildcards when used with workflows

- **`output_files`**: Dictionary mapping source to destination filenames
  - Keys: local output filename (source)
  - Values: final output filename (destination)

- **`output_dir`**: Directory where output files will be written

### Optional Parameters

- **`force`** (bool, default: true): Force overwrite if output file exists
- **`compression`** (int, default: None): ROOT compression level (0-9)
  - 0 = no compression
  - 1 = fast compression
  - 6 = default compression
  - 9 = maximum compression
- **`job_num`**: Job ID (useful for batch processing)

## Examples

### Example 1: Simple Merge

```json
{
    "job_num": 1,
    "input_files": {
        "run1.root": "/data/run1.root",
        "run2.root": "/data/run2.root"
    },
    "output_files": {
        "merged.root": "combined.root"
    },
    "output_dir": "output"
}
```

### Example 2: Merge with Compression

```json
{
    "job_num": 1,
    "input_files": {
        "file1.root": "/data/file1.root",
        "file2.root": "/data/file2.root",
        "file3.root": "/data/file3.root"
    },
    "output_files": {
        "merged.root": "compressed_output.root"
    },
    "output_dir": "output",
    "compression": 9
}
```

### Example 3: Workflow with Wildcards

```json
{
    "input_files": {
        "analysis.root": "/data/run*/analysis_*.root"
    },
    "output_files": {
        "merged.root": "all_runs_merged.root"
    },
    "output_dir": "/scratch/merged"
}
```

Use with:
```bash
hps-mc-workflow -n 20 -w merge_all python/jobs/merge_root_job.py params.json
```

## Troubleshooting

### hadd command not found

Make sure ROOT is properly set up in your environment:

```bash
source /path/to/root/bin/thisroot.sh
```

Or add ROOT's bin directory to your PATH:

```bash
export PATH=$PATH:/path/to/root/bin
```

### Input files not found

The job will fail if any input file doesn't exist. Verify all paths are correct and accessible.

### Output file already exists

If `force: false` and the output file exists, hadd will fail. Either:
- Set `force: true` in your parameters
- Delete the existing output file
- Use a different output filename

## Notes

- The MergeROOT component uses ROOT's `hadd` utility, which requires all input files to have compatible ROOT tree structures
- Large merges may require significant memory and time
- Consider using compression to reduce output file size
- For very large numbers of files, consider merging in stages (merge groups first, then merge the results)

## Related Components

- **LCIOMerge**: For merging LCIO files
- **MergeFiles**: For merging StdHep files
- **LCIOConcat**: For concatenating LCIO files
