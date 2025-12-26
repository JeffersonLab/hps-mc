"""! @package prepare_merge_jobs
Scan directories for ROOT files and prepare merge job configurations.

This module scans run directories (e.g., hps_014185), collects ROOT files,
batches them into groups of up to 20 files, and generates input file lists
for use with hps-mc-job-template.
"""

import os
import sys
import glob
import argparse
import json
from pathlib import Path


class MergeJobPreparation:
    """! Prepare merge jobs by scanning directories for ROOT files."""

    def __init__(self, parent_dir, output_prefix="merge_jobs", max_files_per_job=20,
                 file_pattern="*.root", run_pattern="hps_*"):
        """! Initialize the merge job preparation.

        @param parent_dir  Parent directory containing run subdirectories
        @param output_prefix  Prefix for output file lists and job configs
        @param max_files_per_job  Maximum number of ROOT files per merge job
        @param file_pattern  Glob pattern for files to merge (default: *.root)
        @param run_pattern  Glob pattern for run directories (default: hps_*)
        """
        self.parent_dir = Path(parent_dir).resolve()
        self.output_prefix = output_prefix
        self.max_files_per_job = max_files_per_job
        self.file_pattern = file_pattern
        self.run_pattern = run_pattern

        if not self.parent_dir.is_dir():
            raise ValueError(f"Parent directory does not exist: {self.parent_dir}")

    def scan_directories(self):
        """! Scan parent directory for run directories and ROOT files.

        @return Dictionary mapping run names to lists of ROOT file paths
        """
        run_files = {}

        # Find all run directories matching the pattern
        run_dirs = sorted(self.parent_dir.glob(self.run_pattern))

        if not run_dirs:
            print(f"Warning: No directories matching '{self.run_pattern}' found in {self.parent_dir}")
            return run_files

        print(f"Found {len(run_dirs)} run directories")

        # Scan each run directory for ROOT files
        for run_dir in run_dirs:
            if not run_dir.is_dir():
                continue

            run_name = run_dir.name
            root_files = sorted(run_dir.glob(self.file_pattern))

            if root_files:
                run_files[run_name] = [str(f) for f in root_files]
                print(f"  {run_name}: {len(root_files)} files")

        return run_files

    def create_batches(self, run_files):
        """! Create batches of files for merge jobs.

        If a run has more than max_files_per_job files, it will be split into
        multiple batches.

        @param run_files  Dictionary mapping run names to lists of file paths
        @return List of batch dictionaries with metadata
        """
        batches = []
        batch_id = 0

        for run_name, files in run_files.items():
            # Split files into batches of max_files_per_job
            for i in range(0, len(files), self.max_files_per_job):
                batch_files = files[i:i + self.max_files_per_job]

                batch_info = {
                    'batch_id': batch_id,
                    'run_name': run_name,
                    'batch_num': i // self.max_files_per_job,
                    'total_batches': (len(files) + self.max_files_per_job - 1) // self.max_files_per_job,
                    'files': batch_files,
                    'num_files': len(batch_files)
                }

                batches.append(batch_info)
                batch_id += 1

        return batches

    def write_input_file_lists(self, batches, single_file=False):
        """! Write input file lists for each batch.

        Creates either a single file list or separate files per batch.

        @param batches  List of batch dictionaries
        @param single_file  If True, write all files to one list; if False, one list per batch
        @return List of file paths written or single file path if single_file=True
        """
        if single_file:
            output_file = f"{self.output_prefix}_input_files.txt"
            with open(output_file, 'w') as f:
                for batch in batches:
                    for file_path in batch['files']:
                        f.write(f"{file_path}\n")

            total_files = sum(batch['num_files'] for batch in batches)
            print(f"\nWrote {total_files} file paths to: {output_file}")
            return output_file
        else:
            # Write separate file for each batch
            file_lists = []
            for batch in batches:
                batch_file = f"{self.output_prefix}_batch{batch['batch_id']:03d}_files.txt"
                with open(batch_file, 'w') as f:
                    for file_path in batch['files']:
                        f.write(f"{file_path}\n")
                file_lists.append(batch_file)

            print(f"\nWrote {len(batches)} separate input file lists:")
            for i, file_list in enumerate(file_lists):
                print(f"  Batch {i}: {file_list} ({batches[i]['num_files']} files)")

            return file_lists

    def write_batch_metadata(self, batches, output_file=None):
        """! Write batch metadata to a JSON file.

        This provides information about how files were grouped into batches,
        useful for generating appropriate output file names.

        @param batches  List of batch dictionaries
        @param output_file  Path to output file (default: {output_prefix}_batches.json)
        @return Path to the written file
        """
        if output_file is None:
            output_file = f"{self.output_prefix}_batches.json"

        with open(output_file, 'w') as f:
            json.dump(batches, f, indent=2)

        print(f"Wrote batch metadata to: {output_file}")

        return output_file

    def generate_iteration_vars(self, batches, output_file=None):
        """! Generate iteration variables JSON for hps-mc-job-template.

        Since the template system creates Cartesian products of iteration variables,
        we create a single "batch_index" variable that can be used to index into
        the batch metadata.

        Note: For merge jobs, it's often simpler to NOT use iteration variables
        and instead use the -r (repeat) option with file path parsing in templates.

        @param batches  List of batch dictionaries
        @param output_file  Path to output file (default: {output_prefix}_vars.json)
        @return Path to the written file
        """
        if output_file is None:
            output_file = f"{self.output_prefix}_vars.json"

        # Create a single iteration variable to avoid Cartesian product issues
        # Users can use this with the batch metadata file if needed
        vars_dict = {
            'batch_index': list(range(len(batches)))
        }

        with open(output_file, 'w') as f:
            json.dump(vars_dict, f, indent=2)

        print(f"Wrote iteration variables to: {output_file}")
        print(f"Note: Contains single batch_index variable to avoid Cartesian products")

        return output_file

    def run(self, write_vars=True, write_metadata=True, separate_lists=True):
        """! Run the full preparation workflow.

        @param write_vars  Write iteration variables JSON file
        @param write_metadata  Write batch metadata JSON file
        @param separate_lists  Write separate input file list per batch
        @return Dictionary with paths to generated files and batch info
        """
        print(f"Scanning parent directory: {self.parent_dir}")
        print(f"Run pattern: {self.run_pattern}")
        print(f"File pattern: {self.file_pattern}")
        print(f"Max files per job: {self.max_files_per_job}")
        print()

        # Scan directories
        run_files = self.scan_directories()

        if not run_files:
            print("\nNo files found. Exiting.")
            return None

        # Create batches
        batches = self.create_batches(run_files)

        print(f"\nCreated {len(batches)} batches:")
        for batch in batches:
            suffix = ""
            if batch['total_batches'] > 1:
                suffix = f" (batch {batch['batch_num'] + 1}/{batch['total_batches']})"
            print(f"  Batch {batch['batch_id']}: {batch['run_name']}{suffix} - {batch['num_files']} files")

        # Write file lists
        file_lists = self.write_input_file_lists(batches, single_file=not separate_lists)

        result = {
            'file_lists': file_lists if separate_lists else [file_lists],
            'num_batches': len(batches),
            'batches': batches,
            'separate_lists': separate_lists
        }

        if write_metadata:
            metadata_file = self.write_batch_metadata(batches)
            result['metadata_file'] = metadata_file

        if write_vars:
            vars_file = self.generate_iteration_vars(batches)
            result['vars_file'] = vars_file

        return result


def main():
    """! Command-line interface for merge job preparation."""

    parser = argparse.ArgumentParser(
        description="Scan directories for ROOT files and prepare merge job configurations",
        formatter_class=argparse.RawDescriptionHelpFormatter,
        epilog="""
Examples:
  # Scan directory and prepare job files
  %(prog)s /path/to/runs

  # Use custom output prefix
  %(prog)s /path/to/runs -o my_merge

  # Change max files per job
  %(prog)s /path/to/runs -n 10

  # Custom file and directory patterns
  %(prog)s /path/to/runs -f "*_recon.root" -r "run_*"

  # Skip generating vars file (only create input file list)
  %(prog)s /path/to/runs --no-vars
        """
    )

    parser.add_argument(
        'parent_dir',
        help='Parent directory containing run subdirectories'
    )

    parser.add_argument(
        '-o', '--output-prefix',
        default='merge_jobs',
        help='Prefix for output files (default: merge_jobs)'
    )

    parser.add_argument(
        '-n', '--max-files',
        type=int,
        default=20,
        help='Maximum number of files per merge job (default: 20)'
    )

    parser.add_argument(
        '-f', '--file-pattern',
        default='*.root',
        help='Glob pattern for files to merge (default: *.root)'
    )

    parser.add_argument(
        '-r', '--run-pattern',
        default='hps_*',
        help='Glob pattern for run directories (default: hps_*)'
    )

    parser.add_argument(
        '--no-vars',
        action='store_true',
        help='Do not generate iteration variables JSON file'
    )

    parser.add_argument(
        '--no-metadata',
        action='store_true',
        help='Do not generate batch metadata JSON file'
    )

    parser.add_argument(
        '--single-list',
        action='store_true',
        help='Write all files to a single input list instead of separate lists per batch (default: separate lists)'
    )

    args = parser.parse_args()

    try:
        prep = MergeJobPreparation(
            parent_dir=args.parent_dir,
            output_prefix=args.output_prefix,
            max_files_per_job=args.max_files,
            file_pattern=args.file_pattern,
            run_pattern=args.run_pattern
        )

        result = prep.run(
            write_vars=not args.no_vars,
            write_metadata=not args.no_metadata,
            separate_lists=not args.single_list
        )

        if result:
            print("\n" + "="*60)
            print("Preparation complete!")
            print("="*60)
            print(f"\nGenerated files:")

            if result['separate_lists']:
                print(f"  - {len(result['file_lists'])} separate input file lists:")
                for file_list in result['file_lists']:
                    print(f"      {file_list}")
            else:
                print(f"  - Input file list: {result['file_lists'][0]}")

            if 'vars_file' in result:
                print(f"  - Iteration vars:  {result['vars_file']}")
            if 'metadata_file' in result:
                print(f"  - Batch metadata:  {result['metadata_file']}")

            print(f"\nNext steps:")
            print(f"  1. Create/use the job template: merge_root.json.tmpl")
            print(f"  2. Generate jobs for each batch:")
            print()

            if result['separate_lists']:
                print(f"     # Process each batch separately (recommended)")
                print(f"     for batch_file in {args.output_prefix}_batch*_files.txt; do")
                print(f"       batch_num=$(echo $batch_file | grep -oP 'batch\\K[0-9]+')")
                print(f"       hps-mc-job-template \\")
                print(f"         -j $batch_num \\")
                print(f"         -i root_files $batch_file $(cat $batch_file | wc -l) \\")
                print(f"         merge_root.json.tmpl \\")
                print(f"         {args.output_prefix}_batch${{batch_num}}_jobs.json")
                print(f"     done")
                print()
                print(f"     # Or combine all into one jobs file:")
                print(f"     cat {args.output_prefix}_batch*_jobs.json | jq -s 'add' > {args.output_prefix}_all_jobs.json")
            else:
                print(f"     hps-mc-job-template \\")
                print(f"       -i root_files {result['file_lists'][0]} {args.max_files} \\")
                print(f"       merge_root.json.tmpl \\")
                print(f"       {args.output_prefix}_jobs.json")

            return 0
        else:
            return 1

    except Exception as e:
        print(f"Error: {e}", file=sys.stderr)
        return 1


if __name__ == '__main__':
    sys.exit(main())
