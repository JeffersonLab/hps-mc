#!/bin/bash
#
# Example workflow for merging ROOT files from multiple run directories
#
# This example demonstrates how to:
# 1. Scan run directories for ROOT files
# 2. Prepare merge job configurations
# 3. Generate job JSON files
# 4. Execute merge jobs
#

# Step 1: Scan directories and prepare input files
# ------------------------------------------------------
# This scans a parent directory for run directories (hps_XXXXXX),
# finds all ROOT files, and batches them into groups of up to 20 files.
#
# Arguments:
#   parent_dir: Directory containing run subdirectories
#   -o: Output prefix for generated files
#   -n: Maximum number of files per merge job (default: 20)
#   -f: File pattern to match (default: *.root)
#   -r: Run directory pattern (default: hps_*)

PARENT_DIR="/sdf/data/hps/physics2021/data/recon/pass4_v8/"
OUTPUT_PREFIX="merge_jobs"
MAX_FILES=20

hps-mc-prepare-merge-jobs \
    $PARENT_DIR \
    -o $OUTPUT_PREFIX \
    -n $MAX_FILES

# This creates:
#   - merge_jobs_batch*_files.txt (individual batch file lists)
#   - merge_jobs_vars.json        (iteration variables for template)
#   - merge_jobs_batches.json     (metadata about batches)

# Step 2: Generate job configurations using template
# ------------------------------------------------------
# Process each batch separately (recommended by hps-mc-prepare-merge-jobs)

for batch_file in ${OUTPUT_PREFIX}_batch*_files.txt; do
    batch_num=$(echo $batch_file | grep -oP 'batch\K[0-9]+')
    hps-mc-job-template \
        -j $batch_num \
        -i root_files $batch_file $(cat $batch_file | wc -l) \
        merge_root.json.tmpl \
        ${OUTPUT_PREFIX}_batch${batch_num}_jobs.json
done

# Combine all batch job files into one
cat ${OUTPUT_PREFIX}_batch*_jobs.json | jq -s 'add' > ${OUTPUT_PREFIX}_jobs.json

# This creates:
#   - merge_jobs_batch*_jobs.json  (individual batch job configurations)
#   - merge_jobs_jobs.json  (combined job configurations)

# Step 3: Run the merge jobs
# ------------------------------------------------------
# Execute each job using the hps-mc job runner

# Get the number of jobs
NUM_JOBS=$(python3 -c "import json; print(len(json.load(open('${OUTPUT_PREFIX}_jobs.json'))))")

echo "Generated $NUM_JOBS merge jobs"

# Execute all jobs (or submit to batch system)
for job_id in $(seq 0 $((NUM_JOBS - 1))); do
    echo "Running job $job_id..."
    python3 python/hpsmc/job.py run python/jobs/root_merge_job.py \
        -i $job_id \
        ${OUTPUT_PREFIX}_jobs.json
done

# Alternative: Submit to batch system (SLURM example)
# for job_id in $(seq 0 $((NUM_JOBS - 1))); do
#     sbatch --job-name=merge_$job_id \
#            --wrap="python3 python/hpsmc/job.py run python/jobs/root_merge_job.py -i $job_id ${OUTPUT_PREFIX}_jobs.json"
# done

# Step 4: Verify merged outputs
# ------------------------------------------------------
# Check that output files were created successfully

ls -lh output/*/merged_*.root
