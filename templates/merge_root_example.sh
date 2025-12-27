#!/bin/bash
#
# Example workflow for merging ROOT files from multiple run directories
#
# This example demonstrates how to:
# 1. Scan run directories for ROOT files
# 2. Prepare merge job configurations
# 3. Generate job JSON files (optimized single-call approach)
# 4. Execute merge jobs
#
# Performance Note:
# This script uses --single-list for hps-mc-prepare-merge-jobs which allows
# a SINGLE call to hps-mc-job-template instead of one call per batch.
# For large datasets, this provides ~100x speedup over the per-batch approach.
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
    -n $MAX_FILES \
    --single-list

# This creates:
#   - merge_jobs_input_files.txt  (single consolidated file list)
#   - merge_jobs_vars.json        (iteration variables for template)
#   - merge_jobs_batches.json     (metadata about batches)

# Step 2: Generate job configurations using template
# ------------------------------------------------------
# Single call to hps-mc-job-template (much faster than per-batch processing)

hps-mc-job-template \
    -i root_files ${OUTPUT_PREFIX}_input_files.txt $MAX_FILES \
    merge_root.json.tmpl \
    ${OUTPUT_PREFIX}_jobs.json

# This creates:
#   - merge_jobs_jobs.json  (all job configurations in one file)

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

# Alternative Approaches
# ------------------------------------------------------
#
# APPROACH 1: Per-batch processing (slower, but separate job files per batch)
# Useful if you need separate job files for different batch submissions
#
# hps-mc-prepare-merge-jobs \
#     $PARENT_DIR \
#     -o $OUTPUT_PREFIX \
#     -n $MAX_FILES
#     # (omit --single-list)
#
# for batch_file in ${OUTPUT_PREFIX}_batch*_files.txt; do
#     batch_num=$(echo $batch_file | grep -oP 'batch\K[0-9]+')
#     hps-mc-job-template \
#         -j $batch_num \
#         -i root_files $batch_file $(wc -l < $batch_file) \
#         merge_root.json.tmpl \
#         ${OUTPUT_PREFIX}_batch${batch_num}_jobs.json
# done
#
# cat ${OUTPUT_PREFIX}_batch*_jobs.json | jq -s 'add' > ${OUTPUT_PREFIX}_jobs.json
#
#
# APPROACH 2: Parallel per-batch processing (requires GNU parallel)
# Faster than sequential per-batch, but still slower than --single-list
#
# ls ${OUTPUT_PREFIX}_batch*_files.txt | parallel -j $(nproc) '
#     batch_num=$(echo {} | grep -oP "batch\K[0-9]+")
#     hps-mc-job-template \
#         -j $batch_num \
#         -i root_files {} $(wc -l < {}) \
#         merge_root.json.tmpl \
#         '${OUTPUT_PREFIX}'_batch${batch_num}_jobs.json
# '
#
# cat ${OUTPUT_PREFIX}_batch*_jobs.json | jq -s 'add' > ${OUTPUT_PREFIX}_jobs.json
