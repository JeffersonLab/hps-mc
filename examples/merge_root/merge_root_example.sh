#!/bin/bash
#
# Example workflow for merging ROOT files from multiple run directories
#
# Arguments:
#   parent_dir: Directory containing run subdirectories
#   -o: Output prefix for generated files
#   -n: Maximum number of files per merge job (default: 20)
#   -f: File pattern to match (default: *.root)
#   -r: Run directory pattern (default: hps_*)

PARENT_DIR="/sdf/data/hps/physics2021/data/recon/pass4_v8/"
OUTPUT_PREFIX="merge_jobs"
MAX_FILES=50

hps-mc-prepare-merge-jobs \
    $PARENT_DIR \
    -o $OUTPUT_PREFIX \
    -n $MAX_FILES \
    -f *v0skim*root \
    --single-list

# This creates:
#   - merge_jobs_input_files.txt  (single consolidated file list)
#   - merge_jobs_vars.json        (iteration variables for template)
#   - merge_jobs_batches.json     (metadata about batches)

# Step 2: Generate job configurations using template
# ------------------------------------------------------

for batch_file in ${OUTPUT_PREFIX}_batch*_files.txt; do
    batch_num=$(echo $batch_file | grep -oP 'batch\K[0-9]+')
    hps-mc-job-template \
        -j $batch_num \
        -i root_files $batch_file $(wc -l < $batch_file) \
        merge_root.json.tmpl \
        ${OUTPUT_PREFIX}_batch${batch_num}_jobs.json
done

cat ${OUTPUT_PREFIX}_batch*_jobs.json | jq -s 'add' > ${OUTPUT_PREFIX}_jobs.json
