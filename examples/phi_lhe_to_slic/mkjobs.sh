#!/bin/bash

# Specify the input JSON file and the number of entries to generate
input_file="job.json"       # Change this to your actual input file path
num_entries=176                 # Change this to the desired number of entries

output_file="jobs.json"

# Read the input JSON structure once
template=$(<"$input_file")

# Start creating the output JSON file
echo "[" > "$output_file"

# Loop to create the specified number of entries
for i in $(seq 1 "$num_entries"); do
    # Replace fields in the template for the new entry
    new_entry=$(echo "$template" | sed "
        s/\"job_id\": [0-9]\+/\"job_id\": $i/;
        s|\"phi_rot.stdhep\": \"[^\"]*\"|\"phi_rot.stdhep\": \"stdhep/phi_4pt55_rot_${i}.stdhep\"|;
        s|\"phi_rot.slcio\": \"[^\"]*\"|\"phi_rot.slcio\": \"slic/phi_4pt55_rot_${i}.slcio\"|;
        s|\"/home/groups/laurenat/majd/lhe_files/phiKK_4550_MeV_7883/phiKK_4550_MeV_7883.lhe_[0-9]*\"|\"/home/groups/laurenat/majd/lhe_files/phiKK_4550_MeV_7883/phiKK_4550_MeV_7883.lhe_$i\"|
    ")

    # Append the new entry to the output file
    if [ "$i" -eq 1 ]; then
        echo "$new_entry" >> "$output_file"
    else
        echo ",$new_entry" >> "$output_file"
    fi
done

# Close the JSON array
echo "]" >> "$output_file"

echo "Generated $output_file with $num_entries entries."
