#/bin/bash
for I in $(seq 0 1 25); do
	#sbatch scan_yields_array_v2.sh $I
	#sbatch scan_yields_array_v8_ann.sh $I
	sbatch scan_yields_array_v9_ALL.sh $I
done
