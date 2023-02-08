#!/bin/bash

cd ap_gen_to_slic
source cleanup.sh
sbatch --output=../logs/ap_gen_to_slic.out --error=../logs/ap_gen_to_slic.err run.sh 

cd ../beam_coords
source cleanup.sh
sbatch --output=../logs/beam_coords.out --error=../logs/beam_coords.err run.sh

cd ../beam_gen
source cleanup.sh
sbatch --output=../logs/beam_gen.out --error=../logs/beam_gen.err run.sh

cd ../beam_slic
source cleanup.sh
sbatch --output=../logs/beam_slic.out --error=../logs/beam_slic.err run.sh

cd ../data_cnv
source cleanup.sh
sbatch --output=../logs/data_cnv.out --error=../logs/data_cnv.err run.sh

cd ../fee_slic_to_recon
source cleanup.sh
sbatch --output=../logs/fee_slic_to_recon.out --error=../logs/fee_slic_to_recon.err run.sh

cd ../hpstr
source cleanup.sh
sbatch --output=../logs/hpstr.out --error=../logs/hpstr.err run.sh

cd ../moller_gen
source cleanup.sh
sbatch --output=../logs/moller_gen.out --error=../logs/moller_gen.err run.sh

cd ../rad_gen
source cleanup.sh
sbatch --output=../logs/rad_gen.out --error=../logs/rad_gen.err run.sh

cd ../readout_recon
source cleanup.sh
sbatch --output=../logs/readout_recon.out --error=../logs/readout_recon.err run.sh

cd ../simp
source cleanup.sh
sbatch --output=../logs/simp.out --error=../logs/simp.err run.sh

cd ../slic_to_anaMC
source cleanup.sh
sbatch --output=../logs/slic_to_anaMC.out --error=../logs/slic_to_anaMC.err run.sh

cd ../tritrig_beam
source cleanup.sh
sbatch --output=../logs/tritrig_beam.out --error=../logs/tritrig_beam.err run.sh

cd ../tritrig_gen
source cleanup.sh
sbatch --output=../logs/tritrig_gen.out --error=../logs/tritrig_gen.err run.sh

cd ../tritrig_slic_full_chain
source cleanup.sh
sbatch --output=../logs/tritrig_slic_full_chain.out --error=../logs/tritrig_slic_full_chain.err run.sh

cd ../wab_gen_sample
source cleanup.sh
sbatch --output=../logs/wab_gen_sample.out --error=../logs/wab_gen_sample.err run.sh

cd ..