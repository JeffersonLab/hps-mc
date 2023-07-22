#!/bin/bash
#SBATCH --ntasks=1
#SBATCH --time=1:00:00
#SBATCH --mem=6000M
#SBATCH --array=1-1
#SBATCH --partition=production
#SBATCH --job-name=evio_slcio
<<co
#SBATCH --output=/dev/null
co
OPTIND=1

while getopts o:s: flag
do
    case "${flag}" in
        o) output_dir=${OPTARG};;
    esac

done

jobjson=/w/hallb-scshelf2102/hps/rodwyer/sw/hps-mc/examples/baselines/jobs.json
runnumber=14546
scratch_dir=/w/hallb-scshelf2102/hps/rodwyer/baselines/scratch3/baselines${runnumber}
output_dir=${scratch_dir}

export JOB_ID=$(($SLURM_ARRAY_TASK_ID))
export SCRATCHDIR=${scratch_dir}/2dhistos/${JOB_ID}
echo ${SCRATCHDIR}
#mkdir ${SCRATCH}/lcio
#mkdir ${scratch_dir}/lcio/${JOB_ID}
mkdir -p ${SCRATCHDIR}
mkdir ${SCRATCHDIR}/../logs
touch ${SCRATCHDIR}/../logs/job.${JOB_ID}.{log,err,out}

cd ${SCRATCHDIR}
#Configure HPSTR and hps-mc
cfg_env=~/.bashrc
source $cfg_env

/usr/bin/python3 ${HPSMC_DIR}/lib/python/hpsmc/job.py run -o ${SCRATCHDIR}/../logs/job.${JOB_ID}.out -e ${SCRATCHDIR}/../logs/job.${JOB_ID}.err -d ${SCRATCHDIR} -i $JOB_ID ${HPSMC_DIR}/../examples/baselines/baseline.py $jobjson > ${SCRATCHDIR}/../logs/job.${JOB_ID}.log


