#!/bin/zsh

#SBATCH -t 179
#SBATCH -A GCAM
#SBATCH -p shared
#SBATCH -J parquet2numpy

module purge
module load gcc/8.1.0
module load python/anaconda3.6
source /share/apps/python/anaconda3.6/etc/profile.d/conda.sh

pyscript=`Rscript -e 'cat(system.file("inst/scripts/pq2npy", package="stayinalive"))'`

nproc=$SLURM_ARRAY_TASK_COUNT
indx=$SLURM_ARRAY_TASK_ID

outdir=/pic/scratch/rpl/drought-duration-output
indir=$1

python $pyscript $indir $indx $nproc $outdir
