#!/bin/zsh

#SBATCH -t 179
#SBATCH -A GCAM
#SBATCH -p shared
#SBATCH -J parquet2numpy

module purge
module load gcc/8.1.0
module load python/anaconda3.6
source /share/apps/python/anaconda3.6/etc/profile.d/conda.sh
module load R/3.4.3
module load intel

echo 'start time' `date`

pyscript=`Rscript -e 'cat(system.file("scripts/pq2npy/parquet2numpy.py", package="stayinalive"))'`

##nproc=$SLURM_ARRAY_TASK_COUNT
## PIC uses a super old version of slurm that doesn't support array_task_count
nproc=160
indx=$SLURM_ARRAY_TASK_ID

outdir=/pic/scratch/rpl/drought-duration-output
indir=$1

python $pyscript $indir $indx $nproc $outdir

echo 'end time' `date`
