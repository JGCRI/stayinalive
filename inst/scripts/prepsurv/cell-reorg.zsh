#!/bin/zsh

#SBATCH -t 60
#SBATCH -A GCAM
#SBATCH -p shared
#SBATCH -J cell-reorg

module load R/3.4.3
module load python/anaconda3.6

date

indir=`readlink -e $1`
echo "Input dir:  $indir"

outdir=`readlink -e $2`
echo "Output dir: $outdir"

script=`Rscript -e "cat(system.file('scripts/prepsurv/cell-reorg.py', package='stayinalive'))"`

echo "Script file:  $script"

tid=$SLURM_ARRAY_TASK_ID
echo "Task ID:  $tid"

echo python $script $indir $outdir $tid 
time python $script $indir $outdir $tid 

date

