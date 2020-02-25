#!/bin/zsh

#SBATCH -t 179
#SBATCH -A GCAM
#SBATCH -J runsurv


####
####  Run as:  sbatch -a 0-69 runsurv.zsh inputdir outputdir
####
#### The inputdir should have the files drgt_matrix_*.pkl.  
module purge
module load gcc/8.1.0
module load python/anaconda3.6
source /share/apps/python/anaconda3.6/etc/profile.d/conda.sh 
module load R/3.4.3

echo 'start time:  ' `date`

indir=`readlink -e $1`
echo "Input dir:  $indir"

outdir=`readlink -e $2`
echo "Output dir: $outdir"

script=`Rscript -e "cat(system.file('scripts/runsurv/runsurv.R', package='stayinalive'))"`

echo "Script file:  $script"

## Source the file with the necessary R functions in it and run the
## survival analysis.  gcblksize = 10000, subblksize = 1000.
## Since there are 67420 grid cells, that makes 7 blocks with 10
## subblocks each, for a total of 70 tasks (numbered 0-69).
cmd="source('$script'); run_sa($SLURM_ARRAY_TASK_ID , 10000, 1000, '$indir', '$outdir', 12)"

echo $cmd
time Rscript -e $cmd


echo 'end time:   ' `date`

