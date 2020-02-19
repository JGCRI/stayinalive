#!/bin/zsh

#SBATCH -t 180
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

#### Could also do this if we wanted to reduce the total job count by
#### doing several batches in each job.
##
##jobblksize=5
##(( strt = $tid * $jobblksize ))
##(( end = ($tid+1) * $jobblksize -1 ))
##
#### Runids are valid up through 544.  That means our tids should go up
#### through 108
##runidmax=544
##
##for runid in {$strt..$end}; do
##    if [[ $runid < $runidmax ]]; then
##        echo python $script $indir $outdir $runid
##    	time python $script $indir $outdir $runid
##    fi
##done
##

date

