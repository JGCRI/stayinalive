#!/bin/zsh                                                                                                                                                                  

module load R/3.4.3
module load python/anaconda3.6

date

indir=`readlink -e $1`
echo "Input dir:  $indir"

outdir=`readlink -e $2`
echo "Output dir: $outdir"

script=`Rscript -e "cat(system.file('scripts/prepsurv/prepsurv.R', package='stayinalive'))"`

echo "Script file:  $script"

cmd="source('$script'); npy2event(0, 1, '$indir', '$outdir', 12)"

echo $cmd
time Rscript -e $cmd


date

