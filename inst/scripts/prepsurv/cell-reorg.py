"""
Usage:  python cell-reorg.py <indir> <outdir> <taskid>

Our grid cells are organized into matrices with grid cells in rows and
months in columns, and one matrix for each run.  This script converts
to the format we really want, which is runs in rows, months in
columns, and one matrix for each grid cell.

Complicating matters is that the runs have different climate scenarios
associated with them, so we need to keep track of that too.  On top of
that, the full set of runs can't fit in memory at once.  Finally, we
need to do this in a way that we're not writing hundreds of thousands
of files, which will clobber our file system.

Strategy:

1. Group data by RCP, model, and "batch".  A batch is a block of 10000
cells that will all be written into a file together.  Therefore, there
are 34 batches.

2. RCPs are indexed 0-3, models also 0-3, and batches 0-67.
Therefore, each processing ID is:
    * I = (b*Nr + r)*Nm + m.
Conversely:
    * b = floor(I/(Nr*Nm))
    * r = floor(I/Nm) - b*Nr
    * m = mod(I, Nm)
Therefore, the total number of jobs is 4*4*7 = 112 (running 0-111)


3. Filenames for the Xanthos output data are constructed according to
this template:
    * f'{xanthos_var}_{model_name}_{rcp_name}_{irun}_{ifld}.npy'
    where xanthos_var can be duration, severity, or intensity
The ifld variable is because each run generated multiple fields, which
were separated in a previous step.  Each field from a run counts as a
separate "run" for our purposes here.

4. The value of I is passed in as a parameter.  We decode r and m to
get the values we will need to construct the family of filenames for
the data.

5. Load all of the files and select from each matrix the grid cells
for the batch we are working.

6. For each grid cell, create a matrix whose rows are that grid cell's
row from each of the matrices from step 5.

7. Store these in a list and write out in a pickle file of the form
    * f'drgt_matrix_{model_name}_{rcp_name}_{batch}.pkl'
"""

import numpy as np
import pickle
import sys
import glob
import os.path

## constants
Nr = 4                          # Number of RCP scenarios
Nm = 4                          # Number of Earth system models
Ngrid = 67420                   # Number of grid cells
batchsize = 10000               # Number of grid cells in a batch
Nb = (Ngrid // batchsize) + 1   # Number of batches

modnames = ["GFDL-ESM2M", "IPSL-CM5A-LR", "HadGEM2-ES", "MIROC5"]
rcpnames = ["rcp26", "rcp45", "rcp60", "rcp85"]

indir = sys.argv[1]
outdir = sys.argv[2]

## Get the task ID.  This will be used to determine which files and
## grid cells we are processing (step 1)
tid = int(sys.argv[3])
b = tid // (Nr*Nm)
r = tid//Nm - b*Nr
m = tid % Nm

modname = modnames[m]
rcpname = rcpnames[r]

xanthos_var = sys.argv[4]

files = glob.glob(os.path.join(indir, f'{xanthos_var}_{modname}_{rcpname}_*.npy'))

if len(files) == 0:
    sys.stdout.write(f'No files matching pattern: {xanthos_var}_{modname}_{rcpname}_*.npy\n')
    sys.stdout.write(f'indir = {indir}')
    raise RuntimeError()
else:
    sys.stdout.write('Processing files:\n')
    for filen in files:
        sys.stdout.write(f'\t{filen}\n')
sys.stdout.flush()

cellstrt = batchsize * b        # first cell
cellend = batchsize * (b+1)     # last cell (not included)
if cellend > Ngrid:             # last block isn't full.
    cellend = Ngrid
ncell = cellend - cellstrt

mats_by_run = [np.load(file)[cellstrt:cellend, :] for file in files]
nrun = len(mats_by_run)
nmth = mats_by_run[0].shape[1]

## check that data was read correctly
for mat in mats_by_run:
    if mat.shape != (ncell, nmth):
        sys.stdout.write(f'Invalid matrix shape.  Got:  {mat.shape}  Expected:  {(ncell, nmth)}\n')
        sys.stdout.flush()
        raise RuntimeError()


## create a list of matrices
mats_by_cell = [None] * ncell
for icell in range(ncell):
    cell = np.empty((nrun, nmth), dtype=np.int16)
    for irun in range(nrun):
        cell[irun, :] = mats_by_run[irun][icell, :]
    mats_by_cell[icell] = cell

## output to file
outfilen = os.path.join(outdir, f'drgt_matrix_{xanthos_var}_{modname}_{rcpname}_batch-{b:03d}.pkl')
sys.stdout.write(f'\noutput file:  {outfilen}\n')
sys.stdout.flush()
outfile = open(outfilen, "wb")
pickle.dump(mats_by_cell, outfile)
outfile.close()

sys.stdout.write("FIN.\n")
