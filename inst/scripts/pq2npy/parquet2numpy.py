import fastparquet as pq
import numpy as np
import glob
import re
import sys
import os.path
from math import ceil

from fastparquet import ParquetFile

def get_pqfiles(inputdir, index, nproc, xanthos_var):
    pqfiles = glob.glob(os.path.join(inputdir, 'drought_{}*.parquet'.format(xanthos_var)))
    pqfiles.sort()
    nfiles = len(pqfiles)
    nbatch = int(ceil(nfiles/nproc))
    istrt = index*nbatch
    istop = (index+1)*nbatch
    if istop > nfiles:
        istop = nfiles

    return pqfiles[istrt:istop]


def convertfiles(pqfiles, outputdir, xanthos_var):
    patrn = r'drought_{}_trn_abcd_([^_]+)_([^_]+)_([0-9]+)'.format(xanthos_var)
    cpatrn = re.compile(patrn)

    ngrid = 67420

    for pqfile in pqfiles:
        m = cpatrn.search(pqfile)
        if m:
            print(f'Processing: {pqfile}')
            (model, scen, run) = m.groups()

            data = pq.ParquetFile(pqfile)
            ar = data.to_pandas().to_numpy().astype(np.int16)
            ## Grid cells are in rows, months in columns
            (ncell, nmonth) = ar.shape
            nfield = ncell // ngrid
            if nfield != ncell/ngrid:
                print(f'ERROR:  array is not an integer number of fields.  ngrid= {ngrid}\tncell= {ncell}')
            for i in range(nfield):
                strt = i*ngrid
                end = (i+1)*ngrid
                arsv = ar[strt:end, :]
                npyfilename = os.path.join(outputdir, f'{xanthos_var}_{model}_{scen}_{run}_{i}.npy')
                print(f'\tWriting: {npyfilename}')
                np.save(npyfilename, arsv)
        else:
            print(f'{pqfile} does not match the file naming pattern: {patrn}\tSkipping.')


if __name__ == '__main__':
    if len(sys.argv) < 4:
        print(f"""Usage: {sys.argv[0]} <inputdir> <index> [<nproc> <outputdir>]
                  inputdir:  directory containing the parquet files
                  index:  array index of this process (used to determine which files will be processed
                          by this process.
                  nproc:  number of process in the array (default = 100)
                  outputdir:  Directory to write outputs to (default = .)
        """)
        sys.exit()

    inputdir = sys.argv[1]
    index = int(sys.argv[2])
    
    if len(sys.argv) > 3:
        nproc = int(sys.argv[3])
    else:
        nproc = 100

    if len(sys.argv) > 4:
        outputdir = sys.argv[4]
    else:
        outputdir = '.'

    if len(sys.argv) > 5:
        xanthos_var = str(sys.argv[5])
    else:
        xanthos_var = 'duration'  # default is to perform operation on all 'duration' files

    pqfiles = get_pqfiles(inputdir, index, nproc, xanthos_var)
    convertfiles(pqfiles, outputdir, xanthos_var)
        
    print('FIN.')
