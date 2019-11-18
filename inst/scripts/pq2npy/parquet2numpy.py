import fastparquet as pq
import numpy as np
import glob
import re
import sys
import os.path
from math import ceil

from fastparquet import ParquetFile

def get_pqfiles(inputdir, index, nproc):
    pqfiles = glob.glob(os.path.join(inputdir, '*.parquet'))
    pqfiles.sort()
    nfiles = len(pqfiles)
    nbatch = int(ceil(nfiles/nproc))
    istrt = index*nbatch
    istop = (index+1)*nbatch
    if istop > nfiles:
        istop = nfiles

    return pqfiles[istrt:istop]


def convertfiles(pqfiles, outputdir):
    patrn = r'drought_duration_trn_abcd_([^_]+)_([^_]+)_([0-9]+)'
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
                npyfilename = os.path.join(outputdir, f'duration_{model}_{scen}_{run}_{i}.npy')
                print(f'\tWriting: {npyfilename}')
                np.save(npyfilename, arsv)
        else:
            print(f'{pqfile} does not match the file naming pattern: {patrn}\tSkipping.')


if __name__ == '__main__':
    if len(sys.argv) < 3:
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

    pqfiles = get_pqfiles(inputdir, index, nproc)
    convertfiles(pqfiles, outputdir)
        
    print('FIN.')
