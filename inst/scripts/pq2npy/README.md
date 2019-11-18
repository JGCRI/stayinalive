This directory contains the code that performs the zeroth processing
stage, which reformats the grid output from Xanthos from parquet into
numpy and splits the multiple grids from each Xanthos run into
individual files.  Each file has a single matrix with grid cells in
rows and months in columns.  Since the drought durations are always
integers, we write these out as `int16` arrays to keep them smaller.

* **input**: `drought_duration_trn_abcd_<model>_<scenario>_<runindex>.parquet`
* **output**: `duration_<model>_<scenario>_<runindex>_<gridindex>.npy`

