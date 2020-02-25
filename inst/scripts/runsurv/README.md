This directory contains the programs that prepare the data for use in
the survival analysis.  There are several stages to this:

1. Ingest arrays of grid data and convert them to event data.
2. Separate the events for each grid cell and write them to a
   temporary file for each grid cell / scenario
3. Merge the events from all of the scenarios for each grid cell, and
   write these out for use in the survival analysis.
   
Complicating this is that we have 67420 grid cells, and 1280 temporary
files (in the current configuration) per grid cell, so we have to
create some kind of hierarchy.  We'll divide grid cells into batches
of 100, with a subdir for each batch, and a subdir for each grid cell
within a batch.  Thus, the file names will be constructed as
`drgt-events/batchid/cellid/temp_drgt_event_<cellid>_<scenarioid>.rds`

* *inputs*: Results of stage 0 processing (npy files)
* *intermediate*:
  `batchid/cellid/temp_drgt_event_<cellid>_<scenarioid>.rds`
* *output*: `batchid/cellid/drgt_event_<cellid>.rds`

