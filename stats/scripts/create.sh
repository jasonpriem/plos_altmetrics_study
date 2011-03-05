#!/bin/sh
# run as ash -x create.sh to see debug

# clean derived data to start fresh
rm -f ../data/derived/*.txt*
rm -f ../data/derived/*.csv*
rm -f ../data/derived/*.RData*


# Want to append stdout and stderr to output file so won't use R CMD BATCH
## overwrite for the first step, then append

R --no-save < event_counts_altmetrics_cleaned.create.R > log.out 2>&1
python -u events_raw_wos_2008.create.py >> log.out
cp ../data/derived/events_raw_wos_2008.txt.gz ../data/raw/events_raw_wos_2008.txt.gz
R --no-save < event_counts_wos_extracted.create.R >> log.out 2>&1
R --no-save < event_counts_merged_cleaned.create.R >> log.out 2>&1
R --no-save < event_counts_research.create.R >> log.out 2>&1
R --no-save < event_counts_normalized.create.R >> log.out 2>&1
R --no-save < corr_pearson_normalized.create.R >> log.out 2>&1
R --no-save < event_counts_factor_scores.create.R >> log.out 2>&1

