#!/bin/sh

set -v

ln -s /data/ZHULAB/grassland data
Rscript --vanilla -e "update.packages(ask = FALSE, Ncpus = 4, repos = 'https://cloud.r-project.org/')"
