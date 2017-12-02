#!/bin/bash
Rscript ../R/prepare/data.r
bash ../src/stan_model.sh
Rscript ../R/process_posterior.r
