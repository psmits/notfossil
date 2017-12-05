#!/bin/bash
Rscript ../R/prepare_data.r
bash ../src/stan_model.sh
Rscript ../R/process_posterior.r
