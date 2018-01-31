#!/bin/bash
FILES=../data/data_dump/diversity_data_full.data.R
for f in $FILES;
do
  for i in `seq 1 4`;
  do
    ../stan/trunc_time sample \
      adapt delta=0.99 \
      num_samples=2000 num_warmup=2000 thin=2 \
      algorithm=hmc engine=nuts max_depth=10 \
      id=$i \
      random seed=420 \
      data file=$f \
      output file=../data/mcmc_out/trunc_${i}_full.csv &
  done
  wait
done
