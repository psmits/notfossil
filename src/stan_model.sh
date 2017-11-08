#!/bin/bash
FILES=../data/data_dump/unit_data.data.R
for f in $FILES;
do
  for i in `seq 1 4`;
  do
    ../stan/hurdle_medium sample num_samples=1000 num_warmup=1000 \
      id=$i \
      data file=$f \
      output file=../data/mcmc_out/hurdle_medium_${i}.csv &
  done
  wait
done
